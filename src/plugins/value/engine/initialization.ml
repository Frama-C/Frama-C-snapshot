(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* Creation of the inital state of abstract domain. *)

open Cil_types
open Eval

type source = Supplied | Computed

module type S = sig
  type state
  val initial_state : unit -> state Bottom.or_bottom
  val initial_state_with_formals : kernel_function -> state or_bottom
end

(* Is the padding filled with fully initialized values. In this case, we
   can speed up the generation of the initial state in a few cases. *)
let fully_initialized_padding () =
  Value_parameters.InitializationPaddingGlobals.get () = "yes"


let warn_unknown_size_aux pp v (messt, t) =
  Value_parameters.warning ~once:true ~current:true
    "@[during initialization@ of %a,@ size of@ type '%a'@ cannot be@ computed@ \
     (%s)@]" pp v Printer.pp_typ t messt

let warn_unknown_size =
  warn_unknown_size_aux
    (fun fmt v -> Format.fprintf fmt "variable '%a'" Printer.pp_varinfo v)

let warn_size vi =
  try
    ignore (Cil.bitsSizeOf vi.vtype);
    false
  with Cil.SizeOfError (s, t)->
    warn_unknown_size vi (s, t);
    true


let counter = ref 0

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S with type value = Value.t)
    (Domain: Abstract_domain.External with type value = Value.t
                                       and type location = Loc.location)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type origin = Domain.origin
                        and type loc = Domain.location)
= struct

  incr counter;

  module Transfer = Domain.Transfer (Eva.Valuation)

  (** Padding value. The exact contents (bottom | zero | top_int),
      initialized or not, is determined from [lib_entry] and option
      [-val-initialization-padding-globals] *)
  let padding_value lib_entry =
    match Value_parameters.InitializationPaddingGlobals.get () with
    | "yes" -> `Value ((if lib_entry then Value.top_int else Value.zero), true)
    | "no"  -> `Bottom
    | "maybe" ->
      `Value ((if lib_entry then Value.top_int else Value.zero), false)
    | _ -> assert false

  exception Initialization_failed

  (* Evaluation in Top state.
     We do not want the location to depend on other globals. *)
  let lval_to_loc lval = Eva.lvaluate ~for_writing:false Domain.top lval

  let init_var state lval varinfo value =
    ignore (warn_size varinfo);
    let lloc = Loc.eval_varinfo varinfo in
    Domain.initialize_var state lval lloc (`Value (value, true))

  let init_var' state lval varinfo v =
    ignore (warn_size varinfo);
    let loc = Loc.eval_varinfo varinfo in
    Domain.initialize_var state lval loc v

  let init_var_lib_entry state lval varinfo =
    let loc = Loc.eval_varinfo varinfo in
    if warn_size varinfo then
      Domain.initialize_var state lval loc (`Value (Value.top_int, true))
    else
      (* add padding everywhere *)
      let padding = padding_value true in
      let state = Domain.initialize_var state lval loc padding in
      (* then initialize non-padding bits according to the type *)
      Domain.initialize_var_using_type state varinfo


  let assign_single_initializer state lval expr =
    lval_to_loc lval >>= fun (valuation, lloc, ltyp) ->
    Eva.evaluate ~valuation state expr >>=: fun (valuation, value) ->
    let left_lv = { lval; ltyp; lloc } in
    left_lv, expr, Assign value, valuation

  (* Evaluation of a [SingleInit] in Cil parlance.
     TODO: volatile *)
  let init_single_initializer state lval expr =
    let eval, alarms = assign_single_initializer state lval expr in
    let with_alarms = Value_util.warn_all_quiet_mode () in
    Alarmset.emit with_alarms alarms;
    match eval with
    | `Bottom ->
      Value_parameters.result ~source:(fst expr.eloc)
        "Evaluation of initializer '%a' failed@." Printer.pp_exp expr;
      raise Initialization_failed
    | `Value (lv, expr, assigned, valuation) ->
      Transfer.assign Kglobal lv expr assigned valuation state


  (* Apply an initializer (not recursively). Take volatile qualifiers into
     account. If [warn] holds, we warn when an initializer is ignored
     because it points to a volatile location.
     TODO: volatile *)
  let rec init_initializer_or_volatile state lval init warn =
    if Cil.typeHasQualifier "volatile" (Cil.typeOfLval lval) then begin
      if warn then
        Value_util.warning_once_current
          "global initialization of volatile zone %a ignored"
          Printer.pp_lval lval;
      let eval, _alarms = lval_to_loc lval in
      eval >>-: fun (_valuation, loc, _typ) ->
      Domain.initialize_var state lval loc (`Value (Value.top_int, true))
    end
    else
      match init with
      | SingleInit exp -> init_single_initializer state lval exp
      | CompoundInit (base_typ, l) ->
        Cil.foldLeftCompound
          ~implicit:false
          ~doinit:
            (fun off init _typ state ->
               state >>- fun state ->
               let lval' = Cil.addOffsetLval off lval in
               init_initializer_or_volatile state lval' init warn)
          ~ct:base_typ
          ~initl:l
          ~acc:(`Value state)

  (* Special initializers. Only lval with attributes 'const' and non-volatile
     are initialized *)
  let rec init_const_initializer state lval = function
    | SingleInit exp ->
      let typ_lval = Cil.typeOfLval lval in
      if Cil.typeHasQualifier "const" typ_lval &&
         not (Cil.typeHasQualifier "volatile" typ_lval)
      then
        init_single_initializer state lval exp
      else `Value state
    | CompoundInit (base_typ, l) ->
      if Cil.typeHasQualifier "volatile" base_typ ||
         not (Cil.typeHasAttributeDeep "const" base_typ)
      then `Value state (* initializer is not useful *)
      else
        Cil.foldLeftCompound
          ~implicit:true
          ~doinit:
            (fun off init _typ state ->
               state >>- fun state ->
               init_const_initializer state (Cil.addOffsetLval off lval) init)
          ~ct:base_typ
          ~initl:l
          ~acc:(`Value state)


  (* initialize [vi] when [-lib-entry] is not set, by writing successively
     the padding, zero, and the initializers. *)
  let init_var_not_lib_entry_initializer vi init state =
    Cil.CurrentLoc.set vi.vdecl;
    let volatile_somewhere = Cil.typeHasAttributeDeep "volatile" vi.vtype in
    let volatile_everywhere = Cil.typeHasQualifier "volatile" vi.vtype in
    let lval = Var vi, NoOffset in
    if fully_initialized_padding () &&
       (volatile_everywhere || not volatile_somewhere)
    then
      (* shortcut: padding and volatile won't interfere, we can do a global
         initialisation, then write the initializer on top if there is one. *)
      if volatile_everywhere then begin
        if init <> None then
          Value_util.warning_once_current
            "global initialization of volatile variable %a ignored"
            Printer.pp_varinfo vi;
        `Value (init_var state lval vi Value.top_int)
      end
      else
        let state = init_var state lval vi Value.zero in
        match init with
        | None -> `Value state
        | Some init -> init_initializer_or_volatile state lval init true
    else (* "slow" initialization *)
      let padding = padding_value false in
      let state = init_var' state lval vi padding in
      let typ = vi.vtype in
      let loc = Cil_datatype.Location.unknown in
      let zi = Cil.makeZeroInit ~loc typ in
      (* initialise everything (except padding) to zero). Do not warn, as
         most of the initializer is generated. *)
      init_initializer_or_volatile state lval zi false >>- fun state ->
      (* then write the real initializer on top *)
      match init with
      | None -> `Value state
      | Some init -> init_initializer_or_volatile state lval init true


  (* initialize [vi] as if in [-lib-entry] mode. Active when [-lib-entry] is set,
     or when [vi] is extern. [const] initializers, explicit or implicit, are
     taken into account *)
  let init_var_lib_entry_initializer vi init state =
    Cil.CurrentLoc.set vi.vdecl;
    let lval = Var vi, NoOffset in
    if Cil.typeHasQualifier "const" vi.vtype && not (vi.vstorage = Extern)
    then (* Fully const base. Ignore -lib-entry altogether *)
      init_var_not_lib_entry_initializer vi init state
    else
      (* Fill padding + contents of non-padding bits according to the type *)
      let state = init_var_lib_entry state lval vi in
      (* if needed, initialize const fields according to the initialiser
         (or generate one if there are none). In the first phase, they have been
         set to generic values *)
      if Cil.typeHasAttributeDeep "const" vi.vtype && not (vi.vstorage = Extern)
      then
        let init = match init with
          | None -> Cil.makeZeroInit ~loc:vi.vdecl vi.vtype
          | Some init -> init
        in
        init_const_initializer state lval init
      else  `Value state


  module Domain_with_Bottom = Bottom.Make_Datatype (Domain)

  module NotLibEntryGlobals =
    State_builder.Option_ref
      (Domain_with_Bottom)
      (struct
        let name = "Value.Initialization.NotLibEntryGlobals"
                   ^ "(" ^ string_of_int !counter ^ ")"
        let dependencies =
          [ Ast.self; Kernel.AbsoluteValidRange.self;
            Value_parameters.InitializationPaddingGlobals.self ]
      end)

  module LibEntryGlobals =
    State_builder.Option_ref
      (Domain_with_Bottom)
      (struct
        let name = "Value.Initialization.LibEntryGlobals"
                   ^ "(" ^ string_of_int !counter ^ ")"
        open Value_parameters
        let dependencies =
          [ Ast.self; Kernel.AbsoluteValidRange.self;
            InitializationPaddingGlobals.self; AllocatedContextValid.self;
            AutomaticContextMaxWidth.self; AutomaticContextMaxDepth.self;
          ]
      end)
  let () = Ast.add_monotonic_state LibEntryGlobals.self

  let initial_state ~lib_entry () =
    Value_parameters.debug ~level:2 "Computing globals values";
    try
      `Value
        (Globals.Vars.fold_in_file_order
           (fun vi init state ->
              if vi.vsource then begin
                let initialize =
                  if lib_entry || (vi.vstorage = Extern (* use -lib-entry mode. *))
                  then init_var_lib_entry_initializer
                  else init_var_not_lib_entry_initializer
                in
                match initialize vi init.init state with
                | `Bottom -> raise Initialization_failed
                | `Value state -> state
              end
              else state
           ) (Domain.empty ()))
    with Initialization_failed -> `Bottom

  let initial_state_not_lib_entry () =
    NotLibEntryGlobals.memo (initial_state ~lib_entry:false)

  let initial_state_lib_entry () =
    LibEntryGlobals.memo (initial_state ~lib_entry:true)

  let compute_initial_state () =
    if snd (Globals.entry_point ()) then
      initial_state_lib_entry ()
    else
      initial_state_not_lib_entry ()

  let initial_state () =
    match Domain.global_state () with
    | Some state -> state
    | None -> compute_initial_state ()

  (* Print cvalue state only. TODO: apply to the whole state. *)
  let report_initial_state init_state source =
    let cvalue_state = Cvalue_domain.extract Domain.get init_state in
    match source with
    | Supplied ->
      Value_parameters.feedback "Initial state supplied by user";
      Value_parameters.printf
        ~header:(fun fmt -> Format.pp_print_string fmt
                    "Values of globals")
        ~level:2 "@[  %a@]" Cvalue.Model.pretty cvalue_state
    | Computed ->
      Value_parameters.feedback "Initial state computed";
      Value_parameters.printf ~dkey:Value_parameters.dkey_initial_state
        ~header:(fun fmt -> Format.pp_print_string fmt
                    "Values of globals at initialization")
        "@[  %a@]" Cvalue.Model.pretty cvalue_state

  let compute_main_formals kf state =
    match kf.fundec with
    | Declaration (_, _, None, _) -> state
    | Declaration (_, _, Some l, _)
    | Definition ({ sformals = l }, _) ->
      if l <> [] && Value_parameters.InterpreterMode.get()
      then begin
        Value_parameters.error "Entry point %a has arguments"
          Kernel_function.pretty kf;
        exit 0;
      end;
      List.fold_right
        (fun vi state -> Domain.initialize_var_using_type state vi)
        l
        state

  let add_main_formals kf state =
    match Db.Value.fun_get_args () with
    | None -> compute_main_formals kf state
    | Some _actuals -> assert false


  let initial_state_with_formals kf =
    let init_state, source =
      match Domain.global_state () with
      | Some state -> state, Supplied
      | None ->
        Value_parameters.feedback "Computing initial state";
        compute_initial_state (), Computed
    in
    Db.Value.initial_state_only_globals :=
      (fun () -> Cvalue_domain.extract Domain.get init_state);
    report_initial_state init_state source;
    init_state >>-: add_main_formals kf

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
