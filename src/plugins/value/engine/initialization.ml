(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* Creation of the initial state of abstract domains. *)

open Cil_types
open Eval

module type S = sig
  type state
  val initial_state : lib_entry:bool -> state or_bottom
  val initial_state_with_formals :
    lib_entry:bool -> kernel_function -> state or_bottom
  val initialize_local_variable:
    stmt -> varinfo -> Cil_types.init -> state -> state or_bottom
end

type padding_initialization = [
  | `Initialized
  | `Uninitialized
  | `MaybeInitialized
]

(* There are two different options for locals and for globals variables:
   a three-valued parameter of Eva for globals, and a boolean parameter of
   the kernel for locals. Please don't ask. *)
let padding_initialization ~local : padding_initialization =
  if local
  then
    if Kernel.InitializedPaddingLocals.get ()
    then `Initialized else `Uninitialized
  else
    match Value_parameters.InitializationPaddingGlobals.get () with
    | "yes" -> `Initialized
    | "maybe" -> `MaybeInitialized
    | "no" -> `Uninitialized
    | _ -> assert false

(* Warn if the size is unknown. *)
let warn_unknown_size vi =
  try
    ignore (Cil.bitsSizeOf vi.vtype);
    false
  with Cil.SizeOfError (s, t)->
    let pp fmt v = Format.fprintf fmt "variable '%a'" Printer.pp_varinfo v in
    Value_parameters.warning ~once:true ~current:true
      "@[during initialization@ of %a,@ size of@ type '%a'@ cannot be@ \
       computed@ (%s)@]" pp vi Printer.pp_typ t s;
    true

let warn_on_volatile kinstr lval =
  let is_local = match kinstr with Kglobal -> false | Kstmt _ -> true in
  let is_var = match lval with (Var _, NoOffset) -> true | _ -> false in
  Value_util.warning_once_current
    "%sinitialization of volatile %s %a ignored"
    (if is_local then "" else "global ")
    (if is_var then "variable" else "zone")
    Printer.pp_lval lval

(* A bottom in any part of an initializer results in a bottom for the
   whole initialization. Thus, the following monad raises an exception on a
   bottom case; the exception is catched by the root initialization functions
   to return a proper `Bottom. *)
exception Initialization_failed

let (>>>) t f = match t with
  | `Bottom -> raise Initialization_failed
  | `Value v -> f v

let counter = ref 0

module Make
    (Domain: Abstract_domain.External)
    (Eva: Evaluation.S with type state = Domain.state
                        and type loc = Domain.location)
    (Transfer: Transfer_stmt.S with type state = Domain.t)
= struct

  incr counter;;

  (* Evaluation in the top state: we do not want a location to depend on
     other globals. *)
  let lval_to_loc lval =
    fst (Eva.lvaluate ~for_writing:false Domain.top lval)
    >>> fun (_valuation, loc, _typ) -> loc


  (* ------------------------- Apply initializer ---------------------------- *)

  (* Conventions:
     - functions in *_var_* act on the entire variables, and receive only
       the corresponding varinfo
     - other functions act on a lvalue, which they directly receive *)

  (* Initializes an entire variable [vi], in particular padding bits,
     according to [local] and [lib_entry] mode. *)
  let initialize_var_padding ~local ~lib_entry vi state =
    let lval = Cil.var vi in
    match padding_initialization ~local with
    | `Uninitialized -> state
    | `Initialized | `MaybeInitialized as i ->
      let initialized = i = `Initialized in
      let init_value =
        if not local && lib_entry
        then Abstract_domain.Top
        else Abstract_domain.Zero
      in
      let location = lval_to_loc lval in
      Domain.initialize_variable lval location ~initialized init_value state

  (* Initializes a volatile lvalue to top. *)
  let initialize_volatile lval state =
    let location = lval_to_loc lval in
    let init_value = Abstract_domain.Top in
    Domain.initialize_variable lval location ~initialized:true init_value state

  (* Applies a single Cil initializer, using the standard transfer function on
     assignments. Warns if the results is bottom. *)
  let apply_cil_single_initializer kinstr state lval expr =
    match Transfer.assign state kinstr lval expr with
    | `Bottom ->
      if kinstr = Kglobal then
        Value_parameters.result ~source:(fst expr.eloc)
          "evaluation of initializer '%a' failed@." Printer.pp_exp expr;
      raise Initialization_failed
    | `Value v -> v

  (* Applies an initializer. Take volatile qualifiers into account.
     If [warn] holds, warns when an initializer is ignored because it points
     to a volatile location. *)
  let rec apply_cil_initializer kinstr ~warn lval init state =
    if Cil.typeHasQualifier "volatile" (Cil.typeOfLval lval)
    then
      if warn
      then (warn_on_volatile kinstr lval; state)
      else initialize_volatile lval state
    else
      match init with
      | SingleInit exp -> apply_cil_single_initializer kinstr state lval exp
      | CompoundInit (typ, l) ->
        let doinit off init _typ state =
          let lval = Cil.addOffsetLval off lval in
          apply_cil_initializer kinstr ~warn lval init state
        in
        Cil.foldLeftCompound ~implicit:false ~doinit ~ct:typ ~initl:l ~acc:state

  (* Initialization of a variable to zero (or top if volatile), field by field.
     Very inefficient. *)
  let initialize_var_zero_or_volatile kinstr vi state =
    let loc = Cil_datatype.Location.unknown in
    let zero_init = Cil.makeZeroInit ~loc vi.vtype in
    apply_cil_initializer kinstr ~warn:false (Cil.var vi) zero_init state

  (* ----------------------- Non Lib-entry mode ----------------------------- *)

  (* Initializes a varinfo, padding bits + optionaly an initializer. *)
  let initialize_var_not_lib_entry kinstr ~local vi init state =
    ignore (warn_unknown_size vi);
    let typ = vi.vtype in
    let lval = Cil.var vi in
    let volatile_everywhere = Cil.typeHasQualifier "volatile" typ in
    if volatile_everywhere && padding_initialization ~local = `Initialized
    then
      let () = if init <> None then warn_on_volatile kinstr lval in
      initialize_volatile lval state
    else
      (* Initializes padding bits everywhere (non padding bits are overwritten
         afterwards). *)
      let state =
        initialize_var_padding vi ~local ~lib_entry:false state
      in
      (* Initializes everything except padding bits: non-volatile locations
         to zero, volatile locations to top. We only do so if the variable
         must be different from zero somewhere. This is a not-so minor
         optimization. *)
      let state =
        if padding_initialization ~local = `Initialized &&
           not (Cil.typeHasAttributeDeep "volatile" typ)
        then state
        else initialize_var_zero_or_volatile kinstr vi state
      in
      (* Applies the real initializer on top. *)
      match init with
      | None -> state
      | Some init -> apply_cil_initializer kinstr ~warn:true lval init state


  (* --------------------------- Lib-entry mode ----------------------------- *)

  (* Special application of an initializer: only non-volatile lval with
     attributes 'const' are initialized. *)
  let rec apply_cil_const_initializer kinstr state lval = function
    | SingleInit exp ->
      let typ_lval = Cil.typeOfLval lval in
      if Cil.typeHasQualifier "const" typ_lval &&
         not (Cil.typeHasQualifier "volatile" typ_lval)
      then apply_cil_single_initializer kinstr state lval exp
      else state
    | CompoundInit (typ, l) ->
      if Cil.typeHasQualifier "volatile" typ ||
         not (Cil.typeHasAttributeDeep "const" typ)
      then state (* initializer is not useful *)
      else
        let doinit off init _typ state =
          apply_cil_const_initializer
            kinstr state (Cil.addOffsetLval off lval) init
        in
        Cil.foldLeftCompound ~implicit:true ~doinit ~ct:typ ~initl:l ~acc:state

  (* Initializes [vi] as if in [-lib-entry] mode. Active when [-lib-entry] is
     set, or when [vi] is extern. [const] initializers, explicit or implicit,
     are taken into account *)
  let initialize_var_lib_entry kinstr vi init state =
    if Cil.typeHasQualifier "const" vi.vtype && not (vi.vstorage = Extern)
    then (* Fully const base. Ignore -lib-entry altogether. *)
      initialize_var_not_lib_entry kinstr ~local:false vi init state
    else
      let unknown_size =  warn_unknown_size vi in
      let state =
        if unknown_size then
          (* the type is unknown, initialize everything to Top *)
          let lval = Cil.var vi in
          let loc = lval_to_loc lval in
          let v = Abstract_domain.Top in
          Domain.initialize_variable lval loc ~initialized:true v state
        else
          (* Add padding everywhere. *)
          let state =
            initialize_var_padding vi ~local:false ~lib_entry:true state
          in
          (* Then initialize non-padding bits according to the type. *)
          let kind = Abstract_domain.Library_Global in
          Domain.initialize_variable_using_type kind vi state
      in
      (* If needed, initializes const fields according to the initialiser
         (or generate one if there are none). In the first phase, they have been
         set to generic values. *)
      if Cil.typeHasAttributeDeep "const" vi.vtype && not (vi.vstorage = Extern)
      then
        let init = match init with
          | None -> Cil.makeZeroInit ~loc:vi.vdecl vi.vtype
          | Some init -> init
        in
        apply_cil_const_initializer kinstr state (Cil.var vi) init
      else state


  (* ------------- Adds formal argument of the main function  --------------- *)

  (* Compute values for the formals of [kf] (as if those were variables in
     lib-entry mode) and add them to [state] *)
  let compute_main_formals kf state =
    match kf.fundec with
    | Declaration (_, _, None, _) -> state
    | Declaration (_, _, Some l, _)
    | Definition ({ sformals = l }, _) ->
      if l <> [] && Value_parameters.InterpreterMode.get ()
      then
        Value_parameters.abort "Entry point %a has arguments"
          Kernel_function.pretty kf
      else
        let kind = Abstract_domain.Main_Formal in
        List.fold_right (Domain.initialize_variable_using_type kind) l state

  (* Use the values supplied in [actuals] for the formals of [kf], and
     bind them in [state] *)
  let add_supplied_main_formals kf actuals state =
    match Domain.get Cvalue_domain.key with
    | None ->
      Value_parameters.abort "Function Db.Value.fun_set_args cannot be used \
                              without the Cvalue domain"
    | Some get_cvalue ->
      let formals = Kernel_function.get_formals kf in
      if (List.length formals) <> List.length actuals then
        raise Db.Value.Incorrect_number_of_arguments;
      let cvalue_state = get_cvalue state in
      let add_actual state actual formal =
        let actual = Eval_op.offsetmap_of_v ~typ:formal.vtype actual in
        Cvalue.Model.add_base (Base.of_varinfo formal) actual state
      in
      let cvalue_state =
        List.fold_left2 add_actual cvalue_state actuals formals
      in
      let set_domain = Domain.set Cvalue_domain.key in
      set_domain cvalue_state state

  let add_main_formals kf state =
    match Db.Value.fun_get_args () with
    | None -> compute_main_formals kf state
    | Some actuals -> add_supplied_main_formals kf actuals state


  (* ------------------------ High-level functions -------------------------- *)

  let initialize_local_variable stmt vi init state =
    try
      `Value
        (initialize_var_not_lib_entry
           (Kstmt stmt) ~local:true vi (Some init) state)
    with Initialization_failed -> `Bottom

  let initialize_global_variable ~lib_entry vi init state =
    Cil.CurrentLoc.set vi.vdecl;
    let state = Domain.introduce_globals [vi] state in
    if vi.vsource then
      let initialize =
        if lib_entry || (vi.vstorage = Extern)
        then initialize_var_lib_entry
        else initialize_var_not_lib_entry ~local:false
      in
      initialize Kglobal vi init.init state
    else state

  (* Compute the initial state with all global variable initialized. *)
  let compute_global_state ~lib_entry () =
    Value_parameters.debug ~level:2 "Computing globals values";
    let state = Domain.empty () in
    let initialize = initialize_global_variable ~lib_entry in
    try `Value (Globals.Vars.fold_in_file_order initialize state)
    with Initialization_failed -> `Bottom

  (* Dependencies for the Frama-C states containing the initial states
     of EVA: all correctness parameters of EVA, plus the AST itself. We
     cannot use [Db.Value.self] directly, because we do not want to
     depend on the tuning parameters. Previously, we use a more
     fine-grained list, but this lead to bugs. See mantis #2277. *)
  let correctness_deps =
    Ast.self ::
    List.map
      (fun p -> State.get p.Typed_parameter.name)
      Value_parameters.parameters_correctness

  module InitialState =
    State_builder.Option_ref
      (Bottom.Make_Datatype (Domain))
      (struct
        let name = "Value.Initialization" ^ "(" ^ string_of_int !counter ^ ")"
        let dependencies = correctness_deps
      end)
  let () = Ast.add_monotonic_state InitialState.self

  (* The computation depends on the lib_entry option, which is a corrrectness
     parameter of the analyzer: the InitialState memoization is thus safely
     cleaned when lib_entry changes. *)
  let global_state ~lib_entry =
    InitialState.memo (compute_global_state ~lib_entry)

  (* The global cvalue state may be supplied by the user. *)
  let supplied_state () =
    let cvalue_state = Db.Value.globals_state () in
    if Cvalue.Model.is_reachable cvalue_state
    then `Value (Domain.set Cvalue_domain.key cvalue_state Domain.top)
    else `Bottom

  let initial_state ~lib_entry =
    if Db.Value.globals_use_supplied_state ()
    then supplied_state ()
    else global_state ~lib_entry

  let initial_state_with_formals ~lib_entry kf =
    let init_state =
      if Db.Value.globals_use_supplied_state ()
      then begin
        Value_parameters.feedback "Initial state supplied by user";
        supplied_state ()
      end
      else begin
        Value_parameters.feedback "Computing initial state";
        let state = global_state ~lib_entry in
        Value_parameters.feedback "Initial state computed";
        state
      end
    in
    Domain.Store.register_global_state init_state;
    (* Prints the initial cvalue state. *)
    let cvalue_state = Cvalue_domain.extract Domain.get init_state in
    Value_parameters.printf ~dkey:Value_parameters.dkey_initial_state
      ~header:(fun fmt -> Format.pp_print_string fmt
                  "Values of globals at initialization")
      "@[  %a@]" Cvalue.Model.pretty cvalue_state;
    init_state >>-: add_main_formals kf

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
