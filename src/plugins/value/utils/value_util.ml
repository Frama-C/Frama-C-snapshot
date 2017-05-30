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

open Cil_types

(* Callstacks related types and functions *)

(* Function called, and calling instruction. *)
type call_site = (kernel_function * kinstr)
type callstack =  call_site list

let call_stack : callstack ref = ref []
(* let call_stack_for_callbacks : (kernel_function * kinstr) list ref = ref [] *)

let clear_call_stack () =
  call_stack := []

let pop_call_stack () =
  Value_perf.stop_doing !call_stack;
  call_stack := List.tl !call_stack
;;

let push_call_stack kf ki =
  call_stack := (kf,ki) :: !call_stack;
  Value_perf.start_doing !call_stack
;;


let current_kf () = 
  let (kf,_) = (List.hd !call_stack) in kf;;

let call_stack () = !call_stack

let pp_callstack fmt =
  if Value_parameters.PrintCallstacks.get () then
    Format.fprintf fmt "@ stack: %a"
      Value_types.Callstack.pretty (call_stack())
;;

(* Misc *)

let get_rounding_mode () =
  if Value_parameters.AllRoundingModes.get ()
  then Fval.Any
  else Fval.Nearest_Even

let stop_if_stop_at_first_alarm_mode () =
  if Stop_at_nth.incr()
  then begin
      Value_parameters.log "Stopping at nth alarm" ;
      raise Db.Value.Aborted
    end

(* Assertions emitted during the analysis *)

let emitter = 
  Emitter.create
    "Value"
    [ Emitter.Property_status; Emitter.Alarm ] 
    ~correctness:Value_parameters.parameters_correctness
    ~tuning:Value_parameters.parameters_tuning

let () = Db.Value.emitter := emitter

let warn_all_mode = CilE.warn_all_mode

let with_alarm_stop_at_first =
  let stop = 
    {warn_all_mode.CilE.others with CilE.a_call = stop_if_stop_at_first_alarm_mode}
  in
  {
    CilE.imprecision_tracing = CilE.a_ignore;
    defined_logic = stop;
    unspecified =   stop;
    others =        stop;
  }  

let warn_all_quiet_mode () =
  if Value_parameters.StopAtNthAlarm.get () <> max_int 
  then with_alarm_stop_at_first
  else
    if Value_parameters.verbose_atleast 1 then
      warn_all_mode
    else
      { warn_all_mode with CilE.imprecision_tracing = CilE.a_ignore }

let get_slevel kf =
  try Value_parameters.SlevelFunction.find kf
  with Not_found -> Value_parameters.SemanticUnrollingLevel.get ()

let pretty_actuals fmt actuals =
  let pp fmt (e,x,_) = Cvalue.V.pretty_typ (Some (Cil.typeOf e)) fmt x in
  Pretty_utils.pp_flowlist pp fmt actuals

let pretty_current_cfunction_name fmt =
  Kernel_function.pretty fmt (current_kf())

let warning_once_current fmt =
  Value_parameters.warning ~current:true ~once:true fmt

(* Emit alarms in "non-warning" mode *)
let alarm_report ?(level=1) ?current ?source ?emitwith ?echo ?once ?append =
  if Value_parameters.AlarmsWarnings.get () then
    Value_parameters.warning ?current ?source ?emitwith ?echo ?once ?append
  else
    Value_parameters.result ~dkey:Value_parameters.dkey_alarm
      ?current ?source ?emitwith ?echo ?once ?append ~level


module DegenerationPoints =
  Cil_state_builder.Stmt_hashtbl
    (Datatype.Bool)
    (struct
      let name = "Value_util.Degeneration"
      let size = 17
      let dependencies = [ Db.Value.self ]
    end)

let warn_indeterminate kf =
  let params = Value_parameters.WarnCopyIndeterminate.get () in
  Kernel_function.Set.mem kf params

let register_new_var v typ =
  if Cil.isFunctionType typ then
    Globals.Functions.replace_by_declaration (Cil.empty_funspec()) v v.vdecl
  else
    Globals.Vars.add_decl v

let create_new_var name typ =
  let vi = Cil.makeGlobalVar ~source:false ~temp:false name typ in
  register_new_var vi typ;
  vi

let is_const_write_invalid typ =
  Kernel.ConstReadonly.get () && Cil.typeHasQualifier "const" typ

let float_kind = function
  | FFloat -> Fval.Float32
  | FDouble -> Fval.Float64
  | FLongDouble ->
    if Cil.theMachine.Cil.theMachine.sizeof_longdouble <> 8 then
      Value_parameters.error ~once:true
        "type long double not implemented. Using double instead";
    Fval.Float64

(* Find if a postcondition contains [\result] *)
class postconditions_mention_result = object
  inherit Visitor.frama_c_inplace

  method! vterm_lhost = function
  | TResult _ -> raise Exit
  | _ -> Cil.DoChildren
end
let postconditions_mention_result spec =
  (* We save the current location because the visitor modifies it. *)
  let loc = Cil.CurrentLoc.get () in
  let vis = new postconditions_mention_result in
  let aux_bhv bhv =
    let aux (_, post) = ignore (Visitor.visitFramacIdPredicate vis post) in
    List.iter aux bhv.b_post_cond
  in
  let res =
    try
      List.iter aux_bhv spec.spec_behavior;
      false
    with Exit -> true
  in
  Cil.CurrentLoc.set loc;
  res

let conv_comp op =
  let module C = Abstract_interp.Comp in
  match op with
  | Eq -> C.Eq
  | Ne -> C.Ne
  | Le -> C.Le
  | Lt -> C.Lt
  | Ge -> C.Ge
  | Gt -> C.Gt
  | _ -> assert false

let conv_relation rel =
  let module C = Abstract_interp.Comp in
  match rel with
  | Req -> C.Eq
  | Rneq -> C.Ne
  | Rle -> C.Le
  | Rlt -> C.Lt
  | Rge -> C.Ge
  | Rgt -> C.Gt

let loc_dummy_value =
  let l = { Lexing.dummy_pos with Lexing.pos_fname = "_value_" } in
  l, l

let zero e =
  let loc = loc_dummy_value in
  match Cil.unrollType (Cil.typeOf e) with
  | TFloat (fk, _) -> Cil.new_exp ~loc (Const (CReal (0., fk, None)))
  | TEnum ({ekind = ik },_)
  | TInt (ik, _) -> Cil.new_exp ~loc (Const (CInt64 (Integer.zero, ik, None)))
  | TPtr _ ->
    let ik = Cil.(theMachine.upointKind) in
    Cil.new_exp ~loc (Const (CInt64 (Integer.zero, ik, None)))
  | typ -> Value_parameters.fatal ~current:true "non-scalar type %a"
             Printer.pp_typ typ

let is_value_zero e =
  e.eloc == loc_dummy_value

let dump_garbled_mix () =
  let l = Cvalue.V.get_garbled_mix () in
  if l <> [] && Value_parameters.(is_debug_key_enabled dkey_garbled_mix) then
    let pp_one fmt v = Format.fprintf fmt "@[<hov 2>%a@]" Cvalue.V.pretty v in
    Value_parameters.warning
      "Garbled mix generated during analysis:@.\
      @[<v>%a@]"
      (Pretty_utils.pp_list ~pre:"" ~suf:"" ~sep:"@ " pp_one) l

let rec zone_of_expr find_loc expr =
  let rec process expr = match expr.enode with
    | Lval lval -> zone_of_lval find_loc lval
    | UnOp (_, e, _) | CastE (_, e) | Info (e, _) -> process e
    | BinOp (_, e1, e2, _) -> Locations.Zone.join (process e1) (process e2)
    | StartOf lv | AddrOf lv -> zone_of_lval find_loc lv
    | _ -> Locations.Zone.bottom
  in
  process expr

and zone_of_lval find_loc lval =
  let loc = find_loc lval in
  let zone = Locations.enumerate_bits (Precise_locs.imprecise_location loc) in
  Locations.Zone.join zone
    (indirect_zone_of_lval find_loc lval)

and indirect_zone_of_lval find_loc (lhost, offset) =
  (Locations.Zone.join
     (zone_of_lhost find_loc lhost) (zone_of_offset find_loc offset))

and zone_of_lhost find_loc = function
  | Var _ -> Locations.Zone.bottom
  | Mem e -> zone_of_expr find_loc e

and zone_of_offset find_loc = function
  | NoOffset -> Locations.Zone.bottom
  | Field (_, o) -> zone_of_offset find_loc o
  | Index (e, o) ->
    Locations.Zone.join
      (zone_of_expr find_loc e) (zone_of_offset find_loc o)


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
