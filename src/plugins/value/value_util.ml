(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Callstacks related types and functions *)

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




let pretty_call_stack_short fmt callstack =
  Pretty_utils.pp_flowlist ~left:"" ~sep:" <- " ~right:""
     (fun fmt (kf,_) -> Kernel_function.pretty fmt kf)
    fmt
    callstack

let pretty_call_stack fmt callstack =
  Format.fprintf fmt "@[<hv>";
  List.iter (fun (kf,ki) ->
    Kernel_function.pretty fmt kf;
    match ki with
      | Kglobal -> ()
      | Kstmt stmt -> Format.fprintf fmt " :: %a <-@ "
          Cil_datatype.Location.pretty (Cil_datatype.Stmt.loc stmt)
  ) callstack;
  Format.fprintf fmt "@]"

let pp_callstack fmt =
  if Value_parameters.PrintCallstacks.get () then
    Format.fprintf fmt "@ stack: %a"
      pretty_call_stack (call_stack())
;;

(** Misc *)

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

(** Assertions emitted during the analysis *)

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

let with_alarms_raise_exn exn =
  let raise_exn () = raise exn in
  let stop = { CilE.a_log = false; CilE.a_call = raise_exn } in
  { CilE.imprecision_tracing = CilE.a_ignore;
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

let set_loc kinstr =
  match kinstr with
  | Kglobal -> Cil.CurrentLoc.clear ()
  | Kstmt s -> Cil.CurrentLoc.set (Cil_datatype.Stmt.loc s)

let pretty_actuals fmt actuals =
  let pp fmt (e,x,_) = Cvalue.V.pretty_typ (Some (Cil.typeOf e)) fmt x in
  Pretty_utils.pp_flowlist pp fmt actuals

let pretty_current_cfunction_name fmt =
  Kernel_function.pretty fmt (current_kf())

let warning_once_current fmt =
  Value_parameters.warning ~current:true ~once:true fmt

let debug_result kf (last_ret,_,last_clob) =
  Value_parameters.debug
    "@[RESULT FOR %a <-%a:@\n\\result -> %t@\nClobered set:%a@]"
    Kernel_function.pretty kf
    pretty_call_stack (call_stack ())
    (fun fmt ->
      match last_ret with
        | None -> ()
        | Some v -> Cvalue.V_Offsetmap.pretty fmt v)
    Base.SetLattice.pretty last_clob




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

(** Find if a postcondition contains [\result] *)
class postconditions_mention_result = object
  inherit Visitor.frama_c_inplace

  method! vterm_lhost = function
  | TResult _ -> raise Exit
  | _ -> Cil.DoChildren
end
let postconditions_mention_result spec =
  let vis = new postconditions_mention_result in
  let aux_bhv bhv =
    let aux (_, post) = ignore (Visitor.visitFramacIdPredicate vis post) in
    List.iter aux bhv.b_post_cond
  in
  try
    List.iter aux_bhv spec.spec_behavior;
    false
  with Exit -> true

let written_formals kf =
  let module S = Cil_datatype.Varinfo.Set in
  match kf.fundec with
  | Declaration _ -> []
  | Definition (fdec,  _) ->
    let add_addr_taken acc vi = if vi.vaddrof then S.add vi acc else acc in
    let referenced_formals =
      ref (List.fold_left add_addr_taken S.empty fdec.sformals)
    in
    let obj = object
      inherit Visitor.frama_c_inplace

      method! vinst i =
        begin match i with
        | Call (Some (Var vi, _), _, _, _)
        | Set ((Var vi, _), _, _) ->
          if Kernel_function.is_formal vi kf then
            referenced_formals := S.add vi !referenced_formals
        | _ -> ()
        end;
        Cil.SkipChildren
    end
    in
    ignore (Visitor.visitFramacFunction (obj :> Visitor.frama_c_visitor) fdec);
    S.elements !referenced_formals

module WrittenFormals =
  Kernel_function.Make_Table(Datatype.List(Cil_datatype.Varinfo))
    (struct
      let size = 17
      let dependencies = [Ast.self]
      let name = "Value_util.WrittenFormals"
     end)

let written_formals = WrittenFormals.memo written_formals


let bind_block_locals states b =
  (* Bind [vi] in [states] *)
  let bind_local_stateset states vi =
    let b = Base.of_varinfo vi in
    match Cvalue.Default_offsetmap.default_offsetmap b with
    | `Bottom -> states
    | `Map offsm ->
       (* Bind [vi] in [state], and does not modify the trace *)
       let bind_local_state (state, trace) =
         (Cvalue.Model.add_base b offsm state, trace)
       in
       State_set.map bind_local_state states
  in
  List.fold_left bind_local_stateset states b.blocals


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
