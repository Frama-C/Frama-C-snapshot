(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
  then Ival.Float_abstract.Any
  else Ival.Float_abstract.Nearest_Even

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

let warn_all_mode = CilE.warn_all_mode emitter pp_callstack

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
  let stop = { CilE.a_log = None; CilE.a_call = raise_exn } in
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
  let name = Kernel_function.get_name kf in
  Value_parameters.SlevelFunction.find name

let set_loc kinstr =
  match kinstr with
  | Kglobal -> Cil.CurrentLoc.clear ()
  | Kstmt s -> Cil.CurrentLoc.set (Cil_datatype.Stmt.loc s)

module Got_Imprecise_Value =
  State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Eval.Got_Imprecise_Value"
       let dependencies = [ Db.Value.self ]
       let default () = false
     end)

let pretty_actuals fmt actuals =
  Pretty_utils.pp_flowlist (fun fmt (_,x,_) -> Cvalue.V.pretty fmt x)
    fmt actuals

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


let map_outputs f =
  List.map
    (fun ((res: Cvalue.V_Offsetmap.t option), (out: Cvalue.Model.t)) -> (res, f out))


let remove_formals_from_state formals state =
  if formals <> [] then
    let formals = List.map Base.of_varinfo formals in
    let cleanup acc v = Cvalue.Model.remove_base v acc in
    List.fold_left cleanup state formals
  else state


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
  if Datatype.String.Set.mem "@all" params then
    not (Datatype.String.Set.mem ("-" ^ Kernel_function.get_name kf) params)
  else
    Datatype.String.Set.mem (Kernel_function.get_name kf) params

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
