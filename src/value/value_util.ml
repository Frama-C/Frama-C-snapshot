(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

type called_function =
    { called_kf : kernel_function;
      call_site : kinstr;
      mutable called_merge_current : degenerate:bool -> unit}

let call_stack : called_function list ref = ref []
let call_stack_for_callbacks : (kernel_function * kinstr) list ref = ref []

let clear_call_stack () =
  call_stack := [];
  call_stack_for_callbacks := []

let pop_call_stack () =
  call_stack := List.tl !call_stack;
  call_stack_for_callbacks := List.tl !call_stack_for_callbacks

let push_call_stack cf =
  call_stack := cf :: !call_stack;
  call_stack_for_callbacks :=
    (cf.called_kf, cf.call_site) :: !call_stack_for_callbacks

let push_call_stack kf ki =
  let cf = { called_kf = kf; call_site = ki;
             called_merge_current = fun ~degenerate:_ -> () }
  in
  push_call_stack cf

let call_stack_set_merge_current mc =
  (List.hd !call_stack).called_merge_current <- mc

let current_kf () = (List.hd !call_stack).called_kf

let call_stack () = !call_stack
let for_callbacks_stack () = !call_stack_for_callbacks

let pretty_call_stack fmt callstack =
  Pretty_utils.pp_flowlist ~left:"" ~sep:" <- " ~right:""
     (fun fmt {called_kf = kf} -> Kernel_function.pretty fmt kf)
    fmt
    callstack

let pretty_callbacks_call_stack fmt callstack =
  Format.fprintf fmt "@[<hv>";
  List.iter (fun (kf, ki) ->
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
      pretty_callbacks_call_stack !call_stack_for_callbacks
;;

(** Misc *)

let get_rounding_mode () =
  if Value_parameters.AllRoundingModes.get ()
  then Ival.Float_abstract.Any
  else Ival.Float_abstract.Nearest_Even

let do_degenerate lv =
  List.iter
    (fun {called_merge_current = merge_current } ->
      merge_current ~degenerate:true)
    (call_stack ());
  !Db.Value.degeneration_occurred (CilE.current_stmt ()) lv

(** Assertions emitted during the analysis *)

let emitter_value = 
  Emitter.create
    "value analysis"
    [ Emitter.Property_status; Emitter.Code_annot ] 
    ~correctness:Value_parameters.parameters_correctness
    ~tuning:Value_parameters.parameters_tuning

let warn_all_mode = CilE.warn_all_mode emitter_value pp_callstack

let stop_at_first_alarm () =
  exit 0 (* TODO: same mechanism as do_degenerate *)

let stop_if_stop_at_first_alarm_mode () =
  if Value_parameters.StopAtFirstAlarm.get ()
  then stop_at_first_alarm ()

let with_alarm_stop_at_first =
  let stop = CilE.Acall stop_at_first_alarm in {
    CilE.imprecision_tracing = CilE.Aignore;
    defined_logic = stop;
    unspecified =   stop;
    others =        stop;
}

let warn_all_quiet_mode () =
  if Value_parameters.StopAtFirstAlarm.get () then with_alarm_stop_at_first
  else
    if Value_parameters.verbose_atleast 1 then
      warn_all_mode
    else
      { warn_all_mode with CilE.imprecision_tracing = CilE.Aignore }

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


(* Cached versions of [Stmts_graph.stmt_can_reach] *)

module StmtCanReachCache =
  Kernel_function.Make_Table
    (Datatype.Function
       (struct include Cil_datatype.Stmt let label = None end)
       (Datatype.Function
          (struct include Cil_datatype.Stmt let label = None end)
          (Datatype.Bool)))
    (struct
      let name = "Eval_funs.StmtCanReachCache"
      let size = 17
      let dependencies = [ Ast.self ]
     end)

let stmt_can_reach_memo = StmtCanReachCache.memo Stmts_graph.stmt_can_reach
let stmt_can_reach kf =
  if Value_parameters.MemoryFootprint.get () >= 3
  then stmt_can_reach_memo kf
  else Stmts_graph.stmt_can_reach kf



let debug_result kf (last_ret,_,last_clob) =
  Value_parameters.debug
    "@[RESULT FOR %a <-%a:@\n\\result -> %t@\nClobered set:%a@]"
    Kernel_function.pretty kf
    pretty_call_stack (call_stack ())
    (fun fmt ->
      match last_ret with
        | None -> ()
        | Some v -> Cvalue.V_Offsetmap.pretty fmt v)
    Locations.Location_Bits.Top_Param.pretty last_clob


let map_outputs f =
  List.map
    (fun ((res: Cvalue.V_Offsetmap.t option), (out: Cvalue.Model.t)) -> (res, f out))


let remove_formals_from_state formals state =
  if formals != [] then
    let formals = List.map Base.create_varinfo formals in
    let cleanup acc v = Cvalue.Model.remove_base v acc in
    List.fold_left cleanup state formals
  else state


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
