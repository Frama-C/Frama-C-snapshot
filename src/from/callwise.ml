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

open Cil_datatype


module Tbl =
  Cil_state_builder.Kinstr_hashtbl
    (Function_Froms)
    (struct
       let name = "Callwise dependencies"
       let size = 17
       let dependencies = [ Db.Value.self ]
     end)
let () = From_parameters.ForceCallDeps.set_output_dependencies [Tbl.self]

let merge_call_froms table callsite froms =
  try
    let current = Kinstr.Hashtbl.find table callsite in
    let new_froms = Function_Froms.join froms current in
    Kinstr.Hashtbl.replace table callsite new_froms
  with Not_found ->
    Kinstr.Hashtbl.add table callsite froms

let call_froms_stack = ref []

let record_callwise_dependencies_in_db call_site froms =
  try
    let previous = Tbl.find call_site in
    Tbl.replace call_site (Function_Froms.join previous froms)
  with Not_found -> Tbl.add call_site froms

let call_for_individual_froms (state, call_stack) =
  if From_parameters.ForceCallDeps.get () then begin
    let current_function, call_site = List.hd call_stack in
    if not (!Db.Value.use_spec_instead_of_definition current_function) then
      let table_for_current_function = Kinstr.Hashtbl.create 7 in
      call_froms_stack :=
        (current_function,table_for_current_function) :: !call_froms_stack
    else
      try
        let _above_function, table = List.hd !call_froms_stack in
        let froms =
          From_compute.compute_using_prototype_for_state
            state current_function
        in
        merge_call_froms table call_site froms;
        record_callwise_dependencies_in_db call_site froms;
      with Failure "hd" ->
        From_parameters.fatal "calldeps internal error 23 empty callfromsstack %a"
          Kernel_function.pretty current_function
  end

let end_record call_stack froms =
    let (current_function, call_site) = List.hd call_stack in
    record_callwise_dependencies_in_db call_site froms;
    (* pop + record in top of stack the froms of function that just finished *)
    match !call_froms_stack with
      | (current_function2, _) :: (((_caller, table) :: _) as tail) ->
          if current_function2 != current_function then
            From_parameters.fatal "calldeps %a != %a@."
              Kernel_function.pretty current_function (* g *)
              Kernel_function.pretty current_function2; (* f *)
          call_froms_stack := tail;
          merge_call_froms table call_site froms

    | _ ->  (* the entry point, probably *)
        Tbl.mark_as_computed ();
        call_froms_stack := []


module MemExec =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (Function_Froms)
    (struct
       let size = 17
       let dependencies = [Tbl.self]
       let name = "From.Callwise.MemExec"
     end)

let compute_call_from_value_states current_function states =
  let module Froms_To_Use =
      struct
        let get _f callsite =
          let _current_function, table = List.hd !call_froms_stack in
          try Kinstr.Hashtbl.find table callsite
          with Not_found -> raise From_compute.Call_did_not_take_place
      end
  in
  let module Values_To_Use = struct
    let get_stmt_state s =
      try Stmt.Hashtbl.find states s
      with Not_found -> Cvalue.Model.bottom

    let lval_to_zone_with_deps s ~deps ~for_writing lv =
      let state = get_stmt_state s in
      !Db.Value.lval_to_zone_with_deps_state state ~deps ~for_writing lv

    let expr_to_kernel_function kinstr ~deps exp =
      let state = get_stmt_state kinstr in
      !Db.Value.expr_to_kernel_function_state state ~deps exp

    let access_expr stmt expr =
      let state = get_stmt_state stmt in
      !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state expr
  end
  in
  let module Recording_To_Do =
      struct
        let accept_base_in_lmap kf base =
          let fundec = Kernel_function.get_definition kf in
          not (Base.is_formal_or_local base fundec)
        let final_cleanup _kf froms = froms
        let record_kf _kf _last_froms = ()
      end
  in
  let module Callwise_Froms =
      From_compute.Make(Values_To_Use)(Froms_To_Use)(Recording_To_Do)
  in
  Callwise_Froms.compute_and_return current_function


let record_for_individual_froms (call_stack, value_res) =
  if From_parameters.ForceCallDeps.get () then begin
    let froms = match value_res with
      | Value_types.Normal states | Value_types.NormalStore (states, _) ->
          let cur_kf, _ = List.hd call_stack in
          let froms =
            if !Db.Value.no_results (Kernel_function.get_definition cur_kf) then
              Function_Froms.top
            else
              compute_call_from_value_states cur_kf (Lazy.force states)
          in
          (match value_res with
             | Value_types.NormalStore (_, memexec_counter) ->
                 MemExec.replace memexec_counter froms
             | _ -> ());
          froms

      | Value_types.Reuse counter ->
          MemExec.find counter
    in
    end_record call_stack froms
    
  end


(* Register our callbacks inside the value analysis *)
let () = From_parameters.ForceCallDeps.add_update_hook
  (fun _bold bnew ->
    if bnew then begin
      Db.Value.Call_Value_Callbacks.extend_once call_for_individual_froms;
      Db.Value.Record_Value_Callbacks_New.extend_once
        record_for_individual_froms;
    end)


let force_compute_all_calldeps ()=
  if Db.Value.is_computed () then
    Project.clear
      ~selection:(State_selection.with_dependencies Db.Value.self) 
      ();
  !Db.Value.compute ()

(* Registration for call-wise from *)
let () =
  Db.register_guarded_compute
    "From.compute_all_calldeps"
    Tbl.is_computed
    Db.From.compute_all_calldeps
    force_compute_all_calldeps;
  Db.From.Callwise.iter := Tbl.iter;
  Db.From.Callwise.find := Tbl.find

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
