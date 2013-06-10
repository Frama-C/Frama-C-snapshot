(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
open Cil
open Cil_datatype
open Db
open Locations

module Functionwise_Pathdeps =
  Kernel_function.Make_Table
    (Zone)
    (struct
       let name = "Functionwise pathdeps"
       let size = 17
       let dependencies = [ Value.self ]
     end)

class do_pathdeps froms callwise_states_with_formals =
object(self)
  inherit Cil.nopCilVisitor
  val mutable inputs = Zone.bottom

  method result = inputs

  method join new_ =
    inputs <- Zone.join new_ inputs;

  method vstmt s =
    if Value.is_reachable
      (Value.get_stmt_state (Extlib.the self#current_stmt))
    then begin
      match s.skind with
      | UnspecifiedSequence seq ->
        List.iter
          (fun (stmt,_,_,_,_) ->
            ignore (visitCilStmt (self:>cilVisitor) stmt))
          seq;
        SkipChildren (* do not visit the additional lvals *)
      | If (_cond, _th, _el, _) ->
        DoChildren (* for _cond and for the statements in _th, _el *)
      | Loop _ | Block _ ->
        DoChildren (* for the statements *)
      | Switch _ ->
        DoChildren (* for the statements and the expression *)
      | Instr _ ->
        DoChildren (* for Calls *)
      | Return _ | Goto _ | Break _ | Continue _ ->
        SkipChildren
      | TryExcept _ | TryFinally _ -> assert false
    end
    else SkipChildren

  method stmt_froms =
    let stmt = Extlib.the (self#current_stmt) in
    Stmt.Hashtbl.find froms stmt

  method vlval lv =
    let deps,loc =
      !Value.lval_to_loc_with_deps
        ~with_alarms:CilE.warn_none_mode
        ~deps:Zone.bottom
        (Kstmt (Extlib.the self#current_stmt))
        lv
    in
    let bits_loc = enumerate_valid_bits ~for_writing:false loc in
    let all = Zone.join bits_loc deps in
    let froms = self#stmt_froms in
    let all_f = Lmap_bitwise.From_Model.find froms all in
    self#join all_f;
    (*    Format.printf "lval: all %a all_f %a@."
          Zone.pretty all
          Zone.pretty all_f; *)
    SkipChildren

  method vinst i =
    let current_stmt = Extlib.the self#current_stmt in
    if Value.is_reachable (Value.get_stmt_state current_stmt)
    then begin
      match i with
      | Call (_lv_opt,exp,_args,_) ->
        let current_stmt = Extlib.the self#current_stmt in

        let deps_callees, _callees =
          !Value.expr_to_kernel_function
            ~with_alarms:CilE.warn_none_mode
            ~deps:(Some Zone.bottom)
            (Kstmt current_stmt) exp
        in

        let states_with_formals =
          try Stmt.Hashtbl.find callwise_states_with_formals current_stmt
          with Not_found -> assert false
        in
        let all_f =
          List.fold_left
            (fun acc (kf, state_with_formals) ->
              if not (!Db.Value.use_spec_instead_of_definition kf)
              then
                let deps =
                  try
                    Functionwise_Pathdeps.find kf
                  with Not_found ->
                    Format.printf "pathdeps dependencies not found for %a@."
                      Kernel_function.pretty kf;
                    assert false
                in
                let deps_f = Lmap_bitwise.From_Model.find
                  state_with_formals
                  deps
                in
                Zone.join acc deps_f
              else begin
                Format.printf "Assuming library function %a has no path dependencies@."
                  Kernel_function.pretty kf;
                acc
              end)
            deps_callees
            states_with_formals
        in
        self#join all_f;
        SkipChildren
      | _ -> SkipChildren
    end
    else SkipChildren

  method vexpr exp =
    match exp.enode with
    | AddrOf lv | StartOf lv ->
      let deps,_loc =
        !Value.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
          ~deps:Zone.bottom
          (Kstmt (Extlib.the self#current_stmt))
          lv
      in
      let froms = self#stmt_froms in
      let deps_f = Lmap_bitwise.From_Model.find froms deps in
      self#join deps_f;
        (*      Format.printf "AddrOf: deps %a deps_f %a@."
                Zone.pretty deps
                Zone.pretty deps_f; *)
      SkipChildren
    | _ -> DoChildren

end


let compute_pathdeps (stack, froms, callwise_states_with_formals) =
  let kf = Stack.top stack in
  let name = Kernel_function.get_name kf in
  Format.printf "Computing path dependencies for function %s@." name;
  match kf.fundec with
    Definition (f, _) -> begin
      let computer = new do_pathdeps froms callwise_states_with_formals in
      ignore (visitCilFunction (computer:>cilVisitor) f);
      let result = computer#result in
      Format.printf "Path dependencies of %s: %a@."
        name
        Zone.pretty result;
      try
        ignore (Functionwise_Pathdeps.find kf);
        assert false
      with Not_found ->
        Functionwise_Pathdeps.add kf result
      end
  | Declaration _ ->
      assert false

let () =
  Cmdline.run_after_configuring_stage
    (fun () ->
      if From_parameters.PathDeps.get ()
      then Db.From.Record_From_Callbacks.extend_once compute_pathdeps)
