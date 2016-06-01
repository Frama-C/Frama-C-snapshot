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

open Cil_types

module Self = Plugin.Register
    (struct
      let name = "nonterm"
      let shortname = "nonterm"
      let help =
        "Warns when definitively non-terminating functions/loops are \
         detected (e.g. reachable functions with unreachable returns)."
    end)

module Enabled =
  Self.WithOutput
    (struct
      let option_name = "-nonterm"
      let help = "when on (off by default),\
                  warns about non-terminating functions/loops"
      let output_by_default = false
    end)

let warn_unreachable_return kf stmt =
  Self.warning ~source:(fst (Cil_datatype.Stmt.loc stmt))
    "unreachable return statement for function %a"
    Kernel_function.pretty kf

let warn_nonterminating_instruction kf stmt =
  Self.warning ~source:(fst (Cil_datatype.Stmt.loc stmt))
    "non-terminating instruction in function %a:@ %a"
    Kernel_function.pretty kf Cil_printer.pp_stmt stmt

(* checks for functions containing unreachable returns *)
let check_unreachable_returns kf =
  if !Db.Value.use_spec_instead_of_definition kf then
    (* TODO: consider as non-terminating if spec has
       \terminates(false) or \ensures(false) *)
    Self.debug "not analyzing function %a@ \
                (using specification instead of definition),@ \
                considered as always terminating"
      Kernel_function.pretty kf
  else
    let st = Db.Value.get_initial_state kf in
    if Db.Value.is_reachable st then begin
      try
        let ret_stmt = Kernel_function.find_return kf in
        if not (Db.Value.is_reachable_stmt ret_stmt) then
          warn_unreachable_return kf ret_stmt
      with
      | Kernel_function.No_Statement -> (* should never happen *)
        Self.error "function %a has no return statement, skipping"
          Kernel_function.pretty kf;
    end

(* simple instruction collector: accumulates a list of all
   statements containing instructions *)
class instr_stmt_collector = object
  inherit Visitor.frama_c_inplace
  val instr_stmts = ref []
  method! vstmt stmt =
    begin
      match stmt.skind with
      | Instr _ -> instr_stmts := stmt :: !instr_stmts
      | _ -> ()
    end;
    Cil.DoChildren
  method get_instr_stmts = List.rev !instr_stmts
end

(* checks for non-terminating instructions *)
let check_nonterminating_instructions fd kf =
  let vis = new instr_stmt_collector in
  ignore (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) fd);
  List.iter (fun stmt ->
      Self.debug "%a: processing stmt:@ %a"
        Printer.pp_location (Cil_datatype.Stmt.loc stmt) Printer.pp_stmt stmt;
      let reach_before =
        Db.Value.fold_stmt_state_callstack
          (fun state acc ->
             acc || Db.Value.is_reachable state) false ~after:false stmt
      in
      let reach_after =
        Db.Value.fold_stmt_state_callstack
          (fun state acc ->
             acc || Db.Value.is_reachable state) false ~after:true stmt
      in
      if reach_before && not reach_after then
        warn_nonterminating_instruction kf stmt
    ) vis#get_instr_stmts
;;

let run () =
  if not (Ast.is_computed ()) then
    Self.abort "nonterm requires a computed AST";
  if not (Db.Value.is_computed ()) then
    Self.abort "nonterm requires a computed value analysis";
  Self.debug "Starting analysis...";
  let file = Ast.get () in
  let globals = file.globals in
  List.iter (fun glob ->
      match glob with
      | GFun (fd, _loc) ->
        let kf = Globals.Functions.get fd.svar in
        Self.debug "considering function: %a" Kernel_function.pretty kf;
        check_unreachable_returns kf;
        check_nonterminating_instructions fd kf
      | _ -> ()
    ) globals;
  Self.feedback ~level:2 "Analysis done."
;;

let run_once, _ = State_builder.apply_once "Nonterm.run" [Db.Value.self] run

let main () =
  if Enabled.get () then run_once ()

let () =
  Db.Main.extend main
