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

open Metrics_parameters
;;

let () = Enabled.set_output_dependencies
  [ Ast.self; AstType.self; OutputFile.self; SyntacticallyReachable.self;
    Libc.self ]
;;

let syntactic ?(libc=Metrics_parameters.Libc.get ()) () =
  begin
    match AstType.get () with
      | "cil" -> Metrics_cilast.compute_on_cilast ~libc
      (* Cabs metrics are experimental. unregistered, unjournalized *)
      | "cabs" -> Metrics_cabs.compute_on_cabs ()
      | "acsl" -> Metrics_acsl.dump()
      | _ -> assert false (* the possible values are checked by the kernel*)
  end;
  SyntacticallyReachable.iter
    (fun kf ->
       let reachable = Metrics_coverage.compute_syntactic ~libc kf in
       let cov_printer = new Metrics_coverage.syntactic_printer ~libc reachable in
       Metrics_parameters.result "%a"
         cov_printer#pp_reached_from_function kf)

let () = ValueCoverage.set_output_dependencies [Db.Value.self; Libc.self]

let value ~libc () =
  !Db.Value.compute ();
  if Db.Value.is_computed () then begin
    let cov_metrics = Metrics_coverage.compute ~libc in
    let cov_printer = new Metrics_coverage.semantic_printer ~libc cov_metrics in
    Metrics_parameters.result "%t" cov_printer#pp_value_coverage;
    Metrics_parameters.result "%t" cov_printer#pp_unreached_calls;
    Metrics_parameters.result "%t" cov_printer#pp_stmts_reached_by_function;
  end
;;

let main () =
  let libc = Libc.get () in
  if Enabled.get () then Enabled.output (syntactic ~libc);
  if ValueCoverage.get () then ValueCoverage.output (value ~libc);
  if LocalsSize.is_set () then begin
    Ast.compute ();
    Metrics_parameters.result "function\tlocals_size_no_temps\t\
                               locals_size_with_temps\t\
                               max_call_size_no_temps\t\
                               max_call_size_with_temps";
    LocalsSize.iter (fun kf -> Metrics_cilast.compute_locals_size kf)
  end
;;

(* Register main entry points *)
let () = Db.Main.extend main


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
