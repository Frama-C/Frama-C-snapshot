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

open Metrics_parameters
;;

let () = Enabled.set_output_dependencies
  [ Ast.self; AstType.self; OutputFile.self; SyntacticallyReachable.self;
    Libc.self ]
;;

let syntactic () =
  begin
    match AstType.get () with
      | "cil" -> Metrics_cilast.compute_on_cilast ()
      (* Cabs metrics are experimental. unregistered, unjournalized *)
      | "cabs" -> Metrics_cabs.compute_on_cabs ()
      | "acsl" -> Metrics_acsl.dump()
      | _ -> assert false (* the possible values are checked by the kernel*)
  end;

  SyntacticallyReachable.iter
    (fun kf ->
      Metrics_parameters.result "%a"
        Metrics_coverage.pp_reached_from_function kf)

let () = ValueCoverage.set_output_dependencies [Db.Value.self; Libc.self]

let value () =
  !Db.Value.compute ();
  if Db.Value.is_computed () then begin
    let f1, f2 = Metrics_coverage.pp_value_coverage () in
    Metrics_parameters.result "%t" f1;
    Metrics_parameters.result "%t" f2;
    Metrics_parameters.result "%t" 
      Metrics_coverage.pp_stmts_reached_by_function;
  end
;;

let main () =
  if Enabled.get () then Enabled.output syntactic;
  if ValueCoverage.get () then ValueCoverage.output value;
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
