(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
;;

(* Register main entry points *)
let () = Db.Main.extend main


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
