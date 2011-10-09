(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Metrics_base
open Metrics_parameters

let () = Enabled.set_output_dependencies
  [Ast.self; AST_type.self; OutputFile.self; SyntacticallyReachable.self]

let syntactic () =
  begin
    match AST_type.get () with
      | "cil" ->
        let r = !Db.Metrics.compute () in
        Metrics.result
          "@[<v 0>Syntactic metrics@ \
                  -----------------@ %a@]"
          !Db.Metrics.pretty r

      (* Cabs metrics are experimental. unregistered, unjournalized *)
      | "cabs" -> Metrics_cabs.compute_on_cabs ()

      | _ -> assert false (* the possible values are checked by the kernel*)
  end;

  SyntacticallyReachable.iter
    (fun s ->
      try let kf = Globals.Functions.find_by_name s in
          Metrics.result "%a" Metrics_coverage.pp_reached_from_function kf
      with Not_found -> Metrics.error "Unknown function %s" s
    );
;;


let () = ValueCoverage.set_output_dependencies [Db.Value.self]

let value () =
  !Db.Value.compute ();
  if Db.Value.is_computed () then begin
    let f1, f2 = Metrics_coverage.pp_value_coverage () in
    Metrics.result "%t" f1;
    Metrics.result "%t" f2;
    Metrics.result "%t" Metrics_coverage.pp_stmts_reached_by_function;
  end
;;

let main () =
  if Enabled.get () then Enabled.output syntactic;
  if ValueCoverage.get () then ValueCoverage.output value;
;;


(* Main entry points *)
let () = Db.Main.extend main

(* Register some functions in Frama-C's DB *)
let () =
   Db.register
    (Db.Journalize
       ("Metrics.compute", Datatype.func Datatype.unit DatatypeMetrics.ty))
    Db.Metrics.compute Metrics_cilast.compute_on_cilast;

  Db.register
    (Db.Journalize
       ("Metrics.pretty", Datatype.func Datatype.formatter
         (Datatype.func DatatypeMetrics.ty  Datatype.unit)))
    Db.Metrics.pretty pretty;
;;
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
