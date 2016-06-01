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

type reachable_functions = {
  syntactic : Cil_datatype.Varinfo.Set.t;
  semantic : Cil_datatype.Varinfo.Set.t;
}
;;

val percent_coverage : reachable_functions -> float ;;

val compute : unit ->
  reachable_functions * (Cil_datatype.Varinfo.Hashtbl.key * Cil_types.init) list
;;

val compute_syntactic: Kernel_function.t -> Cil_datatype.Varinfo.Set.t
(** List of functions that can be syntactically reached from the function *)

val compute_semantic: unit -> Cil_datatype.Varinfo.Set.t
(** Functions analyzed by the value analysis *)

val compute_coverage_by_fun:  Cil_datatype.Varinfo.Set.t ->
  (Cil_types.kernel_function * int * int * float) list

val pp_reached_from_function: Format.formatter -> Kernel_function.t -> unit
(** Pretty-print the functions that can be syntactically reached from the
    parameter *)

val pp_value_coverage:
  unit -> (Format.formatter -> unit) * (Format.formatter -> unit)
(** Return two fonctions that pretty-print the coverage reached by the value
    analysis wrt. the functions syntactically reachable from main *)

val pp_stmts_reached_by_function: Format.formatter -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
