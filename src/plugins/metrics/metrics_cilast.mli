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

(** Visitor to compute various syntactic metrics.
    In particular, it fetches all necessary informations to compute
    cyclomatic complexity .
*)
class type sloc_visitor = object
  inherit Visitor.generic_frama_c_visitor

  (* Get the number of times a function has been called if it has been
     defined (fundef) or not (fundecl).
  *)
  method fundecl_calls: int Metrics_base.VInfoMap.t
  method fundef_calls: int Metrics_base.VInfoMap.t
  method extern_global_vars: Metrics_base.VInfoSet.t

  (* Get the computed metris *)
  method get_metrics: Metrics_base.BasicMetrics.t

  (* Print the metrics of a file [string] to a formatter
     Yields a fatal error if the file does not exist (or has no metrics).
  *)
  method pp_file_metrics: Format.formatter -> string -> unit

  method pp_detailed_text_metrics: Format.formatter -> unit
  (** Print results of all file and functions to the given formatter as text *)

  method print_stats: Format.formatter -> unit
(** Print computed metrics to a formatter *)
end

class slocVisitor: sloc_visitor ;;

val get_metrics : unit -> Metrics_base.BasicMetrics.t ;;

(** Compute metrics on whole CIL AST *)
val compute_on_cilast: unit -> unit ;;
