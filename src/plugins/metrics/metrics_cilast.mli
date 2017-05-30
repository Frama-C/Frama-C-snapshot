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

(** In the definitions below, setting argument [libc] to [true] will
    include functions/variables from the C stdlib in the metrics. *)

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

  (* Get the computed metrics *)
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

class slocVisitor : libc:bool -> sloc_visitor ;;

val get_metrics : libc:bool -> Metrics_base.BasicMetrics.t ;;

type cilast_metrics = {
  fundecl_calls: int Metrics_base.VInfoMap.t;
  fundef_calls: int Metrics_base.VInfoMap.t;
  extern_global_vars: Metrics_base.VInfoSet.t;
  basic_metrics: Metrics_base.BasicMetrics.t
} ;;

val get_cilast_metrics : libc:bool -> cilast_metrics ;;

(** Compute metrics on whole CIL AST *)
val compute_on_cilast: libc:bool -> unit ;;

(** Compute and print the size (in bytes) of local variables on the CIL AST.
    This is a rough approximation, neither guaranteed to be smaller or
    larger than the actual value. Only automatic, non-ghost and
    non-temporary variables present in the source are included.
    This is useful to estimate the stack size of a function. *)
val compute_locals_size: Kernel_function.t -> unit;;
