(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
  method get_global_metrics: Metrics_base.BasicMetrics.t

  (* Print the metrics of a file [string] to a formatter
     Yields a fatal error if the file does not exist (or has no metrics).
  *)
  method pp_file_metrics: Format.formatter -> Datatype.Filepath.t -> unit

  method pp_detailed_text_metrics: Format.formatter -> unit
  (** Print results of all file and functions to the given formatter as text *)

  method print_stats: Format.formatter -> unit
  (** Print computed metrics to a formatter *)

  method get_metrics_map:
    (Metrics_base.BasicMetrics.t Metrics_base.OptionKf.Map.t)
      Datatype.Filepath.Map.t
  (** Compute and return per-function metrics *)
end

class slocVisitor : libc:bool -> sloc_visitor ;;

(** Returns the computed metrics for the entire AST. *)
val get_global_metrics : libc:bool -> Metrics_base.BasicMetrics.t ;;

type cilast_metrics = {
  fundecl_calls: int Metrics_base.VInfoMap.t;
  fundef_calls: int Metrics_base.VInfoMap.t;
  extern_global_vars: Metrics_base.VInfoSet.t;
  basic_global_metrics: Metrics_base.BasicMetrics.t
} ;;

val get_cilast_metrics : libc:bool -> cilast_metrics ;;

(** Computes and returns individual metrics per function. *)
val get_metrics_map : libc:bool ->
  (Metrics_base.BasicMetrics.t Metrics_base.OptionKf.Map.t)
    Datatype.Filepath.Map.t

(** Compute metrics on whole CIL AST *)
val compute_on_cilast: libc:bool -> unit ;;

(** Compute and print the size (in bytes) of local variables on the CIL AST.
    This is a rough approximation, neither guaranteed to be smaller or
    larger than the actual value. Only automatic, non-ghost and
    non-temporary variables present in the source are included.
    This is useful to estimate the stack size of a function. *)
val compute_locals_size: Kernel_function.t -> unit;;

(** Computes the set of global variables which are syntactically reachable
    from the entry point of the program.
    Returns [None] if there is no entry point. *)
val reachable_from_main: unit -> Cil_types.varinfo list option;;

(** Computes the set of files defining all global variables syntactically 
    reachable from the entry point of the program
    (as given by [reachable_from_main]).
    This function requires a defined entry point.
 *)
val used_files: unit -> Datatype.Filepath.Set.t

(** Pretty-prints the result of [used_files] in a verbose way. *)
val pretty_used_files: Datatype.Filepath.Set.t -> unit
