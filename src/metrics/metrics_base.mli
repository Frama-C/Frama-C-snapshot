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

module DatatypeMetrics: Datatype.S with type t = Db.Metrics.t

val html_tag_functions: Format.formatter_tag_functions;;
(** Tag functions handling html tags for Format *)

type my_metrics = {
  cfile_name : string; (* Filename *)
  cfunc_name : string; (* Function name if applicable, eg. not in the case of
                          global metrics
                       *)
  cslocs: int;         (* Lines of code w.r.t. statements *)
  cifs: int;           (* If / cases of switch *)
  cloops: int;         (* Loops: for, while, do...while *)
  ccalls: int;         (* Function calls *)
  cgotos: int;         (* Gotos *)
  cassigns: int;       (* Assignments *)
  cexits: int;         (* Exit points: return *)
  cfuncs: int;         (* Functions defined: 1 in the case of a single function,
                          possibly more in the case of a file
                       *)
  cptrs: int;           (* Access to pointers *)
  cdecision_points: int;           (* Decision points of the program: ifs,
                                      switch cases, exception handlers, ... *)
}
(** Storing metrics information *)

val empty_metrics: my_metrics;;
(** Initial empty values for metrics computing. *)

val cyclo: my_metrics -> int;;
(** Compute cyclomatic complexity from my_metrics record type. *)

val pp_my_metrics: Format.formatter -> my_metrics -> unit;;
val pp_metrics_as_html_row: Format.formatter -> my_metrics -> unit;;
(** Pretty printers for metrics as text or html. *)


module VInfoMap: sig
  include Map.S with type key = Cil_types.varinfo
(* This should be removed whenever 3.12 will be the oldest
   OCaml version used and replaced by Map.cardinal.
*)
  val map_cardinal: 'a t -> int;;
  (** Cardinal of a VInfoMap *)

  val to_varinfo_map: 'a t -> 'a Cil_datatype.Varinfo.Map.t
end
;;
(** Local varinfo map where the comparison function is the lexicographic one on
    their respectives names.*)

val map_cardinal_varinfomap: 'a Cil_datatype.Varinfo.Map.t -> int;;

val pretty_set :
  ((Cil_types.varinfo -> int -> unit) -> 'a -> 'b) ->
  Format.formatter -> 'a -> unit
;;
(** Pretty print a varinfo set *)

val number_entry_points :
  ((Cil_types.varinfo -> int -> int -> int) -> 'a -> int -> 'b) -> 'a -> 'b
;;

val pretty_entry_points :
  ((Cil_types.varinfo -> int -> unit) -> 'a -> unit) -> Format.formatter ->
  'a -> unit
;;

val pretty : Format.formatter -> DatatypeMetrics.t -> unit;;
(** Pretty print results *)

val file_of_vinfodef: Cil_types.varinfo -> string;;
(** Get the filename where the definition of a varinfo occurs *)

val file_of_fundef: Cil_types.fundec -> string;;
(** Get the filename containing the function definition *)

val extract_fundef_name: Cabs.single_name -> string;;
val get_filename: Cabs.definition -> string;;

type output_type =
  | Html
  | Text
;;
(** Type of the generated report file.
    Automatically set according to the file extension.
*)

val get_file_type: string -> output_type;;
(** get_file_type [extension] sets the output type according to [extension].
    Raise an error if [extension] is not among supported extensions or is empty.
*)

val consider_function: Cil_types.varinfo -> bool
(** consider_function [vinfo] returns false if the varinfo is not a function we
    are interested in.
    For example, builtins should not be part of the analysis and return false.
    Skip them using this auxiliary function.
*)
