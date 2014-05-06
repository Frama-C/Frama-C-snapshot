(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Tag functions handling html tags for Format *)
val html_tag_functions : Format.formatter_tag_functions;;

(** mk_hdr [level] [ppf] [hdr_strg] produces a title from [hdr_strg] with an
    underline of the same length.
    The character of the underline is set according to [level]:
    - level 1 headers are underlined by '='
    - level 2 headers by '-'
    - level 3 headers by '~'
    This function is supposed to follow reStructuredText's conventions.
*)
val mk_hdr : int -> Format.formatter -> string -> unit;;

module BasicMetrics : sig
  (** Simple type of metrics. *)
  type t = {
    cfile_name : string ;    (** Filename *)
    cfunc_name : string ;    (** Function name if applicable, eg. not for
                                 global metrics *)
    cslocs: int ;            (** Lines of code w.r.t. statements *)
    cifs: int ;              (** If / cases of switch *)
    cloops: int ;            (** Loops: for, while, do...while *)
    ccalls: int ;            (** Function calls *)
    cgotos: int ;            (** Gotos *)
    cassigns: int ;          (** Assignments *)
    cexits: int ;            (** Exit points: return *)
    cfuncs: int ;            (** Functions defined: 1 in the case of a single
                                 function, possibly more for a file.*)
    cptrs: int ;             (** Access to pointers *)
    cdecision_points: int ;  (** Decision points of the program: ifs,
                              switch cases, exception handlers, ... *)
    cglob_vars: int;         (** Global variables *)
  }


  (** Helpers for metrics purposes for single increment steps *)
  val incr_funcs : t -> t ;;
  val incr_slocs : t -> t ;;
  val incr_ptrs : t -> t ;;
  val incr_ifs : t -> t ;;
  val incr_dpoints : t -> t ;;
  val incr_loops : t -> t ;;
  val incr_gotos : t -> t ;;
  val incr_exits : t -> t ;;
  val incr_assigns : t -> t ;;
  val incr_calls : t -> t ;;
  val incr_glob_vars : t -> t ;;

  (** Update a reference from a pure functional function.
      Used in particular in combination with helper functions above.
  *)
  val apply_then_set : (t -> t) -> t ref -> unit ;;

  (** Initial empty values for metrics computing. *)
  val empty_metrics: t;;

  (** Compute cyclomatic complexity from base_metrics record type. *)
  val cyclo: t -> int;;

  (** Matrix-like representation of the record in "Title: value" stytle *)
  val to_list : t -> string list list ;;

  (** Pretty printers for base metrics as text or html. *)
  val pp_base_metrics: Format.formatter -> t -> unit;;
  val pp_base_metrics_as_html_row: Format.formatter -> t -> unit;;

end
;;

(** Local varinfo map where the comparison function is the lexicographic one on
    their respectives names.
*)
module VInfoMap: sig
  include FCMap.S with type key = Cil_types.varinfo

  val to_varinfo_map: 'a t -> 'a Cil_datatype.Varinfo.Map.t
end
;;

val map_cardinal_varinfomap: 'a Cil_datatype.Varinfo.Map.t -> int;;

(** Pretty print a varinfo set. *)
val pretty_set :
  ((Cil_types.varinfo -> int -> unit) -> 'a -> unit) ->
  Format.formatter -> 'a -> unit
;;

(** Handling entry points informations *)
val number_entry_points :
  ((Cil_types.varinfo -> int -> int -> int) -> 'a -> int -> 'b) -> 'a -> 'b
;;

val pretty_entry_points :
  ((Cil_types.varinfo -> int -> unit) -> 'a -> unit) -> Format.formatter ->
  'a -> unit
;;

(** Get the filename where the definition of a varinfo occurs *)
val file_of_vinfodef: Cil_types.varinfo -> string;;

(** Get the filename containing the function definition *)
val file_of_fundef: Cil_types.fundec -> string;;


val extract_fundef_name: Cabs.single_name -> string;;
val get_filename: Cabs.definition -> string;;

(** Type of the generated report file.
    Automatically set according to the file extension.
*)
type output_type =
  | Html
  | Text
;;

(** get_file_type [extension] sets the output type according to [extension].
    Raises an error if [extension] is not among supported extensions or is empty.
*)
val get_file_type: string -> output_type;;

(** consider_function [vinfo] returns false if the varinfo is not a function we
    are interested in.
    For example, builtins should not be part of the analysis and return false.
    Skip them using this auxiliary function.
*)
val consider_function: Cil_types.varinfo -> bool

(** Convert float to string with the following convention:
    - if the float is an integer (ie, it has no digits after the decimal point),
    print it as such;
    - otherwise, print the first two digits after the decimal point.
*)
val float_to_string : float -> string ;;
