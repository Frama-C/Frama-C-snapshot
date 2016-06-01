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

(** Pretty-printer utilities.
    @plugin development guide *)

(* ********************************************************************** *)
(** {2 pretty-printing to a string} *)
(* ********************************************************************** *)

val sfprintf: ('a,Format.formatter,unit,string) format4 -> 'a
(** similar as Format.sprintf, but %a are allowed in the formatting string
    
    NB: Since 4.01, Format.asprintf provides the same feature. This function
    should be deprecated when OCaml >= 4.01.0 becomes mandatory.
 *)

val ksfprintf:
  (string -> 'b) -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** similar to Format.kfprintf, but the continuation is given the result
    string instead of a formatter.
    @since Magnesium-20151001
*)

val to_string: ?margin:int -> (Format.formatter -> 'a -> unit) -> 'a -> string
(** pretty-prints the supplied value into a string. [margin] is the
  maximal width of the box before a line-break is inserted.
  See {!Format.set_margin} *)

(** {2 separators} *)

val pp_print_string_fill : Format.formatter -> string -> unit
(** transforms every space in a string in breakable spaces.*)

val escape_underscores : string -> string

(* ********************************************************************** *)
(** {2 pretty printers for standard types} *)
(* ********************************************************************** *)

type sformat = (unit,Format.formatter,unit) Pervasives.format
type 'a formatter = Format.formatter -> 'a -> unit
type ('a,'b) formatter2 = Format.formatter -> 'a -> 'b -> unit

val pp_list: ?pre:sformat -> ?sep:sformat -> ?last:sformat -> ?suf:sformat ->
  'a formatter -> 'a list formatter
(** pretty prints a list. The optional arguments stands for
    - the prefix to output before a non-empty list (default: open a box)
    - the separator between two elements (default: nothing)
    - the last separator to be put just before the last element (default:sep)
    - the suffix to output after a non-empty list (default: close box) *)

val pp_array: ?pre:sformat -> ?sep:sformat -> ?suf:sformat ->
  (int,'a) formatter2 -> 'a array formatter
(** pretty prints an array. The optional arguments stands for
    - the prefix to output before a non-empty list (default: open a box)
    - the separator between two elements (default: nothing)
    - the suffix to output after a non-empty list (default: close box) *)

val pp_iter:
  ?pre:sformat -> ?sep:sformat -> ?suf:sformat ->
  (('a -> unit) -> 'b -> unit) ->
  'a formatter -> 'b formatter
(** pretty prints any structure using an iterator on it. The argument
    [pre] (resp. [suf]) is output before (resp. after) the iterator
    is started (resp. has ended). The optional argument [sep] is output bewteen
    two calls to the ['a formatter]. Default: open a box for [pre], close
    a box for [suf], nothing for [sep]. *)

val pp_iter2:
  ?pre:sformat -> ?sep:sformat -> ?suf:sformat -> ?between:sformat ->
  (('key -> 'v -> unit) -> 'a -> unit) ->
  'key formatter -> 'v formatter -> 'a formatter
(** pretty prints any map-like structure using an iterator on it. The argument
    [pre] (resp. [suf]) is output before (resp. after) the iterator
    is started (resp. has ended). The optional argument [sep] is output bewteen
    two calls to the ['a formatter]. The optional argument [between] is
    output between the key and the value. Default: open a box for [pre], close
    a box for [suf], nothing for [sep], break-space for [between]. *)

val pp_opt: ?pre:sformat -> ?suf:sformat -> 'a formatter -> 'a option formatter
(** pretty-prints an optional value. Prefix and suffix default to "@[" and "@]"
    respectively. Nothing is printed if the option is [None]. *)

val pp_cond: ?pr_false:sformat -> bool -> sformat formatter
(** [pp_cond cond f s]  pretty-prints [s] if cond is [true] and the optional
    pr_false, which defaults to nothing, otherwise *)

val pp_pair: ?pre:sformat -> ?sep:sformat -> ?suf:sformat ->
  'a formatter -> 'b formatter -> ('a * 'b)  formatter
(** [pp_pair ?pre ?sep ?suf pp_a pp_b (a,b)] pretty prints the pair [(a,b)],
    using the pretty printers [pp_a] and [pp_b], with optional
    prefix/separator/suffix, whose default values are:
    - pre: open a box
    - sep: print a comma character
    - suf: close a box.
    @since Magnesium-20151001 *)

val pp_flowlist: 
  ?left:sformat -> ?sep:sformat -> ?right:sformat -> 'a formatter -> 
  'a list formatter

val pp_blocklist: 
  ?left:sformat -> ?right:sformat -> 'a formatter -> 'a list formatter

val pp_open_block : Format.formatter -> ('a,Format.formatter,unit) format -> 'a
val pp_close_block : Format.formatter -> ('a,Format.formatter,unit) format -> 'a

val pp_trail : 'a formatter -> 'a formatter
(** pretty-prints its contents inside an '(** ... **)' horizontal block trailed
    with '*' *) 

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
