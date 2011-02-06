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

(** {2 pretty-printing to a string} *)

(** similar as Format.sprintf, but %a are allowed in the formatting string*)
val sfprintf: ('a,Format.formatter,unit,string) format4 -> 'a

val to_string: (Format.formatter -> 'a -> unit) -> 'a -> string

(** {2 separators} *)

(** do nothing *)
val no_sep: (unit,Format.formatter,unit) format

(** a breakable space *)
val space_sep: (unit,Format.formatter,unit) format

(** forces a newline *)
val nl_sep: (unit,Format.formatter,unit) format

(** transforms every space in a string in breakable spaces.*)
val pp_print_string_fill : Format.formatter -> string -> unit

(** opens a new formatting box. *)
val open_box: (unit,Format.formatter,unit) format

(** close a formatting box. *)
val close_box: (unit,Format.formatter,unit) format

val escape_underscores : string -> string

(** {2 pretty printers for standard types} *)

type sformat = (unit,Format.formatter,unit) Pervasives.format
type 'a formatter = Format.formatter -> 'a -> unit
type ('a,'b) formatter2 = Format.formatter -> 'a -> 'b -> unit

(** pretty prints a list. The optional arguments stands for
- the prefix to output before a non-empty list (default: open a box)
- the separator between two elements (default: nothing)
- the suffix to output after a non-empty list (default: close box)
*)
val pp_list: ?pre:sformat -> ?sep:sformat -> ?suf:sformat ->
  'a formatter -> 'a list formatter

(** pretty prints an array. The optional arguments stands for
- the prefix to output before a non-empty list (default: open a box)
- the separator between two elements (default: nothing)
- the suffix to output after a non-empty list (default: close box)
*)
val pp_array: ?pre:sformat -> ?sep:sformat -> ?suf:sformat ->
  (int,'a) formatter2 -> 'a array formatter

(** pretty prints any structure using an iterator on it. The argument
    [pre] (resp. [suf]) is output before (resp. after) the iterator
    is started (resp. has ended). The optional argument [sep] is output bewteen
    two calls to the ['a formatter]. Default: open a box for [pre], close
    a box for [suf], nothing for [sep]
*)
val pp_iter:
  ?pre:sformat -> ?sep:sformat -> ?suf:sformat ->
  (('a -> unit) -> 'b -> unit) ->
  'a formatter -> 'b formatter



(** pretty-prints an optional value. Prefix and suffix default to nothing.
    Nothing is printed if the option is None.
*)
val pp_opt: ?pre:sformat -> ?suf:sformat -> 'a formatter -> 'a option formatter

(** pp_cond cond f s  pretty-prints s if cond is true and the optional
    pr_false, which defaults to nothing, otherwise *)
val pp_cond: ?pr_false:sformat -> bool -> sformat formatter



val pp_flowlist : ?left:string -> ?sep:string -> ?right:string -> 'a formatter -> 'a list formatter
val pp_blocklist : ?left:string -> ?right:string -> 'a formatter -> 'a list formatter

val pp_open_block : Format.formatter -> ('a,Format.formatter,unit) format -> 'a
val pp_close_block : Format.formatter -> ('a,Format.formatter,unit) format -> 'a

(** pretty-prints its contents inside an '(** ... **)' horizontal block trailed with '*' *)
val pp_trail : 'a formatter -> 'a formatter

(*
Local Variables:
compile-command: "make -C ../.. -j"
End:
*)
