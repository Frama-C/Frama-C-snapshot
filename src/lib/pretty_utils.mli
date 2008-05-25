(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, either version 3 of the License, or (at your option)      *)
(*  any later version.                                                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public Licence version 3 for more details  *)
(*  (enclosed in the file licences/LGPLv3).                               *)
(*                                                                        *)
(**************************************************************************)

(* $Id: pretty_utils.mli,v 1.2 2008/01/18 17:02:25 uid562 Exp $ *)

(** {2 pretty-printing to a string} *)

(** similar as Format.sprintf, but %a are allowed in the formatting string*)
val sfprintf: ('a,Format.formatter,unit,string) format4 -> 'a

(** {2 separators} *)

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

(** pretty prints a list. The optional arguments stands for
- the prefix to output before a non-empty list (default: open a box)
- the separator between two elements (default: nothing)
- the suffix to output after a non-empty list (default: close box)
*)
val pp_list:
?pre:(unit,Format.formatter,unit) format ->
?sep:(unit,Format.formatter,unit) format ->
?suf:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

(** pretty-prints an optional value. Prefix and suffix default to nothing.
    Nothing is printed if the option is None.
*)
val pp_opt:
  ?pre:(unit,Format.formatter,unit) format ->
  ?suf:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->  Format.formatter -> 'a option -> unit

(** pp_cond cond f s  pretty-prints s if cond is true and the optional
    pr_false, which defaults to nothing, otherwise *)
val pp_cond: ?pr_false:(unit,Format.formatter,unit) format -> bool ->
  Format.formatter -> (unit,Format.formatter,unit) format -> unit

(*
Local Variables:
compile-command: "make -C ../.. -j"
End:
*)
