(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* -------------------------------------------------------------------------- *)
(** Text with Tags *)
(* -------------------------------------------------------------------------- *)

type message (** Message with tags *)

val size : message -> int
val char_at : message -> int -> char
val string : message -> string
val substring : message -> int -> int -> string

val tags_at : message -> int -> (Transitioning.Format.stag * int * int) list
(** Returns the list of tags at the given position.
    Inner tags come first, outer tags last. *)

val visit :
  ?output:(string -> int -> int -> unit) ->
  ?open_tag:(Transitioning.Format.stag -> int -> int -> unit) ->
  ?close_tag:(Transitioning.Format.stag -> int -> int -> unit) ->
  message -> unit
(** Visit the message, with depth-first recursion on tags.
    All methods are called with text or tag, position and length. *)

val pretty : ?vbox:int -> Format.formatter -> message -> unit
(** Pretty-print the message onto the given formatter, with the tags.
    The original message has been {i already} laidout with respect to
    horizontal and vertical boxes, and this layout will be output as-it-is
    into the formatter.

    Here, you have two different strategies to render the message properly.
    If [~vbox] is specified, a vertical box is opened around the message,
    and newlines are emitted with a ["@\n"] and the given indentation.
    Otherwise, no box is used and newlines are emitted as
    ["\n"], which only makes sense if there is no current indentation in the
    output formatter. *)

(* -------------------------------------------------------------------------- *)
(** Message Buffer  *)
(* -------------------------------------------------------------------------- *)

(** Buffer for creating messages.

    The buffer grows on demand, but is protected against huge mesages.
    Maximal size is around 2 billions ASCII characters, which sould be enough
    to store more than 25kloc source text. *)
type buffer

(** Create a buffer.

    The right-margin is set to [~margin] and
    maximum indentation to [~indent].
    Default values are those of [Format.make_formatter], which are
    [~indent:68] and [~margin:78] in OCaml 4.05.
*)
val create : ?indent:int -> ?margin:int -> unit -> buffer

val message : buffer -> message
(** Buffer contents, with its formatting tags. *)

val add_char : buffer -> char -> unit (** Buffer-like *)
val add_string : buffer -> string -> unit (** Buffer-like *)
val add_substring : buffer -> string -> int -> int -> unit (** Buffer-like *)

val formatter : buffer -> Format.formatter
val bprintf : buffer -> ('a,Format.formatter,unit) format -> 'a
val kprintf :
  (Format.formatter -> 'a) ->
  buffer -> ('b,Format.formatter,unit,'a) format4 -> 'b

(** Similar to [Buffer.contents] *)
val contents : buffer -> string

(** Similar to [Buffer.sub] *)
val sub : buffer -> int -> int -> string

(** Sub-string with range. [range b p q] is [sub b p (q+1-p)] *)
val range : buffer -> int -> int -> string

(** Range of non-blank leading and trailing characters. *)
val trim : buffer -> int * int

(** Resize the buffer to roughly fit its actual content. *)
val shrink : buffer -> unit

(* -------------------------------------------------------------------------- *)
