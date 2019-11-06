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

type json = Json.t

(** All-in-one formatter. Return the JSON encoding of formatted text. *)
val format : ?indent:int -> ?margin:int ->
  ('a,Format.formatter,unit,json) format4 -> 'a

(** All-in-one formatter. Return the JSON encoding of formatted text. *)
val to_json : ?indent:int -> ?margin:int ->
  (Format.formatter -> 'a -> unit) -> 'a -> json

(** Buffer for encoding formatted text. *)
type buffer

(** Create a formatter with [~indent] maximum indentation and
    [~margin] right-margin. Defaults are those of [Format.make_formatter],
    which are [~indent:68] and [~margin:78] with OCaml 4.05. *)
val create : ?indent:int -> ?margin:int -> unit -> buffer

(** The underlying formatter of a buffer. *)
val formatter : buffer -> Format.formatter

(** Prints into the buffer's formatter. *)
val bprintf : buffer -> ('a,Format.formatter,unit) format -> 'a

val append : buffer -> string -> int -> int -> unit
val flush : buffer -> unit -> unit
val push_tag : buffer -> Format.tag -> unit
val pop_tag : buffer -> Format.tag -> unit

(** Flushes the buffer and returns its JSON enoding. This pops all pending
    tags. *)
val contents : buffer -> json

(** Prints back a JSON encoding onto the provided formatter.
    @raise Yojson.Basic.Util.Type_error in case of ill formatted buffer. *)
val fprintf : Format.formatter -> json -> unit

(* -------------------------------------------------------------------------- *)
