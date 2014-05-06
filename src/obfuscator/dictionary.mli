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

val fresh: Obfuscator_kind.t -> string -> string
(** Generate a fresh name of the given kind *)

val id_of_literal_string: string -> string
(** @return the generated name for a literal string.
    @raise Not_found if no name has already been generated. *)

val pretty_kind: Format.formatter -> Obfuscator_kind.t -> unit
val pretty: Format.formatter -> unit

val mark_as_computed: unit -> unit
val is_computed: unit -> bool

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
