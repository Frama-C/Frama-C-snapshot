(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
compile-command: "make -C ../../.."
End:
*)
