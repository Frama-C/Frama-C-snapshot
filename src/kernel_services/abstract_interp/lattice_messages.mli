(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Message and logging facility for abstract lattices. *)

type t =
  | Approximation of string
    (** Abstract transfer function that intentionally approximates its result *)
  | Imprecision of string
    (** Abstract transfer function not fully implemented *)
  | Costly of string
    (** Abstract operation will be costly *)
  | Unsoundness of string
    (** Unsound abstract operation *)

type emitter

(** Register a new emitter for a message. *)
val register: string -> emitter;;

val emitter_name: emitter -> string

(** Emit a message. *)
val emit: emitter -> t -> unit
val emit_imprecision: emitter -> string -> unit
val emit_approximation: emitter -> ('a, Format.formatter, unit) format -> 'a
val emit_costly: emitter -> ('a, Format.formatter, unit) format -> 'a

(**/**)
(* Internal; defines where emitted messages go.  *)
val message_destination:(emitter -> t -> unit) ref;;

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
