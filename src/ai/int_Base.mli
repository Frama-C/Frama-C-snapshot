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

(** Big integers with an additional top element. *)

type i = Top | Value of Integer.t

include Datatype.S with type t = i

val zero: t
val one: t
val minus_one: t
val top: t
val neg: t -> t

val is_zero: t -> bool
val is_top: t -> bool

exception Error_Top

val inject: Integer.t -> t
val project: t -> Integer.t
  (** @raise Error_Top if the argument is {!Top}. *)

val cardinal_zero_or_one: t -> bool

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
