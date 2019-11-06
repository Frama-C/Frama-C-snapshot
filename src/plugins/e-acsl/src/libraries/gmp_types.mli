(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

(** GMP Values. *)

open Cil_types

val init: unit -> unit
(** Must be called before any use of GMP *)

(**************************************************************************)
(******************************** Types ***********************************)
(**************************************************************************)

(** Signature of a GMP type *)
module type S = sig

  val t: unit -> typ
  (** @return the GMP type *)

  val t_as_ptr: unit -> typ
  (** type equivalent to [t] but seen as a pointer *)

  val is_now_referenced: unit -> unit
  (** Call this function when using this type for the first time. *)

  val is_t: typ -> bool
  (** @return true iff the given type is equivalent to the GMP type. *)

end

(** Representation of the unbounded integer type at runtime *)
module Z: S

(** Representation of the rational type at runtime *)
module Q: S
