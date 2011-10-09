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

(* ------------------------------------------------------------------------ *)
(** Bitvector naive implementation.
    @since Carbon-20101201
*)
(* ------------------------------------------------------------------------ *)

type t

val create : int -> t (** A vector of [n] bits *)

val mem : t -> int -> bool
val set : t -> int -> unit
val clear : t -> int -> unit
val set_range : t -> int -> int -> unit
val is_empty : t -> bool

val pretty : Format.formatter -> t -> unit
  (** Bit vector, as blocs of 8-bits separated by space,
      first bits to last bits from left to right. *)

val pp_bits : Format.formatter -> int -> unit
  (** 0b... format, for bytes only, most significant bits on left. *)
