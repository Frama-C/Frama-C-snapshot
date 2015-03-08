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

(* ------------------------------------------------------------------------ *)
(** Bitvectors.
    @since Carbon-20101201
*)
(* ------------------------------------------------------------------------ *)

type t

val create : int -> t (** Create a vector of [n] bits, with all bits unset. *)
val create_set : int -> t (** Create a vector of [n] bits, with all bits set.*)

val capacity : t -> int 
(** Maximum number of bits in the bitvector. *)

val resize : int -> t -> t 
(** A copy of the bitvector up-to or down-to [n] bits. 
    Extra bits up to final bitvector capacity are set to zero. *)

val mem : t -> int -> bool
val set : t -> int -> unit
val clear : t -> int -> unit
val once : t -> int -> bool (** return [true] if unset, then set the bit. *)
val set_range : t -> int -> int -> unit
val is_empty : t -> bool

val equal: t -> t -> bool
val compare: t -> t -> int
val hash: t -> int

(** {2 Bitwise Binary Operations} 
    The first argument is the size of the vectors. *)

val bnot: int -> t -> t
val band: int -> t -> t -> t
val bor: int -> t -> t -> t
val bxor: int -> t -> t -> t        (* bitwise difference *)
val beq: int -> t -> t -> t         (* bitwise equivalence/equality *)

(** {2 Generic Bitwise Operations}. 
    Prefer using these rather than create intermediary bitvectors. *)

val bitwise_op2: int -> (int -> int -> int) -> t -> t -> t
val bitwise_op3: int -> (int -> int -> int -> int) -> t -> t -> t -> t
val bitwise_op4: int -> (int -> int -> int -> int -> int) -> t -> t -> t -> t -> t

(** {2 Sized Concatenation} *)

val concat: t -> int -> t -> int -> t
(** [concat b1 s1 b2 s2] concatenates 
    the [s1] first bits of [b1] with 
    the [s2] first bits of [b2]. *)

(** {2 Misc} *)

val iter_true : (int -> unit) -> t -> unit
  (** Iterates on all indexes of the bitvector with their bit set. *)

val fold_true : ('a -> int -> 'a) -> 'a -> t -> 'a
  (** Iterates on all indexes of the bitvector with their bit set. *)

val find_next_true: t -> int -> int
(** [find_next_true i a] returns the first index greater or equal to
    [i] with its bit set.  To find the first true element, call
    [find_next_true -1 a]. If no next true element exists, or [i] is
    larger than the array, then raise [Not_found]. *)

val pretty : Format.formatter -> t -> unit
  (** Bit vector, as blocs of 8-bits separated by space,
      first bits to last bits from left to right. *)

val pp_bits : Format.formatter -> int -> unit
  (** 0b... format, for bytes only, most significant bits on left. *)
