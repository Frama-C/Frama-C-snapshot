(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Numerors_utils

(** Opaque type of an interval. The type as an invariant : both bounds of
    the interval use the same precision *)
type t

val pretty : Format.formatter -> t -> unit

(** Returns the precisions of the bounds of its input *)
val prec : t -> Precisions.t

(** Returns the biggest exponent of its input *)
val get_max_exponent : t -> int

(** Returns the exponent of the bounds ot its input *)
val get_exponents : t -> int * int

(** Returns the bounds of its inputs *)
val get_bounds : t -> Numerors_float.t * Numerors_float.t

(** Returns the interval [-oo ; +oo] with NaNs at the precision <prec> *)
val top      : prec:Precisions.t -> t

(** Returns the interval [+oo ; +oo] at the precision <prec> *)
val pos_inf  : prec:Precisions.t -> t

(** Returns the interval [-oo ; -oo] at the precision <prec> *)
val neg_inf  : prec:Precisions.t -> t

(** Returns an interval containing only NaN values at the precision <prec> *)
val nan      : prec:Precisions.t -> t

(** Returns the interval [-0 ; +0] at the precision <prec> *)
val zero     : prec:Precisions.t -> t

(** Returns the interval [+0 ; +0] at the precision <prec> *)
val pos_zero : prec:Precisions.t -> t

(** Add NaN into the interval *)
val add_nan : t -> t

(** Replace the infinite bounds of its input into the maximum float of the
    precision. Does not change the interval if it is finite *)
val make_finite : prec:Precisions.t -> t -> t Eval.or_bottom

(** Enlarge the bounds of the interval by taking the previous float of the lower
    bound and the following float of the upper bound *)
val enlarge : t -> t

(** The function of_<typ> ~prec (x, y) returns the interval [x' ; y'] where
    x' is a Numerors float containing the value of x (of type <typ>) rounded
    toward -oo and y' is a Numerors float containing the value of y rounded
    toward +oo. Both use the precision <prec> *)
val of_ints    : prec:Precisions.t -> int    * int    -> t
val of_floats  : prec:Precisions.t -> float  * float  -> t
val of_strings : prec:Precisions.t -> string * string -> t

(** Returns the interval corresponding to the given bounds. Fails with an
    exception if the inputs do not have the same precision *)
val of_numerors_floats : Numerors_float.t * Numerors_float.t -> t

(** Works in the same way as of_floats but the bounds are rounded toward
    nearest *)
val of_floats_without_rounding : prec:Precisions.t -> float * float -> t

(** Change the precision of the bounds *)
val change_prec : Precisions.t -> t -> t

(** Returns the interval [-epsilon ; +epsilon] for the input precision *)
val epsilon : Precisions.t -> t

(** Comparison functions *)
val compare : t -> t -> int
val eq : t -> t -> bool
val le : t -> t -> bool
val lt : t -> t -> bool
val ge : t -> t -> bool
val gt : t -> t -> bool

(** Lattice functions. Those functions work only if their inputs use the same
    precision. One can see this as if there is a lattice for each precision. *)
val is_included : t -> t -> bool
val join   : t -> t -> t
val narrow : t -> t -> t Eval.or_bottom

(** Check if the interval contains only NaN values *)
val is_nan : t -> bool

(** Check if the bounds of its input are finite *)
val is_finite : t -> bool

(** Check if the bounds of its input are both zero (without considering their
    signs) *)
val is_zero   : t -> bool

(** Check if the bounds of its input are positive zeros *)
val is_pos_zero : t -> bool

(** Check if the bounds of its input are negative zeros *)
val is_neg_zero : t -> bool

(** Check if the bounds of its input are positive infinites *)
val is_pos_inf : t -> bool

(** Check if the bounds of its input are negative infinites *)
val is_neg_inf : t -> bool

(** Check if its input contains at least a NaN value *)
val contains_nan : t -> bool

(** Check if there is a zero between the bounds of its input (without
    considering the signs *)
val contains_a_zero   : t -> bool

(** Check if there is a positive zero between the bounds of its input *)
val contains_pos_zero : t -> bool

(** Check if there is a negative zero between the bounds of its input *)
val contains_neg_zero : t -> bool

(** Check if its input contains at least an infinite bound *)
val contains_infinity : t -> bool

(** Check if there is at least a strictly positive value in its input *)
val contains_strictly_pos : t -> bool

(** Check if there is at least a strictly negative value in its input *)
val contains_strictly_neg : t -> bool

(** Check if all the values of its input are positives *)
val is_strictly_pos : t -> bool

(** Check if all the values of its input are negatives *)
val is_strictly_neg : t -> bool

(** These functions perform arithmetic operations on intervals using the
    precision <prec>. If exact=true then the bounds are computed using
    rounding to nearest mode else they are computed using rounding
    toward +oo for the upper bound and toward -oo for the lower one *)
val add : ?prec:Precisions.t -> t -> t -> t
val sub : ?prec:Precisions.t -> t -> t -> t
val mul : ?prec:Precisions.t -> t -> t -> t
val div : ?prec:Precisions.t -> t -> t -> t

(** These functions perform mathematic unidimensionnal operations of intervals
    using the precision <prec> *)
val neg    : t -> t
val abs    : t -> t
val sqrt   : ?prec:Precisions.t -> t -> t
val square : ?prec:Precisions.t -> t -> t
val log    : ?prec:Precisions.t -> t -> t
val exp    : ?prec:Precisions.t -> t -> t

(** These functions perform backward propagation on intervals using the
    precision <prec> *)
val backward_le : ?prec:Precisions.t -> t -> t -> t Bottom.or_bottom
val backward_lt : ?prec:Precisions.t -> t -> t -> t Bottom.or_bottom
val backward_ge : ?prec:Precisions.t -> t -> t -> t Bottom.or_bottom
val backward_gt : ?prec:Precisions.t -> t -> t -> t Bottom.or_bottom

(** These functions perform backward propagation for arithmetic *)
val backward_add : ?prec:Precisions.t -> left:t -> right:t ->
  result:t -> (t * t) Bottom.or_bottom
val backward_sub : ?prec:Precisions.t -> left:t -> right:t ->
  result:t -> (t * t) Bottom.or_bottom
val backward_mul : ?prec:Precisions.t -> left:t -> right:t ->
  result:t -> (t * t) Bottom.or_bottom
val backward_div : ?prec:Precisions.t -> left:t -> right:t ->
  result:t -> (t * t) Bottom.or_bottom
