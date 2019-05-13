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

open Eval
open Numerors_utils
module I = Numerors_interval

(** Type manipulated by the arithmetics *)
type t = { exact : I.t ; approx : I.t ; abs_err : I.t ; rel_err : I.t }

(** Pretty printer *)
val pretty : Format.formatter -> t -> unit

(** Return a value with all fields to zero. The <approx> field will
    use the precision of the approx field of the parameter *)
val zero : t -> t

(** Return the precision of the <approx> field *)
val prec : t -> Precisions.t

(** Create a record from intervals *)
val create : I.t -> I.t -> I.t -> I.t -> t

(** Apply an operation on each fields of the operands *)
val apply : (I.t -> I.t -> I.t) -> t -> t -> t

(** Return a new value with the same fields as the input but with
    an <approx> field with the given precision *)
val change_prec : Precisions.t -> t -> t

(** Handling of forward interactions *)
val forward_interaction : t -> t

(** Lattice methods *)
val join        : t -> t -> t
val narrow      : t -> t -> t or_bottom
val compare     : t -> t -> int
val is_included : t -> t -> bool

(** Signature of an arithmetic *)
module type Arithmetic = sig

  (* Type returned by the forward operations *)
  type forward

  module Forward  : sig
    val neg   : t -> forward
    val log   : t -> forward
    val exp   : t -> forward
    val sqrt  : t -> forward
    val add   : t -> t -> forward
    val sub   : t -> t -> forward
    val mul   : t -> t -> forward
    val div   : t -> t -> forward
  end

  module Backward : sig
    val neg   : t -> t -> I.t or_bottom
    val add   : t -> t -> t -> (I.t * I.t) or_bottom
    val sub   : t -> t -> t -> (I.t * I.t) or_bottom
    val mul   : t -> t -> t -> (I.t * I.t) or_bottom
    val div   : t -> t -> t -> (I.t * I.t) or_bottom
  end

end

(** Modules which implement the previous signature for each field of <t> *)
module Exact    : Arithmetic with type forward = I.t
module Approx   : Arithmetic with type forward = I.t

(* The forward type of Abs_Err forces to pass two aditionnal parameters to the
   functions. The first one is the exact computation of the current expression
   while the second one is its approx computation. *)
module Abs_Err  : Arithmetic with
  type forward = exact:I.t -> approx:I.t -> I.t

(* The forward type of Rel_Err forces to pass two aditionnal parameters to the
   functions. The first one is the exact computation of the current expression
   while the second one is its absolute error computation. *)
module Rel_Err  : Arithmetic with
  type forward = exact:I.t -> abs_err:I.t -> I.t

(** Backward comparisons *)
module Backward_Comparisons : sig
  val lt : t -> t -> (t * t) or_bottom
  val le : t -> t -> (t * t) or_bottom
  val gt : t -> t -> (t * t) or_bottom
  val ge : t -> t -> (t * t) or_bottom
end
