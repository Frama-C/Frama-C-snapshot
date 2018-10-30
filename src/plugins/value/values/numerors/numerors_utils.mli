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

(*-----------------------------------------------------------------------------
 *  Module describing the different precisions that will be manipulated
 *---------------------------------------------------------------------------*)
module Precisions : sig

  (** We handle the format defined in C. The Real constructor represents the
      precision of the floats used as real *)
  type t = Simple | Double | Long_Double | Real

  val pretty : Format.formatter -> t -> unit

  (** Returns the precision associated to the Cil construction fkind, which
      represents the C floating point type *)
  val of_fkind : Cil_types.fkind -> t

  (** Returns the number of bits of the significand of the given precision,
      counting the implicit one. This size is fixed by the option
      -eva-numerors-real-size for the Real precision. *)
  val get : t -> int

  (** Returns the number of bits of the exponent of the given precision. The
      exponent of the Real precision is set to max int arbitrally. *)
  val exponent : t -> int

  (** Returns the integer corresponding to the exponent of the denormalized
      numbers of the given precision. The value 2^denormalized is the smallest
      denormalized number of the precision and is also the gap between two
      denormalized numbers. The returned integer is negative. For the
      Real precision, this integer is arbitrally set to min int. *)
  val denormalized : t -> int

  val compare : t -> t -> int
  val eq : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t

end



(*-----------------------------------------------------------------------------
 *  Module describing signs of infinite values
 *---------------------------------------------------------------------------*)
module Sign : sig

  type t = Positive | Negative

  val pretty : Format.formatter -> t -> unit

  val of_int : int -> t

  val compare : t -> t -> int
  val eq : t -> t -> bool

  val neg : t -> t
  val mul : t -> t -> t

  val is_pos : t -> bool
  val is_neg : t -> bool

end



(*-----------------------------------------------------------------------------
 *  Module describing the used rounding modes
 *---------------------------------------------------------------------------*)
module Rounding : sig

  (** We only use the rounding to nearest (represented by the constructor Near),
      the rounding toward +oo (represented by the constructor Up) and the
      rounding toward -oo (represented by the constructor Down) *)
  type t = Up | Down | Near

  val pretty : Format.formatter -> t -> unit

  val eq : t -> t -> bool

end



(*-----------------------------------------------------------------------------
 *  Module describing the interaction mode
 *---------------------------------------------------------------------------*)
module Mode : sig

  (** Those constructors corresponds to the possible values of the option
      -eva-numerors-mode *)
  type t = Abs_From_Rel | Rel_From_Abs | No_Interaction | With_Interactions

  val get : unit -> t

end
