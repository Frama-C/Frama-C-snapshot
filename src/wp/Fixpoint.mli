(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Generic Fixpoint Computation over a Domain                         --- *)
(* -------------------------------------------------------------------------- *)

module type Domain =
sig
  type t
  val bot : t
  val leq : t -> t -> bool
  val cap : t -> t -> t
  val cup : t -> t -> t
  val wide : t -> t -> t
  val pretty : Format.formatter -> t -> unit
end

module Make(D : Domain) :
sig

  type var
  type system
  type fixpoint

  type f1 = D.t -> D.t
  type f2 = D.t -> D.t -> D.t
  type fn = D.t list -> D.t

  val create : unit -> system
  val var : system -> var
  val add : system -> var -> var -> unit 
  (** [add x y] requires x >= y *)
  val add0 : system -> var -> D.t -> unit 
  (** [add0 x d] requires x >= d *)
  val add1 : system -> var -> f1 -> var -> unit 
  (** [add x f y] requires x >= f(y) *)
  val add2 : system -> var -> f2 -> var -> var -> unit 
  (** [add x f y z] requires x >= f(y,z) *)
  val addn : system -> var -> fn -> var list -> unit
  (** [add x f ys] requires x >= f(ys) *)

  val fixpoint : system:system -> root:var -> timeout:int -> fixpoint
  (** Computes the least fixpoint solution satifying all added
      requirements. Chains of pure-copies (see [add]) are detected and
      optimized. Uses Bourdoncle's weak partial ordering to compute
      the solution. For each component, after [timeout]-steps of
      non-stable iteration, the widening operator of [D] is used to
      stabilize the computation. *)

  val get : fixpoint -> var -> D.t

end
