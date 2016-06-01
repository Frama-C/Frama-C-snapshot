(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(** Natural arithmetics. *)

module type Z =
sig

  type t

  val zero : t
  val one : t
  val minus_one : t

  val succ : t -> t
  val pred : t -> t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t

  val div : t -> t -> t (* acsl division  *)
  val rem : t -> t -> t (* acsl remainder *)
  val div_rem : t -> t -> t * t   (* acsl *)

  val equal : t -> t -> bool

  val leq : t -> t -> bool
  val lt  : t -> t -> bool

  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string

  val hash : t -> int
  val compare : t -> t -> int

end
