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

(* -------------------------------------------------------------------------- *)
(** Merging Set Functor *)
(* -------------------------------------------------------------------------- *)

module type Elt =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(E : Elt) :
sig

  type elt = E.t

  type t = elt list
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val empty : t

  (* good sharing *)
  val add : elt -> t -> t

  (* good sharing *)
  val remove : elt -> t -> t
  val mem : elt -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  (* good sharing *)
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t

  (* good sharing *)
  val union : t -> t -> t

  (* good sharing *)
  val inter : t -> t -> t

  (* good sharing *)
  val diff : t -> t -> t

  val subset : t -> t -> bool
  val intersect : t -> t -> bool
  val factorize : t -> t -> t * t * t
  (** Returns (left,common,right) *)

  val big_union : t list -> t
  val big_inter : t list -> t

end
