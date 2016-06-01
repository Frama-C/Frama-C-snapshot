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
(* --- Relations                                                          --- *)
(* -------------------------------------------------------------------------- *)

module type S =
sig
  type t
  type elt
  val empty : t
  val add : elt -> elt -> t -> t
  val mem : elt -> elt -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val filter : (elt -> elt -> bool) -> t -> t
  val iter : (elt -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
end

module type Elt =
sig
  type t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

module Rel(E:Elt) : S with type elt = E.t (** General relation. *)
module Sym(E:Elt) : S with type elt = E.t (** Symmetrical relation. *)
