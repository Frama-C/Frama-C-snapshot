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
