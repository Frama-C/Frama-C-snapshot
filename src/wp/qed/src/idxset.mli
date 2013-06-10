(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Set of indexed elements implemented as Patricia sets. *)

module type S =
sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val mapf : (elt -> elt option) -> t -> t
  val intersect : t -> t -> bool
end

module type IndexedElements =
sig
  type t
  val id : t -> int (** unique per t *)
end

module Make( E : IndexedElements ) : S with type elt = E.t
module Positive( E : IndexedElements ) : S with type elt = E.t
  (** With only positive index *)
