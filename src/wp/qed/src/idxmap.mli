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

(** Map with indexed keys *)

module type S =
sig
  type key
  type 'a t
  val is_empty : 'a t -> bool
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : (key -> 'a -> 'b) -> 'a t -> 'b t
  val mapf : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val mapq : (key -> 'a -> 'a option) -> 'a t -> 'a t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val interf : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val interq : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diffq : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t 

  (** [insert (fun key v old -> ...) key v map] *)
  val insert : (key -> 'a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t

  val change : (key -> 'b -> 'a option -> 'a option) -> key -> 'b -> 'a t -> 'a t

end

module type IndexedKey =
sig
  type t
  val id : t -> int (** unique per t *)
end

module Make( K : IndexedKey ) : S with type key = K.t
