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

(* -------------------------------------------------------------------------- *)
(** Merging Map Functor *)
(* -------------------------------------------------------------------------- *)

module type Key =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(K : Key) :
sig

  type key = K.t

  type 'a t = (key * 'a) list Intmap.t

  val is_empty : 'a t -> bool

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val findk : key -> 'a t -> key * 'a
  val remove : key -> 'a t -> 'a t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val mapf : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val iter_sorted : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_sorted: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val subset : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iterk : (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val iter2 : (key -> 'a option -> 'b option -> unit) -> 'a t -> 'b t -> unit
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

end
