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

(** Maps with integers keys using Patricia Trees.

    From the paper of Chris Okasaki and Andrew Gill: 
    'Fast Mergeable Integer Maps'.
*)

type 'a t

val empty : 'a t
val singleton : int -> 'a -> 'a t

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val is_empty : 'a t -> bool
val size : 'a t -> int
val mem : int -> 'a t -> bool
val find : int -> 'a t -> 'a (** or raise Not_found *)

val add : int -> 'a -> 'a t -> 'a t
val remove : int -> 'a t -> 'a t

(** [insert (fun key v old -> ...) key v map] *)
val insert : (int -> 'a -> 'a -> 'a) -> int -> 'a -> 'a t -> 'a t

val change : (int -> 'b -> 'a option -> 'a option) -> int -> 'b -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val foldi : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val mapl : (int -> 'a -> 'b) -> 'a t -> 'b list

val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val mapf : (int -> 'a -> 'b option) -> 'a t -> 'b t
val mapq : (int -> 'a -> 'a option) -> 'a t -> 'a t
val filter : (int -> 'a -> bool) -> 'a t -> 'a t
val partition : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
val partition_split : (int -> 'a -> 'a option * 'a option) -> 'a t -> 'a t * 'a t

val for_all: (int -> 'a -> bool) -> 'a t -> bool
val exists: (int -> 'a -> bool) -> 'a t -> bool

val union : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
val inter : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val interf : (int -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
val interq : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
val diffq :  (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
val subsetk : 'a t -> 'b t -> bool
val subset : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
val intersect : 'a t -> 'b t -> bool
val intersectf : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool


val merge : (int -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
val iter2 : (int -> 'a option -> 'b option -> unit) -> 'a t -> 'b t -> unit
val iterk : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit

val pp_bits : Format.formatter -> int -> unit
val pp_tree : string -> Format.formatter -> 'a t -> unit
