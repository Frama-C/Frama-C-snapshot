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

(** Set of integers using Patricia Trees.

    From the paper of Chris Okasaki and Andrew Gill:
    'Fast Mergeable Integer Maps'.
*)

type t

val compare : t -> t -> int
val equal : t -> t -> bool

val empty : t
val singleton : int -> t

val is_empty : t -> bool
val cardinal : t -> int
val elements : t -> int list

val mem : int -> t -> bool
val add : int -> t -> t
val remove :int -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val subset : t -> t -> bool

val iter : (int -> unit) -> t -> unit
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (int -> bool) -> t -> bool
val exists : (int -> bool) -> t -> bool
val filter : (int -> bool) -> t -> t
val partition : (int -> bool) -> t -> t * t

val intersect : t -> t -> bool
