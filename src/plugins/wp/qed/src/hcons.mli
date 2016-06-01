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
(**    Hash-Consing Utilities                                                 *)
(* -------------------------------------------------------------------------- *)

val primes : int array
val hash_int : int -> int
val hash_tag : 'a -> int
val hash_pair : int -> int -> int
val hash_triple : int -> int -> int -> int
val hash_list : ('a -> int) -> int -> 'a list -> int
val hash_array : ('a -> int) -> int -> 'a array -> int
val hash_opt : ('a -> int) -> int -> 'a option -> int

val eq_list : 'a list -> 'a list -> bool (** Uses [==]. *)
val eq_array : 'a array -> 'a array -> bool (** Uses [==]. *)
val equal_list : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val equal_array : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
val compare_list : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val exists_array : ('a -> bool) -> 'a array -> bool
val forall_array : ('a -> bool) -> 'a array -> bool

val fold_list : ('a -> 'a -> 'a) -> ('b -> 'a) -> 'a -> 'b list -> 'a
val fold_array : ('a -> 'a -> 'a) -> ('b -> 'a) -> 'a -> 'b array -> 'a
