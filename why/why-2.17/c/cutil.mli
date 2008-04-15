(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module Option : sig
  val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
  val some : 'a option -> 'a option -> 'a option
  val app : ('a -> 'b) -> 'a option -> 'b option
  val fold : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
  val binapp : ('a -> 'a -> 'b) -> 'a option -> 'a option -> 'b option
  val transform : ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
  val pretty :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
end

module Pair : sig
  val any : ('a -> bool) -> 'a -> 'a -> bool
  val both : ('a -> bool) -> 'a -> 'a -> bool
  module Make (L1 : Set.OrderedType) (L2 : Set.OrderedType) 
      : Set.OrderedType with type t = L1.t * L2.t
end

module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string
module Int32Map : Map.S with type key = int32
module Int32Set : Set.S with type elt = int32
module Int31Map : Map.S with type key = int
module Int31Set : Set.S with type elt = int

val list1 : 'a list -> 'a
val list2 : 'a list -> 'a * 'a
val list3 : 'a list -> 'a * 'a * 'a
val list4 : 'a list -> 'a * 'a * 'a * 'a
val list5 : 'a list -> 'a * 'a * 'a * 'a * 'a
