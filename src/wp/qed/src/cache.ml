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
(* --- Simple Caches                                                      --- *)
(* -------------------------------------------------------------------------- *)

module type S =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module type Cache =
sig
  type 'a value
  type 'a cache
  val create : size:int -> 'a cache
  val clear : 'a cache -> unit
  val compute : 'a cache -> 'a value -> 'a value
end

let rec log2up n a b =
  let c = (a+b) / 2 in
  let s = 1 lsl c in
  if s = n then s else
  if c = a then 1 lsl b else
  if s < n then log2up n c b else log2up n a c

let alloc size =
  if size >= Sys.max_array_length 
  then Sys.max_array_length
  else log2up size 0 (Sys.word_size - 3)

module Unary(A : S) =
struct

  type 'a value = A.t -> 'a
  type 'a cell = N | C of A.t * 'a
  type 'a cache = 'a cell array

  let clear m = Array.fill m 0 (Array.length m) N

  let compute m f x =
    let h = A.hash x land (pred (Array.length m)) in
    match m.(h) with
    | C( e , r ) when A.equal x e -> r
    | _ -> let r = f x in m.(h) <- C(x,r) ; r

  let create ~size = Array.create (alloc size) N

end

module Binary(A : S) =
struct

  type 'a value = A.t -> A.t -> 'a
  type 'a cell = N | C of A.t * A.t * 'a
  type 'a cache = 'a cell array

  let clear m = Array.fill m 0 (Array.length m) N

  let compute m f x y =
    let s = Array.length m in
    let h = (A.hash x * 5 + A.hash y * 7) land (pred s) in
    match m.(h) with
    | C( a , b , r ) when A.equal x a && A.equal y b -> r
    | _ -> let r = f x y in m.(h) <- C(x,y,r) ; r

  let create ~size = Array.create (alloc size) N

end
