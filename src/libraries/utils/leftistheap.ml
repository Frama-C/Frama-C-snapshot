(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(* Leftist heaps.

   See for instance Chris Okasaki's "Purely Functional Data Structures" *)


exception Empty

module Make(X: Set.OrderedType) :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : X.t -> t -> t
  val min : t -> X.t
  val extract_min : t -> X.t * t
  val merge : t -> t -> t
  val fold: (X.t -> 'a -> 'a) -> t -> 'a -> 'a
  val elements: t -> X.t list
  val of_list: X.t list -> t
end
=
struct

  type t = E | T of int * X.t * t * t

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let make x a b =
    let ra = rank a and rb = rank b in
    if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

  let empty = E

  let is_empty = function E -> true | T _ -> false

  let rec merge h1 h2 = match h1,h2 with
    | E, h | h, E ->
      h
    | T (_,x,a1,b1), T (_,y,a2,b2) ->
      if X.compare x y <= 0
      then make x a1 (merge b1 h2)
      else make y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h

  let min = function E -> raise Empty | T (_,x,_,_) -> x

  let extract_min = function
    | E -> raise Empty
    | T (_,x,a,b) -> x, merge a b

  (* Added by CEA *)

  let rec fold f h acc = match h with
    | E -> acc
    | T (_, x, h1, h2) -> fold f h2 (f x (fold f h1 acc))

  let elements h = fold (fun e l -> e :: l) h []

  (* can probably be improved *)
  let of_list l = List.fold_left (fun h x -> insert x h) E  l

end
