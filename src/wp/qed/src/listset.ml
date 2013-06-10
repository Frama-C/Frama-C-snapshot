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
(* --- Merging List-Set Functor                                           --- *)
(* -------------------------------------------------------------------------- *)

module type Elt =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(E : Elt) =
struct

  type elt = E.t

  type t = E.t list

  let compare = Hcons.compare_list E.compare
  let equal = Hcons.equal_list E.equal

  let empty = []
      
  let rec add x = function
    | [] -> [x]
    | (e::es) as m ->
	let c = E.compare x e in
	if c < 0 then x :: m else
	  if c > 0 then e :: add x es else m
    
  let rec mem x = function
    | [] -> false
    | e::es ->
	let c = E.compare x e in
	if c < 0 then false else
	  if c > 0 then mem x es else true

  let iter = List.iter
  let fold = List.fold_right

  let filter = List.filter
  let partition = List.partition
      
  let rec union xs ys =
    match xs , ys with
      | [] , zs | zs , [] -> zs
      | (x::xtail) , (y::ytail) ->
	  let c = E.compare x y in
	  if c < 0 then x :: union xtail ys else
	    if c > 0 then y :: union xs ytail else
	      x :: union xtail ytail
      
  let rec inter xs ys =
    match xs , ys with
      | [] , _ | _ , [] -> []
      | (x::xtail) , (y::ytail) ->
	  let c = E.compare x y in
	  if c < 0 then inter xtail ys else
	    if c > 0 then inter xs ytail else
	      x :: inter xtail ytail

  let rec subset xs ys =
    match xs , ys with
      | [] , _ -> true
      | _::_ , [] -> false
      | (x::xtail) , (y::ytail) ->
	  let c = E.compare x y in
	  if c < 0 then false else
	    if c > 0 then subset xs ytail else
	      subset xtail ytail

  let rec diff xs ys =
    match xs , ys with
      | [] , _ -> []
      | _ , [] -> xs
      | (x::xtail) , (y::ytail) ->
	  let c = E.compare x y in
	  if c < 0 then x :: diff xtail ys else
	    if c > 0 then diff xs ytail else
	      diff xtail ytail

  let rec intersect xs ys =
    match xs , ys with
      | [] , _ | _ , [] -> false
      | (x::xtail) , (y::ytail) ->
	  let c = E.compare x y in
	  if c < 0 then intersect xtail ys else
	    if c > 0 then intersect xs ytail else
	      true

  let rec fact rxs cxs rys xs ys =
    match xs , ys with
      | [] , _ | _ , [] ->
	  List.rev_append rxs xs , List.rev cxs , List.rev_append rys ys
      | x::xtail , y::ytail ->
	  let c = E.compare x y in
	  if c < 0 then fact (x::rxs) cxs rys xtail ys else
	    if c > 0 then fact rxs cxs (y::rys) xs ytail else
	      fact rxs (x::cxs) rys xtail ytail

  let factorize xs ys = fact [] [] [] xs ys

  let rec big_union = function
    | [] -> []
    | e::es -> union e (big_union es)

  let rec big_inter = function
    | [] -> []
    | [e] -> e
    | e::es -> inter e (big_inter es)

end
