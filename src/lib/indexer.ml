(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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
(* --- Index of items                                                     --- *)
(* -------------------------------------------------------------------------- *)

module type Elt =
sig
  type t
  val compare : t -> t -> int
end

module Make(E : Elt) =
struct

  type t = 
    | Empty
    | Node of int * t * E.t * t

  (* -------------------------------------------------------------------------- *)
  (* --- Access                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let size = function Empty -> 0 | Node(n,_,_,_) -> n

  let rec lookup n a = function
    | Empty -> raise Not_found
    | Node(_,p,e,q) ->
	let cmp = E.compare a e in
	if cmp < 0 then lookup n a p else
	  if cmp > 0 then lookup (n+size p+1) a q else
	    n + size p

  let index = lookup 0

  let rindex e t = try index e t with Not_found -> (-1)

  let rec mem a = function
    | Empty -> false
    | Node(_,p,e,q) ->
	let cmp = E.compare a e in
	if cmp < 0 then mem a p else
	  if cmp > 0 then mem a q else
	    true

  let rec get k = function
    | Empty -> raise Not_found
    | Node(_,p,e,q) ->
	let n = size p in
	if k < n then get k p else
	  if k > n then get (k-n-1) q else
	    e

  let rec iter f = function
    | Empty -> ()
    | Node(_,p,e,q) -> iter f p ; f e ; iter f q

  let rec walk n f = function
    | Empty -> ()
    | Node(_,p,e,q) -> 
	let m = n + size p in
	walk n f p ; f m e ; walk (m+1) f q

  let iteri = walk 0

  (* -------------------------------------------------------------------------- *)
  (* --- Constructors                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let empty = Empty

  let node p e q = Node(size p + size q + 1,p,e,q)

  (*TODO: can be better *)
  let rec balance p e q =
    match p , q with
      | Node(_,p1,x,p2) , _ when size q < size p1 -> node p1 x (balance p2 e q)
      | _ , Node(_,q1,y,q2) when size p < size q2 -> node (balance p e q1) y q2
      | _ -> node p e q

  (* -------------------------------------------------------------------------- *)
  (* --- Add,Remove                                                         --- *)
  (* -------------------------------------------------------------------------- *)
	    
  let rec add a = function
    | Empty -> Node(1,Empty,a,Empty)
    | Node(n,p,e,q) ->
	let cmp = E.compare a e in
	if cmp < 0 then balance (add a p) e q else
	  if cmp > 0 then balance p e (add a q) else
	    Node(n,p,a,q)

  (* requires x<y for each x in p and y in q *)
  let rec join p q =
    match p,q with
      | Empty,r | r,Empty -> r
      | Node(n,p1,x,p2) , Node(m,q1,y,q2) ->
	  if n >= m 
	  then balance p1 x (join p2 q) 
	  else balance (join p q1) y q2

  let rec remove a = function
    | Empty -> Empty
    | Node(_,p,e,q) ->
	let cmp = E.compare a e in
	if cmp < 0 then balance (remove a p) e q else
	  if cmp > 0 then balance p e (remove a q) else
	    join p q

  let rec filter f = function
    | Empty -> Empty
    | Node(_,p,e,q) -> 
	let p = filter f p in
	let q = filter f q in
	if f e then balance p e q else join p q

  (* -------------------------------------------------------------------------- *)
  (* --- Update                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let update x y t =
    match x , y with
      | None , None -> (* identify *) 0,-1,t
      | Some x , None -> (* remove x *)
	  let i = rindex x t in
	  if i < 0 then 0,-1,t else i,size t-1,remove x t
      | None , Some y -> (* add y *)
	  let t = add y t in
	  let j = index y t in
	  j , size t-1 , t
      | Some x , Some y ->
	  let i = rindex x t in
	  if i < 0 then
	    let t = add y t in
	    let j = rindex y t in
	    j , size t-1 , t
	  else
	    let t = add y (remove x t) in
	    let j = rindex y t in
	    min i j , max i j , t

end
