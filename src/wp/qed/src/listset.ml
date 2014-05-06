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

  (* used for better sharing between a list and a modified list *)
  let rev_append_until i l1 l2 = 
    let rec aux acc = function
      | [] -> acc
      | i'::_ when i'==i -> acc
      | i'::l -> aux (i'::acc) l
    in aux l2 l1

  (* used for better sharing between a list and a modified list *)
  let append_until i l1 l2 = 
    List.rev_append (rev_append_until i l1 []) l2

  (* good sharing *) 
  let mapq f l =
    let rec aux ((res,rest) as acc) = function
      | [] -> List.rev_append res rest
      | i :: resti -> 
          (match f i with
           | None -> (* remove *) aux ((rev_append_until i rest res),resti) resti
           | Some i' -> 
               if i' = i then (* add idem *) aux acc resti 
               else (* add new *) aux ((i'::(rev_append_until i rest res)),resti) resti)
    in aux ([],l) l

  (* good sharing *) 
  (* idem List.filter, but returns l if no element is removed. *) 
  let filter f l =
    let rec aux ((res,rest) as acc) = function
      | [] -> List.rev_append res rest
      | i :: resti -> 
          if f i then aux acc resti 
          else aux ((rev_append_until i rest res),resti) resti
    in aux ([],l) l

  (* good sharing *) 
  let partition f l  =
    let rec aux ((res,rest) as acc) ((res',rest') as acc') = function
      | [] -> (List.rev_append res rest), (List.rev_append res' rest')
      | i :: resti -> 
          if f i then aux acc ((rev_append_until i rest' res'),resti) resti 
          else aux ((rev_append_until i rest res),resti) acc' resti
    in aux ([],l) ([],l) l

  (* good sharing *) 
  let add k l = 
    let rec aux = function
      | [] -> l @ [k]
      | (k'::next) as w ->
          let c = E.compare k k' in
          if c < 0 then append_until k' l (k::w)
          else if c = 0 then l
          else (* c > 0 *) aux next
    in aux l

  (* good sharing *) 
  let remove k l = 
    let rec aux = function
      | [] -> l
      | (k'::next) as w ->
          let c = E.compare k k' in
          if c > 0 then append_until k' l w
          else if c = 0 then append_until k' l next
          else (* c > 0 *) aux next
    in aux l

  let rec mem x = function
    | [] -> false
    | e::es ->
        let c = E.compare x e in
        if c < 0 then false else
        if c > 0 then mem x es else true

  let iter = List.iter
  let fold = List.fold_right

  (* good sharing with w1 *) 
  let union w1 w2 =
    let rec aux ((res,o1) as acc) w1 w2 =
      match w1 , w2 with
      | [] , _ -> (* adding w2 *) List.rev_append res (List.append o1 w2)
      | _ , [] -> (* adding w1 *) List.rev_append res o1
      | a1::r1 , a2::r2 ->
          let c = E.compare a1 a2 in
          if c < 0 then (* adding a1 *) aux acc r1 w2
          else if c = 0 then (* adding a1 *) aux acc r1 r2 
          else (* c > 0 *) (* adding a2 *) aux ((a2::(rev_append_until a1 o1 res)),w1) w1 r2
    in aux ([],w1) w1 w2

  (* good sharing with w1 *) 
  let interf f w1 w2 =
    let rec aux ((res,o1) as acc) w1 w2 =
      match w1 , w2 with
      | [] , _ ->    (* no addition *) List.rev_append res o1
      | a1::_, [] -> (* no addition *) List.rev_append res (List.rev (rev_append_until a1 o1 []))
      | a1::r1 , a2::r2 ->
          let c = E.compare a1 a2 in
          if c < 0 then (* remove a1 *) aux ((rev_append_until a1 o1 res),r1) r1 w2
          else if c > 0 then (* skip a2 *) aux acc w1 r2
          else if not (f a1) then (* remove a1 *) aux ((rev_append_until a1 o1 res),r1) r1 r2
          else (* adding a1 *) aux acc r1 r2 
    in aux ([],w1) w1 w2

  let inter = interf (fun _ -> true)

  (* good sharing with w1 *) 
  let diff w1 w2 =
    let rec aux ((res,o1) as acc) w1 w2 =
      match w1 , w2 with
      | [] , _ -> (* no addition *) List.rev_append res o1
      | _ , [] -> (* adding w1 *) List.rev_append res o1
      | a1::r1 , a2::r2 ->
          let c = E.compare a1 a2 in
          if c < 0 then (* adding a1 *) aux acc r1 w2
          else if c > 0 then (* skip *) aux acc w1 r2
          else (* remove a1 *) aux ((rev_append_until a1 o1 res),r1) r1 r2
    in aux ([],w1) w1 w2

  let rec subsetf f xs ys =
    match xs , ys with
    | [] , _ -> true
    | _::_ , [] -> false
    | (x::xtail) , (y::ytail) ->
        let c = E.compare x y in
        if c < 0 then false else
        if c > 0 then subsetf f xs ytail else
          (f x && subsetf f xtail ytail)

  let subset = subsetf (fun _ -> true)

  let rec intersectf f xs ys =
    match xs , ys with
    | [] , _ | _ , [] -> false
    | (x::xtail) , (y::ytail) ->
        let c = E.compare x y in
        if c < 0 then intersectf f xtail ys else
        if c > 0 then intersectf f xs ytail else
          f x
  let intersect = intersectf (fun _ -> true)

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
