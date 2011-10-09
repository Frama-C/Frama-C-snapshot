(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* ------------------------------------------------------------------------ *)
(* ---  List with constant-time concat                                  --- *)
(* ------------------------------------------------------------------------ *)

type 'a t =
  | Empty
  | Elt of 'a
  | Add of 'a * 'a t
  | List of 'a list
  | Concat of 'a t * 'a t

let empty = Empty
let elt x = Elt x

let length t =
  let rec scan n = function
    | Empty -> n
    | Elt _ -> succ n
    | Add(_,t) -> scan (succ n) t
    | List xs -> n + List.length xs
    | Concat(a,b) -> scan (scan n a) b
  in scan 0 t

let add x = function
  | Empty -> Elt x
  | t -> Add(x,t)

let list = function
  | [] -> Empty
  | [x] -> Elt x
  | xs -> List xs

let concat a b =
  match a,b with
    | Empty,c | c,Empty -> c
    | Elt x,t -> Add(x,t)
    | Concat(a,b),c -> Concat(a,Concat(b,c)) (* 1-time optim *)
    | _ -> Concat(a,b)

let rec ulist = function
  | [] -> Empty
  | x::xs -> concat x (ulist xs)

let rec map f = function
  | Empty -> Empty
  | Elt x -> Elt (f x)
  | Add(x,t) -> Add(f x,map f t)
  | List xs -> List(List.map f xs)
  | Concat(a,b) -> Concat(map f a,map f b)

let rec umap f = function
  | Empty -> Empty
  | Elt x -> f x
  | Add(x,t) -> concat (f x) (umap f t)
  | List xs -> umap_list f xs
  | Concat(a,b) -> concat (umap f a) (umap f b)

and umap_list f = function
  | [] -> Empty
  | x::xs -> concat (f x) (umap_list f xs)

let rec iter f = function
  | Empty -> ()
  | Elt x -> f x
  | Add(x,t) -> f x ; iter f t
  | List xs -> List.iter f xs
  | Concat(a,b) -> iter f a ; iter f b

let rec fold_left f w = function
  | Empty -> w
  | Elt x -> f w x
  | Add(x,t) -> fold_left f (f w x) t
  | List xs -> List.fold_left f w xs
  | Concat(a,b) -> fold_left f (fold_left f w a) b

let rec fold_right f t w = match t with
  | Empty -> w
  | Elt x -> f x w
  | Add(x,t) -> f x (fold_right f t w)
  | List xs -> List.fold_right f xs w
  | Concat(a,b) -> fold_right f a (fold_right f b w)

let rec filter f = function
  | Empty -> Empty
  | Elt x as e -> if f x then e else Empty
  | Add(x,ts) -> if f x then add x (filter f ts) else filter f ts
  | List xs -> list (List.filter f xs)
  | Concat(a,b) -> concat (filter f a) (filter f b)

let rec partition f = function
  | Empty -> Empty , Empty
  | Elt x as e -> if f x then e,Empty else Empty,e
  | Add(x,ts) ->
      let pos,neg = partition f ts in
      if f x then add x pos , neg else pos , add x neg
  | List xs ->
      let pos,neg = List.partition f xs in
      list pos , list neg
  | Concat(a,b) ->
      let apos,aneg = partition f a in
      let bpos,bneg = partition f b in
      concat apos bpos , concat aneg bneg

let rec is_empty = function
  | Empty | List [] -> true
  | Add(_,_) | Elt _ | List _ -> false
  | Concat(a,b) -> is_empty a && is_empty b

let rec singleton = function
  | Elt x | List [x] -> Some x
  | Empty | List _ -> None
  | Add(x,t) -> if is_empty t then Some x else None
  | Concat(a,b) ->
      match singleton a with
        | Some x -> if is_empty b then Some x else None
        | None -> if is_empty a then singleton b else None
