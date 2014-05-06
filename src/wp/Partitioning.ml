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
(* --- Variables Partitioning                                             --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Lang
open Lang.F

type partition = {
  mutable color : var Vmap.t ;
  mutable depend : Vars.t Vmap.t ;
  mutable mem : var Tmap.t ;
}

let zero = Var.dummy
let create () = {
  color = Vmap.empty ;
  depend = Vmap.empty ;
  mem = Tmap.empty ;
}

(* -------------------------------------------------------------------------- *)
(* --- Current Partition                                                  --- *)
(* -------------------------------------------------------------------------- *)

let rec color w x = 
  try
    let y = Vmap.find x w.color in
    let z = color w y in
    if z != y then w.color <- Vmap.add x z w.color ; z
  with Not_found -> x

let depend w x =
  try Vmap.find (color w x) w.depend
  with Not_found -> Vars.empty

(* -------------------------------------------------------------------------- *)
(* --- Unification & Dependencies                                         --- *)
(* -------------------------------------------------------------------------- *)

(* keep x, bind y *)    
let merge w x y =
  w.color <- Vmap.add y x w.color ;
  let xs = depend w x in
  let ys = depend w y in
  let zs = Vars.union xs ys in
  w.depend <- Vmap.add x zs (Vmap.remove y w.depend)

let unify w x y =
  if x == zero then y else
    if y == zero then x else
      let x = color w x in
      let y = color w y in
      let cmp = Var.compare x y in
      if cmp < 0 then (merge w x y ; x) else
	if cmp > 0 then (merge w y x ; y) else
	  x

let add_depend w x xs =
  let x = color w x in
  let ys = depend w x in
  w.depend <- Vmap.add x (Vars.union xs ys) w.depend

(* -------------------------------------------------------------------------- *)
(* --- Segregation                                                        --- *)
(* -------------------------------------------------------------------------- *)

let is_varray x = match Var.sort x with Sarray _ -> true | _ -> false

let color_of w xs c e =
  let ms,xs = Vars.partition is_varray (Vars.diff (F.varsp e) xs) in
  let c = Vars.fold (unify w) ms c in
  let d = Vars.fold (unify w) xs zero in
  if c == zero then d else
    (if d != zero then add_depend w c (Vars.singleton d) ; c)

(* -------------------------------------------------------------------------- *)
(* --- Collection                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec walk w xs p = 
  match F.pred p with
    | Eq(a,b) | Leq(a,b) | Lt(a,b) | Neq(a,b) ->
	let ca = color_of w xs zero a in
	let cb = color_of w xs zero b in
	ignore (unify w ca cb)
    | Fun(_,es) ->
	ignore 
	  (List.fold_left
	     (fun c e ->
		let ce = color_of w xs zero e in
		unify w c ce) 
	     zero es)
    | And ps | Or ps -> List.iter (walk w xs) ps
    | Not p -> walk w xs p
    | Imply(hs,p) -> List.iter (walk w xs) (p::hs)
    | Bind(_,x,p) -> walk w (Vars.add x xs) p
    | _ -> ignore (color_of w xs zero p)

let collect w = walk w Vars.empty

(* -------------------------------------------------------------------------- *)
(* --- Partition                                                          --- *)
(* -------------------------------------------------------------------------- *)

type classeq = partition * Vars.t

(* dependencies must be normalized *)
let rec closure w x xs =
  let x = color w x in
  if Vars.mem x xs then xs else
    Vars.fold (closure w) (depend w x) (Vars.add x xs)

let classes w =
  w.depend <- Vmap.map (fun _ xs -> Vars.map (color w) xs) w.depend ;
  Vars.fold
    (fun x cs -> ( w , closure w x Vars.empty ) :: cs)
    (Vmap.fold 
       (fun _ x xs -> Vars.add (color w x) xs)
       w.color Vars.empty)
    []

(* Tautologies: False ==> P and P ==> True for all P *)
(* Requires: filter false p ==> p *)
(* Requires: p ==> filter true p *)
let rec filter w positive xs p = 
  match F.pred p with
    | And ps -> F.p_all (filter w positive xs) ps
    | Or ps -> F.p_any (filter w positive xs) ps
    | Not p -> F.p_not (filter w (not positive) xs p)
    | Imply(hs,p) -> 
	let hs = List.map (filter w (not positive) xs) hs in
	F.p_hyps hs (filter w positive xs p)
    | Bind(q,x,p) -> F.p_bind q x (filter w positive (Vars.add x xs) p)
    | _ -> 
	if Vars.exists (fun x -> Vars.mem (color w x) xs) (F.varsp p) 
	then p
	else if positive then p_true else p_false

let filter_hyp (w,xs) = filter w true xs
let filter_goal (w,xs) = filter w false xs
