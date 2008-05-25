(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type pure_type =
  | PTint
  | PTbool
  | PTreal
  | PTunit
  | PTvar of string
  | PTexternal of pure_type list * string

type constant =
  | ConstInt of string
  | ConstBool of bool
  | ConstUnit
  | ConstFloat of string

type term =
  | Tconst of constant
  | Tvar of string
  | Tapp of string * term list
  | Tif of term * term * term

type predicate =
  | Pvar of string
  | Papp of string * term list
  | Ptrue
  | Pfalse
  | Pimplies of predicate * predicate
  | Pif of term * predicate * predicate
  | Pand of predicate * predicate
  | Por of predicate * predicate
  | Pxor of predicate * predicate
  | Piff of predicate * predicate
  | Pnot of predicate
  | Pforall of string * pure_type * predicate
  | Pexists of string * pure_type * predicate
  | Pnamed of string * predicate

type decl =
  | Function of string * pure_type list * pure_type
  | Predicate of string * pure_type list
  | Axiom of string * predicate
  | Goal of string * predicate
  | Type of pure_type

let pand = function
  | Ptrue, p2 -> p2
  | p1, Ptrue -> p1
  | p1, p2 -> Pand (p1, p2)

let pands = List.fold_right (fun p1 p2 -> pand (p1, p2))

let pimplies = function
  | Ptrue, p2 -> p2
  | Pfalse, _ -> Ptrue
  | _, Ptrue -> Ptrue
  | p1, p2 -> Pimplies (p1, p2)

let pnot = function
  | Papp ("neq_int_bool", l) ->
      Papp ("eq_int_bool", l)
  | Papp ("eq_bool", l) ->
      Papp ("neq_bool", l)
  | Papp ("neq_bool", l) ->
      Papp ("eq_bool", l)

  | p -> Pnot p

let term_to_predicate t =
  Papp ("eq_bool",[t;Tconst (ConstBool true)])

let por = function
  | Ptrue, _ | _,Ptrue -> Ptrue
  | Pfalse,p |p,Pfalse -> p
  | p,p' -> Por (p,p')

let pxor = function
  | Ptrue, Ptrue -> Pfalse
  | Ptrue,_ | _,Ptrue -> Ptrue
  | Pfalse, p | p, Pfalse -> p
  | p,p' -> Pxor(p,p')

let pors = List.fold_right (fun p1 p2 -> por (p1, p2))

let pif = function
  | (_,Ptrue, Ptrue ) -> Ptrue
  | (_,Pfalse, Pfalse ) -> Pfalse


  | (c,Ptrue,p ) -> por ((term_to_predicate c),p)

  | (c,p, Ptrue ) -> por (pnot (term_to_predicate c),p)

  | (c,Pfalse,p ) -> pand (pnot (term_to_predicate c),p)

  | (c,p, Pfalse ) -> pand (term_to_predicate c,p)

  | (t,p1,p2) -> Pif (t,p1,p2)

let piff = function p,p' -> Piff (p,p')
let papp = function p,p' -> Papp (p,p')

let rec subst_in_term x fresh t = match t with
| Tconst _ -> t
| Tvar v -> if v = x then fresh else t
| Tapp (n,tl) ->
    Tapp(n,List.map (subst_in_term x fresh) tl)
| Tif (t1,t2,t3) -> Tif(subst_in_term x fresh t1,
                        subst_in_term x fresh t2,
                        subst_in_term x fresh t3)

let rec subst_in_predicate x fresh p =
  let subst_pred = subst_in_predicate x fresh in
  let subst_term = subst_in_term x fresh in
  match p with
  | Pvar _ | Ptrue | Pfalse -> p

  | Pif (t,p1,p2) -> pif (subst_term t,
                          subst_pred p1,
                          subst_pred p2)
  | Pnot p -> pnot (subst_pred p)
  | Pforall (v,pt,p') ->
      if x = v then p else
        Pforall (v,pt,subst_pred p')
  | Pexists (v,pt,p') ->
      if x = v then p else
        Pexists (v,pt,subst_pred p')
  | Pnamed (n,p) ->
     Pnamed (n,subst_pred p)
  | Pimplies (p1,p2) ->
      pimplies (subst_pred p1,subst_pred p2)
  | Pand (p1,p2) ->
      pand (subst_pred p1,subst_pred p2)
  | Por (p1,p2) ->
      por (subst_pred p1,subst_pred p2)
  | Pxor(p1,p2) ->
      pxor (subst_pred p1, subst_pred p2)
  | Piff (p1,p2) ->
      piff (subst_pred p1,subst_pred p2)
  | Papp (n,t)
    -> papp (n,List.map subst_term t)

let fresh_name =
  let counter = ref 0 in
  fun name ->
    incr counter;
    name^"_" ^ string_of_int !counter

let plet x ty t p =
  if p = Ptrue then
    Ptrue
  else begin
    match t with
    | Tconst _ ->
        subst_in_predicate x t p
    | _ ->
        let x' = fresh_name x in
        Pforall (x', ty,
	         Pimplies
                   (Papp ("eq", [Tvar x'; t]),
		    Pforall (x, ty,
			     Pimplies
                               (Papp ("eq", [Tvar x; Tvar x']),
			        p))))
  end

let pforall (x,ty,p) =
  let fresh = fresh_name x in
  let p = subst_in_predicate x (Tvar fresh) p in
  Pforall(fresh, ty, p)

let pexists (x,ty,p) =
  let fresh = fresh_name x in
  let p = subst_in_predicate x (Tvar fresh) p in
  Pexists(fresh, ty, p)
