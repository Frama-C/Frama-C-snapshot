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

type var = string

type 'd d_term =
  | Tconst of constant
  | Tdata of 'd
  | Tapp of string * 'd d_term list
  | Tif of 'd d_term * 'd d_term * 'd d_term

type term = var d_term

type 't t_pred =
  | Pvar of string
  | Papp of string * 't list
  | Ptrue
  | Pfalse
  | Pimplies of 't t_pred * 't t_pred
  | Pif of 't * 't t_pred * 't t_pred
  | Pand of 't t_pred * 't t_pred
  | Por of 't t_pred * 't t_pred
  | Pxor of 't t_pred * 't t_pred
  | Piff of 't t_pred * 't t_pred
  | Pnot of 't t_pred
  | Pforall of string * pure_type * 't t_pred
  | Pexists of string * pure_type * 't t_pred
  | Pnamed of string * 't t_pred

type 'd d_pred = ('d d_term) t_pred
type predicate = term t_pred

type 't gen_decl =
  | Function of string * pure_type list * pure_type
  | Predicate of string * pure_type list
  | Axiom of string * 't t_pred
  | Goal of string * 't t_pred
  | Type of pure_type

type decl = term gen_decl

let pand (p1, p2) = match p1, p2 with
  | Ptrue, p2 -> p2
  | p1, Ptrue -> p1
  | p1, p2 -> Pand (p1, p2)

let pands l p = List.fold_right (fun p1 p2 -> pand (p1, p2)) l p

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

let pors l p = List.fold_right (fun p1 p2 -> por (p1, p2)) l p

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


let rec change_exp_in_pred do_exp quantif_do_exp p =
  let subst_pred = change_exp_in_pred do_exp quantif_do_exp in
  match p with
  | Ptrue -> Ptrue
    | Pfalse -> Pfalse 
  | Pvar v -> Pvar v
  | Pif (t,p1,p2) -> 
      Pif (do_exp t, subst_pred p1, subst_pred p2)
  | Pnot p -> pnot (subst_pred p)
  | Pforall (v,pt,p') -> 
      let f = quantif_do_exp do_exp v in
        Pforall (v,pt,change_exp_in_pred f quantif_do_exp p')
  | Pexists (v,pt,p') -> 
      let f = quantif_do_exp do_exp v in
        Pforall (v,pt,change_exp_in_pred f quantif_do_exp p')
  | Pnamed (n,p) -> Pnamed (n,subst_pred p)
  | Pimplies (p1,p2) -> pimplies (subst_pred p1,subst_pred p2)
  | Pand (p1,p2) -> pand (subst_pred p1,subst_pred p2)
  | Por (p1,p2) -> por (subst_pred p1,subst_pred p2)
  | Pxor(p1,p2) -> pxor (subst_pred p1, subst_pred p2)
  | Piff (p1,p2) -> piff (subst_pred p1,subst_pred p2)
  | Papp (n,t) -> papp (n,List.map do_exp t)

let rec change_data_in_exp do_data t = match t with
| Tconst c -> Tconst c
| Tdata v -> do_data v
| Tapp (n,tl) -> Tapp(n, List.map (change_data_in_exp do_data) tl)
| Tif (t1,t2,t3) -> Tif(change_data_in_exp do_data t1,
                        change_data_in_exp do_data t2,
                        change_data_in_exp do_data t3)

let change_data_in_pred do_data quantif_do_data p =
  let do_exp e = change_data_in_exp do_data e in
  let quantif_do_exp f_exp v = 
    let f_data = fun d -> f_exp (Tdata d) in
    let new_f_data = quantif_do_data f_data v in
    let new_f_exp = change_data_in_exp new_f_data in
      new_f_exp
  in
    change_exp_in_pred do_exp quantif_do_exp p

let translate_data_in_pred do_data p =
  let do_exp e = change_data_in_exp do_data e in
  let quantif_do_exp f_exp _ = f_exp in
    change_exp_in_pred do_exp quantif_do_exp p

let subst_vars_in_predicate f_x_exp_opt p =
  let do_data d = match f_x_exp_opt d with | Some e -> e | None -> Tdata d in
  let quantif_do_data f v = 
    (fun d -> if d = v then Tdata d else f d) in
    change_data_in_pred do_data quantif_do_data p

let subst_in_predicate x exp p =
  subst_vars_in_predicate (fun v -> if v = x then Some exp else None) p

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
                   (Papp ("eq", [Tdata x'; t]),
		    Pforall (x, ty,
			     Pimplies
                               (Papp ("eq", [Tdata x; Tdata x']),
			        p))))
  end

let pforall (x,ty,p) =
  let fresh = fresh_name x in
  let p = subst_in_predicate x (Tdata fresh) p in
  Pforall(fresh, ty, p)

let pexists (x,ty,p) =
  let fresh = fresh_name x in
  let p = subst_in_predicate x (Tdata fresh) p in
  Pexists(fresh, ty, p)
