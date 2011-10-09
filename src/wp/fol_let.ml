(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open Fol

(* -------------------------------------------------------------------------- *)
(* --- Alpha Conversion                                                   --- *)
(* -------------------------------------------------------------------------- *)

(* alpha-conversion *)

module Smap = Datatype.String.Map

type sigma = {
  cpt : int Smap.t ;
  var : term Vmap.t ;
}

let fresh sigma x =
  let base = Var.basename x in
  let tau = Var.var_type x in
  let k = try Smap.find base sigma.cpt with Not_found -> 0 in
  let y = Var.ident_named_var base k tau in
  y , { cpt = Smap.add base (succ k) sigma.cpt ;
        var = Vmap.add x (e_var y) sigma.var }

let alpha sigma x =
  try Vmap.find x sigma.var
  with Not_found -> Wp_parameters.fatal "Unbound fol-variable %s (let)" (Var.var_name x)

let rec def = function Pnamed(_,p) -> def p | p -> p
let rec redef p p' = match p with Pnamed(a,p0) -> Pnamed(a,redef p0 p') | _ -> p'
let rec is_simple = function (Tvar _ | Tconst _ | Tapp(_,[])) -> true | _ -> false
let bind x v sigma = { sigma with var = Vmap.add x v sigma.var }

let rec term sigma = function
  | Tconst _ as c -> c
  | Tvar v -> alpha sigma v
  | Tgetfield(f,r) -> e_getfield f (term sigma r)
  | Tsetfield(f,r,v) -> e_setfield f (term sigma r) (term sigma v)
  | Taccess(t,i) -> e_access (term sigma t) (term sigma i)
  | Tupdate(t,i,v) -> e_update (term sigma t) (term sigma i) (term sigma v)
  | Tapp (n,tl) -> e_app n (List.map (term sigma) tl)
  | Tif (t1,t2,t3) -> e_if (term sigma t1) (term sigma t2) (term sigma t3)
  | Tlet (x,v,t) ->
      let v = term sigma v in
      if is_simple v && Wp_parameters.Simpl.get()
      then
        term (bind x v sigma) t
      else
        let x,sigma = fresh sigma x in
        e_let x v (term sigma t)

let rec pred sigma = function
  | Papp(f,ts) -> p_app f (List.map (term sigma) ts)
  | Ptrue -> Ptrue
  | Pfalse -> Pfalse
  | Pif(e,p,q) -> p_if (term sigma e) (pred sigma p) (pred sigma q)
  | Pand(p,q) -> p_and (pred sigma p) (pred sigma q)
  | Por(p,q) -> p_or (pred sigma p) (pred sigma q)
  | Piff(p,q) -> p_iff (pred sigma p) (pred sigma q)
  | Pnot p -> p_not (pred sigma p)
  | Pnamed(a,p) -> p_named a (pred sigma p)
  | Pforall(x,p) -> let x,sigma = fresh sigma x in p_forall x (pred sigma p)
  | Pexists(x,p) -> let x,sigma = fresh sigma x in p_exists x (pred sigma p)
  | Plet(x,v,p) ->
      let v = term sigma v in
      if is_simple v && Wp_parameters.Simpl.get()
      then
        pred (bind x v sigma) p
      else
        let x,sigma = fresh sigma x in
        p_let x v (pred sigma p)
  | Pimplies(p,q) ->
      match def p with
        | Papp(("eq"|"eq_int"|"eq_real"),([Tvar x;e] | [e;Tvar x])) when Wp_parameters.Simpl.get() ->
            let ve = term sigma e in
            if is_simple ve then
              let vx = alpha sigma x in
              let q = pred (bind x ve sigma) q in
              let p = redef p (Papp("eq",[vx;ve])) in
              p_implies p q
            else
              let vx = alpha sigma x in
              let q = pred sigma q in
              let p = redef p (Papp("eq",[vx;ve])) in
              p_implies p q
        | _ ->
            let p = pred sigma p in
            let q = pred sigma q in
            match def p with
              | Papp("eq",([Tvar x;v] | [v;Tvar x]))
                  when is_simple v && Wp_parameters.Simpl.get() ->
                  let q = Fol.pred_replace (fun _ -> None) x v q in
                  p_implies p q
              | _ -> p_implies p q

let empty =  { cpt = Smap.empty ; var = Vmap.empty }
 
let compile = pred empty

let rec fresh_params ys sigma = function
  | [] -> List.rev ys , sigma
  | x::xs ->
      let y,sigma = fresh sigma x in
      fresh_params (y::ys) sigma xs

let compile_def xs p =
  let ys,sigma = fresh_params [] empty xs in
  ys , pred sigma p
  
