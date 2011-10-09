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

(* ------------------------------------------------------------------------ *)
(* ---  Remove let with Closure Conversion                                  *)
(* ------------------------------------------------------------------------ *)

open Fol

(* I. such as closure conversion :
   This section is for generated terms and predicate
   for ergo and for coq output.
   Tlet (x,t1,t2) becomes Tapp(f,t1::free_vars t2)
   when f is a function such that :
   {name = f; param =x::free_vars t2 ; body = t2}
   Plet (x,t,p) becomes Pforall(nx,Pimplies(Papp("eq",[Tvar nx;t]),p)

*)

(* [free_vars bvars fvars t] computes the free variables of [t] without
   repetition, accroding to the already bound variables [bvars] and
   the already found free variables [fvars] *)
let rec free_vars bvars fvars = function
  | Tlet(x,t1,t2) ->
      let f1 = free_vars bvars fvars t1 in
      free_vars (Vset.add x bvars) f1 t2
  | Tapp(_f,tl) ->
      (List.fold_left (fun f t -> free_vars bvars f t) fvars tl)
  | Tif (c,tt,tf) ->
      let fc = free_vars bvars fvars c in
      let ftt = free_vars bvars fc tt in
      free_vars bvars ftt tf
  | Tvar x ->
      if Vset.mem x bvars  then fvars else (Vset.add x fvars)
  | _ -> fvars


type f_let =
    { name : string ;
      param : Var.t list ;
      body :  term;}

let mk_def xl t defs =
  let f = "_let_"^(string_of_int (snd !defs) ) in
  defs :=
    ({name = f ; param = xl; body = t}::(fst !defs)),
  1+(snd !defs);f

let new_name x sigma cpt =
  incr cpt;
  let sx = Var.basename x^(string_of_int !cpt) in
  let nx = Var.fresh_named_var sx (Var.var_type x) in
  nx,(Vmap.add x nx sigma)

let rec unlet_term sigma bvars defs = function
  | Tlet(x,t1,t2) ->
      let fvars = free_vars (Vset.add x bvars) Vset.empty t2 in
      let args = x::(Vset.elements fvars) in
      let t2' = unlet_term sigma (Vset.add x bvars) defs t2 in
      let f = mk_def  args t2' defs in
      e_app f
        ((unlet_term sigma bvars defs t1)::
           (List.map (fun x -> e_var x) args))
  | Tapp(f,tl) ->
      e_app f  (List.map (fun t -> unlet_term sigma bvars defs t) tl)
  | Tif (c,tt,tf) ->
      e_if (unlet_term sigma bvars defs c)
          (unlet_term sigma bvars defs tt)
          (unlet_term sigma bvars defs tf)
  | Tvar x -> e_var (try Vmap.find x sigma with Not_found -> x)
  | t -> t

let rec unlet_pred sigma bvars defs cpt = function
  | Papp (f, tl) ->
      p_app f (List.map (fun t -> unlet_term sigma bvars defs t) tl)
  | Pimplies (p1,p2) ->
      Pimplies(unlet_pred sigma bvars defs cpt p1,
               unlet_pred sigma bvars defs cpt p2)
  | Pif (t,p1,p2) ->
      Pif(unlet_term sigma bvars defs t,
          unlet_pred sigma bvars defs cpt p1,
          unlet_pred sigma bvars defs cpt p2)
  | Pand (p1,p2) ->
      Pand(unlet_pred sigma bvars defs cpt p1, unlet_pred sigma bvars defs cpt p2)
  | Por (p1,p2) ->
      Por(unlet_pred sigma bvars defs cpt p1, unlet_pred sigma bvars defs cpt p2)
  | Piff (p1,p2) ->
      Piff(unlet_pred sigma bvars defs cpt p1, unlet_pred sigma bvars defs cpt p2)
  | Pnot p -> Pnot (unlet_pred sigma bvars defs cpt p)
  | Pforall (x,p) ->
      let nx,nsigma = new_name x sigma cpt in
      Pforall(nx,unlet_pred nsigma (Vset.add x bvars) defs cpt p)
  | Pexists(x,p) ->
      let nx,nsigma = new_name x sigma cpt in
      Pexists(nx,unlet_pred nsigma (Vset.add x bvars) defs cpt p)
  | Plet (x,t,p) ->
      let nx,nsigma =  new_name x sigma cpt in
      let b = Vset.add x bvars in
      p_forall nx
        (p_implies
           (p_app "eq" [e_var nx;unlet_term sigma b defs t])
           (unlet_pred nsigma b defs cpt p))
  | Pnamed(s,p) -> Pnamed(s,unlet_pred sigma bvars defs cpt p)
  | p -> p


let unlet p =
  let cpt = ref 0 in
  let defs= ref ([] , 0) in
  let p' = unlet_pred Vmap.empty Vset.empty defs cpt p in
  (fst !defs),p'
