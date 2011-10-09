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

(* III. let expansion *)

let new_fname f x sigma cpt =
  incr cpt;
  let sx = Var.basename x in
  let nx = Var.ident_named_var sx !cpt (Var.var_type x) in
  nx,(Vmap.add x (f nx) sigma)

let rec expt sigma = function
  | Tlet(x,t1,t2) ->
      let t1 = expt sigma t1 in
      expt (Vmap.add x t1 sigma) t2
  | Tapp(f,tl) ->
      e_app f  (List.map (expt sigma) tl)
  | Tif (c,ta,tb) ->
      e_if (expt sigma c)(expt sigma ta)(expt sigma tb)
  | Tconst _ as c -> c
  | Taccess(t,i) -> e_access(expt sigma t) (expt sigma i)
  | Tupdate(t,i,v) -> e_update(expt sigma t) (expt sigma i) (expt sigma v)
  | Tgetfield(f,r) -> e_getfield f (expt sigma r)
  | Tsetfield(f,r,v) -> e_setfield f (expt sigma r) (expt sigma v)
  | Tvar x as t ->
      begin
        try Vmap.find x sigma
        with Not_found -> t
      end

 let rec expp sigma cpt = function
  | Pimplies (p1,p2) ->
    p_implies (expp sigma cpt p1)(expp sigma cpt p2)

  | Pif (t,p1,p2) ->
      p_if (expt sigma t)
        (expp sigma cpt p1)(expp sigma cpt p2)
  | Pand (p1,p2) -> p_and(expp sigma cpt p1)(expp sigma cpt p2)
  | Por (p1,p2) -> p_or(expp sigma cpt p1)(expp sigma cpt p2)
  | Piff (p1,p2) -> p_iff (expp sigma cpt p1)(expp sigma cpt p2)
  | Pnot p -> p_not (expp sigma cpt p)
  | Papp (f, tl) -> p_app f (List.map (expt sigma) tl)
  | Pexists(x,p) ->
      let nx,sigma = new_fname e_var x sigma cpt in
      p_exists nx (expp sigma cpt p)
  | Pforall (x,p) ->
      let nx,sigma = new_fname e_var x sigma cpt in
      p_forall nx (expp sigma cpt p)
  | Plet (x,t,p) ->
      let t1 = expt sigma t in
      expp (Vmap.add x t1 sigma) cpt p
  | Pnamed(s,p) -> Pnamed(s,expp sigma cpt p)
  | p -> p

 let plet_expansion p =
   expp Vmap.empty (ref 0) p

 let elet_expansion t =
   expt Vmap.empty t
