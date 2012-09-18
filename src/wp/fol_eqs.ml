(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
(* ---  Remove let with unversally quantified variables                     *)
(* ------------------------------------------------------------------------ *)

open Fol

module Smap = Datatype.String.Map

let fresh global sigma x =
  let base = Var.basename x in
  let tau = Var.var_type x in
  let k = try Smap.find base !global with Not_found -> 0 in
  let v = Var.ident_named_var base k tau in
  global := Smap.add base (succ k) !global ;
  v , Vmap.add x (e_var v) sigma

let alpha sigma x =
  try Vmap.find x sigma
  with Not_found -> Wp_parameters.fatal "Unbound fol-variable %s (eqs)" (Var.var_name x)


(* -------------------------------------------------------------------------- *)
(* --- Removal of lets                                                    --- *)
(* -------------------------------------------------------------------------- *)

(* "\let x=e; P" is equivalent
   to  "\forall T x; x==e ==> P"
   and "\exists T x; x==e &&  P".
   If the \let has a positive polarity then the \forall form is used.
   Otherwise, the \exists form is used.
   Provers such Alt-ergo give better result with this heuristic. *)

let rec term global defs sigma = function
  | Tconst _ as c -> c
  | Tvar v -> alpha sigma v
  | Tgetfield(f,r) -> e_getfield f (term global defs sigma r)
  | Tsetfield(f,r,v) -> e_setfield f (term global defs sigma r) (term global defs sigma v)
  | Taccess(t,i) -> e_access (term global defs sigma t) (term global defs sigma i)
  | Tupdate(t,i,v) -> e_update (term global defs sigma t) (term global defs sigma i) (term global defs sigma v)
  | Tapp (n,tl) -> e_app n (List.map (term global defs sigma) tl)
  | Tif (t1,t2,t3) -> e_if (term global defs sigma t1) (term global defs sigma t2) (term global defs sigma t3)
  | Tlet (x,v,t) ->
      let v = term global defs sigma v in
      let x,sigma = fresh global sigma x in
      defs := (x,v) :: !defs ;
      term global defs sigma t

let flush pol defs p =
  if pol then
    List.fold_left
      (fun p (x,_) -> p_forall x p)
      (List.fold_left
	 (fun p (x,v) ->
            p_implies (p_eq (e_var x) v) p
	 ) p defs)
      defs
  else
    List.fold_left
      (fun p (x,_) -> p_exists x p)
      (List.fold_left
	 (fun p (x,v) ->
            p_and (p_eq (e_var x) v) p
	 ) p defs)
      defs
 
let rec pred pol global sigma = function
  | Ptrue -> Ptrue
  | Pfalse -> Pfalse
  | Pimplies(p,q) -> p_implies (pred (not pol) global sigma p) (pred pol global sigma q)
  | Pand(p,q) -> p_and (pred pol global sigma p) (pred pol global sigma q)
  | Por(p,q) -> p_or (pred pol global sigma p) (pred pol global sigma q)
  | Piff(p,q) -> p_iff (pred pol global sigma p) (pred pol global sigma q)
  | Pnot p -> p_not (pred (not pol) global sigma p)
  | Pnamed(a,p) -> p_named a (pred pol global sigma p)
  | Pforall(x,p) -> let x,sigma = fresh global sigma x in p_forall x (pred pol global sigma p)
  | Pexists(x,p) -> let x,sigma = fresh global sigma x in p_exists x (pred pol global sigma p)

  | Papp(f,es) ->
      let defs = ref [] in
      let es = List.map (term global defs sigma) es in
      flush pol !defs (p_app f es)

  | Pif(e,p,q) ->
      let defs = ref [] in
      let e = term global defs sigma e in
      let p = pred pol global sigma p in
      let q = pred pol global sigma q in
      flush pol !defs (p_if e p q)

  | Plet(x,v,p) ->
      let defs = ref [] in
      let v = term global defs sigma v in
      let x,sigma = fresh global sigma x in
      let p = pred pol global sigma p in
      flush pol ( (x,v)::!defs ) p

let compile p = pred true (ref Smap.empty) Vmap.empty p
