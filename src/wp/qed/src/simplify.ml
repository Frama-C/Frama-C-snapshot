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
(* --- Solver infrastructure for Qed                                      --- *)
(* -------------------------------------------------------------------------- *)

open Logic

exception Absurd
exception Unknown
  
module Make(T : Logic.Term) =
struct

  open T
  
  type var = T.term
  type exp = var T.expression

  module Vmap = Tmap
  module Vset = Tset

  let vtrue = T.e_true
  let vfalse = T.e_false
      
  let map f r = match r with
    | Bind _ -> r
    | _ -> T.repr (T.r_map f r)

  let vmap eq map merge m =
    Tmap.fold
      (fun x a m ->
         let y = eq x in
         let b = map a in
         let b = try merge b (Tmap.find y m) with Not_found -> b in
         Tmap.add y b m
      ) m Vmap.empty

  class type context =
    object
      method build : exp -> var
      method query : exp -> maybe
      method infer : exp -> unit
    end

  class type theory =
    object
      method copy : theory
      method define : var -> exp -> unit
      method assume : exp -> bool -> unit
      method rewrite : Vset.t -> (var -> var) -> unit
      method resolve : context -> exp -> var
    end
  
  type state = {
    theories : theory list ;
    mutable variables : Tset.t ;
    mutable merged : Tset.t ;
    mutable equalities : term Tmap.t ;
    mutable hypotheses : Tset.t ;
    mutable queries : Tset.t ;
  }

  let rec find s a =
    try
      let a0 = Tmap.find a s.equalities in
      let a1 = find s a0 in
      if a0 != a1 then s.equalities <- Tmap.add a a1 s.equalities ; a1
    with Not_found -> a

  let rec register s a =
    match T.repr a with
    | True | False | Kint _ | Kreal _ | Var _ | Bind _ -> a
    | _ ->
        if Tset.mem a s.variables then find s a
        else
          begin
            (* a is nor defined, nor in lookup *)
            let r = T.repr (find s (T.e_map (register s) a)) in
            (* a and r are generally (==) *)
            List.iter (fun th -> th#define a r) s.theories ;
            s.variables <- Tset.add a s.variables ; a
          end

  let rewrite s a b =
    let a = find s a in
    let b = find s b in
    begin
      s.equalities <- Tmap.add a b s.equalities ;
      s.merged <- Tset.add a s.merged ;
    end

  let propagate s =
    begin
      let hs = s.hypotheses in
      s.hypotheses <- Tset.empty ;
      Tset.iter
        (fun h ->
           rewrite s h e_true ;
           rewrite s (e_not h) e_false ;
           let v,l = T.literal h in
           let e = T.repr l in
           List.iter (fun th -> th#assume e v) s.theories ;
        ) hs ;
      let domain = s.merged in
      s.merged <- Tset.empty ;
      List.iter
        (fun th -> th#rewrite domain (find s))
        s.theories ;
    end

  let create ths = {
    theories = ths ;
    variables = Tset.empty ;
    merged = Tset.empty ;
    equalities = Tmap.empty ;
    hypotheses = Tset.empty ;
    queries = Tset.empty ;
  }

  let copy s = { s with theories = List.map (fun th -> th#copy) s.theories }

  let rec hyps s p =
    match T.repr p with
    | And ps -> List.iter (hyps s) ps
    | True -> ()
    | False -> raise Absurd
    | Eq(a,b) ->
        let a = register s a in
        let b = register s b in
        let cmp = T.compare a b in
        begin
          if cmp < 0 then rewrite s b a ;
          if cmp > 0 then rewrite s a b ;
        end
    | _ ->
        let p = register s p in
        s.hypotheses <- Tset.add p s.hypotheses
            
  let assume s p = hyps s p ; propagate s

  class local s =
    object

      method build = function
        | Bind(q,x,p) -> e_bind q x p
        | r -> find s (r_map (find s) r)
                 
      method query r =
        let e = find s (T.e_repr r) in
        match T.repr e with
        | True -> Logic.Yes
        | False -> Logic.No
        | _ -> s.queries <- Tset.add e s.queries ; Logic.Maybe

      method infer r = hyps s (T.e_repr r)

    end

  let resolve s e =
    let c = new local s in
    List.iter
      (fun th ->
         try
           let r = th#resolve c (T.repr e) in
           rewrite s e r ;
         with Unknown -> ()
      ) s.theories ;
    find s e

  let fixpoint s =
    begin
      let qs = s.queries in
      s.queries <- Tset.empty ;
      Tset.iter
        (fun q ->
           let q' = resolve s q in
           if not (T.is_primitive q') then 
             s.queries <- Tset.add q' s.queries ;
        ) qs ;
      Tset.equal qs s.queries ;
    end

  let rec query s a =
    if not (T.is_primitive a) && not (Tset.mem a s.queries) then
      begin
        s.queries <- Tset.add a s.queries ;
        T.e_iter (query s) a ;
      end

  let rec rebuild s m xs e =
    try Tmap.find e !m
    with Not_found -> 
        let m = match T.repr e with Bind _ -> ref Tmap.empty | _ -> m in
        let r0 = T.f_map (rebuild s m) xs e in
        let r1 = if Vars.intersect (T.vars e) xs then r0 else find s r0 in
        m := Tmap.add e r1 !m ; r1

  let simplify s ?(timeout=128) e =
    let e = register s e in
    query s e ; 
    let n = ref timeout in
    while !n > 0 && not (fixpoint s) do decr n done ;
    rebuild s (ref Tmap.empty) Vars.empty e

end
