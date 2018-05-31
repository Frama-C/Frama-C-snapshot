(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
(* ---     First Order Logic                                              --- *)
(* -------------------------------------------------------------------------- *)

open Hcons
open Logic

module Make
    (ADT : Logic.Data)
    (Field : Logic.Field)
    (Fun : Logic.Function) =
struct

  (* -------------------------------------------------------------------------- *)

  type tau = (Field.t,ADT.t) Logic.datatype
  type path = int list

  module Tau = Kind.MakeTau(Field)(ADT)

  module POOL = Pool.Make
      (struct
        type t = tau
        let dummy = Prop
        let equal = Kind.eq_tau Field.equal ADT.equal
      end)

  open POOL
  type var = POOL.var

  module VID = struct type t = var let id x = x.vid end
  module Vars = Idxset.Make(VID)
  module Vmap = Idxmap.Make(VID)

  type term = {
    id : int ;
    hash : int ;
    size : int ;
    vars : Vars.t ;
    bind : Bvars.t ;
    sort : sort ;
    repr : repr ;
  }
  and repr = (Field.t,ADT.t,Fun.t,var,term,term) term_repr

  type bind = term

  type 'a expression = (Field.t,ADT.t,Fun.t,var,bind,'a) term_repr

  (* ------------------------------------------------------------------------ *)
  (* ---  Term Set,Map and Vars                                           --- *)
  (* ------------------------------------------------------------------------ *)

  module E =
  struct
    type t = term
    let id t = t.id
  end
  module Tset = Idxset.Make(E)
  module Tmap = Idxmap.Make(E)
  
  (* ------------------------------------------------------------------------ *)
  (* ---  Parameters                                                      --- *)
  (* ------------------------------------------------------------------------ *)

  module ADT = ADT
  module Field = Field
  module Fun = Fun
  module Var : Variable with type t = var =
  struct
    type t = var
    let hash x = x.vid
    let equal = (==)
    let compare = POOL.compare
    let pretty = POOL.pretty
    let debug x = Printf.sprintf "%s_%d" x.vbase x.vid
    let basename x = x.vbase
    let sort x = Kind.of_tau x.vtau
    let dummy = POOL.dummy
  end

  (* -------------------------------------------------------------------------- *)
  (* ---  Variables                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let tau_of_var x = x.vtau
  let sort_of_var x = Kind.of_tau x.vtau
  let base_of_var x = x.vbase

  type pool = POOL.pool
  let pool = POOL.create

  let add_var pool x = POOL.add pool x
  let add_vars pool xs = Vars.iter (POOL.add pool) xs
  let add_term pool t = Vars.iter (POOL.add pool) t.vars

  let fresh pool ?basename tau =
    let base = match basename with
      | Some base -> base | None -> Tau.basename tau
    in POOL.fresh pool base tau

  let alpha pool x = POOL.alpha pool x

  let rec basename t = match t.repr with
    | Kint _ -> "x"
    | Kreal _ -> "r"
    | Aset(a,_,_) -> basename a
    | Acst _ -> "a"
    | _ -> Kind.basename t.sort

  (* -------------------------------------------------------------------------- *)
  (* ---  Representation                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let repr e = e.repr
  let hash e = e.hash
  let id e = e.id

  let hash_subterms = function
    | False -> 0
    | True  -> 0
    | Kint n -> Z.hash n
    | Kreal x -> hash_pair (Z.hash x.Q.num) (Z.hash x.Q.den)
    | Times(n,t) -> Z.hash n * t.hash
    | Add xs | Mul xs | And xs | Or xs -> hash_list hash 0 xs
    | Div(x,y) | Mod(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y)
    | Aget(x,y) -> hash_pair x.hash y.hash
    | Acst(_,v) -> v.hash
    | Not e -> succ e.hash
    | Imply(hs,p) -> hash_list hash p.hash hs
    | If(e,a,b) | Aset(e,a,b) -> hash_triple e.hash a.hash b.hash
    | Fun(f,xs) -> hash_list hash (Fun.hash f) xs
    | Rdef fxs ->
        hash_list (fun (f,x) -> hash_pair (Field.hash f) x.hash) 0 fxs
    | Rget(e,f) -> hash_pair e.hash (Field.hash f)
    | Fvar x -> Var.hash x
    | Bvar(k,_) -> k
    | Bind(Forall,_,e) -> 1 + 31 * e.hash
    | Bind(Exists,_,e) -> 2 + 31 * e.hash
    | Bind(Lambda,_,e) -> 3 + 31 * e.hash
    | Apply(a,xs) -> hash_list hash a.hash xs

  let hash_head = function
    | False   ->  0
    | True    ->  1
    | Kint _  ->  2
    | Kreal _ ->  3
    | Times _ ->  4
    | Add _   ->  5
    | Mul _   ->  6
    | And _   ->  7
    | Or _    ->  8
    | Div _   ->  9
    | Mod _   -> 10
    | Eq _    -> 11
    | Neq _   -> 12
    | Leq _   -> 13
    | Lt  _   -> 14
    | Not _   -> 15
    | Imply _ -> 16
    | If _    -> 17
    | Fun _   -> 18
    | Fvar _  -> 19
    | Bvar _  -> 20
    | Bind _  -> 21
    | Apply _ -> 22
    | Aset _  -> 23
    | Aget _  -> 24
    | Acst _  -> 25
    | Rdef _  -> 26
    | Rget _  -> 27

  let hash_repr t = hash_head t + 31 * hash_subterms t

  let equal_repr a b =
    match a,b with
    | True , True -> true
    | False , False -> true
    | Kint n , Kint m -> Z.equal n m
    | Kreal x , Kreal y -> Q.equal x y
    | Times(n,x) , Times(m,y) -> x==y && Z.equal n m
    | Add xs , Add ys
    | Mul xs , Mul ys
    | And xs , And ys
    | Or  xs , Or  ys -> eq_list xs ys
    | Div(x,y) , Div(x',y')
    | Mod(x,y) , Mod(x',y')
    | Eq(x,y) , Eq(x',y')
    | Neq(x,y) , Neq(x',y')
    | Leq(x,y) , Leq(x',y')
    | Lt(x,y) , Lt(x',y')
    | Aget(x,y) , Aget(x',y') -> x==x' && y==y'
    | Not a , Not b -> a==b
    | Imply(hs,p) , Imply(hs',q) -> p==q && eq_list hs hs'
    | If(e,a,b) , If(e',a',b')
    | Aset(e,a,b) , Aset(e',a',b') -> e==e' && a==a' && b==b'
    | Fun(f,xs) , Fun(g,ys) -> Fun.equal f g && eq_list xs ys
    | Fvar x , Fvar y -> Var.equal x y
    | Bvar(k,t) , Bvar(k',t') -> k = k' && Tau.equal t t'
    | Bind(q,t,e) , Bind(q',t',e') -> q=q' && Tau.equal t t' && e==e'
    | Acst(t,v) , Acst(t',v') -> Tau.equal t t' && v==v'
    | Apply(x,ys) , Apply(x',ys') -> x==x' && eq_list ys ys'
    | Rget(x,f) , Rget(x',g) -> x==x' && Field.equal f g
    | Rdef fxs , Rdef gys ->
        equal_list (fun (f,x) (g,y) -> x==y && Field.equal f g) fxs gys
    | _ ->
        assert (hash_head a <> hash_head b) ; false

  let sort x = x.sort
  let vars x = x.vars
  let bvars x = x.bind

  let vars_repr = function
    | True | False | Kint _ | Kreal _ -> Vars.empty
    | Times(_,x) | Not x | Rget(x,_) | Acst(_,x) -> x.vars
    | Add xs | Mul xs | And xs | Or xs | Fun(_,xs) ->
        Hcons.fold_list Vars.union (fun x -> x.vars) Vars.empty xs
    | Div(x,y) | Mod(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y) | Aget(x,y) ->
        Vars.union x.vars y.vars
    | Imply(xs,a) | Apply(a,xs) ->
        Hcons.fold_list Vars.union vars a.vars xs
    | If(e,a,b) | Aset(e,a,b) -> Vars.union e.vars (Vars.union a.vars b.vars)
    | Fvar x -> Vars.singleton x
    | Bvar _ -> Vars.empty
    | Bind(_,_,e) -> e.vars
    | Rdef fxs -> List.fold_left (fun s (_,x) -> Vars.union s x.vars) Vars.empty fxs

  let bind_repr = function
    | True | False | Kint _ | Kreal _ -> Bvars.empty
    | Times(_,x) | Not x | Rget(x,_) | Acst(_,x) -> x.bind
    | Add xs | Mul xs | And xs | Or xs | Fun(_,xs) ->
        Hcons.fold_list Bvars.union (fun x -> x.bind) Bvars.empty xs
    | Div(x,y) | Mod(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y) | Aget(x,y) ->
        Bvars.union x.bind y.bind
    | Imply(xs,a) | Apply(a,xs) ->
        Hcons.fold_list Bvars.union bvars a.bind xs
    | If(e,a,b) | Aset(e,a,b) -> Bvars.union e.bind (Bvars.union a.bind b.bind)
    | Bvar(k,_) -> Bvars.singleton k
    | Fvar _ -> Bvars.empty
    | Bind(_,_,e) -> Bvars.bind e.bind
    | Rdef fxs -> List.fold_left (fun s (_,x) -> Bvars.union s x.bind) Bvars.empty fxs

  let sort_repr = function
    | True | False -> Sbool
    | Kint _ -> Sint
    | Kreal _ -> Sreal
    | Times(_,x) -> Kind.merge Sint x.sort
    | Add xs | Mul xs -> Kind.merge_list sort Sint xs
    | And xs | Or xs ->  Kind.merge_list sort Sbool xs
    | Imply(hs,p) -> Kind.merge_list sort p.sort hs
    | Not x -> x.sort
    | Fun(f,_) -> Fun.sort f
    | Aget(m,_) -> Kind.image m.sort
    | Aset(m,_,_) -> m.sort
    | Acst(_,v) -> Sarray v.sort
    | Rget(_,f) -> Field.sort f
    | Rdef _ -> Sdata
    | Div(x,y) | Mod(x,y) -> Kind.merge x.sort y.sort
    | Leq _ | Lt _ -> Sbool
    | Apply(x,_) -> x.sort
    | If(_,a,b) -> Kind.merge a.sort b.sort
    | Fvar x -> Kind.of_tau x.vtau
    | Bvar(_,t) -> Kind.of_tau t
    | Bind((Forall|Exists),_,_) -> Sprop
    | Bind(Lambda,_,e) -> e.sort
    | Eq(a,b) | Neq(a,b) ->
        match a.sort , b.sort with
        | Sprop , _ | _ , Sprop -> Sprop
        | _ -> Sbool

  let rec size_list n w = function
    | [] -> n+w
    | x::xs -> size_list (succ n) (max w x.size) xs

  let rec size_rdef n w = function
    | [] -> n+w
    | (_,x)::fxs -> size_rdef (succ n) (max w x.size) fxs

  let size_repr = function
    | True | False | Kint _ -> 0
    | Fvar _ | Bvar _ | Kreal _ -> 1
    | Times(_,x) -> succ x.size
    | Add xs | Mul xs | And xs | Or xs -> size_list 1 0 xs
    | Imply(hs,p) -> size_list 1 p.size hs
    | Not x -> succ x.size
    | Fun(_,xs) -> size_list 1 0 xs
    | Aget(a,b) -> 1 + max a.size b.size
    | Aset(a,b,c) -> 1 + max a.size (max b.size c.size)
    | Acst(_,v) -> succ v.size
    | Rget(a,_) -> succ a.size
    | Rdef fxs -> 1 + size_rdef 0 0 fxs
    | Div(x,y) | Mod(x,y) -> 2 + max x.size y.size
    | Eq(x,y) | Neq(x,y) | Lt(x,y) | Leq(x,y) -> 1 + max x.size y.size
    | Apply(x,xs) -> size_list 1 x.size xs
    | If(a,b,c) -> 2 + max a.size (max b.size c.size)
    | Bind(_,_,p) -> 3 + p.size

  (* -------------------------------------------------------------------------- *)
  (* --- Symbols                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  type t = term
  let equal = (==)

  let is_atomic e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Fvar _ | Bvar _ -> true
    | _ -> false

  let is_simple e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Fvar _ | Bvar _ | Fun(_,[]) -> true
    | _ -> false

  let is_closed e = Vars.is_empty e.vars

  let is_prop e = match e.sort with
    | Sprop | Sbool -> true
    | _ -> false

  let is_int e = match e.sort with
    | Sint -> true
    | _ -> false

  let is_real e = match e.sort with
    | Sreal -> true
    | _ -> false

  let is_arith e = match e.sort with
    | Sreal | Sint -> true
    | _ -> false

  (* -------------------------------------------------------------------------- *)
  (* --- Recursion Breakers                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let cached_not = ref (fun _ -> assert false)
  let extern_not = ref (fun _ -> assert false)
  let extern_ite = ref (fun _ -> assert false)
  let extern_eq = ref (fun _ -> assert false)
  let extern_neq = ref (fun _ -> assert false)
  let extern_leq = ref (fun _ -> assert false)
  let extern_lt = ref (fun _ -> assert false)
  let extern_fun = ref (fun _ -> assert false)

  (* -------------------------------------------------------------------------- *)
  (* --- Comparison                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  module COMPARE =
  struct

    let fun_rank f =
      match Fun.category f with
      | Function -> 3
      | Injection -> 2
      | Constructor -> 1
      | Operator _ -> 0

    let cmp_size a b = Pervasives.compare a.size b.size
    let rank_bind = function Forall -> 0 | Exists -> 1 | Lambda -> 2
    let cmp_bind p q = rank_bind p - rank_bind q
    let cmp_field phi (f,x) (g,y) =
      let cmp = Field.compare f g in
      if cmp <> 0 then cmp else phi x y

    let cmp_struct phi a b =
      match a.repr , b.repr with

      | True , True -> 0
      | True , _ -> (-1)
      | _ , True -> 1

      | False , False -> 0
      | False , _ -> (-1)
      | _ , False -> 1

      | Kint a , Kint b -> Z.compare a b
      | Kint _ , _ -> (-1)
      | _ , Kint _ -> 1

      | Kreal a , Kreal b -> Q.compare a b
      | Kreal _ , _ -> (-1)
      | _ , Kreal _ -> 1

      | Fvar x , Fvar y -> Var.compare x y
      | Fvar _ , _ -> (-1)
      | _ , Fvar _ -> 1

      | Bvar(k1,_) , Bvar(k2,_) -> k1 - k2
      | Bvar _ , _ -> (-1)
      | _ , Bvar _ -> 1

      | Eq(a1,b1) , Eq(a2,b2)
      | Neq(a1,b1) , Neq(a2,b2)
      | Lt(a1,b1) , Lt(a2,b2)
      | Leq(a1,b1) , Leq(a2,b2)
      | Div(a1,b1) , Div(a2,b2)
      | Mod(a1,b1) , Mod(a2,b2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = phi a1 a2 in
            if cmp <> 0 then cmp else phi b1 b2
      | Fun(f,xs) , Fun(g,ys) ->
          let cmp = fun_rank f - fun_rank g in
          if cmp <> 0 then cmp else
            let cmp = cmp_size a b in
            if cmp <> 0 then cmp else
              let cmp = Fun.compare f g in
              if cmp <> 0 then cmp else
                Hcons.compare_list phi xs ys
      | Fun (_,[]) , _ -> (-1)  (* (a) as a variable *)
      | _ , Fun (_,[]) -> 1
      | Eq _ , _ -> (-1)        (* (b) equality *)
      |  _ , Eq _ -> 1
      | Neq _ , _ -> (-1)       (* (c) other comparison *)
      |  _ , Neq _ -> 1
      | Lt _ , _ -> (-1)
      |  _ , Lt _ -> 1
      | Leq _ , _ -> (-1)
      |  _ , Leq _ -> 1
      | Fun _ , _ -> (-1)       (* (d) predicate *)
      | _ , Fun _ -> 1

      | Times(a1,x) , Times(a2,y) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = Z.compare a1 a2 in
            if cmp <> 0 then cmp else phi x y
      | Times _ , _ -> (-1)
      | _ , Times _ -> 1

      | Not x , Not y ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            phi x y
      | Not _ , _ -> (-1)
      |  _ , Not _ -> 1

      | Imply(h1,p1) , Imply(h2,p2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            Hcons.compare_list phi (p1::h1) (p2::h2)
      | Imply _ , _ -> (-1)
      |  _ , Imply _ -> 1

      | Add xs , Add ys
      | Mul xs , Mul ys
      | And xs , And ys
      | Or xs , Or ys ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            Hcons.compare_list phi xs ys

      | Add _ , _ -> (-1)
      | _ , Add _ -> 1
      | Mul _ , _ -> (-1)
      | _ , Mul _ -> 1
      | And _ , _ -> (-1)
      | _ , And _ -> 1
      | Or _ , _ -> (-1)
      | _ , Or _ -> 1
      | Div _ , _ -> (-1)
      |  _ , Div _ -> 1
      | Mod _ , _ -> (-1)
      |  _ , Mod _ -> 1

      | If(a1,b1,c1) , If(a2,b2,c2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = phi a1 a2 in
            if cmp <> 0 then cmp else
              let cmp = phi b1 b2 in
              if cmp <> 0 then cmp else phi c1 c2
      | If _ , _ -> (-1)
      |  _ , If _ -> 1

      | Acst(t1,v1) , Acst(t2,v2) ->
          let cmp = Tau.compare t1 t2 in
          if cmp<>0 then cmp else phi v1 v2
      | Acst _ , _ -> (-1)
      | _ , Acst _ -> 1
    
      | Aget(a1,b1) , Aget(a2,b2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = phi a1 a2 in
            if cmp <> 0 then cmp else phi b1 b2
      | Aget _ , _ -> (-1)
      |  _ , Aget _ -> 1

      | Aset(a1,k1,v1) , Aset(a2,k2,v2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = phi a1 a2 in
            if cmp <> 0 then cmp else
              let cmp = phi k1 k2 in
              if cmp <> 0 then cmp else phi v1 v2
      | Aset _ , _ -> (-1)
      |  _ , Aset _ -> 1

      | Rget(r1,f1) , Rget(r2,f2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = phi r1 r2 in
            if cmp <> 0 then cmp else Field.compare f1 f2
      | Rget _ , _ -> (-1)
      |  _ , Rget _ -> 1

      | Rdef fxs , Rdef gys ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            Hcons.compare_list (cmp_field phi) fxs gys
      | Rdef _ , _ -> (-1)
      |  _ , Rdef _ -> 1

      | Apply(a,xs) , Apply(b,ys) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            Hcons.compare_list phi (a::xs) (b::ys)
      | Apply _ , _ -> (-1)
      | _ , Apply _ -> 1

      | Bind(q1,t1,p1) , Bind(q2,t2,p2) ->
          let cmp = cmp_size a b in
          if cmp <> 0 then cmp else
            let cmp = cmp_bind q1 q2 in
            if cmp <> 0 then cmp else
              let cmp = phi p1 p2 in
              if cmp <> 0 then cmp else
                Tau.compare t1 t2

    let rec compare a b =
      if a == b then 0 else
        cmp_struct compare a b

  end

  let weigth e = e.size
  let atom_min a b = if 0 < COMPARE.compare a b then b else a

  let compare a b =
    if a == b then 0
    else
      let a' = if is_prop a then !extern_not a else a in
      let b' = if is_prop b then !extern_not b else b in
      if a == b' || a' == b
      then COMPARE.compare a b
      else COMPARE.compare (atom_min a a') (atom_min b b')

  exception Absorbant

  let compare_raising_absorbant a b =
    if a == b then 0
    else
      let negate ~abs e =
        let ne = !extern_not e in
        if abs == ne then raise Absorbant ; ne in
      let a' = if is_prop a then negate ~abs:b a else a in
      let b' = if is_prop b then negate ~abs:a b else b in
      if a == b' || a' == b
      then COMPARE.compare a b
      else COMPARE.compare (atom_min a a') (atom_min b b')

  (* -------------------------------------------------------------------------- *)
  (* ---  Hconsed                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  module W = Weak.Make
      (struct
        type t = term
        let hash t = t.hash
        let equal t1 t2 = equal_repr t1.repr t2.repr
      end)

  (* -------------------------------------------------------------------------- *)
  (* --- Builtins                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  module BUILTIN = Map.Make(Fun)

  (* -------------------------------------------------------------------------- *)
  (* --- Cache                                                              --- *)
  (* -------------------------------------------------------------------------- *)

  type cmp = EQ | NEQ | LT | LEQ

  type operation =
    | NOT of term (* Only AND, OR and IMPLY *)
    | CMP of cmp * term * term
    | FUN of Fun.t * term list

  module C = Cache.Unary
      (struct
        type t = operation
        let hash_op = function
          | EQ -> 2 | NEQ -> 3 | LT -> 5 | LEQ -> 7
        let hash = function
          | NOT p -> 5 * p.hash
          | CMP(c,a,b) -> hash_op c * Hcons.hash_pair a.hash b.hash
          | FUN(f,es) -> Hcons.hash_list hash (Fun.hash f) es
        let equal a b = match a,b with
          | NOT p,NOT q -> p==q
          | CMP(c,a,b),CMP(c',a',b') -> c=c' && a==a' && b==b'
          | FUN(f,xs) , FUN(g,ys) -> Fun.equal f g && Hcons.equal_list (==) xs ys
          | _ -> false
      end)

  module STRUCTURAL = struct type t = term let compare = COMPARE.compare end
  module STmap = Map.Make(STRUCTURAL)
  module STset = Set.Make(STRUCTURAL)
  
  (* -------------------------------------------------------------------------- *)
  (* --- Global State                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  type state = {
    mutable kid : int ;
    weak : W.t ;
    cache : term C.cache ;
    mutable checks : STset.t STmap.t ;
    mutable builtins_fun : (term list -> term) BUILTIN.t ;
    mutable builtins_eq  : (term -> term -> term) BUILTIN.t ;
    mutable builtins_leq : (term -> term -> term) BUILTIN.t ;
  }

  let empty () = {
    kid = 0 ;
    weak = W.create 32993 ; (* 3-th Leyland Prime number *)
    cache = C.create ~size:0x1000 ; (* 4096 entries *)
    checks = STmap.empty ;
    builtins_fun = BUILTIN.empty ;
    builtins_eq  = BUILTIN.empty ;
    builtins_leq = BUILTIN.empty ;
  }

  let state = ref (empty ())
  let get_state () = !state
  let set_state st = state := st
  let release () =
      C.clear !state.cache ;
      !state.checks <- STmap.empty

  let clock = ref true
  let constants = ref Tset.empty
  let constant c = assert !clock ; constants := Tset.add c !constants ; c

  let create () =
    begin
      clock := false ;
      let s = empty () in
      let add s c = W.add s.weak c ; s.kid <- max s.kid (succ c.id) in
      Tset.iter (add s) !constants ; s
    end

  let clr_state st =
    st.kid <- 0 ;
    W.clear st.weak;
    C.clear st.cache;
    st.checks <- STmap.empty;
    st.builtins_fun <- BUILTIN.empty ;
    st.builtins_eq  <- BUILTIN.empty ;
    st.builtins_leq <- BUILTIN.empty ;
    let add s c = W.add s.weak c ; s.kid <- max s.kid (succ c.id) in
    Tset.iter (add st) !constants


  (* -------------------------------------------------------------------------- *)
  (* --- Hconsed insertion                                                  --- *)
  (* -------------------------------------------------------------------------- *)

  let insert r =
    let h = hash_repr r in
    (* Only [hash] and [repr] are significant for lookup in weak hmap *)
    let e0 = {
      id = 0 ;
      hash = h ;
      repr = r ;
      size = 0;
      vars = Vars.empty ;
      bind = Bvars.empty ;
      sort = Sdata ;
    } in
    try W.find !state.weak e0
    with Not_found ->
      let k = !state.kid in
      !state.kid <- succ k ;
      assert (k <> -1) ;
      let e = {
        id = k ;
        hash = h ;
        repr = r ;
        vars = vars_repr r ;
        bind = bind_repr r ;
        sort = sort_repr r ;
        size = size_repr r ;
      }
      in W.add !state.weak e ; e

  (* -------------------------------------------------------------------------- *)
  (* --- Checker                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let check r x =
    let y = insert r in
    if x != y then
      begin
        let s =
          try STmap.find x !state.checks
          with Not_found -> STset.empty in
        !state.checks <- STmap.add x (STset.add y s) !state.checks
      end ;
    x

  let check_unit ~qed ~raw =
    let p = insert (Eq(qed,raw)) in p
  (* TODO:VAR: Vars.fold (fun x p -> insert (Bind(Forall,x,p))) p.vars p *)

  let iter_checks f =
    STmap.iter
      (fun qed s -> STset.iter (fun raw -> f ~qed ~raw) s)
      !state.checks

  (* -------------------------------------------------------------------------- *)
  (* --- Constructors for normalized terms                                  --- *)
  (* -------------------------------------------------------------------------- *)

  let e_false  = constant (insert False)
  let e_true   = constant (insert True)
  let e_zero   = constant (insert (Kint Z.zero))
  let e_one    = constant (insert (Kint Z.one))
  let e_int n  = insert (Kint (Z.of_int n))
  let e_float r = insert (Kreal (Q.of_float r))
  let e_zint z = insert (Kint z)
  let e_real x = insert (Kreal x)
  let e_var x  = insert(Fvar x)
  let e_bvar k t = insert(Bvar(k,t))

  let c_div x y = insert (Div(x,y))
  let c_mod x y = insert (Mod(x,y))
  let c_leq x y = insert (Leq(x,y))
  let c_lt  x y = insert (Lt (x,y))
  let insert_eq  x y = insert (Eq (x,y))
  let insert_neq x y = insert (Neq(x,y))
  let sym c x y = if compare x y < 0 then c y x else c x y
  let compare_field (f,x) (g,y) =
    let cmp = Field.compare f g in
    if cmp = 0 then compare x y else cmp

  let c_eq = sym insert_eq
  let c_neq = sym insert_neq

  let c_fun f xs = insert(Fun(f,xs))

  let c_add = function
    | [] -> e_zero
    | [x] -> x
    | xs -> insert(Add(List.sort compare xs))

  let c_mul = function
    | [] -> e_one
    | [x] -> x
    | xs -> insert(Mul(List.sort compare xs))

  let c_times z t = insert(Times(z,t))

  let c_and = function
    | [] -> e_true
    | [x] -> x
    | xs -> insert(And(xs))

  let c_or = function
    | [] -> e_false
    | [x] -> x
    | xs -> insert(Or(xs))

  let c_imply hs p = match hs with
    | [] -> p
    | hs -> insert(Imply(hs,p))

  let c_not x = insert(Not x)

  let c_if e a b = insert(If(e,a,b))

  let c_apply a es = if es=[] then a else insert(Apply(a,es))

  let c_bind q t e =
    if Bvars.closed e.bind then e else
      insert(Bind(q,t,e))

  let c_const t v = insert(Acst(t,v))
  
  let c_get m k = insert(Aget(m,k))

  let c_set m k v = insert(Aset(m,k,v))

  let c_getfield m f = insert(Rget(m,f))

  let c_record fxs =
    match fxs with
    | [] | [_] -> insert(Rdef fxs)
    | fx::gys ->
        try
          let base (f,v) =
            match v.repr with
            | Rget(r,g) when Field.equal f g -> r
            | _ -> raise Exit
          in
          let r = base fx in
          List.iter (fun gy -> if base gy != r then raise Exit) gys ; r
        with Exit ->
          insert(Rdef (List.sort compare_field fxs))

  [@@@ warning "-32"]
  let insert _ = assert false (* [insert] should not be used afterwards *)
  [@@@ warning "+32"]

  let rec subterm e = function
      [] -> e
    | n :: l ->
        let children = match e.repr with
          | True | False | Kint _ | Kreal _ | Bvar _ | Fvar _ -> []
          | Times (n,e) -> [ e_zint n; e]
          | Add l | Mul l | And l | Or l | Fun (_,l) -> l
          | Div (e1,e2) | Mod (e1,e2) | Eq(e1,e2) | Neq(e1,e2)
          | Leq (e1,e2) | Lt(e1,e2) | Aget(e1,e2) -> [e1;e2]
          | Not e | Bind(_,_,e) | Acst(_,e) -> [e]
          | Imply(l,e) -> l @ [e]
          | If(e1,e2,e3) | Aset(e1,e2,e3) -> [e1;e2;e3]
          | Rget(e,_) -> [e]
          | Rdef fxs -> List.map snd fxs
          | Apply(e,es) -> e::es
        in subterm (List.nth children n) l

  let is_primitive e =
    match e.repr with
    | True | False | Kint _ | Kreal _ -> true
    | _ -> false

  (* -------------------------------------------------------------------------- *)
  (* --- Cache & Builtin Simplifiers                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let builtin_fun f es =
    try (BUILTIN.find f !state.builtins_fun) es
    with Not_found -> c_fun f es

  let simplify_eq e a b =
    match e.repr with
    | Fun(f,_) -> BUILTIN.find f !state.builtins_eq a b
    | _ -> raise Not_found
             
  let simplify_leq e a b =
    match e.repr with
    | Fun(f,_) -> BUILTIN.find f !state.builtins_leq a b
    | _ -> raise Not_found

  let builtin_eq a b =
    try simplify_eq a a b with Not_found -> simplify_eq b a b

  let builtin_leq a b =
    try simplify_leq a a b with Not_found -> simplify_leq b a b

  let builtin_cmp cmp a b =
    try
      match cmp with
      | EQ -> builtin_eq a b
      | LEQ -> builtin_leq a b
      | NEQ -> !extern_not (builtin_eq a b)
      | LT  -> !extern_not (builtin_leq b a)
    with Not_found ->
    match cmp with
    | EQ  -> c_eq a b
    | NEQ -> c_neq a b
    | LT  -> c_lt a b
    | LEQ -> c_leq a b

  let dispatch = function
    | NOT p -> !cached_not p.repr
    | CMP(cmp,a,b) -> builtin_cmp cmp a b
    | FUN(f,es) -> builtin_fun f es
  let operation op = C.compute !state.cache dispatch op

  let distribute_if_over_operation force op x y f a b =
    match a.repr, b.repr with
    | If(ac,a1,a2), If(bc,b1,b2) when ac == bc
      -> !extern_ite ac (f a1 b1) (f a2 b2)
    | If(ac,a1,a2), _ when force || ((is_primitive a1 || is_primitive a2) && is_primitive b)
      -> !extern_ite ac (f a1 b) (f a2 b)
    | _, If(bc,b1,b2) when force || ((is_primitive b1 || is_primitive b2) && is_primitive a)
      -> !extern_ite bc (f a b1) (f a b2)
    | If(ac,a1,a2), If(_,b1,b2) when (is_primitive a1 && is_primitive a2) && (is_primitive b1 || is_primitive b2)
      -> !extern_ite ac (f a1 b) (f a2 b)
    | If(_,a1,a2), If(bc,b1,b2) when (is_primitive a1 || is_primitive a2) && (is_primitive b1 && is_primitive b2)
      -> !extern_ite bc (f a b1) (f a b2)
    | _ -> op x y

  let distribute f = function
    | x::[] as xs -> 
        begin
          match x.repr with
        | If(c,a,b) ->  !extern_ite c (!extern_fun f [a]) (!extern_fun f [b])
          | _ -> operation (FUN(f,xs))
        end
    | a::b::[] as xs ->
        distribute_if_over_operation false (fun f xs -> operation (FUN(f,xs))) f xs (fun a b -> !extern_fun f [a;b]) a b
    | xs -> operation (FUN(f,xs))

  let c_builtin_fun f xs = distribute f xs
  let c_builtin_eq  a b = distribute_if_over_operation true (fun a b -> operation (CMP(EQ ,a,b))) a b !extern_eq  a b
  let c_builtin_neq a b = distribute_if_over_operation true (fun a b -> operation (CMP(NEQ,a,b))) a b !extern_neq a b
  let c_builtin_lt  a b = distribute_if_over_operation true (fun a b -> operation (CMP(LT ,a,b))) a b !extern_lt  a b
  let c_builtin_leq a b = distribute_if_over_operation true (fun a b -> operation (CMP(LEQ,a,b))) a b !extern_leq a b

  let prepare_builtin f m =
    release () ;
    if BUILTIN.mem f m then
      let msg = Printf.sprintf
          "Builtin already registered for '%s'" (Fun.debug f) in
      raise (Failure msg)

  let set_builtin f p =
    begin
      prepare_builtin f !state.builtins_fun ;
      !state.builtins_fun <- BUILTIN.add f p !state.builtins_fun ;
    end

  let set_builtin_eq f p =
    begin
      prepare_builtin f !state.builtins_eq ;
      !state.builtins_eq <- BUILTIN.add f p !state.builtins_eq ;
    end

  let set_builtin_leq f p =
    begin
      prepare_builtin f !state.builtins_leq ;
      !state.builtins_leq <- BUILTIN.add f p !state.builtins_leq ;
    end

  let set_builtin_map f phi = set_builtin f (fun es -> c_fun f (phi es))

  (* -------------------------------------------------------------------------- *)
  (* --- Negation                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let rec e_not p =
    match p.repr with
    | True -> e_false
    | False -> e_true
    | Lt(x,y) -> !extern_leq y x
    | Leq(x,y) -> !extern_lt y x
    | Eq(x,y) -> c_neq x y
    | Neq(x,y) -> c_eq x y
    | Not x -> x
    | (And _ | Or _ | Imply _) -> operation (NOT p)
    | Bind(Forall,t,p) -> c_bind Exists t (e_not p)
    | Bind(Exists,t,p) -> c_bind Forall t (e_not p)
    | _ -> c_not p

  let () = extern_not := e_not

  (* -------------------------------------------------------------------------- *)
  (* --- User Operators                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let rec op_revassoc phi xs = function
    | [] -> xs
    | e::es ->
        match e.repr with
        | Fun(f,ts) when Fun.equal f phi ->
            op_revassoc phi (op_revassoc f xs ts) es
        | _ -> op_revassoc phi (e::xs) es

  let rec op_idempotent = function
    | [] -> []
    | [_] as l -> l
    | x::( (y::_) as w ) -> if x==y then op_idempotent w else x :: op_idempotent w

  let op_invertible xs ys =
    let rec simpl modified turn xs ys = match xs , ys with
      | x::xs , y::ys when x==y -> simpl true turn xs ys
      | _ ->
          let xs = List.rev xs in
          let ys = List.rev ys in
          if turn
          then simpl modified false xs ys
          else modified,xs,ys
    in simpl false true xs ys

  let rec element = function
    | E_none -> assert false
    | E_int k -> e_int k
    | E_true -> e_true
    | E_false -> e_false
    | E_fun (f,l) -> c_fun f (List.map element l)

  let rec is_element e x = match e , x.repr with
    | E_int k , Kint z -> Z.equal (Z.of_int k) z
    | E_true , True -> true
    | E_false , False -> false
    | E_fun (f,fl) , Fun(g,gl) ->
        Fun.equal f g &&
        List.length fl = List.length gl &&
        List.for_all2 is_element fl gl
    | _ -> false

  let isnot_element e x = not (is_element e x)

  let is_neutral f e =
    match Fun.category f with
    | Operator op -> is_element op.neutral e
    | _ -> false

  let is_absorbant f e =
    match Fun.category f with
    | Operator op -> is_element op.absorbant e
    | _ -> false

  let op_fun f op xs =
    let xs =
      if op.associative then
        let xs = op_revassoc f [] xs in
        if op.commutative
        then List.sort compare xs
        else List.rev xs
      else
      if op.commutative
      then List.sort compare xs
      else xs
    in
    if op.absorbant <> E_none &&
       List.exists (is_element op.absorbant) xs
    then element op.absorbant
    else
      let xs =
        if op.neutral <> E_none
        then List.filter (isnot_element op.neutral) xs
        else xs in
      let xs = if op.idempotent then op_idempotent xs else xs in
      match xs with
      | [] when op.neutral <> E_none -> element op.neutral
      | [x] when op.associative -> x
      | _ -> c_builtin_fun f xs

  let e_fungen f xs =
    match Fun.category f with
    | Logic.Operator op -> op_fun f op xs
    | _ -> c_builtin_fun f xs

  let e_fun = e_fungen
  let () = extern_fun := e_fun

  (* -------------------------------------------------------------------------- *)
  (* --- Ground & Arithmetics                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let rec i_ground f c xs = function
    | {repr=Kint n}::ts -> i_ground f (f c n) xs ts
    | x::ts -> i_ground f c (x::xs) ts
    | [] -> c , xs

  let rec r_ground f c xs = function
    | {repr=Kreal z}::ts -> r_ground f (f c z) xs ts
    | {repr=Kint n}::ts -> r_ground f (f c (Q.of_bigint n)) xs ts
    | x::ts -> r_ground f c (x::xs) ts
    | [] -> c , xs

  type sign = Null | Negative | Positive
  let sign z =
    if Z.lt z Z.zero then Negative else
    if Z.lt Z.zero z then Positive else
      Null
(*
  let pp fmt a =
    match a.repr with
    | Kint z -> Format.pp_print_string fmt (Z.to_string z)
    | Kreal q -> Format.pp_print_string fmt (Q.to_string q)
    | Fvar x -> Var.pretty fmt x
    | _ -> Format.fprintf fmt "#%d" a.id
*)

  let r_affine_rel fz fe c xs ys =
    let a , xs = r_ground Q.add (Q.of_bigint c) [] xs in
    let b , ys = r_ground Q.add Q.zero [] ys in
    let c = Q.sub a b in
    match xs , ys with
    | [] , [] -> if fz c Q.zero then e_true else e_false
    | [] , _ -> fe (e_real c) (c_add ys)
    | _ , [] -> fe (c_add xs) (e_real (Q.neg c))
    | _ ->
        let s = Q.sign c in
        if s < 0 then fe (c_add xs) (c_add (e_real (Q.neg c) :: ys)) else
        if s > 0 then fe (c_add (e_real c :: xs)) (c_add ys) else
          fe (c_add xs) (c_add ys)

  let i_affine_rel fc fe c xs ys =
    match xs , ys with
    | [] , [] -> if fc c Z.zero then e_true else e_false
    | [] , _ -> fe (e_zint c) (c_add ys) (* c+0 R ys <-> c R ys *)
    | _ , [] -> fe (c_add xs) (e_zint (Z.neg c)) (* c+xs R 0 <-> xs R -c *)
    | _ ->
        match sign c with
        (* 0+xs R ys <-> xs R ys *)
        | Null -> fe (c_add xs) (c_add ys)
        (* c+xs R ys <-> xs R (-c+ys) *)
        | Negative -> fe (c_add xs) (c_add (e_zint (Z.neg c) :: ys))
        (* c+xs R ys <-> (c+xs) R ys *)
        | Positive -> fe (c_add (e_zint c :: xs)) (c_add ys)

  let i_affine xs ys =
    not (List.exists is_real xs || List.exists is_real ys)

  let affine_eq c xs ys =
    if i_affine xs ys
    then i_affine_rel Z.equal c_builtin_eq c xs ys
    else r_affine_rel Q.equal c_builtin_eq c xs ys
  
  let affine_neq c xs ys =
    if i_affine xs ys
    then i_affine_rel (fun x y -> not (Z.equal x y)) c_builtin_neq c xs ys
    else r_affine_rel (fun x y -> not (Q.equal x y)) c_builtin_neq c xs ys

  let affine_leq c xs ys =
    if i_affine xs ys then
      if Z.equal c Z.one
      then i_affine_rel Z.lt c_builtin_lt Z.zero xs ys
      else i_affine_rel Z.leq c_builtin_leq c xs ys
    else r_affine_rel Q.leq c_builtin_leq c xs ys

  let affine_lt c xs ys =
    if i_affine xs ys then
      if not (Z.equal c Z.zero)
      then i_affine_rel Z.leq c_builtin_leq (Z.succ c) xs ys
      else i_affine_rel Z.lt c_builtin_lt c xs ys
    else r_affine_rel Q.lt c_builtin_lt c xs ys

  let affine_cmp = function
    | EQ -> affine_eq
    | LT -> affine_lt
    | NEQ -> affine_neq
    | LEQ -> affine_leq
  
  (* --- Times --- *)

  let q_times k z =
    if Z.equal k Z.one then z else
    if Z.equal k Z.zero then Q.zero else
      Q.(make (Z.mul k z.num) z.den)

  let rec times z e =
    if Z.equal z Z.one then e else
    if Z.equal z Z.zero then e_zint Z.zero else
      match e.repr with
      | Kint z' -> e_zint (Z.mul z z')
      | Kreal r -> e_real (q_times z r)
      | Times(z',t) -> times (Z.mul z z') t
      | _ -> c_times z e

  (* --- Additions --- *)

  let rec unfold_affine acc k = function
    | [] -> acc
    | t::others -> unfold_affine (unfold_affine1 acc k t) k others

  and unfold_affine1 acc k t =
    match t.repr with
    | Times(n,t) -> unfold_affine1 acc (Z.mul k n) t
    | Kint z -> if z == Z.zero then acc else (Z.mul k z , e_one) :: acc
    | Add ts -> unfold_affine acc k ts
    | Kreal r when Q.(equal r zero) -> acc
    | Kreal r -> (Z.one , e_real (q_times k r)) :: acc
    | _ -> (k,t) :: acc

  (* sorts monoms by terms *)
  let compare_monoms (_,t1) (_,t2) = Pervasives.compare t1.id t2.id

  (* factorized monoms *)
  let fold_monom ts k t =
    if Z.equal Z.zero k then ts else
    if Z.equal Z.one k then t::ts else
      times k t :: ts

  (* monoms sorted by terms *)
  let rec fold_affine f a = function
    | (n1,t1)::(n2,t2)::kts when t1 == t2 ->
        fold_affine f a ((Z.add n1 n2,t1)::kts)
    | (k,t)::kts ->
        begin match t.repr , kts with
          | Kreal z , ( k' , { repr = Kreal z' } ) :: kts' ->
              let q = Q.add (q_times k z) (q_times k' z') in
              fold_affine f a ((Z.one,e_real q) :: kts')
          | _ -> fold_affine f (f a k t) kts
        end
    | [] -> a

  let affine a =
    let kts = unfold_affine1 [] Z.one a in
    let fact,const = List.partition (fun (_,base) -> base.id = e_one.id) kts in
    let base = List.fold_left (fun z (k,_) -> Z.add z k) Z.zero const in
    { constant = base ; factors = fact }

  (* ts normalized *)
  let addition ts =
    let kts = unfold_affine [] Z.one ts in
    let kts = List.sort compare_monoms kts in
    c_add (fold_affine fold_monom [] kts)

  (* --- Relations --- *)

  let is_affine e = match e.repr with
    | Kint _ | Kreal _ | Times _ | Add _ -> true
    | _ -> false

  let fold_coef g xs k t = fold_monom xs (Z.div k g) t
  
  let rec coef_monoms c = function
    | [] -> c , Z.one
    | (n,e)::w ->
        if e == e_one then coef_monoms (Z.add c n) w else
          let rec coef_gcd c p = function
            | [] -> c , p
            | (n,e)::w ->
                if e == e_one
                then coef_gcd (Z.add c n) p w
                else coef_gcd c Z.(gcd p (abs n)) w
          in coef_gcd c (Z.abs n) w
          
  let rec partition_monoms phi xs ys = function
    | [] -> xs,ys
    | (k,t) :: kts ->
        if t == e_one
        then partition_monoms phi xs ys kts
        else
        if Z.leq Z.zero k
        then partition_monoms phi (phi xs k t) ys kts
        else partition_monoms phi xs (phi ys (Z.neg k) t) kts

  let collect_monoms xs k t = if Z.(equal k zero) then xs else (k,t)::xs

  (* Congruence Theorem:
     (proved with Alt-Ergo for B in -10..10)
     Assumes 0 < |r| < g
     Then:
     CONG-EQ:  gB+r =  0 <-> false
     CONG-NEQ: gB+r <> 0 <-> true
     CONG-LEQ-POS: 0 < r -> gB+r <= 0 <-> B <  0
     CONG-LEQ-NEG: r < 0 -> gB+r <= 0 <-> B <= 0
     CONG-LT-POS:  0 < r -> gB+r <  0 <-> B <  0
     CONG-LT-NEG:  r < 0 -> gB+r <  0 <-> B <= 0
  *)
  
  let relation rel cmp x y =
    if is_affine x || is_affine y then
      let kts = unfold_affine1 (unfold_affine1 [] Z.one x) Z.minus_one y in
      let kts = List.sort compare_monoms kts in
      let kts = fold_affine collect_monoms [] kts in
      let k,g = coef_monoms Z.zero kts in
      if Z.(equal g one) || List.exists (fun (_,e) -> not (is_int e)) kts then
        let xs,ys = partition_monoms fold_monom [] [] kts in
        affine_cmp cmp k xs ys
      else
        let k,r = Z.div_rem k g in
        if Z.(equal r zero) then
          let xs,ys = partition_monoms (fold_coef g) [] [] kts in
          affine_cmp cmp k xs ys
        else match cmp with
          | EQ -> e_false (* CONG-EQ *)
          | NEQ -> e_true (* CONG-NEQ *)
          | LT | LEQ ->
              let xs,ys = partition_monoms (fold_coef g) [] [] kts in
              (* CONG-LEQ|LT-POS|NEQ *)
              let cmp = if Z.(lt zero r) then LT else LEQ in
              affine_cmp cmp k xs ys
    else rel x y

  (* --- Multiplications --- *)

  let rec mul_unfold acc = function
    | [] -> acc
    | t::others ->
        match t.repr with
        | Times(z,t) -> mul_unfold (e_zint z :: acc) (t::others)
        | Mul ts -> mul_unfold (mul_unfold acc ts) others
        | _ -> mul_unfold (t::acc) others

  let multiplication ts = (* ts normalized *)
    let ts = mul_unfold [] ts in
    if List.exists is_real ts then
      let r,ts = r_ground Q.mul Q.one [] ts in
      if Q.equal Q.zero r then e_real Q.zero else
      if ts=[] then e_real r else
      if Q.equal r Q.one then c_mul ts else  c_mul (e_real r :: ts)
    else
      let s,ts = i_ground Z.mul Z.one [] ts in
    if Z.equal Z.zero s then e_zint Z.zero else
    if ts=[] then e_zint s else
      let t = c_mul ts in
      if Z.equal s Z.one then t else c_times s t

  (* --- Divisions --- *)

  let e_times k x =
    if Z.equal k Z.zero then e_zero else
    if Z.equal k Z.one then x else
      times k x

  let e_div a b =
    match a.repr , b.repr with
    | _ , Kint z when Z.equal z Z.one -> a
    | _ , Kint z when Z.equal z Z.minus_one -> times Z.minus_one a
    | Times(k,e) , Kint k' when not (Z.equal k' Z.zero) ->
        let q,r = Z.div_rem k k' in
        if Z.equal r Z.zero
        then e_times q e
        else c_div a b
    | Kint k , Kint k' when not (Z.equal k' Z.zero) -> e_zint (Z.div k k')
    | Kreal r , Kint a when not (Z.equal a Z.zero) ->
        e_real Q.(make r.num (Z.mul a r.den))
    | Kint a , Kreal b when not (Q.equal b Q.zero) ->
        e_real Q.(make (Z.mul a b.den) b.num)
    | Kreal a , Kreal b when not (Q.equal b Q.zero) ->
        e_real (Q.div a b)
    | _ -> c_div a b

  let e_mod a b =
    match a.repr , b.repr with
    | _ , Kint z when Z.equal z Z.one -> e_zero
    | Times(k,e) , Kint k' when not (Z.equal k' Z.zero) ->
        let r = Z.rem k k' in
        if Z.equal r Z.zero
        then e_zero
        else c_mod (e_times r e) b
    | Kint k , Kint k' when not (Z.equal k' Z.zero) -> e_zint (Z.rem k k')
    | _ -> c_mod a b

  (* --- Comparisons --- *)

  let e_lt x y =
    if x==y then e_false else relation c_builtin_lt LT x y
  let () = extern_lt := e_lt

  let e_leq x y =
    if x==y then e_true else relation c_builtin_leq LEQ x y
  let () = extern_leq := e_leq

  (* -------------------------------------------------------------------------- *)
  (* --- Logical                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let decide e = (e == e_true)

  let is_true e = match e.repr with
    | True -> Logic.Yes
    | False -> Logic.No
    | _ -> Logic.Maybe

  let is_false e = match e.repr with
    | True -> Logic.No
    | False -> Logic.Yes
    | _ -> Logic.Maybe

  let rec fold_and acc xs =
    match xs with
    | [] -> acc
    | x::others ->
        match x.repr with
        | False  -> raise Absorbant
        | True   -> fold_and acc others
        | And xs -> fold_and (fold_and acc xs) others
        | _      -> fold_and (x::acc) others

  let rec fold_or acc xs =
    match xs with
    | [] -> acc
    | x::others ->
        match x.repr with
        | True  -> raise Absorbant
        | False -> fold_or acc others
        | Or xs -> fold_or (fold_or acc xs) others
        | _     -> fold_or (x::acc) others

  let conjunction ts =
    try
      let ms = fold_and [] ts in
      let ms = List.sort_uniq compare_raising_absorbant ms in
      c_and ms
    with Absorbant -> e_false

  let disjunction ts =
    try
      let ms = fold_or [] ts in
      let ms = List.sort_uniq compare_raising_absorbant ms in
      c_or ms
    with Absorbant -> e_true

  module Consequence =
  struct
    type p = CONJ | DISJ
    type t = { mutable modif : bool ; polarity : p }

    let mark w = w.modif <- true ; w

    let rec gen w hs ts =
      match hs with
      | [] -> ts
      | h :: hws ->
          match w.polarity with
          | CONJ -> aux w ~absorb:(e_not h) ~filter:h hws ts
          | DISJ -> aux w ~absorb:h ~filter:(e_not h) hws ts

    and aux w ~absorb ~filter hws ts =
      match ts with
      | [] -> ts
      | t :: tws ->
          if absorb == t then raise Absorbant ;
          let cmp = compare filter t in
          if cmp < 0
          then gen w hws ts else
          if cmp > 0
          then t :: aux (mark w) ~absorb ~filter hws tws
          else gen (mark w) hws tws

    let filter polarity hs ts =
      let w = { modif = false ; polarity } in
      let ws = gen w hs ts in
      if w.modif then ws else ts

  end

  let consequence_and = Consequence.(filter CONJ)
  let consequence_or  = Consequence.(filter DISJ)

  let merge hs hs0 = List.sort_uniq compare_raising_absorbant (hs@hs0)

  let rec implication hs b = match b.repr with
    | Imply(hs0,b0) -> implication_imply hs b hs0 b0
    | And bs -> implication_and [] hs b bs
    | Or bs  -> implication_or  [] hs b bs
    | _ -> c_imply hs b
  and implication_and hs0 hs b0 bs = try
      let hs'= merge hs0 hs in
      try 
        match consequence_and hs bs with
        | []  -> e_true (* [And hs] implies [b0] *)
        | [b] -> implication hs' b
        | bs' -> c_imply hs' (if bs'==bs then b0 else c_and bs')
      with Absorbant -> implication_false hs' (* [And hs] implies [Not b0] *)
    with Absorbant -> e_true (* [False = And (hs@hs0)] *)
  and implication_or hs0 hs b0 bs = try
      let hs'= merge hs0 hs in
      match consequence_or hs bs with
      | []  -> implication_false hs' (* [And hs] implies [Not b0] *)
      | [b] -> implication hs' b
      | bs' -> c_imply hs' (if bs'==bs then b0 else c_or bs')
    with Absorbant -> e_true (* [False = And (hs@hs0)] or [And hs] implies [b] *)
  and implication_imply hs b hs0 b0 = try
      match consequence_and hs [b0] with
      | [] -> e_true (* [And hs] implies [b0] *)
      | _ -> try
            match consequence_and hs0 hs with
            | [] -> b (* [And hs0] implies [And hs] *)
            | hs ->
                match b0.repr with
                | And bs -> implication_and hs0 hs b0 bs
                | Or bs  -> implication_or  hs0 hs b0 bs
                | _ -> c_imply (merge hs0 hs) b0
          with Absorbant -> e_true (* [False = And (hs@hs0)] *)
    with Absorbant -> (* [And hs] implies [Not b0] *)
    try implication_false (merge hs hs0)
    with Absorbant -> e_true  (* [False = And (hs@hs0)] *)
  and implication_false hs =
    e_not (c_and hs)

  let rec consequence_aux hs x = match x.repr with
    | And xs -> begin try 
          match consequence_and hs xs with
          | [] -> e_true
          | [x] -> consequence_aux hs x
          | hs -> if hs==xs then x else c_and hs
        with Absorbant -> e_false
      end
    | Or xs -> begin try 
          match consequence_and hs xs with
          | [] -> e_false
          | [x] -> consequence_aux hs x
          | hs -> if hs==xs then x else c_or hs
        with Absorbant -> e_true
      end
    | Not x -> e_not (consequence_aux hs x)
    | Imply (xs, b) -> begin
        let b' = consequence_aux hs b in
        match b'.repr with
        | True -> b'
        | _ -> begin try
              let xs' = consequence_and hs xs in
              match b==b', xs==xs', xs' with
              | true,  true,  _  -> x
              | _,     false, [] -> b'
              | true,  false, _  -> c_imply xs' b'
              | false, _,     _  -> implication xs' b'
            with Absorbant -> e_false
          end
      end
    | _ -> x

  let consequence h x = 
    let not_x = e_not x in
    match h.repr with
    | True -> x
    | False -> (* what_ever *) x
    | _ when h == x     -> e_true
    | _ when h == not_x -> e_false
    | And hs -> consequence_aux hs x
    | _      -> consequence_aux [h] x

  type structural =
    | S_diff         (* different constructors *)
    | S_injection    (* same injective function *)
    | S_invertible   (* same invertible function *)
    | S_functions    (* general functions *)

  let structural f g =
    if Fun.equal f g then
      match Fun.category f with
      | Logic.Operator { invertible=true } -> S_invertible
      | Logic.Injection | Logic.Constructor -> S_injection
      | Logic.Function | Logic.Operator _ -> S_functions
    else
      match Fun.category f , Fun.category g with
      | Logic.Constructor , Logic.Constructor -> S_diff
      | _ -> S_functions

  let contrary x y = (is_prop x || is_prop y) && (e_not x == y)

  (* -------------------------------------------------------------------------- *)
  (* --- List All2/Any2                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let e_all2 phi xs ys =
    let n = List.length xs in
    let m = List.length ys in
    if n <> m then e_false else conjunction (List.map2 phi xs ys)

  let e_any2 phi xs ys =
    let n = List.length xs in
    let m = List.length ys in
    if n <> m then e_true else disjunction (List.map2 phi xs ys)

  (* -------------------------------------------------------------------------- *)
  (* --- Equality                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let rec e_eq x y =
    if x == y then e_true else relation eq_symb EQ x y

  and eq_symb x y =
    match x.repr , y.repr with
    | Kint z , Kint z' -> if Z.equal z z' then e_true else e_false
    | Kreal z , Kreal z' -> if Q.equal z z' then e_true else e_false
    | Kint a , Kreal r | Kreal r , Kint a ->
        if Q.equal r (Q.of_bigint a) then e_true else e_false
    | True , _ -> y
    | _ , True -> x
    | False , _ -> e_not y
    | _ , False -> e_not x
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_diff -> e_false
          | S_injection -> e_all2 e_eq xs ys
          | S_functions -> c_builtin_eq x y
          | S_invertible ->
              let modified,xs,ys = op_invertible xs ys in
              if modified
              then c_builtin_eq (e_fun f xs) (e_fun g ys)
              else c_builtin_eq x y
        end
    | Rdef fxs , Rdef gys ->
        begin
          try e_all2 eq_field fxs gys
          with Exit -> e_false
        end
        
    | Acst(_,a) , Acst(_,b) -> e_eq a b
    | Acst(_,v0) , Aset(m,_,v) -> conjunction [e_eq v v0 ; e_eq x m]
    | Aset(m,_,v) , Acst(_,v0) -> conjunction [e_eq v v0 ; e_eq m y]
          
    | _ when contrary x y -> e_false
      
    | Fun _ , _ | _ , Fun _ -> c_builtin_eq x y
    | _ -> c_eq x y

  and eq_field (f,x) (g,y) =
    if Field.equal f g then e_eq x y else raise Exit

  let () = extern_eq := e_eq

  (* -------------------------------------------------------------------------- *)
  (* --- Disequality                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec e_neq x y =
    if x == y then e_false else relation neq_symb NEQ x y

  and neq_symb x y =
    match x.repr , y.repr with
    | Kint z , Kint z' -> if Z.equal z z' then e_false else e_true
    | Kreal z , Kreal z' -> if Q.equal z z' then e_false else e_true
    | Kreal r , Kint a | Kint a , Kreal r ->
        if Q.equal r (Q.of_bigint a) then e_false else e_true
    | True , _ -> e_not y
    | _ , True -> e_not x
    | False , _ -> y
    | _ , False -> x
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_diff -> e_true
          | S_injection -> e_any2 e_neq xs ys
          | S_functions -> c_builtin_neq x y
          | S_invertible ->
              let modified,xs,ys = op_invertible xs ys in
              if modified
              then c_builtin_neq (e_fun f xs) (e_fun g ys)
              else c_builtin_neq x y
        end
    | Rdef fxs , Rdef gys ->
        begin
          try e_any2 neq_field fxs gys
          with Exit -> e_true
        end
        
    | Acst(_,a) , Acst(_,b) -> e_neq a b
    | Acst(_,v0) , Aset(m,_,v) -> disjunction [e_neq v v0 ; e_neq x m]
    | Aset(m,_,v) , Acst(_,v0) -> disjunction [e_neq v v0 ; e_neq m y]
  
    | _ when contrary x y -> e_true
      
    | Fun _ , _ | _ , Fun _ -> c_builtin_neq x y
    | _ -> c_neq x y

  and neq_field (f,x) (g,y) =
    if Field.equal f g then e_neq x y else raise Exit

  let () = extern_neq := e_neq

  (* -------------------------------------------------------------------------- *)
  (* --- Boolean Simplifications                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let e_or = function
    | [] -> e_false
    | [t] -> t
    | ts -> disjunction ts

  let e_and = function
    | [] -> e_true
    | [t] -> t
    | ts -> conjunction ts

  let rec imply1 a b =
    match a.repr , b.repr with
    | _ , False -> e_not a
    | Not p , Not q -> imply1 q p
    | _  when a == b -> e_true
    | _  when a == e_not b -> b
    | _, _ -> implication [a] b

  let imply2 hs b =
    match b.repr with
    | And bs -> implication_and [] hs b bs
    | _ -> try
          match consequence_and hs [b] with
          | [] -> e_true (* [And hs] implies [b] *)
          | _  ->
              match b.repr with
              | Or bs -> implication_or [] hs b bs
              | Imply(hs0,b0) -> implication_imply hs b hs0 b0
              | _ -> c_imply hs b
        with Absorbant -> implication_false hs (* [And hs] implies [Not b] *)

  let e_imply hs p =
    match p.repr with
    | True -> e_true
    | _ ->
        try
          let hs = fold_and [] hs in
          let hs = List.sort_uniq compare_raising_absorbant hs in
          match hs with
          | []  -> p
          | [a] -> imply1 a p
          | _   -> imply2 hs p
        with Absorbant -> e_true

  let () = cached_not := function
      | And xs -> e_or (List.map e_not xs)
      | Or  xs -> e_and (List.map e_not xs)
      | Imply(hs,p) -> e_and (e_not p :: hs)
      | _ -> assert false

  let e_if e a b =
    match e.repr with
    | True -> a
    | False -> b
    | _ ->
        if a == b then a else
          match a.repr , b.repr with
          | True , _  -> disjunction [e;b]
          | _ , False -> conjunction [e;a]
          | False , _ -> conjunction [e_not e;b]
          | _ , True  -> disjunction [e_not e;a]
          | _ ->
              match e.repr with
              | Not e0 -> c_if e0 b a
              | Neq(u,v) -> c_if (e_eq u v) b a
              | _ -> c_if e a b
  let () = extern_ite := e_if

  let e_bool = function true -> e_true | false -> e_false
  let e_literal v p = if v then p else e_not p
  let literal p = match p.repr with
    | Neq(a,b) -> false , c_eq a b
    | Lt(x,y) -> false , c_leq y x
    | Not q -> false , q
    | _ -> true , p
  let are_equal a b = is_true (e_eq a b)
  let eval_eq a b = (e_eq a b == e_true)
  let eval_neq a b = (e_eq a b == e_false)
  let eval_lt a b = (e_lt a b == e_true)
  let eval_leq a b = (e_leq a b == e_true)

  (* -------------------------------------------------------------------------- *)
  (* --- Arrays                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let rec e_get m k =
    match m.repr with
    | Acst(_,v) -> v
    | Aset(m0,k0,v0) ->
        begin
          match are_equal k k0 with
          | Yes -> v0
          | No -> e_get m0 k
          | Maybe -> c_get m k
        end
    | _ -> c_get m k

  let rec e_set m k v =
    match m.repr with
    | Acst(_,v0) ->
        begin
          match are_equal v v0 with
          | Yes -> m
          | No | Maybe -> c_set m k v
        end
    | Aset(m0,k0,_) ->
        begin
          match are_equal k k0 with
          | Yes -> e_set m0 k0 v
          | No | Maybe -> c_set m k v
        end
    | _ -> c_set m k v

  let e_const (k:tau) v = c_const k v
  
  (* -------------------------------------------------------------------------- *)
  (* --- Records                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let rec get_field m0 f = function
    | [] -> c_getfield m0 f
    | (g,y)::gys -> if Field.equal f g then y else get_field m0 f gys

  let e_getfield m f =
    match m.repr with
    | Rdef gys -> get_field m f gys
    | _ -> c_getfield m f

  let e_record fxs = c_record fxs
  type record = (Field.t * term) list

  (* -------------------------------------------------------------------------- *)
  (* --- Non-Binding Morphism                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let rebuild f e =
    match e.repr with
    | Kint _ | Kreal _ | Fvar _ | Bvar _ | True | False -> e
    | Not e -> e_not (f e)
    | Add xs -> addition (List.map f xs)
    | Mul xs -> multiplication (List.map f xs)
    | And xs -> e_and (List.map f xs)
    | Or  xs -> e_or  (List.map f xs)
    | Mod(x,y) -> e_mod (f x) (f y)
    | Div(x,y) -> e_div (f x) (f y)
    | Eq(x,y)  -> e_eq  (f x) (f y)
    | Neq(x,y) -> e_neq (f x) (f y)
    | Lt(x,y)  -> e_lt  (f x) (f y)
    | Leq(x,y) -> e_leq (f x) (f y)
    | Times(z,t) -> times z (f t)
    | If(e,a,b) -> e_if (f e) (f a) (f b)
    | Imply(hs,p) -> e_imply (List.map f hs) (f p)
    | Fun(g,xs) -> e_fun g (List.map f xs)
    | Acst(t,v) -> e_const t v
    | Aget(x,y) -> e_get (f x) (f y)
    | Aset(x,y,z) -> e_set (f x) (f y) (f z)
    | Rget(x,g) -> e_getfield (f x) g
    | Rdef gxs -> e_record (List.map (fun (g,x) -> g, f x) gxs)
    | Apply(e,es) -> c_apply (f e) (List.map f es)
    | Bind(q,t,e) -> c_bind q t (f e)

  (* -------------------------------------------------------------------------- *)
  (* --- Smart Constructors                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let e_equiv = e_eq
  let e_sum = addition
  let e_prod = multiplication
  let e_opp x = times Z.minus_one x
  let e_add x y = addition [x;y]
  let e_sub x y = addition [x;e_opp y]
  let e_mul x y = multiplication [x;y]

  (* -------------------------------------------------------------------------- *)
  (* --- Locally Memoized                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  type sigma = term Tmap.t ref

  let sigma () = ref Tmap.empty

  let cache_find m e = Tmap.find e !m
  let cache_bind m e v = m := Tmap.add e v !m ; v

  let sigma_add m t = m := Tmap.union (fun _ _ v2 -> v2) !m t

  (* -------------------------------------------------------------------------- *)
  (* --- Locally Nameless                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let rec lc_subst_var m x v e =
    if not (Vars.mem x e.vars) then e else
      match e.repr with
      | Fvar y when Var.equal x y -> v
      | _ ->
          try cache_find m e
          with Not_found -> cache_bind m e (rebuild (lc_subst_var m x v) e)

  let lc_bind x e =
    let k = Bvars.order e.bind in
    let t = tau_of_var x in
    lc_subst_var (sigma ()) x (e_bvar k t) e

  let e_subst_var x v e =
    lc_subst_var (sigma ()) x v e

  let rec lc_open m k v e =
    if not (Bvars.contains k e.bind) then e else
      match e.repr with
      | Bvar _ -> v
      | _ ->
          try cache_find m e
          with Not_found -> cache_bind m e (rebuild (lc_open m k v) e)

  let lc_open_term t e =
    let k = Bvars.order e.bind in
    lc_open (sigma ()) k t e

  let lc_open x e =
    let k = Bvars.order e.bind in
    lc_open (sigma ()) k (e_var x) e

  let lc_closed e = Bvars.closed e.bind
  let lc_closed_at n e = Bvars.closed_at n e.bind
  let lc_vars e = e.bind
  let lc_repr e = e

  (* -------------------------------------------------------------------------- *)
  (* --- Binders                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let let_intro_case q x a =
    let res = ref None in
    let is_term_ok a b =
      let found_term t = assert (!res = None);
        assert (not (Vars.mem x t.vars));
        res := Some t; true
      in
      match a.repr with
      | Fvar w -> assert (Var.equal x w); found_term b
      | Add e ->
          let is_var t = match t.repr with|Fvar v -> Var.equal x v|_->false in
          let rec add_case es = match es with
            | [] -> assert false (* because [x] is in [e] *)
            | t::ts ->
                if not (Vars.mem x t.vars) then add_case ts else
                if not (is_var t) then false (* [x] is too far in [t] *) else
                if not (List.for_all (fun t -> not (Vars.mem x t.vars)) ts)
                then false (* [x] is also in [ts] *)
                else begin (* var [x] is only in [t] that is also exactly [x] *)
                  let rec fold_until_es acc ys = match ys with
                    | [] -> assert false
                    | _ when ys==es -> acc (* first terms until [es] *)
                    | y::ys -> fold_until_es (y::acc) ys
                  in
                  let extracted = List.rev_append (fold_until_es [] e) ts in
                  let reverse = e_sum (b::(List.map e_opp extracted)) in
                  found_term reverse
                end
          in add_case e
      | _ -> false
    in
    let is_var_ok u v =
      match (Vars.mem x u.vars), (Vars.mem x v.vars) with
      | true,false -> is_term_ok u v
      | false,true -> is_term_ok v u
      | _,_ -> false
    in
    let is_eq e  = match e.repr with|Eq(u,v) -> is_var_ok u v |_ -> false in
    let is_neq e = match e.repr with|Neq(u,v)-> is_var_ok u v |_ -> false in
    match q with
    | Lambda -> None
    | Forall ->
        let rec forall_case e = match e.repr with
          | Or b -> List.exists is_neq b
          | Imply (hs,b) -> List.exists is_eq hs || is_neq b
          | Bind(Forall,_,b) -> forall_case b (* skip intermediate forall *)
          | _ -> is_neq e
        in ignore(forall_case a); !res
    | Exists ->
        let rec exists_case e = match e.repr with
          | And b -> List.exists is_eq b
          | Bind(Exists,_,b) -> exists_case b (* skip intermediate exists *)
          | _ -> is_eq e
        in ignore(exists_case a); !res

  let e_bind q x a =
    assert (lc_closed a) ;
    let do_bind =
      match q with Forall | Exists -> Vars.mem x a.vars | Lambda -> true in
    if do_bind then
      match let_intro_case q x a with
      | None -> c_bind q (tau_of_var x) (lc_bind x a)
      | Some e -> lc_subst_var (sigma ()) x e a (* case [let x = e ; a] *)
    else a

  let rec bind_xs q xs e =
    match xs with [] -> e | x::xs -> e_bind q x (bind_xs q xs e)

  let e_forall = bind_xs Forall
  let e_exists = bind_xs Exists
  let e_lambda = bind_xs Lambda

  let rec binders e =
    match e.repr with
    | Bind(q,_,e) -> q :: binders e
    | _ -> []

  (* -------------------------------------------------------------------------- *)
  (* --- Substitutions                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let r_apply = ref (fun _ _ _ -> assert false)

  let rec subst sigma xs d e =
    (* substitute bound variable d+i with xs.(i) for 0 <= i < xs.length *)
    if not (Bvars.overlap d (Array.length xs) e.bind)
    then e else
      match e.repr with
      | Bvar(k,_) -> xs.(k-d)
      | _ ->
          try cache_find sigma e
          with Not_found ->
            cache_bind sigma e
              begin match e.repr with
                | Apply(e,es) ->
                    let e = subst sigma xs d e in
                    let es = List.map (subst sigma xs d) es in
                    !r_apply [] e es
                | _ ->
                    rebuild (subst sigma xs d) e
              end

  let rec apply xs a es =
    match a.repr , es with
    | Bind(_,_,a) , e::es -> apply (e::xs) a es
    | _ ->
        let core =
          if xs=[] then a else
            let sigma = sigma () in
            let xs = Array.of_list xs in
            let d = Bvars.order a.bind + 1 - Array.length xs in
            subst sigma xs d a
        in c_apply core es

  let () = r_apply := apply

  let e_apply e es = apply [] e es

  (* -------------------------------------------------------------------------- *)
  (* --- General Substitutions                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let rec gsubst mu sigma e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Bvar _ -> e
    | _ ->
        try cache_find mu e
        with Not_found ->
          let e0 = rebuild (gsubst mu sigma) e in
          let e1 =
            if lc_closed e0 then
              try sigma e0 with Not_found -> e0
            else e0 in
          cache_bind mu e e1

  let e_subst ?sigma f e =
    let cache = match sigma with None -> ref Tmap.empty | Some c -> c in
    gsubst cache f e

  (* -------------------------------------------------------------------------- *)
  (* --- Iterators                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let e_repr = function
    | True -> e_true
    | False -> e_false
    | Kint z -> e_zint z
    | Kreal r -> e_real r
    | Fvar x -> e_var x
    | Bvar(k,t) -> e_bvar k t
    | Bind(q,t,e) -> c_bind q t e
    | Apply(a,xs) -> e_apply a xs
    | Times(k,e) -> e_times k e
    | Not e -> e_not e
    | Add xs -> addition xs
    | Mul xs -> multiplication xs
    | And xs -> e_and xs
    | Or  xs -> e_or xs
    | Mod(x,y) -> e_mod x y
    | Div(x,y) -> e_div x y
    | Eq(x,y)  -> e_eq  x y
    | Neq(x,y) -> e_neq x y
    | Lt(x,y)  -> e_lt  x y
    | Leq(x,y) -> e_leq x y
    | If(e,a,b) -> e_if e a b
    | Imply(hs,p) -> e_imply hs p
    | Fun(g,xs) -> e_fun g xs
    | Acst(t,v) -> e_const t v
    | Aget(m,k) -> e_get m k
    | Aset(m,k,v) -> e_set m k v
    | Rget(r,f) -> e_getfield r f
    | Rdef fvs -> e_record fvs

  let e_map pool f e =
    match e.repr with
    | Apply(a,xs) -> e_apply (f a) (List.map f xs)
    | Bind(q,t,e) ->
        add_term pool e ;
        let x = fresh pool t in
        e_bind q x (f (lc_open x e))
    | _ -> rebuild f e

  let f_map f n e =
    match e.repr with
    | Bind(q,t,e) -> c_bind q t (f (succ n) e)
    | Apply(a,xs) -> e_apply (f n a) (List.map (f n) xs)
    | _ -> rebuild (f n) e

  let lc_map f e =
    match e.repr with
    | Apply(a,xs) -> e_apply (f a) (List.map f xs)
    | _ -> rebuild f e

  let lc_iter f e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Fvar _ | Bvar _ -> ()
    | Times(_,e) | Not e | Rget(e,_) | Acst(_,e) -> f e
    | Add xs | Mul xs | And xs | Or xs -> List.iter f xs
    | Mod(x,y) | Div(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y)
    | Aget(x,y) -> f x ; f y
    | Rdef fvs -> List.iter (fun (_,v) -> f v) fvs
    | If(e,a,b) | Aset(e,a,b) -> f e ; f a ; f b
    | Imply(xs,x) -> List.iter f xs ; f x
    | Fun(_,xs) -> List.iter f xs
    | Apply(x,xs) -> f x ; List.iter f xs
    | Bind(_,_,e) -> f e

  let e_iter pool f e =
    match e.repr with
    | Bind(_,t,e) ->
        add_term pool e ;
        let x = fresh pool t in
        lc_iter f (lc_open x e)
    | _ -> lc_iter f e

  let f_iter f n e =
    match e.repr with
    | Bind(_,_,e) -> f (succ n) e
    | _ -> lc_iter (f n) e

  (* -------------------------------------------------------------------------- *)
  (* --- Sub-terms                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let change_subterm e pos child =
    let bad_position () = failwith "cannot replace subterm at given position" in
    let rec change_in_list children cur_pos rest =
      match children, cur_pos with
      | [], _ -> bad_position ()
      | e::l, 0 -> (aux e rest) :: l
      | e::l, n -> e :: (change_in_list l (n-1) rest)
    (* since all repr might be shared, better work on an immutable copy than
       on the original array.
    *)
    and aux e pos =
      match pos with
        [] -> child
      | i::l -> begin
          match e.repr with
          | True | False | Kint _ | Kreal _ | Fvar _ | Bvar _ ->
              bad_position ()
          | Times (_,e) when i = 0 && l = [] ->
              begin
                match child.repr with
                  Kint n -> times n e
                | _ -> e_mul child e
              end
          | Times(n,e) when i = 1 -> times n (aux e l)
          | Times _ -> bad_position ()
          | Add ops -> e_sum (change_in_list ops i l)
          | Mul ops -> e_prod (change_in_list ops i l)
          | Div (e1,e2) when i = 0 -> e_div (aux e1 l) e2
          | Div (e1,e2) when i = 1 -> e_div e1 (aux e2 l)
          | Div _ -> bad_position ()
          | Mod (e1,e2) when i = 0 -> e_mod (aux e1 l) e2
          | Mod (e1,e2) when i = 1 -> e_mod e1 (aux e2 l)
          | Mod  _ -> bad_position ()
          | Eq (e1,e2) when i = 0 -> e_eq (aux e1 l) e2
          | Eq (e1,e2) when i = 1 -> e_eq e1 (aux e2 l)
          | Eq _ -> bad_position ()
          | Neq (e1,e2) when i = 0 -> e_neq (aux e1 l) e2
          | Neq (e1,e2) when i = 1 -> e_neq e1 (aux e2 l)
          | Neq _ -> bad_position ()
          | Leq (e1,e2) when i = 0 -> e_leq (aux e1 l) e2
          | Leq (e1,e2) when i = 1 -> e_leq e1 (aux e2 l)
          | Leq _ -> bad_position ()
          | Lt (e1,e2) when i = 0 -> e_lt (aux e1 l) e2
          | Lt (e1,e2) when i = 1 -> e_lt e1 (aux e2 l)
          | Lt _ -> bad_position ()
          | Acst (k,v) when i = 0 -> e_const k v
          | Acst _ -> bad_position ()
          | Aget (e1,e2) when i = 0 -> e_get (aux e1 l) e2
          | Aget (e1,e2) when i = 1 -> e_get e1 (aux e2 l)
          | Aget _ -> bad_position ()
          | And ops -> e_and (change_in_list ops i l)
          | Or ops -> e_or (change_in_list ops i l)
          | Not e when i = 0 -> e_not (aux e l)
          | Not _ -> bad_position ()
          | Imply(ops,e) ->
              let nb = List.length ops in
              if i < nb then e_imply (change_in_list ops i l) e
              else if i = nb then e_imply ops (aux e l)
              else bad_position ()
          | If(e1,e2,e3) when i = 0 -> e_if (aux e1 l) e2 e3
          | If(e1,e2,e3) when i = 1 -> e_if e1 (aux e2 l) e3
          | If(e1,e2,e3) when i = 2 -> e_if e1 e2 (aux e3 l)
          | If _ -> bad_position ()
          | Aset(e1,e2,e3) when i = 0 -> e_set (aux e1 l) e2 e3
          | Aset(e1,e2,e3) when i = 1 -> e_set e1 (aux e2 l) e3
          | Aset(e1,e2,e3) when i = 2 -> e_set e1 e2 (aux e3 l)
          | Aset _ -> bad_position ()
          | Rdef _ | Rget _ ->
              failwith "change in place for records not yet implemented"
          | Fun (f,ops) -> e_fun f (change_in_list ops i l)
          | Bind(q,x,t) when i = 0 -> c_bind q x (aux t l)
          | Bind _ -> bad_position ()
          | Apply(f,args) when i = 0 ->
              e_apply (aux f l) args
          | Apply (f,args) ->
              e_apply f (change_in_list args i l)
        end
    in aux e pos

  (* -------------------------------------------------------------------------- *)
  (* --- DEBUG                                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let pp_bind fmt = function
    | Forall -> Format.pp_print_string fmt "Forall"
    | Exists -> Format.pp_print_string fmt "Exists"
    | Lambda -> Format.pp_print_string fmt "Lambda"

  let pp_var fmt x = Format.fprintf fmt "X%03d(%s:%d)" x.vid x.vbase x.vrank
  let pp_id fmt x = Format.fprintf fmt " #%03d" x.id
  let pp_ids fmt xs = List.iter (pp_id fmt) xs
  let pp_field fmt (f,x) = Format.fprintf fmt "@ %a:%a;" Field.pretty f pp_id x
  let pp_record fmt fxs = List.iter (pp_field fmt) fxs
  let pp_repr fmt = function
    | Kint z -> Format.fprintf fmt "constant %s" (Z.to_string z)
    | Kreal z -> Format.fprintf fmt "real constant %s" (Q.to_string z)
    | True  -> Format.pp_print_string fmt "true"
    | False -> Format.pp_print_string fmt "false"
    | Times(z,x) -> Format.fprintf fmt "times %s%a" (Z.to_string z) pp_id x
    | Add xs -> Format.fprintf fmt "add%a" pp_ids xs
    | Mul xs -> Format.fprintf fmt "mul%a" pp_ids xs
    | And xs -> Format.fprintf fmt "and%a" pp_ids xs
    | Div(a,b) -> Format.fprintf fmt "div%a%a" pp_id a pp_id b
    | Mod(a,b) -> Format.fprintf fmt "mod%a%a" pp_id a pp_id b
    | Or xs -> Format.fprintf fmt "or%a" pp_ids xs
    | If(e,a,b) -> Format.fprintf fmt "if%a%a%a" pp_id e pp_id a pp_id b
    | Imply(hs,p) -> Format.fprintf fmt "imply%a =>%a" pp_ids hs pp_id p
    | Neq(a,b) -> Format.fprintf fmt "neq%a%a" pp_id a pp_id b
    | Eq(a,b) -> Format.fprintf fmt "eq%a%a" pp_id a pp_id b
    | Leq(a,b) -> Format.fprintf fmt "leq%a%a" pp_id a pp_id b
    | Lt(a,b) -> Format.fprintf fmt "lt%a%a" pp_id a pp_id b
    | Not e -> Format.fprintf fmt "not%a" pp_id e
    | Fun(f,es) -> Format.fprintf fmt "fun %a%a" Fun.pretty f pp_ids es
    | Apply(phi,es) -> Format.fprintf fmt "apply%a%a" pp_id phi pp_ids es
    | Fvar x -> Format.fprintf fmt "var %a" pp_var x
    | Bvar(k,_) -> Format.fprintf fmt "bvar #%d" k
    | Bind(q,t,e) -> Format.fprintf fmt "bind %a %a. %a" pp_bind q Tau.pretty t pp_id e
    | Rdef fxs -> Format.fprintf fmt "@[<hov 2>record {%a }@]" pp_record fxs
    | Rget(e,f) -> Format.fprintf fmt "field %a.%a" pp_id e Field.pretty f
    | Aset(m,k,v) -> Format.fprintf fmt "array%a[%a :=%a ]" pp_id m pp_id k pp_id v
    | Aget(m,k) -> Format.fprintf fmt "array%a[%a ]" pp_id m pp_id k
    | Acst(t,v) -> Format.fprintf fmt "const[%a ->%a]" Tau.pretty t pp_id v
  let pp_rid fmt e = pp_repr fmt e.repr

  let rec pp_debug disp fmt e =
    if not (Intset.mem e.id !disp) then
      begin
        Format.fprintf fmt "%a{%a} = %a@."
          pp_id e Bvars.pretty e.bind pp_repr e.repr ;
        disp := Intset.add e.id !disp ;
        pp_children disp fmt e ;
      end

  and pp_children disp fmt e = lc_iter (pp_debug disp fmt) e

  let debug fmt e =
    Format.fprintf fmt "%a with:@." pp_id e ;
    pp_debug (ref Intset.empty) fmt e

  let pretty = debug

  (* ------------------------------------------------------------------------ *)
  (* ---  Record Decomposition                                            --- *)
  (* ------------------------------------------------------------------------ *)

  let record_with fvs =
    let bases = ref Tmap.empty in
    let best = ref None in
    List.iter
      (fun (f,v) ->
         match v.repr with
         | Rget(base,g) when Field.equal f g ->
             let count =
               try succ (Tmap.find base !bases)
               with Not_found -> 1
             in
             bases := Tmap.add base count !bases ;
             ( match !best with
               | Some(_,c) when c < count -> ()
               | _ -> best := Some(base,count) )
         | _ -> ()
      ) fvs ;
    match !best with
    | None -> None
    | Some(base,_) ->
        let fothers = List.filter
            (fun (f,v) ->
               match v.repr with
               | Rget( other , g ) ->
                   other != base || not (Field.equal f g)
               | _ -> true)
            fvs
        in
        Some ( base , fothers )

  (* ------------------------------------------------------------------------ *)
  (* ---  Symbol                                                          --- *)
  (* ------------------------------------------------------------------------ *)

  module Term =
  struct
    type t = term
    let hash = hash
    let equal = equal
    let compare = compare
    let pretty = pretty
    let debug e = Printf.sprintf "E%03d" e.id
  end

  (* ------------------------------------------------------------------------ *)
  (* ---  Sizing Terms                                                    --- *)
  (* ------------------------------------------------------------------------ *)

  let rec count k m e =
    if not (Tset.mem e !m) then
      begin
        incr k ;
        m := Tset.add e !m ;
        lc_iter (count k m) e ;
      end

  let size e =
    let k = ref 0 in count k (ref Tset.empty) e ; !k


  (* ------------------------------------------------------------------------ *)
  (* ---  Sub Term Test                                                   --- *)
  (* ------------------------------------------------------------------------ *)

  let rec scan_subterm m a e =
    if a == e then raise Exit ;
    if a.size <= e.size && not (Tset.mem e !m) then
      begin
        m := Tset.add e !m ;
        if Vars.subset a.vars e.vars then
          lc_iter (scan_subterm m a) e
      end

  let is_subterm a e =
    (a == e) ||
    try scan_subterm (ref Tset.empty) a e ; false
    with Exit -> true

  (* ------------------------------------------------------------------------ *)
  (* ---  Shared Sub-Terms                                                --- *)
  (* ------------------------------------------------------------------------ *)

  type mark =
    | Unmarked  (* first traversal *)
    | FirstMark (* second traversal *)
    | Marked    (* finished *)

  type marks = {
    marked : (term -> bool) ;    (* context-letified terms *)
    shareable : (term -> bool) ; (* terms that can be shared *)
    subterms : (term -> unit) -> term -> unit ; (* subterm iterator *)
    mutable mark : mark Tmap.t ; (* current marks during traversal *)
    mutable shared : Tset.t ;    (* marked several times *)
    mutable roots : term list ;  (* added as marked roots *)
  }

  let get_mark m e =
    try Tmap.find e m.mark
    with Not_found -> Unmarked

  let set_mark m e t =
    m.mark <- Tmap.add e t m.mark

  (* r is the order of the root term being marked,
     it is constant during the recursive traversal.
     This is also the floor of bound variables ;
     bvars k > r can not be shared, as they are not free in the term.
  *)
  let rec walk m r e =
    if not (is_simple e) then
      begin
        match get_mark m e with
        | Unmarked ->
            if m.marked e then
              set_mark m e Marked
            else
              begin
                set_mark m e FirstMark ;
                m.subterms (walk m r) e ;
              end
        | FirstMark ->
            if m.shareable e && lc_closed_at r e
            then m.shared <- Tset.add e m.shared
            else m.subterms (walk m r) e ;
            set_mark m e Marked
        | Marked ->
            ()
      end

  let mark m e =
    m.roots <- e :: m.roots ;
    walk m (Bvars.order e.bind) e

  let share m e =
    if lc_closed e then
      begin
        m.roots <- e :: m.roots ;
        m.shared <- Tset.add e m.shared ;
        m.mark <- Tmap.add e Marked m.mark ;
        m.subterms (walk m (Bvars.order e.bind)) e
      end
    else mark m e

  type defs = {
    mutable stack : term list ;
    mutable defined : Tset.t ;
  }

  let rec collect shared defs e =
    if not (Tset.mem e defs.defined) then
      begin
        lc_iter (collect shared defs) e ;
        if Tset.mem e shared then
          defs.stack <- e :: defs.stack ;
        defs.defined <- Tset.add e defs.defined ;
      end

  let none = fun _ -> false
  let all = fun _ -> true

  let marks ?(shared=none) ?(shareable=all) ?(subterms=lc_iter) () =
    {
      shareable ; subterms ;
      marked = shared ; (* already shared are set to be marked *)
      shared = Tset.empty ; (* accumulator initially empty *)
      mark = Tmap.empty ;
      roots = [] ;
    }

  let defs m =
    let defines = { stack=[] ; defined=Tset.empty } in
    List.iter (collect m.shared defines) m.roots ;
    List.rev defines.stack

  let shared ?shared ?shareable ?subterms es =
    let m = marks ?shared ?shareable ?subterms () in
    List.iter (mark m) es ;
    defs m

  (* -------------------------------------------------------------------------- *)
  (* --- Typing                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let tau_of_sort = function
    | Sint -> Int
    | Sreal -> Real
    | Sbool -> Bool
    | Sprop | Sdata | Sarray _ -> raise Not_found

  let tau_of_arraysort = function
    | Sarray s -> tau_of_sort s
    | _ -> raise Not_found

  let tau_merge a b =
    match a,b with
    | Bool , Bool -> Bool
    | (Bool|Prop) , (Bool|Prop) -> Prop
    | Int , Int -> Int
    | (Int|Real) , (Int|Real) -> Real
    | _ -> raise Not_found

  let rec merge_list t f = function
    | [] -> t
    | e::es -> merge_list (tau_merge t (f e)) f es

  type env = {
    field : Field.t -> tau ;
    record : Field.t -> tau ;
    call : Fun.t -> tau option list -> tau ;
  }

  let rec typecheck env e =
    match e.sort with
    | Sint -> Int
    | Sreal -> Real
    | Sbool -> Bool
    | Sprop -> Prop
    | Sdata | Sarray _ ->
        match e.repr with
        | Bvar (_,ty) -> ty
        | Fvar x -> tau_of_var x
        | Acst(t,v) -> Array(t,typecheck env v)
        | Aset(m,k,v) ->
            (try typecheck env m
             with Not_found ->
               Array(typecheck env k,typecheck env v))
        | Fun(f,es) ->
            (try tau_of_sort (Fun.sort f)
             with Not_found -> env.call f (List.map (typeof env) es))
        | Aget(m,_) ->
            (try match typecheck env m with
               | Array(_,v) -> v
               | _ -> raise Not_found
             with Not_found -> tau_of_arraysort m.sort)
        | Rdef [] -> raise Not_found
        | Rdef ((f,_)::_) -> env.record f
        | Rget (_,f) ->
            (try tau_of_sort (Field.sort f)
             with Not_found -> env.field f)
        | True | False -> Bool
        | Kint _ -> Int
        | Kreal _ -> Real
        | Times(_,e) -> typecheck env e
        | Add es | Mul es -> merge_list Int (typecheck env) es
        | Div (a,b) | Mod (a,b) | If(_,a,b) ->
            tau_merge (typecheck env a) (typecheck env b)
        | Eq _ | Neq _ | Leq _ | Lt _ | And _ | Or _ | Not _ | Imply _ -> Bool
        | Bind((Forall|Exists),_,_) -> Prop
        | Apply _ | Bind(Lambda,_,_) -> raise Not_found

  and typeof env e = try Some (typecheck env e) with Not_found -> None
  
  let undefined _ = raise Not_found
  let typeof ?(field=undefined) ?(record=undefined) ?(call=undefined) e =
    typecheck { field ; record ; call } e

end
