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
(* ---     First Order Logic                                              --- *)
(* -------------------------------------------------------------------------- *)

open Hcons
open Logic

module Make
    (Z:Arith.Z)
    (ADT : Logic.Data)
    (Field : Logic.Field)
    (Fun : Logic.Function) =
struct

  module Z = Z

  (* -------------------------------------------------------------------------- *)

  type tau = (Field.t,ADT.t) Logic.datatype
  type signature = (Field.t,ADT.t) Logic.funtype
  type path = int list

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

  type 'a expression = (Z.t,Field.t,Fun.t,var,'a) term_repr

  type term = {
    id : int ;
    hash : int ;
    size : int ;
    vars : Vars.t ;
    sort : sort ;
    repr : repr ;
  }
  and repr = term expression

  (* ------------------------------------------------------------------------ *)
  (* ---  Term Set,Map and Vars                                           --- *)
  (* ------------------------------------------------------------------------ *)

  module E = 
  struct 
    type t = term 
    let id t = t.id 
    let hash t = t.hash
    let equal = (==)
  end
  module Tset   = Idxset.Make(E)
  module Tmap   = Idxmap.Make(E)

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
  let base_of_var x = x.vbase

  let base_of_tau = function
    | Int -> "i"
    | Real -> "r"
    | Prop -> "p"
    | Bool -> "p"
    | Data(a,_) -> ADT.basename a
    | Array _ -> "t"
    | Tvar 1 -> "a"
    | Tvar 2 -> "b"
    | Tvar 3 -> "c"
    | Tvar 4 -> "d"
    | Tvar 5 -> "e"
    | Tvar _ -> "f"
    | Record _ -> "r"

  type pool = POOL.pool
  let pool = POOL.create

  let add_var pool x = POOL.add pool x
  let add_vars pool xs = Vars.iter (POOL.add pool) xs
  let add_term pool t = Vars.iter (POOL.add pool) t.vars

  let fresh pool ?basename tau =
    let base = match basename with Some base -> base | None -> base_of_tau tau in
    POOL.fresh pool base tau

  let alpha pool x = POOL.alpha pool x

  let rec basename t = match t.repr with
    | Kint _ -> "x"
    | Kreal _ -> "r"
    | Aset(a,_,_) -> basename a
    | _ -> Kind.basename t.sort

  (* -------------------------------------------------------------------------- *)
  (* ---  Representation                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let repr e = e.repr
  let hash e = e.hash
  let id e = e.id
  let vars e = e.vars

  let hash_subterms = function
    | False -> 0
    | True  -> 0
    | Kint n -> Z.hash n
    | Kreal x -> R.hash x
    | Times(n,t) -> Z.hash n * t.hash
    | Add xs | Mul xs | And xs | Or xs -> hash_list hash 0 xs
    | Div(x,y) | Mod(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y) 
    | Aget(x,y) -> hash_pair x.hash y.hash
    | Not e -> succ e.hash
    | Imply(hs,p) -> hash_list hash p.hash hs
    | If(e,a,b) | Aset(e,a,b) -> hash_triple e.hash a.hash b.hash
    | Fun(f,xs) -> hash_list hash (Fun.hash f) xs
    | Rdef fxs -> 
        hash_list (fun (f,x) -> hash_pair (Field.hash f) x.hash) 0 fxs
    | Rget(e,f) -> hash_pair e.hash (Field.hash f)
    | Var x -> Var.hash x
    | Bind(Forall,x,e) -> 1 + 7 * Var.hash x + 31 * e.hash
    | Bind(Exists,x,e) -> 2 + 7 * Var.hash x + 31 * e.hash
    | Bind(Lambda,x,e) -> 3 + 7 * Var.hash x + 31 * e.hash
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
    | Var _   -> 19
    | Bind _  -> 20
    | Apply _ -> 21
    | Aset _  -> 22
    | Aget _  -> 23
    | Rdef _  -> 24
    | Rget _  -> 25

  let hash_repr t = hash_head t + 31 * hash_subterms t

  let equal_repr a b =
    match a,b with
    | True , True -> true
    | False , False -> true
    | Kint n , Kint m -> Z.equal n m
    | Kreal x , Kreal y -> R.equal x y
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
    | Var x , Var y -> Var.equal x y
    | Bind(q,x,e) , Bind(q',x',e') -> q=q' && Var.equal x x' && e==e'
    | Apply(x,ys) , Apply(x',ys') -> x==x' && eq_list ys ys'
    | Rget(x,f) , Rget(x',g) -> x==x' && Field.equal f g
    | Rdef fxs , Rdef gys ->
        equal_list (fun (f,x) (g,y) -> x==y && Field.equal f g) fxs gys
    | _ -> 
        assert (hash_head a <> hash_head b) ; false

  let sort x = x.sort
  let vars x = x.vars

  let vars_repr = function
    | True | False | Kint _ | Kreal _ -> Vars.empty
    | Times(_,x) | Not x | Rget(x,_) -> x.vars
    | Add xs | Mul xs | And xs | Or xs | Fun(_,xs) ->
        Hcons.fold_list Vars.union (fun x -> x.vars) Vars.empty xs
    | Div(x,y) | Mod(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y) | Aget(x,y) ->
        Vars.union x.vars y.vars
    | Imply(xs,a) | Apply(a,xs) ->
        Hcons.fold_list Vars.union vars a.vars xs
    | If(e,a,b) | Aset(e,a,b) -> Vars.union e.vars (Vars.union a.vars b.vars)
    | Var x -> Vars.singleton x
    | Bind(_,x,e) -> Vars.remove x e.vars
    | Rdef fxs -> List.fold_left (fun s (_,x) -> Vars.union s x.vars) Vars.empty fxs

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
    | Rget(_,f) -> Field.sort f
    | Rdef _ -> Sdata
    | Div(x,y) | Mod(x,y) -> Kind.merge x.sort y.sort
    | Leq _ | Lt _ -> Sbool
    | Eq(x,y) | Neq(x,y) -> Kind.merge x.sort y.sort
    | Apply(x,_) -> x.sort
    | If(_,a,b) -> Kind.merge a.sort b.sort
    | Var x -> Kind.of_tau x.vtau
    | Bind((Forall|Exists),_,_) -> Sprop
    | Bind(Lambda,_,e) -> e.sort

  let rec size_list n w = function
    | [] -> n+w
    | x::xs -> size_list (succ n) (max w x.size) xs

  let rec size_rdef n w = function
    | [] -> n+w
    | (_,x)::fxs -> size_rdef (succ n) (max w x.size) fxs

  let size_repr = function
    | True | False | Kint _ -> 0
    | Var _ | Kreal _ -> 1
    | Times(_,x) -> succ x.size
    | Add xs | Mul xs | And xs | Or xs -> size_list 1 0 xs
    | Imply(hs,p) -> size_list 1 p.size hs
    | Not x -> succ x.size
    | Fun(_,xs) -> size_list 1 0 xs
    | Aget(a,b) -> 1 + max a.size b.size
    | Aset(a,b,c) -> 1 + max a.size (max b.size c.size)
    | Rget(a,_) -> 1 + a.size
    | Rdef fxs -> 1 + size_rdef 0 0 fxs
    | Div(x,y) | Mod(x,y) -> 2 + max x.size y.size
    | Eq(x,y) | Neq(x,y) | Lt(x,y) | Leq(x,y) -> 1 + max x.size y.size
    | Apply(x,xs) -> size_list 1 x.size xs
    | If(a,b,c) -> 2 + max a.size (max b.size c.size)
    | Bind(_,_,p) -> 3 + p.size

  (* -------------------------------------------------------------------------- *)
  (* --- Comparison                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  module COMPARE = 
  struct

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

      | Kreal a , Kreal b -> R.compare a b
      | Kreal _ , _ -> (-1)
      | _ , Kreal _ -> 1

      | Var x , Var y -> Var.compare x y
      | Var _ , _ -> (-1)
      | _ , Var _ -> 1


      | Eq(a1,b1) , Eq(a2,b2)
      | Neq(a1,b1) , Neq(a2,b2)
      | Lt(a1,b1) , Lt(a2,b2)
      | Leq(a1,b1) , Leq(a2,b2)
      | Div(a1,b1) , Div(a2,b2)
      | Mod(a1,b1) , Mod(a2,b2) -> 
          let cmp = phi a1 a2 in
          if cmp <> 0 then cmp else phi b1 b2
      | Eq _ , _ -> (-1)
      |  _ , Eq _ -> 1
      | Neq _ , _ -> (-1)
      |  _ , Neq _ -> 1
      | Lt _ , _ -> (-1)
      |  _ , Lt _ -> 1
      | Leq _ , _ -> (-1)
      |  _ , Leq _ -> 1

      | Fun(f,xs) , Fun(g,ys) ->
          let cmp = Fun.compare f g in
          if cmp <> 0 then cmp else 
            Hcons.compare_list phi xs ys
      | Fun _ , _ -> (-1)
      | _ , Fun _ -> 1

      | Times(a,x) , Times(b,y) -> 
          let cmp = Z.compare a b in
          if cmp <> 0 then cmp else phi x y
      | Times _ , _ -> (-1)
      | _ , Times _ -> 1

      | Not x , Not y -> phi x y
      | Not _ , _ -> (-1)
      |  _ , Not _ -> 1

      | Imply(h1,p1) , Imply(h2,p2) ->
          Hcons.compare_list phi (p1::h1) (p2::h2)
      | Imply _ , _ -> (-1)
      |  _ , Imply _ -> 1

      | Add xs , Add ys 
      | Mul xs , Mul ys 
      | And xs , And ys 
      | Or xs , Or ys -> Hcons.compare_list phi xs ys

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
          let cmp = phi a1 a2 in
          if cmp <> 0 then cmp else
            let cmp = phi b1 b2 in
            if cmp <> 0 then cmp else phi c1 c2
      | If _ , _ -> (-1)
      |  _ , If _ -> 1

      | Aget(a1,b1) , Aget(a2,b2) ->
          let cmp = phi a1 a2 in
          if cmp <> 0 then cmp else phi b1 b2
      | Aget _ , _ -> (-1)
      |  _ , Aget _ -> 1

      | Aset(a1,k1,v1) , Aset(a2,k2,v2) ->
          let cmp = phi a1 a2 in
          if cmp <> 0 then cmp else
            let cmp = phi k1 k2 in
            if cmp <> 0 then cmp else phi v1 v2
      | Aset _ , _ -> (-1)
      |  _ , Aset _ -> 1

      | Rget(r1,f1) , Rget(r2,f2) ->
          let cmp = phi r1 r2 in
          if cmp <> 0 then cmp else Field.compare f1 f2
      | Rget _ , _ -> (-1)
      |  _ , Rget _ -> 1

      | Rdef fxs , Rdef gys ->
          Hcons.compare_list (cmp_field phi) fxs gys
      | Rdef _ , _ -> (-1)
      |  _ , Rdef _ -> 1

      | Apply(a,xs) , Apply(b,ys) ->
          Hcons.compare_list phi (a::xs) (b::ys)
      | Apply _ , _ -> (-1)
      | _ , Apply _ -> 1

      | Bind(q1,x1,p1) , Bind(q2,x2,p2) ->
          let cmp = cmp_bind q1 q2 in
          if cmp <> 0 then cmp else
            let cmp = phi p1 p2 in
            if cmp <> 0 then cmp else Var.compare x1 x2

    let rec compare a b =
      if a == b then 0 else
        let cmp = cmp_size a b in
        if cmp <> 0 then cmp else 
          cmp_struct compare a b

  end

  let weigth e = e.size
  let compare = COMPARE.compare

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

  (* -------------------------------------------------------------------------- *)
  (* --- Global State                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  type state = {
    mutable kid : int ;
    weak : W.t ;
    cache : term C.cache ;
    mutable checks : Tset.t Tmap.t ;
    mutable builtins_fun : (term list -> term) BUILTIN.t ;
    mutable builtins_eq  : (term -> term -> term) BUILTIN.t ;
    mutable builtins_leq : (term -> term -> term) BUILTIN.t ;
  }

  let empty () = {
    kid = 0 ;
    weak = W.create 32993 ; (* 3-th Leyland Prime number *)
    cache = C.create ~size:0x1000 ; (* 4096 entries *)
    checks = Tmap.empty ;
    builtins_fun = BUILTIN.empty ;
    builtins_eq  = BUILTIN.empty ;
    builtins_leq = BUILTIN.empty ;
  }

  let state = ref (empty ())
  let get_state () = !state
  let set_state st = state := st
  let clr_state st = 
    begin
      C.clear st.cache ;
      st.checks <- Tmap.empty ;
    end
  let release () = clr_state !state

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

  (* -------------------------------------------------------------------------- *)
  (* --- Hconsed insertion                                                  --- *)
  (* -------------------------------------------------------------------------- *)

  let insert r =
    let h = hash_repr r in
    (* Only [hash] and [repr] are significant for lookup in weak hmap *)
    let e0 = {
      id=0 ;
      hash=h ;
      repr=r ;
      size=0;
      vars=Vars.empty ;
      sort=Sdata ;
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
          try Tmap.find x !state.checks
          with Not_found -> Tset.empty in
        !state.checks <- Tmap.add x (Tset.add y s) !state.checks
      end ;
    x

  let check_unit ~qed ~raw = 
    let p = insert (Eq(qed,raw)) in
    Vars.fold (fun x p -> insert (Bind(Forall,x,p))) p.vars p

  let iter_checks f = 
    Tmap.iter 
      (fun qed s -> Tset.iter (fun raw -> f ~qed ~raw) s) 
      !state.checks

  (* -------------------------------------------------------------------------- *)
  (* --- Constructors for normalized terms                                  --- *)
  (* -------------------------------------------------------------------------- *)

  let e_false  = constant (insert False)
  let e_true   = constant (insert True)
  let e_zero   = constant (insert (Kint Z.zero))
  let e_one    = constant (insert (Kint Z.one))
  let e_int n  = insert (Kint (Z.of_int n))
  let e_zint z = insert (Kint z)
  let e_real x = insert (Kreal x)
  let e_var x  = insert(Var x)

  let c_div x y = insert (Div(x,y))
  let c_mod x y = insert (Mod(x,y))
  let c_leq x y = insert (Leq(x,y))
  let c_lt  x y = insert (Lt (x,y))
  let insert_eq  x y = insert (Eq (x,y))
  let insert_neq x y = insert (Neq(x,y))
  let sym c x y = if compare x y > 0 then c y x else c x y
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
    | xs -> insert(And(List.sort compare xs))

  let c_or = function
    | [] -> e_false
    | [x] -> x
    | xs -> insert(Or(List.sort compare xs))

  let c_imply hs p = insert(Imply(List.sort compare hs,p))

  let c_not x = insert(Not x)

  let c_if e a b = insert(If(e,a,b))

  let c_apply a es = if es=[] then a else insert(Apply(a,es))

  let c_bind q x e = insert(Bind(q,x,e))

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

  let insert _ = assert false (* [insert] should not be used afterwards *)

  let rec subterm e = function
      [] -> e
    | n :: l ->
        let children = match e.repr with
          | True | False | Kint _ | Kreal _ | Var _ -> []
          | Times (n,e) -> [ e_zint n; e]
          | Add l | Mul l | And l | Or l | Fun (_,l) -> l
          | Div (e1,e2) | Mod (e1,e2) | Eq(e1,e2) | Neq(e1,e2)
          | Leq (e1,e2) | Lt(e1,e2) | Aget(e1,e2) -> [e1;e2]
          | Not e | Bind(_,_,e) -> [e]
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

  let builtin_leq a b =
    let simplify g = (BUILTIN.find g !state.builtins_leq) a b
    in
    match a.repr with
    | Fun(f,_) -> (try simplify f
                   with Not_found -> (match b.repr with
	           | Fun(g,_) when not (Fun.equal f g) -> simplify g
		   | _ -> raise Not_found))
    | _ -> (match b.repr with | Fun(g,_) -> simplify g | _ -> raise Not_found)

  let builtin_eq a b =
    let simplify g x y = (BUILTIN.find g !state.builtins_eq) x y
    in
    match a.repr with
    | Fun(f,_) -> (try simplify f a b
                   with Not_found -> (match b.repr with
	           | Fun(g,_) when not (Fun.equal f g) -> simplify g b a
		   | _ -> raise Not_found))
    | _ -> (match b.repr with | Fun(g,_) -> simplify g b a | _ -> raise Not_found)


  let cached_not = ref (fun _ -> assert false)
  let extern_not = ref (fun _ -> assert false)
  let extern_ite = ref (fun _ -> assert false)
  let extern_eq = ref (fun _ -> assert false)
  let extern_neq = ref (fun _ -> assert false)
  let extern_leq = ref (fun _ -> assert false)
  let extern_lt = ref (fun _ -> assert false)
  let extern_fun = ref (fun _ -> assert false)

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

  let distribute_if_over_operation op x y f a b =
    match a.repr, b.repr with
    | _, _ when true (* [PB] true: until alt-ergo 0.95.2 trouble *)
	-> op x y
    | If(ac,a1,a2), _ when (is_primitive a1 || is_primitive a2) && is_primitive b
	-> !extern_ite ac (f a1 b) (f a2 b)
    | _, If(bc,b1,b2) when (is_primitive b1 || is_primitive b2) && is_primitive a
	-> !extern_ite bc (f a b1) (f a b2)
    | If(ac,a1,a2), If(bc,b1,b2) when ac == bc
	-> !extern_ite ac (f a1 b1) (f a2 b2)
    | If(ac,a1,a2), If(_,b1,b2) when (is_primitive a1 && is_primitive a2) && (is_primitive b1 || is_primitive b2)
	-> !extern_ite ac (f a1 b) (f a2 b)
    | If(_,a1,a2), If(bc,b1,b2) when (is_primitive a1 || is_primitive a2) && (is_primitive b1 && is_primitive b2)
	-> !extern_ite bc (f a b1) (f a b2)
    | _ -> op x y

  let c_builtin_fun f = function
    | x::[] as xs -> (match x.repr with
			| If(c,a,b) ->  !extern_ite c (!extern_fun f [a]) (!extern_fun f [b])
			| _ -> operation (FUN(f,xs)))
    | a::b::[] as xs ->   distribute_if_over_operation (fun f xs -> operation (FUN(f,xs))) f xs (fun a b -> !extern_fun f [a;b]) a b
    | xs -> operation (FUN(f,xs))
  let c_builtin_eq  a b = distribute_if_over_operation (fun a b -> operation (CMP(EQ ,a,b))) a b !extern_eq  a b
  let c_builtin_neq a b = distribute_if_over_operation (fun a b -> operation (CMP(NEQ,a,b))) a b !extern_neq a b
  let c_builtin_lt  a b = distribute_if_over_operation (fun a b -> operation (CMP(LT ,a,b))) a b !extern_lt  a b
  let c_builtin_leq a b = distribute_if_over_operation (fun a b -> operation (CMP(LEQ,a,b))) a b !extern_leq a b

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

  (* -------------------------------------------------------------------------- *)
  (* --- Negation                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let e_not p =
    match p.repr with
    | True -> e_false
    | False -> e_true
    | Lt(x,y) -> c_leq y x
    | Leq(x,y) -> c_lt y x
    | Eq(x,y) -> c_neq x y
    | Neq(x,y) -> c_eq x y
    | Not x -> x
    | (And _ | Or _ | Imply _) -> operation (NOT p)
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

  let op_inversible xs ys =
    let rec simpl modified turn xs ys = match xs , ys with
      | x::xs , y::ys when x==y -> simpl true turn xs ys
      | _ -> 
          let xs = List.rev xs in
          let ys = List.rev ys in
          if turn 
          then simpl modified false xs ys
          else modified,xs,ys
    in simpl false true xs ys

  let element = function
    | E_none -> assert false
    | E_int k -> e_int k
    | E_true -> e_true
    | E_false -> e_false
    | E_const f -> c_fun f []

  let is_element e x = match e , x.repr with
    | E_int k , Kint z -> Z.equal (Z.of_int k) z
    | E_true , True -> true
    | E_false , False -> false
    | E_const f , Fun(g,[]) -> Fun.equal f g
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

  let e_funraw = c_fun
  let e_fun = e_fungen 
  let () = extern_fun := e_fun

  (* -------------------------------------------------------------------------- *)
  (* --- Symbols                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  type t = term
  let equal = (==)

  let is_atomic e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Var _ -> true
    | _ -> false

  let is_simple e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Var _ | Fun(_,[]) -> true
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
  (* --- Ground & Arithmetics                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let z_op c f x y =
    match x.repr , y.repr with
    | Kint z , Kint z' -> e_zint (f z z')
    | _ -> c x y

  type sign = Null | Negative | Positive
  let sign z = 
    if Z.lt z Z.zero then Negative else
    if Z.lt Z.zero z then Positive else
      Null

  let affine_rel fc fe c xs ys = 
    match xs , ys with
    | [] , [] -> if fc c Z.zero then e_true else e_false
    | [] , _ -> fe (e_zint c) (c_add ys) (* c+0 R ys <-> c R ys *)
    | _ , [] -> fe (c_add xs) (e_zint (Z.neg c)) (* c+xs R 0 <-> xs R -c *)
    | _ -> 
        match sign c with
        | Null -> fe (c_add xs) (c_add ys)
        (* 0+xs R ys <-> xs R ys *)
        | Negative -> fe (c_add xs) (c_add (e_zint (Z.neg c) :: ys))
        (* c+xs R ys <-> xs R (-c+ys) *)
        | Positive -> fe (c_add (e_zint c :: xs)) (c_add ys)
  (* c+xs R ys <-> (c+xs) R ys *)

  let affine_eq = affine_rel Z.equal c_builtin_eq
  let affine_neq = affine_rel (fun x y -> not (Z.equal x y)) c_builtin_neq

  let affine_leq c xs ys =
    if Z.equal c Z.one && List.for_all is_int xs && List.for_all is_int ys
    then affine_rel Z.lt c_builtin_lt Z.zero xs ys
    else affine_rel Z.leq c_builtin_leq c xs ys

  let affine_lt c xs ys =
    if not (Z.equal c Z.zero) && List.for_all is_int xs && List.for_all is_int ys 
    then affine_rel Z.leq c_builtin_leq (Z.succ c) xs ys 
    else affine_rel Z.lt c_builtin_lt c xs ys

  let rec ground f c xs = function
    | {repr=Kint n}::ts -> ground f (f c n) xs ts
    | x::ts -> ground f c (x::xs) ts
    | [] -> c , xs

  (* --- Times --- *)

  let rec times z e =
    if Z.equal z Z.one then e else
    if Z.equal z Z.zero then e_zint Z.zero else
      match e.repr with
      | Kint z' -> e_zint (Z.mul z z')
      | Kreal r when Z.equal z Z.minus_one -> e_real (R.opp r)
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
    | Kreal z when R.is_zero z -> acc
    | Kreal r when R.negative r -> (Z.neg k,e_real (R.opp r)) :: acc
    | _ -> (k,t) :: acc

  (* sorts monoms by terms *)
  let compare_monoms (_,t1) (_,t2) = Pervasives.compare t1.id t2.id

  (* factorized monoms *)
  let rec fold_monom ts k t =
    if Z.equal Z.zero k then ts else
    if Z.equal Z.one k then t::ts else
      times k t :: ts

  (* monoms sorted by terms *)
  let rec fold_affine f a = function
    | (n1,t1)::(n2,t2)::kts when t1 == t2 ->
        fold_affine f a ((Z.add n1 n2,t1)::kts)
    | (k,t)::kts ->
        fold_affine f (f a k t) kts
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
    | Kint _ | Times _ | Add _ -> true
    | Kreal z -> R.is_zero z
    | _ -> false

  let rec partition_monoms phi c xs ys = function
    | [] -> phi c xs ys
    | (k,t) :: kts ->
        if t == e_one 
        then partition_monoms phi (Z.add k c) xs ys kts
        else
        if Z.leq Z.zero k
        then partition_monoms phi c (fold_monom xs k t) ys kts
        else partition_monoms phi c xs (fold_monom ys (Z.neg k) t) kts

  let relation cmp affine_cmp x y =
    if is_affine x || is_affine y then
      let kts = unfold_affine1 (unfold_affine1 [] Z.one x) Z.minus_one y in
      let kts = List.sort compare_monoms kts in
      let kts = fold_affine (fun ts k t -> (k,t)::ts) [] kts in
      partition_monoms affine_cmp Z.zero [] [] kts
    else cmp x y

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
    let s,ts = ground Z.mul Z.one [] ts in
    if Z.equal Z.zero s then e_zint Z.zero else
    if ts=[] then e_zint s else
      let t = c_mul ts in
      if Z.equal s Z.one then t else c_times s t

  (* --- Divisions --- *)

  let e_div a b = match b.repr with
    | Kint z when Z.equal z Z.one -> a
    | _ -> c_div a b

  let e_mod a b = match b.repr with
    | Kint z when Z.equal z Z.one -> a
    | _ -> c_mod a b

  (* --- Comparisons --- *)

  let e_lt x y = 
    if x==y then e_false else relation c_builtin_lt affine_lt x y 
  let () = extern_lt := e_lt

  let e_leq x y = 
    if x==y then e_true else relation c_builtin_leq affine_leq x y
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

  exception Absorbant

  let rec fold_and acc xs =
    match xs with
    | [] -> acc
    | x::others ->
        match x.repr with
        | False  -> raise Absorbant
        | True   -> fold_and acc others
        | And xs -> fold_and (fold_and acc xs) others
        | _      -> fold_and ((x,e_not x)::acc) others

  let rec fold_or acc xs =
    match xs with
    | [] -> acc
    | x::others ->
        match x.repr with
        | True  -> raise Absorbant
        | False -> fold_or acc others
        | Or xs -> fold_or (fold_or acc xs) others
        | _     -> fold_or ((x,e_not x)::acc) others

  (* an atom is (t,not t) *)

  let atom_eq a b = fst a == fst b
  let atom_opp a b = fst a == snd b || snd a == fst a

  let compare_atom (x1,nx1) (x2,nx2) =
    Pervasives.compare (min x1.id nx1.id) (min x2.id nx2.id)

  let rec fact_atom acc ms =
    match acc , ms with
    | a::_ , b::qs when atom_eq a b -> fact_atom acc qs
    | a::_ , b::_ when atom_opp a b -> raise Absorbant
    | _ , b::qs -> fact_atom (b::acc) qs
    | _ , [] -> acc

  let conjunction ts =
    try
      let ms = fold_and [] ts in
      let ms = fact_atom [] (List.sort compare_atom ms) in
      c_and (List.map fst ms)
    with Absorbant -> e_false

  let disjunction ts =
    try
      let ms = fold_or [] ts in
      let ms = fact_atom [] (List.sort compare_atom ms) in
      c_or (List.map fst ms)
    with Absorbant -> e_true

  let rec implication a b =
    match a.repr , b.repr with
    | True , _ -> b
    | False , _ -> e_true
    | _ , True -> e_true
    | _ , False -> e_not a
    | Not p , Not q -> implication q p
    | And ts , _ ->
        if List.memq b ts then e_true else
          let c = e_not b in
          begin
            match List.filter (fun t -> t != c) ts with
            | [] -> b
            | ts -> c_imply ts b
          end
    | _ ->
        if a == b then e_true else
          let c = e_not b in
          if c == a then c else c_imply [a] b

  type structural =
    | S_equal        (* equal constants or constructors *)
    | S_disequal     (* different constants or constructors *)
    | S_injection    (* same function, injective or constructor *)
    | S_inversible   (* same function, inversible on both side *)
    | S_disjunction  (* both constructors, but different ones *)
    | S_functions    (* general functions *)

  let structural f g =
    if Fun.equal f g then
      match Fun.category f with
      | Logic.Injection -> S_injection
      | Logic.Operator { inversible=true } -> S_inversible
      | Logic.Constructor -> S_equal
      | Logic.Function | Logic.Operator _ -> S_functions
    else
      match Fun.category f , Fun.category g with
      | Logic.Constructor , Logic.Constructor -> S_disequal
      | _ -> S_functions

  let contrary x y = (is_prop x || is_prop y) && (e_not x == y)

  let rec eq_all phi xs ys =
    match xs , ys with
    | [] , [] -> Yes
    | [] , _ | _ , [] -> No
    | x::xs , y::ys ->
        match (phi x y).repr with
        | False -> No
        | True -> eq_all phi xs ys
        | _ -> match eq_all phi xs ys with
          | No -> No
          | Yes | Maybe -> Maybe

  let rec neq_any phi xs ys =
    match xs , ys with
    | [] , [] -> No
    | [] , _ | _ , [] -> Yes
    | x::xs , y :: ys ->
        match (phi x y).repr with
        | True -> Yes
        | False -> neq_any phi xs ys
        | _ -> match neq_any phi xs ys with
          | Yes -> Yes
          | No | Maybe -> Maybe

  (* -------------------------------------------------------------------------- *)
  (* --- Equality on R                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let kreal_of_kint z = R.of_string (Z.to_string z ^ ".0")

  let eq_real x y z z' =
    match R.eq z z' with
    | R.Sure_true -> e_true
    | R.Sure_false -> e_false
    | R.Unknown -> c_eq x y

  let neq_real x y z z' =
    match R.neq z z' with
    | R.Sure_true -> e_true
    | R.Sure_false -> e_false
    | R.Unknown -> c_neq x y

  (* -------------------------------------------------------------------------- *)
  (* --- Equality                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let rec e_eq x y =
    if x == y then e_true else relation eq_symb affine_eq x y

  and eq_symb x y =
    match x.repr , y.repr with
    | Kint z , Kint z' -> if Z.equal z z' then e_true else e_false
    | Kreal z , Kreal z' -> eq_real x y z z'
    | Kint a , Kreal r | Kreal r , Kint a -> eq_real x y r (kreal_of_kint a)
    | True , _ -> y
    | _ , True -> x
    | False , _ -> e_not y
    | _ , False -> e_not x
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_equal -> e_true
          | S_disequal -> e_false
          | S_injection -> eq_maybe x y (eq_all e_eq xs ys)
          | S_disjunction -> e_false
          | S_functions -> c_builtin_eq x y
          | S_inversible ->
              let modified,xs,ys = op_inversible xs ys in
              if modified
              then c_builtin_eq (e_fun f xs) (e_fun g ys) 
              else c_builtin_eq x y
        end
    | Rdef fxs , Rdef gys ->
        begin
          try eq_maybe x y (eq_all eq_field fxs gys)
          with Exit -> e_false
        end
    | _ when contrary x y -> e_false
    | Fun _ , _ | _ , Fun _ -> c_builtin_eq x y
    | _ -> c_eq x y

  and eq_maybe x y = function 
    | Yes -> e_true | No -> e_false | Maybe -> c_builtin_eq x y

  and eq_field (f,x) (g,y) =
    if Field.equal f g then e_eq x y else raise Exit
  let () = extern_eq := e_eq

  (* -------------------------------------------------------------------------- *)
  (* --- Disequality                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec e_neq x y =
    if x == y then e_false else relation neq_symb affine_neq x y

  and neq_symb x y =
    match x.repr , y.repr with
    | Kint z , Kint z' -> if Z.equal z z' then e_false else e_true
    | Kreal z , Kreal z' -> neq_real x y z z'
    | Kreal r , Kint a | Kint a , Kreal r -> neq_real x y r (kreal_of_kint a)
    | True , _ -> e_not y
    | _ , True -> e_not x
    | False , _ -> y
    | _ , False -> x
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_equal -> e_false
          | S_disequal -> e_true
          | S_injection -> neq_maybe x y (neq_any e_neq xs ys)
          | S_disjunction -> e_true
          | S_functions -> c_builtin_neq x y
          | S_inversible ->
              let modified,xs,ys = op_inversible xs ys in
              if modified 
              then c_builtin_neq (e_fun f xs) (e_fun g ys) 
              else c_builtin_neq x y
        end
    | Rdef fxs , Rdef gys ->
        begin
          try neq_maybe x y (neq_any neq_field fxs gys)
          with Exit -> e_true
        end
    | _ when contrary x y -> e_true
    | Fun _ , _ | _ , Fun _ -> c_builtin_neq x y
    | _ -> c_neq x y

  and neq_maybe x y = function 
    | Yes -> e_true | No -> e_false | Maybe -> c_builtin_neq x y

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

  let e_imply hs p =
    match p.repr with
    | Imply(hs',p') -> implication (e_and (hs @ hs')) p'
    | _ -> implication (e_and hs) p

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
    | Aset(m0,k0,_) ->
        begin
          match are_equal k k0 with
          | Yes -> e_set m0 k0 v
          | No | Maybe -> c_set m k v
        end
    | _ -> c_set m k v

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
    | Kint _ | Kreal _ | True | False -> e
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
    | Aget(x,y) -> e_get (f x) (f y)
    | Aset(x,y,z) -> e_set (f x) (f y) (f z)
    | Rget(x,g) -> e_getfield (f x) g
    | Rdef gxs -> e_record (List.map (fun (g,x) -> g, f x) gxs)
    | Var _ | Bind _ | Apply _ -> assert false

  (* -------------------------------------------------------------------------- *)
  (* --- Binders                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  exception Applies

  let e_bind q x a =
    match q with
    | (Forall | Exists) ->
        if not (Vars.mem x a.vars) then a
        else c_bind q x a 
    | Lambda ->
        c_bind q x a

  let rec e_forall xs a = 
    match xs with
    | [] -> a
    | x::xs ->
        let a = e_forall xs a in
        if Vars.mem x a.vars then c_bind Forall x a else a

  let rec e_exists xs a =
    match xs with
    | [] -> a
    | x::xs ->
        let a = e_exists xs a in
        if Vars.mem x a.vars then c_bind Exists x a else a

  let rec e_lambda xs a =
    match xs with
    | [] -> a
    | x::xs -> e_bind Lambda x (e_lambda xs a)

  (* -------------------------------------------------------------------------- *)
  (* --- Substitutions                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  (* substitution environment *)
  type senv = {
    pool : POOL.pool ;
    mutable hmem : term Intmap.t ; (* memoization table *)
    sigma : term Intmap.t ; (* substitution : var.id -> term *)
    domain : Vars.t ; (* Domain(sigma) *)
    codomain : Vars.t ; (* Codomain (sigma) *)
  }

  let senv pool = { 
    pool = pool ;
    hmem = Intmap.empty ; 
    sigma = Intmap.empty ; 
    domain = Vars.empty ;
    codomain = Vars.empty ;
  }

  let rec e_apply ?pool (a:term) (xs:term list) : term = 
    if xs=[] then a else 
      let pool = match pool with Some p -> p | None ->
          let p = POOL.create () in
          add_term p a ; List.iter (add_term p) xs ; p
      in
      reduction (senv pool) a xs

  and reduction senv (a:term) (args:term list) : term =
    match a.repr , args with
    | Bind(_,x,core) , arg::args -> 
        let senv = 
          { senv with
            sigma = Intmap.add x.vid arg senv.sigma ;
            domain = Vars.add x senv.domain ;
            codomain = Vars.union arg.vars senv.codomain ;
          } in
        reduction senv core args
    | _ ->
        (* sigma is now as much as possible *)
        if Vars.is_empty senv.domain 
        then c_apply a args
        else c_apply (apply_subst senv a) args

  and apply_subst senv (a:term) : term =
    if not (Vars.intersect a.vars senv.domain) then a 
    else
      try Intmap.find a.id senv.hmem (* memoized *)
      with Not_found ->
          let result =
            match a.repr with
            | Var x -> 
                (try Intmap.find x.vid senv.sigma with Not_found -> a)
            | Bind(q,x,b) ->
                if Vars.mem x senv.codomain then
                  let y = POOL.alpha senv.pool x in
                  let senv0 = {
                    pool = senv.pool ;
                    hmem = Intmap.empty ;
                    domain = Vars.add x senv.domain ;
                    codomain = Vars.add y senv.codomain ;
                    sigma = Intmap.add x.vid (e_var y) senv.sigma 
                  } in
                  e_bind q y (apply_subst senv0 b) 
                else
                  e_bind q x (apply_subst senv b)
            | Apply(phi,vs) -> 
                let vs' = List.map (apply_subst senv) vs in
                let phi' = apply_subst senv phi in
                e_apply phi' vs'
            | _ -> 
                rebuild (apply_subst senv) a
          in (* memoization *)
          senv.hmem <- Intmap.add a.id result senv.hmem ; 
          result

  let e_subst ?pool x a b =
    let pool = match pool with Some p -> p | None ->
        let p = POOL.create () in
        add_var p x ; add_term p a ; add_term p b ; p in
    let senv = {
      pool = pool ;
      hmem = Intmap.empty ;
      domain = Vars.singleton x ;
      codomain = a.vars ;
      sigma = Intmap.add x.vid a Intmap.empty ;
    } in
    apply_subst senv b

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
  let e_times k x =
    if Z.equal k Z.zero then e_zero else
    if Z.equal k Z.one then x else
      times k x

  (* -------------------------------------------------------------------------- *)
  (* --- Congruence                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  exception NO_CONGRUENCE
  exception FIELD_NEQ

  let rec concat2 f xs ys = match xs,ys with
    | [],[] -> []
    | x::xs , y::ys -> f x y @ (concat2 f xs ys)
    | _ -> raise NO_CONGRUENCE

  let rec congr_eq a b =
    match a.repr , b.repr with
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_equal | S_disequal | S_disjunction | S_inversible -> []
          | S_injection -> concat2 congr_argeq xs ys
          | S_functions -> raise NO_CONGRUENCE
        end
    | Rdef fxs , Rdef gys -> concat2 congr_fieldeq fxs gys
    | _ -> raise NO_CONGRUENCE

  and congr_argeq a b = try congr_eq a b with NO_CONGRUENCE -> [a,b]
  and congr_fieldeq (f,a) (g,b) =
    if Field.equal f g then congr_argeq a b else raise NO_CONGRUENCE

  let congruence_eq a b = try Some (congr_eq a b) with NO_CONGRUENCE -> None

  let rec congr_neq a b =
    match a.repr , b.repr with
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_equal | S_disequal | S_disjunction | S_inversible -> []
          | S_injection -> concat2 congr_argneq xs ys
          | S_functions -> raise NO_CONGRUENCE
        end
    | Rdef fxs , Rdef gys -> 
        begin
          try concat2 congr_fieldneq fxs gys
          with FIELD_NEQ -> []
        end
    | _ -> raise NO_CONGRUENCE

  and congr_argneq a b = try congr_neq a b with NO_CONGRUENCE -> [a,b]
  and congr_fieldneq (f,a) (g,b) =
    if Field.equal f g then congr_argneq a b else raise FIELD_NEQ

  let congruence_neq a b = try Some(congr_neq a b) with NO_CONGRUENCE -> None

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
  (* --- Flat Reasoning                                                  --- *)
  (* -------------------------------------------------------------------------- *)

  let rec flat_eq a b =
    match a.repr , b.repr with
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_equal -> e_true
          | S_disequal -> e_false
          | S_injection -> e_all2 flat_eq xs ys
          | S_disjunction -> e_false
          | S_functions | S_inversible -> e_eq a b
        end
    | Rdef fxs , Rdef gys ->
        begin
          try e_all2 (fun (f,x) (g,y) -> 
              if Field.equal f g then flat_eq x y else raise Exit
            ) fxs gys
          with Exit -> e_false
        end
    | _ -> e_eq a b

  let rec flat_neq a b =
    match a.repr , b.repr with
    | Fun(f,xs) , Fun(g,ys) ->
        begin
          match structural f g with
          | S_equal -> e_false
          | S_disequal -> e_true
          | S_injection -> e_any2 flat_neq xs ys
          | S_disjunction -> e_true
          | S_functions | S_inversible -> e_neq a b
        end
    | Rdef fxs , Rdef gys ->
        begin
          try e_any2 (fun (f,x) (g,y) -> 
              if Field.equal f g then flat_neq x y else raise Exit
            ) fxs gys
          with Exit -> e_true
        end
    | _ -> e_neq a b

  let flattens a b = match a.repr , b.repr with
    | (Rdef _ | Fun _) , (Rdef _ | Fun _) -> true
    | _ -> false

  let rec flat qs e = match e.repr with
    | Eq(a,b) when flattens a b -> (flat_eq a b)::qs
    | Neq(a,b) when flattens a b -> (flat_neq a b)::qs
    | And ps -> List.fold_left flat qs ps
    | _ -> e::qs

  let flatten p = List.rev (flat [] p)

  let flattenable e = match e.repr with
    | Eq(a,b) | Neq(a,b) -> flattens a b
    | And _ -> true
    | _ -> false

  (* -------------------------------------------------------------------------- *)
  (* --- Iterators                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let e_repr = function
    | True -> e_true
    | False -> e_false
    | Kint z -> e_zint z
    | Kreal r -> e_real r
    | Var x -> e_var x
    | Bind(q,x,e) -> e_bind q x e
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
    | Aget(m,k) -> e_get m k
    | Aset(m,k,v) -> e_set m k v
    | Rget(r,f) -> e_getfield r f
    | Rdef fvs -> e_record fvs

  let e_map f e =
    match e.repr with
    | Var _ -> e
    | Apply(a,xs) -> e_apply (f a) (List.map f xs)
    | Bind _ -> raise (Invalid_argument "Qed.Term.e_map")
    | _ -> rebuild f e

  let f_map f xs e =
    match e.repr with
    | Var _ -> e
    | Apply(a,ps) -> e_apply (f xs a) (List.map (f xs) ps)
    | Bind(q,x,p) -> e_bind q x (f (Vars.add x xs) p)
    | _ -> rebuild (f xs) e

  let r_map f = function
    | True -> e_true
    | False -> e_false
    | Kint z -> e_zint z
    | Kreal r -> e_real r
    | Var x -> e_var x
    | Apply(a,xs) -> e_apply (f a) (List.map f xs)
    | Bind _ -> raise (Invalid_argument "Qed.Term.r_map")
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
    | Aget(x,y) -> e_get (f x) (f y)
    | Aset(x,y,z) -> e_set (f x) (f y) (f z)
    | Rget(x,g) -> e_getfield (f x) g
    | Rdef gxs -> e_record (List.map (fun (g,x) -> g, f x) gxs)

  let e_iter f e =
    match e.repr with
    | True | False | Kint _ | Kreal _ | Var _ -> ()
    | Times(_,e) | Not e | Bind(_,_,e) | Rget(e,_) -> f e
    | Add xs | Mul xs | And xs | Or xs -> List.iter f xs
    | Mod(x,y) | Div(x,y) | Eq(x,y) | Neq(x,y) | Leq(x,y) | Lt(x,y)
    | Aget(x,y) -> f x ; f y
    | Rdef fvs -> List.iter (fun (_,v) -> f v) fvs
    | If(e,a,b) | Aset(e,a,b) -> f e ; f a ; f b
    | Imply(xs,x) -> List.iter f xs ; f x
    | Apply(x,xs) -> f x ; List.iter f xs
    | Fun(_,xs) -> List.iter f xs

  let f_iter f xs e =
    match e.repr with
    | Bind(_,x,e) -> f (Vars.add x xs) e
    | _ -> e_iter (f xs) e

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
          | True | False | Kint _ | Kreal _ | Var _ ->
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
          | Bind(q,x,t) when i = 0 -> e_bind q x (aux t l)
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
    | Kreal z -> Format.fprintf fmt "real constant %s" (R.to_string z)
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
    | Var x -> Format.fprintf fmt "var %a" pp_var x
    | Bind(q,x,e) -> Format.fprintf fmt "bind %a %a. %a" pp_bind q pp_var x pp_id e
    | Rdef fxs -> Format.fprintf fmt "@[<hov 2>record {%a }@]" pp_record fxs
    | Rget(e,f) -> Format.fprintf fmt "field %a.%a" pp_id e Field.pretty f
    | Aset(m,k,v) -> Format.fprintf fmt "array%a[%a :=%a ]" pp_id m pp_id k pp_id v
    | Aget(m,k) -> Format.fprintf fmt "array%a[%a ]" pp_id m pp_id k
  let pp_rid fmt e = pp_repr fmt e.repr

  let rec pp_debug disp fmt e =
    if not (Intset.mem e.id !disp) then
      begin
        Format.fprintf fmt "%a = %a@." pp_id e pp_repr e.repr ;
        disp := Intset.add e.id !disp ;
        pp_children disp fmt e ;
      end

  and pp_children disp fmt e = e_iter (pp_debug disp fmt) e

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
  (* ---  Sizing Terms                                                    --- *)
  (* ------------------------------------------------------------------------ *)

  let rec count k m e =
    if not (Tset.mem e !m) then 
      begin
        incr k ;
        m := Tset.add e !m ;
        e_iter (count k m) e ;
      end

  let size e = 
    let k = ref 0 in count k (ref Tset.empty) e ; !k

  (* ------------------------------------------------------------------------ *)
  (* ---  Shared Sub-Terms                                                --- *)
  (* ------------------------------------------------------------------------ *)

  type mark =
    | Unmarked  (* first traversal *)
    | FirstMark (* second traversal *)
    | Marked    (* finished *)

  type marks = {
    closed : Vars.t ;            (* context-declared variables *)
    marked : (term -> bool) ;    (* context-letified terms *)
    shareable : (term -> bool) ; (* terms that can be shared *)
    mutable mark : mark Tmap.t ; (* current marks during traversal *)
    mutable shared : Tset.t ;    (* marked several times *)
    mutable roots : term list ;  (* added as marked roots *)
  }

  let get_mark m e =
    try Tmap.find e m.mark 
    with Not_found -> Unmarked

  let set_mark m e t =
    m.mark <- Tmap.add e t m.mark

  let rec walk m xs e =
    if not (is_simple e) then
      begin
        match get_mark m e with
        | Unmarked ->
            if m.marked e then 
              set_mark m e Marked
            else 
              begin
                set_mark m e FirstMark ;
                f_iter (walk m) xs e ;
              end
        | FirstMark ->
            if m.shareable e 
            && Vars.subset e.vars m.closed 
            && not (Vars.intersect e.vars xs)
            then m.shared <- Tset.add e m.shared 
            else f_iter (walk m) xs e ;
            set_mark m e Marked
        | Marked ->
            ()
      end

  let mark m e = m.roots <- e :: m.roots ; walk m Vars.empty e

  type defs = {
    mutable stack : term list ;
    mutable defined : Tset.t ; 
  }

  let rec collect shared defs e =
    if not (Tset.mem e defs.defined) then
      begin
        e_iter (collect shared defs) e ;
        if Tset.mem e shared then 
          defs.stack <- e :: defs.stack ;
        defs.defined <- Tset.add e defs.defined ;
      end

  let marks
      ?(shared=fun _ -> false)
      ?(shareable=fun _ -> true)
      ?(closed=Vars.empty) 
      () = {
    closed = closed ;
    marked = shared ;
    shareable = shareable ;
    shared = Tset.empty ;
    mark = Tmap.empty ;
    roots = [] ;
  }

  let defs m =
    let defines = { stack=[] ; defined=Tset.empty } in
    List.iter (collect m.shared defines) m.roots ; 
    List.rev defines.stack

  let shared ?shared ?shareable ?closed es =
    let m = marks ?shared ?shareable ?closed () in
    List.iter (mark m) es ; 
    defs m

end
