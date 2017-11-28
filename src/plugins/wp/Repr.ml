(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type env = Lang.F.pool
type var = Lang.F.var
type tau = Lang.F.tau
type field = Lang.field
type lfun = Lang.lfun
type term = Lang.F.term
type pred = Lang.F.pred

let env vars = Lang.new_pool ~vars ()

type repr =
  | True
  | False
  | And of term list
  | Or of term list
  | Not of term
  | Imply of term list * term
  | If of term * term * term
  | Forall of tau * (env -> var * term)
  | Exists of tau * (env -> var * term)
  | Var of var
  | Int of Z.t
  | Real of Q.t
  | Add of term list
  | Mul of term list
  | Div of term * term
  | Mod of term * term
  | Eq of term * term
  | Neq of term * term
  | Lt of term * term
  | Leq of term * term
  | Times of Z.t * term
  | Call of lfun * term list
  | Field of term * field
  | Record of (field * term) list
  | Get of term * term
  | Set of term * term * term
  | Abstract

let disclose tau bind env =
  let x = Lang.F.fresh env tau in x , Lang.F.QED.lc_open x bind

module L = Qed.Logic

let term e : repr =
  match Lang.F.repr e with
  | L.True -> True
  | L.False -> False
  | L.And ts -> And ts
  | L.Or ts -> Or ts
  | L.Not t -> Not t
  | L.If(a,b,c) -> If(a,b,c)
  | L.Imply(hs,p) -> Imply(hs,p)
  | L.Bind(L.Forall,t,q) -> Forall(t,disclose t q)
  | L.Bind(L.Exists,t,q) -> Exists(t,disclose t q)
  | L.Kint z -> Int z
  | L.Kreal r -> Real r
  | L.Add ts -> Add ts
  | L.Mul ts -> Mul ts
  | L.Div(a,b) -> Div(a,b)
  | L.Mod(a,b) -> Mod(a,b)
  | L.Eq(a,b) -> Eq(a,b)
  | L.Neq(a,b) -> Neq(a,b)
  | L.Lt(a,b) -> Lt(a,b)
  | L.Leq(a,b) -> Leq(a,b)
  | L.Times(k,t) -> Times(k,t)
  | L.Fun(f,ts) -> Call(f,ts)
  | L.Rget(r,f) -> Field(r,f)
  | L.Rdef fvs -> Record fvs
  | L.Aget(a,k) -> Get(a,k)
  | L.Aset(a,k,v) -> Set(a,k,v)
  | L.Fvar x -> Var x
  | L.Bvar _ | L.Bind(L.Lambda,_,_) | L.Apply _ -> Abstract

let pred p = term (Lang.F.e_prop p)

let lfun = Lang.name_of_lfun
let field = Lang.name_of_field

(* -------------------------------------------------------------------------- *)
