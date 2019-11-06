(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

type var = Lang.F.var
type tau = Lang.F.tau
type field = Lang.field
type lfun = Lang.lfun
type term = Lang.F.term
type pred = Lang.F.pred

type repr =
  | True
  | False
  | And of term list
  | Or of term list
  | Not of term
  | Imply of term list * term
  | If of term * term * term
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
  | Cst of tau * term
  | Get of term * term
  | Set of term * term * term
  | HigherOrder

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
  | L.Acst(t,v) -> Cst(t,v)
  | L.Aget(a,k) -> Get(a,k)
  | L.Aset(a,k,v) -> Set(a,k,v)
  | L.Fvar x -> Var x
  | L.Bvar _ | L.Bind _ | L.Apply _
    -> HigherOrder

let pred p = term (Lang.F.e_prop p)

let lfun = Lang.name_of_lfun
let field = Lang.name_of_field

(* -------------------------------------------------------------------------- *)
