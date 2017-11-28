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

(** {2 Term & Predicate Introspection} *)

(** Environment for binder resolution, see Forall & Exists *)
type env

type tau = Lang.F.tau
type var = Lang.F.var
type field = Lang.field
type lfun = Lang.lfun
type term = Lang.F.term
type pred = Lang.F.pred

(** Create environment from a set of free variables *)
val env : Lang.F.Vars.t -> env

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

val term : term -> repr
val pred : pred -> repr

val lfun : lfun -> string
val field : field -> string

(* -------------------------------------------------------------------------- *)
