(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type pure_type =
  | PTint
  | PTbool
  | PTreal
  | PTunit
  | PTvar of string
  | PTexternal of pure_type list * string

type constant =
  | ConstInt of string
  | ConstBool of bool
  | ConstUnit
  | ConstFloat of string

type term =
  | Tconst of constant
  | Tvar of string
  | Tapp of string * term list
  | Tif of term * term * term

type predicate =
  | Pvar of string
  | Papp of string * term list
  | Ptrue
  | Pfalse
  | Pimplies of predicate * predicate
  | Pif of term * predicate * predicate
  | Pand of predicate * predicate
  | Por of predicate * predicate
  | Pxor of predicate * predicate
  | Piff of predicate * predicate
  | Pnot of predicate
  | Pforall of string * pure_type * predicate
  | Pexists of string * pure_type * predicate
  | Pnamed of string * predicate

type decl =
  | Function of string * pure_type list * pure_type
  | Predicate of string * pure_type list
  | Axiom of string * predicate
  | Goal of string * predicate
  | Type of pure_type

val pand :  predicate * predicate -> predicate
val pands: predicate list -> predicate -> predicate
val pimplies:predicate * predicate -> predicate
val papp: string * term list -> predicate
val por : predicate * predicate -> predicate
val pxor : predicate * predicate -> predicate
val pors : predicate list -> predicate -> predicate
val piff : predicate * predicate -> predicate
val pforall : string * pure_type * predicate -> predicate
val pif :  term * predicate * predicate -> predicate
val pnot : predicate -> predicate
val plet : string -> pure_type -> term -> predicate -> predicate
val fresh_name : string -> string
