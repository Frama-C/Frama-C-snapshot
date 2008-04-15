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

type var = string

type 'd d_term =
  | Tconst of constant
  | Tdata of 'd
  | Tapp of string * 'd d_term list
  | Tif of 'd d_term * 'd d_term * 'd d_term

type term = var d_term

type 't t_pred =
  | Pvar of string
  | Papp of string * 't list
  | Ptrue
  | Pfalse
  | Pimplies of 't t_pred * 't t_pred
  | Pif of 't * 't t_pred * 't t_pred
  | Pand of 't t_pred * 't t_pred
  | Por of 't t_pred * 't t_pred
  | Pxor of 't t_pred * 't t_pred
  | Piff of 't t_pred * 't t_pred
  | Pnot of 't t_pred
  | Pforall of string * pure_type * 't t_pred
  | Pexists of string * pure_type * 't t_pred
  | Pnamed of string * 't t_pred

type 'd d_pred = ('d d_term) t_pred
type predicate = term t_pred

type 't gen_decl =
  | Function of string * pure_type list * pure_type
  | Predicate of string * pure_type list
  | Axiom of string * 't t_pred
  | Goal of string * 't t_pred
  | Type of pure_type

type decl = term gen_decl

val pand :  't t_pred * 't t_pred -> 't t_pred
val pands: 't t_pred list -> 't t_pred -> 't t_pred
val pimplies:'t t_pred * 't t_pred -> 't t_pred
val papp: string * 't list -> 't t_pred
val por : 't t_pred * 't t_pred -> 't t_pred
val pxor : 't t_pred * 't t_pred -> 't t_pred
val pors : 't t_pred list -> 't t_pred -> 't t_pred
val piff : 't t_pred * 't t_pred -> 't t_pred
val pnot : 't t_pred -> 't t_pred

val plet : string -> pure_type -> term -> predicate -> predicate
val pforall : string * pure_type * predicate -> predicate
val pif :  'd d_term * 'd d_pred * 'd d_pred -> 'd d_pred

val fresh_name : string -> string

(** This function gives a predicate with some modification on the terms only.
* The first parameter is a function that defines the term modification.
* The second one can modify the fist function when a variable is quantified.
*)
val change_exp_in_pred : ('t -> 't) -> (('t -> 't) -> string -> ('t -> 't)) ->
  't t_pred -> 't t_pred

val change_data_in_exp : ('d -> 'e d_term) -> 'd d_term -> 'e d_term

(** similar to [change_exp_in_pred] but only change data *)
val change_data_in_pred : ('d -> 'd d_term) -> 
  (('d -> 'd d_term) -> string -> ('d -> 'd d_term)) ->
  'd d_pred -> 'd d_pred

val translate_data_in_pred : ('d -> 'e d_term) -> 
  'd d_pred -> 'e d_pred

val subst_in_predicate : string -> term -> predicate -> predicate
val subst_vars_in_predicate : (string -> term option) -> predicate -> predicate

