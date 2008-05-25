(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: ltyping.mli,v 1.28 2008/04/10 14:43:57 filliatr Exp $ i*)

(*s Typing on the logical side *)

open Logic
open Types
open Ptree
open Ast
open Env

val unify : Logic.pure_type -> Logic.pure_type -> bool

val pure_type : local_env -> ppure_type -> pure_type

val type_v : 
  Loc.position -> Label.t -> local_env -> ptype_v -> type_v

val type_c :
  Loc.position -> Label.t -> local_env -> ptype_c -> type_c

val logic_type : plogic_type -> logic_type

val predicate : Label.t -> local_env -> lexpr -> predicate

val term : Label.t -> local_env -> lexpr -> term * pure_type

val type_assert : 
  ?namer:(Ident.name -> Ident.t) ->
  Label.t -> local_env -> Ptree.assertion -> assertion

val type_post : 
  Label.t -> local_env -> Ident.t -> type_v -> Effect.t -> 
  Ptree.postcondition -> postcondition

val binders : 
  Loc.position -> Label.t -> local_env -> 
  ptype_v binder list -> 
  type_v binder list * local_env

val instance : Ident.t -> var_subst -> pure_type list

(* errors *)

val expected_num : Loc.position -> 'a

val expected_type : Loc.position -> type_v -> 'a

(* instances: the following table contains all closed instances of logical
   symbols that have been used so far. 
   It is later used to generate all monomorphic
   instances of symbols/predicates/axioms in the CVC Lite output. *)
(*
module Instances : Set.S with type elt = pure_type list

val add_instance_if_closed : Ident.t -> pure_type list -> unit
val instances : Ident.t -> Instances.t

val iter_instances : (Ident.t -> pure_type list -> unit) -> unit
*)
