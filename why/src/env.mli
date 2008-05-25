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

(*i $Id: env.mli,v 1.52 2008/04/10 14:43:57 filliatr Exp $ i*)

(*s Environment for imperative programs.
 
   Here we manage the global environment, which is imperative,
   and we provide a functional local environment. 
  
   The most important functions, [is_in_env], [type_in_env] and [fold_all]
   first look in the local environment then in the global one. *)

open Cc
open Logic
open Types
open Ast

(*s type schemes *)

module Vset : Set.S with type elt = type_var
module Vmap : Map.S with type key = type_var

type 'a scheme = private { scheme_vars : Vset.t; scheme_type : 'a }
val empty_scheme : 'a -> 'a scheme

(*s AST for typed programs are decorated with local environments *)

type local_env

type typing_info = { 
  t_loc : Loc.position;
  t_env : local_env;
  t_label : label;
  mutable t_userlabel : label;
  t_result_name : Ident.t;
  t_result_type : type_v;
  t_effect : Effect.t;
  t_post : postcondition option 
}
  
type typed_expr = typing_info Ast.t

(*s global environment *)

val add_global : Ident.t -> type_v -> typed_expr option -> unit
val add_global_gen : Ident.t -> type_v scheme -> typed_expr option -> unit
val is_global : Ident.t -> bool
val iter_global : (Ident.t * Types.type_v scheme -> unit) -> unit
val lookup_global : Ident.t -> type_v

(*s types (only global) *)

val add_type : Loc.position -> Ident.t list -> Ident.t -> unit
val is_type : Ident.t -> bool
val type_arity : Ident.t -> int

(*s exceptions (only global) *)

val add_exception : Ident.t -> pure_type option -> unit
val is_exception : Ident.t -> bool
val find_exception : Ident.t -> pure_type option

(*s logical elements *)

type var_subst = type_var Vmap.t

val add_global_logic : Ident.t -> logic_type scheme -> unit
val find_global_logic : Ident.t -> var_subst * logic_type

val iter_global_logic : (Ident.t -> logic_type scheme -> unit) -> unit
val is_global_logic : Ident.t -> bool
val is_logic_function : Ident.t -> bool

(*s a table keeps the program (for extraction) *)

val find_pgm : Ident.t -> typed_expr option

(*s local environments *)

val empty_progs : unit -> local_env
val empty_logic : unit -> local_env

val add : ?generalize:bool -> Ident.t -> type_v -> local_env -> local_env
val is_local : local_env -> Ident.t -> bool

val find_type_var : Ident.t -> local_env -> type_var

(*s access in env (local then global) *)

val type_in_env : local_env -> Ident.t -> type_v
val is_in_env : local_env -> Ident.t -> bool
val is_ref : local_env -> Ident.t -> bool

val fold_all : (Ident.t * type_v -> 'a -> 'a) -> local_env -> 'a -> 'a

val add_rec : Ident.t -> local_env -> local_env
val is_rec : Ident.t -> local_env -> bool

(*s logical elements *)

val add_logic : Ident.t -> pure_type -> local_env -> local_env
val find_logic : Ident.t -> local_env -> pure_type

val type_v_of_logic : pure_type list -> pure_type -> type_v

(* type variables and generalization/specialization *)

val new_type_var : ?user:bool -> unit -> type_var

val type_var_name : type_var -> string (* for error messages only *)

val generalize_logic_type : logic_type -> logic_type scheme
val generalize_type_v : type_v -> type_v scheme
val generalize_predicate : predicate -> predicate scheme
val generalize_predicate_def : predicate_def -> predicate_def scheme
val generalize_function_def : function_def -> function_def scheme
val generalize_sequent : sequent -> sequent scheme

val specialize_type_scheme : type_v scheme -> var_subst * type_v
val specialize_logic_type : logic_type scheme -> var_subst * logic_type
val specialize_predicate : predicate scheme -> var_subst * predicate
val specialize_predicate_def : 
  predicate_def scheme -> var_subst * predicate_def
val specialize_function_def : 
  function_def scheme -> var_subst * function_def
val specialize_sequent : sequent scheme -> var_subst * sequent

val subst_sequent : var_subst -> sequent -> sequent

val specialize_cc_type : Cc.cc_type -> var_subst * Cc.cc_type
val specialize_validation : 
  Cc.cc_type -> Cc.validation -> var_subst * Cc.cc_type * Cc.validation

val specialize_cc_functional_program : 
  Cc.cc_type -> Cc.cc_functional_program 
  -> var_subst * Cc.cc_type * Cc.cc_functional_program

(*s Labels *)

module Label : sig 
  type t 
  val empty : t
  val add : string -> t -> t
  val mem : string -> t -> bool
end

(* debug *)

val dump_type_var : (type_var -> unit) ref
