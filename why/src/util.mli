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

(*i $Id: util.mli,v 1.72 2008/04/25 14:38:01 stoulsn Exp $ i*)

open Cc
open Logic
open Misc
open Types
open Ast
open Env

val erase_label : string -> predicate -> predicate
val change_label : string -> string -> predicate -> predicate
val put_label_term : local_env -> string -> term -> term
val put_label_predicate : local_env -> string -> predicate -> predicate

val traverse_binders : local_env -> (type_v binder) list -> local_env
val initial_renaming : local_env -> Rename.t

val apply_term : 
  Rename.t -> local_env -> term -> term
val apply_assert : 
  Rename.t -> local_env -> Types.assertion -> Types.assertion
val a_apply_assert : 
  Rename.t -> local_env -> assertion -> assertion
val apply_post : 
  label -> Rename.t -> local_env -> Types.postcondition -> Types.postcondition
val a_apply_post : 
  label -> Rename.t -> local_env -> postcondition -> postcondition

val oldify : local_env -> Effect.t -> term -> term
val type_c_subst_oldify : local_env -> Ident.t -> term -> type_c -> type_c

val is_reference : local_env -> Ident.t -> bool
val predicate_now_refs : local_env -> predicate -> Ident.set
val predicate_refs : local_env -> predicate -> Ident.set
val term_now_refs : local_env -> term -> Ident.set
val term_refs : local_env -> term -> Ident.set
val post_refs : local_env -> postcondition -> Ident.set

val deref_type : type_v -> pure_type
val dearray_type : type_v -> pure_type

val decomp_type_c : type_c -> 
  (Ident.t * type_v) * Effect.t * 
  Types.precondition list * Types.postcondition option
val decomp_kappa : typing_info -> 
  (Ident.t * type_v) * Effect.t * Ast.postcondition option

val equality : term -> term -> predicate
val tequality : type_v -> term -> term -> predicate

val distinct : term list -> predicate


val  split_one : Logic.predicate list ->
  int -> Logic.predicate -> Logic.predicate list
val  split : context_element list ->
  (unit -> Ident.t) -> context_element list -> context_element list
val  intros : context_element list ->
  predicate -> (unit -> Ident.t) -> bool -> context_element list * Logic.predicate
  
val decomp_boolean : postcondition -> predicate * predicate

val effect : typed_expr -> Effect.t
val post : typed_expr -> postcondition option
val result_type : typed_expr -> type_v
val result_name : typing_info -> Ident.t

val erase_exns : typing_info -> typing_info

val forall : 
  ?is_wp:is_wp -> Ident.t -> type_v -> ?triggers:triggers 
  -> predicate -> predicate
val foralls : ?is_wp:is_wp -> (Ident.t * type_v) list -> predicate -> predicate
val exists : Ident.t -> type_v -> predicate -> predicate

(* more efficient version when there are many variables *)
val foralls_many : 
  ?is_wp:is_wp -> (Ident.t * type_v) list -> predicate -> predicate


(* versions performing simplifcations *)
val pforall : ?is_wp:is_wp -> Ident.t -> type_v -> predicate -> predicate
val pexists : Ident.t -> type_v -> predicate -> predicate

(* decomposing universal quantifiers, renaming variables on the fly *)

val decomp_forall : 
  ?ids:Ident.set -> predicate -> (Ident.t * pure_type) list * predicate

(*s Occurrences *)

val occur_term : Ident.t -> term -> bool
val occur_predicate : Ident.t -> predicate -> bool
val occur_assertion : Ident.t -> assertion -> bool
val occur_post : Ident.t -> postcondition option -> bool
val occur_type_v : Ident.t -> type_v -> bool
val occur_type_c : Ident.t -> type_c -> bool

(*s Functions to translate array operations *)

val array_info : 
  local_env -> Ident.t -> pure_type

val make_raw_access :
  local_env -> Ident.t * Ident.t -> term -> term

val make_raw_store :
  local_env -> Ident.t * Ident.t -> term -> term -> term

val make_pre_access :
  local_env -> Ident.t -> term -> predicate

(*s AST builders for program transformation *)

val make_lnode : 
  Loc.position -> typing_info Ast.t_desc ->
  local_env -> type_c -> typed_expr
val make_var : 
  Loc.position -> Ident.t -> type_v -> local_env -> typed_expr
val make_expression :
  Loc.position -> term -> type_v -> local_env -> typed_expr
val make_annot_bool :
  Loc.position -> bool -> local_env -> typed_expr
val make_void :
  Loc.position -> local_env -> typed_expr
val make_raise :
  Loc.position -> Ident.t -> type_v -> local_env -> typed_expr

val change_desc : 'a Ast.t -> 'a Ast.t_desc -> 'a Ast.t

val force_post :
  local_env -> postcondition option -> typed_expr -> typed_expr

val create_postval : predicate -> assertion option

val create_post : predicate -> (assertion * 'b list) option

(* explanations *)

val loc_of_label: string -> Loc.floc

val cook_explanation : 
    string option -> raw_vc_explain -> Logic_decl.expl_kind * Loc.floc 

val program_locs : (string,(string * string * Loc.floc)) Hashtbl.t

val explanation_table : (int,Cc.raw_vc_explain) Hashtbl.t

val reg_explanation : Cc.raw_vc_explain -> Logic.term_label

(*s Pretty printers. *)

open Format

val print_pred_binders : bool ref

val print_pure_type : formatter -> pure_type -> unit
val print_logic_type : formatter -> logic_type -> unit

val print_term : formatter -> term -> unit
val print_predicate : formatter -> predicate -> unit
(*
val raw_loc : ?pref:string -> formatter -> Loc.floc -> unit
val print_explanation : formatter -> ((*Loc.floc * *) Logic_decl.vc_expl) -> unit
*)
val print_assertion : formatter -> assertion -> unit
val print_wp : formatter -> assertion option -> unit

val print_type_v : formatter -> type_v -> unit
val print_type_c : formatter -> type_c -> unit
val print_typing_info : formatter -> typing_info -> unit
val print_expr : formatter -> typed_expr -> unit

val print_cc_type : formatter -> Cc.cc_type -> unit
val print_cc_term : formatter -> ('a * predicate) Cc.cc_term -> unit
val print_cc_pattern : formatter -> Cc.cc_pattern -> unit

val print_subst : formatter -> substitution -> unit
val print_cc_subst : formatter -> ('a * predicate) Cc.cc_term Ident.map -> unit

val print_env : formatter -> local_env -> unit

val print_ptree : formatter -> Ptree.parsed_program -> unit
val print_pfile : formatter -> Ptree.decl list -> unit

val print_decl : formatter -> Logic_decl.t -> unit
