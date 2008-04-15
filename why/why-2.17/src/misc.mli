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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: misc.mli,v 1.91 2008/11/05 14:03:17 filliatr Exp $ i*)

(* Some misc. functions *)

open Logic
open Types
open Ptree
open Ast
open Cc


val is_mutable : type_v -> bool
val is_pure : type_v -> bool
val is_closed_pure_type : pure_type -> bool
val normalize_pure_type : pure_type -> pure_type

val is_default_post : assertion -> bool

(* Substitution within assertions and pre/post-conditions *)
val asst_app : (predicate -> predicate) -> assertion -> assertion
val optasst_app : 
  (predicate -> predicate) -> assertion option -> assertion option
val post_app : ('a -> 'b) -> 'a * ('c * 'a) list -> 'b * ('c * 'b) list
val optpost_app :
  ('a -> 'b) -> ('a * ('c * 'a) list) option -> ('b * ('c * 'b) list) option
val optpost_fold :
  (predicate -> 'a -> 'a) ->
  (assertion * ('b * assertion) list) option -> 'a -> 'a
(***
val post_app : (predicate -> predicate) -> postcondition -> postcondition
val optpost_app : 
  (predicate -> predicate) -> postcondition option -> postcondition option
val optpost_fold : (predicate -> 'a -> 'a) -> postcondition option -> 'a -> 'a
***)

val asst_fold : (predicate -> 'a -> 'a) -> assertion -> 'a -> 'a

(* Substitution within some parts of postconditions (value or exns) *)
val val_app : (predicate -> predicate) -> postcondition -> postcondition
val exn_app : Ident.t -> 
              (predicate -> predicate) -> postcondition -> postcondition
val optval_app : 
  (predicate -> predicate) -> postcondition option -> postcondition option
val optexn_app : 
  Ident.t -> 
  (predicate -> predicate) -> postcondition option -> postcondition option

val a_value : assertion -> predicate
val a_values : assertion list -> predicate list

val anonymous : Loc.position -> predicate -> assertion
val pre_named  : Loc.position -> predicate -> assertion
val post_named  : Loc.position -> predicate -> assertion
val wp_named : Loc.position -> predicate -> assertion

(*s Default exceptional postcondition (inserted when not given) *)
val default_post : assertion

(***
val force_post_name : postcondition option -> postcondition option
val force_bool_name : postcondition option -> postcondition option
***)
val force_wp_name : assertion option -> assertion option

val force_loc : Loc.position -> assertion -> assertion
val force_post_loc : Loc.position -> postcondition -> postcondition
(***
val force_type_c_loc : Loc.position -> type_c -> type_c
***)

val post_val : 'a * 'b -> 'a
val post_exn : Ident.t -> 'a * (Ident.t * 'b) list -> 'b
val optpost_val : postcondition option -> assertion option
val optpost_exn : Ident.t -> postcondition option -> assertion option

val map_succeed : ('a -> 'b) -> 'a list -> 'b list

val option_app : ('a -> 'b) -> 'a option -> 'b option
val option_iter : ('a -> unit) -> 'a option -> unit
val option_fold : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b

val list_of_some : 'a option -> 'a list
val difference : 'a list -> 'a list -> 'a list
val last : 'a list -> 'a

val list_combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list

val list_first : ('a -> 'b) -> 'a list -> 'b

val if_labelled : (Ident.t * string -> unit) -> Ident.t -> unit

type avoid = Ident.set
val renaming_of_ids : avoid -> Ident.t list -> (Ident.t * Ident.t) list * avoid

val pre_name    : Ident.name -> Ident.t
val post_name   : unit -> Ident.t
val inv_name    : Ident.name -> Ident.t
val test_name   : unit -> Ident.t
val wp_name     : unit -> Ident.t
val h_name      : Ident.name -> Ident.t

val bool_name   : unit -> Ident.t
val variant_name : unit -> Ident.t
val phi_name    : unit -> Ident.t
val for_name    : unit -> Ident.t
val label_name  : unit -> string
val wf_name     : unit -> Ident.t
val fresh_var   : unit -> Ident.t
val fresh_c_var : unit -> Ident.t
val fresh_hyp   : unit -> Ident.t
val fresh_axiom : unit -> Ident.t

val post_name_from : Ident.name -> Ident.t

val reset_names : unit -> unit

val id_of_name : Ident.name -> Ident.t

val rationalize : string -> string * string

(*s Functions over terms and predicates. *)

val applist : term -> term list -> term
val papplist : predicate -> term list -> predicate

val predicate_of_term : term -> predicate

val term_vars : term -> Ident.set
val predicate_vars : predicate -> Ident.set
val post_vars : Types.postcondition -> Ident.set
val apost_vars : postcondition -> Ident.set

val subst_in_term : var_substitution -> term -> term
val tsubst_in_term : substitution -> term -> term

val subst_in_assertion : var_substitution -> assertion -> assertion

val subst_in_predicate : var_substitution -> predicate -> predicate
val tsubst_in_predicate : substitution -> predicate -> predicate

val subst_in_pattern : var_substitution -> pattern -> pattern
val tsubst_in_pattern : substitution -> pattern -> pattern

val subst_in_triggers : var_substitution -> triggers -> triggers

val subst_one : Ident.t -> term -> substitution
val subst_onev : Ident.t -> Ident.t -> var_substitution
val subst_many : Ident.t list -> term list -> substitution
val subst_manyv : Ident.t list -> Ident.t list -> var_substitution

val map_predicate : (predicate -> predicate) -> predicate -> predicate

val type_v_subst : var_substitution -> type_v -> type_v
val type_c_subst : var_substitution -> type_c -> type_c

val type_v_rsubst : substitution -> type_v -> type_v
val type_c_rsubst : substitution -> type_c -> type_c

val type_c_of_v : type_v -> type_c
val make_arrow : type_v binder list -> type_c -> type_v

val make_binders_type_v : type_v -> type_v
val make_binders_type_c : type_c -> type_c

val unref_term : term -> term

val equals_true : term -> term
val equals_false : term -> term

val mlize_type : type_v -> pure_type

(*s Smart constructors for terms and predicates. *)

val ttrue : term
val tfalse : term
val tresult : term
val tvoid : term

val relation : Ident.t -> term -> term -> predicate
val not_relation : Ident.t -> term -> term -> predicate
val make_int_relation : Ident.t -> Ident.t

val lt : term -> term -> predicate
val le : term -> term -> predicate
val lt_int : term -> term -> predicate
val le_int : term -> term -> predicate
val gt : term -> term -> predicate
val ge : term -> term -> predicate
val ge_real : term -> term -> predicate
val eq : term -> term -> predicate
val neq : term -> term -> predicate

val array_length : Ident.t -> pure_type -> term

val pif : term -> predicate -> predicate -> predicate
val pand : ?is_wp:is_wp -> ?is_sym:bool -> predicate -> predicate -> predicate
val pands : ?is_wp:is_wp -> ?is_sym:bool -> predicate list -> predicate
val por : predicate -> predicate -> predicate
val pors : predicate list -> predicate
val pnot : predicate -> predicate
val pimplies : ?is_wp:is_wp -> predicate -> predicate -> predicate

(* WP connectives *)
val wpand : ?is_sym:bool -> predicate -> predicate -> predicate
val wpands : ?is_sym:bool -> predicate list -> predicate
val wpimplies : predicate -> predicate -> predicate

val simplify : predicate -> predicate

(*s Equalities *)

val eq_pure_type : pure_type -> pure_type -> bool
val eq_term : term -> term -> bool
val eq_predicate : predicate -> predicate -> bool (* no alpha-equivalence *)

(*s Debug functions *)

module Size : sig
  val predicate : predicate -> int
  val term : term -> int
  val postcondition : postcondition -> int
  val postcondition_opt : postcondition option -> int
end 

(*s functions over CC terms *)

val cc_var : Ident.t -> 'a cc_term
val cc_term : term -> 'a cc_term
val cc_applist : 'a cc_term -> 'a cc_term list -> 'a cc_term
val cc_lam : cc_binder list -> 'a cc_term -> 'a cc_term

val tt_var : Ident.t -> cc_type
val tt_arrow : cc_binder list -> cc_type -> cc_type

(*s Pretty-print *)

open Format

val warning : string -> unit
val wprintf : Loc.position -> ('a, Format.formatter, unit) format -> 'a
val unlocated_wprintf : ('a, Format.formatter, unit) format -> 'a

(* [do_not_edit f before sep after] updates the part of file [f] following a
   line matching [sep] exactly (e.g. \verb!(* DO NOT EDIT BELOW THIS LINE *)!);
   a backup of the original file is done in [f.bak].
   when [f] does not exists, it is created by calling [before], inserting
   [sep] and then calling [after]. *)

val do_not_edit_below : 
  file:string -> 
  before:(formatter -> unit) -> 
  sep:string -> 
  after:(formatter -> unit) -> unit

val do_not_edit_above : 
  file:string -> 
  before:(formatter -> unit) -> 
  sep:string -> 
  after:(formatter -> unit) -> unit

val file_formatter : (Format.formatter -> unit) -> (out_channel -> unit)

