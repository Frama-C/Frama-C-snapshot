(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

open Cil_types
open Ctypes
open Qed
open Qed.Logic

(** Logic Language based on Qed *)

(** {2 Naming} Unique identifiers. *)

val comp_id  : compinfo -> string
val field_id : fieldinfo -> string
val type_id  : logic_type_info -> string
val logic_id : logic_info -> string
val lemma_id : string -> string

(** {2 Theory} *)

type theory = string

(** {2 Symbols} *)

type adt = private
  | Mtype of mdt (** External type *)
  | Mrecord of mdt * fields (** External record-type *)
  | Atype of logic_type_info (** Logic Type *)
  | Comp of compinfo (** C-code struct or union *)
and mdt = {
  mdt_link : string ;
  mdt_theory : theory ;
}
and fields = { mutable fields : field list }
and field =
  | Mfield of mdt * fields * string * tau
  | Cfield of fieldinfo
and tau = (field,adt) Logic.datatype

type scope = External of theory | Generated
type lfun =
  | Function of lfunction 
  | Predicate of lpredicate
  | ACSL of logic_info
  | CTOR of logic_ctor_info
      
and lfunction = {
  f_scope : scope ;
  f_link : Engine.link ;
  f_category : lfun category ;
  f_params : sort list ;
  f_result : sort ;
}

and lpredicate = {
  p_scope : scope ;
  p_params : sort list ;
  p_prop : string ;
  p_bool : string ;
}

val builtin : name:string -> link:string -> theory:string -> unit
val datatype : link:string -> theory:string -> adt
val record : link:string -> theory:string -> (string * tau) list -> adt
val atype : logic_type_info -> adt
val comp : compinfo -> adt
val field : adt -> string -> field
val fields_of_tau : tau -> field list
val fields_of_field : field -> field list

type balance = Nary | Left | Right

val extern_s : 
  theory:theory -> ?balance:balance -> 
  ?category:lfun category -> ?params:sort list -> ?result:sort ->
  string -> lfun

val extern_f : 
  theory:theory -> ?balance:balance -> 
  ?category:lfun category -> ?params:sort list -> ?result:sort ->
  ('a,Format.formatter,unit,lfun) format4 -> 'a

val extern_p : 
  theory:theory -> prop:string -> bool:string -> 
  ?params:sort list -> unit -> lfun

val extern_fp : theory:theory -> ?params:sort list -> string -> lfun

val generated_f : ?category:lfun category -> 
  ?params:sort list -> ?result:sort -> 
  ('a,Format.formatter,unit,lfun) format4 -> 'a

val generated_p : string -> lfun

val link : Engine.cmode -> lfun -> Engine.link
val theory : lfun -> string

(** {2 Sorting and Typing} *)

val tau_of_comp : compinfo -> tau
val tau_of_object : c_object -> tau
val tau_of_ctype : typ -> tau
val tau_of_ltype : logic_type -> tau
val tau_of_return : logic_info -> tau
val tau_of_lfun : lfun -> tau
val tau_of_field : field -> tau
val tau_of_record : field -> tau

val array : tau -> tau
val farray : tau -> tau -> tau

val pointer : (typ -> tau) Context.value (** type of pointers *)
val poly : string list Context.value (** polymorphism *)

(** {2 Logic Formulae} *)

module ADT : Logic.Data with type t = adt
module Field : Logic.Field with type t = field
module Fun : Logic.Function with type t = lfun

module F :
sig

  (** {3 Expressions} *)
 
  include Logic.Term with module ADT = ADT
		     and module Field = Field
		     and module Fun = Fun

  type unop = term -> term
  type binop = term -> term -> term

  val e_zero : term
  val e_one : term
  val e_minus_one : term
  val e_zero_real : term

  val e_int64 : int64 -> term
  val e_fact : int64 -> term -> term
  val e_bigint : Integer.t -> term
  val e_mthfloat : float -> term
  val e_hexfloat : float -> term
  val e_setfield : term -> field -> term -> term
  val e_range : term -> term -> term (** e_range a b = b+1-a *)
  val is_zero : term -> bool

  (** {3 Predicates} *)

  type pred
  type cmp = term -> term -> pred

  val p_true : pred
  val p_false : pred

  val p_equal : term -> term -> pred
  val p_neq : term -> term -> pred
  val p_leq : term -> term -> pred
  val p_lt : term -> term -> pred
  val p_positive : term -> pred

  val is_ptrue : pred -> Logic.maybe
  val is_pfalse : pred -> Logic.maybe
  val is_equal : term -> term -> Logic.maybe
  val eqp : pred -> pred -> bool
  val comparep : pred -> pred -> int

  val p_bool : term -> pred
  val e_prop : pred -> term
  val p_bools : term list -> pred list
  val e_props : pred list -> term list
  val lift : (term -> term) -> pred -> pred

  val p_not : pred -> pred
  val p_and : pred -> pred -> pred
  val p_or  : pred -> pred -> pred
  val p_imply : pred -> pred -> pred
  val p_equiv : pred -> pred -> pred
  val p_hyps : pred list -> pred -> pred
  val p_if : pred -> pred -> pred -> pred

  val p_conj : pred list -> pred
  val p_disj : pred list -> pred
    
  val p_any : ('a -> pred) -> 'a list -> pred
  val p_all : ('a -> pred) -> 'a list -> pred

  val p_call : lfun -> term list -> pred

  val p_forall : var list -> pred -> pred
  val p_exists : var list -> pred -> pred
  val p_bind : binder -> var -> pred -> pred

  val p_subst : ?pool:pool -> var -> term -> pred -> pred

  val p_close : pred -> pred

  val idp : pred -> int
  val varsp : pred -> Vars.t
  val occurs : var -> term -> bool
  val occursp : var -> pred -> bool
  val intersect : term -> term -> bool
  val intersectp : pred -> pred -> bool

  val pp_var : Format.formatter -> var -> unit
  val pp_vars : Format.formatter -> Vars.t -> unit
  val pp_term : Format.formatter -> term -> unit
  val pp_pred : Format.formatter -> pred -> unit
  val debugp : Format.formatter -> pred -> unit

  type env
  val empty : env
  val closed : Vars.t -> env
  val marker : env -> marks
  val mark_e : marks -> term -> unit
  val mark_p : marks -> pred -> unit
  val define : (env -> string -> term -> unit) -> env -> marks -> env
  val pp_eterm : env -> Format.formatter -> term -> unit
  val pp_epred : env -> Format.formatter -> pred -> unit

  val pred : pred -> (field,lfun,var,pred) Logic.term_repr

  module Pmap : Qed.Idxmap.S with type key = pred
  module Pset : Qed.Idxset.S with type elt = pred

  type pattern = Fun.t Qed.Pattern.fpattern
  val rewrite : name:string -> vars:tau array -> pattern -> (term array -> term) -> unit

  val add_builtin_1 : lfun -> (term -> term) -> unit
  val add_builtin_2 : lfun -> (term -> term -> term) -> unit
  val add_builtin_peq : lfun -> (term -> term -> pred) -> unit

end

(** {2 Fresh Variables and Constraints} *)

open F
  
type gamma
val new_pool : ?copy:pool -> unit -> pool
val new_gamma : ?copy:gamma -> unit -> gamma

val local : ?pool:pool -> ?gamma:gamma -> ('a -> 'b) -> 'a -> 'b 

val freshvar : ?basename:string -> tau -> var
val freshen : var -> var
val assume : pred -> unit  
val without_assume : ('a -> 'b) -> 'a -> 'b
val epsilon : ?basename:string -> tau -> (term -> pred) -> term
val hypotheses : gamma -> pred list
val variables : gamma -> var list

val get_pool : unit -> pool
val get_gamma : unit -> gamma
val get_hypotheses : unit -> pred list
val get_variables : unit -> var list

(** {2 Alpha Conversion} *)

module Alpha :
sig

  type t
  val create : unit -> t
  val get : t -> var -> var
  val iter : (var -> var -> unit) -> t -> unit
    
  val convert : t -> term -> term
  val convertp : t -> pred -> pred

end

(* -------------------------------------------------------------------------- *)
