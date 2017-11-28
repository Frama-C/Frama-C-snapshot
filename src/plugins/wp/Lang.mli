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

open Cil_types
open Ctypes
open Qed
open Qed.Logic

(** Logic Language based on Qed *)

(** {2 Library} *)

type library = string


type 'a infoprover = {
  altergo: 'a;
  why3   : 'a;
  coq    : 'a;
}
(** generic way to have different informations for the provers *)

val infoprover: 'a -> 'a infoprover
(** same information for all the provers *)
(** {2 Naming} Unique identifiers. *)

val comp_id  : compinfo -> string
val field_id : fieldinfo -> string
val type_id  : logic_type_info -> string
val logic_id : logic_info -> string
val lemma_id : string -> string

(** {2 Symbols} *)

type adt = private (** A type is never registered in a Definition.t *)
  | Mtype of mdt (** External type *)
  | Mrecord of mdt * fields (** External record-type *)
  | Atype of logic_type_info (** Logic Type *)
  | Comp of compinfo (** C-code struct or union *)
and mdt = string extern (** name to print to the provers *)
and 'a extern = {
  ext_id     : int;
  ext_link   : 'a infoprover;
  ext_library : library; (** a library which it depends on *)
  ext_debug  : string; (** just for printing during debugging *)
}
and fields = { mutable fields : field list }
and field =
  | Mfield of mdt * fields * string * tau
  | Cfield of fieldinfo
and tau = (field,adt) Logic.datatype


type lfun =
  | ACSL of Cil_types.logic_info (** Registered in Definition.t,
                                     only  *)
  | CTOR of Cil_types.logic_ctor_info (** Not registered in Definition.t
                                          directly converted/printed *)
  | Model of model (** *)

and model = {
  m_category : lfun category ;
  m_params : sort list ;
  m_result : sort ;
  m_typeof : tau option list -> tau ;
  m_source : source ;
}

and source =
  | Generated of string
  | Extern of Engine.link extern

val builtin_type : name:string -> link:string infoprover -> library:string -> adt
val is_builtin_type : name:string -> tau -> bool
val datatype : library:string -> string -> adt
val record :
  link:string infoprover -> library:string -> (string * tau) list -> adt
val atype : logic_type_info -> adt
val comp : compinfo -> adt
val field : adt -> string -> field
val fields_of_adt : adt -> field list
val fields_of_tau : tau -> field list
val fields_of_field : field -> field list

type balance = Nary | Left | Right

val extern_s :
  library:library ->
  ?link:(Engine.link infoprover) ->
  ?category:lfun category ->
  ?params:sort list ->
  ?sort:sort ->
  ?result:tau ->
  ?typecheck:(tau option list -> tau) ->
  string -> lfun

val extern_f :
  library:library ->
  ?link:(Engine.link infoprover) ->
  ?balance:balance ->
  ?category:lfun category ->
  ?params:sort list ->
  ?sort:sort ->
  ?result:tau ->
  ?typecheck:(tau option list -> tau) ->
  ('a,Format.formatter,unit,lfun) format4 -> 'a
(** balance just give a default when link is not specified *)

val extern_p :
  library:library ->
  ?bool:string ->
  ?prop:string ->
  ?link:Engine.link infoprover ->
  ?params:sort list ->
  unit -> lfun

val extern_fp : library:library -> ?params:sort list ->
  ?link:string infoprover -> string -> lfun

val generated_f : ?category:lfun category ->
  ?params:sort list -> ?sort:sort -> ?result:tau ->
  ('a,Format.formatter,unit,lfun) format4 -> 'a

val generated_p : string -> lfun


(** {2 Sorting and Typing} *)

val tau_of_comp : compinfo -> tau
val tau_of_object : c_object -> tau
val tau_of_ctype : typ -> tau
val tau_of_ltype : logic_type -> tau
val tau_of_return : logic_info -> tau
val tau_of_lfun : lfun -> tau option list -> tau
val tau_of_field : field -> tau
val tau_of_record : field -> tau

val array : tau -> tau
val farray : tau -> tau -> tau

val pointer : (typ -> tau) Context.value (** type of pointers *)
val poly : string list Context.value (** polymorphism *)
val parameters : (lfun -> sort list) -> unit (** definitions *)

val name_of_lfun : lfun -> string
val name_of_field : field -> string

(** {2 Logic Formulae} *)

module ADT : Logic.Data with type t = adt
module Field : Logic.Field with type t = field
module Fun : Logic.Function with type t = lfun

class virtual idprinting :
  object
    method virtual basename : string -> string
    (** Allows to sanitize the basename used for generated or ACSL
        name (not the one provided by the driver. *)
    method virtual infoprover : 'a. 'a infoprover -> 'a
    (** Specify the field to use in an infoprover *)

    method datatypename : string -> string
    method fieldname    : string -> string
    method funname      : string -> string

    method datatype : ADT.t   -> string
    method field    : Field.t -> string
    method link     : Fun.t   -> Engine.link
  end

module F :
sig

  module QED : Logic.Term with module ADT = ADT
                           and module Field = Field
                           and module Fun = Fun

  (** {3 Types and Variables} *)

  type var = QED.var
  type tau = QED.tau
  type pool = QED.pool
  module Tau = QED.Tau
  module Var = QED.Var
  module Vars : Qed.Idxset.S with type elt = var
  module Vmap : Qed.Idxmap.S with type key = var

  val pool : ?copy:pool -> unit -> pool
  val fresh : pool -> ?basename:string -> tau -> var
  val add_var : pool -> var -> unit
  val add_vars : pool -> Vars.t -> unit

  val tau_of_var : var -> tau

  (** {3 Expressions} *)

  type term = QED.term
  type record = (field * term) list

  val hash : term -> int (** Constant time *)
  val equal : term -> term -> bool (** Same as [==] *)
  val compare : term -> term -> int

  module Tset : Qed.Idxset.S with type elt = term
  module Tmap : Qed.Idxmap.S with type key = term

  type unop = term -> term
  type binop = term -> term -> term

  val e_zero : term
  val e_one : term
  val e_minus_one : term
  val e_one_real : term
  val e_zero_real : term

  val constant : term -> term

  val e_fact : int -> term -> term

  val e_int64 : int64 -> term
  val e_bigint : Integer.t -> term
  val e_float : float -> term
  val e_setfield : term -> field -> term -> term
  val e_range : term -> term -> term (** e_range a b = b+1-a *)
  val is_zero : term -> bool

  val e_true : term
  val e_false : term
  val e_bool : bool -> term
  val e_literal : bool -> term -> term
  val e_int : int -> term
  val e_zint : Z.t -> term
  val e_real : Q.t -> term
  val e_var : var -> term
  val e_opp : term -> term
  val e_times : Z.t -> term -> term
  val e_sum : term list -> term
  val e_prod : term list -> term
  val e_add : term -> term -> term
  val e_sub : term -> term -> term
  val e_mul : term -> term -> term
  val e_div : term -> term -> term
  val e_mod : term -> term -> term
  val e_eq  : term -> term -> term
  val e_neq : term -> term -> term
  val e_leq : term -> term -> term
  val e_lt  : term -> term -> term
  val e_imply : term list -> term -> term
  val e_equiv : term -> term -> term
  val e_and   : term list -> term
  val e_or    : term list -> term
  val e_not   : term -> term
  val e_if    : term -> term -> term -> term
  val e_get   : term -> term -> term
  val e_set   : term -> term -> term -> term
  val e_getfield : term -> Field.t -> term
  val e_record : record -> term
  val e_fun : Fun.t -> term list -> term
  val e_bind : binder -> var -> term -> term

  (** {3 Predicates} *)

  type pred
  type cmp = term -> term -> pred
  type operator = pred -> pred -> pred
  module Pmap : Qed.Idxmap.S with type key = pred
  module Pset : Qed.Idxset.S with type elt = pred

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

  type sigma = QED.sigma
  val sigma : unit -> sigma
  val e_subst : ?sigma:sigma -> (term -> term) -> term -> term
  val p_subst : ?sigma:sigma -> (term -> term) -> pred -> pred
  val p_apply : var -> term -> pred -> pred

  val e_vars : term -> var list (** Sorted *)
  val p_vars : pred -> var list (** Sorted *)

  val p_close : pred -> pred (** Quantify over (sorted) free variables *)

  val pp_tau : Format.formatter -> tau -> unit
  val pp_var : Format.formatter -> var -> unit
  val pp_vars : Format.formatter -> Vars.t -> unit
  val pp_term : Format.formatter -> term -> unit
  val pp_pred : Format.formatter -> pred -> unit
  val debugp : Format.formatter -> pred -> unit

  type env
  type marks = QED.marks

  val env : Vars.t -> env
  val marker : env -> marks
  val mark_e : marks -> term -> unit
  val mark_p : marks -> pred -> unit
  (** Returns a list of terms to be shared among all {i shared} or {i
      marked} subterms.  The order of terms is consistent with
      definition order: head terms might be used in tail ones. *)
  val defs : marks -> term list
  val define : (env -> string -> term -> unit) -> env -> marks -> env
  val pp_eterm : env -> Format.formatter -> term -> unit
  val pp_epred : env -> Format.formatter -> pred -> unit

  val p_expr : pred -> pred QED.expression
  val e_expr : pred -> term QED.expression
  val p_iter : (pred -> unit) -> (term -> unit) -> pred -> unit

  (** {3 Binders} *)

  val lc_closed : term -> bool
  val lc_iter : (term -> unit) -> term -> unit
  val lc_map : (term -> term) -> term -> term

  (** {3 Utilities} *)

  val decide   : term -> bool (** Return [true] if and only the term is [e_true]. Constant time. *)
  val basename : term -> string
  val is_true  : term -> maybe (** Constant time. *)
  val is_false : term -> maybe (** Constant time. *)
  val is_prop  : term -> bool (** Boolean or Property *)
  val is_int   : term -> bool (** Integer sort *)
  val is_real  : term -> bool (** Real sort *)
  val is_arith : term -> bool (** Integer or Real sort *)

  val is_closed : term -> bool (** No bound variables *)
  val is_simple : term -> bool (** Constants, variables, functions of arity 0 *)
  val is_atomic : term -> bool (** Constants and variables *)
  val is_primitive : term -> bool (** Constants only *)
  val is_neutral : Fun.t -> term -> bool
  val is_absorbant : Fun.t -> term -> bool
  val record_with : record -> (term * record) option

  val are_equal : term -> term -> maybe (** Computes equality *)
  val eval_eq   : term -> term -> bool  (** Same as [are_equal] is [Yes] *)
  val eval_neq  : term -> term -> bool  (** Same as [are_equal] is [No]  *)
  val eval_lt   : term -> term -> bool  (** Same as [e_lt] is [e_true] *)
  val eval_leq  : term -> term -> bool  (** Same as [e_leq] is [e_true]  *)

  val repr : term -> QED.repr (** Constant time *)
  val sort : term -> Logic.sort (** Constant time *)
  val vars : term -> Vars.t (** Constant time *)
  val varsp : pred -> Vars.t (** Constant time *)
  val occurs : var -> term -> bool
  val occursp : var -> pred -> bool
  val intersect : term -> term -> bool
  val intersectp : pred -> pred -> bool
  val is_subterm : term -> term -> bool
  
  (** Try to extract a type of term.
      Parameterized by optional extractors for field and functions.
      Extractors may raise [Not_found] ; however, they are only used when
      the provided kinds for fields and functions are not precise enough.
      @param field type of a field value
      @param record type of the record containing a field
      @param call type of the values returned by the function
      @raise Not_found if no type is found. *)
  val typeof :
    ?field:(Field.t -> tau) ->
    ?record:(Field.t -> tau) ->
    ?call:(Fun.t -> tau option list -> tau) ->
    term -> tau

  (** {3 Builtins}

      The functions below register simplifiers for function [f]. The computation
      code may raise [Not_found], in which case the symbol is not interpreted.

      If [f] is an operator with algebraic rules (see type [operator]), the
      children are normalized {i before} builtin call.

      Highest priority is [0]. Recursive calls must be performed on strictly
      smaller terms. *)

  val set_builtin : lfun -> (term list -> term) -> unit
  val set_builtin_1 : lfun -> unop -> unit
  val set_builtin_2 : lfun -> binop -> unit
  val set_builtin_eq : lfun -> binop -> unit
  val set_builtin_leq : lfun -> binop -> unit
  val set_builtin_eqp : lfun -> cmp -> unit

  val release : unit -> unit (** Empty local caches *)

  (** {3 Internal Checks} *)

  module Check :
  sig
    val reset : unit -> unit
    val set : string -> unit (* check constructor *)
    val is_set : unit -> bool
    val iter : (qed:term -> raw:term -> goal:pred -> unit) -> unit
  end

end


module N: sig
  (** simpler notation for writing {!F.term} and {F.pred} *)

  val ( + ): F.binop (** {! F.p_add } *)
  val ( - ): F.binop (** {! F.p_sub } *)
  val ( ~- ): F.unop (** [fun x -> p_sub 0 x] *)
  val ( * ): F.binop (** {! F.p_mul} *)
  val ( / ): F.binop (** {! F.p_div} *)
  val ( mod ): F.binop (** {! F.p_mod} *)

  val ( = ): F.cmp (** {! F.p_equal} *)
  val ( < ): F.cmp (** {! F.p_lt} *)
  val ( > ): F.cmp (** {! F.p_lt} with inversed argument *)
  val ( <= ): F.cmp (** {! F.p_leq } *)
  val ( >= ): F.cmp (** {! F.p_leq } with inversed argument *)
  val ( <> ): F.cmp (** {! F.p_neq } *)

  val ( && ): F.operator (** {! F.p_and } *)
  val ( || ): F.operator (** {! F.p_or } *)
  val not: F.pred -> F.pred (** {! F.p_not } *)

  val ( $ ): lfun -> F.term list -> F.term (** {! F.e_fun } *)
  val ( $$ ): lfun -> F.term list -> F.pred (** {! F.p_call } *)
end


(** {2 Fresh Variables and Constraints} *)

open F

type gamma
val new_pool : ?copy:F.pool -> ?vars:Vars.t -> unit -> pool
val new_gamma : ?copy:gamma -> unit -> gamma

val local : ?pool:pool -> ?vars:Vars.t -> ?gamma:gamma -> ('a -> 'b) -> 'a -> 'b

val freshvar : ?basename:string -> tau -> var
val freshen : var -> var
val assume : pred -> unit
val without_assume : ('a -> 'b) -> 'a -> 'b
val epsilon : ?basename:string -> tau -> (term -> pred) -> term
val hypotheses : gamma -> pred list
val variables : gamma -> var list

val get_pool : unit -> pool
val get_gamma : unit -> gamma
val has_gamma : unit -> bool
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

(** {2 Substitution} *)

module Subst :
sig
  
  type sigma

  val sigma : F.var list -> F.term list -> sigma
  val e_apply : sigma -> F.term -> F.term
  val p_apply : sigma -> F.pred -> F.pred
  
end

(* -------------------------------------------------------------------------- *)
