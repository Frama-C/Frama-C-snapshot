(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(* -------------------------------------------------------------------------- *)
(** {1 First Order Logic Definition}                                          *)
(* -------------------------------------------------------------------------- *)

type 'a element =
  | E_none
  | E_true
  | E_false
  | E_int of int
  | E_const of 'a

(** Algebraic properties for user operators. *)
type 'a operator = {
  inversible : bool ; (* x+y = x+z <-> y=z (on both side) *)
  associative : bool ; (* x+(y+z)=(x+y)+z *)
  commutative : bool ; (* x+y=y+x *)
  idempotent : bool ; (* x+x = x *)
  neutral : 'a element ;
  absorbant : 'a element ;
}

(** Algebraic properties for functions. *)
type 'a category =
  | Function      (** no reduction rule *)
  | Constructor   (** [f xs = g ys] iff [f=g && xi=yi] *)
  | Injection     (** [f xs = f ys] iff [xi=yi] *)
  | Operator of 'a operator

(** Quantifiers and Binders *)
type binder =
  | Forall
  | Exists
  | Lambda

type ('f,'a) datatype =
  | Prop
  | Bool
  | Int
  | Real
  | Tvar of int (** ranges over [1..arity] *)
  | Array of ('f,'a) datatype * ('f,'a) datatype
  | Record of ('f *  ('f,'a) datatype) list
  | Data of 'a * ('f,'a) datatype list

type sort =
  | Sprop
  | Sbool
  | Sint
  | Sreal
  | Sdata
  | Sarray of sort

type maybe = Yes | No | Maybe

(** Ordered, hash-able and pretty-printable symbols *)
module type Symbol =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val debug : t -> string (** for printing during debug *)
end

(** {2 Abstract Data Types} *)
module type Data =
sig
  include Symbol
  val basename : t -> string (** hint for generating fresh names *)
end

(** {2 Field for Record Types} *)
module type Field =
sig
  include Symbol
  val sort : t -> sort (** of field *)
end

(** {2 User Defined Functions} *)
module type Function =
sig
  include Symbol
  val category : t -> t category
  val params : t -> sort list (** params ; exceeding params use Sdata *)
  val sort : t -> sort (** result *)
end

(** {2 Bound Variables} *)
module type Variable =
sig
  include Symbol
  val sort : t -> sort
  val basename : t -> string
  val dummy : t
end

(** {2 Representation of Patterns, Functions and Terms} *)

type ('f,'a) funtype = {
  result : ('f,'a) datatype ; (** Type of returned value *)
  params : ('f,'a) datatype list ; (** Type of parameters *)
}

(** representation of terms. type arguments are the following:
  - 'z: representation of integral constants
  - 'f: representation of fields
  - 'a: representation of abstract data types
  - 'd: representation of functions
  - 'x: representation of free variables
  - 'b: representation of bound term (phantom type equal to 'e)
  - 'e: sub-expression
*)
type ('z,'f,'a,'d,'x,'b,'e) term_repr =
  | True
  | False
  | Kint  of 'z
  | Kreal of R.t
  | Times of 'z * 'e
  | Add   of 'e list
  | Mul   of 'e list
  | Div   of 'e * 'e
  | Mod   of 'e * 'e
  | Eq    of 'e * 'e
  | Neq   of 'e * 'e
  | Leq   of 'e * 'e
  | Lt    of 'e * 'e
  | Aget  of 'e * 'e
  | Aset  of 'e * 'e * 'e
  | Rget  of 'e * 'f
  | Rdef  of ('f * 'e) list
  | And   of 'e list
  | Or    of 'e list
  | Not   of 'e
  | Imply of 'e list * 'e
  | If    of 'e * 'e * 'e
  | Fun   of 'd * 'e list (** Complete call (no partial app.) *)
  | Fvar  of 'x
  | Bvar  of int * ('f,'a) datatype
  | Apply of 'e * 'e list (** High-Order application (Cf. binder) *)
  | Bind  of binder * ('f,'a) datatype * 'b

type ('z,'a) affine = { constant : 'z ; factors : ('z * 'a) list }

(** {2 Formulae} *)
module type Term =
sig

  module Z : Arith.Z
  module ADT : Data
  module Field : Field
  module Fun : Function
  module Var : Variable

  type term
  type bind

  (** {3 Variables} *)

  type var = Var.t
  type tau = (Field.t,ADT.t) datatype
  type signature = (Field.t,ADT.t) funtype

  module Tau : Data with type t = tau
  module Vars : Idxset.S with type elt = var
  module Vmap : Idxmap.S with type key = var

  type pool
  val pool : ?copy:pool -> unit -> pool

  val add_var : pool -> var -> unit
  val add_vars : pool -> Vars.t -> unit
  val add_term : pool -> term -> unit

  val fresh : pool -> ?basename:string -> tau -> var
  val alpha : pool -> var -> var

  val tau_of_var : var -> tau
  val sort_of_var : var -> sort
  val base_of_var : var -> string

  (** {3 Terms} *)

  type 'a expression = (Z.t,Field.t,ADT.t,Fun.t,var,bind,'a) term_repr

  type repr = term expression
  type path = int list (** position of a subterm in a term. *)
  type record = (Field.t * term) list

  val decide   : term -> bool (** Return [true] if and only the term is [e_true]. Constant time. *)
  val is_true  : term -> maybe (** Constant time. *)
  val is_false : term -> maybe (** Constant time. *)
  val is_prop  : term -> bool (** Boolean or Property *)
  val is_int   : term -> bool (** Integer sort *)
  val is_real  : term -> bool (** Real sort *)
  val is_arith : term -> bool (** Integer or Real sort *)

  val are_equal : term -> term -> maybe (** Computes equality *)
  val eval_eq   : term -> term -> bool  (** Same as [are_equal] is [Yes] *)
  val eval_neq  : term -> term -> bool  (** Same as [are_equal] is [No]  *)
  val eval_lt   : term -> term -> bool  (** Same as [e_lt] is [e_true] *)
  val eval_leq  : term -> term -> bool  (** Same as [e_leq] is [e_true]  *)

  val repr : term -> repr  (** Constant time *)
  val sort : term -> sort   (** Constant time *)
  val vars : term -> Vars.t (** Constant time *)

  val subterm: term -> path -> term
  val change_subterm: term -> path -> term -> term

  (** {3 Basic constructors} *)

  val e_true : term
  val e_false : term
  val e_bool : bool -> term
  val e_literal : bool -> term -> term
  val e_int : int -> term
  val e_zint : Z.t -> term
  val e_real : R.t -> term
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

  val e_repr : repr -> term

  (** {3 Quantification and Binding} *)

  val e_forall : var list -> term -> term
  val e_exists : var list -> term -> term
  val e_lambda : var list -> term -> term
  val e_bind : binder -> var -> term -> term
  val e_apply : term -> term list -> term

  (** {3 Generalized Substitutions} *)

  type sigma
  val sigma : unit -> sigma

  val e_subst : ?sigma:sigma -> (term -> term) -> term -> term
  val e_subst_var : var -> term -> term -> term

  (** {3 Localy Nameless Representation} *)

  val lc_bind : var -> term -> bind (** Close [x] as a new bound variable *)
  val lc_open : var -> bind -> term (** Instanciate top bound variable *)
  val lc_open_term : term -> bind -> term
  (** Instanciate top bound variable with the given term *)
  val lc_closed : term -> bool
  val lc_closed_at : int -> term -> bool
  val lc_vars : term -> Bvars.t
  val lc_repr : bind -> term

  (** {3 Recursion Scheme} *)

  val e_map  : pool -> (term -> term) -> term -> term
  val e_iter : pool -> (term -> unit) -> term -> unit

  val f_map  : (int -> term -> term) -> int -> term -> term
  val f_iter : (int -> term -> unit) -> int -> term -> unit

  val lc_map : (term -> term) -> term -> term
  val lc_iter : (term -> unit) -> term -> unit

  (** {3 Support for Builtins} *)

  val set_builtin : Fun.t -> (term list -> term) -> unit
  (** Register a simplifier for function [f]. The computation code
      	may raise [Not_found], in which case the symbol is not interpreted.

      	If [f] is an operator with algebraic rules (see type
      	[operator]), the children are normalized {i before} builtin
      	call.

      	Highest priority is [0].
      	Recursive calls must be performed on strictly smaller terms.
  *)

  val set_builtin_eq : Fun.t -> (term -> term -> term) -> unit
  (** Register a builtin equality for comparing any term with head-symbol.
      	{b Must} only use recursive comparison for strictly smaller terms.
      	The recognized term with head function symbol is passed first.

      	Highest priority is [0].
      	Recursive calls must be performed on strictly smaller terms.
  *)

  val set_builtin_leq : Fun.t -> (term -> term -> term) -> unit
  (** Register a builtin for comparing any term with head-symbol.
      	{b Must} only use recursive comparison for strictly smaller terms.
      	The recognized term with head function symbol can be on both sides.
      	Strict comparison is automatically derived from the non-strict one.

      	Highest priority is [0].
      	Recursive calls must be performed on strictly smaller terms.
  *)

  (** {3 Specific Patterns} *)

  val consequence : term -> term -> term
  (** Kowning [h], [consequence h a] returns [b] such that [h -> (a<->b)] *)
  val literal : term -> bool * term
  val congruence_eq : term -> term -> (term * term) list option
  (** If [congruence_eq a b] returns [[ai,bi]], [a=b] is equivalent to [And{ai=bi}]. *)
  val congruence_neq : term -> term -> (term * term) list option
  (** If [congruence_eq a b] returns [[ai,bi]], [a<>b] is equivalent to [Or{ai<>bi}]. *)
  val flattenable : term -> bool
  val flattens : term -> term -> bool (** The comparison flattens *)
  val flatten : term -> term list (** Returns an equivalent conjunction *)
  val affine : term -> (Z.t,term) affine
  val record_with : record -> (term * record) option

  (** {3 Symbol} *)

  type t = term
  val id : t -> int (** unique identifier (stored in t) *)
  val hash : t -> int (** constant access (stored in t) *)
  val equal : t -> t -> bool (** physical equality *)
  val compare : t -> t -> int (** atoms are lower than complex terms ; otherwise, sorted by id. *)
  val pretty : Format.formatter -> t -> unit
  val weigth : t -> int (** Informal size *)

  (** {3 Utilities} *)

  val is_closed : t -> bool (** No bound variables *)
  val is_simple : t -> bool (** Constants, variables, functions of arity 0 *)
  val is_atomic : t -> bool (** Constants and variables *)
  val is_primitive : t -> bool (** Constants only *)
  val is_neutral : Fun.t -> t -> bool
  val is_absorbant : Fun.t -> t -> bool

  val size : t -> int
  val basename : t -> string

  val debug : Format.formatter -> t -> unit
  val pp_id : Format.formatter -> t -> unit (** internal id *)
  val pp_rid : Format.formatter -> t -> unit (** head symbol with children id's *)
  val pp_repr : Format.formatter -> repr -> unit (** head symbol with children id's *)

  module Term : Symbol with type t = term
  module Tset : Idxset.S with type elt = term
  module Tmap : Idxmap.S with type key = term

  (** {2 Shared sub-terms} *)

  val shared :
    ?shared:(term -> bool) ->
    ?shareable:(term -> bool) ->
    term list -> term list
  (** Computes the sub-terms that appear several times.
      	[shared marked linked e] returns the shared subterms of [e].

      	The list of shared subterms is consistent with
      	order of definition: each trailing terms only depend on heading ones.

      	The traversal is controled by two optional arguments:
      	- [atomic] those terms are not traversed (considered as atomic)
      	- [shareable] those terms that can be shared (all by default)
  *)

  (** Low-level shared primitives: [shared] is actually a combination of
      building marks, marking terms, and extracting definitions:

      {[ let share ?... e =
           let m = marks ?... () in 
           List.iter (mark m) es ; 
           defs m ]} *)

  type marks

  (** Create a marking accumulator
      @param shared terms that are (or will be) already shared
      (default to none) 
      @param shareable terms that can be shared (default to all) *)
  val marks :
    ?shared:(term -> bool) ->
    ?shareable:(term -> bool) ->
    unit -> marks

  (** Mark a term to be printed *)
  val mark : marks -> term -> unit

  (** Mark a term to be explicitely shared *)
  val share : marks -> term -> unit

  (** Returns a list of terms to be shared among all {i shared} or {i
      marked} subterms.  The order of terms is consistent with
      definition order: head terms might be used in tail ones. *)
  val defs : marks -> term list

end
