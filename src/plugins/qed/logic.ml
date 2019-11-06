(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
  | E_fun of 'a * 'a element list

(** Algebraic properties for user operators. *)
type 'a operator = {
  invertible : bool ; (* x+y = x+z <-> y=z (on both side) *)
  associative : bool ; (* x+(y+z)=(x+y)+z *)
  commutative : bool ; (* x+y=y+x *)
  idempotent : bool ; (* x+x = x *)
  neutral : 'a element ;
  absorbant : 'a element ;
}

(** Algebraic properties for functions. *)
type 'a category =
  | Function      (** logic function *)
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
type ('f,'a,'d,'x,'b,'e) term_repr =
  | True
  | False
  | Kint  of Z.t
  | Kreal of Q.t
  | Times of Z.t * 'e      (** mult: k1 * e2 *)
  | Add   of 'e list      (** add:  e11 + ... + e1n *)
  | Mul   of 'e list      (** mult: e11 * ... * e1n *)
  | Div   of 'e * 'e
  | Mod   of 'e * 'e
  | Eq    of 'e * 'e
  | Neq   of 'e * 'e
  | Leq   of 'e * 'e
  | Lt    of 'e * 'e
  | Aget  of 'e * 'e      (** access: array1[idx2] *)
  | Aset  of 'e * 'e * 'e (** update: array1[idx2 -> elem3] *)
  | Acst  of ('f,'a) datatype * 'e (** constant array [ type -> value ] *)
  | Rget  of 'e * 'f
  | Rdef  of ('f * 'e) list
  | And   of 'e list      (** and: e11 && ... && e1n *)
  | Or    of 'e list      (** or:  e11 || ... || e1n *)
  | Not   of 'e
  | Imply of 'e list * 'e (** imply: (e11 && ... && e1n) ==> e2 *)
  | If    of 'e * 'e * 'e (** ite: if c1 then e2 else e3 *)
  | Fun   of 'd * 'e list (** Complete call (no partial app.) *)
  | Fvar  of 'x
  | Bvar  of int * ('f,'a) datatype
  | Apply of 'e * 'e list (** High-Order application (Cf. binder) *)
  | Bind  of binder * ('f,'a) datatype * 'b

type 'a affine = { constant : Z.t ; factors : (Z.t * 'a) list }

(** {2 Formulae} *)
module type Term =
sig

  module ADT : Data
  module Field : Field
  module Fun : Function
  module Var : Variable

  type term
  type lc_term
  (** Loosely closed terms. *)

  module Term : Symbol with type t = term

  (** Non-structural, machine dependent,
      but fast comparison and efficient merges *)
  module Tset : Idxset.S with type elt = term

  (** Non-structural, machine dependent,
      but fast comparison and efficient merges *)
  module Tmap : Idxmap.S with type key = term

  (** Structuraly ordered, but less efficient access and non-linear merges *)
  module STset : Set.S with type elt = term

  (** Structuraly ordered, but less efficient access and non-linear merges *)
  module STmap : Map.S with type key = term

  (** {3 Variables} *)

  type var = Var.t
  type tau = (Field.t,ADT.t) datatype

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

  type 'a expression = (Field.t,ADT.t,Fun.t,var,lc_term,'a) term_repr

  type repr = term expression

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

  (** Path-positioning access

      This part of the API is DEPRECATED
  *)

  type path = int list (** position of a subterm in a term. *)

  val subterm: term -> path -> term
  [@@deprecated "Path-access might be unsafe in presence of binders"]

  val change_subterm: term -> path -> term -> term
  [@@deprecated "Path-access might be unsafe in presence of binders"]

  (** {3 Basic constructors} *)

  val e_true : term
  val e_false : term
  val e_bool : bool -> term
  val e_literal : bool -> term -> term
  val e_int : int -> term
  val e_float : float -> term
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
  val e_const : tau -> term -> term
  val e_get   : term -> term -> term
  val e_set   : term -> term -> term -> term
  val e_getfield : term -> Field.t -> term
  val e_record : record -> term
  val e_fun : ?result:tau -> Fun.t -> term list -> term
  val e_repr : ?result:tau -> repr -> term
  (** @raise Invalid_argument on [Bvar] and [Bind] *)

  (** {3 Quantifiers and Binding} *)

  val e_forall : var list -> term -> term
  val e_exists : var list -> term -> term
  val e_lambda : var list -> term -> term
  val e_apply : term -> term list -> term

  val e_bind : binder -> var -> term -> term
  (** Bind the given variable if it appears free in the term,
      or return the term unchanged. *)

  val lc_open : var -> lc_term -> term
  [@@deprecated "Use e_unbind instead"]

  val e_unbind : var -> lc_term -> term
  (** Opens the top-most bound variable with a (fresh) variable.
      Can be only applied on top-most lc-term from `Bind(_,_,_)`,
      thanks to typing. *)

  val e_open : pool:pool -> ?forall:bool -> ?exists:bool -> ?lambda:bool ->
    term -> (binder * var) list * term
  (** Open all the specified binders (flags default to `true`, so all
      consecutive top most binders are opened by default).
      The pool must contain all free variables of the term. *)

  val e_close : (binder * var) list -> term -> term
  (** Closes all specified binders *)

  (** {3 Generalized Substitutions} *)

  type sigma
  val sigma : ?pool:pool -> unit -> sigma

  module Subst :
  sig
    type t = sigma
    val create : ?pool:pool -> unit -> t

    val fresh : t -> tau -> var
    val get : t -> term -> term
    val filter : t -> term -> bool

    val add : t -> term -> term -> unit
    (** Must bind lc-closed terms, or raise Invalid_argument *)

    val add_map : t -> term Tmap.t -> unit
    (** Must bind lc-closed terms, or raise Invalid_argument *)

    val add_fun : t -> (term -> term) -> unit
    (** Must bind lc-closed terms, or raise Invalid_argument *)

    val add_filter : t -> (term -> bool) -> unit
    (** Only modifies terms that {i pass} the filter. *)

    val add_var : t -> var -> unit
    (** To the pool *)

    val add_vars : t -> Vars.t -> unit
    (** To the pool *)

    val add_term : t -> term -> unit
    (** To the pool *)
  end

  val e_subst : sigma -> term -> term
  (**
     The environment sigma must be prepared with the desired substitution.
     Its pool of fresh variables must covers the entire domain and co-domain
     of the substitution, and the transformed values.
  *)

  val e_subst_var : var -> term -> term -> term

  (** {3 Locally Nameless Representation}

      These functions can be {i unsafe} because they might expose terms
      that contains non-bound b-vars. Never use such terms to build
      substitutions (sigma).
  *)

  val lc_vars : term -> Bvars.t
  val lc_closed : term -> bool
  (** All bound variables are under their binder *)

  val lc_repr : lc_term -> term
  (** Calling this function is {i unsafe} unless the term is lc_closed *)

  val lc_iter : (term -> unit) -> term -> unit
  (** Similar to [f_iter] but exposes non-closed sub-terms of `Bind`
      as regular [term] values instead of [lc_term] ones. *)

  (** {3 Iteration Scheme} *)

  val f_map  : ?pool:pool -> ?forall:bool -> ?exists:bool -> ?lambda:bool
    -> (term -> term) -> term -> term
  (** Pass and open binders, maps its direct sub-terms
      and then close then opened binders
      Raises Invalid_argument in case of a bind-term without pool.
      The optional pool must contain all free variables of the term. *)

  val f_iter : ?pool:pool -> ?forall:bool -> ?exists:bool -> ?lambda:bool
    -> (term -> unit) -> term -> unit
  (** Iterates over its direct sub-terms (pass and open binders)
      Raises Invalid_argument in case of a bind-term without pool.
      The optional pool must contain all free variables of the term. *)

  (** {3 Partial Typing} *)

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
    ?call:(Fun.t -> tau option list -> tau) -> term -> tau

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

  val set_builtin' : Fun.t -> (term list -> tau option -> term) -> unit

  val set_builtin_map : Fun.t -> (term list -> term list) -> unit
  (** Register a builtin for rewriting [f a1..an] into [f b1..bm].

      This is short cut for [set_builtin], where the head application of [f] avoids
      to run into an infinite loop.
  *)

  val set_builtin_get : Fun.t -> (term list -> tau option -> term -> term) -> unit
  (** [set_builtin_get f rewrite] register a builtin
      for rewriting [(f a1..an)[k]] into [rewrite (a1..an) k].
      The type given is the type of (f a1..an).
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
  (** Knowing [h], [consequence h a] returns [b] such that [h -> (a<->b)] *)
  val literal : term -> bool * term

  val affine : term -> term affine
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

  (** {2 Shared sub-terms} *)

  val is_subterm : term -> term -> bool
  (** Occurrence check. [is_subterm a b] returns [true] iff [a] is a subterm
      of [b]. Optimized {i wrt} shared subterms, term size, and term
      variables. *)

  val shared :
    ?shared:(term -> bool) ->
    ?shareable:(term -> bool) ->
    ?subterms:((term -> unit) -> term -> unit) ->
    term list -> term list
  (** Computes the sub-terms that appear several times.
        [shared marked linked e] returns the shared subterms of [e].

        The list of shared subterms is consistent with
        order of definition: each trailing terms only depend on heading ones.

        The traversal is controlled by two optional arguments:
      - [shared] those terms are not traversed (considered as atomic, default to none)
      - [shareable] those terms ([is_simple] excepted) that can be shared (default to all)
      - [subterms] those sub-terms a term to be considered during
          traversal ([lc_iter] by default)
  *)

  (** Low-level shared primitives: [shared] is actually a combination of
      building marks, marking terms, and extracting definitions:

      {[ let share ?... e =
           let m = marks ?... () in
           List.iter (mark m) es ;
           defs m ]} *)

  type marks

  (** Create a marking accumulator.
      Same defaults than [shared]. *)

  val marks :
    ?shared:(term -> bool) ->
    ?shareable:(term -> bool) ->
    ?subterms:((term -> unit) -> term -> unit) ->
    unit -> marks

  (** Mark a term to be printed *)
  val mark : marks -> term -> unit

  (** Mark a term to be explicitly shared *)
  val share : marks -> term -> unit

  (** Returns a list of terms to be shared among all {i shared} or {i
      marked} subterms.  The order of terms is consistent with
      definition order: head terms might be used in tail ones. *)
  val defs : marks -> term list

end
