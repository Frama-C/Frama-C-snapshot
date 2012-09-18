(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
  associative : bool ; (* x+(y+z)=(x+y)+z *)
  commutative : bool ; (* x+y=y+x *)
  stable : bool ;      (* x+x = x *)
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
  val id : t -> string (** full, unique, identifier *)
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
  val sort : t -> sort (** of result *)
end

(** {2 Bound Variables} *)
module type Variable =
sig
  include Symbol
  val sort : t -> sort
  val basename : t -> string
end

(** {2 Representation of Patterns, Functions and Terms} *)

type ('f,'a) funtype = {
  result : ('f,'a) datatype ; (** Type of returned value *)
  params : ('f,'a) datatype list ; (** Type of parameters *)
}

type ('f,'d,'x,'e) term_repr =
  | True
  | False
  | Kint  of Z.t
  | Kreal of R.t
  | Times of Z.t * 'e
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
  | Fun   of 'd * 'e list
  | Var   of 'x
  | Apply of 'e * 'e list
  | Bind  of binder * 'x * 'e

type 'a affine = { constant : Z.t ; factors : (Z.t * 'a) list }

(** {2 Formulae} *)
module type Term =
sig

  module ADT : Data
  module Field : Field
  module Fun : Function
  module Var : Variable

  type term

  (** {3 Variables} *)

  type var = Var.t
  type tau = (Field.t,ADT.t) datatype
  type signature = (Field.t,ADT.t) funtype

  module Vars : Idxset.S with type elt = var

  type pool
  val pool : unit -> pool

  val add_var : pool -> var -> unit
  val add_vars : pool -> Vars.t -> unit
  val add_term : pool -> term -> unit

  val fresh : pool -> ?basename:string -> tau -> var
  val alpha : pool -> var -> var

  val tau_of_var : var -> tau
  val base_of_var : var -> string

  (** {3 Terms} *)

  type repr = (Field.t,Fun.t,var,term) term_repr
  type path = int list (** position of a subterm in a term. *)
  type record = (Field.t * term) list

  val is_true  : term -> maybe (** Constant time *)
  val is_false : term -> maybe (** Constant time *)
  val is_prop  : term -> bool (** Boolean or Property *)
  val is_int   : term -> bool (** Integer sort *)
  val is_real  : term -> bool (** Real sort *)
  val is_arith : term -> bool (** Integer or Real sort *)

  val are_equal : term -> term -> maybe (** Computes equality *)

  val repr : term -> repr   (** Constant time *)
  val sort : term -> sort   (** Constant time *)
  val vars : term -> Vars.t (** Constant time *)

  val subterm: term -> path -> term
  val change_subterm: term -> path -> term -> term

  (** {3 Basic constructors} *)

  val e_true : term
  val e_false : term
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

  (** {3 Quantification and Binding} *)

  val e_forall : var list -> term -> term
  val e_exists : var list -> term -> term
  val e_lambda : var list -> term -> term
  val e_bind : binder -> var -> term -> term
  val e_subst : ?pool:pool -> var -> term -> term -> term
  val e_apply : ?pool:pool -> term -> term list -> term

  (** {3 Recursion Scheme} *)

  val e_map  : (term -> term) -> term -> term
    (** @raise Invalid_argument on Bind constructor *)

  val e_iter : (term -> unit) -> term -> unit
    (** Also goes into Bind constructor *)

  (** {3 Specific Patterns} *)

  val flattenable : term -> bool
  val flattens : term -> term -> bool (** The comparison flattens *)
  val flatten : term -> term list (** Returns an equivalent conjunction *)
  val affine : term -> term affine
  val record_with : record -> (term * record) option

  (** {3 Symbol} *)

  type t = term
  val id : t -> int (** unique identifier (stored in t) *)
  val hash : t -> int (** constant access (stored in t) *)
  val equal : t -> t -> bool (** physical equality *)
  val compare : t -> t -> int (** atoms are lower than complex terms ; otherwise, sorted by id. *)
  val pretty : Format.formatter -> t -> unit

  (** {3 Utilities} *)

  val is_closed : t -> bool (** No bound variables *)
  val is_simple : t -> bool (** Constants, variables, functions of arity 0 *)
  val is_atomic : t -> bool (** Constants and variables *)
  val is_primitive : t -> bool (** Constants only *)

  val debug : Format.formatter -> t -> unit
  val state : Format.formatter -> ( string * t ) list -> unit

  module Tset : Idxset.S with type elt = term
  module Tmap : Idxmap.S with type key = term

  (** {2 Shared sub-terms} *)

  val shared : 
    ?atomic:(term -> bool) -> 
    ?shareable:(term -> bool) -> 
    ?closed:Vars.t -> 
    term list -> term list
    (** Computes the sub-terms that appear several times.
	[shared marked linked e] returns the shared subterms of [e].

	The list of shared subterms is consistent with
	order of definition: each trailing terms only depend on heading ones.

	The traversal is controled by two optional arguments:
	- [atomic] those terms are not traversed (considered as atomic)
	- [shareable] those terms that can be shared (all by default)
	- [closed] free variables of [t] authorized in sub-terms
    *)

end
