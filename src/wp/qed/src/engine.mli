(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* --- Engine Signature                                                   --- *)
(* -------------------------------------------------------------------------- *)

(** Generic Engine Signature *)

open Format
open Logic
open Plib
open Linker

type op =
  | Op of string (** Infix or prefix operator *)
  | Assoc of string (** Left-associative binary operator (like + and -) *)
  | Call of string (** Logic function or predicate *)

type link =
  | F_call  of string (** n-ary function *)
  | F_subst of string (** n-ary function with substitution "foo(%1,%2)" *)
  | F_left  of string (** 2-ary function left-to-right + *)
  | F_right of string (** 2-ary function right-to-left + *)
  | F_assoc of string (** associative infix operator *)
  | F_bool_prop of string * string (** Has a bool and prop version *)

type callstyle =
  | CallVar  (** Call is [f(x,...)] ; [f()] can be written [f] *)
  | CallVoid (** Call is [f(x,...)] ; in [f()], [()] is mandatory *)
  | CallApply (** Call is [f x ...] *)

type mode = 
  | Mpositive  (** Current scope is [Prop] in positive position. *)
  | Mnegative  (** Current scope is [Prop] in negative position. *)
  | Mterm      (** Current scope is [Term]. *)
  | Mterm_int  (** [Int]  is required but actual scope is [Term]. *)
  | Mterm_real (** [Real] is required but actual scope is [Term]. *)
  | Mint       (** Current scope is [Int]. *)
  | Mreal      (** Current scope is [Real]. *)

type flow = Flow | Atom

type cmode = Cprop | Cterm
type amode = Aint | Areal
type pmode = Positive | Negative | Boolean

type ('x,'f) ftrigger =
  | TgAny
  | TgVar  of 'x
  | TgGet  of ('x,'f) ftrigger * ('x,'f) ftrigger
  | TgSet  of ('x,'f) ftrigger * ('x,'f) ftrigger * ('x,'f) ftrigger
  | TgFun  of 'f * ('x,'f) ftrigger list
  | TgProp of 'f * ('x,'f) ftrigger list

type ('t,'f,'c) ftypedef =
  | Tabs
  | Tdef of 't
  | Trec of ('f * 't) list
  | Tsum of ('c * 't list) list

(** Generic Engine Signature *)

class type virtual ['z,'adt,'field,'logic,'tau,'var,'term] engine =
  object

    (** {3 Linking} *)

    method virtual datatype : 'adt -> string
    method virtual field : 'field -> string
    method basename : string -> string
    (** Allows to sanitize the basename used for every name except function. *)
    method virtual link : 'logic -> link

    (** {3 Global and Local Environment} *)

    method declare : string -> unit
    method declare_all : string list -> unit

    method local : (unit -> unit) -> unit
    (** Calls the continuation in a local copy of the environment.
        	Previous environment is restored after return, but allocators
        	are left unchanged to enforce on-the-fly alpha-conversion. *)

    method global : (unit -> unit) -> unit
    (** Calls the continuation in a fresh local environment.
        	Previous environment is restored after return. *)

    (** {3 Types} *)

    method t_int  : string
    method t_real : string
    method t_bool : string
    method t_prop : string
    method t_atomic : 'tau -> bool

    method pp_array : 'tau printer (** For [Z->a] arrays *)
    method pp_farray : 'tau printer2 (** For [k->a] arrays *)

    method pp_tvar : int printer (** Type variables. *)
    method pp_datatype : 'adt -> 'tau list printer 

    method pp_tau : 'tau printer (** Without parentheses. *)
    method pp_subtau : 'tau printer (** With parentheses if non-atomic. *)

    (** {3 Current Mode} 

        The mode represents the expected type for a
        term to printed.  A requirement for all term printers in the
        engine is that current mode must be correctly set before call.
        Each term printer is then responsible for setting appropriate
        modes for its sub-terms.
    *)

    method mode : mode
    method with_mode : mode -> (mode -> unit) -> unit
    (** Calls the continuation with given mode for sub-terms.
        	The englobing mode is passed to continuation and then restored. *)

    method op_scope : amode -> string option
    (** Optional scoping post-fix operator when entering arithmetic mode. *)

    (** {3 Primitives} *)

    method e_true : cmode -> string (** ["true"] *)
    method e_false : cmode -> string (** ["false"] *)

    method pp_int : amode -> 'z printer
    method pp_real : R.t printer
    method pp_cst : Numbers.cst printer (** Non-zero reals *)

    (** {3 Variables} *)

    method pp_var : 'var printer (** Default to local env *)

    (** {3 Calls} 

        These printers only applies to connective, operators and
        functions that are morphisms {i w.r.t} current mode.
    *)

    method callstyle : callstyle
    method pp_fun : cmode -> 'logic -> 'term list printer
    method pp_apply : cmode -> 'term -> 'term list printer

    (** {3 Arithmetics Operators} *) 

    method op_real_of_int : op
    method op_add : amode -> op
    method op_sub : amode -> op
    method op_mul : amode -> op
    method op_div : amode -> op
    method op_mod : amode -> op
    method op_minus : amode -> op

    method pp_times : formatter -> 'z -> 'term -> unit
    (** Defaults to [self#op_minus] or [self#op_mul] *)

    (** {3 Comparison Operators} *) 

    method op_equal : cmode -> op
    method op_noteq : cmode -> op
    method op_eq  : cmode -> amode -> op
    method op_neq : cmode -> amode -> op
    method op_lt  : cmode -> amode -> op
    method op_leq : cmode -> amode -> op

    method pp_equal : 'term printer2
    method pp_noteq : 'term printer2

    (** {3 Arrays} *)

    method pp_array_get : formatter -> 'term -> 'term -> unit
    (** Access ["a[k]"]. *)

    method pp_array_set : formatter -> 'term -> 'term -> 'term -> unit
    (** Update ["a[k <- v]"]. *)

    (** {3 Records} *)

    method pp_get_field : formatter -> 'term -> 'field -> unit
    (** Field access. *)

    method pp_def_fields : ('field * 'term) list printer
    (** Record construction. *)

    (** {3 Logical Connectives} *)

    method op_not   : cmode -> op
    method op_and   : cmode -> op
    method op_or    : cmode -> op
    method op_imply : cmode -> op
    method op_equiv : cmode -> op

    (** {3 Conditionals} *)

    method pp_not : 'term printer
    method pp_imply : formatter -> 'term list -> 'term -> unit

    method pp_conditional : formatter -> 'term -> 'term -> 'term -> unit

    (** {3 Binders} *)

    method pp_forall : 'tau -> 'var list printer (** with separator *)
    method pp_exists : 'tau -> 'var list printer (** with separator *)
    method pp_lambda : 'var list printer

    (** {3 Bindings} *)

    method is_shareable : 'term -> bool    
    method bind : 'var -> unit
    method pp_let : formatter -> pmode -> string -> 'term -> unit

    (** {3 Terms} *)

    method is_atomic : 'term -> bool
    (** Sub-terms that require parentheses.
        	Shared sub-terms are detected on behalf of this method. *)

    method pp_flow : 'term printer
    (** Printer with shared sub-terms and without parentheses. *)

    method pp_atom : 'term printer
    (** Printer with shared sun-terms and parentheses for non-atomic expressions. *)

    (** {3 Top Level} *)

    method pp_term : 'term printer
    (** Prints in {i term} mode. 
        	Default uses [self#pp_shared] with mode [Mterm] inside an [<hov>] box. *)

    method pp_prop : 'term printer
    (** Prints in {i prop} mode. 
        	Default uses [self#pp_shared] with mode [Mprop] inside an [<hv>] box. *)

    method pp_expr : 'tau -> 'term printer
    (** Prints in {i term}, {i arithemtic} or {i prop} mode with 
        	respect to provided type. *)

    method declare_type : formatter -> 'adt -> int -> ('tau,'field,'logic) ftypedef -> unit
    method declare_axiom :
      formatter -> string -> 'var list -> ('var,'logic) ftrigger list list -> 'term -> unit
    method declare_signature : formatter -> 'logic -> 'tau list -> 'tau -> unit
    method declare_definition : formatter -> 'logic -> 'var list -> 'tau -> 'term -> unit

  end
