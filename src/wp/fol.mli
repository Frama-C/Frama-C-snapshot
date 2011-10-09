(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

type constant = private
  | ConstInt of string
  | ConstBool of bool
  | ConstUnit
  | ConstFloat of string

val c_bool : bool -> constant
val c_int_of_str : string -> constant
val c_int : int -> constant
val c_float : float -> constant
val c_float_of_str : string -> constant

(** Fol Variable definition*)
module Var : sig

  type t 
    (* = (string * int option * Formula.tau * Cil_types.logic_type option) *)

  val mk : 
    string -> int option -> Formula.tau -> Cil_types.logic_type option -> t
  val ident_named_var : string -> int -> Formula.tau -> t
  val fresh_named_var : string -> Formula.tau -> t
  val fresh_var : t -> t
  val var_type : t -> Formula.tau
  val var_name : t -> string
  val basename : t -> string
  val kind_of_var : t -> Formula.kind
  val equal : t -> t -> bool
  val compare : t -> t -> int

end

module Vset : Set.S with type elt = Var.t
module Vmap : Map.S with type key = Var.t

(*----------------------------------------------------------------------------*)
(** Terms *)

type term = private
  | Tconst of constant
  | Tvar of Var.t
  | Tapp of string * term list
  | Tgetfield of Cil_types.fieldinfo * term
  | Tsetfield of Cil_types.fieldinfo * term * term
  | Taccess of term * term
  | Tupdate of term * term * term
  | Tif of term * term * term
  | Tlet of Var.t * term * term

val e_true : term
val e_false : term
val e_int : int -> term
val e_int64 : int64 -> term
val e_float : float -> term
val e_cnst : constant -> term
val e_var : Var.t -> term
val e_if : term -> term -> term -> term
val e_app : string -> term list -> term
val e_getfield : Cil_types.fieldinfo -> term -> term
val e_setfield : Cil_types.fieldinfo -> term -> term -> term

val e_access : term -> term -> term
val e_update : term -> term -> term -> term
val e_let : Var.t -> term -> term -> term

val change_in_exp : (Var.t -> term option) -> term -> term
val term_replace : (Var.t -> Var.t option) -> Var.t -> term ->term -> term

(*----------------------------------------------------------------------------*)
(** Predicates *)

type pred =
  | Papp of string * term list
  | Ptrue
  | Pfalse
  | Pimplies of pred * pred
  | Pif of term * pred * pred
  | Pand of pred * pred
  | Por of pred * pred
  | Piff of pred * pred
  | Pnot of pred
  | Pforall of Var.t * pred
  | Pexists of Var.t * pred
  | Plet of Var.t * term * pred
  | Pnamed of string * pred

val eq_terms : term -> term -> bool (** structural equality *)
val eq_preds : pred -> pred -> bool (** (partial) structural equality *)

val p_and :  pred -> pred -> pred
val p_app: string -> term list -> pred
val p_or : pred -> pred -> pred
val p_xor : pred -> pred -> pred
val p_iff : pred -> pred -> pred
val p_not : pred -> pred
val p_if :  term -> pred -> pred -> pred
val p_implies: pred -> pred -> pred
val p_conj : pred list -> pred
val p_disj : pred list -> pred

val p_eq : term -> term -> pred
val p_neq : term -> term -> pred

val p_forall : Var.t -> pred -> pred
val p_exists : Var.t -> pred -> pred
val p_let : Var.t -> term -> pred -> pred
val p_named : string -> pred -> pred

val is_true : pred -> bool
val is_false : pred -> bool

val e_has_var : Var.t list -> term -> bool
val p_has_var : Var.t list -> pred -> bool
val e_closed : Var.t list -> term -> bool
val p_closed : Var.t list -> pred -> bool

val term_calls : string -> term -> bool
val pred_calls : string -> pred -> bool

val subst_in_pred : Var.t -> term -> pred -> pred
val pred_replace :  (Var.t -> Var.t option) -> Var.t -> term -> pred -> pred

val let_pred : fresh:bool -> Var.t -> term -> pred -> pred

val p_forall_vars : Var.t list ->  pred -> pred

val p_exists_vars : Var.t list ->  pred -> pred


(** [pred_alpha_c data_alpha alpha p] -> [alpha', p']*)
val pred_alpha_cv : Var.t Vmap.t  -> pred -> Var.t Vmap.t * pred

val p_alpha_cv : pred -> Var.t list * pred

(*----------------------------------------------------------------------------*)
val huge_term : int -> term -> bool
val huge_pred : int -> pred -> bool
(*----------------------------------------------------------------------------*)

type decl = (Var.t,term,pred) Formula.declaration

val pp_term : (Format.formatter -> term -> unit) ref
