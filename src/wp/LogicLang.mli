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

(* -------------------------------------------------------------------------- *)
(** Logical Language *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau

(* -------------------------------------------------------------------------- *)
(** {2 Primitives} *)
(* -------------------------------------------------------------------------- *)

type integer_op = Iadd | Isub | Imul | Idiv | Imod
type real_op = Radd | Rsub | Rmul | Rdiv
type cmp_op = Ceq | Cneq | Clt | Cleq

(* -------------------------------------------------------------------------- *)
(** {2 Terms} *)
(* -------------------------------------------------------------------------- *)

type term 

(** {3 Primitives} *)

val e_true : term
val e_false : term
val e_zero : term
val e_int : int -> term
val e_float : float -> term
val e_bigint : My_bigint.t -> term
val e_icst : string -> term
val e_rcst : string -> term

(** {3 Arithmetics} *)

val e_ineg : term -> term
val e_rneg : term -> term
val e_iop : integer_op -> term -> term -> term
val e_rop : real_op -> term -> term -> term
val e_icmp : cmp_op -> term -> term -> term
val e_rcmp : cmp_op -> term -> term -> term

val e_bnot : term -> term
val e_band : term -> term -> term
val e_bor  : term -> term -> term
val e_bxor : term -> term -> term
val e_lshift : term -> term -> term
val e_rshift : term -> term -> term

val e_int_of_real : term -> term
val e_real_of_int : term -> term

(** {3 Booleans} *)

val e_not  : term -> term
val e_and  : term -> term -> term
val e_or   : term -> term -> term

(** {3 Structures} *)

val e_getfield : term -> field -> term
val e_setfield : term -> field -> term -> term
val e_access : term -> term -> term
val e_update : term -> term -> term -> term

(** {2 Predicates} *)

type pred

val p_true : pred
val p_false : pred
val p_bool : term -> pred
val p_not : pred -> pred
val p_and : pred -> pred -> pred
val p_or  : pred -> pred -> pred
val p_xor : pred -> pred -> pred
val p_implies : pred -> pred -> pred
val p_iff : pred -> pred -> pred

val p_icmp : cmp_op -> term -> term -> pred
val p_rcmp : cmp_op -> term -> term -> pred
val p_equal : term -> term -> pred
val p_neq : term -> term -> pred

val p_conj : pred list -> pred
val p_disj : pred list -> pred
val p_goal : pred list -> pred -> pred

(** {2 Generic Terms and Formulas} *)

val e_call : id -> term list -> term
val p_call : id -> term list -> pred

val e_cond : term -> term -> term -> term
val p_cond : term -> pred -> pred -> pred

val p_named : id -> pred -> pred
val p_hide : pred -> pred

(** {2 Variables} *)

type var
type pool

val pool : unit -> pool
val fresh : pool -> string -> tau -> var

val e_var : var -> term
val e_let : ?pool:pool -> var -> term -> term -> term
val p_let : ?pool:pool -> var -> term -> pred -> pred
val p_forall : var -> pred -> pred
val p_exists : var -> pred -> pred

val tau_of_var : var -> tau
val is_atomic : term -> bool

module Vset : Set.S with type elt = var
module Vmap : Map.S with type key = var

(** {2 Pretty Printers} *)

val pp_tau : Format.formatter -> tau -> unit
val pp_term : Format.formatter -> term -> unit
val pp_pred : Format.formatter -> pred -> unit

(** {2 Dependencies} *)

val add_depend_tau : Iset.t -> tau -> Iset.t
val add_depend_term : Iset.t -> term -> Iset.t
val add_depend_pred : Iset.t -> pred -> Iset.t
