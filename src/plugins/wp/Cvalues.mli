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
(* --- Lifting Operations over Memory Values                              --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Sigs
open Lang.F

val equation : Sigs.equation -> pred

(** {2 Pretty Printing} *)

type 'a printer = Format.formatter -> 'a -> unit

val pp_bound : term option printer
val pp_value : 'a printer -> 'a value printer
val pp_logic : 'a printer -> 'a logic printer
val pp_region : 'a printer -> 'a region printer
val pp_sloc : 'a printer -> 'a sloc printer
val pp_rloc : 'a printer -> 'a rloc printer

(** {2 Int-As-Booleans} *)

val bool_val : unop
val bool_eq : binop
val bool_lt : binop
val bool_neq : binop
val bool_leq : binop
val bool_and : binop
val bool_or  : binop
val is_true  : pred -> term (** [p ? 1 : 0] *)
val is_false : pred -> term (** [p ? 0 : 1] *)

(** {2 Null Values} *)

val null : (term -> pred) Context.value (** test for null pointer value *)

val is_null : c_object -> term -> pred

(** {2 Start of Arrays} *)

(** Shift a location with 0-indices wrt to its array type *)
val startof : shift:('a -> c_object -> term -> 'a) -> 'a -> typ -> 'a

(** {2 Typing and Sub-Typing for C and ACSL Types} *)

val is_object : c_object -> 'a value -> pred
val has_ctype : typ -> term -> pred
val has_ltype : logic_type -> term -> pred

val cdomain : c_object -> (term -> pred) option
val ldomain : logic_type -> (term -> pred) option

(** {2 Volatile Access} *)

val volatile : ?warn:string -> unit -> bool
(** Check if a volatile access must be properly modelled or ignored.
    In case the volatile attribute comes to be ignored,
    the provided warning is emitted, if any. *)

(** {2 ACSL Equality} *)

val equal_object : c_object -> term -> term -> pred
val equal_comp : compinfo -> term -> term -> pred
val equal_array : Matrix.matrix -> term -> term -> pred

(** {2 C and ACSL Constants} *)

val ainf : term option (** Array lower-bound, ie `Some(0)` *)
val asup : int -> term option (** Array upper-bound, ie `Some(n-1)` *)

val constant : constant -> term
val logic_constant : logic_constant -> term
val constant_exp : exp -> term
val constant_term : Cil_types.term -> term

(** {2 Lifting Operations over Memory Values} *)

val map_sloc : ('a -> 'b) -> 'a Sigs.sloc -> 'b Sigs.sloc
val map_value : ('a -> 'b) -> 'a Sigs.value -> 'b Sigs.value
val map_logic : ('a -> 'b) -> 'a Sigs.logic -> 'b Sigs.logic
val plain : logic_type -> term -> 'a Sigs.logic

(** {2 ACSL Utilities} *)

(** positive goal
    negative hypothesis
*)
type polarity = [ `Positive | `Negative | `NoPolarity ]
val negate : polarity -> polarity

module Logic(M : Sigs.Model) :
sig

  open M
  type logic = M.loc Sigs.logic
  type segment = c_object * loc Sigs.sloc
  type region = loc Sigs.region

  (** {3 Projections} *)

  val value : logic -> term
  val loc   : logic -> loc
  val vset  : logic -> Vset.set
  val region : c_object -> logic -> region
  val rdescr : loc sloc -> var list * loc * pred

  (** {3 Morphisms} *)

  val map : unop -> logic -> logic
  val map_opp : logic -> logic
  val map_loc : (loc -> loc) -> logic -> logic
  val map_l2t : (loc -> term) -> logic -> logic
  val map_t2l : (term -> loc) -> logic -> logic

  val apply : binop -> logic -> logic -> logic
  val apply_add : logic -> logic -> logic
  val apply_sub : logic -> logic -> logic

  (** {3 Locations} *)

  val field : logic -> fieldinfo -> logic
  val shift : logic -> c_object -> ?size:int -> logic -> logic
  val load : Sigma.t -> c_object -> logic -> logic

  (** {3 Sets of loc-or-values} *)

  val union : logic_type -> logic list -> logic
  val inter : logic_type -> logic list -> logic
  val subset : logic_type -> logic -> logic_type -> logic -> pred

  (** {3 Regions} *)

  val separated : region list -> pred
  val included : segment -> segment -> pred
  val valid : Sigma.t -> acs -> segment -> pred
  val invalid : Sigma.t -> segment -> pred

end
