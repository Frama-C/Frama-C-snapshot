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
(* --- Lifting Operations over Memory Values                              --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Memory
open Lang.F

(** {2 Int-As-Boolans} *)
  
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

(** {2 Typing and Sub-Typing for C and ACSL Types} *)

val is_object : c_object -> 'a value -> pred
val has_ctype : typ -> term -> pred
val has_ltype : logic_type -> term -> pred

val cdomain : typ -> (term -> pred) option
val ldomain : logic_type -> (term -> pred) option

(** {2 ACSL Equality} *)

val equal_object : c_object -> term -> term -> pred
val equal_comp : compinfo -> term -> term -> pred
val equal_array : Matrix.matrix -> term -> term -> pred

(** {2 C and ACSL Constants} *)

val constant : constant -> term
val logic_constant : logic_constant -> term
val constant_exp : exp -> term
val constant_term : Cil_types.term -> term

(** {2 Lifting Operations over Memory Values} *)

val map_sloc : ('a -> 'b) -> 'a Memory.sloc -> 'b Memory.sloc
val map_value : ('a -> 'b) -> 'a Memory.value -> 'b Memory.value
val map_logic : ('a -> 'b) -> 'a Memory.logic -> 'b Memory.logic

(** {2 ACSL Utilities} *)

module Logic(M : Memory.Model) :
sig

  open M
  type logic = M.loc Memory.logic

  (** {3 Projections} *)

  val value : logic -> term
  val loc   : logic -> loc
  val vset  : logic -> Vset.set
  val sloc  : logic -> loc sloc list
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

  (** {3 Regions} *)

  type region = loc sloc list

  val separated : (c_object * region) list -> pred
  val included : c_object -> region -> c_object -> region -> pred
  val valid : Sigma.t -> acs -> c_object -> region -> pred

end
