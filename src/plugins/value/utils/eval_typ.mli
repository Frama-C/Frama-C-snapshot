(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Functions related to type conversions *)

(** Bitfields *)
val is_bitfield: typ -> bool

val sizeof_lval_typ: typ -> Int_Base.t
(** Size of the type of a lval, taking into account that the lval might have
    been a bitfield. *)


(** [offsetmap_matches_type t o] returns true if either:
    - [o] contains a single scalar binding, of the expected scalar type [t]
    (float or integer)
    - [o] contains multiple bindings, pointers, etc.
    - [t] is not a scalar type. *)
val offsetmap_matches_type: typ -> Cvalue.V_Offsetmap.t -> bool

val need_cast: typ -> typ -> bool
(** return [true] if the two types are statically distinct, and a cast
    from one to the other may have an effect on an abstract value. *)

(* [compatible_functions typ kfs] filters the list [kfs] to only keep functions
   compatible with the type [typ]. The returned boolean is true if some
   functions were incompatible. If a list of arguments [args] is provided, also
   removes functions incompatible with them. Used to verify a call through a
   function pointer is ok.
   In theory, we could only check that both types are compatible as defined by
   C99, 6.2.7. However, some industrial codes do not strictly follow the norm,
   and we must be more lenient. Thus, some functions are also kept when Eva can
   ignore more or less safely the incompatibility in the types (which is however
   reported in the returned boolean). *)
val compatible_functions:
  typ -> ?args:exp list -> Kernel_function.t list ->
  Kernel_function.t list * bool

val expr_contains_volatile: exp -> bool
val lval_contains_volatile: lval -> bool
(** Those two expressions indicate that one l-value contained inside
    the arguments (and the l-value itself for [lval_contains_volatile]) has
    volatile qualifier. Relational analyses should not learn anything on
    such values. *)

(** Abstraction of an integer type, more convenient than an [ikind] because
    it can also be used for bitfields. *)
type integer_range = { i_bits: int; i_signed: bool }

module DatatypeIntegerRange: Datatype.S with type t = integer_range

val ik_range: ikind -> integer_range
val ik_attrs_range: ikind -> attributes -> integer_range
(** Range for an integer type with some attributes. The attribute
    {!Cil.bitfield_attribute_name} influences the width of the type. *)

val range_inclusion: integer_range -> integer_range -> bool
(** Checks inclusion of two integer ranges. *)

val range_lower_bound: integer_range -> Integer.t
val range_upper_bound: integer_range -> Integer.t

(** Abstraction of scalar types -- in particular, all those that can be involved
    in a cast. Enum and integers are coalesced. *)
type scalar_typ =
  | TSInt of integer_range
  | TSPtr of integer_range
  | TSFloat of fkind

(* Classifies a cil type as a scalar type; returns None for non-scalar types. *)
val classify_as_scalar: typ -> scalar_typ option
