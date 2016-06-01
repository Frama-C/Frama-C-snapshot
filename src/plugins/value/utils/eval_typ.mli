(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

val bitfield_size: typ -> Integer.t option

val cast_lval_if_bitfield : typ -> Int_Base.t -> Cvalue.V.t -> Cvalue.V.t
(** if needed, cast the given abstract value to the given size. Useful
    to handle bitfield. The type given as argument must be the type of
    the l-value the abstract value is written into, which is of size [size]. *)

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

type fct_pointer_compatibility =
  | Compatible
  | Incompatible
  | Incompatible_but_accepted

val compatible_functions:
  typ_pointed:typ -> typ_fun:typ -> fct_pointer_compatibility
(** Test that two functions types are compatible; used to verify that a call
    through a function pointer is ok. In theory, we could only check that
    both types are compatible as defined by C99, 6.2.7. However, some industrial
    codes do not strictly follow the norm, and we must be more lenient.
    Thus, we return [Incompatible_but_accepted] if Value can ignore more or
    less safely the incompatibleness in the types. *)

val resolve_functions :
  typ_pointer:typ -> Cvalue.V.t -> Kernel_function.Hptset.t Eval.or_top * bool
(** given [(funs, warn) = resolve_functions typ v], [funs] is the set of
    functions pointed to by [v] that have a type compatible with [typ].
    Compatibility is interpreted in a relaxed way, using
    {!compatible_functions}. [warn] indicates that at least one value of [v]
    was not a function, or  was a function with a type incompatible with [v];
    for [warn], compatibility is interpreted in a strict way. *)

val expr_contains_volatile: exp -> bool
val lval_contains_volatile: lval -> bool
(** Those two expressions indicate that one l-value contained inside
    the arguments (or the l-value itself for [lval_contains_volatile]) has
    volatile qualifier. Relational analyses should not learn anything on
    such values. *)

