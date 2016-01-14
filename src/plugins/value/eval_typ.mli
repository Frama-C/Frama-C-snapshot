(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** This module contains functions related to type conversions *)

(** Bitfields *)
val is_bitfield: typ -> bool

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
