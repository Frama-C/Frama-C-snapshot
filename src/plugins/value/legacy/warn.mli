(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Alarms and imprecision warnings emitted during the analysis. *)

open CilE
open Cil_types
open Locations

val warn_div : warn_mode -> addresses:bool -> unit
(** division. If [addresses] holds, also emit an alarm about the denominator
    not being comparable to \null. *)

val warn_shift : warn_mode -> unit
val warn_integer_overflow : warn_mode -> unit
val warn_float_to_int_overflow: warn_mode -> unit
val warn_pointer_subtraction : warn_mode -> unit
val warn_pointer_comparison : Cil_types.typ -> warn_mode -> unit
(** warn on invalid pointer comparison. The first argument is the type
    of the arguments of the comparison *)
val warn_nan_infinite: warn_mode -> unit
val warn_uninitialized : warn_mode -> unit
val warn_escapingaddr : warn_mode -> unit
val warn_mem: warn_mode -> unit


val warn_imprecise_lval_read:
  with_alarms:CilE.warn_mode ->
  lval -> location -> Location_Bytes.t -> unit

val warn_locals_escape:
  bool -> fundec -> Base.t -> Base.SetLattice.t -> unit

val warn_right_exp_imprecision:
  with_alarms:CilE.warn_mode ->
  Cil_types.lval -> Locations.location -> Cvalue.V.t -> unit

val warn_float:
  with_alarms:CilE.warn_mode ->
  ?non_finite:bool -> ?addr:bool ->
  unit -> unit

val warn_top: unit -> 'a
(** Abort the analysis, signaling that Top has been found. (Should not
    actually appear. No operation should produce Top, or those operations
    should be abstracted unsoundly.) *)
