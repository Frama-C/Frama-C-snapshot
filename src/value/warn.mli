(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
open Locations


val check_not_comparable :
  binop -> Location_Bytes.t -> Location_Bytes.t -> bool

val check_no_recursive_call: kernel_function -> bool

(** This function should be used to treat a call [lv = kf(...)].
    [warn_modified_result_loc alarms loc state lv] checks that evaluating [lv]
    in [state] results in [location]. If it is not the case, a warning about
    a modification of [lv] during the call to [kf] is emitted *)
val warn_modified_result_loc:
  with_alarms:CilE.warn_mode ->
  kernel_function -> location -> Cvalue.Model.t -> lval -> unit

val warn_imprecise_lval_read:
  with_alarms:CilE.warn_mode ->
  lval -> location -> Location_Bytes.t -> unit

val warn_locals_escape:
  bool -> fundec -> Base.t -> Base.SetLattice.t -> unit

val warn_locals_escape_result:
  fundec -> Base.SetLattice.t -> unit

val warn_right_exp_imprecision:
  with_alarms:CilE.warn_mode ->
  Cil_types.lval -> Locations.location -> Cvalue.V.t -> unit

val warn_overlap:
  with_alarms:CilE.warn_mode ->
  lval * Locations.location -> lval * Locations.location -> unit

val warn_float:
  with_alarms:CilE.warn_mode ->
  ?overflow:bool -> ?addr:bool ->
  fkind option -> (Format.formatter -> unit) ->
  unit

val warn_float_addr:
  with_alarms:CilE.warn_mode -> (Format.formatter -> unit) -> unit

(** Returns the first eventual imprecise part contained in an offsetmap *)
val offsetmap_contains_imprecision:
  Cvalue.V_Offsetmap.t -> Cvalue.V.t option

(** If the supplied offsetmap has an arithmetic type and contains indeterminate
    bits (uninitialized, or escaping address), raises the corresponding alarm(s)
    and returns the reduced offsetmap.
    The syntactic context must have been positioned by the caller. If
    some bits are guaranteed to be indeterminate, returns [None]; this indicates
    completely erroneous code. *)
val warn_indeterminate_offsetmap:
  with_alarms:CilE.warn_mode ->
  typ -> Cvalue.V_Offsetmap.t -> Cvalue.V_Offsetmap.t option
