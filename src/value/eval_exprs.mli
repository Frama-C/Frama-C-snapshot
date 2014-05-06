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

(* -------------------------------------------------------------------------- *)
(* --- Evaluation to values                                               --- *)
(* -------------------------------------------------------------------------- *)

val eval_expr :
  with_alarms:CilE.warn_mode -> Cvalue.Model.t -> exp -> Cvalue.V.t

val eval_expr_with_deps_state :
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  exp ->
  Cvalue.Model.t * Zone.t option * Location_Bytes.t

val eval_expr_with_deps_state_subdiv :
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  exp ->
  Cvalue.Model.t * Zone.t option * Location_Bytes.t

val eval_lval :
  conflate_bottom:bool ->
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  lval -> Cvalue.Model.t * Zone.t option * Cvalue.V.t * typ


(* -------------------------------------------------------------------------- *)
(* --- Evaluation to locations                                            --- *)
(* -------------------------------------------------------------------------- *)

val lval_to_loc :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> lval -> location

val lval_to_precise_loc :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> lval -> Precise_locs.precise_location


val lval_to_loc_state :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> lval -> Cvalue.Model.t * location * typ

val lval_to_precise_loc_state :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> lval -> Cvalue.Model.t * Precise_locs.precise_location * typ


val lval_to_loc_deps_state :
  with_alarms:CilE.warn_mode ->
  deps:Zone.t option ->
  Cvalue.Model.t ->
  reduce_valid_index:Kernel.SafeArrays.t ->
  lval ->
  Cvalue.Model.t * Zone.t option * location * typ

val lval_to_precise_loc_deps_state :
  with_alarms:CilE.warn_mode ->
  deps:Zone.t option ->
  Cvalue.Model.t ->
  reduce_valid_index:Kernel.SafeArrays.t ->
  lval ->
  Cvalue.Model.t * Zone.t option * Precise_locs.precise_location * typ


(* -------------------------------------------------------------------------- *)
(* --- Reduction                                                          --- *)
(* -------------------------------------------------------------------------- *)

(** Reduction by operators condition *)

type cond = { exp : exp; positive : bool; }

exception Reduce_to_bottom
val reduce_by_cond : Cvalue.Model.t -> cond -> Cvalue.Model.t
(** Never returns [Model.bottom]. Instead, raises [Reduce_to_bottom] *)

(** Reduction by accesses *)

val reduce_by_valid_loc :
  positive:bool ->
  for_writing:bool -> location -> typ -> Cvalue.Model.t -> Cvalue.Model.t

val reduce_by_accessed_loc : 
  for_writing:bool ->
  Cvalue.Model.t -> Cil_types.lval -> Locations.location ->
  Cvalue.Model.t * Locations.location


(** Misc functions related to reduction *)

exception Cannot_find_lv

val find_lv :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> exp -> lval

val get_influential_vars :
  Cvalue.Model.t -> exp -> location list


(* -------------------------------------------------------------------------- *)
(* --- Alarms and imprecision                                             --- *)
(* -------------------------------------------------------------------------- *)




(* -------------------------------------------------------------------------- *)
(* --- Alarms and reduction                                               --- *)
(* -------------------------------------------------------------------------- *)

val warn_reduce_by_accessed_loc:
 with_alarms:CilE.warn_mode ->
  for_writing:bool ->
  Cvalue.Model.t -> Locations.location -> Cil_types.lval ->
  Cvalue.Model.t * Locations.location

(* -------------------------------------------------------------------------- *)
(* --- Misc                                                               --- *)
(* -------------------------------------------------------------------------- *)

val resolv_func_vinfo :
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  exp -> Kernel_function.Hptset.t * Zone.t option

val offsetmap_of_lv:
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> lval ->
  Precise_locs.precise_location * Cvalue.Model.t * Cvalue.V_Offsetmap.t option


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
