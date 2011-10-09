(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

exception Reduce_to_bottom
exception Cannot_find_lv
exception Too_linear
exception Leaf

type cond = { exp : exp; positive : bool; }

val check_comparable :
  binop ->
  Location_Bytes.t ->
  Location_Bytes.t ->
  bool * Location_Bytes.t * Location_Bytes.t

val do_cast :
  with_alarms:CilE.warn_mode -> typ -> Cvalue.V.t -> Cvalue.V.t


val eval_binop_float :
  with_alarms:CilE.warn_mode ->
  Cvalue.V.t -> binop -> Cvalue.V.t -> Cvalue.V.t
val eval_binop_int :
  with_alarms:CilE.warn_mode ->
  ?typ:typ ->
  te1:typ ->
  Cvalue.V.t -> binop -> Cvalue.V.t -> Cvalue.V.t

val eval_unop:
  with_alarms:CilE.warn_mode ->
  Cvalue.V.t ->
  typ (** Type of the expression under the unop *) ->
  Cil_types.unop -> Cvalue.V.t


val is_bitfield :
  lval -> ?sizelv:Int_Base.t -> ?sizebf:Int_Base.t -> unit -> bool

val lval_to_loc :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> lval -> location
val exp_lval_to_loc :
  Cvalue.Model.t -> exp -> lval * location
val lval_to_loc_deps_option :
  with_alarms:CilE.warn_mode ->
  deps:Zone.t option ->
  Cvalue.Model.t ->
  reduce_valid_index:Kernel.SafeArrays.t ->
  lval ->
  Cvalue.Model.t * Zone.t option * location
val lval_to_loc_with_offset_deps_only :
  deps:Zone.t ->
  Cvalue.Model.t ->
  lval ->
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t * Zone.t option * location
val lval_to_loc_with_deps :
  deps:Zone.t ->
  Cvalue.Model.t ->
  lval ->
  with_alarms:CilE.warn_mode ->
  reduce_valid_index:Kernel.SafeArrays.t ->
  Cvalue.Model.t * Zone.t option * location
val lval_to_loc_with_offset_deps_only_option :
  with_alarms:CilE.warn_mode ->
  deps:Zone.t option ->
  Cvalue.Model.t ->
  lval ->
  Cvalue.Model.t * Zone.t option * location
val pass_cast :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> exn -> typ -> exp -> unit
val find_lv :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> exp -> lval
val find_lv_plus :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> exp -> (lval * Ival.t) list
val base_to_loc :
  with_alarms:CilE.warn_mode ->
  ?deps:Zone.t ->
  Cvalue.Model.t ->
  lval ->
  lhost ->
  Ival.t -> Cvalue.Model.t * Zone.t option * location
val eval_expr :
  with_alarms:CilE.warn_mode -> Cvalue.Model.t -> exp -> Cvalue.V.t
val get_influential_vars :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t -> exp -> location list
val reduce_by_valid_expr :
  positive:bool ->
  for_writing:bool -> exp -> Cvalue.Model.t -> Cvalue.Model.t
val reduce_by_valid_loc :
  positive:bool ->
  for_writing:bool -> location -> typ -> Cvalue.Model.t -> Cvalue.Model.t
val reduce_by_initialized_loc :
  with_alarms:'a ->
  positive:bool ->
  typ * Location_Bytes.t ->
  Cvalue.Model.t -> Cvalue.Model.t
val eval_BinOp :
  with_alarms:CilE.warn_mode ->
  exp ->
  Zone.t option ->
  Cvalue.Model.t -> Cvalue.Model.t * Zone.t option * Cvalue.V.t
val eval_expr_with_deps :
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t -> exp -> Zone.t option * Cvalue.V.t
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
val cast_lval_bitfield :
  lval -> Abstract_interp.Int.t -> Cvalue.V.t -> Cvalue.V.t
val cast_lval_when_bitfield :
  lval ->
  ?sizelv:Int_Base.t -> ?sizebf:Int_Base.t -> Cvalue.V.t -> Cvalue.V.t
val eval_lval_using_main_memory :
  conflate_bottom:bool ->
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  lval -> Cvalue.Model.t * Zone.t option * Cvalue.V.t
val eval_lval :
  conflate_bottom:bool ->
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  lval -> Cvalue.Model.t * Zone.t option * Cvalue.V.t
val eval_offset :
  reduce_valid_index:bool ->
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  typ ->
  Cvalue.Model.t ->
  offset -> Cvalue.Model.t * Zone.t option * Ival.t
val eval_as_exact_loc :
  with_alarms:CilE.warn_mode ->
  Cvalue.Model.t ->
  exp -> location * Cvalue.V.t * typ
type eval_int_float = {
  eval_symetric :
    bool -> binop -> Cvalue.V.t -> Cvalue.V.t -> Cvalue.V.t;
  eval_antisymetric :
    typ_loc:typ ->
    bool -> binop -> Cvalue.V.t -> Cvalue.V.t -> Cvalue.V.t;
}
val eval_int : eval_int_float
val eval_float : bool -> eval_int_float
val eval_from_type : typ -> bool -> eval_int_float
val reduce_by_comparison :
  with_alarms:CilE.warn_mode ->
  eval_int_float ->
  bool ->
  exp ->
  binop -> exp -> Cvalue.Model.t -> Cvalue.Model.t
val reduce_by_cond :
  with_alarms:CilE.warn_mode -> Cvalue.Model.t -> cond -> Cvalue.Model.t
val resolv_func_vinfo :
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  exp -> Kernel_function.Hptset.t * Zone.t option

