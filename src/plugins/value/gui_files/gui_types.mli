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

type gui_callstack =
  | GC_Filtered
  | GC_Consolidated
  | GC_Single of Value_types.callstack
  | GC_Callstack of Value_types.callstack

val hash_gui_callstack : gui_callstack -> int
val compare_gui_callstack : gui_callstack -> gui_callstack -> int

module GCallstackMap :  FCMap.S with type key = gui_callstack


type gui_selection =
  | GS_TLVal of Cil_types.term
  | GS_LVal of Cil_types.lval
  | GS_AbsoluteMem
  | GS_Expr of Cil_types.exp
  | GS_Term of Cil_types.term
  | GS_Predicate of Cil_types.predicate

val pretty_gui_selection : Format.formatter -> gui_selection -> unit
val gui_selection_equal : gui_selection -> gui_selection -> bool


type gui_offsetmap_res =
  | GO_Bottom
  | GO_Empty
  | GO_Top
  | GO_InvalidLoc
  | GO_Offsetmap of Cvalue.V_Offsetmap.t

val equal_gui_offsetmap_res :
  gui_offsetmap_res -> gui_offsetmap_res -> bool
val pretty_gui_offsetmap_res :
  ?typ:Cil_types.typ -> Format.formatter -> gui_offsetmap_res -> unit
val join_gui_offsetmap_res :
  gui_offsetmap_res -> gui_offsetmap_res -> gui_offsetmap_res


type gui_loc =
  | GL_Stmt of Cil_types.kernel_function * Cil_types.stmt
  | GL_Pre of Cil_types.kernel_function
  | GL_Post of Cil_types.kernel_function

val gui_loc_equal : gui_loc -> gui_loc -> bool
val gui_loc_loc : gui_loc -> Cil_types.location
val kf_of_gui_loc : gui_loc -> Cil_types.kernel_function

val pretty_callstack :
  Format.formatter -> Value_types.callstack -> unit
val pretty_callstack_short :
  Format.formatter -> Value_types.callstack -> unit

type 'a gui_res =
  | GR_Empty
  | GR_Offsm of gui_offsetmap_res * Cil_types.typ option
  | GR_Value of 'a Eval.flagged_value * Cil_types.typ option
  | GR_Status of Eval_terms.predicate_status
  | GR_Zone of Locations.Zone.t

type 'a gui_after =
  | GA_After of 'a gui_res
  | GA_Bottom
  | GA_NA
  | GA_Unchanged


module type S = sig
  type value

  val pretty_gui_res : Format.formatter -> value gui_res -> unit
  val equal_gui_res : value gui_res -> value gui_res -> bool
  val vars_in_gui_res : value gui_res -> Cil_types.varinfo list

  val pretty_gui_after : Format.formatter -> value gui_after -> unit
  val equal_gui_after : value gui_after -> value gui_after -> bool
end

(** The types below depend on the abstract values currently available. *)
module Make (V : Abstractions.Value) : sig
  include S with type value := V.t

  val get_cvalue : (V.t -> Main_values.CVal.t) option
  val from_cvalue : Main_values.CVal.t -> V.t
end
