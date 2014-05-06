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

open SlicingTypes
open Cil_types
open SlicingInternals

type select = sl_mark PdgMarks.select


(** selection mode (ie which mark to associate to the node
    and how to propagate in the different kinds of dependencies) *)
type n_or_d_marks

val build_simple_node_selection :
  ?nd_marks:n_or_d_marks -> sl_mark -> n_or_d_marks
val build_addr_dpds_selection :
  ?nd_marks:n_or_d_marks -> sl_mark -> n_or_d_marks
val build_data_dpds_selection :
  ?nd_marks:n_or_d_marks -> sl_mark -> n_or_d_marks
val build_ctrl_dpds_selection :
  ?nd_marks:n_or_d_marks -> sl_mark -> n_or_d_marks
val build_node_and_dpds_selection :
  ?nd_marks:n_or_d_marks -> sl_mark -> n_or_d_marks

val translate_crit_to_select :
  Db.Pdg.t -> ?to_select:select ->
  ((PdgTypes.Node.t * Locations.Zone.t option) list * n_or_d_marks) list
  -> select

val mk_fct_crit : fct_info -> fct_crit -> criterion
val mk_crit_fct_user_select : fct_info -> select -> criterion
val mk_crit_fct_top : fct_info -> sl_mark -> criterion
val mk_crit_prop_persit_marks : fct_info -> select -> criterion
val mk_ff_user_select : fct_slice -> select -> criterion
val mk_crit_choose_call : fct_slice -> stmt -> criterion
val mk_crit_change_call : fct_slice -> stmt -> called_fct -> criterion
val mk_crit_missing_inputs : fct_slice -> stmt -> select * bool -> criterion
val mk_crit_missing_outputs : fct_slice -> stmt -> select * bool -> criterion
val mk_crit_examines_calls :
  fct_slice -> sl_mark PdgMarks.info_called_outputs -> criterion
val mk_appli_select_calls : fct_info -> criterion
val mk_crit_mark_calls : fct_info -> kernel_function -> sl_mark -> criterion
val mk_crit_add_output_marks : fct_slice -> select -> criterion

(** Printing *)
val print_nd_and_mark_list : Format.formatter -> n_or_d_marks -> unit
val print_nodes : Format.formatter -> PdgTypes.Node.t list -> unit
val print_sel_marks_list : Format.formatter -> select -> unit
val print_crit : Format.formatter -> criterion -> unit
val print_f_crit : Format.formatter -> fct_user_crit -> unit
val print_list_crit : Format.formatter -> criterion list -> unit
