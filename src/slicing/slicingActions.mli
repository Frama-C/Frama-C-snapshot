(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

type t_select = SlicingTypes.sl_mark PdgMarks.t_select

                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** selection mode (ie which mark to associate to the node
* and how to propagate in the different kinds of dependencies) *)
type t_n_or_d_marks

val build_simple_node_selection :
  ?nd_marks:t_n_or_d_marks -> SlicingTypes.sl_mark -> t_n_or_d_marks
val build_addr_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> SlicingTypes.sl_mark -> t_n_or_d_marks
val build_data_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> SlicingTypes.sl_mark -> t_n_or_d_marks
val build_ctrl_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> SlicingTypes.sl_mark -> t_n_or_d_marks
val build_node_and_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> SlicingTypes.sl_mark -> t_n_or_d_marks
                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
val translate_crit_to_select :
  Db.Pdg.t -> ?to_select:t_select ->
  ((PdgTypes.Node.t * Locations.Zone.t option) list * t_n_or_d_marks) list
  -> t_select
                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

val mk_fct_crit : SlicingInternals.fct_info -> SlicingInternals.t_fct_crit -> SlicingInternals.t_criterion
val mk_crit_fct_user_select : SlicingInternals.fct_info -> t_select -> SlicingInternals.t_criterion
val mk_crit_fct_top : SlicingInternals.fct_info -> SlicingTypes.sl_mark -> SlicingInternals.t_criterion
val mk_crit_prop_persit_marks : SlicingInternals.fct_info -> t_select -> SlicingInternals.t_criterion
val mk_ff_user_select : SlicingInternals.t_fct_slice -> t_select -> SlicingInternals.t_criterion
val mk_crit_choose_call : SlicingInternals.t_fct_slice -> Cil_types.stmt -> SlicingInternals.t_criterion
val mk_crit_change_call :
  SlicingInternals.t_fct_slice -> Cil_types.stmt -> SlicingInternals.t_called_fct -> SlicingInternals.t_criterion
val mk_crit_missing_inputs :
  SlicingInternals.t_fct_slice -> Cil_types.stmt -> t_select * bool -> SlicingInternals.t_criterion
val mk_crit_missing_outputs :
  SlicingInternals.t_fct_slice -> Cil_types.stmt -> t_select * bool -> SlicingInternals.t_criterion
val mk_crit_examines_calls :
  SlicingInternals.t_fct_slice -> SlicingTypes.sl_mark PdgMarks.t_info_called_outputs -> SlicingInternals.t_criterion
val mk_appli_select_calls : SlicingInternals.fct_info -> SlicingInternals.t_criterion
val mk_crit_mark_calls :
  SlicingInternals.fct_info -> Cil_types.kernel_function -> SlicingTypes.sl_mark -> SlicingInternals.t_criterion
val mk_crit_add_output_marks :
  SlicingInternals.t_fct_slice -> t_select -> SlicingInternals.t_criterion
                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
val print_nd_and_mark_list :
  Format.formatter -> t_n_or_d_marks -> unit
val print_nodes : Format.formatter -> PdgTypes.Node.t list -> unit
val print_sel_marks_list : Format.formatter -> t_select -> unit
val print_crit : Format.formatter -> SlicingInternals.t_criterion -> unit
val print_f_crit : Format.formatter -> SlicingInternals.t_fct_user_crit -> unit
val print_list_crit : Format.formatter -> SlicingInternals.t_criterion list -> unit
