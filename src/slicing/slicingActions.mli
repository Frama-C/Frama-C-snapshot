(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type t_mark = SlicingMarks.t_mark
type t_select = t_mark PdgMarks.t_select

                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** selection mode (ie which mark to associate to the node
* and how to propagate in the different kinds of dependencies) *)
type t_n_or_d_marks

val build_simple_node_selection :
  ?nd_marks:t_n_or_d_marks -> t_mark -> t_n_or_d_marks
val build_addr_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> t_mark -> t_n_or_d_marks
val build_data_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> t_mark -> t_n_or_d_marks
val build_ctrl_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> t_mark -> t_n_or_d_marks
val build_node_and_dpds_selection :
  ?nd_marks:t_n_or_d_marks -> t_mark -> t_n_or_d_marks
                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
val translate_crit_to_select :
  Db.Pdg.t -> ?to_select:t_select ->
  ((Db.Pdg.t_node * Locations.Zone.t option) list * t_n_or_d_marks) list
  -> t_select
                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
type t_fct_info = SlicingInternals.t_fct_info
type t_fct_slice = SlicingInternals.t_fct_slice
type t_call_id = SlicingInternals.t_call_id
type t_fct_crit = SlicingInternals.t_fct_crit
type t_criterion = SlicingInternals.t_criterion

val mk_fct_crit : t_fct_info -> t_fct_crit -> t_criterion
val mk_crit_fct_user_select : t_fct_info -> t_select -> t_criterion
val mk_crit_fct_top : t_fct_info -> t_mark -> t_criterion
val mk_crit_prop_persit_marks : t_fct_info -> t_select -> t_criterion
val mk_ff_user_select : t_fct_slice -> t_select -> t_criterion
val mk_crit_choose_call : t_fct_slice -> t_call_id -> t_criterion
val mk_crit_change_call :
  t_fct_slice -> t_call_id -> SlicingInternals.t_called_fct -> t_criterion
val mk_crit_missing_inputs :
  t_fct_slice -> t_call_id -> t_select * bool -> t_criterion
val mk_crit_missing_outputs :
  t_fct_slice -> t_call_id -> t_select * bool -> t_criterion
val mk_crit_examines_calls :
  t_fct_slice -> t_mark PdgMarks.t_info_called_outputs -> t_criterion
val mk_appli_select_calls : t_fct_info -> t_criterion
val mk_crit_mark_calls :
  t_fct_info -> Cil_types.kernel_function -> t_mark -> t_criterion
val mk_crit_add_output_marks :
  t_fct_slice -> t_select -> t_criterion
                    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
val print_nd_and_mark_list :
  Format.formatter -> t_n_or_d_marks -> unit
val print_nodes : Format.formatter -> Db.Pdg.t_node list -> unit
val print_sel_marks_list : Format.formatter -> t_select -> unit
val print_crit : Format.formatter -> t_criterion -> unit
val print_f_crit : Format.formatter -> SlicingInternals.t_fct_user_crit -> unit
val print_list_crit : Format.formatter -> t_criterion list -> unit
