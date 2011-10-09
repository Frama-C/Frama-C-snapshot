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


type t_fct_slice = SlicingInternals.t_fct_slice
type t_mark = SlicingInternals.t_pdg_mark
type t_crit = SlicingInternals.t_criterion

(**
* @raise SlicingTypes.ExternalFunction if the function has no source code,
*        because there cannot be any slice for it.
   * @raise SlicingTypes.NoPdg when there is no PDG for the function.
*)
val make_new_ff : SlicingInternals.t_fct_info -> bool ->
                  t_fct_slice * t_crit list

val merge_slices : t_fct_slice -> t_fct_slice ->
                   t_fct_slice * t_crit list

val copy_slice : t_fct_slice -> t_fct_slice

val filter_already_in : t_fct_slice ->
                      SlicingInternals.t_fct_base_criterion ->
                      SlicingInternals.t_fct_base_criterion

val apply_add_marks :
  t_fct_slice -> SlicingInternals.t_fct_base_criterion ->
    t_crit list

val add_marks_to_fi :
                     SlicingInternals.t_project  ->
                     SlicingInternals.t_fct_info  ->
                     SlicingInternals.t_fct_base_criterion ->
                     bool -> t_crit list ->
                     bool * t_crit list

val add_top_mark_to_fi :
                     SlicingInternals.t_fct_info  ->
                     SlicingInternals.t_pdg_mark ->
                     bool -> t_crit list ->
                     t_crit list

val check_outputs_before_change_call :  SlicingInternals.t_project ->
                  t_fct_slice -> SlicingInternals.t_call_id ->
                  t_fct_slice -> t_crit list

val apply_change_call : SlicingInternals.t_project ->
                  t_fct_slice -> SlicingInternals.t_call_id ->
                  SlicingInternals.t_called_fct ->
                  t_crit list

val apply_choose_call : SlicingInternals.t_project ->
                  t_fct_slice -> SlicingInternals.t_call_id ->
                  t_crit list

val apply_missing_inputs : SlicingInternals.t_project ->
                  t_fct_slice -> SlicingInternals.t_call_id ->
                  (SlicingInternals.t_fct_base_criterion * bool) ->
                  t_crit list

val apply_missing_outputs : SlicingInternals.t_project ->
                  t_fct_slice -> SlicingInternals.t_call_id ->
                  SlicingInternals.t_fct_base_criterion -> bool ->
                  t_crit list

val apply_examine_calls : t_fct_slice ->
                          t_mark PdgMarks.t_info_called_outputs ->
                  t_crit list

val get_called_slice :
  t_fct_slice -> SlicingInternals.t_call_id -> (t_fct_slice option * bool)

val get_node_mark : t_fct_slice -> PdgTypes.Node.t -> t_mark
val get_node_key_mark : t_fct_slice -> PdgIndex.Key.t -> t_mark

val get_top_input_mark : SlicingInternals.t_fct_info -> t_mark
val get_stmt_mark : t_fct_slice -> Cil_types.stmt -> t_mark
val get_label_mark : t_fct_slice -> Cil_types.stmt -> Cil_types.label -> t_mark
val get_param_mark : t_fct_slice -> int -> t_mark
val get_local_var_mark : t_fct_slice -> Cil_types.varinfo -> t_mark
val get_input_loc_under_mark : t_fct_slice -> Locations.Zone.t -> t_mark

val get_mark_from_src_fun : SlicingInternals.t_project -> Kernel_function.t -> t_mark

val merge_inputs_m1_mark : t_fct_slice -> t_mark

val clear_ff : SlicingInternals.t_project -> t_fct_slice -> unit

val print_ff_sig : Format.formatter -> t_fct_slice -> unit
