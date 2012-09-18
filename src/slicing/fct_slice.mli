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


(**
* @raise SlicingTypes.ExternalFunction if the function has no source code,
*        because there cannot be any slice for it.
   * @raise SlicingTypes.NoPdg when there is no PDG for the function.
*)
val make_new_ff : SlicingInternals.fct_info -> bool ->
                  SlicingInternals.t_fct_slice * SlicingInternals.t_criterion list

val merge_slices : SlicingInternals.t_fct_slice -> SlicingInternals.t_fct_slice ->
                   SlicingInternals.t_fct_slice * SlicingInternals.t_criterion list

val copy_slice : SlicingInternals.t_fct_slice -> SlicingInternals.t_fct_slice

val filter_already_in : SlicingInternals.t_fct_slice ->
                      SlicingInternals.t_fct_base_criterion ->
                      SlicingInternals.t_fct_base_criterion

val apply_add_marks :
  SlicingInternals.t_fct_slice -> SlicingInternals.t_fct_base_criterion ->
    SlicingInternals.t_criterion list

val add_marks_to_fi :
                     SlicingInternals.t_project  ->
                     SlicingInternals.fct_info  ->
                     SlicingInternals.t_fct_base_criterion ->
                     bool -> SlicingInternals.t_criterion list ->
                     bool * SlicingInternals.t_criterion list

val add_top_mark_to_fi :
                     SlicingInternals.fct_info  ->
                     SlicingInternals.pdg_mark ->
                     bool -> SlicingInternals.t_criterion list ->
                     SlicingInternals.t_criterion list

val check_outputs_before_change_call :  SlicingInternals.t_project ->
                  SlicingInternals.t_fct_slice -> Cil_types.stmt ->
                  SlicingInternals.t_fct_slice -> SlicingInternals.t_criterion list

val apply_change_call : SlicingInternals.t_project ->
                  SlicingInternals.t_fct_slice -> Cil_types.stmt ->
                  SlicingInternals.t_called_fct ->
                  SlicingInternals.t_criterion list

val apply_choose_call : SlicingInternals.t_project ->
                  SlicingInternals.t_fct_slice -> Cil_types.stmt ->
                  SlicingInternals.t_criterion list

val apply_missing_inputs : SlicingInternals.t_project ->
                  SlicingInternals.t_fct_slice -> Cil_types.stmt ->
                  (SlicingInternals.t_fct_base_criterion * bool) ->
                  SlicingInternals.t_criterion list

val apply_missing_outputs : SlicingInternals.t_project ->
                  SlicingInternals.t_fct_slice -> Cil_types.stmt ->
                  SlicingInternals.t_fct_base_criterion -> bool ->
                  SlicingInternals.t_criterion list

val apply_examine_calls : SlicingInternals.t_fct_slice ->
                          SlicingInternals.pdg_mark PdgMarks.t_info_called_outputs ->
                  SlicingInternals.t_criterion list

val get_called_slice :
  SlicingInternals.t_fct_slice -> Cil_types.stmt -> (SlicingInternals.t_fct_slice option * bool)

val get_node_mark : SlicingInternals.t_fct_slice -> PdgTypes.Node.t -> SlicingInternals.pdg_mark
val get_node_key_mark : SlicingInternals.t_fct_slice -> PdgIndex.Key.t -> SlicingInternals.pdg_mark

val get_top_input_mark : SlicingInternals.fct_info -> SlicingInternals.pdg_mark
val get_stmt_mark : SlicingInternals.t_fct_slice -> Cil_types.stmt -> SlicingInternals.pdg_mark
val get_label_mark : SlicingInternals.t_fct_slice -> Cil_types.stmt -> Cil_types.label -> SlicingInternals.pdg_mark
val get_param_mark : SlicingInternals.t_fct_slice -> int -> SlicingInternals.pdg_mark
val get_local_var_mark : SlicingInternals.t_fct_slice -> Cil_types.varinfo -> SlicingInternals.pdg_mark
val get_input_loc_under_mark : SlicingInternals.t_fct_slice -> Locations.Zone.t -> SlicingInternals.pdg_mark

val get_mark_from_src_fun : SlicingInternals.t_project -> Kernel_function.t -> SlicingInternals.pdg_mark

val merge_inputs_m1_mark : SlicingInternals.t_fct_slice -> SlicingInternals.pdg_mark

val clear_ff : SlicingInternals.t_project -> SlicingInternals.t_fct_slice -> unit

val print_ff_sig : Format.formatter -> SlicingInternals.t_fct_slice -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
