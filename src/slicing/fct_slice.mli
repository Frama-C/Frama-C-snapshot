(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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


type t_fct_slice = SlicingTypes.Internals.t_fct_slice
type t_mark = SlicingTypes.Internals.t_pdg_mark
type t_crit = SlicingTypes.Internals.t_criterion

(**
* @raise SlicingTypes.ExternalFunction if the function has no source code,
*        because there cannot be any slice for it.
   * @raise SlicingTypes.NoPdg when there is no PDG for the function.
*)
val make_new_ff : SlicingTypes.Internals.t_fct_info -> bool -> 
                  t_fct_slice * t_crit list

val merge_slices : t_fct_slice -> t_fct_slice -> 
                   t_fct_slice * t_crit list

val copy_slice : t_fct_slice -> t_fct_slice

val filter_already_in : t_fct_slice ->
                      SlicingTypes.Internals.t_fct_base_criterion ->
                      SlicingTypes.Internals.t_fct_base_criterion

val apply_add_marks : 
  t_fct_slice -> SlicingTypes.Internals.t_fct_base_criterion ->
    t_crit list

val add_marks_to_fi : 
                     SlicingTypes.Internals.t_project  -> 
                     SlicingTypes.Internals.t_fct_info  -> 
                      SlicingTypes.Internals.t_fct_base_criterion ->
                      bool -> t_crit list ->
                      t_crit list

val check_outputs_before_change_call :  SlicingTypes.Internals.t_project ->
                  t_fct_slice -> SlicingTypes.Internals.t_call_id ->
                  t_fct_slice -> t_crit list

val apply_change_call : SlicingTypes.Internals.t_project ->
                  t_fct_slice -> SlicingTypes.Internals.t_call_id ->
                  SlicingTypes.Internals.t_called_fct -> 
                  t_crit list

val apply_choose_call : SlicingTypes.Internals.t_project -> 
                  t_fct_slice -> SlicingTypes.Internals.t_call_id ->
                  t_crit list

val apply_missing_inputs : SlicingTypes.Internals.t_project ->
                  t_fct_slice -> SlicingTypes.Internals.t_call_id ->
                  (SlicingTypes.Internals.t_fct_base_criterion * bool) ->
                  t_crit list
                          
val apply_missing_outputs : SlicingTypes.Internals.t_project ->
                  t_fct_slice -> SlicingTypes.Internals.t_call_id ->
                  SlicingTypes.Internals.t_fct_base_criterion -> bool ->
                  t_crit list

val apply_examine_calls : t_fct_slice ->
                          t_mark PdgMarks.t_info_called_outputs ->
                  t_crit list

val get_called_slice : 
  t_fct_slice -> SlicingTypes.Internals.t_call_id -> (t_fct_slice option * bool)

val get_top_input_mark : SlicingTypes.Internals.t_fct_info -> t_mark
val get_stmt_mark : t_fct_slice -> Cil_types.stmt -> t_mark
val get_label_mark : t_fct_slice -> Cil_types.stmt -> Cil_types.label -> t_mark
val get_param_mark : t_fct_slice -> int -> t_mark
val get_local_var_mark : t_fct_slice -> Cil_types.varinfo -> t_mark
val get_node_mark : t_fct_slice -> PdgTypes.Node.t -> t_mark
val get_input_loc_under_mark : t_fct_slice -> Locations.Zone.t -> t_mark

val merge_inputs_m1_mark : t_fct_slice -> t_mark

val clear_ff : SlicingTypes.Internals.t_project -> t_fct_slice -> unit

val print_ff_sig : Format.formatter -> t_fct_slice -> unit
