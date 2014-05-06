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

open SlicingInternals
open Cil_types

(**
* @raise SlicingTypes.ExternalFunction if the function has no source code,
*        because there cannot be any slice for it.
   * @raise SlicingTypes.NoPdg when there is no PDG for the function.
*)
val make_new_ff : fct_info -> bool -> fct_slice * criterion list

val merge_slices : fct_slice -> fct_slice -> fct_slice * criterion list

val copy_slice : fct_slice -> fct_slice

val filter_already_in : fct_slice -> fct_base_criterion -> fct_base_criterion

val apply_add_marks : fct_slice -> fct_base_criterion -> criterion list

val add_marks_to_fi :
  project -> fct_info -> fct_base_criterion -> bool -> criterion list ->
  bool * criterion list

val add_top_mark_to_fi :
  fct_info -> pdg_mark -> bool -> criterion list -> criterion list

val check_outputs_before_change_call :
  project -> fct_slice -> stmt -> fct_slice -> criterion list

val apply_change_call :
  project -> fct_slice -> stmt -> called_fct -> criterion list

val apply_choose_call : project -> fct_slice -> stmt -> criterion list

val apply_missing_inputs :
  project -> fct_slice -> stmt -> (fct_base_criterion * bool) ->
  criterion list

val apply_missing_outputs :
  project -> fct_slice -> stmt -> fct_base_criterion -> bool ->
  criterion list

val apply_examine_calls :
  fct_slice -> pdg_mark PdgMarks.info_called_outputs -> criterion list

val get_called_slice : fct_slice -> stmt -> (fct_slice option * bool)

val get_node_mark : fct_slice -> PdgTypes.Node.t -> pdg_mark
val get_node_key_mark : fct_slice -> PdgIndex.Key.t -> pdg_mark

val get_top_input_mark : fct_info -> pdg_mark
val get_stmt_mark : fct_slice -> stmt -> pdg_mark
val get_label_mark : fct_slice -> stmt -> label -> pdg_mark
val get_param_mark : fct_slice -> int -> pdg_mark
val get_local_var_mark : fct_slice -> varinfo -> pdg_mark
val get_input_loc_under_mark : fct_slice -> Locations.Zone.t -> pdg_mark

val get_mark_from_src_fun : project -> Kernel_function.t -> pdg_mark

val merge_inputs_m1_mark : fct_slice -> pdg_mark

val clear_ff : project -> fct_slice -> unit

val print_ff_sig : Format.formatter -> fct_slice -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
