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

  (** [in_marks_to_caller] translate the input information part returned by
  * [mark_and_propagate] into [(node, mark) list] related to a call.
  * Example : if marks has been propagated in [f] and some input marks has
  * changed, they have to be propagated into [f] callers.
  * So this function takes one call to [f] and translate input keys into nodes.
  *
  * The function ([m2m]) is called for each element to translate.
  * See {!PdgMarks.t_m2m} for more information about how to use it.
  * *)
val in_marks_to_caller : PdgTypes.Pdg.t -> Cil_types.stmt ->
  't_mark PdgMarks.t_m2m ->
  ?rqs:('t_mark PdgMarks.t_select) ->
  't_mark PdgMarks.t_info_caller_inputs ->
  't_mark PdgMarks.t_select

(** translate the input information part returned by [mark_and_propagate]
* using [in_marks_to_caller] for each call. (see above)
* *)
val translate_in_marks :
      PdgTypes.Pdg.t
     -> 't_mark PdgMarks.t_info_caller_inputs
     -> ?m2m:('t_mark PdgMarks.t_call_m2m)
     -> 't_mark PdgMarks.t_pdg_select
     -> 't_mark PdgMarks.t_pdg_select

(** we have a list of a call output marks, and we want to translate it
  * into a list of marks on the called function nodes.
  * The pdg is the called_pdg.
  * *)
val call_out_marks_to_called : PdgTypes.Pdg.t ->
  't_mark PdgMarks.t_m2m ->
  ?rqs:('t_mark PdgMarks.t_select) ->
  (PdgIndex.Signature.t_out_key * 't_mark) list ->
  't_mark PdgMarks.t_select

(** use both [translate_in_marks] and [call_out_marks_to_called]
* to translate the information provided by [mark_and_propagate]
* info selection on other functions.
 * *)
val translate_marks_to_prop :
      PdgTypes.Pdg.t -> 't_mark PdgMarks.t_info_inter
     -> ?in_m2m:('t_mark PdgMarks.t_call_m2m)
     -> ?out_m2m:('t_mark PdgMarks.t_call_m2m)
     -> 't_mark PdgMarks.t_pdg_select
     -> 't_mark PdgMarks.t_pdg_select

module F_Proj (C : PdgMarks.T_Config) :
  PdgMarks.T_Proj with type t_mark = C.M.t
         and type t_call_info = C.M.t_call_info

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
