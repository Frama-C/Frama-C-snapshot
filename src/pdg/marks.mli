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

open PdgMarks

(** [in_marks_to_caller] translate the input information part returned by
    [mark_and_propagate] into [(node, mark) list] related to a call.
    Example : if marks has been propagated in [f] and some input marks has
    changed, they have to be propagated into [f] callers.
    So this function takes one call to [f] and translate input keys into nodes.

    The function ([m2m]) is called for each element to translate.
    See {!m2m} for more information about how to use it. *)
val in_marks_to_caller :
  PdgTypes.Pdg.t ->
  Cil_types.stmt ->
  'mark m2m ->
  ?rqs:('mark select) ->
  'mark info_caller_inputs ->
  'mark select

(** translate the input information part returned by [mark_and_propagate]
    using [in_marks_to_caller] for each call. (see above) *)
val translate_in_marks :
  PdgTypes.Pdg.t->
  'mark info_caller_inputs->
  ?m2m:('mark call_m2m) ->
  'mark pdg_select ->
  'mark pdg_select

(** we have a list of a call output marks, and we want to translate it
    into a list of marks on the called function nodes.
    The pdg is the called_pdg. *)
val call_out_marks_to_called :
  PdgTypes.Pdg.t ->
  'mark m2m ->
  ?rqs:('mark select) ->
  (PdgIndex.Signature.out_key * 'mark) list ->
  'mark select

(** use both [translate_in_marks] and [call_out_marks_to_called]
    to translate the information provided by [mark_and_propagate]
    info selection on other functions. *)
val translate_marks_to_prop :
  PdgTypes.Pdg.t ->
  'mark info_inter ->
  ?in_m2m:('mark call_m2m) ->
  ?out_m2m:('mark call_m2m) ->
  'mark pdg_select ->
  'mark pdg_select

module F_Proj (C : Config) :
  Proj with type mark = C.M.t
       and type call_info = C.M.call_info

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
