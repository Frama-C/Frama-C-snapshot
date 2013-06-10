(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Program Dependences Graph. *)

(** Functions are registered through the [Db] module, or the dynamic API. *)

(* OCaml 3.12: module type of Marks.F_proj *)

module Register : sig
  (** [stmt] is a call in the [pdg] function.
  * Interprocedural information is provided to know which marks have to be
  * propagatedfrom the called funciton.
  * [in_marks_to_caller] translate this [info_caller_inputs]
  * into a (node, mark) list where the marks are filtered by a  m2m function.
  * Ths result is added to the [rqs] list which is empty by default. *)
  val in_marks_to_caller :
    PdgTypes.Pdg.t -> Cil_types.stmt ->
    ('mark PdgMarks.m2m) ->
    ?rqs:('mark PdgMarks.select) ->
    'mark PdgMarks.info_caller_inputs ->
    'mark PdgMarks.select

  (** similar to [in_marks_to_caller] except that it is done
  * for every callers of the function. *)
  val translate_in_marks :
      PdgTypes.Pdg.t -> 'mark PdgMarks.info_caller_inputs
     -> ?m2m:'mark PdgMarks.call_m2m
     -> 'mark PdgMarks.pdg_select
     -> 'mark PdgMarks.pdg_select

  (** similar to [in_marks_to_caller] except that it is for the outputs
  * of a function propagated into its calls *)
  val call_out_marks_to_called : PdgTypes.Pdg.t -> 'mark PdgMarks.m2m ->
    ?rqs:('mark PdgMarks.select) ->
    (PdgIndex.Signature.out_key * 'mark) list ->
    'mark PdgMarks.select

  (** translate all the interprocedural information returned by a propagation in
 * a function the (node, mark) list of both callers and called function. *)
  val translate_marks_to_prop : Db.Pdg.t -> 'mark PdgMarks.info_inter
     -> ?in_m2m:'mark PdgMarks.call_m2m
     -> ?out_m2m:'mark PdgMarks.call_m2m
     -> 'mark PdgMarks.pdg_select
     -> 'mark PdgMarks.pdg_select

  (** Full backward interprocedural propagation.
  * Can be configured using the funtor parameter.
  * Used for instance in [Sparecode]. *)
  module F_Proj (C : PdgMarks.Config) : PdgMarks.Proj
         with type mark = C.M.t
          and type call_info = C.M.call_info
end
