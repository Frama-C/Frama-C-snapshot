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

(* $Id: Pdg.mli,v 1.9 2008-04-01 09:25:21 uid568 Exp $ *)

(** Program Dependences Graph. *)


module Register : sig
  (** [stmt] is a call in the [pdg] function.
  * Interprocedural information is provided to know which marks have to be
  * propagatedfrom the called funciton.
  * [in_marks_to_caller] translate this [t_info_caller_inputs]
  * into a (node, mark) list where the marks are filtered by a  m2m function.
  * Ths result is added to the [rqs] list which is empty by default. *)
  val in_marks_to_caller :
    PdgTypes.Pdg.t -> Cil_types.stmt ->
    ('t_mark PdgMarks.t_m2m) ->
    ?rqs:('t_mark PdgMarks.t_select) ->
    't_mark PdgMarks.t_info_caller_inputs ->
    't_mark PdgMarks.t_select

  (** similar to [in_marks_to_caller] except that it is done
  * for every callers of the function. *)
  val translate_in_marks :
      PdgTypes.Pdg.t -> 't_mark PdgMarks.t_info_caller_inputs
     -> ?m2m:'t_mark PdgMarks.t_call_m2m
     -> 't_mark PdgMarks.t_pdg_select
     -> 't_mark PdgMarks.t_pdg_select

  (** similar to [in_marks_to_caller] except that it is for the outputs
  * of a function propagated into its calls *)
  val call_out_marks_to_called : PdgTypes.Pdg.t -> 't_mark PdgMarks.t_m2m ->
    ?rqs:('t_mark PdgMarks.t_select) ->
    (PdgIndex.Signature.t_out_key * 't_mark) list ->
    't_mark PdgMarks.t_select

  (** translate all the interprocedural information returned by a propagation in
 * a function the (node, mark) list of both callers and called function. *)
  val translate_marks_to_prop : Db.Pdg.t -> 't_mark PdgMarks.t_info_inter
     -> ?in_m2m:'t_mark PdgMarks.t_call_m2m
     -> ?out_m2m:'t_mark PdgMarks.t_call_m2m
     -> 't_mark PdgMarks.t_pdg_select
     -> 't_mark PdgMarks.t_pdg_select

  (** Full backward interprocedural propagation.
  * Can be configured using the funtor parameter.
  * Used for instance in [Sparecode]. *)
  module F_Proj (C : PdgMarks.T_Config) : PdgMarks.T_Proj
         with type t_mark = C.M.t
          and type t_call_info = C.M.t_call_info
end
