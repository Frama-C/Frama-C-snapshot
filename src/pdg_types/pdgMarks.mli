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

(** This module provides elements to mapped information (here called 'marks')
* to PDG elements and propagate it along the dependencies.
*
* Some more functions are defined in the PDG pluggin itself
* (in [pdg/marks]):
* the signatures of these public functions can be found in file [Pdg.mli] *)

(** Signature of the module to use in order to instanciate the computation *)
module type Mark = sig

  (** type of the information mapped to the nodes *)
  type t

  (** type of the information mapped to the function calls.
  * This can be [unit] if there is nothing to store for the calls.
  * (see {!PdgIndex.FctIndex} for more information)
  * *)
  type call_info

  (** used to test [combine] result (see below) *)
  val is_bottom : t -> bool

  (** merge two pieces of information *)
  val merge : t -> t -> t

  (** [combine] is used during propagation. It should return
  * [(new_mark, mark_to_prop) = combine old_mak new_mark]
  * where [new_mark] is the mark to associate with the node,
  * and [mark_to_prop] the mark to propagate to its dependencies.
  * If [is_bottom mark_to_prop], the propagation is stopped.
  * *)
  val combine : t -> t -> t * t

  val pretty : Format.formatter -> t -> unit

end

(** When selecting or propagating marks in a function,
* the marks are most of the time associated to pdg nodes,
* but we also need to associate marks to input locations
* in order to propage information to the callers about undefined data.
* *)
type select_elem = private
  | SelNode of PdgTypes.Node.t * Locations.Zone.t option
  | SelIn of Locations.Zone.t

val mk_select_node :
  ?z_opt:Locations.Zone.t option -> PdgTypes.Node.t -> select_elem
val mk_select_undef_zone : Locations.Zone.t -> select_elem

type 'tm select = (select_elem * 'tm) list

val add_to_select : 'tm select -> select_elem -> 'tm -> 'tm select

val add_node_to_select :
    'tm select -> (PdgTypes.Node.t * Locations.Zone.t option) ->
    'tm -> 'tm select
val add_undef_in_to_select :
       'tm select -> Locations.Zone.t option -> 'tm -> 'tm select

(** we sometime need a list of [t_select] associated with its pdg when dealing
    with several functions at one time. *)
type 'tm pdg_select_info = SelList of  'tm select | SelTopMarks of 'tm list
type 'tm pdg_select = (PdgTypes.Pdg.t * 'tm pdg_select_info) list

(** Represent the information to propagate from a function inputs to its
    calls. Notice that the input keys don't necessarily correspond to nodes
    especially when one want to select a data that is not defined in the
    function. **)
type 'tm info_caller_inputs = (PdgIndex.Signature.in_key * 'tm) list

(** Represent the information to propagate from a call outputs to the called
    function. The [stmt] are the calls to consider. *)
type 'tm info_called_outputs =
    (Cil_types.stmt * (PdgIndex.Signature.out_key * 'tm) list) list

(** when some marks have been propagated in a function, there is some
    information to propagate in the callers and called functions to have an
    interprocedural processing. *)
type 'tm info_inter = 'tm info_caller_inputs * 'tm info_called_outputs

module type Fct = sig

  type mark
  type call_info
  type fi = (mark, call_info) PdgIndex.FctIndex.t
  type t = PdgTypes.Pdg.t * fi

  val create : PdgTypes.Pdg.t -> t
  val get_idx : t -> fi

  type mark_info_inter = mark info_inter

  val empty_to_prop : mark_info_inter

  val mark_and_propagate :
    t -> ?to_prop:mark_info_inter -> mark select -> mark_info_inter

end

module F_Fct(M : Mark) :
  Fct with type mark = M.t and type call_info = M.call_info

type 't_mark m2m =  select_elem -> 't_mark -> 't_mark option

type 't_mark call_m2m =
    Cil_types.stmt option -> PdgTypes.Pdg.t -> 't_mark m2m

(** this is the type of the functor dedicated to interprocedural propagation.
    It is defined in PDG pluggin *)
module type Proj = sig
  type t

  type mark
  type call_info
  type fct = (mark, call_info) PdgIndex.FctIndex.t

  val empty: unit -> t
  val find_marks: t -> Cil_types.varinfo -> fct option
  val mark_and_propagate: t -> PdgTypes.Pdg.t -> mark select -> unit
end

module type Config = sig
  module M : Mark

  (** define how to translate an input mark of a function into a mark
  * to propagate in the callers.
  * The statement specify to which call we are about to propagate,
  * and the pdg is the one of the caller in which the call is.
  * If it returns [None], the propagation is stopped.
  * A simple propagation can be done by returning [Some m].
  * The [call] parameter can be [None] when the caller has a Top PDG.
  * *)
  val mark_to_prop_to_caller_input : M.t call_m2m

  (** define how to translate a mark of a call output into a mark
  * to propagate in the called function.
  * The statement specify from which call we are about to propagate,
  * and the pdg is the one of the called function.
  * *)
  val mark_to_prop_to_called_output : M.t call_m2m

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
