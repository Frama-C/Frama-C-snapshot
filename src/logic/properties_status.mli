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

(** Status of properties.
    @since Boron-20100401 *)

open Cil_types
open Db_types

val get_all:
  ?who:State.t list -> Property.t -> annotation_status list
  (** For a given annotation, get all the status set by each plug-in.
      @since Carbon-20101201 *)

val strongest: Property.t -> annotation_status * State.t option
  (** Checks status consistency according to the following
      partial order: [Unknown < Maybe < True] and [Maybe < False]
      @return the most precise status available for the
      property according to the above partial order.
      In case of consistent multiple status, the most recent
      is returned.
      The returned state is the one associated with the returned status itself.
      @since Carbon-20101201 *)

val get_state : Property.t -> State.t -> State.t
  (** Get the state associated to the given key and state.
      @raise Not_found if there is no such binding
      @since Carbon-20101201 *)

val pretty_all: Format.formatter -> Property.t -> unit
  (** Pretty print all the status of a given annotation.
      @since Carbon-20101201 *)

module Consolidation_tree : sig

  type 'a value = private
      { value: 'a;
	hypothesis: forest;
	dependencies: State.t value list }

  and t = private
      { property: Property.t;
	state: State.t;
	mutable status: (annotation_status * State.t) value list }

  and forest = t list

  val get_all: unit -> forest
  val get: Property.t -> t

  type vertex =
    | Property of t
    | State of State.t value
    | Status of (annotation_status * State.t) value

  val state_of_vertex: vertex -> State.t

  type edge = And | Or

  module G: Graph.Sig.G with type V.t = vertex
			and type E.label = edge
			and type E.t = vertex * edge * vertex
  val get_full_graph: unit -> G.t
  val get_graph: Property.t -> G.t

  val dump: G.t -> string -> unit

end

(** Apply this functor in order to be able to modify status of annotations
    within a plug-in.
    @since Carbon-20101201 *)
module Make_updater
  (P: sig
     val name: string (** Plug-in name. *)
     val emitter: State.t
   end) :
sig

  val set:
    Property.t -> Property.t list -> annotation_status ->
    unit
    (** Set the status of an annotation.
        Do not reset the dependencies of this status.
	@since Carbon-20101201 *)

  val update:
    Property.t ->
    Property.t list ->
    (annotation_status -> annotation_status) ->
    annotation_status
      (** Update the status of a given annotation according to the old
	  status. Do not reset the dependencies of this status.
	  @since Carbon-20101201 *)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
