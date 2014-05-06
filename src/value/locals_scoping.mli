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

(** Auxiliary functions to mark invalid (more precisely 'escaping') the
    references to a variable whose scope ends. *)

(** Set of bases that might contain a reference to a local or formal
    variable. Those references must be marked as dangling once we leave
    the scope of those local or formals. *)
type clobbered_set = {
  mutable clob: Base.SetLattice.t
}

val bottom: unit -> clobbered_set
val top: unit -> clobbered_set

val remember_bases_with_locals: clobbered_set -> Base.SetLattice.t -> unit
(** Add the given set of bases to an existing clobbered set *)

val remember_if_locals_in_value:
  clobbered_set -> Locations.location -> Cvalue.V.t -> unit
(** [remember_locals_in_value clob loc v] adds all bases pointed to by [loc]
    to [clob] if [v] contains the address of a local or formal *)

val remember_if_locals_in_offsetmap:
  clobbered_set -> Locations.location -> Cvalue.V_Offsetmap.t -> unit
(** Same as above with an entire offsetmap *)


type topify_offsetmap =
  Cvalue.V_Offsetmap.t ->
  Base.SetLattice.t * Cvalue.V_Offsetmap.t
(** Type of a function that topifies the references to a local in an offsetmap.
    It returns the cleared up offsetmap, and the of variables whose address
    was found *)

type topify_offsetmap_approx =
  exact:bool ->
  topify_offsetmap
(** Type of a function that partially topifies the references to a local in
    an offsetmap. If [exact] is false, references to locals are both kept and
    flagged as being escaping addresses. *)

type topify_state = Cvalue.Model.t -> Cvalue.Model.t
(** Type of a function that topifies a state. Introduced here by symmetry. *)


val offsetmap_top_addresses_of_locals:
  (Base.t -> bool) ->
  topify_offsetmap_approx
(** [offsetmap_top_addresses_of_locals is_local] returns a function that
    topifies all the parts of an offsetmap that contains a pointer verifying
    [is_local]. For efficicent reasons, this function is meant to be partially
    applied to its first argument. *)

val state_top_addresses_of_locals:
  exact:bool ->
  (Base.t -> Base.SetLattice.t -> unit) ->
  topify_offsetmap_approx ->
  clobbered_set ->
  topify_state
(** [state_top_addresses_of_locals exact warn topoffsm clob] generalizes
    [topoffsm] into a function that topifies a memory state. [topoffsm] is
    called only on the offsetmaps bound to the bases in [clob]. The [exact]
    argument is passed to [topoffsm]. If escaping locals [locals] are referenced
    in the offsetmap bound to [b], [warn b locals] is called. *)

val top_addresses_of_locals:
  Cil_types.fundec -> clobbered_set -> topify_offsetmap * topify_state
(** Return two functions that topifies all references to the locals and formals
    of the given function. For memory states, only the offsetmaps bound
    to the variables in the clobbered set are treated. *)

val block_top_addresses_of_locals:
  Cil_types.fundec -> clobbered_set -> Cil_types.block list -> topify_state
(** Return a function that topifies all references to the variables local
    to the given blocks. Only the offsetmaps bound to the variables in the
    clobbered set are treated. *)
