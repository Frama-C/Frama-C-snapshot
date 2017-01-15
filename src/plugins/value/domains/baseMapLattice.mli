(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(** Simple memory abstraction for scalar variables. *)

module Make(V: sig
    include Abstract_value.Internal
    (** Abstraction of the values variables are mapped to. *)

    val track_variable: Cil_types.varinfo -> bool
    (** This function must return [true] if the given variable should be
        tracked by the domain. All untracked variables are implicitely
        mapped to [V.top]. *)

    val name: string
    (** Name of the abstraction, for debugging purposes only. *)

    val pretty_debug: t Pretty_utils.formatter
    (** Can be equal to {!pretty} *)
  end) : sig

  include Datatype.S_with_collections

  val top: t
  (** The top abstraction, which maps all variables to {!V.top}. *)

  val join: t -> t -> t
  val is_included: t -> t -> bool
  val join_and_is_included: t -> t -> t * bool
  val widen: (V.t -> V.t -> V.t) -> t -> t -> t
  (** [widen f s1 s2] computes the widening of [s1] and [s2], using [f]
      as the widening function on values. *)
  
  val add: Precise_locs.precise_location -> Cil_types.typ -> V.t -> t -> t
  (** [add loc typ v state] binds [loc] to [v] in state. If [typ] does
      not match the effective type of the location pointed, [V.top] is
      bound instead. This function automatically handles the case where
      [loc] abstracts multiple locations, or when some locations are not
      tracked by the domain. *)

  val find: Precise_locs.precise_location -> Cil_types.typ -> t -> V.t
  (** [find loc typ state] returns the join of the abstract values stored
      in the locations abstracted to by [loc] in [state], assuming the
      result has type [typ]. When [loc] includes untracked locations, or when
      [typ] does not match the type of the locations in [loc], the
      result is approximated. *)

  val remove: Precise_locs.precise_location -> t -> t
  (** [remove loc state] drops all information on the locations pointed to
      by [loc] from [state]. *)

  val remove_variables: Cil_types.varinfo list -> t -> t
  (** [remove_variables list state] drops all information about the variables
      in [list] from state. *)
end
