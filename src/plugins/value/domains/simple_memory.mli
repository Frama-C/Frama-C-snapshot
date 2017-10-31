(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Simple memory abstraction for scalar non-volatile variables, built upon a
    value abstraction. Basically a map from variable to values. *)

(** Abstraction of the values variables are mapped to. *)
module type Value = sig
  include Datatype.S

  (** Lattice structure. *)

  val top : t
  val join : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t Eval.or_bottom
  val is_included : t -> t -> bool

  (** This function must return [true] if the given variable should be
      tracked by the domain. All untracked variables are implicitely
      mapped to [V.top]. *)
  val track_variable: Cil_types.varinfo -> bool

  (** Can be equal to {!pretty} *)
  val pretty_debug: t Pretty_utils.formatter
end

(** Signature of a simple memory abstraction for scalar variables.  *)
module type S = sig
  type t
  type value

  (** [add loc typ v state] binds [loc] to [v] in state. If [typ] does
      not match the effective type of the location pointed, [V.top] is
      bound instead. This function automatically handles the case where
      [loc] abstracts multiple locations, or when some locations are not
      tracked by the domain. *)
  val add: Precise_locs.precise_location -> Cil_types.typ -> value -> t -> t

  (** [find loc typ state] returns the join of the abstract values stored
      in the locations abstracted to by [loc] in [state], assuming the
      result has type [typ]. When [loc] includes untracked locations, or when
      [typ] does not match the type of the locations in [loc], the
      result is approximated. *)
  val find: Precise_locs.precise_location -> Cil_types.typ -> t -> value

  (** [remove loc state] drops all information on the locations pointed to
      by [loc] from [state]. *)
  val remove: Precise_locs.precise_location -> t -> t

  (** [remove_variables list state] drops all information about the variables
      in [list] from state. *)
  val remove_variables: Cil_types.varinfo list -> t -> t

  (** Fold on base value pairs. *)
  val fold: (Base.t -> value -> 'a -> 'a) -> t -> 'a -> 'a

end

(* Builds a memory from a value abstraction. *)
module Make_Memory (Value: Value) : sig
  include Datatype.S_with_collections
  include S with type t := t
             and type value := Value.t

  val top: t
  (** The top abstraction, which maps all variables to {!V.top}. *)

  val join: t -> t -> t
  val widen: t -> t -> t
  val is_included: t -> t -> bool
end


(* Builds a complete Eva domain from a value abstraction. *)
module Make_Domain
    (Info: sig val name: string end)
    (Value: Value)
  : sig

    include Abstract_domain.Internal with type value = Value.t
                                      and type location = Precise_locs.precise_location

    include S with type t := t
               and type value := Value.t

  end
