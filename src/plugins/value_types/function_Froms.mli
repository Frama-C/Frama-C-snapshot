(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Datastructures and common operations for the results of the From plugin. *)

module Deps : sig

  type deps = {
    data: Locations.Zone.t;
    indirect: Locations.Zone.t;
  }

  val bottom: deps
  val top: deps
  val join: deps -> deps -> deps
  val to_zone: deps -> Locations.Zone.t

  val add_data_dep: deps -> Locations.Zone.t -> deps
  val add_indirect_dep: deps -> Locations.Zone.t -> deps


  val from_data_deps: Locations.Zone.t -> deps
  val from_indirect_deps: Locations.Zone.t -> deps

  val map: (Locations.Zone.t -> Locations.Zone.t) -> deps -> deps

  include Datatype.S with type t = deps

  val pretty_precise : Format.formatter -> t -> unit
end


module DepsOrUnassigned : sig

  type deps_or_unassigned =
  | DepsBottom (** Bottom of the lattice, never bound inside a memory state
                   at a valid location. (May appear for bases for which the
                   validity does not start at 0, currently only NULL.) *)
  | Unassigned (** Location has never been assigned *)
  | AssignedFrom of Deps.t (** Location guaranteed to have been overwritten,
                               its contents depend on the [Deps.t] value *)
  | MaybeAssignedFrom of Deps.t  (** Location may or may not have been
                                     overwritten *)
  (** The lattice is [DepsBottom <= Unassigned], [DepsBottom <= AssignedFrom z],
      [Unassigned <= MaybeAssignedFrom] and
      [AssignedFrom z <= MaybeAssignedFrom z]. *)

  include Lmap_bitwise.With_default with type t = deps_or_unassigned

  val subst: (Deps.t -> Deps.t) -> t -> t

  val extract_data: t -> Locations.Zone.t
  val extract_indirect: t -> Locations.Zone.t

  val may_be_unassigned: t -> bool

  val compose: t -> t -> t
  (** [compose d1 d2] is the sequential composition of [d1] after [d2], ie.
      the dependencies needed to execute [d1] after having executed [d2].
      It is computed as [d1] if [d1 = AssignedFrom _] (as executing [d1]
      completely overwrites what [d2] wrote), and as a partial join between
      [d1] and [d2] in the other cases. *)

  val pretty_precise : Format.formatter -> t -> unit

  val to_zone: t -> Locations.Zone.t
  val to_deps: t -> Deps.deps
end

module Memory : sig
  include Lmap_bitwise.Location_map_bitwise with type v = DepsOrUnassigned.t

  (** Prints the detail of address and data dependencies, as opposed to [pretty]
      that prints the backwards-compatible union of them *)
  val pretty_ind_data : Format.formatter -> t -> unit

  val find: t -> Locations.Zone.t -> Locations.Zone.t
  (** Imprecise version of find, in which data and indirect dependencies are
      not distinguished *)

  val find_precise: t -> Locations.Zone.t -> Deps.t
  (** Precise version of find *)

  val add_binding: exact:bool -> t -> Locations.Zone.t -> Deps.t -> t
  val add_binding_loc: exact:bool -> t -> Locations.location -> Deps.t -> t
  val add_binding_precise_loc:
    exact:bool -> t -> Precise_locs.precise_location -> Deps.t -> t
  val bind_var: Cil_types.varinfo -> Deps.t -> t -> t
  val unbind_var: Cil_types.varinfo -> t -> t

  val map: (DepsOrUnassigned.t -> DepsOrUnassigned.t) -> t -> t

  val compose: t -> t -> t
  (** Sequential composition. See {!DepsOrUnassigned.compose}. *)

  val substitute: t -> Deps.t -> Deps.t
  (** [substitute m d] applies [m] to [d] so that any dependency in [d] is
      expressed using the dependencies already present in [m]. For example,
      [substitute 'x From y' 'x'] returns ['y']. *)


  (** Dependencies for [\result]. *)

  type return = Deps.t
  (* Currently, this type is equal to [Deps.t]. However, some of the functions
     below are more precise, and will be more useful when 'return' are
     represented by a precise offsetmap. *)

  (** Default value to use for storing the dependencies of [\result] *)
  val default_return: return

  (** Completly imprecise return *)
  val top_return: return

  (** Completly imprecise return of the given size *)
  val top_return_size: Int_Base.t -> return

  (** Add some dependencies to [\result], between bits [start] and
      [start+size-1], to the [Deps.t] value; default value for [start] is 0.
      If [m] is specified, the dependencies are added to it. Otherwise,
      {!default_return} is used. *)
  val add_to_return:
    ?start:int -> size:Int_Base.t -> ?m:return -> Deps.t -> return

  val collapse_return: return -> Deps.t
end



type froms = {
  deps_return : Memory.return
       (** Dependencies for the returned value *);
  deps_table : Memory.t
    (** Dependencies on all the zones modified by the function *);
}

include Datatype.S with type t = froms

val join: froms -> froms -> froms

val top: froms
(** Display dependencies of a function, using the function's type to improve
readability *)
val pretty_with_type: Cil_types.typ -> froms Pretty_utils.formatter
(** Display dependencies of a function, using the function's type to improve
readability, separating direct and indirect dependencies *)
val pretty_with_type_indirect: Cil_types.typ -> froms Pretty_utils.formatter

(** Extract the left part of a from result, ie. the zones that are written *)
val outputs: froms -> Locations.Zone.t

(** Extract the right part of a from result, ie. the zones on which the
    written zones depend. If [include_self] is true, and the from is
    of the form [x FROM y (and SELF)], [x] is added to the result;
    default value is [false]. *)
val inputs: ?include_self:bool -> froms -> Locations.Zone.t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
