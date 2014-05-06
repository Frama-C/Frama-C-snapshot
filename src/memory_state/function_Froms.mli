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

(** Datastructures and common operations for the results of the From plugin. *)

module Deps : sig

  type from_deps = {
    data: Locations.Zone.t;
    indirect: Locations.Zone.t;
  }

  include Lmap_bitwise.With_default with type t = from_deps

  val add_data_dep : t -> Locations.Zone.t -> t
  val add_indirect_dep : t -> Locations.Zone.t -> t
  val subst : (Locations.Zone.t -> Locations.Zone.t) -> t -> t

  val data_deps: Locations.Zone.t -> t
  val indirect_deps: Locations.Zone.t -> t

  val pretty_precise : Format.formatter -> t -> unit

  val to_zone: t -> Locations.Zone.t
end

module Memory : sig
  include Lmap_bitwise.Location_map_bitwise with type y = Deps.t

(* to print the detail of address and data dependencies, as opposed to [pretty]
   that prints the backwards-compatible union of them *)
  val pretty_ind_data : Format.formatter -> t -> unit

  val find: t -> Locations.Zone.t -> Locations.Zone.t
  (** Imprecise version of find, in which data and indirect dependencies are
      not distinguished *)

  val find_precise: t -> Locations.Zone.t -> Deps.t
  (** Precise version of find *)
end



type froms = {
  deps_return : Memory.LOffset.t
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
compile-command: "make -C ../.."
End:
*)
