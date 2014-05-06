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

(** The datastructures of this module can be used to track the origin
    of a major imprecision in the values of an abstract domain. *)

(** This module is generic, although currently used only by the plugin Value.
    Within Value, values that have an imprecision origin are "garbled mix",
    ie. a numeric value that contains bits extracted from at least one
    pointer, and that are not the result of a translation *)


(** Sets of source locations *)
module LocationSetLattice : sig
  include Lattice_type.Lattice_Set with type O.elt = Cil_types.location
  val currentloc_singleton : unit -> t
    val compare:t -> t -> int
end

(** List of possible origins. Most of them also include the set of
    source locations where the operation took place. *)
type origin =
  | Misalign_read of LocationSetLattice.t (** Read of not all the bits of a
                                   pointer, typicaller through a pointer cast *)
  | Leaf of LocationSetLattice.t (** Result of a function without a body *)
  | Merge of LocationSetLattice.t (** Join between two control-flows *)
  | Arith of LocationSetLattice.t (** Arithmetic operation that cannot be
                                      represented, eg. ['&x * 2'] *)
  | Well (** Imprecise variables of the intial state *)
  | Unknown

include Datatype.S with type t = origin


type kind =
  | K_Misalign_read
  | K_Leaf
  | K_Merge
  | K_Arith

val current: kind -> origin
(** This is automatically extracted from [Cil.CurrentLoc] *)

val pretty_as_reason: Format.formatter -> t -> unit
(** Pretty-print [because of <origin>] if the origin is not {!Unknown}, or
    nothing otherwise *)

val top: t
val is_top: t -> bool

val bottom: t

val join: t -> t -> t
val meet: t -> t -> t
val narrow: t -> t -> t

val is_included: t -> t -> bool

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
