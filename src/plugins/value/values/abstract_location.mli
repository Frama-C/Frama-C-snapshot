(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Abstract memory locations of the analysis. *)

open Cil_types
open Eval

type 'v truth = 'v Abstract_value.truth

(** Signature of abstract memory locations. *)
module type S = sig
  type value

  type location  (** abstract locations *)
  type offset    (** abstract offsets *)

  val top: location

  val equal_loc: location -> location -> bool
  val equal_offset: offset -> offset -> bool
  val pretty_loc: Format.formatter -> location -> unit
  val pretty_offset : Format.formatter -> offset -> unit

  val to_value : location -> value
  val size : location -> Int_Base.t

  (** {3 Alarms } *)

  (** These functions are used to create the alarms that report undesirable
      behaviors, when a location abstraction does not meet the prerequisites of
      an operation. Thereafter, the location is assumed to meet them to continue
      the analysis.
      See the documentation of {!Abstract_value.truth} for more details. *)

  (** Assumes that two locations do not overlap. If [partial] is true, the
      concrete locations may be equal, but different locations must not overlap.
      Otherwise, the locations must be completely separate. *)
  val assume_no_overlap:
    partial:bool -> location -> location -> (location * location) truth

  (** Assumes that the given location is valid for a read or write operation,
      according to the [for_writing] boolean. Used to emit memory access alarms.
      If the location is not completely valid, reduces it to its valid part.
      [bitfield] indicates whether the location may be the one of a bitfield;
      if it is false, the location can be assumed to be byte aligned. *)
  val assume_valid_location:
    for_writing:bool -> bitfield:bool -> location -> location truth

  (** {3 Forward Offset Operations } *)

  val no_offset : offset

  (** Computes the field offset of a fieldinfo, with the given remaining offset.
      The given type must the one of the structure or the union. *)
  val forward_field : typ -> fieldinfo -> offset -> offset

  (** [forward_index typ value offset] computes the array index offset of
      (Index (ind, off)), where the index expression [ind] evaluates to [value]
      and the remaining offset [off] evaluates to [offset].
      [typ] must be the type pointed by the array. *)
  val forward_index : typ -> value -> offset -> offset

  (** {3 Forward Locations Operations } *)

  (** Evaluation of the location of an lvalue, when the offset has already
      been evaluated. In case of a pointer, its expression has also been
      evaluated to a value. *)

  (** Var case in the AST: the host is a variable. *)
  val forward_variable : typ -> varinfo -> offset -> location or_bottom

  (** Mem case in the AST: the host is a pointer. *)
  val forward_pointer : typ -> value -> offset -> location or_bottom

  val eval_varinfo : varinfo -> location

  (** {3 Backward Operations } *)

  (** For an unary forward operation F, the inverse backward operator B tries to
      reduce the argument values of the operation, given its result.

      It must satisfy:
        if [B arg res] = v
        then ∀ a ⊆ arg such that [F a] ⊆ res, a ⊆ v

      i.e. [B arg res] returns a value [v] larger than all subvalues of [arg]
      whose result through F is included in [res].

      If [F arg] ∈ [res] is impossible, then [v] should be bottom.

      Any n-ary operator may be considered as a unary operator on a vector
      of values, the inclusion being lifted pointwise.
  *)

  val backward_variable : varinfo -> location -> offset or_bottom
  val backward_pointer : value -> offset -> location -> (value * offset) or_bottom
  val backward_field : typ -> fieldinfo -> offset -> offset or_bottom
  val backward_index :
    typ -> index:value -> remaining:offset -> offset -> (value * offset) or_bottom
end

(** Key and structure for locations. See {structure.mli},
    and {domain.mli} where the mechanism is explained in detail.*)

type 'a key = 'a Structure.Key_Location.k
type 'a structure = 'a Structure.Key_Location.structure

module type Internal = sig
  include S
  val structure : location structure
end

module type External = sig
  include S
  include Structure.External with type t := location
                              and type 'a key := 'a key
end

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
