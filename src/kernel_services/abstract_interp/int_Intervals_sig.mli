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

(** Sets of intervals with a lattice structure. Consecutive intervals are
    automatically fused. *)

open Abstract_interp

type itv = Int.t * Int.t

include Lattice_type.Full_Lattice
include Lattice_type.With_Error_Top

val is_top: t -> bool

val inject_bounds: Int.t -> Int.t -> t
val inject_itv: itv -> t
val inject: itv list -> t
val from_ival_size: Ival.t -> Int_Base.t -> t
  (** Conversion from an ival, which represents the beginning of
      each interval. The size if taken from the [Int_Base.t] argument.
      If the result contains more than [-plevel] arguments, it is
      automatically over-approximated. *)

val from_ival_size_under: Ival.t -> Int_Base.t -> t
  (** Same as [from_ival_size], except that the result is an under-approximation
      if the ival points to too many locations *)

val project_set: t -> itv list
(** May raise [Error_Top].
    As intervals are not represented as lists, this function has an overhead.
    Use iterators whenever possible instead. *)

val project_singleton: t -> itv option

(** Iterators *)
val fold: (itv -> 'a -> 'a) -> t -> 'a -> 'a
(** May raise [Error_Top] *)
val iter: (itv -> unit) -> t -> unit
(** May raise [Error_Top] *)

val pretty_typ: Cil_types.typ option -> t Pretty_utils.formatter
  (** Pretty-printer that supposes the intervals are subranges of
      a C type, and use the type to print nice offsets *)

val range_covers_whole_type: Cil_types.typ -> t -> bool
  (** Does the interval cover the entire range of bits that are valid
      for the given type. *)


(**/**)

val pretty_debug: t Pretty_utils.formatter


(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
