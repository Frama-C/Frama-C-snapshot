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

(** Sets of disjoint intervals with a lattice structure. Consecutive
    intervals are automatically fused. Current implementation uses a
    sorted list. *)

open Abstract_interp

type itv = Int.t * Int.t


module Int_Intervals : sig
  include Lattice_type.Full_Lattice
  include Lattice_type.With_Error_Top

  val id: t -> int

  val is_top: t -> bool

  val inject_bounds: Int.t -> Int.t -> t
  val inject: itv list -> t
  val from_ival_size: Ival.t -> Int_Base.t -> t
    (** Conversion from an ival, which represents the beginning of
        each interval. The size if taken from the [Int_Base.t] argument.
        If the result contains more than [-plevel] arguments, it is
        automatically approximated. *)

  exception Not_a_set
  val project_set: t -> itv list
    (** may raise [Not_a_set] *)

  val project_singleton: t -> itv option

  (** Iterators *)
  val fold: (itv -> 'a -> 'a) -> t -> 'a -> 'a

  val pretty_typ: Cil_types.typ option -> t Pretty_utils.formatter
    (** Pretty-printer that supposes the intervals are subranges of
        a C type, and use the type to print nice offsets *)

  val compare_itvs: t -> t -> int
    (** Comparison that lifts the standard order between two intervals
        to lattices. If you want constant-time comparison, use [compare]. *)

end

(**/**) (* This is automatically set by the Value plugin. Do not call. *)
val plevel: int ref
(**/**)


(*
Local Variables:
compile-command: "make -C ../.. byte"
End:
*)
