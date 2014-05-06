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

(** Type of the arguments of functor {!Offsetmap.Make} *)

open Lattice_type

include Bounded_Join_Semi_Lattice
include With_Top with type t := t
include With_Widening with type t := t
include With_Cardinal_One with type t := t

(** Are the bits independent? *)
val is_isotropic : t -> bool

val extract_bits :
  topify:Origin.kind ->
  start:Integer.t -> stop:Integer.t -> size:Integer.t ->
  t ->
  bool * t
(** Extract the bits between {!start} and {!stop} in the value of type [t],
    assuming this value has {!size} bits. Return the corresponding value, and
    a boolean indicating that an imprecision occurred during the operation.
    In the latter case, the origin of the imprecision is flagged as having kind
    [topify] *)

val little_endian_merge_bits :
  topify:Origin.kind ->
  conflate_bottom:bool ->
  total_length:int -> value:t -> offset:Integer.t -> t -> t

val big_endian_merge_bits :
  topify:Origin.kind ->
  conflate_bottom:bool ->
  total_length:int -> length:Integer.t -> value:t -> offset:Integer.t -> t -> t

val merge_neutral_element: t
(** Value that can be passed to {!little_endian_merge_bits} or
    {!big_endian_merge_bits} as the starting value. This value must
    be neutral wrt. concatenation of values. *)

val topify_with_origin : Origin.t -> t -> t
(** Force a value to be isotropic, when a loss of imprecision occurs.
    The resulting value must verify {!is_isotropic}. *)

val anisotropic_cast : size:Integer.t -> t -> t
(** Convert the given value so that it fits in [size] bits. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
