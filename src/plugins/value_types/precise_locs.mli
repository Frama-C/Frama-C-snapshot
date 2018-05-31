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

(** This module provides transient datastructures that may be more precise
    than an {!Ival.t}, {!Locations.Location_Bits.t} and {!Locations.location}
    respectively, typically for l-values such as [t[i][j]], [p->t[i]], etc.
    Those structures do not have a lattice structure, and cannot be stored
    as an abstract domain. However, they can be use to model more precisely
    read or write accesses to semi-imprecise l-values. *)


(** {2 Precise offsets} *)

type precise_offset
val pretty_offset : Format.formatter -> precise_offset -> unit

val equal_offset: precise_offset -> precise_offset -> bool

val offset_zero : precise_offset
val offset_bottom : precise_offset
val offset_top : precise_offset
val inject_ival : Ival.t -> precise_offset

val is_bottom_offset : precise_offset -> bool

val imprecise_offset : precise_offset -> Ival.t

(*val _scale_offset : Integer.t -> precise_offset -> precise_offset*)
val shift_offset_by_singleton : Integer.t -> precise_offset -> precise_offset
val shift_offset : Ival.t -> precise_offset -> precise_offset


(** {2 Precise location_bits} *)

type precise_location_bits
val pretty_loc_bits : Format.formatter -> precise_location_bits -> unit
val bottom_location_bits : precise_location_bits

val inject_location_bits : Locations.Location_Bits.t -> precise_location_bits
val combine_base_precise_offset :
  Base.t -> precise_offset -> precise_location_bits
val combine_loc_precise_offset :
  Locations.Location_Bits.t -> precise_offset -> precise_location_bits

val imprecise_location_bits :
  precise_location_bits -> Locations.Location_Bits.t


(** {2 Precise locations} *)

type precise_location

val equal_loc: precise_location -> precise_location -> bool

val loc_size: precise_location -> Int_Base.t

val make_precise_loc :
  precise_location_bits -> size:Int_Base.t -> precise_location

val imprecise_location : precise_location -> Locations.location

val loc_bottom : precise_location
val is_bottom_loc: precise_location -> bool

val loc_top : precise_location
val is_top_loc: precise_location -> bool

val fold:
  (Locations.location -> 'a -> 'a) -> precise_location -> 'a -> 'a

val enumerate_valid_bits:
  for_writing:bool -> precise_location -> Locations.Zone.t

val valid_cardinal_zero_or_one: for_writing:bool -> precise_location -> bool
(** Is the restriction of the given location to its valid part precise enough
    to perform a strong read, or a strong update. *)

val cardinal_zero_or_one: precise_location -> bool
(** Should not be used, {!valid_cardinal_zero_or_one} is almost always more
    useful *)

val pretty_loc: precise_location Pretty_utils.formatter

val valid_part:
  for_writing:bool -> bitfield:bool -> precise_location -> precise_location
(** Overapproximation of the valid part of the given location for a read or write
    operation, according to the [for_writing] boolean.
    [bitfield] indicates whether the location may be the one of a bitfield, and
    is true by default. If it is set to false, the location is assumed to be
    byte aligned, and its offset (expressed in bits) is reduced to be congruent
    to 0 modulo 8. *)
