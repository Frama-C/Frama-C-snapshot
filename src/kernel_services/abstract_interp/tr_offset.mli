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

(** Reduction of a location (expressed as an Ival.t and a size)
    by a base validity. Only the locations in the trimmed result are valid.
    All offsets are expressed in bits. *)

type t = private
  | Invalid (** No location is valid *)
  | Set of Integer.t list (** Limited number of locations *)
  | Interval of (** min *) Integer.t *
                (** max *) Integer.t *
                (** modu *)Integer.t 
  | Overlap of (** min *) Integer.t *
               (** max *) Integer.t *
               Origin.t  (** The location covers the entire range [min..max],
                             but consecutive offsets overlap *)

val pretty: t Pretty_utils.formatter

(** [trim_by_validity ?origin offsets size validity] reduces [offsets] so that
    all accesses to [offsets+(0..size-1)] are valid according to [validity].
    For a size of 0, consider the offsets up to the validity past-one valid.
    The returned boolean indicates that at least one of the offsets does not
    comply with [validity]. If the valid offsets cannot be represented
    precisely, the [Overlap] constructor is returned. When specified,
    the [origin] argument is used as the source of this imprecision . *)
val trim_by_validity :
  ?origin:Origin.t ->
  Ival.t ->
  Integer.t ->
  Base.validity ->
  bool (** alarm *) * t

(** This is a more complete specification of this function, for a single offset
    [o]. We want to write [size>0 bits], on a base possibly valid between
    [min_valid..max_maybe_valid], and guaranteed to be valid between
    [min_valid..max_sure_valid]. The case [max_sure_valid < min_valid] is
    possible: in this case, no bit is guaranteed to be valid. For Valid and
    non-Empty bases, [min_valid<max_maybe_valid] holds. We write
    [start_to==o] and [stop_to==start_to+size-1]. Then

    - If [start_to..stop_to] is not included in [min_valid..max_maybe_valid],
      then the write completely fails: at least one bit is outside the validity.
      This translates to [start_to<min_valid || stop_to > max_maybe_valid]

    - If [start_to..stop_to] is not included in [min_valid..max_sure_valid],
      then we must emit an alarm. This translates to
      [start_to<min_valid || stop_to > max_sure_valid]. This convention works
      even when [min_valid..max_sure_valid] is not a real interval.
 *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
