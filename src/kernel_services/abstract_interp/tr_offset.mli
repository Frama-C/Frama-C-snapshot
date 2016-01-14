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

(** Reduction of a location (expressed as an Ival.t and a size)
    by a base validity. Only the locations in the trimmed result are valid. *)

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

(** [trim_by_validity ?origin offsets size validity] reduces [offsets] so that
    all accesses to [offsets+(0..size-1)] are valid according to [validity].
    The returned boolean indicates that at least one of the offsets does not
    comply with [validity]. If the valid offsets cannot be represented
    precisely, the [Imprecise] constructor is returned. When specified,
    the [origin] argument is used as the source of this imprecision . *)
val trim_by_validity :
  ?origin:Origin.t ->
  Ival.t ->
  Integer.t ->
  Base.validity ->
  bool (** alarm *) * t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
