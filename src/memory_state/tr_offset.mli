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

(** Reduction of a location (expressed as an Ival.t and a size)
    by a base validity. Only the locations in the trimmed result are valid. *)

type t =
  | Invalid (** No location is valid *)
  | Set of Integer.t list (** Limited number of locations *)
  | Interval of (** min *) Integer.t *
                (** max *) Integer.t *
                (** modu *)Integer.t 
  | Imprecise of (** min *) Integer.t *
                 (** max *) Integer.t

val filter_by_bound_for_reading :
  with_alarms:CilE.warn_mode ->
    Ival.t -> Integer.t -> Base.validity -> t

val filter_by_bound_for_writing :
  with_alarms:CilE.warn_mode ->
  exact:bool ->
  Ival.t -> Integer.t -> Base.validity -> bool * t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
