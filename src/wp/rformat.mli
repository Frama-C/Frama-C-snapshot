(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Format

val epsilon : float

val get_time : float array -> float -> int
  (** [get_time T t] returns [k] such that [T[k-1] <= t <= T[k]],
      [T] is extended with [T[-1]=0] and [T[N]=+oo]. *) 

val pp_time : formatter -> float -> unit
  (** Pretty print time in hour, minutes, seconds, or milliseconds, as appropriate *)

val pp_time_range : float array -> formatter -> float -> unit

type command =
  | CMD of string
  | ARG of string * string
  | TEXT

val command : string -> command

val pretty : (formatter -> string -> string -> unit) -> formatter -> string -> unit
