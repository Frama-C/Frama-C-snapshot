(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types

type param = NotUsed | ByAddr | ByValue | ByShift | ByRef | InContext | InArray

val pp_param : Format.formatter -> param -> unit

type partition

val empty : partition
val set : varinfo -> param -> partition -> partition

type zone =
  | Var of varinfo   (** [&x] the cell x *)
  | Ptr of varinfo   (** [p] the cell pointed by p *)
  | Arr of varinfo   (** [p+(..)] the cell and its neighbors pointed by p *)

type clause =
  | Valid of zone
  | Separated of zone list list

(** Build the separation clause from a partition,
    including the clauses related to the pointer validity *)
val requires : partition -> clause list

val pp_zone : Format.formatter -> zone -> unit
val pp_clause : Format.formatter -> clause -> unit
