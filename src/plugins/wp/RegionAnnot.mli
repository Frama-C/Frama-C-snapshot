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

type lrange =
  | R_index of term
  | R_range of term option * term option

type lpath = {
  loc : location ;
  lnode : lnode ;
  ltype : typ ;
}
and lnode =
  | L_var of varinfo
  | L_region of string
  | L_addr of lpath
  | L_star of typ * lpath
  | L_shift of lpath * typ * lrange
  | L_index of lpath * typ * lrange
  | L_field of lpath * fieldinfo list
  | L_cast of typ * lpath

module Lpath :
sig
  type t = lpath
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

type region_pattern =
  | FREE
  | PVAR
  | PREF
  | PMEM
  | PVECTOR
  | PMATRIX

type region_spec = {
  region_name: string option ;
  region_pattern: region_pattern ;
  region_lpath: lpath list ;
}

val p_name : region_pattern -> string
val of_extension : acsl_extension -> region_spec list
val of_behavior : behavior -> region_spec list

val register : unit -> unit (** Auto when `-wp-region` *)
