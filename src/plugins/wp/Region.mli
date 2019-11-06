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
open Layout

type region
type map

module R : Layout.Data with type t = region
module Map : Qed.Idxmap.S with type key = region
module Set : Qed.Idxset.S with type elt = region

val create : unit -> map
val is_empty : map -> bool
val iter : map -> (region -> unit) -> unit

val id: region -> int
val noid: int

val get_addrof : map -> region -> region
val add_pointed : map -> region -> region
val add_offset : map -> region -> offset -> region
val field_offset : map -> fieldinfo -> int * int

val get_froms : map -> region -> region from list
val get_roots : map -> region -> root
val has_roots : map -> region -> bool

val is_garbled : region -> bool
val has_pointed : region -> bool
val has_layout : region -> bool
val has_offset : region -> offset -> bool
val has_copies : region -> bool
val has_deref : region -> bool
val has_names : region -> bool
val has_return : map -> bool

val get_pointed : map -> region -> region option
val get_offset : map -> region -> offset -> region option
val get_copies : map -> region -> region list
val get_alias : map -> region -> region
val get_merged : map -> region -> region option
val eq_alias : map -> region -> region -> bool

val acs_read : region -> lvalue -> unit
val acs_write : region -> lvalue -> unit
val acs_shift : region -> lvalue -> unit
val acs_deref : region -> deref -> unit
val acs_copy : src:region -> tgt:region -> unit

val is_read : region -> bool
val is_written : region -> bool
val is_shifted : region -> bool
val is_aliased : region -> bool

val iter_read : (lvalue -> unit) -> region -> unit
val iter_write : (lvalue -> unit) -> region -> unit
val iter_shift : (lvalue -> unit) -> region -> unit
val iter_deref : (deref -> unit) -> region -> unit
val iter_offset : map -> (offset -> region -> unit) -> region -> unit
val iter_copies : map -> (region -> unit) -> region -> unit
val iter_vars : map -> (varinfo -> region -> unit) -> unit
val iter_names : map -> (string -> region -> unit) -> unit
val iter_strings : map -> (region -> string -> unit) -> unit

val of_null : map -> region
val of_return : map -> region
val of_cvar : map -> varinfo -> region
val of_cstring : map -> eid:int -> cst:string -> region
val of_name : map -> string -> region
val of_class : map -> string option -> region

val region : map -> int -> region
val cluster : map -> region -> region cluster
val chunk : map -> region -> region chunk
val chunks : map -> region -> chunks

val alias : map -> region -> region -> region
val do_alias : map -> region -> region -> unit
val add_alias : map -> into:region -> region -> unit

val fusion : map -> unit
val fusionned : map -> bool
val iter_fusion : map -> (int -> region -> unit) -> unit
val fixpoint : map -> unit
