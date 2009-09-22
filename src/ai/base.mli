(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

val name : string
type cell_class_attributes

type validity =
  | All
  | Unknown of Abstract_interp.Int.t*Abstract_interp.Int.t
  | Known of Abstract_interp.Int.t*Abstract_interp.Int.t

type t = private
  | Var of Cil_types.varinfo*validity (** Base for uninitialized variables *)
  | Initialized_Var of Cil_types.varinfo*validity
      (** Base for variables initialized to zero . *)
  | Null (** Base for adresses like [(int* )0x123] *)
  | String of int*string (** String constants *)
  | Cell_class of cell_class_attributes (** A class of memory cells *)

val pretty : Format.formatter -> t -> unit
val pretty_validity : Format.formatter -> validity -> unit

val compare : t -> t -> int
val typeof : t -> Cil_types.typ option
val hash : t -> int
val equal : t -> t -> bool
val null : t

val is_null : t -> bool
val bits_sizeof : t -> Int_Base.t
val id : t -> int
val is_aligned_by : t -> Abstract_interp.Int.t -> bool
val validity : t -> validity

exception Not_valid_offset
val is_valid_offset : Abstract_interp.Int.t -> t -> Ival.t -> unit

val is_formal_or_local : t -> Cil_types.fundec -> bool
val is_any_formal_or_local : t -> bool
val is_any_local : t -> bool
val is_formal_of_prototype : t -> Cil_types.varinfo -> bool
val is_local: t -> Cil_types.fundec -> bool
val is_block_local: t -> Cil_types.block -> bool
val is_hidden_variable : t -> bool
val validity_from_type : Cil_types.varinfo -> validity
val create_varinfo : Cil_types.varinfo -> t
val create_logic :  Cil_types.varinfo -> validity -> t
val create_initialized :  Cil_types.varinfo -> validity -> t
val create_string : string -> t

module Datatype: Project.Datatype.S with type t = t

val min_valid_absolute_address: unit -> Abstract_interp.Int.t
val max_valid_absolute_address: unit -> Abstract_interp.Int.t
