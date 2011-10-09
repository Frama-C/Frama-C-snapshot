(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

val name : string

type validity =
  | All
  | Unknown of Abstract_interp.Int.t*Abstract_interp.Int.t
  | Known of Abstract_interp.Int.t*Abstract_interp.Int.t
  | Periodic of Abstract_interp.Int.t*Abstract_interp.Int.t*
      Abstract_interp.Int.t

type string_id

type base = private
  | Var of Cil_types.varinfo * validity (** Base for uninitialized variables *)
  | Initialized_Var of Cil_types.varinfo * validity
      (** Base for variables initialized to zero . *)
  | Null (** Base for addresses like [(int* )0x123] *)
  | String of int * string_id (** String constants *)

include Datatype.S_with_collections with type t = base
module Hptset: Hptset.S with type elt = t

val pretty_validity : Format.formatter -> validity -> unit

val typeof : t -> Cil_types.typ option
val null : t

val is_null : t -> bool
val is_read_only : t -> bool

val bits_sizeof : t -> Int_Base.t
val id : t -> int
val is_aligned_by : t -> Abstract_interp.Int.t -> bool
val validity : t -> validity

exception Not_valid_offset
val is_valid_offset :
  for_writing:bool -> Abstract_interp.Int.t -> t -> Ival.t -> unit

val is_function : t -> bool

val is_formal_or_local : t -> Cil_types.fundec -> bool
val is_any_formal_or_local : t -> bool
val is_any_local : t -> bool
val is_global : t -> bool
val is_formal_of_prototype : t -> Cil_types.varinfo -> bool
val is_local: t -> Cil_types.fundec -> bool
val is_formal: t -> Cil_types.fundec -> bool
val is_block_local: t -> Cil_types.block -> bool
val is_hidden_variable : t -> bool
val validity_from_type : Cil_types.varinfo -> validity

val create_varinfo : Cil_types.varinfo -> t
  (** Return the base corresponding to a program variable. This function's name
      is short for "create_from_varinfo". The validity of the base is inferred
      from the type of the variable. *)
exception Not_a_variable
val get_varinfo: t -> Cil_types.varinfo
  (** If the base corresponds to a variable, return the variable's varinfo.
      *raise Not_a_variable if the base is not a variable. *)

val create_logic :  Cil_types.varinfo -> validity -> t
  (** Return the base corresponding to a logic variable. This function's name
      is short for "create_from_logic". *)

val find: Cil_types.varinfo -> t
  (** Return the base corresponding to a variable. *)

val create_initialized :  Cil_types.varinfo -> validity -> t
val create_string : Cil_types.exp -> t

type cstring = CSString of string | CSWstring of Escape.wstring
val get_string : string_id -> cstring

val min_valid_absolute_address: unit -> Abstract_interp.Int.t
val max_valid_absolute_address: unit -> Abstract_interp.Int.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
