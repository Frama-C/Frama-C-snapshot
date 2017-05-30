(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Lang
open Lang.F

type env
type label

type value =
  | Term
  | Addr of Memory.lval
  | Lval of Memory.lval * label
  | Chunk of string * label

val create : unit -> env
val register : Conditions.sequence -> env

val at : env -> id:int -> label
val find : env -> F.term -> value
val updates : env -> label Memory.sequence -> Vars.t -> Memory.update Bag.t
val visible : label -> bool
val subterms : env -> (F.term -> unit) -> F.term -> bool
val prev : label -> label list
val next : label -> label list
val iter : (Memory.mval -> term -> unit) -> label -> unit
val branching : label -> bool

class virtual engine :
  object
    method virtual pp_atom : Format.formatter -> term -> unit
    method virtual pp_flow : Format.formatter -> term -> unit

    method is_atomic_lv : Memory.lval -> bool

    method pp_ofs : Format.formatter -> Memory.offset -> unit
    method pp_offset : Format.formatter -> Memory.offset list -> unit
    method pp_host : Format.formatter -> Memory.host -> unit (** current state *)
    method pp_lval : Format.formatter -> Memory.lval -> unit (** current state *)
    method pp_addr : Format.formatter -> Memory.lval -> unit
    method pp_label : Format.formatter -> label -> unit (** label name *)
    method pp_chunk : Format.formatter -> string -> unit (** chunk name *)
  end
