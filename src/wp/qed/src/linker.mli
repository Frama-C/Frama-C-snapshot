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

(* -------------------------------------------------------------------------- *)
(* --- Identifiers Management                                             --- *)
(* -------------------------------------------------------------------------- *)

open Format
open Logic
open Plib

(** {2 Identifiers} *)

val is_ident : string -> bool (** Operators is an identifier *)
val ident : string -> string (** Filter out non-letter characters *)

(** {2 Allocators} *)

type allocator

val allocator : unit -> allocator
val declare : allocator -> string -> unit
val fresh : allocator -> string -> string
val copy : allocator -> allocator

(** {2 Linkers} *)

class type ['a,'idx] linker = 
  object
    method lock  : unit
    method clear : unit
    method push  : 'idx
    method pop   : 'idx -> unit
    method mem   : 'a -> bool
    method find  : 'a -> string
    method link  : 'a -> string -> unit
    method print : 'a printer
    method alloc : basename:string -> 'a -> string
    method reserve : basename:string -> string
    method bind_reserved : 'a -> string -> unit
    method alloc_with : allocator -> unit
  end

module Link(A : Symbol) :
sig
  type index
  val linker : unit -> (A.t,index) linker
end

module Record(T : Logic.Term) :
sig

  type t
  val create : unit -> t
  val register : t -> T.ADT.t -> T.Field.t list -> unit
  val get_fields : t -> T.ADT.t -> T.Field.t list
  val get_record : t -> T.Field.t list -> T.ADT.t

end
