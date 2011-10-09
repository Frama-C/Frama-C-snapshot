(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(** Logical Identifiers *)

type id
val library : string -> id (** Returns an id for an external symbol *)
val create : string -> id (** Create a new fresh identifier with the given basename *)
val basename : id -> string

(** {3 Datatype} *)

type t = id
val compare : id -> id -> int
val equal : id -> id -> bool
val hash : id -> int
val pretty : Format.formatter -> id -> unit
val dummy : id (** Only usable for represent in projectified definitions *)

(** {3 Map and Set} *)

module Iset : Set.S with type elt = t
module Imap : Map.S with type key = t
module Ihmap : Hashtbl.S with type key = t

(** {3 Name Spaces} *)

type space
val space : unit -> space (** Creates an empty name space (but with the external names). *)
val copy : space -> space (** Duplicates a name space. *)

val link : space -> id -> string -> unit
  (** Link the identifier to some absolute name. 
      Also reserves this name. *)

val reserved : space -> string list -> unit
  (** Reserves the given names to avoid clash with generated ones. *)

val name : space -> ?unique:bool -> id -> string
  (** Assigns a unique string to an identifier in the space.
      When flag [unique] is set to true, the generated name 
      is preferrably the basename of the identifier. *)

val pp_id : space -> Format.formatter -> id -> unit
  (** Combines [name] and [Format]. *)

val unique : space -> id -> unit
  (** Reserves the identifier to be assigned its basename as name, if possible.
      Same as using [name space ~unique:true id]. *)

val touch : space -> id -> unit
  (** Count the identifier in the space.
      If the identifier has been counted exactly once, 
      the first time its name is asked, the basename would be
      assigned to it. *)

val push : space -> string -> id (* Allocate a local identifier in the space name *)
val pop : space -> id -> unit (* Deallocate a local identifier from the space name *)
val clear : space -> unit (* Deallocate all local identifiers *)

type mark
val mark : space -> mark (* Marks all current locals *)
val unmark : space -> mark -> unit (* Deallocate non-marked locals *)

val iter : (id -> string -> unit) -> space -> unit

(** {3 Sorting} *)

val alpha : string -> string -> int
  (** User-friendly name sorting. Namely: [a<A<b] and [1<10]. *)
