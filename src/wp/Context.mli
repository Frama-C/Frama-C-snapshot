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

(** Current Loc *)

val with_current_loc : Cil_types.location -> ('a -> 'b) -> 'a -> 'b

(** Contextual Values *)

type 'a value

val create : ?default:'a -> string -> 'a value
  (** Creates a new context with name *)

val defined : 'a value -> bool
  (** The current value is defined. *)

val get : 'a value -> 'a
  (** Retrieves the current value of the context.
      Raise an exception if not bound. *)

val set : 'a value -> 'a -> unit
  (** Define the current value. Previous one is lost *)

val update : 'a value -> ('a -> 'a) -> unit
  (** Modification of the current value *)

val bind : 'a value -> 'a -> ('b -> 'c) -> 'b -> 'c
  (** Performs the job with local context bound to local value. *)

val free : 'a value -> ('b -> 'c) -> 'b -> 'c
  (** Performs the job with local context cleared. *)

val clear : 'a value -> unit
  (** Clear the current value. *)

val push : 'a value -> 'a -> 'a option
val pop : 'a value -> 'a option -> unit

val name : 'a value -> string

val once : (unit -> unit) -> unit -> unit
  (** A global configure, executed once. *)
  
