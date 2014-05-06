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
(* --- Linker for ACSL Builtins                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Lang

type category = Lang.lfun Qed.Logic.category

type kind = 
  | Z                   (** integer *)
  | R                   (** real *)
  | I of Ctypes.c_int   (** C-ints *)
  | F of Ctypes.c_float (** C-floats *)
  | A                   (** Abstract Data *)

type driver
val driver: driver Context.value
val new_driver: ?includes:string list -> id:string -> descr:string -> unit
(** reset the context to an empty driver *)

val id : driver -> string
val descr : driver -> string
val is_default : driver -> bool
val compare : driver -> driver -> int

val find_lib: string -> string
(** find a file in the includes of the current drivers *)

val dependencies : string -> string list (** Of external theories. 
					     Raises Not_found if undefined *)

val add_library : string -> string list -> unit (** External theories *)

val add_builtin : string -> kind list -> lfun -> unit

val add_alias : string -> kind list -> alias:string -> unit -> unit

val add_type : string -> library:string ->
  ?link:string infoprover -> unit -> unit

val add_ctor : string -> kind list ->
  library:string -> link:Qed.Engine.link infoprover -> unit -> unit

val add_logic : kind -> string -> kind list -> 
  library:string -> ?category:category -> 
  link:Qed.Engine.link infoprover -> unit -> unit

val add_predicate : string -> kind list ->
  library:string -> link:string infoprover -> unit -> unit

val add_option :
  driver_dir:string -> string -> string -> library:string -> string -> unit
  (** add a value to an option (group, name) *)
val set_option :
  driver_dir:string -> string -> string -> library:string -> string -> unit
  (** reset and add a value to an option (group, name) *)

type doption

val create_option:
  (driver_dir:string -> string -> string) -> string -> string -> doption
(** [add_option_sanitizer ~driver_dir group name]
    add a sanitizer for group [group] and option [name] *)

val get_option : doption -> library:string -> string list
  (** return the values of option (group, name),
      return the empty list if not set *)

type builtin =
  | ACSLDEF
  | LFUN of lfun
  | CONST of F.term

val logic : logic_info -> builtin
val ctor : logic_ctor_info -> builtin
val constant : string -> builtin

val dump : unit -> unit
