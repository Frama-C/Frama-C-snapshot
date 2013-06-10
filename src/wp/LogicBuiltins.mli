(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

val dependencies : string -> string list (** Of external theories. 
					     Raises Not_found if undefined *)

val add_library : string -> string list -> unit (** External theories *)

val add_const : string -> F.term -> unit

val add_type : string -> theory:string -> ?link:string -> unit -> unit

val add_ctor : string -> kind list ->
  theory:string -> ?link:string -> unit -> unit

val add_logic : kind -> string -> kind list -> 
  theory:string -> ?category:category -> 
  ?balance:Lang.balance -> ?link:string -> unit -> unit

val add_predicate : string -> kind list ->
  theory:string -> ?link:string -> unit -> unit

val symbol : string -> lfun

type builtin =
  | ACSLDEF
  | LFUN of lfun
  | CONST of F.term

val logic : logic_info -> builtin
val ctor : logic_ctor_info -> builtin

val dump : unit -> unit
