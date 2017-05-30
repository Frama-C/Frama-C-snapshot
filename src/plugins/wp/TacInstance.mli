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

(** Built-in Instance Tactical (auto-registered) *)

open Lang.F
open Tactical
open Strategy

val tactical : Tactical.t
val fields : selection field list
val params : parameter list
val filter : var -> term -> bool

type bindings = (var * selection) list

val complexity : bindings -> Integer.t
val cardinal : int -> bindings -> int option
(** less than limit *)

val instance_goal : ?title:string -> bindings -> pred -> Tactical.process
val instance_have : ?title:string -> ?at:int -> bindings -> pred -> Tactical.process
val wrap : selection field list -> selection list -> argument list

(** {2 Strategies} *)

val strategy : ?priority:float -> selection -> selection list -> strategy

(**************************************************************************)
