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
(* --- Logic Path and Regions                                             --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F
open Vset

(** {2 Paths} *)

type path = offset list
and offset = 
  | Oindex of term
  | Ofield of field

val access : term -> path -> term
val update : term -> path -> term -> term

(** {2 Regions} *)

type rpath = roffset list
and roffset = 
  | Rindex of set
  | Rfield of field

type region

val empty : region
val full : region
val path : path -> region (** Empty, but Full for the path *)
val rpath : rpath -> region (** Empty, but Full for the r-paths *)
val merge : region -> region -> region

val disjoint : region -> region -> pred
val subset : region -> region -> pred
val equal_but : tau -> region -> term -> term -> pred

val vars : region -> Vars.t
val occurs : var -> region -> bool
val pretty : Format.formatter -> region -> unit
