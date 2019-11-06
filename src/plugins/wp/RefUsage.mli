(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(* --- Variable Analysis                                                  --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

(** By lattice order of usage *)
type access =
  | NoAccess (** Never used *)
  | ByRef   (** Only used as ["*x"],   equals to [load(shift(load(&x),0))] *)
  | ByArray (** Only used as ["x[_]"], equals to [load(shift(load(&x),_))] *)
  | ByValue (** Only used as ["x"],    equals to [load(&x)] *)
  | ByAddr  (** Widely used, potentially up to ["&x"] *)

val get : ?kf:kernel_function -> ?init:bool -> varinfo -> access

val iter: ?kf:kernel_function -> ?init:bool -> (varinfo -> access -> unit) -> unit

val print : varinfo -> access -> Format.formatter -> unit
val dump : unit -> unit
val compute : unit -> unit
