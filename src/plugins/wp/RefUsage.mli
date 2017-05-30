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

(* -------------------------------------------------------------------------- *)
(* --- Variable Analysis                                                  --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

type access =
  | NoAccess
  | ByRef     (* The expression ["*x"], equal to [load(load(&x))] *)
  | ByArray   (* The expression ["x[_]"], equal to [load(shift(load(&x),_))] *)
  | ByValue   (* The expression ["x"], equal to [load(&x)] *)
  | ByAddr    (* The expression ["&x"] *)

val get : ?kf:kernel_function -> ?init:bool -> varinfo -> access

val iter: ?kf:kernel_function -> ?init:bool -> (varinfo -> access -> unit) -> unit

val dump : unit -> unit
val compute : unit -> unit
