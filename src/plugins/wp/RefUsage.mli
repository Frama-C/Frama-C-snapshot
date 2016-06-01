(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Variable Analysis                                                  --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Cil_types
open Cil_datatype

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
