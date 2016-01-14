(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Weak topological ordering of statements. See "Bourdoncle,
    Efficient chaotic iteration strategies with widenings" for a
    complete explanation. *)

open Cil_types

(* This type represents a list; Nil is the empty list, Node conses a
   single element, while Component conses a whole component. Note:
   Bourdoncle paper always has a single element as the header of a
   component, and this type does not enforce this. *)
type wto =
| Nil
| Node of stmt * wto
| Component of wto * wto


(** wto as Datatype *)
module WTO : Datatype.S

(** Returns the depth of a statement *)
val depth_of_stmt : stmt -> int

(** Returns the wto of a kernel function *)
val wto_of_kf : kernel_function -> wto
