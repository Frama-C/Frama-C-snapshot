(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: stmts_graph.mli,v 1.11 2008/11/18 12:13:41 uid568 Exp $ *)

(** Statements graph. 
    @plugin development guide *)

open Db_types
open Cil_types

val stmt_can_reach: kernel_function -> stmt -> stmt -> bool
  (** [stmt_can_reach kf s1 s2] is [true] iff the control flow can reach
      [s2] starting at [s1] in function [kf]. *)

val reachable_stmts: kernel_function -> stmt -> stmt list
