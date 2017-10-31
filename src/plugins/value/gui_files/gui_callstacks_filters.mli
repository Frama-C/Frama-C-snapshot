(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Filtering on analysis callstacks *)

(** List.rev on a callstack, enforced by strong typing outside of this module *)
type rcallstack

val empty: rcallstack
val from_callstack: Value_types.callstack -> rcallstack


(** Filters on callstacks. [None] means that all callstacks are active *)
type filter = rcallstack list option

val callstack_matches: filter -> rcallstack -> bool
val callsite_matches: filter -> Cil_types.stmt -> bool

(* Callstacks currently being focused. *)
val focused_callstacks: unit -> filter

(* Focuses on the given callstacks. *)
val focus_on_callstacks: filter -> unit

val is_reachable_stmt: filter -> Cil_types.stmt -> bool
val is_non_terminating_instr: filter -> Cil_types.stmt -> bool

(* This function must be called each time a new Gui_eval.S is built over the
   abstractions used for an Eva analysis. It registers the two functions
   [lval_to_zone_gui] and [tlval_to_zone_gui], that depend on the abstractions
   used by the analysis and on the focused callstacks. *)
val register_to_zone_functions : (module Gui_eval.S) -> unit
