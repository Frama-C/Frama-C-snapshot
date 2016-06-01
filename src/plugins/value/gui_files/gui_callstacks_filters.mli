(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
val is_reachable_stmt: filter -> Cil_types.stmt -> bool
val is_non_terminating_instr: filter -> Cil_types.stmt -> bool

(** This function must be called when callstacks are focused. The callstacks
    are used by some dynamic functions like [lval_to_zone_gui] *)
val set_callstacks_filter: filter -> unit
