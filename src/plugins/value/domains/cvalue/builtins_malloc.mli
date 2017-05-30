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

(** Dynamic allocation related builtins.
    Most functionality is exported as builtins. *)

val malloced_bases: unit -> Base.Hptset.t
(** All bases that have been dynamically created in the current execution. *)

val alloc_size_ok: Cvalue.V.t -> Alarmset.status
(* [alloc_size_ok size] checks that [size] represents a valid allocation
   size w.r.t. the total address space. [True] means that the requested size is
   small enough, [False] that the allocation is guaranteed to fail (because
   the size is always greater than SIZE_MAX). *)

(**/**)
val register_malloced_base: Base.t -> unit
(* Should not be used by casual users. *)
