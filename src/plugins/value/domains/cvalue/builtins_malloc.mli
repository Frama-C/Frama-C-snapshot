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

val fold_dynamic_bases: (Base.t -> Value_types.Callstack.t -> 'a -> 'a) -> 'a -> 'a
(** [fold_dynamic_bases f init] folds [f] to each dynamically allocated base,
    with initial accumulator [init].
    Note that this also includes bases created by [alloca] and [VLAs]. *)

val alloc_size_ok: Cvalue.V.t -> Alarmset.status
(* [alloc_size_ok size] checks that [size] represents a valid allocation
   size w.r.t. the total address space. [True] means that the requested size is
   small enough, [False] that the allocation is guaranteed to fail (because
   the size is always greater than SIZE_MAX). *)

val free_automatic_bases: Value_types.Callstack.t -> Cvalue.Model.t -> Cvalue.Model.t
(** Performs the equivalent of [free] for each location that was allocated via
    [alloca()] in the current function (as per [Value_util.call_stack ()]).
    This function must be called during finalization of a function call. *)

(**/**)
val register_malloced_base: ?stack:Value_types.Callstack.t -> Base.t -> unit
(* Should not be used by casual users. *)
