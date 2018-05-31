(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Make the property statuses of the initial project accessible when
    doing the main translation *)

type kind =
  | K_Assert
  | K_Invariant
  | K_Variant
  | K_StmtSpec
  | K_Allocation
  | K_Assigns
  | K_Decreases
  | K_Terminates (* TODO: should be removed: not part of the E-ACSL subset *)
  | K_Complete
  | K_Disjoint
  | K_Requires
  | K_Ensures

val clear: unit -> unit
(** to be called before any program transformation *)

val push: Kernel_function.t -> kind -> Property.t -> unit
(** store the given property of the given kind for the given function *)

val before_translation: unit -> unit
(** to be called just before the main translation *)

val must_translate: Kernel_function.t -> kind -> bool
(** To be called just before transforming a property of the given kind for the
    given function.
    VERY IMPORTANT: the property of the n-th call to this function exactly
    correspond to the n-th pushed property (see {!push}).
    @return true if and only if the translation must occur. *)
