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

(** Loop specific actions. *)

open Cil_types

val apply_after_transformation: Project.t -> unit

val mv_invariants: Env.t -> old:stmt -> stmt -> unit
(** Transfer the loop invariants from the [old] loop to the new one.
    Both statements must be loops. *)

val preserve_invariant: 
  Project.t -> Env.t -> Kernel_function.t -> stmt -> stmt * Env.t * bool
(** modify the given stmt loop to insert the code which preserves its loop
    invarariants. Also return the modify environment and a boolean which
    indicates whether the annotations corresponding to the loop invariant must
    be moved from the new statement to the old one. *)

(*
Local Variables:
compile-command: "make"
End:
*)
