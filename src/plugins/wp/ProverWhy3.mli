(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

val add_specific_equality:
  for_tau:(Lang.tau -> bool) ->
  mk_new_eq:Lang.F.binop ->
  unit
(** Equality used in the goal, simpler to prove than polymorphic equality *)

val prove : ?timeout:int -> ?steplimit:int -> prover:Why3Provers.t ->
  Wpo.t -> VCS.result Task.task
(** Return NoResult if it is already proved by Qed *)

type mode = NoCache | Update | Replay | Rebuild | Offline | Cleanup

val set_mode : mode -> unit
val get_mode : unit -> mode
val get_hits : unit -> int
val get_miss : unit -> int
val get_removed : unit -> int

val cleanup_cache : mode:mode -> unit

(**************************************************************************)
