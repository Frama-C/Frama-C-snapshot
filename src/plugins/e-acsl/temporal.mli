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

(** Transformations to detect temporal memory errors (e.g., derererence of
    stale pointers). *)

val enable: bool -> unit
(** Enable/disable temporal transformations *)

val is_enabled: unit -> bool
(** Return a boolean value indicating whether temporal analysis is enabled *)

val handle_function_parameters: Cil_types.kernel_function -> Env.t -> Env.t
(** [handle_function_parameters kf env] updates the local environment [env],
    according to the parameters of [kf], with statements allowing to track
    referent numbers across function calls. *)

val handle_stmt: Cil_types.stmt -> Env.t -> Env.t
(** Update local environment ([Env.t]) with statements tracking temporal
    properties of memory blocks *)

val generate_global_init: Cil_types.varinfo -> Cil_types.offset ->
  Cil_types.init -> Env.t -> Cil_types.stmt option
  (** Generate [Some s], where [s] is a statement tracking global initializer
      or [None] if there is no need to track it *)
