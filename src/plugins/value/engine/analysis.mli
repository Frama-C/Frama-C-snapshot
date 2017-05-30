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

open Cil_types
open Eval

module type Results = sig
  type state
  type value

  val get_stmt_state : stmt -> state or_bottom
  val eval_expr : state -> exp -> value evaluated
end


module Make (Abstract: Abstractions.S) : sig

  val compute_from_entry_point : kernel_function -> lib_entry:bool -> unit
  val compute_from_init_state: kernel_function -> Abstract.Dom.t -> unit

  include Results with type state := Abstract.Dom.state
                   and type value := Abstract.Val.t
end


module type S = sig
  include Abstractions.S
  include Results with type state := Dom.state
                   and type value := Val.t
end

val current : (module S) ref
(** The abstractions used in the latest analysis, and its results. *)

val compute : Abstractions.config -> lib_entry:bool -> kernel_function -> unit
(** Perform a full analysis, starting from the given kernel_function and with
    the abstractions specified by the configuration. *)

val force_compute : unit -> unit
(** Perform a full analysis, starting from the [main] function. *)

val cvalue_initial_state: unit -> Cvalue.Model.t
(** Return the initial state of the cvalue domain only. *)
