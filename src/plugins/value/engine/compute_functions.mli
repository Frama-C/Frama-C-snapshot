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

(** Value analysis of entire functions, using Eva engine. *)

open Cil_types
open Eval

module Make
    (Abstract: Abstractions.S)
    (Eva: Evaluation.S with type value = Abstract.Val.t
                        and type origin = Abstract.Dom.origin
                        and type loc = Abstract.Loc.location
                        and type state = Abstract.Dom.t)
  : sig

    (** Compute a call to the main function. *)
    val compute_from_entry_point: kernel_function -> lib_entry:bool -> unit

    (** Compute a call to the main function from the given initial state. *)
    val compute_from_init_state: kernel_function -> Abstract.Dom.t -> unit

    val initial_state: lib_entry:bool -> Abstract.Dom.t or_bottom
  end
