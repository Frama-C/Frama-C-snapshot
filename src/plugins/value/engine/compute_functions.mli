(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
    (Value: Abstract_value.S)
    (Loc: Abstract_location.External with type value = Value.t)
    (Domain : Abstract_domain.External with type location = Loc.location
                                        and type value = Value.t)
    (Eva: Evaluation.S with type value = Domain.value
                        and type origin = Domain.origin
                        and type loc = Domain.location
                        and type state = Domain.t)
    (Init: Initialization.S with type state := Domain.t)
  : sig

    val compute_from_entry_point: kernel_function -> unit or_bottom

  end


val run:
  (kernel_function -> unit or_bottom) ->
  ?library:bool -> kernel_function -> unit
