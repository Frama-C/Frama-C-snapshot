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

val get : Cil_datatype.Fundec.Set.t -> cast_intro:bool -> Project.t
(** Propagate constant into the functions given by name.
    note: the propagation is performed into all functions when the set is
    empty; and casts can be introduced when [cast_intro] is true. *)

val compute: unit -> unit
(** Propagate constant into the functions given by the parameters (in the
    same way that {!get}. Then pretty print the resulting program.
    @since Beryllium-20090901 *)

val self: State.t
(** Internal state of the constant propagation plugin. *)
