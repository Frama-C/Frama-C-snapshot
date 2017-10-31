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

(** Computations of the statements that write a given memory zone. *)

(** Given an effect [e], something is directly modified by [e] (through an
    affectation, or through a call to a leaf function) if [direct] holds, and
    indirectly (through the effects of a call) otherwise.  *)
type effects = {
  direct: bool (** Direct affectation [lv = ...], or modification through
                   a call to a leaf function. *);
  indirect: bool (** Modification inside the body of called function [f(...)]*);
}

val compute: Locations.Zone.t -> (Cil_types.stmt * effects) list
(** [compute z] finds all the statements that modifies [z], and for each
    statement, indicates whether the modification is direct or indirect. *)
