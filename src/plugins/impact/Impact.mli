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

(* $Id: Impact.mli,v 1.1 2008-04-08 14:59:02 uid568 Exp $ *)

open Cil_types

(** Impact analysis.
    @see <../impact/index.html> internal documentation. *)
module Register : sig
  val compute_pragmas: (unit -> stmt list)
    (** Compute the impact analysis from the impact pragma in the program.
        Print and slice the results according to the parameters -impact-print
        and -impact-slice.
        @return the impacted statements *)
  val from_stmt: (stmt -> stmt list)
    (** Compute the impact analysis of the given statement.
        @return the impacted statements *)
  val from_nodes:
    (kernel_function -> PdgTypes.Node.t list -> PdgTypes.NodeSet.t)
    (** Compute the impact analysis of the given set of PDG nodes,
        that come from the given function.
        @return the impacted nodes *)
end
