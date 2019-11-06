(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Sparecode analysis. *)
(** Interface for the unused code detection.
    @see <../sparecode/index.html> internal documentation. *)

module Register: sig
  val get: select_annot:bool -> select_slice_pragma:bool -> Project.t
  (** Remove in each function what isn't used to compute its outputs,
   *   or its annotations when [select_annot] is true,
   *   or its slicing pragmas when [select_slice_pragmas] is true.
   *  @return a new project where the sparecode has been removed.
  *)
  val rm_unused_globals : ?new_proj_name:string -> ?project:Project.t -> unit -> Project.t
  (** Remove  unused global types and variables from the given project
    * (the current one if no project given).
    * The source project is not modified.
    * The result is in the returned new project.
    * @modify Carbon-20110201 optional argument [new_proj_name] added
    * *)
end
