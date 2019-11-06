(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

val move: Visitor.generic_frama_c_visitor -> old:stmt -> stmt -> unit
(** Move all labels of the [old] stmt onto the new [stmt].
    Both stmts must be in the new project. *)

val get_stmt: Visitor.generic_frama_c_visitor -> logic_label -> stmt
(** @return the statement where the logic label points to. *)

val new_labeled_stmt: stmt -> stmt
(** @return the labeled stmt to use instead of the given one (which
    previously contained a label *)

val self: State.t
(** Internal state *)

(*
Local Variables:
compile-command: "make"
End:
*)
