(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Internal information about control dependencies *)
type t

(** Compute some information on the function in order to be able to compute
* the control dependencies later on *)
val compute : Kernel_function.t -> t

(** Compute the list of the statements that should have a control dependency
* on the given IF statement. *)
val get_if_controled_stmts : t -> Cil_types.stmt -> Cil_datatype.Stmt.Hptset.t

(** Compute the list of the statements that should have a control dependency
* on the given jump statement. This statement can be a [goto] of course,
* but also a [break], a [continue], or even a loop because CIL transformations
   make them of the form {v while(true) body; v} which is equivalent to
   {v L : body ; goto L; v}
* *)
val get_jump_controled_stmts : t -> Cil_types.stmt -> Cil_datatype.Stmt.Hptset.t
val get_loop_controled_stmts : t -> Cil_types.stmt -> Cil_datatype.Stmt.Hptset.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
