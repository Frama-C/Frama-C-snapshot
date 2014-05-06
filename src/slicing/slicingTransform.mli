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

(** Export a CIL application from a slicing project *)

val default_slice_names:(Cil_types.kernel_function -> bool  -> int -> string)

(** Apply the actions still waiting in the project
* and transform the program (CIL AST) using slicing results
* Can optionally specify how to name the sliced functions using [f_slice_names].
* (see db.mli)
*)
val extract :
  f_slice_names:(Cil_types.kernel_function -> bool  -> int -> string)
  -> string -> Db.Slicing.Project.t -> Project.t

(** Return [true] if the source function is called
* (even indirectly via transitivity) from a [Slice.t]. *)
val is_src_fun_called :
    Db.Slicing.Project.t -> Cil_types.kernel_function -> bool

(** Return [true] if the source function is visible
* (even indirectly via transitivity) from a [Slice.t]. *)
val is_src_fun_visible :
    Db.Slicing.Project.t -> Cil_types.kernel_function -> bool
