(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
