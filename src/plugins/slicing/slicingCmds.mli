(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(* TODO: This .mli exists mainly to avoid problems with 'make -j'. This API
   is too vast and must be simplified. For example, functions should not
   receive variables as names (ie. strings) but directly as zones, possibly
   with a hint to the function that does to conversion. Also, most functions
   are slightly modified in Register, then registered in Db. This module and
   Register should be fused. *)

val topologic_propagation : Db.Slicing.Project.t -> unit

val select_pdg_nodes :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  PdgTypes.Node.t list ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt :
  Db.Slicing.Select.set ->
  spare:bool ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_func_calls_to :
  Db.Slicing.Select.set ->
  spare:bool ->
  Kernel_function.t ->
  Db.Slicing.Select.set

val select_func_calls_into :
  Db.Slicing.Select.set ->
  spare:bool ->
  Kernel_function.t ->
  Db.Slicing.Select.set

val select_func_zone :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  Locations.Zone.t ->
  kernel_function ->
  Db.Slicing.Select.set

val select_func_return :
  Db.Slicing.Select.set ->
  spare:bool ->
  Kernel_function.t ->
  Db.Slicing.Select.set

val select_stmt_ctrl :
  Db.Slicing.Select.set ->
  spare:bool ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt_zone :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  Locations.Zone.t ->
  before:bool ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt_lval :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  Datatype.String.Set.t ->
  before:bool ->
  stmt ->
  eval:stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt_lval_rw :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  rd:Datatype.String.Set.t ->
  wr:Datatype.String.Set.t ->
  stmt ->
  eval:stmt ->
  Kernel_function.t ->
  Db.Slicing.Select.set

val select_stmt_pred :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  predicate named ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt_term :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  term ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt_annot :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  spare:bool ->
  code_annotation ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_stmt_annots :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  spare:bool ->
  threat:bool ->
  user_assert:bool ->
  slicing_pragma:bool ->
  loop_inv:bool ->
  loop_var:bool ->
  stmt ->
  kernel_function ->
  Db.Slicing.Select.set

val select_func_annots :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  spare:bool ->
  threat:bool ->
  user_assert:bool ->
  slicing_pragma:bool ->
  loop_inv:bool ->
  loop_var:bool ->
  kernel_function ->
  Db.Slicing.Select.set

val select_func_lval :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  Datatype.String.Set.t ->
  Kernel_function.t ->
  Db.Slicing.Select.set

val select_func_lval_rw :
  Db.Slicing.Select.set ->
  Db.Slicing.Mark.t ->
  rd:Datatype.String.Set.t ->
  wr:Datatype.String.Set.t ->
  eval:stmt -> Kernel_function.t -> Db.Slicing.Select.set

val add_selection : Db.Slicing.Project.t -> Db.Slicing.Select.set -> unit
val add_persistent_selection :
  Db.Slicing.Project.t -> Db.Slicing.Select.set -> unit
val add_persistent_cmdline : Db.Slicing.Project.t -> unit

val apply_all : Db.Slicing.Project.t -> propagate_to_callers:bool -> unit
