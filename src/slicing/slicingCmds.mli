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

open Cil_types
open Db

(* TODO: This .mli exists mainly to avoid problems with 'make -j'. This API
   is too vast and must be simplified. For example, functions should not
   receive variables as names (ie. strings) but directly as zones, possibly
   with a hint to the function that does to conversion. Also, most functions
   are slightly modified in Register, then registered in Db. This module and
   Register should be fused. *)

val topologic_propagation : Slicing.Project.t -> unit

val select_pdg_nodes :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  PdgTypes.Node.t list ->
  kernel_function ->
  Slicing.Select.set

val select_stmt :
  Slicing.Select.set ->
  spare:bool ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_func_calls_to :
  Slicing.Select.set ->
  spare:bool ->
  Kernel_function.t ->
  Slicing.Select.set

val select_func_calls_into :
  Slicing.Select.set ->
  spare:bool ->
  Kernel_function.t ->
  Slicing.Select.set

val select_func_zone :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  Locations.Zone.t ->
  kernel_function ->
  Slicing.Select.set

val select_func_return :
  Slicing.Select.set ->
  spare:bool ->
  Kernel_function.t ->
  Slicing.Select.set

val select_stmt_ctrl :
  Slicing.Select.set ->
  spare:bool ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_stmt_zone :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  Locations.Zone.t ->
  before:bool ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_stmt_lval :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  Datatype.String.Set.t ->
  before:bool ->
  stmt ->
  scope:stmt ->
  eval:stmt ->
  kernel_function ->
  Slicing.Select.set

val select_stmt_lval_rw :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  rd:Datatype.String.Set.t ->
  wr:Datatype.String.Set.t ->
  stmt ->
  scope:stmt ->
  eval:stmt ->
  Kernel_function.t ->
  Slicing.Select.set

val select_stmt_pred :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  predicate named ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_stmt_term :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  term ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_stmt_annot :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  spare:bool ->
  code_annotation ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_stmt_annots :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  spare:bool ->
  threat:bool ->
  user_assert:bool ->
  slicing_pragma:bool ->
  loop_inv:bool ->
  loop_var:bool ->
  stmt ->
  kernel_function ->
  Slicing.Select.set

val select_func_annots :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  spare:bool ->
  threat:bool ->
  user_assert:bool ->
  slicing_pragma:bool ->
  loop_inv:bool ->
  loop_var:bool ->
  kernel_function ->
  Slicing.Select.set

val select_func_lval :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  Datatype.String.Set.t ->
  Kernel_function.t ->
  Slicing.Select.set

val select_func_lval_rw :
  Slicing.Select.set ->
  Slicing.Mark.t ->
  rd:Datatype.String.Set.t ->
  wr:Datatype.String.Set.t ->
  scope:stmt ->
  eval:stmt -> Kernel_function.t -> Slicing.Select.set

val add_selection : Slicing.Project.t -> Slicing.Select.set -> unit
val add_persistent_selection :
  Slicing.Project.t -> Slicing.Select.set -> unit
val add_persistent_cmdline : Slicing.Project.t -> unit

val apply_all : Slicing.Project.t -> propagate_to_callers:bool -> unit
