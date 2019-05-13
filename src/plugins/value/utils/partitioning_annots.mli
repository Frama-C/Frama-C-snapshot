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

type slevel_annotation =
  | SlevelMerge
  | SlevelDefault
  | SlevelLocal of int

type unroll_annotation = Cil_types.term option

type flow_annotation =
  | FlowSplit of Cil_types.term
  | FlowMerge of Cil_types.term

val get_slevel_annot : Cil_types.stmt -> slevel_annotation option
val get_unroll_annot : Cil_types.stmt -> unroll_annotation list
val get_flow_annot : Cil_types.stmt -> flow_annotation list

val add_slevel_annot : emitter:Emitter.t -> loc:Cil_types.location ->
  Cil_types.stmt -> slevel_annotation -> unit
val add_unroll_annot : emitter:Emitter.t -> loc:Cil_types.location ->
  Cil_types.stmt -> unroll_annotation -> unit
val add_flow_annot : emitter:Emitter.t -> loc:Cil_types.location ->
  Cil_types.stmt -> flow_annotation -> unit
