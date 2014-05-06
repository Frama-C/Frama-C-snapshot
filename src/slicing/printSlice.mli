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

val print_fct_from_pdg :
  Format.formatter ->
  ?ff:SlicingInternals.fct_slice -> PdgTypes.Pdg.t -> unit

val print_marked_ff : Format.formatter -> SlicingInternals.fct_slice -> unit

val print_original_glob : Format.formatter -> Cil_types.global -> unit

val print_fct_stmts :
  Format.formatter ->
  (SlicingTypes.sl_project * Cil_types.kernel_function) ->
  unit

val build_dot_project : string -> string -> SlicingInternals.project -> unit
