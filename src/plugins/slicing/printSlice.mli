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
