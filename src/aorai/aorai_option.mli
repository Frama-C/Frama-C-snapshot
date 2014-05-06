(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

include Plugin.S

module Ltl_File: Parameter_sig.String
module To_Buchi: Parameter_sig.String
module Buchi: Parameter_sig.String
module Ya: Parameter_sig.String
module Output_Spec: Parameter_sig.Bool
module Output_C_File: Parameter_sig.String
module Dot: Parameter_sig.Bool
module DotSeparatedLabels: Parameter_sig.Bool
module AbstractInterpretation: Parameter_sig.Bool
module Axiomatization: Parameter_sig.Bool
module ConsiderAcceptance: Parameter_sig.Bool
module AutomataSimplification: Parameter_sig.Bool
module Test: Parameter_sig.Int
module AddingOperationNameAndStatusInSpecification: Parameter_sig.Bool

(** [true] if the user declares that its ya automaton is deterministic. *)
module Deterministic: State_builder.Ref with type data = bool

val is_on : unit -> bool
val promela_file: unit -> string
val advance_abstract_interpretation: unit -> bool

val emitter: Emitter.t
(** The emitter which emits Aorai annotations.
    @since Oxygen-20120901 *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
