(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open Plugin

include Plugin.S

module Ltl_File : String
module To_Buchi: String
module Buchi: String
module Ya: String
module Output_Spec : Bool
module Output_C_File : String
module Dot : Bool
module DotSeparatedLabels: Bool
module AbstractInterpretation : Bool
module Axiomatization : Bool
module ConsiderAcceptance : Bool
module AutomataSimplification : Bool
module Test : Int
module AddingOperationNameAndStatusInSpecification:Bool

(** [true] if the user declares that its ya automaton is deterministic. *)
module Deterministic: State_builder.Ref with type data = bool

val reset: unit -> unit
(** Resets all options
    @since Nitrogen-20111001
 *)

val is_on : unit -> bool
val promela_file: unit -> string
val advance_abstract_interpretation: unit -> bool

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
