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

include Plugin.General_services

module ForceValues: Parameter_sig.With_output

module AutomaticContextMaxDepth: Parameter_sig.Int
module AutomaticContextMaxWidth: Parameter_sig.Int

module SeparateStmtStart:  Parameter_sig.String_set
module SeparateStmtWord:  Parameter_sig.Int
module SeparateStmtOf:  Parameter_sig.Int

module AllRoundingModes: Parameter_sig.Bool
module AllRoundingModesConstants: Parameter_sig.Bool

module NoResultsFunctions: Parameter_sig.String_set
module NoResultsAll: Parameter_sig.Bool

module ResultsAfter: Parameter_sig.Bool
module ResultsCallstack: Parameter_sig.Bool

module WarnLeftShiftNegative: Parameter_sig.Bool
module WarnPointerSubstraction: Parameter_sig.Bool
module WarnCopyIndeterminate: Parameter_sig.String_set

module IgnoreRecursiveCalls: Parameter_sig.Bool

module MemoryFootprint: Parameter_sig.Int

module SemanticUnrollingLevel: Parameter_sig.Int
module SlevelFunction: Parameter_sig.String_hashtbl with type value = int
module SlevelMergeAfterLoop: Parameter_sig.Bool
module WideningLevel: Parameter_sig.Int
module ArrayPrecisionLevel: Parameter_sig.Int

module AllocatedContextValid: Parameter_sig.Bool
module InitializedPaddingGlobals: Parameter_sig.Bool

module UndefinedPointerComparisonPropagateAll: Parameter_sig.Bool


module UsePrototype: Parameter_sig.String_set

module RmAssert: Parameter_sig.Bool

module Subdivide_float_in_expr: Parameter_sig.Int
module BuiltinsOverrides: Parameter_sig.String_hashtbl with type value = string
module SplitReturnFunction: Parameter_sig.String_hashtbl 
  with type value = Split_strategy.t
module SplitReturnAuto: Parameter_sig.Bool

module ValShowProgress: Parameter_sig.Bool
module ValShowInitialState: Parameter_sig.Bool
module FloatTimingStep: State_builder.Ref with type data = float
module ValShowPerf: Parameter_sig.Bool
module ShowSlevel: Parameter_sig.Int
module PrintCallstacks: Parameter_sig.Bool

module MemExecAll: Parameter_sig.Bool


module InterpreterMode: Parameter_sig.Bool
module ObviouslyTerminatesAll: Parameter_sig.Bool
module ObviouslyTerminatesFunctions: Parameter_sig.String_set
module StopAtNthAlarm: Parameter_sig.Int


val parameters_correctness: Typed_parameter.t list
val parameters_tuning: Typed_parameter.t list

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
