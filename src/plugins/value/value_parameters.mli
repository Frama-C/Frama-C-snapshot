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

include Plugin.General_services

module ForceValues: Parameter_sig.With_output

module Eva: Parameter_sig.Bool

module EnumerateCond: Parameter_sig.Bool
module OracleDepth: Parameter_sig.Int
module ReductionDepth: Parameter_sig.Int

module CvalueDomain: Parameter_sig.Bool
module EqualityDomain: Parameter_sig.Bool
module BitwiseOffsmDomain: Parameter_sig.Bool

module ApronOctagon: Parameter_sig.Bool
module ApronBox: Parameter_sig.Bool
module PolkaLoose: Parameter_sig.Bool
module PolkaStrict: Parameter_sig.Bool
module PolkaEqualities: Parameter_sig.Bool


module AutomaticContextMaxDepth: Parameter_sig.Int
module AutomaticContextMaxWidth: Parameter_sig.Int

module SeparateStmtStart:  Parameter_sig.String_set
module SeparateStmtWord:  Parameter_sig.Int
module SeparateStmtOf:  Parameter_sig.Int

module AllRoundingModes: Parameter_sig.Bool
module AllRoundingModesConstants: Parameter_sig.Bool

module NoResultsFunctions: Parameter_sig.Fundec_set
module NoResultsAll: Parameter_sig.Bool

module ResultsCallstack: Parameter_sig.Bool
module JoinResults: Parameter_sig.Bool

module WarnLeftShiftNegative: Parameter_sig.Bool
module WarnPointerSubstraction: Parameter_sig.Bool
module WarnCopyIndeterminate: Parameter_sig.Kernel_function_set

module IgnoreRecursiveCalls: Parameter_sig.Bool

module MemoryFootprint: Parameter_sig.Int

module SemanticUnrollingLevel: Parameter_sig.Int
module SlevelFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                    and type value = int

module SlevelMergeAfterLoop: Parameter_sig.Kernel_function_set
module WideningLevel: Parameter_sig.Int
module ArrayPrecisionLevel: Parameter_sig.Int

module AllocatedContextValid: Parameter_sig.Bool
module InitializationPaddingGlobals: Parameter_sig.String

module SaveFunctionState:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = string
module LoadFunctionState:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = string
val get_SaveFunctionState : unit -> Cil_types.kernel_function * string
val get_LoadFunctionState : unit -> Cil_types.kernel_function * string

module UndefinedPointerComparisonPropagateAll: Parameter_sig.Bool
module WarnPointerComparison: Parameter_sig.String

module ReduceOnLogicAlarms: Parameter_sig.Bool

module UsePrototype: Parameter_sig.Kernel_function_set

module RmAssert: Parameter_sig.Bool

module LinearLevel: Parameter_sig.Int
module BuiltinsOverrides:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                    and type value = string
module SplitReturnFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                    and type value = Split_strategy.t
module SplitGlobalStrategy: State_builder.Ref with type data = Split_strategy.t

module ValShowProgress: Parameter_sig.Bool
module ValShowInitialState: Parameter_sig.Bool
module FloatTimingStep: State_builder.Ref with type data = float
module ValShowPerf: Parameter_sig.Bool
module ShowSlevel: Parameter_sig.Int
module PrintCallstacks: Parameter_sig.Bool

module MemExecAll: Parameter_sig.Bool


module InterpreterMode: Parameter_sig.Bool
module ObviouslyTerminatesAll: Parameter_sig.Bool
module ObviouslyTerminatesFunctions: Parameter_sig.Fundec_set
module StopAtNthAlarm: Parameter_sig.Int

module ReusedExprs: Parameter_sig.Bool


val parameters_correctness: Typed_parameter.t list
val parameters_tuning: Typed_parameter.t list

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
