(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

module EnumerateCond: Parameter_sig.Bool
module OracleDepth: Parameter_sig.Int
module ReductionDepth: Parameter_sig.Int

module CvalueDomain: Parameter_sig.Bool
module EqualityDomain: Parameter_sig.Bool
module GaugesDomain: Parameter_sig.Bool
module SymbolicLocsDomain: Parameter_sig.Bool
module BitwiseOffsmDomain: Parameter_sig.Bool
module InoutDomain: Parameter_sig.Bool
module SignDomain: Parameter_sig.Bool

module ApronOctagon: Parameter_sig.Bool
module ApronBox: Parameter_sig.Bool
module PolkaLoose: Parameter_sig.Bool
module PolkaStrict: Parameter_sig.Bool
module PolkaEqualities: Parameter_sig.Bool

module EqualityStorage: Parameter_sig.Bool
module SymbolicLocsStorage: Parameter_sig.Bool
module GaugesStorage: Parameter_sig.Bool
module ApronStorage: Parameter_sig.Bool
module BitwiseOffsmStorage: Parameter_sig.Bool


module AutomaticContextMaxDepth: Parameter_sig.Int
module AutomaticContextMaxWidth: Parameter_sig.Int

module AllRoundingModes: Parameter_sig.Bool
module AllRoundingModesConstants: Parameter_sig.Bool

module NoResultsFunctions: Parameter_sig.Fundec_set
module NoResultsAll: Parameter_sig.Bool

module ResultsCallstack: Parameter_sig.Bool
module JoinResults: Parameter_sig.Bool

module WarnLeftShiftNegative: Parameter_sig.Bool
module WarnSignedConvertedDowncast: Parameter_sig.Bool
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
module InitializedLocals: Parameter_sig.Bool

module UsePrototype: Parameter_sig.Kernel_function_set

module RmAssert: Parameter_sig.Bool

module LinearLevel: Parameter_sig.Int
module BuiltinsOverrides:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                    and type value = string
module BuiltinsAuto: Parameter_sig.Bool
module BuiltinsList: Parameter_sig.Bool
module SplitReturnFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                    and type value = Split_strategy.t
module SplitGlobalStrategy: State_builder.Ref with type data = Split_strategy.t

module ValShowProgress: Parameter_sig.Bool
module ValShowInitialState: Parameter_sig.Bool
module ValShowPerf: Parameter_sig.Bool
module ValPerfFlamegraphs: Parameter_sig.String
module ShowSlevel: Parameter_sig.Int
module PrintCallstacks: Parameter_sig.Bool
module AlarmsWarnings: Parameter_sig.Bool
module WarnBuiltinOverride: Parameter_sig.Bool

module MemExecAll: Parameter_sig.Bool


module InterpreterMode: Parameter_sig.Bool
module ObviouslyTerminatesAll: Parameter_sig.Bool
module ObviouslyTerminatesFunctions: Parameter_sig.Fundec_set
module StopAtNthAlarm: Parameter_sig.Int


(** Dynamic allocation *)

module MallocFunctions: Parameter_sig.String_set
module MallocReturnsNull: Parameter_sig.Bool
module MallocLevel: Parameter_sig.Int


val parameters_correctness: Typed_parameter.t list
val parameters_tuning: Typed_parameter.t list
val parameters_abstractions: Typed_parameter.t list

(** Debug categories responsible for printing initial and final states of Value.
   Enabled by default, but can be disabled via the command-line:
   -value-msg-key="-initial_state,-final_state" *)
val dkey_initial_state : Log.category
val dkey_final_states : Log.category

(** Debug category used when emitting an alarm in "non-warning" mode *)
val dkey_alarm: Log.category

(** Debug category used to print garbled mix *)
val dkey_garbled_mix: Log.category

(** Debug category used to print information about invalid pointer comparisons*)
val dkey_pointer_comparison: Log.category

(** Debug category used to print the cvalue domain on Frama_C_[dump|show]_each
    functions. *)
val dkey_cvalue_domain: Log.category

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
