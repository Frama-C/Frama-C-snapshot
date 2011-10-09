(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

include Plugin.S

module ForceValues: Plugin.WithOutput

module PropagateTop: Plugin.Bool

module AutomaticContextMaxDepth: Plugin.Int
module AutomaticContextMaxWidth: Plugin.Int

module SeparateStmtStart:  Plugin.String_set
module SeparateStmtWord:  Plugin.Int
module SeparateStmtOf:  Plugin.Int

module AllRoundingModes: Plugin.Bool

module NoResultsFunctions: Plugin.String_set
module NoResultsAll: Plugin.Bool

module ObviouslyTerminatesAll: Plugin.Bool
module ObviouslyTerminatesFunctions: Plugin.String_set

module ResultsAfter: Plugin.Bool

module SignedOverflow: Plugin.Bool

module IgnoreRecursiveCalls: Plugin.Bool
module ValShowProgress: Plugin.Bool

module MemoryFootprint: Plugin.Int

module SemanticUnrollingLevel: Plugin.Int

module AllocatedContextValid: Plugin.Bool

module UndefinedPointerComparisonPropagateAll: Plugin.Bool

module WideningLevel: Plugin.Int
module SlevelFunction: Plugin.String_hashtbl with type value = int

module UsePrototype: Plugin.String_set

module RmAssert: Plugin.Bool

module Subdivide_float_in_expr: Plugin.Int
module BuiltinsOverrides: Plugin.String_hashtbl with type value = string

module PrintCallstacks: Plugin.Bool

val parameters_correctness: Parameter.t list
val parameters_tuning: Parameter.t list

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
