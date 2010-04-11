(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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


module ForceValues: Plugin.BOOL

module PropagateTop: Plugin.BOOL

module AutomaticContextMaxDepth: Plugin.INT
module AutomaticContextMaxWidth: Plugin.INT

module AllRoundingModes: Plugin.BOOL

module MemFunctions: Plugin.STRING_SET
module MemExecAll: Plugin.BOOL

module NoResultsFunctions: Plugin.STRING_SET
module NoResultsAll: Plugin.BOOL

module SignedOverflow: Plugin.BOOL

module MemoryFootprint: Plugin.INT

module SemanticUnrollingLevel: Plugin.INT
module AllocatedContextValid: Plugin.BOOL

module ArrayPrecisionLevel: Plugin.INT

module WideningLevel: Plugin.INT
module SlevelFunction: Plugin.STRING_HASHTBL with type value = int

module Subdivide_float_in_expr: Plugin.INT
