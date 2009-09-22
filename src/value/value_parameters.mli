(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

module WarnUnspecifiedOrder: Plugin.BOOL
  (** Warns for unspecified sequences containing at least one writes *)

module PropagateTop: Plugin.BOOL

module AutomaticContextMaxDepth: Plugin.INT
module AutomaticContextMaxWidth: Plugin.INT

module MemFunctions: Plugin.STRING_SET
module MemExecAll: Plugin.BOOL

module KeepOnlyLastRun: Plugin.BOOL
  (** Keep only last run of value analysis. This is a debugging option. *)

module MemoryFootprint: Plugin.INT

module SemanticUnrollingLevel: Plugin.INT
module AllocatedContextValid: Plugin.BOOL

module ArrayPrecisionLevel: Plugin.INT

module WideningLevel: Plugin.INT






