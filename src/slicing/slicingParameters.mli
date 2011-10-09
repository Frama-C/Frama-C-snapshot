(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Slicing *)
(* include Log.Messages *)
include Plugin.S

(* modules related to the command line options *)
module Select : sig
  module Calls: Plugin.String_set
  module Return: Plugin.String_set
  module Threat: Plugin.String_set
  module Assert: Plugin.String_set
  module Pragma: Plugin.String_set
  module LoopInv: Plugin.String_set
  module LoopVar: Plugin.String_set
  module RdAccess: Plugin.String_set
  module WrAccess: Plugin.String_set
  module Value: Plugin.String_set
end

module Mode : sig
  module Callers: Plugin.Bool
  module Calls: Plugin.Int
  module SliceUndef: Plugin.Bool
  module KeepAnnotations: Plugin.Bool
end

(** @since Carbon-20110201 *)
module ProjectName: Plugin.String

(** @since Carbon-20110201 *)
module ExportedProjectPostfix: Plugin.String

module Print: Plugin.Bool

val is_on: unit -> bool
val set_off: unit -> unit
val clear: unit -> unit
