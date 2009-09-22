(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
  module Calls: Plugin.STRING_SET
  module Return: Plugin.STRING_SET
  module Threat: Plugin.STRING_SET
  module Assert: Plugin.STRING_SET
  module Pragma: Plugin.STRING_SET
  module LoopInv: Plugin.STRING_SET
  module LoopVar: Plugin.STRING_SET
  module RdAccess: Plugin.STRING_SET
  module WrAccess: Plugin.STRING_SET
  module Value: Plugin.STRING_SET
end
  
module Mode : sig
  module Callers: Plugin.BOOL
  module Calls: Plugin.INT
  module SliceUndef: Plugin.BOOL
  module KeepAnnotations: Plugin.BOOL
end
  
module Print: Plugin.BOOL
  
val is_on: unit -> bool
