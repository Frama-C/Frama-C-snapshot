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

module Enabled: Plugin.Bool

module DoUnsignedOverflow : Plugin.Bool

module DoAll: Plugin.Bool

module Print: Plugin.Bool

module DoSignedOverflow : Plugin.Bool

module DoDownCast : Plugin.Bool

module DoDivMod : Plugin.Bool

module DoMemAccess : Plugin.Bool

module ConstFold : Plugin.Bool

module Warn : Plugin.Bool

module DoCalledPrecond : Plugin.Bool

module FunctionSelection : Plugin.String_set

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
