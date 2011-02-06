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

module Enabled: Plugin.BOOL

module DoUnsignedOverflow : Plugin.BOOL

module DoAll: Plugin.BOOL

module Print: Plugin.BOOL

module DoSignedOverflow : Plugin.BOOL

module DoDownCast : Plugin.BOOL

module DoDivMod : Plugin.BOOL

module DoMemAccess : Plugin.BOOL

module ConstFold : Plugin.BOOL

module Warn : Plugin.BOOL

module DoCalledPrecond : Plugin.BOOL

module FunctionSelection : Plugin.STRING_SET
