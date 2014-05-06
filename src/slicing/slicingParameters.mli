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

(** Slicing *)
(* include Log.Messages *)
include Plugin.S

(* modules related to the command line options *)
module Select : sig
  module Calls: Parameter_sig.String_set
  module Return: Parameter_sig.String_set
  module Threat: Parameter_sig.String_set
  module Assert: Parameter_sig.String_set
  module Pragma: Parameter_sig.String_set
  module LoopInv: Parameter_sig.String_set
  module LoopVar: Parameter_sig.String_set
  module RdAccess: Parameter_sig.String_set
  module WrAccess: Parameter_sig.String_set
  module Value: Parameter_sig.String_set
end

module Mode : sig
  module Callers: Parameter_sig.Bool
  module Calls: Parameter_sig.Int
  module SliceUndef: Parameter_sig.Bool
  module KeepAnnotations: Parameter_sig.Bool
end

(** @since Carbon-20110201 *)
module ProjectName: Parameter_sig.String

(** @since Carbon-20110201 *)
module ExportedProjectPostfix: Parameter_sig.String

module Print: Parameter_sig.Bool

val is_on: unit -> bool
val set_off: unit -> unit
val clear: unit -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
