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

(** Slicing *)
(* include Log.Messages *)
include Plugin.S

(* modules related to the command line options *)
module Select : sig
  module Calls: Parameter_sig.Kernel_function_set
  module Return: Parameter_sig.Kernel_function_set
  module Threat: Parameter_sig.Kernel_function_set
  module Assert: Parameter_sig.Kernel_function_set
  module Pragma: Parameter_sig.Kernel_function_set
  module LoopInv: Parameter_sig.Kernel_function_set
  module LoopVar: Parameter_sig.Kernel_function_set
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
compile-command: "make -C ../../.."
End:
*)
