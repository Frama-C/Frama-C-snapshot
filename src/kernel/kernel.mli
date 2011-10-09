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

(** Provided services for kernel developers. *)

(* ************************************************************************* *)
(** {2 Log Machinery} *)
(* ************************************************************************* *)

include Plugin.S

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

module PrintVersion: Plugin.Bool
  (** Behavior of option "-version" *)

module PrintShare: Plugin.Bool
  (** Behavior of option "-print-share-path" *)

module PrintLib: Plugin.Bool
  (** Behavior of option "-print-lib-path" *)

module PrintPluginPath: Plugin.Bool
  (** Behavior of option "-print-plugin-path" *)

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

module GeneralVerbose: Plugin.Int
  (** Behavior of option "-verbose" *)

module GeneralDebug: Plugin.Int
  (** Behavior of option "-debug" *)

module Quiet: Plugin.Bool
  (** Behavior of option "-quiet" *)

module Unicode: sig
  include Plugin.Bool
  val without_unicode: ('a -> 'b) -> 'a -> 'b
  (** Execute the given function as if the option [-unicode] was not set. *)
end
(** Behavior of option "-unicode" *)

module UseUnicode: Plugin.Bool
  (** Behavior of option "-unicode"
      @deprecated since Nitrogen-20111001 use module {!Unicode} instead.
      @plugin development guide *)

module Time: Plugin.String
  (** Behavior of option "-time" *)

module Collect_messages: Plugin.Bool
(** Behavior of option "-collect-messages" *)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

module PrintCode : Plugin.Bool
  (** Behavior of option "-print" *)

module PrintComments: Plugin.Bool
  (** Behavior of option "-keep-comments" *)

(** Behavior of option "-ocode" *)
module CodeOutput : sig
  include Plugin.String
  val output: (Format.formatter -> unit) -> unit
end

module FloatNormal: Plugin.Bool
  (** Behavior of option "-float-normal" *)

module FloatRelative: Plugin.Bool
  (** Behavior of option "-float-relative" *)

module FloatHex: Plugin.Bool
  (** Behavior of option "-float-hex" *)

module BigIntsHex: Plugin.Int
  (** Behavior of option "-hexadecimal-big-integers" *)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

module SaveState: Plugin.String
  (** Behavior of option "-save" *)

module LoadState: Plugin.String
  (** Behavior of option "-load" *)

module AddPath: Plugin.String_list
  (** Behavior of option "-add-path" *)

module LoadModule: Plugin.String_set
  (** Behavior of option "-load-module" *)

module LoadScript: Plugin.String_set
  (** Behavior of option "-load-script" *)

module Dynlink: Plugin.Bool
  (** Behavior of option "-dynlink" *)

(** Kernel for journalization. *)
module Journal: sig

  module Enable: Plugin.Bool
    (** Behavior of option "-journal-enable" *)

  module Name: Plugin.String
    (** Behavior of option "-journal-name" *)

end

(* ************************************************************************* *)
(** {2 Customizing Normalization} *)
(* ************************************************************************* *)

module UnrollingLevel: Plugin.Int
  (** Behavior of option "-ulevel" *)

(** Behavior of option "-machdep".
    If function [set] is called, then {!File.prepare_from_c_files} must be
    called for well preparing the AST. *)
module Machdep: Plugin.String

module CppCommand: Plugin.String
  (** Behavior of option "-cpp-command" *)

module CppExtraArgs: Plugin.String_set
  (** Behavior of option "-cpp-extra-args" *)

module ReadAnnot: Plugin.Bool
  (** Behavior of option "-read-annot" *)

module PreprocessAnnot: Plugin.Bool
  (** Behavior of option "-pp-annot" *)

module TypeCheck: Plugin.Bool
  (** Behavior of option "-type-check" *)

module ContinueOnAnnotError: Plugin.Bool
  (** Behavior of option "-continue-annot-error" *)

module SimplifyCfg: Plugin.Bool
  (** Behavior of option "-simplify-cfg" *)

module KeepSwitch: Plugin.Bool
  (** Behavior of option "-keep-switch" *)

module Constfold: Plugin.Bool
  (** Behavior of option "-constfold" *)

(** Analyzed files *)
module Files: sig

  include Plugin.String_list
    (** List of files to analyse *)

  module Check: Plugin.Bool
    (** Behavior of option "-check" *)

  module Copy: Plugin.Bool
    (** Behavior of option "-copy" *)

  module Orig_name: Plugin.Bool
    (** Behavior of option "-orig-name" *)

end

val normalization_parameters: Parameter.t list
(** All the normalization options that influence the AST (in particular,
    changing one will reset the AST entirely *)

(* ************************************************************************* *)
(** {3 Customizing cabs2cil options} *)
(* ************************************************************************* *)

module AllowDuplication: Plugin.Bool
  (** Behavior of option "-allow-duplication". *)

module DoCollapseCallCast: Plugin.Bool
  (** Behavior of option "-collapse-call-cast". *)

module ForceRLArgEval: Plugin.Bool
  (** Behavior of option "-force-rl-arg-eval". *)

(* ************************************************************************* *)
(** {2 Analysis Behavior of options} *)
(* ************************************************************************* *)

(** Behavior of option "-main".

    You should usually use {!Globals.entry_point} instead of
    {!MainFunction.get} since the first one handles the case where the entry
    point is invalid in the right way. *)
module MainFunction: sig

  include Plugin.String

  (** {2 Internal functions}

      Not for casual users. *)

  val unsafe_set: t -> unit

end

(** Behavior of option "-lib-entry".

    You should usually use {!Globals.entry_point} instead of
    {!LibEntry.get} since the first one handles the case where the entry point
    is invalid in the right way. *)
module LibEntry: sig
  include Plugin.Bool
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module UnspecifiedAccess: Plugin.Bool
  (** Behavior of option "-unspecified-access" *)

module ArrayPrecisionLevel: Plugin.Int
  (** Temporary option to voluntarily approximate
      results of accesses at an imprecise index
      for the sake of speed. *)

module PreciseUnions: Plugin.Bool
  (** Temporary option to produce precise results
      when accessing type-punned data. *)

module Overflow: Plugin.Bool
  (** Behavior of option "-overflow" *)

module StopAtFirstAlarm: Plugin.Bool
  (** Stop propagation at first alarm *)

module SafeArrays: Plugin.Bool
  (** Behavior of option "-safe-arrays" *)

module AbsoluteValidRange: Plugin.String
  (** Behavior of option "-absolute-valid-range" *)

(*
module FloatFlushToZero: Plugin.Bool
  (** Behavior of option "-float-flush-to-zero" *)
*)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
