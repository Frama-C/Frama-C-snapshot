(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Provided services for kernel developers.
    @plugin development guide *)

(* ************************************************************************* *)
(** {2 Log Machinery} *)
(* ************************************************************************* *)

include Plugin.S

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

module PrintConfig: Parameter_sig.Bool
  (** Behavior of option "-print-config" *)

module PrintVersion: Parameter_sig.Bool
  (** Behavior of option "-print-version" *)

module PrintShare: Parameter_sig.Bool
  (** Behavior of option "-print-share-path" *)

module PrintLib: Parameter_sig.Bool
  (** Behavior of option "-print-lib-path" *)

module PrintPluginPath: Parameter_sig.Bool
  (** Behavior of option "-print-plugin-path" *)

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

module GeneralVerbose: Parameter_sig.Int
  (** Behavior of option "-verbose" *)

module GeneralDebug: Parameter_sig.Int
  (** Behavior of option "-debug" *)

module Quiet: Parameter_sig.Bool
  (** Behavior of option "-quiet" *)

(** @plugin development guide *)
module Unicode: sig
  include Parameter_sig.Bool
  val without_unicode: ('a -> 'b) -> 'a -> 'b
  (** Execute the given function as if the option [-unicode] was not set. *)
end
(** Behavior of option "-unicode".
    @plugin development guide *)

module UseUnicode: Parameter_sig.Bool
  (** Behavior of option "-unicode"
      @deprecated since Nitrogen-20111001 use module {!Unicode} instead. *)

module Time: Parameter_sig.String
  (** Behavior of option "-time" *)

module Collect_messages: Parameter_sig.Bool
(** Behavior of option "-collect-messages" *)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

module PrintCode : Parameter_sig.Bool
  (** Behavior of option "-print" *)

module PrintComments: Parameter_sig.Bool
  (** Behavior of option "-keep-comments" *)

(** Behavior of option "-ocode".
    @plugin development guide *)
module CodeOutput : sig
  include Parameter_sig.String
  val output: (Format.formatter -> unit) -> unit
end

(** Behavior of option "-add-symbolic-path"
    @since Neon-20140301 *)
module SymbolicPath: Parameter_sig.String_set

module FloatNormal: Parameter_sig.Bool
  (** Behavior of option "-float-normal" *)

module FloatRelative: Parameter_sig.Bool
  (** Behavior of option "-float-relative" *)

module FloatHex: Parameter_sig.Bool
  (** Behavior of option "-float-hex" *)

module BigIntsHex: Parameter_sig.Int
  (** Behavior of option "-hexadecimal-big-integers" *)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

module SaveState: Parameter_sig.String
  (** Behavior of option "-save" *)

module LoadState: Parameter_sig.String
  (** Behavior of option "-load" *)

module LoadModule: Parameter_sig.String_list
  (** Behavior of option "-load-module" *)

(** Kernel for journalization. *)
module Journal: sig

  module Enable: Parameter_sig.Bool
    (** Behavior of option "-journal-enable" *)

  module Name: Parameter_sig.String
    (** Behavior of option "-journal-name" *)

end

module Session_dir: Parameter_sig.String
(** Directory in which session files are searched. 
    @since Neon-20140301 *)

module Config_dir: Parameter_sig.String
(** Directory in which config files are searched. 
    @since Neon-20140301 *)

(* ************************************************************************* *)
(** {2 Customizing Normalization and parsing} *)
(* ************************************************************************* *)

module UnrollingLevel: Parameter_sig.Int
  (** Behavior of option "-ulevel" *)

module UnrollingForce: Parameter_sig.Bool
  (** Behavior of option "-ulevel-force" 
      @since Neon-20140301 *)

(** Behavior of option "-machdep".
    If function [set] is called, then {!File.prepare_from_c_files} must be
    called for well preparing the AST. *)
module Machdep: Parameter_sig.String

(** Behavior of option "-enums" *)
module Enums: Parameter_sig.String

module CppCommand: Parameter_sig.String
  (** Behavior of option "-cpp-command" *)

module CppExtraArgs: Parameter_sig.String_list
  (** Behavior of option "-cpp-extra-args" *)

module CppGnuLike: Parameter_sig.Bool
  (** Behavior of option "-cpp-gnu-like" *)

module FramaCStdLib: Parameter_sig.Bool
  (** Behavior of option "-frama-c-stdlib" *)

module CustomAnnot: Parameter_sig.String
  (** Behavior of option "-custom-annot-char". *)

module ReadAnnot: Parameter_sig.Bool
  (** Behavior of option "-read-annot" *)

module PreprocessAnnot: Parameter_sig.Bool
  (** Behavior of option "-pp-annot" *)

module ContinueOnAnnotError: Parameter_sig.Bool
  (** Behavior of option "-continue-annot-error" *)

module SimplifyCfg: Parameter_sig.Bool
  (** Behavior of option "-simplify-cfg" *)

module KeepSwitch: Parameter_sig.Bool
  (** Behavior of option "-keep-switch" *)

module Keep_unused_specified_functions: Parameter_sig.Bool
(** Behavior of option "-keep-unused-specified-function". *)

module SimplifyTrivialLoops: Parameter_sig.Bool
(** Behavior of option "-simplify-trivial-loops". *)

module Constfold: Parameter_sig.Bool
  (** Behavior of option "-constfold" *)

module InitializedPaddingLocals: Parameter_sig.Bool
  (** Behavior of option "-initialized-padding-locals" *)

module AggressiveMerging: Parameter_sig.Bool
  (** Behavior of option "-aggressive-merging" *)

module RemoveExn: Parameter_sig.Bool
  (** Behavior of option "-remove-exn" *)

(** Analyzed files *)
module Files: Parameter_sig.String_list
(** List of files to analyse *)

module Orig_name: Parameter_sig.Bool
(** Behavior of option "-orig-name" *)

val normalization_parameters: Typed_parameter.t list
(** All the normalization options that influence the AST (in particular,
    changing one will reset the AST entirely *)

module WarnDecimalFloat: Parameter_sig.String
  (** Behavior of option "-warn-decimal-float" *)

module WarnUndeclared: Parameter_sig.Bool
  (** Behavior of option "-warn-call-to-undeclared" *)


(* ************************************************************************* *)
(** {3 Customizing cabs2cil options} *)
(* ************************************************************************* *)

module AllowDuplication: Parameter_sig.Bool
  (** Behavior of option "-allow-duplication". *)

module DoCollapseCallCast: Parameter_sig.Bool
  (** Behavior of option "-collapse-call-cast". 

      If false, the destination of a Call instruction should always have the
      same type as the function's return type.  Where needed, CIL will insert a
      temporary to make this happen.

      If true, the destination type may differ from the return type, so there
      is an implicit cast.  This is useful for analyses involving [malloc],
      because the instruction "T* x = malloc(...);" won't be broken into
      two instructions, so it's easy to find the allocation type.

      This is false by default.  Set to true to replicate the behavior
      of CIL 1.3.5 and earlier. *)

module ForceRLArgEval: Parameter_sig.Bool
  (** Behavior of option "-force-rl-arg-eval". *)

(* ************************************************************************* *)
(** {2 Analysis Behavior of options} *)
(* ************************************************************************* *)

(** Behavior of option "-main".

    You should usually use {!Globals.entry_point} instead of
    {!MainFunction.get} since the first one handles the case where the entry
    point is invalid in the right way. *)
module MainFunction: sig

  include Parameter_sig.String

  (** {2 Internal functions}

      Not for casual users. *)

  val unsafe_set: t -> unit

end

(** Behavior of option "-lib-entry".

    You should usually use {!Globals.entry_point} instead of
    {!LibEntry.get} since the first one handles the case where the entry point
    is invalid in the right way. *)
module LibEntry: sig
  include Parameter_sig.Bool
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module ConstReadonly: Parameter_sig.Bool
  (** Global variables with ["const"] qualifier are constant. 
      See also [Cil.typeHasQualifier] *)

module UnspecifiedAccess: Parameter_sig.Bool
  (** Behavior of option "-unspecified-access" *)

module SafeArrays: Parameter_sig.Bool
  (** Behavior of option "-safe-arrays".
      @plugin development guide *)

module SignedOverflow: Parameter_sig.Bool
  (** Behavior of option "-warn-signed-overflow" *)

module UnsignedOverflow: Parameter_sig.Bool
  (** Behavior of option "-warn-unsigned-overflow" *)

module SignedDowncast: Parameter_sig.Bool
  (** Behavior of option "-warn-signed-downcast" *)

module UnsignedDowncast: Parameter_sig.Bool
  (** Behavior of option "-warn-unsigned-downcast" *)

module AbsoluteValidRange: Parameter_sig.String
  (** Behavior of option "-absolute-valid-range" *)

(*
module FloatFlushToZero: Parameter_sig.Bool
  (** Behavior of option "-float-flush-to-zero" *)
*)

(* ************************************************************************* *)
(** {2 Checks} *)
(* ************************************************************************* *)

module Check: Parameter_sig.Bool
  (** Behavior of option "-check" *)

module Copy: Parameter_sig.Bool
(** Behavior of option "-copy" *)

module TypeCheck: Parameter_sig.Bool
  (** Behavior of option "-typecheck" *)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
