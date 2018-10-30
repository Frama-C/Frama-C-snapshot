(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
(** {2 Message and warning categories} *)
(* ************************************************************************* *)

val dkey_alpha: category

val dkey_alpha_undo: category

val dkey_asm_contracts: category

val dkey_ast: category

val dkey_check: category

val dkey_comments: category

val dkey_compilation_db: category

val dkey_dataflow: category

val dkey_dataflow_scc: category

val dkey_dominators: category

val dkey_emitter: category

val dkey_emitter_clear: category

val dkey_exn_flow: category

val dkey_file_transform: category

val dkey_file_print_one: category

val dkey_file_annot: category

val dkey_filter: category

val dkey_globals: category

val dkey_kf_blocks: category

val dkey_linker: category

val dkey_linker_find: category

val dkey_loops: category

val dkey_parser: category

val dkey_pp: category

val dkey_print_attrs: category

val dkey_print_bitfields: category

val dkey_print_builtins: category

val dkey_print_logic_coercions: category

val dkey_print_logic_types: category

val dkey_print_sid: category

val dkey_print_unspecified: category

val dkey_print_vid: category

val dkey_prop_status: category

val dkey_prop_status_emit: category

val dkey_prop_status_merge: category

val dkey_prop_status_graph: category

val dkey_prop_status_reg: category

val dkey_rmtmps: category

val dkey_task: category

val dkey_typing_global: category

val dkey_typing_init: category

val dkey_typing_chunk: category

val dkey_typing_cast: category

val dkey_typing_pragma: category

val dkey_ulevel: category

val dkey_visitor: category

val wkey_annot_error: warn_category
(** error in annotation. If only a warning, annotation will just be ignored. *)

val wkey_drop_unused: warn_category

val wkey_implicit_conv_void_ptr: warn_category

val wkey_incompatible_types_call: warn_category

val wkey_incompatible_pointer_types: warn_category

val wkey_cert_exp_46: warn_category

val wkey_cert_msc_38: warn_category

val wkey_cert_exp_10: warn_category

val wkey_check_volatile: warn_category

val wkey_jcdb: warn_category

val wkey_implicit_function_declaration: warn_category

val wkey_no_proto: warn_category

val wkey_missing_spec: warn_category

val wkey_decimal_float: warn_category

val wkey_acsl_extension: warn_category

val wkey_cmdline: warn_category
(** Command-line related warning, e.g. for invalid options given by the user *)

(* ************************************************************************* *)
(** {2 Functors for late option registration}                                *)
(** Kernel_function-related options cannot be registered in this module:
    They depend on [Globals], which is linked later. We provide here functors
    to declare them after [Globals] *)
(* ************************************************************************* *)

module type Input_with_arg = sig
  include Parameter_sig.Input_with_arg
  val module_name: string
end

module Kernel_function_set(X:Input_with_arg): Parameter_sig.Kernel_function_set

(* ************************************************************************* *)
(** {2 Option groups} *)
(* ************************************************************************* *)

val inout_source: Cmdline.Group.t

val saveload: Cmdline.Group.t

val parsing: Cmdline.Group.t

val normalisation: Cmdline.Group.t

val analysis_options: Cmdline.Group.t

val seq: Cmdline.Group.t

val project: Cmdline.Group.t

val checks: Cmdline.Group.t

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

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

module PrintCode : Parameter_sig.Bool
  (** Behavior of option "-print" *)

module PrintMachdep : Parameter_sig.Bool
  (** Behavior of option "-print-machdep"
      @since Phosphorus-20170501-beta1 *)

module PrintLibc: Parameter_sig.Bool
  (** Behavior of option "-print-libc"
      @since Phosphorus-20170501-beta1 *)

module PrintComments: Parameter_sig.Bool
  (** Behavior of option "-keep-comments" *)

module PrintReturn : Parameter_sig.Bool
  (** Behavior of option "-print-return"
      @since Sulfur-20171101 *)

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

module AutoLoadPlugins: Parameter_sig.Bool
  (** Behavior of option "-autoload-plugins" *)

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

(* this stop special comment does not work as expected (and as explained in the
   OCamldoc manual, Section 15.2.2. It just skips all the rest of the file
   instead of skipping until the next stop comment...
(**/**)
 *)

module Set_project_as_default: Parameter_sig.Bool
(** Undocumented. *)

(* See (meta-)comment on the previous stop comment
(**/**)
 *)

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

(** Behavior of invisible option -keep-logical operator:
    Tries to avoid converting && and || into conditional statements.
    Note that this option is incompatible with many (most) plug-ins of the
    platform and thus should only be enabled with great care and for very
    specific analyses need.
*)
module LogicalOperators: Parameter_sig.Bool

(** Behavior of option "-enums" *)
module Enums: Parameter_sig.String

module CppCommand: Parameter_sig.String
  (** Behavior of option "-cpp-command" *)

module CppExtraArgs: Parameter_sig.String_list
  (** Behavior of option "-cpp-extra-args" *)

module CppGnuLike: Parameter_sig.Bool
  (** Behavior of option "-cpp-frama-c-compliant" *)

module FramaCStdLib: Parameter_sig.Bool
  (** Behavior of option "-frama-c-stdlib" *)

module ReadAnnot: Parameter_sig.Bool
  (** Behavior of option "-read-annot" *)

module PreprocessAnnot: Parameter_sig.Bool
  (** Behavior of option "-pp-annot" *)

module ContinueOnAnnotError: Parameter_sig.Bool
  (** Behavior of option "-continue-annot-error" *)
[@@ deprecated "Use Kernel.wkey_annot_error instead"]

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

module AsmContractsGenerate: Parameter_sig.Bool
  (** Behavior of option "-asm-contracts" *)

module AsmContractsAutoValidate: Parameter_sig.Bool
  (** Behavior of option "-asm-contracts-auto-validate." *)

module RemoveExn: Parameter_sig.Bool
  (** Behavior of option "-remove-exn" *)

(** Analyzed files *)
module Files: Parameter_sig.String_list
(** List of files to analyse *)

module Orig_name: Parameter_sig.Bool
(** Behavior of option "-orig-name" *)

val normalization_parameters: unit -> Typed_parameter.t list
(** All the normalization options that influence the AST (in particular,
    changing one will reset the AST entirely.contents

    @modify Chlorine-20180501 make it non-constant
 *)

module WarnDecimalFloat: Parameter_sig.String
  (** Behavior of option "-warn-decimal-float" *)
[@@ deprecated "Uses kernel.wkey_decimal_float instead."]

module ImplicitFunctionDeclaration: Parameter_sig.String
  (** Behavior of option "-implicit-function-declaration" *)
[@@ deprecated "Uses kernel.wkey_implicit_function_declaration instead."]

module C11: Parameter_sig.Bool
  (** Behavior of option "-c11" *)

module JsonCompilationDatabase: State_builder.Ref with type data = string
  (** Behavior of option "-json-compilation-database" *)

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

module UnspecifiedAccess: Parameter_sig.Bool
  (** Behavior of option "-unspecified-access" *)

module SafeArrays: Parameter_sig.Bool
  (** Behavior of option "-safe-arrays".
      @plugin development guide *)

module SignedOverflow: Parameter_sig.Bool
  (** Behavior of option "-warn-signed-overflow" *)

module UnsignedOverflow: Parameter_sig.Bool
  (** Behavior of option "-warn-unsigned-overflow" *)

module LeftShiftNegative: Parameter_sig.Bool
  (** Behavior of option "-warn-left-shift-negative" *)

module RightShiftNegative: Parameter_sig.Bool
  (** Behavior of option "-warn-right-shift-negative" *)

module SignedDowncast: Parameter_sig.Bool
  (** Behavior of option "-warn-signed-downcast" *)

module UnsignedDowncast: Parameter_sig.Bool
  (** Behavior of option "-warn-unsigned-downcast" *)

module SpecialFloat: Parameter_sig.String
  (** Behavior of option "-warn-special-float" *)

module InvalidBool: Parameter_sig.Bool
  (** Behavior of option "-warn-invalid-bool" *)

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
