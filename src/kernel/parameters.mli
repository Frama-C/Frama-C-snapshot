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

(** Kernel parameters and generic access to plug-in parameters.
    @since Beryllium-20090601-beta1 *)

open Plugin

(* ************************************************************************* *)
(** {2 Generic access to plug-in parameters} *)
(* ************************************************************************* *)

(** Module to use for accessing parameters of plug-ins.
    Assume that the plug-in is already loaded. *)
module Dynamic : sig

  (** Set of common operations on parameters. *)
  module type Common = sig
    type t
    val get: string -> t
    val set: string -> t -> unit
    val clear: string -> unit -> unit
    val is_set: string -> bool
    val is_default: string -> bool
  end

  (** Boolean parameters. *)
  module Bool: sig
    include Common with type t = bool
    val on: string -> unit -> unit
      (** Set the parameter to [true]. *)
    val off : string -> unit -> unit
      (** Set the parameter to [false]. *)
  end

  (** Integer parameters. *)
  module Int : sig
    include Common with type t = int
    val incr : string -> unit -> unit
  end

  (** String parameters. *)
  module String : Common with type t = string

  (** Set of string parameters. *)
  module StringSet : sig
    include Common with type t = Datatype.String.Set.t
    val add: string -> string  -> unit
    val remove: string -> string -> unit
    val is_empty: string -> bool
    val iter: string -> (string -> unit) -> unit
  end
(*
    module IndexedVal(X: sig val ty_name: string end) : sig
      include Common with type t = string
      type value
      val add_choice: string -> string -> value -> unit
      val get_val: string -> value
    end
*)

end

(* ************************************************************************* *)
(** {2 General purpose options} *)
(* ************************************************************************* *)

val check_range: string -> min:int -> max:int -> int -> unit
  (** @since Beryllium-20090601-beta1
      @deprecated Beryllium-20090901 *)

val get_selection_context: unit -> State_selection.t
  (** Selection of all the parameters which define the context of analyses. *)

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

module PrintVersion: BOOL
  (** Behavior of option "-version" *)

module PrintShare: BOOL
  (** Behavior of option "-print-share-path" *)

module PrintLib: BOOL
  (** Behavior of option "-print-lib-path" *)

module PrintPluginPath: BOOL
  (** Behavior of option "-print-plugin-path" *)

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

module GeneralVerbose: INT
  (** Behavior of option "-verbose" *)

module GeneralDebug: INT
  (** Behavior of option "-debug" *)

module Quiet: BOOL
  (** Behavior of option "-quiet" *)

module UseUnicode: BOOL
  (** Behavior of option "-unicode"
      @plugin development guide *)

module Time: STRING
  (** Behavior of option "-time" *)

module Collect_messages: BOOL
(** Behavior of option "-collect-messages" *)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

module PrintCode : BOOL
  (** Behavior of option "-print" *)

module PrintComments: BOOL
  (** Behavior of option "-keep-comments" *)

(** Behavior of option "-ocode" *)
module CodeOutput : sig
  include STRING
  val output: ('a,Format.formatter,unit) format -> 'a
end

module FloatNormal: BOOL
  (** Behavior of option "-float-normal" *)

module FloatRelative: BOOL
  (** Behavior of option "-float-relative" *)

module FloatHex: BOOL
  (** Behavior of option "-float-hex" *)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

module SaveState: STRING
  (** Behavior of option "-save" *)

module LoadState: STRING
  (** Behavior of option "-load" *)

module AddPath: STRING_SET
  (** Behavior of option "-add-path" *)

module LoadModule: STRING_SET
  (** Behavior of option "-load-module" *)

module LoadScript: STRING_SET
  (** Behavior of option "-load-script" *)

module Dynlink: BOOL
  (** Behavior of option "-dynlink" *)

(** Parameters for journalization. *)
module Journal: sig

  module Enable: BOOL
    (** Behavior of option "-journal-enable" *)

  module Name: STRING
    (** Behavior of option "-journal-name" *)

end

(* ************************************************************************* *)
(** {2 Customizing Normalization} *)
(* ************************************************************************* *)

module UnrollingLevel: INT
  (** Behavior of option "-ulevel" *)

(** Behavior of option "-machdep".
    If function [set] is called, then {!File.prepare_from_c_files} must be
    called for well preparing the AST. *)
module Machdep: STRING

module CppCommand: STRING
  (** Behavior of option "-cpp-command" *)

module CppExtraArgs: STRING_SET
  (** Behavior of option "-cpp-extra-args" *)

module ReadAnnot: BOOL
  (** Behavior of option "-read-annot" *)

module PreprocessAnnot: BOOL
  (** Behavior of option "-pp-annot" *)

module TypeCheck: BOOL
  (** Behavior of option "-type-check" *)

module ContinueOnAnnotError: BOOL
  (** Behavior of option "-continue-annot-error" *)

module SimplifyCfg: BOOL
  (** Behavior of option "-simplify-cfg" *)

module KeepSwitch: BOOL
  (** Behavior of option "-keep-switch" *)

module Constfold: BOOL
  (** Behavior of option "-constfold" *)

(** Analyzed files *)
module Files: sig

  include STRING_LIST
    (** List of files to analyse *)

  module Check: BOOL
    (** Behavior of option "-check" *)

  module Copy: BOOL
    (** Behavior of option "-copy" *)

  module Orig_name: BOOL
    (** Behavior of option "-orig-name" *)

end

(* ************************************************************************* *)
(** {3 Customizing cabs2cil options} *)
(* ************************************************************************* *)

module AllowDuplication: BOOL
  (** Behavior of option "-allow-duplication". *)

module DoCollapseCallCast: BOOL
  (** Behavior of option "-collapse-call-cast". *)

module ForceRLArgEval: BOOL
  (** Behavior of option "-force-rl-arg-eval". *)

(* ************************************************************************* *)
(** {2 Analysis Behavior of options} *)
(* ************************************************************************* *)

(** Behavior of option "-main".

    You should usually use {!Globals.entry_point} instead of
    {!MainFunction.get} since the first one handles the case where the entry
    point is invalid in the right way. *)
module MainFunction: sig

  include STRING

  (** {2 Internal functions}

      Not for casual users. *)

  val unsafe_set: t -> unit

end

(** Behavior of option "-lib-entry".

    You should usually use {!Globals.entry_point} instead of
    {!LibEntry.get} since the first one handles the case where the entry point
    is invalid in the right way. *)
module LibEntry: sig
  include BOOL
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module UnspecifiedAccess: BOOL
  (** Behavior of option "-unspecified-access" *)

module Overflow: BOOL
  (** Behavior of option "-overflow" *)

module SafeArrays: BOOL
  (** Behavior of option "-safe-arrays" *)

module AbsoluteValidRange: Plugin.STRING
  (** Behavior of option "-absolute-valid-range" *)

module FloatFlushToZero: BOOL
  (** Behavior of option "-float-flush-to-zero" *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
