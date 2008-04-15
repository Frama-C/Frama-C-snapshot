(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: cmdline.mli,v 1.181 2008/12/09 15:24:13 uid562 Exp $ *)

(** Bunch of values which may be initialize through command line.
    @plugin development guide *)

val get_selection: unit -> Project.Selection.t
  (** Selection of all the options.
      @plugin development guide *)

val get_selection_context: unit -> Project.Selection.t
  (** Selection of all the options which define the context of analyses. *)

val nb_selected_options: unit -> int
  (** Numbers of selected options. *)

val clear_selected_options: unit -> unit
val set_selected_options: unit -> unit
  (** Set all the selected options to their respective values *)

type 'a option_accessor = private {get : unit -> 'a ; set : 'a -> unit }

type kind = private
  | Bool of bool option_accessor
  | Int of int option_accessor
  | String of string option_accessor
  | StringSet of string option_accessor (* Comma separated string list *)
  | StringList of string option_accessor (* Comma separated string list *)

type t = (* private *) kind * string

val iter_on_options: (t -> unit) -> unit

(** {2 Signatures} *)

(** Generic outputs signatures of options.
    @plugin development guide *)
module type S = sig

  type t

  val set: t -> unit
    (** Set the option. *)

  val get: unit -> t
    (** Option value (not necessarly set on the current command line). *)

  val clear: unit -> unit
    (** Set the option to its default value, that is the value if [set] was
	never called. *)

  val is_set: unit -> bool
    (** Is the option different of its default value? *)

  include Project.Computation.OUTPUT

  val equal: t -> t -> bool

    (**/**)
  val unsafe_set: t -> unit
    (** Set but without clearing the dependencies.*)
    (**/**)
end

(** Signature for a boolean option.
    @plugin development guide *)
module type BOOL = sig

  include S with type t = bool

  val on: unit -> unit
    (** Set the boolean to [true]. *)

  val off: unit -> unit
    (** Set the boolean to [false]. *)

end

(** Signature for an integer option.
    @plugin development guide *)
module type INT = sig

  include S with type t = int

  val incr: unit -> unit
    (** Increment the integer. *)
end

(** Signature for a string option.
    @plugin development guide *)
module type STRING = S with type t = string

(** Signature for a generic set of strings option. *)
module type GEN_STRING_SET = sig

  include S

  val set_set: string -> unit
    (** Set each sub-string (separated by "[ \t]*,[ \t]*" regexp)
        to the set option. *)
  val get_set: ?sep:string -> unit -> string
    (** Get a string which concatenates each string in the set with a
	white space separation. *)
  val add: string -> unit
    (** Add a string to the string set option. *)
  val add_set: string -> unit
    (** Add each sub-string (separated by "[ \t]*,[ \t]*" regexp)
        to the set option. *)
  val iter: (string -> unit) -> unit
  val fold: (string -> 'a -> 'a) -> 'a -> 'a
end

module type STRING_SET = sig
  include GEN_STRING_SET with type t = Cilutil.StringSet.t
  val is_empty: unit -> bool
  val remove: string -> unit
    (** Remove a string from the option. *)
  val remove_set: string -> unit
    (** Remove each sub-string (separated by "[ \t]*,[ \t]*" regexp)
        from the option. *)
end

module type STRING_LIST = GEN_STRING_SET with type t = string list

(** {3 Complex values indexed by strings} *)

(** option interface *)
module type INDEXED_VAL = sig
  include STRING
  type value (** the real type for the option*)
  val add_choice: string -> value -> unit
    (** adds a new choice for the option. *)
  val get_val: unit -> value
    (** the currently selected value. *)
end

(** input signature for [IndexedVal] *)
module type COMPLEX_VALUE = sig
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val name: string (** name of the option *)
  val fun_ty: t Type.t
end

(** {3 Interface for dynamic plugins} *)

(** Use this module for options of dynamic plugins. *)
module Dynamic : sig

  (** Functors for registering options of dynamic plugins. *)
  module Register : sig
    module False(X : sig val name : string end) : BOOL
      (** @plugin development guide *)
    module True(X : sig val name : string end) : BOOL
    module Zero (X : sig val name : string end) : INT
      (** @plugin development guide *)
    module EmptyString(X : sig val name : string end) : STRING
    module StringSet(X: sig val name: string end) : STRING_SET
  end

  (** Module to use for applying functions register by one the
      functors of [Register]. *)
  module Apply : sig

    (** Common options *)
    module type Common = sig
      type t
      val get: string -> t
      val set: string -> t -> unit
      val clear: string -> unit -> unit
      val is_set: string  -> bool
    end

    (** Boolean options. *)
    module Bool: sig
      include Common with type t = bool
      val on: string -> unit -> unit
      val off : string -> unit -> unit
    end

    (** Integer options. *)
    module Int : sig
      include Common with type t = int
      val incr : string -> unit -> unit
    end

    (** String options *)
    module String : Common with type t = string

    (** StringSet options. *)
    module StringSet : sig
      include Common with type t = Cilutil.StringSet.t
      val add : string -> string  -> unit
      val add_set : string -> string -> unit
      val is_empty : string -> bool
      val iter :string -> (string -> unit) -> unit
      val fold: string -> (string -> 'a -> 'a) -> 'a -> 'a
    end

  end

  (** Options for configurating dynamic loading *)

  module Debug : INT
  module AddPath : STRING_SET
  module LoadModule : STRING_SET

end

(** {2 Options} *)

(** {3 General Options} *)

module PrintVersion: BOOL
module PrintShare: BOOL
module CodeOutput : sig
  include STRING
  val get_fmt: unit -> Format.formatter
end
module UseUnicode: BOOL
module SaveState: STRING
module LoadState: STRING
module Time: STRING
module Quiet: BOOL

module MainFunction: sig
  include STRING
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module LibEntry: sig
  include BOOL
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module Machdep: sig
  include STRING
    (** If [set] is called, then {!File.prepare_from_c_files} must be
	called for well preparing the AST. *)
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module Debug: INT

(** {3 Journalization} *)
module Journal: sig
  module Disable: BOOL
  module Name: STRING
end

(** {3 Syntactic Tools} *)

module PrintCode : BOOL

module SimplifyCfg: BOOL
  (** Call Cil.prepareCFG on all functions. Removes
      break, continue and switch statemement *)

module KeepSwitch: BOOL
  (** Allows to keep switch statements, even if -simplify-cfg is used. *)

module PrintComments: BOOL
module UnrollingLevel: INT
module Constfold: BOOL
module Obfuscate: BOOL

module Metrics: sig
  module Print: BOOL (** Pretty print metrics on stdout *)
  module Dump: STRING (** Pretty print metrics on the given file *)
  val is_on: unit -> bool (** Have metrics to be computed? *)
end

module WarnUnspecifiedOrder: BOOL
  (** Warns for unspecified sequences containing at least one writes *)

(** {3 Callgraph} *)

module CallgraphFilename: STRING
module CallgraphInitFunc: STRING_SET
module Semantic_Callgraph: sig module Dump : BOOL end

(** {3 Files} *)

module CppCommand: STRING
module CppExtraArgs: STRING_SET
module ReadAnnot: BOOL
module PreprocessAnnot: BOOL

module Files: sig
  include STRING_LIST
  module Check: BOOL
  module Copy: BOOL
  module Orig_name: BOOL
end

(** {3 Memzones} *)

module ForceMemzones: BOOL

(** Occurrence *)

module Occurrence : sig
  module Debug: INT
  module Print: BOOL
end


(** {3 Value Analysis} *)

module ForceValues: BOOL
module MemFunctions: STRING_SET
module MemExecAll: BOOL
module FloatDigits: INT
module PropagateTop: BOOL
module ArrayPrecisionLevel: INT
module SemanticUnrollingLevel: INT
  (** @plugin development guide *)

module WideningLevel: INT

module MinValidAbsoluteAddress: S with type t = Abstract_interp.Int.t
module MaxValidAbsoluteAddress: S with type t = Abstract_interp.Int.t
  (** Absolute address out of this range are considered as
      invalid: this is a threat to write or read at these addresses.
      Default values imply that all values are valid. *)

module AutomaticContextMaxDepth: INT
module AutomaticContextMaxWidth: INT
module AllocatedContextValid: BOOL
module IgnoreOverflow: BOOL

module IgnoreUnspecified: BOOL
  (** ignore alarms related to read/write accesses in UnspecifiedSequence.
      default to false. *)

module UnsafeArrays: BOOL

module KeepOnlyLastRun: BOOL
  (** Keep only last run of value analysis. This is a debugging option. *)

module UseRelations: BOOL
module MemoryFootprint: INT
module WidenVariables: STRING_SET

(** {3 Functional Dependencies} *)

module ForceDeps: BOOL
module ForceCallDeps: BOOL

(** Users *)
module ForceUsers: BOOL
  (** @plugin development guide *)

(** Constant Propagation *)
module Constant_Propagation: sig
  module SemanticConstFolding: BOOL
  module SemanticConstFold: STRING_SET
  module CastIntro: BOOL
end

(** {3 Inout} *)

module ForceOut: BOOL
module ForceInput: BOOL
module ForceInputWithFormals: BOOL
module ForceInout: BOOL
module ForceDeref: BOOL
module ForceAccessPath: BOOL

(** {3 WP} *)

module Wp : sig
  module Cfg: BOOL
  module Post: BOOL
  module Debug: INT
end

(** Security *)
module Security: sig

  module LogicAnnotation: STRING
    (** Which kind of security logical annotations are recognized. *)

  module Analysis: BOOL
    (** Whether perform security analysis or not. *)

  module Lattice: STRING
    (** Security lattice to use. *)

  module PropagateAssertions: BOOL
    (** Propagate security assertions when possible. *)

  module Slicing: BOOL
    (** Perform the security slicing pre-analysis. *)

  val is_on: unit -> bool

  module Debug: INT
    (** Level of debug for the security plugin. *)

  val get_selection_after_slicing: unit -> Project.Selection.t

end

(** Impact *)
module Impact : sig

  module Pragma: STRING_SET
    (** Use pragmas of given function. *)

  module Print: BOOL
    (** Print the impacted stmt on stdout. *)

  module Slicing: BOOL
    (** Slicing from the impacted stmt. *)

  val is_on: unit -> bool

end

(** Jessie *)
module Jessie : sig
  module ProjectName: STRING
  module Behavior: STRING
  module Analysis: BOOL
  module Gui: BOOL
  module WhyOpt: STRING_SET
  module JcOpt: STRING_SET
  type int_model = IMexact | IMbounded | IMmodulo
  module IntModel: INDEXED_VAL with type value = int_model
  module GenOnly: BOOL
  module GenGoals: BOOL
  module SepRegions: BOOL
  module StdStubs: BOOL
  module InferAnnot: STRING
  module AbsDomain: STRING
  module Atp: STRING
  module CpuLimit: INT
  module HintLevel: INT
end

(** Program Dependence Graph *)
module Pdg : sig

  module BuildAll: BOOL
    (** @plugin development guide *)

  module BuildFct: STRING_SET
    (** @plugin development guide *)

  module PrintBw: BOOL
  module DotBasename: STRING
    (** @plugin development guide *)

  module DotPostdomBasename: STRING
    (** @plugin development guide *)

  module Verbosity: INT
    (** @plugin development guide *)

end

(** Spare Code *)
module Sparecode : sig
  module Analysis: BOOL
    (** Whether to perform spare code detection or not. *)
  module NoAnnot : BOOL
    (** don't keep more things to keep all reachable annotations. *)
  module GlobDecl : BOOL
    (** remove unused global types and variables *)
end

(** Slicing *)
module Slicing : sig
  module Select : sig
    module Calls: STRING_SET
    module Return: STRING_SET
    module Threat: STRING_SET
    module Assert: STRING_SET
    module Pragma: STRING_SET
    module LoopInv: STRING_SET
    module LoopVar: STRING_SET
    module RdAccess: STRING_SET
    module WrAccess: STRING_SET
    module Value: STRING_SET
  end
  module Mode : sig
    module Verbose: INT
    module Callers: BOOL
    module Calls: INT
    module SliceUndef: BOOL
  end
  module Print: BOOL
  val is_on: unit -> bool
end

(** {3 Graphical User Interface} *)

module MonospaceFontName: STRING
module GeneralFontName: STRING
  (** @plugin development guide *)

(** {3 Miel} *)

module MielSpecFilename: STRING
module AlcoolExtraction: STRING
module AlcoolPrintLocations: BOOL
module PointersExtraction: STRING
module XmlPointersExtraction: STRING
module RawExtraction: STRING
module PromelaExtraction: STRING
module Relevant: BOOL
module Report: STRING
module ConstFuncArrays: BOOL

(** {3 Cxx} *)

module PrintCxx: BOOL
  (** [true] for pretty-printing the C++ files attached to the project. *)

module Unmangling: INDEXED_VAL with type value = string -> string

(** Aorai plugin (ltl_to_acsl) *)
module Ltl_to_acsl : sig
  module Analysis : BOOL
  module Ltl_File : STRING
  module OnlyToLTL : BOOL
  module Promela_File : STRING
  module Output_Spec : BOOL
  module Verbose : BOOL
  module Output_C_File : STRING
  module OnlyFromPromela : BOOL
  module Dot : BOOL
  module AbstractInterpretation : BOOL
  module AdvanceAbstractInterpretation  : BOOL
end


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
