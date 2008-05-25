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

(* $Id: cmdline.mli,v 1.135 2008/05/22 11:06:58 uid526 Exp $ *)

(** Bunch of values which may be initialize through command line. *)

val get_selection: unit -> Project.Selection.t
  (** Selection of all the options. *)

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


type t = (* private *) kind * string

val iter_on_options: (t -> unit) -> unit

(** {2 Signatures} *)

(** Generic outputs signatures of options. *)
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

end

(** Signature for a boolean option. *)
module type BOOL = sig

  include S with type t = bool

  val on: unit -> unit
    (** Set the boolean to [true]. *)

  val off: unit -> unit
    (** Set the boolean to [false]. *)

end

(** Signature for an integer option. *)
module type INT = sig

  include S with type t = int

  val incr: unit -> unit
    (** Increment the integer. *)
end

(** Signature for a string option. *)
module type STRING = S with type t = string

(** Signature for a string set option. *)
module type STRING_SET = sig

  include S with type t = Cilutil.StringSet.t

  val set_set: string -> unit
    (** Set each sub-string (separated by "[ \t]*,[ \t]*" regexp)
        to the set option. *)

  val add: string -> unit
    (** Add a string to the string set option. *)
  val remove: string -> unit
    (** Remove a string from the option. *)
  val add_set: string -> unit
    (** Add each sub-string (separated by "[ \t]*,[ \t]*" regexp)
        to the set option. *)
  val remove_set: string -> unit
    (** Remove each sub-string (separated by "[ \t]*,[ \t]*" regexp)
        from the option. *)

  val is_empty: unit -> bool
  val iter: (string -> unit) -> unit

end

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
end

(** {2 Options} *)

(** {3 GENERAL OPTIONS} *)
module PrintVersion: BOOL
module CodeOutput : STRING
module UseUnicode: BOOL
module SaveState: STRING
module LoadState: STRING
module Time: STRING
module Quiet: BOOL
  
module MainFunction: sig
  include STRING
  val unsafe_set: string -> unit (** Not for casual users. *)
end
module LibEntry: sig
  include STRING
  val unsafe_set: string -> unit (** Not for casual users. *)
end

module Debug: INT

(** {3 SYNTACTICAL TOOLS} *)
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
module Metrics: BOOL
module Machdep: STRING
  
(** {3 CALLGRAPH} *)
module CallgraphFilename: STRING
module CallgraphInitFunc: STRING_SET
  
(** {3 FILES} *)
module CppCommand: STRING
module CppExtraArgs: STRING
module ReadAnnot: BOOL
module PreprocessAnnot: BOOL
module Files: sig
  include S with type t = string list
  module Check: BOOL
  module Copy: BOOL
end

(** {3 MEMZONES} *)
module ForceMemzones: BOOL
  
(** {3 OCCURRENCE} *)
module Occurrence : sig
  module Debug: INT
  module Print: BOOL
end

(** {3 VALUE ANALYSIS} *)
module ForceValues: BOOL
module MemFunctions: STRING_SET
module MemExecAll: BOOL
module FloatDigits: INT
module PropagateTop: BOOL
module ArrayPrecisionLevel: INT
module SemanticUnrollingLevel: INT
module WideningLevel: INT
  
module MinValidAbsoluteAddress: S with type t = Abstract_interp.Int.t
module MaxValidAbsoluteAddress: S with type t = Abstract_interp.Int.t
  (** Absolute address out of this range are considered as
      invalid: this is a threat to write or read at these addresses.
      Default values imply that all values are valid. *)

module AutomaticContextMaxDepth: INT
module AllocatedContextValid: BOOL
module IgnoreOverflow: BOOL  
module UnsafeArrays: BOOL

module KeepOnlyLastRun: BOOL
  (** Keep only last run of value analysis. This is a debugging option. *)
  
module UseRelations: BOOL
module MemoryFootprint: INT
module WidenVariables: STRING_SET

(** {3 FUNCTIONAL DEPENDENCIES} *)
module ForceDeps: BOOL
module ForceCallDeps: BOOL
  
(** {3 USERS} *)
module ForceUsers: BOOL


(** {3 SEMANTIC CONSTANT FOLDING} *)
module Constant_Propagation: sig
  module SemanticConstFolding: BOOL
  module SemanticConstFold: STRING_SET
end

(** {3 INOUT} *)
module ForceOut: BOOL
module ForceInput: BOOL
module ForceInout: BOOL
module ForceDeref: BOOL
module ForceAccessPath: BOOL

(** {3 WP} *)
module WpCfg: BOOL

(** {3 SECURITY} *)
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

(** {3 IMPACT} *)
module Impact : sig

  module Pragma: STRING_SET
    (** Use pragmas of given function. *)

  module Print: BOOL
    (** Print the impacted stmt on stdout. *)

  module Slicing: BOOL
    (** Slicing from the impacted stmt. *)

  val is_on: unit -> bool

end

(** {3 C TO JESSIE} *)
module Jessie : sig
  module ProjectName: STRING
  module Analysis: BOOL
  module Gui: BOOL
  module WhyOpt: STRING_SET
  type int_model = IMexact | IMbounded | IMmodulo
  module IntModel: INDEXED_VAL with type value = int_model
  module GenOnly: BOOL
end

(** {3 PROGRAM DEPENDENCE GRAPH } *)
module Pdg : sig
  module BuildAll: BOOL
  module BuildFct: STRING_SET
  module PrintBw: BOOL
  module DotBasename: STRING
  module DotPostdomBasename: STRING
  module Verbosity: INT
end

(** {3 SPARE CODE} *)
module Sparecode : sig
  module Analysis: BOOL
    (** Whether to perform spare code detection or not. *)
end

(** {3 SLICING} *)
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

(** {3 VALVIEWER} *)
module MonospaceFontName: STRING
module GeneralFontName: STRING

(** {3 MIEL} *)
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




(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
