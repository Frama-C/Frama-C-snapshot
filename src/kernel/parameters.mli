(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(** Kernel parameters and access to the plug-in parameters.
    @since Beryllium-20090601-beta1 *)

open Plugin

val check_range: string -> min:int -> max:int -> int -> unit
  (** @since Beryllium-20090601-beta1 
      @deprecated Beryllium-20090601-beta1+dev *)

val get_selection_context: unit -> Project.Selection.t
  (** Selection of all the options which define the context of analyses. *)

(** {2 Access to plug-in parameters} *)

(** Module to use for accessing parameters of plug-ins. *)
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
    include Common with type t = Cilutil.StringSet.t
    val add : string -> string  -> unit
    val add_set : string -> string -> unit
    val is_empty : string -> bool
    val iter :string -> (string -> unit) -> unit
      (*      val fold: string -> (string -> 'a -> 'a) -> 'a -> 'a*)
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

(** {2 Kernel parameters} *)

module AddPath : STRING_SET
module LoadModule : STRING_SET
module LoadScript : STRING_SET
module PrintVersion: BOOL
module PrintShare: BOOL
module PrintLib: BOOL
module PrintPluginPath: BOOL
module CodeOutput : sig
  include STRING
  val output: ('a,Format.formatter,unit) format -> 'a
end
module UseUnicode: BOOL
  (** @plugin development guide *)

module SaveState: STRING
module LoadState: STRING
module Time: STRING

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

include Plugin.S

(** Parameters for journalization. *)
module Journal: sig
  module Enable: BOOL
  module Name: STRING
end

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

(** {3 Parameters for files} *)

module CppCommand: STRING
module CppExtraArgs: STRING_SET
module ReadAnnot: BOOL
module PreprocessAnnot: BOOL

module TypeCheck: BOOL (** Performs type-checking. *)
module ContinueOnAnnotError: BOOL
  (** Do not stop for type-checking errors in annotations. *)

module Files: sig
  include STRING_LIST
  module Check: BOOL
  module Copy: BOOL
  module Orig_name: BOOL
end


(** Parameters for analysis (should be used by all relevant plug-ins 
    for consistency *)

module AbsoluteValidRange: Plugin.STRING
  (** Absolute address out of this range are considered as
      invalid: this is a threat to write or read at these addresses.
      Default values imply that all values are valid. *)
module FloatDigits: INT

(** {3 Value Analysis} *)

module Overflow: BOOL
module UnspecifiedAccess: BOOL
  (** Whether emit alarms related to read/write accesses in
      UnspecifiedSequence.
      default to true. *)

module SafeArrays: BOOL


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
