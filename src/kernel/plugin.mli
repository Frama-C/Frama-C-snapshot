(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Provided plug-general services for plug-ins.
    @since Beryllium-20090601-beta1 *)

val at_normal_exit: (unit -> unit) -> unit
  (** Now replaced by {!Cmdline.at_normal_exit}.
      @since Beryllium-20090901
      @deprecated since Boron-20100401 *)

val run_normal_exit_hook: unit -> unit
  (** Now replaced by {!Cmdline.run_normal_exit_hook}.
      @since Beryllium-20090901
      @deprecated since Boron-20100401 *)

(** {2 Signatures} *)

type group
  (** Group of parameters.
      @since Beryllium-20090901 *)

(** Generic outputs signatures of parameters.
    @plugin development guide *)
module type Parameter = sig

  type t

  val set: t -> unit
    (** Set the option. *)

  val add_set_hook: (t -> t -> unit) -> unit
    (** Add a hook to be called whenafter the function {!set} is called.
	The first parameter of the hook is the old value of the parameter while
	the second one is the new value. *)

  val get: unit -> t
    (** Option value (not necessarly set on the current command line). *)

  val clear: unit -> unit
    (** Set the option to its default value, that is the value if [set] was
	never called. *)

  val is_default: unit -> bool
    (** Is the option equal to its default value? *)

  val is_set: unit -> bool
    (** Is the function {!set} has already been called since the last call to
	function {!clear}? *)

  include Project.Computation.OUTPUT

  val equal: t -> t -> bool

    (**/**)
  val unsafe_set: t -> unit
    (** Set but without clearing the dependencies.*)
    (**/**)

  val add_alias: string list -> unit
    (** Add some aliases for this option. That is other option names which have
	exactly the same semantics that the initial option. *)

end

(** Signature for a boolean parameter.
    @plugin development guide *)
module type BOOL = sig

  include Parameter with type t = bool

  val on: unit -> unit
    (** Set the boolean to [true]. *)

  val off: unit -> unit
    (** Set the boolean to [false]. *)

end

(** Signature for an integer parameter.
    @plugin development guide *)
module type INT = sig

  include Parameter with type t = int

  val incr: unit -> unit
    (** Increment the integer. *)

  val set_range: min:int -> max:int -> unit
    (** Set what is the possible range of values for this parameter.
	@since Beryllium-20090901 *)

  val get_range: unit -> int * int
    (** What is the possible range of values for this parameter.
	@since Beryllium-20090901 *)

end

(** Signature for a string parameter.
    @plugin development guide *)
module type STRING = sig

  include Parameter with type t = string

  val set_possible_values: string list -> unit
    (** Set what are the acceptable values for this parameter.
	If the given list is empty, then all values are acceptable.
	@since Beryllium-20090901 *)

  val get_possible_values: unit -> string list
    (** What are the acceptable values for this parameter.
	If the returned list is empty, then all values are acceptable.
	@since Beryllium-20090901 *)
end

(** Signature for a generic set of strings option. *)
module type GEN_STRING_SET = sig

  include Parameter

  val add: string -> unit
    (** Add a string to the string set option. *)

  val remove: string -> unit
    (** Remove a string from the option. *)

  val is_empty: unit -> bool
    (** Check if the set is empty. *)

  val get_set: ?sep:string -> unit -> string
    (** Get a string which concatenates each string in the set with a
	separator. The default separator is ", ". *)

  val iter: (string -> unit) -> unit
    (** Iter on each string in the set. *)

end

module type STRING_SET = GEN_STRING_SET with type t = Cilutil.StringSet.t
module type STRING_LIST = GEN_STRING_SET with type t = string list

(** @since Boron-20100401 *)
module type STRING_HASHTBL = sig

  include GEN_STRING_SET with type t = Cilutil.StringSet.t

  (** @since Boron-20100401 *)
  type value

  val find: string -> value
    (** @since Boron-20100401 *)

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

(** Minimal signature to implement for each parameter corresponding to an
    option on the command line argument. *)
module type Parameter_input = sig
  val option_name: string
    (** The name of the option *)
  val descr: string
    (** A description for this option (e.g. used by -help) *)
end

(** Minimal signature to implement for each parameter corresponding to an
    option on the command line argument which requires an argument. *)
module type Parameter_input_with_arg = sig
  include Parameter_input
  val arg_name: string
    (** A standard name for the argument which may be used in the description.
	If empty, a generic arg_name is generated. *)
end

(** input signature for [IndexedVal] *)
module type COMPLEX_VALUE = sig
  include Parameter_input_with_arg
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val ty: t Type.t
end

module type S = sig
  include Log.Messages

  val add_group: string -> group
    (** Create a new group inside the plug-in.
	The given string must be different of all the other group names of this
	plug-in.
	@since Beryllium-20090901 *)

  module Help: BOOL
  module Verbose: INT
  module Debug: INT

  val help: group
    (** The group containing option -*-help.
	@since Boron-20100401 *)

  val messages: group
    (** The group containing options -*-debug and -*-verbose.
	@since Boron-20100401 *)

end

module type General_services = sig

  include S

  (** {2 Functors for generating a new parameter} *)

  module Bool
    (X:sig
       include Parameter_input
       val default: bool
	 (** The default value of the parameter. So giving the option
	     [option_name] to Frama-C, change the value of the parameter to
	     [not default]. *)
     end) : BOOL

  (** Build a boolean option initialized fo [false], that is not saved. *)
  module Action(X: Parameter_input) : BOOL

  (** Build a boolean option initialized to [false].
      @plugin development guide *)
  module False(X: Parameter_input) : BOOL

  (** Build a boolean option initialized to [true].
      @plugin development guide *)
  module True(X: Parameter_input) : BOOL

  (** Build an integer option.
      @plugin development guide *)
  module Int
    (X: sig val default: int include Parameter_input_with_arg end) : INT

  (** Build an integer option initialized to [0].
      @plugin development guide *)
  module Zero(X:Parameter_input_with_arg) : INT

  (** Build a string option.
      @plugin development guide *)
  module String
    (X: sig include Parameter_input_with_arg val default: string end) : STRING

  (** Build a string option initialized to [""].
      @plugin development guide *)
  module EmptyString(X: Parameter_input_with_arg) : STRING

  (** Build an option as a set of strings, initialized to the empty set. *)
  module StringSet(X: Parameter_input_with_arg) : STRING_SET

  (** Should not be used by casual users *)
  module StringList(X: Parameter_input_with_arg) : STRING_LIST

  (** @plugin development guide *)
  module IndexedVal (V:COMPLEX_VALUE) : INDEXED_VAL with type value = V.t

  (** @since Boron-20100401 *)
  module StringHashtbl
    (X: Parameter_input_with_arg)
    (V: sig
       include Project.Datatype.S
       val parse: string -> string * t
       val no_binding: string -> t
     end) :
    STRING_HASHTBL with type value = V.t

end

(** {2 Configuration of functor applications generating parameters}

    You can apply the below functions juste before applying one of the functors
    provided by the functor [Register] and generating a new parameter. *)

val set_cmdline_stage: Cmdline.stage -> unit
  (** Set the stage where the option corresponding to the parameter is
      recognized. Default is [Cmdline.Configuring].
      @since Beryllium-20090601-beta1 *)

val do_not_journalize: unit -> unit
  (** Call this function in order to not journalize the parameter.
      @since Beryllium-20090601-beta1 *)

val do_not_projectify: unit -> unit
  (** Do not projectify the parameter.
      @since Beryllium-20090601-beta1 *)

val register_kernel: unit -> unit
  (** To be called just before {!Register} in order to activate a
      special mode corresponding to registering some parts of the Frama-C
      kernel and not a standard plug-in.
      @since Beryllium-20090601-beta1 *)

val set_negative_option_name: string -> unit
  (** For boolean parameters, set the name of the negative
      option generating automatically from the positive one (the given option
      name). The default used value prefixes the given option name by "-no".
      Assume that the given string is a valid option name or empty.
      If it is empty, no negative option is created.
      @since Beryllium-20090601-beta1 *)

val set_negative_option_descr: string -> unit
  (** For boolean parameters, set the description of the negative
      option generating automatically.
      Assume that the given string is non empty.
      @since Beryllium-20090601-beta1 *)

val set_optional_descr: (unit, Format.formatter, unit) format -> unit
  (** Concatenate an additional description just after the default one.
      @since Beryllium-20090601-beta1 *)

val set_group: group -> unit
  (** Change the group of the parameter.
      @since Beryllium-20090901 *)

val set_module_name: string -> unit
  (** This function must be called if and only if the next functor application
      generates a new **kernel** parameter. So this function should not be used
      by plug-in developer. The given argument must be the module name
      corresponding to the parameter. *)

val is_visible: unit -> unit
  (** This function must be called in order to allow the parameter created
      by the next functor application to be accessible through function
      {!iter_on_plugins}. By default, only the parameter corresponding to an
      option registered at the {!Cmdline.Configuring} stage are visible.
      @since Boron-20100401 *)

(** Functors for generating plug-ins parameters. *)
module Register
  (P: sig
     val name: string (** Name of the module. Arbitrary non-empty string. *)
     val shortname: string (** Prefix for plugin options. No space allowed. *)
     val descr: string (** description of the module. Free-form text. *)
   end) :
  General_services

(** {2 Handling groups of parameters} *)

type 'a option_accessor = private
    { get: unit -> 'a; set: 'a -> unit; is_set: unit -> bool }

type kind = private
  | Bool of
      bool option_accessor * string option (** the negative option, if any *)
  | Int of int option_accessor * (unit -> int * int) (** getting range *)
  | String of
      string option_accessor * (unit -> string list) (** possible values *)
  | StringSet of string option_accessor (** Comma separated string list *)

type parameter = private { o_name: string; o_descr: string; o_kind: kind }
    (** @since Beryllium-20090901 *)

type plugin = private
    { p_name: string;
      p_descr: string;
      p_parameters: (string, parameter list) Hashtbl.t }
    (** Only visible parameters (see {!is_visible}) are registered in the field
	[p_parameters].
	@since Beryllium-20090901 *)

val iter_on_plugins: (plugin -> unit) -> unit
  (** Iterate on each registered plug-ins.
      @since Beryllium-20090901 *)

val get_selection: unit -> Project.Selection.t
  (** Selection of all the settable parameters.
      @plugin development guide *)

val positive_debug_ref: int ref
  (** Not for casual users.
      @since Boron-20100401 *)

val dynamic_plugin_name: string -> string
  (** Not for casual users.
      @since Boron-20100401 *)

val dynamic_function_name: string -> string -> string
  (** Not for casual users.
      @since Boron-20100401 *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
