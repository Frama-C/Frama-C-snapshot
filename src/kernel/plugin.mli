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

(** Provided plug-general services for plug-ins.
    @since Beryllium-20090601-beta1 *)

(* ************************************************************************* *)
(** {2 Signatures} *)
(* ************************************************************************* *)

type group = Cmdline.Group.t
  (** Group of parameters.
      @since Beryllium-20090901 *)

(** Generic signature of a parameter.
    @plugin development guide *)
module type Parameter = sig

  type t
  (** Type of the parameter (an int, a string, etc). It is concrete for each
      module implementing this signature. *)

  val parameter: Parameter.t
  (** @since Nitrogen-20111001 *)

  val set: t -> unit
    (** Set the option. *)

  val add_set_hook: (t -> t -> unit) -> unit
    (** Add a hook to be called whenafter the function {!set} is called.
        The first parameter of the hook is the old value of the parameter while
        the second one is the new value. *)

  val add_update_hook: (t -> t -> unit) -> unit
  (** Add a hook to be called when the value of the parameter changes (by
      calling {!set} or indirectly by the project library. The first parameter
      of the hook is the old value of the parameter while the second one is the
      new value. Note that it is **not** specified if the hook is applied just
      before or just after the effective change.
      @since Nitrogen-20111001 *)

  val get: unit -> t
    (** Option value (not necessarly set on the current command line). *)

  val clear: unit -> unit
    (** Set the option to its default value, that is the value if [set] was
        never called. *)

  val is_default: unit -> bool
    (** Is the option equal to its default value? *)

  val option_name: string
    (** Name of the option on the command-line
        @since Carbon-20110201  *)

  include State_builder.S

  val equal: t -> t -> bool

  val add_aliases: string list -> unit
  (** Add some aliases for this option. That is other option names which have
      exactly the same semantics that the initial option.
      @raise Invalid_argument if one of the strings is empty *)

  val add_alias: string list -> unit
(** Equivalent to [add_aliases].
    @deprecated since Carbon-20110201 *)

  (**/**)
  val is_set: unit -> bool
  (** Is the function {!set} has already been called since the last call to
      function {!clear}? This function is for special uses and should mostly
      never be used. *)

  val unsafe_set: t -> unit
(** Set but without clearing the dependencies.*)
(**/**)

end

(** Signature for a boolean parameter.
    @plugin development guide *)
module type Bool = sig

  include Parameter with type t = bool

  val on: unit -> unit
    (** Set the boolean to [true]. *)

  val off: unit -> unit
    (** Set the boolean to [false]. *)

end

(** Signature for a boolean parameter that causes something to be output.
    @plugin development guide *)
module type WithOutput = sig
  include Bool

  val set_output_dependencies: State.t list -> unit
  (** Set the dependecies for the output of the option. Two successive
      calls to [output] below will cause only one output, unless some
      of the supplied dependencies have changed between the two calls. *)

  val output: (unit -> unit) -> unit
  (** To be used by the plugin to output the results of the option
      in a controlled way. See [set_output_dependencies] details. *)
end


(** Signature for an integer parameter.
    @plugin development guide *)
module type Int = sig

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
module type String = sig

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
module type String_collection = sig

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

  val exists: (string -> bool) -> bool
    (** Checks if at least one element of the set satisfies the predicate.
    @since Carbon-20101201 *)

end

module type String_set = String_collection with type t = Datatype.String.Set.t
module type String_list = String_collection with type t = string list

(** @since Boron-20100401 *)
module type String_hashtbl = sig
  include String_collection with type t = Datatype.String.Set.t
  type value
    (** @since Boron-20100401 *)
  val find: string -> value
    (** @since Boron-20100401 *)
end

(** {3 Complex values indexed by strings} *)

(** option interface *)
module type Indexed_val = sig
  include String
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
  val help: string
(** A description for this option (e.g. used by -help).
    If [help = ""], then it has the special meaning "undocumented" *)
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
module type Indexed_val_input = sig
  include Parameter_input_with_arg
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val ty: t Type.t
end

module type S = sig

  include Log.Messages

  val add_group: ?memo:bool -> string -> group
    (** Create a new group inside the plug-in.
        The given string must be different of all the other group names of this
        plug-in if [memo] is [false].
        If [memo] is [true] the function will either create a fresh group or
        return an existing group of the same name in the same plugin.
        [memo] defaults to [false]
        @since Beryllium-20090901 *)

  module Help: Bool
  module Verbose: Int
  module Debug: Int

  val help: group
    (** The group containing option -*-help.
        @since Boron-20100401 *)

  val messages: group
    (** The group containing options -*-debug and -*-verbose.
        @since Boron-20100401 *)

  val parameters: unit -> Parameter.t list
(** List of parameters created by this plug-in.
    @since Nitrogen-20111001 *)

end

type plugin = private
    { p_name: string;
      p_help: string;
      p_parameters: (string, Parameter.t list) Hashtbl.t }
(** Only iterable parameters (see {!do_iterate} and {!do_not_iterate}) are
    registered in the field [p_parameters].
    @since Beryllium-20090901 *)

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
     end) : Bool

  (** Build a boolean option initialized fo [false], that is not saved. *)
  module Action(X: Parameter_input) : Bool

  (** Build a boolean option initialized to [false].
      @plugin development guide *)
  module False(X: Parameter_input) : Bool

  (** Build a boolean option initialized to [true].
      @plugin development guide *)
  module True(X: Parameter_input) : Bool

  (** Build a boolean option initialized to [false]. The returned
      [output] function must be used to display the results of this option.
      The results will be displayed if [X.output_by_default] is [true],
      or if option [-foo-print] is given by the user (where [foo] is
      [X.option_name]).
      @since Nitrogen-20111001
      @plugin development guide *)
  module WithOutput
    (X: sig include Parameter_input val output_by_default: bool end) : 
    WithOutput

  (** Build an integer option.
      @plugin development guide *)
  module Int
    (X: sig val default: int include Parameter_input_with_arg end) : Int

  (** Build an integer option initialized to [0].
      @plugin development guide *)
  module Zero(X:Parameter_input_with_arg) : Int

  (** Build a string option.
      @plugin development guide *)
  module String
    (X: sig include Parameter_input_with_arg val default: string end) : String

  (** Build a string option initialized to [""].
      @plugin development guide *)
  module EmptyString(X: Parameter_input_with_arg) : String

  (** Build an option as a set of strings, initialized to the empty set. *)
  module StringSet(X: Parameter_input_with_arg) : String_set

  (** Should not be used by casual users *)
  module StringList(X: Parameter_input_with_arg) : String_list

  (** @plugin development guide *)
  module IndexedVal (V:Indexed_val_input) : Indexed_val with type value = V.t

  (** @since Boron-20100401 *)
  module StringHashtbl
    (X: Parameter_input_with_arg)
    (V: sig
       include Datatype.S
       val parse: string -> string * t
       val no_binding: string -> t
     end) :
    String_hashtbl with type value = V.t

end

(* ************************************************************************* *)
(** {2 Configuration of functor applications generating parameters}

    You can apply the below functions juste before applying one of the functors
    provided by the functor [Register] and generating a new parameter. *)
(* ************************************************************************* *)

val set_cmdline_stage: Cmdline.stage -> unit
  (** Set the stage where the option corresponding to the parameter is
      recognized. Default is [Cmdline.Configuring].
      @since Beryllium-20090601-beta1 *)

val do_not_journalize: unit -> unit
(** Prevent journalization of the parameter.
    @since Beryllium-20090601-beta1 *)

val do_not_projectify: unit -> unit
(** Prevent projectification of the parameter: its state is shared by all the
    existing projects. Also imply {!do_not_save}.
    @since Beryllium-20090601-beta1 *)

val do_not_save: unit -> unit
(** Prevent serialization of the parameter.
    @since Carbon-20110201 *)

val set_negative_option_name: string -> unit
  (** For boolean parameters, set the name of the negative
      option generating automatically from the positive one (the given option
      name). The default used value prefixes the given option name by "-no".
      Assume that the given string is a valid option name or empty.
      If it is empty, no negative option is created.
      @since Beryllium-20090601-beta1 *)

val set_negative_option_help: string -> unit
(** For boolean parameters, set the help message of the negative
      option generating automatically.
      Assume that the given string is non empty.
      @since Beryllium-20090601-beta1 *)

val set_optional_help: (unit, Format.formatter, unit) format -> unit
  (** Concatenate an additional description just after the default one.
      @since Beryllium-20090601-beta1 *)

val set_group: group -> unit
(** Affect a group to the parameter.
      @since Beryllium-20090901 *)

val is_invisible: unit -> unit
(** Prevent the help to list the parameter. Also imply {!do_not_iterate}.
    @since Carbon-20101201
    @modify Nitrogen-20111001 does not appear in the help *)

val do_iterate: unit -> unit
(** Ensure that {!iter_on_plugins} is applied to this parameter. By default
    only parameters corresponding to options registered at the
    {!Cmdline.Configuring} stage are iterable.
    @since Nitrogen-20111001 *)

val do_not_iterate: unit -> unit
(** Prevent {!iter_on_plugins} to be applied on the parameter. By default, only
    parameters corresponding to options registered at the
    {!Cmdline.Configuring} stage are iterable.
    @since Nitrogen-20111001 *)

(**/**)

val register_kernel: unit -> unit
(** Begin to register parameters of the kernel. Not for casual users.
      @since Beryllium-20090601-beta1 *)

val set_module_name: string -> unit
(** For **kernel** parameters, set the name of the module name corresponding to
    the parameter. Not for casual users. *)

(**/**)

(** Functors for generating plug-ins parameters. *)
module Register
  (P: sig
     val name: string (** Name of the module. Arbitrary non-empty string. *)
     val shortname: string (** Prefix for plugin options. No space allowed. *)
     val help: string (** description of the module. Free-form text. *)
   end) :
  General_services

(* ************************************************************************* *)
(** {2 Handling groups of parameters} *)
(* ************************************************************************* *)

val get: string -> plugin
(** Get a plug-in from its shortname.
    Not very efficient yet.
    @since Nitrogen-20111001 *)

val iter_on_plugins: (plugin -> unit) -> unit
  (** Iterate on each registered plug-ins.
      @since Beryllium-20090901 *)

val get_selection: unit -> State_selection.t
  (** Selection of all the settable parameters.
      @plugin development guide *)

val get_selection_context: unit -> State_selection.t
(** Selection of all the parameters which may have an impact on some
    analysis. *)

val positive_debug_ref: int ref
  (** Not for casual users.
      @since Boron-20100401 *)

(* ************************************************************************* *)
(** {2 Deprecated API} *)
(* ************************************************************************* *)

val at_normal_exit: (unit -> unit) -> unit
  (** Now replaced by {!Cmdline.at_normal_exit}.
      @since Beryllium-20090901
      @deprecated since Boron-20100401 *)

val run_normal_exit_hook: unit -> unit
  (** Now replaced by {!Cmdline.run_normal_exit_hook}.
      @since Beryllium-20090901
      @deprecated since Boron-20100401 *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
