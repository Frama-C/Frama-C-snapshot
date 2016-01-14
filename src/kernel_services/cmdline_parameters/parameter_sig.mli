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

(** Signatures for command line options. *)

(* ************************************************************************** *)
(** {2 Input signatures} 

    One of these signatures is required to implement a new command line
    option. *)
(* ************************************************************************** *)

(** Minimal signature to implement for each parameter corresponding to an
    option on the command line argument. *)
module type Input = sig
  val option_name: string
    (** The name of the option *)
  val help: string
(** A description for this option (e.g. used by -help).
    If [help = ""], then it has the special meaning "undocumented" *)
end

(** Minimal signature to implement for each parameter corresponding to an
    option on the command line argument which requires an argument. *)
module type Input_with_arg = sig
  include Input
  val arg_name: string
    (** A standard name for the argument which may be used in the description.
        If empty, a generic arg_name is generated. *)
end

(** Signature required to build custom collection parameters in which elements
    are convertible to string.
    @since Sodium-20150201 *)
module type String_datatype = sig
  include Datatype.S

  val of_string: string -> t
  (** @raise Cannot_build if there is no element corresponding to the given
      string. *)

  val to_string: t -> string
end

(** Signature requires to build custom collection parameters in which elements
    are convertible to string.
    @since Sodium-20150201 *)
module type String_datatype_with_collections = sig
  include Datatype.S_with_collections

  val of_string: string -> t
  (** @raise Cannot_build if there is no element corresponding to the given
      string. *)

  val of_singleton_string: string -> Set.t
  (** If a single string can be mapped to several elements. Can
      default to {!no_element_of_string} to indicate that each string [s] is
      mapped exactly to [of_string s].
   *)

  val to_string: t -> string
end

(** Signature of the optional value associated to the key and required to build
    map parameters.
    @since Sodium-20150201 *)
module type Value_datatype = sig
  include Datatype.S
  type key

  val of_string: key:key -> prev:t option -> string option -> t option
  (** [key] is the key associated to this value, while [prev] is the previous
      value associated to this key (if any). The optional string is [None] if
      there is no value associated to the key, and [Some v] (potentially [v =
      ""]) otherwise.
      @return None if there is no value to associate to the key or [Some v]
      otherwise.
      @raise Cannot_build if there is no element corresponding to the given
      string. *)

  val to_string: key:key -> t option -> string option
(** [key] is the key associated to this value. The optional string is [None] if
    there is no value associated to the key, and [Some v] (potentially [v =
    ""]) otherwise.
    @return None if there is no value to associate to the key or [Some v]
    otherwise. *)

end

(** Signature of the optional value associated to the key and required to build
    multiple map parameters. Almost similar to {!Value_datatype}.
    @since Sodium-20150201 *)
module type Multiple_value_datatype = sig
  include Datatype.S
  type key
  val of_string: key:key -> prev:t list option -> string option -> t option
  val to_string: key:key -> t option -> string option
end

(* ************************************************************************** *)
(** {2 Output signatures} 

    Signatures corresponding to a command line option of a specific type. *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(** {3 Generic signatures} *)
(* ************************************************************************** *)

(** Generic signature of a parameter, without [parameter]. *)
module type S_no_parameter = sig

  type t
  (** Type of the parameter (an int, a string, etc). It is concrete for each
      module implementing this signature. *)

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
    (** Option value (not necessarily set on the current command line). *)

  val clear: unit -> unit
    (** Set the option to its default value, that is the value if [set] was
        never called. *)

  val is_default: unit -> bool
    (** Is the option equal to its default value? *)

  val option_name: string
    (** Name of the option on the command-line
        @since Carbon-20110201  *)

  val print_help: Format.formatter -> unit
  (** Print the help of the parameter in the given formatter as it would be
      printed on the command line by -<plugin>-help. For invisible parameters,
      the string corresponds to the one returned if it would be not invisible.
      @since Oxygen-20120901 *)

  include State_builder.S

  val equal: t -> t -> bool

  val add_aliases: string list -> unit
  (** Add some aliases for this option. That is other option names which have
      exactly the same semantics that the initial option.
      @raise Invalid_argument if one of the strings is empty *)

  (**/**)
  val is_set: unit -> bool
  (** Is the function {!set} has already been called since the last call to
      function {!clear}? This function is for special uses and should mostly
      never be used. *)

  val unsafe_set: t -> unit
(** Set but without clearing the dependencies.*)
(**/**)

end

(** Generic signature of a parameter. *)
module type S = sig
  include S_no_parameter
  val parameter: Typed_parameter.t
  (** @since Nitrogen-20111001 *)
end

(* ************************************************************************** *)
(** {3 Signatures for simple datatypes} *)
(* ************************************************************************** *)

(** Signature for a boolean parameter.
    @plugin development guide *)
module type Bool = sig

  include S with type t = bool

  val on: unit -> unit
    (** Set the boolean to [true]. *)

  val off: unit -> unit
    (** Set the boolean to [false]. *)

end

(** Signature for an integer parameter.
    @plugin development guide *)
module type Int = sig

  include S with type t = int

  val incr: unit -> unit
    (** Increment the integer. *)

  val set_range: min:int -> max:int -> unit
    (** Set what is the possible range of values for this parameter.
        @since Beryllium-20090901 *)

  val get_range: unit -> int * int
    (** What is the possible range of values for this parameter.
        @since Beryllium-20090901 *)

end

(** Signature for a string parameter. *)
module type String = sig

  include S with type t = string

  val set_possible_values: string list -> unit
    (** Set what are the acceptable values for this parameter.
        If the given list is empty, then all values are acceptable.
        @since Beryllium-20090901 *)

  val get_possible_values: unit -> string list
    (** What are the acceptable values for this parameter.
        If the returned list is empty, then all values are acceptable.
        @since Beryllium-20090901 *)

  val get_function_name: unit -> string
    (** returns the given argument only if it is a valid function name
        (see {!Parameter_customize.get_c_ified_functions} for more information),
        and abort otherwise.

        Requires that the AST has been computed. Default getter when
        {!Parameter_customize.argument_is_function_name} has been called.
        @since Sodium-20150201
     *)

  val get_plain_string: unit -> string
  (** always return the argument, even if the argument is not a function name.
      @since Sodium-20150201
   *)
end

(* ************************************************************************** *)
(** {3 Custom signatures} *)
(* ************************************************************************** *)

(** Signature for a boolean parameter that causes something to be output. *)
module type With_output = sig
  include Bool

  val set_output_dependencies: State.t list -> unit
  (** Set the dependencies for the output of the option. Two successive
      calls to [output] below will cause only one output, unless some
      of the supplied dependencies have changed between the two calls. *)

  val output: (unit -> unit) -> unit
  (** To be used by the plugin to output the results of the option
      in a controlled way. See [set_output_dependencies] details. *)
end

(** signature for searching files in a specific directory. *)
module type Specific_dir = sig

  exception No_dir

  val force_dir: bool
  (** For functions below: if [force_dir] is true: if [error] is [false], then
      creates the directory if it does not exist (or raises No_dir if the
      directory cannot be created). Otherwise ([force_dir =
      false]), raise No_dir if [error] is [false]. 
      @since Neon-20140301 *) 

  val dir: ?error:bool -> unit -> string
    (** [dir ~error ()] returns the specific directory name, if
        any. Otherwise, Frama-C halts on an user error if [error] orelse the
	behavior depends on [force_dir]. Default of [error] is [true].
        @raise No_dir if there is no share directory for this plug-in and [not
        error] and [not force_dir]. *)

  val file: ?error:bool -> string -> string
  (** [file basename] returns the complete filename of a file stored in [dir
      ()]. If there is no such directory, Frama-C halts on an user error if
      [error] orelse the behavior depends on [force_dir]. Default of [error] is
      [true].
      @raise No_dir if there is no share directory for this plug-in and [not
      error] and [not force_dir].  *)

  module Dir_name: String
(** Option [-<short-name>-<specific-dir>]. *)

end

(* ************************************************************************** *)
(** {3 Collections} *)
(* ************************************************************************** *)

(** Signature for a category over a collection.
    @since Sodium-20150201 *)
module type Collection_category = sig
  type elt (** Element in the category *)
  type t = elt Parameter_category.t

  val none: t
  (** The category '\@none' *)

  val default: unit -> t
  (** The '\@default' category. By default, it is {!none}. *)

  val set_default: t -> unit
  (** Modify the '\@default' category. *)

  val add: string -> State.t list -> elt Parameter_category.accessor -> t
  (** Adds a new category for this collection with the given name, accessor and
      dependencies. *)

  val enable_all: State.t list -> elt Parameter_category.accessor -> t
  (** The category '\@all' is enabled in positive occurrences, with the given
      interpretation. In negative occurrences, it is always enabled and '-\@all'
      means 'empty'. *)

  val enable_all_as: t -> unit
  (** The category '\@all' is equivalent to the given category. *)

end

(** Common signature to all collections.
    @since Sodium-20150201 *)
module type Collection = sig

  include S
  (** A collection is a standard command line parameter. *)

  type elt
  (** Element in the collection. *)

  val is_empty: unit -> bool
  (** Is the collection empty? *)

  val iter: (elt -> unit) -> unit
  (** Iterate over all the elements of the collection. *)

  val fold: (elt -> 'a -> 'a) -> 'a -> 'a
  (** Fold over all the elements of the collection. *)

  val add: elt -> unit
  (** Add an element to the collection *)

  module As_string: String
  (** A collection is a standard string parameter *)

  module Category: Collection_category with type elt = elt
(** Categories for this collection. *)
end

(** Signature for sets as command line parameters.
    @since Sodium-20150201 *)
module type Set = sig

  include Collection
  (** A set is a collection. *)

  (** {3 Additional accessors to the set.} *)

  val mem: elt -> bool
  (** Does the given element belong to the set? *)

  val exists: (elt -> bool) -> bool
(** Is there some element satisfying the given predicate? *)

end

(** @modify Sodium-20150201 *)
module type String_set =
  Set with type elt = string and type t = Datatype.String.Set.t

(** Set of defined kernel functions. If you want to also include pure
    prototype, use {!Parameter_customize.argument_may_be_fundecl}.
    @since Sodium-20150201
    @plugin development guide *)
module type Kernel_function_set =
  Set with type elt = Cil_types.kernel_function
      and type t = Cil_datatype.Kf.Set.t

(** @since Sodium-20150201 *)
module type Fundec_set =
  Set with type elt = Cil_types.fundec
      and type t = Cil_datatype.Fundec.Set.t

(** Signature for lists as command line parameters.
    @since Sodium-20150201 *)
module type List =  sig

  include Collection
  (** A list is a collection. *)

  (** {3 Additional accessors to the list.} *)

  val append_before: t -> unit
    (** append a list in front of the current state
        @since Neon-20140301 *)

  val append_after: t -> unit
  (** append a list at the end of the current state
      @since Neon-20140301 *)

end

(** @modify Sodium-20150201 *)
module type String_list = List with type elt = string and type t = string list

(** Signature for maps as command line parameters.
    @since Sodium-20150201 *)
module type Map = sig

  type key (** Type of keys of the map. *)
  type value (** Type of the values associated to the keys. *)

  include Collection with type elt = key * value option
  (** A map is a collection in which elements are pairs [(key, value)], but some
      values may be missing. *)

  (** {3 Additional accessors to the map.} *)

  val find: key -> value
(** Search a given key in the map.
    @raise Not_found if there is no such key in the map. *)

  val mem: key -> bool

end

(** Signature for multiple maps as command line parameters. Almost similar to
    {!Map}.
    @since Sodium-20150201 *)
module type Multiple_map = sig
  type key
  type value
  include Collection with type elt = key * value list
  val find: key -> value list
  val mem: key -> bool
end

(* ************************************************************************** *)
(** {2 All the different kinds of command line options as functors} *)
(* ************************************************************************** *)

(** Signatures containing the different functors which may be used to generate
    new command line options.
    @plugin development guide *)
module type Builder = sig

  val no_element_of_string: string -> 'a
  (** @raise Cannot_build for any entry
      @since Sodium-20150201
  *)

  module Bool(X:sig include Input val default: bool end): Bool
  module Action(X: Input) : Bool

  (** @plugin development guide *)
  module False(X: Input) : Bool

  (** @plugin development guide *)
  module True(X: Input) : Bool

  module WithOutput
    (X: sig include Input val output_by_default: bool end): 
    With_output

  (** @plugin development guide *)
  module Int(X: sig include Input_with_arg val default: int end): Int

  (** @plugin development guide *)
  module Zero(X: Input_with_arg): Int

  (** @plugin development guide *)
  module String(X: sig include Input_with_arg val default: string end): String

  (** @plugin development guide *)
  module Empty_string(X: Input_with_arg): String

  exception Cannot_build of string
  module Make_set
    (E:
      sig
        include String_datatype_with_collections
        val of_singleton_string: string -> Set.t
      end)
    (X: sig include Input_with_arg val default: E.Set.t end):
    Set with type elt = E.t and type t = E.Set.t

  (** @plugin development guide *)
  module String_set(X: Input_with_arg): String_set

  module Filled_string_set
    (X: sig
      include Input_with_arg
      val default: Datatype.String.Set.t
    end): String_set

  (** @plugin development guide *)
  module Kernel_function_set(X: Input_with_arg): Kernel_function_set
  module Fundec_set(X: Input_with_arg): Fundec_set

  module Make_list
    (E:
      sig
        include String_datatype
        val of_singleton_string: string -> t list
      end)
    (X: sig include Input_with_arg val default: E.t list end):
    List with type elt = E.t and type t = E.t list

  module String_list(X: Input_with_arg): String_list

  (** Parameter is a map where multibindings are **not** allowed. *)
  module Make_map
    (K: String_datatype_with_collections)
    (V: Value_datatype with type key = K.t)
    (X: sig include Input_with_arg val default: V.t K.Map.t end):
    Map
    with type key = K.t and type value = V.t and type t = V.t K.Map.t

  module String_map
    (V: Value_datatype with type key = string)
    (X: sig include Input_with_arg val default: V.t Datatype.String.Map.t end):
    Map
    with type key = string
    and type value = V.t
    and type t = V.t Datatype.String.Map.t

  (** As for Kernel_function_set, by default keys can only be defined functions.
      Use {!Parameter_customize.argument_may_be_fundecl} to also include
      pure prototypes. *)
  module Kernel_function_map
    (V: Value_datatype with type key = Cil_types.kernel_function)
    (X: sig include Input_with_arg val default: V.t Cil_datatype.Kf.Map.t end):
    Map
    with type key = Cil_types.kernel_function
    and type value = V.t
    and type t = V.t Cil_datatype.Kf.Map.t

  (** Parameter is a map where multibindings are allowed. *)
  module Make_multiple_map
    (K: String_datatype_with_collections)
    (V: Multiple_value_datatype with type key = K.t)
    (X: sig include Input_with_arg val default: V.t list K.Map.t end):
    Multiple_map
    with type key = K.t and type value = V.t and type t = V.t list K.Map.t

  module String_multiple_map
    (V: Multiple_value_datatype with type key = string)
    (X: sig
      include Input_with_arg
      val default: V.t list Datatype.String.Map.t
    end):
    Multiple_map
    with type key = string
    and type value = V.t
    and type t = V.t list Datatype.String.Map.t

  (** As for Kernel_function_set, by default keys can only be defined functions.
      Use {!Parameter_customize.argument_may_be_fundecl} to also include
      pure prototypes. *)
  module Kernel_function_multiple_map
    (V: Multiple_value_datatype with type key = Cil_types.kernel_function)
    (X: sig
      include Input_with_arg
      val default: V.t list Cil_datatype.Kf.Map.t
    end):
    Multiple_map
    with type key = Cil_types.kernel_function
    and type value = V.t
    and type t = V.t list Cil_datatype.Kf.Map.t

  val parameters: unit -> Typed_parameter.t list

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
