(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Signatures for command line options. 
    @plugin development guide *)

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

(* ************************************************************************** *)
(** {2 Output signatures} 

    Signatures corresponding to a command line option of a specific type. *)
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

(** Generic signature of a parameter.
    @plugin development guide *)
module type S = sig
  include S_no_parameter
  val parameter: Typed_parameter.t
  (** @since Nitrogen-20111001 *)
end


(** Signature for a boolean parameter.
    @plugin development guide *)
module type Bool = sig

  include S with type t = bool

  val on: unit -> unit
    (** Set the boolean to [true]. *)

  val off: unit -> unit
    (** Set the boolean to [false]. *)

end

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

end

(** Signature for a generic set of strings option. *)
module type String_collection = sig

  include S

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

  val fold: (string -> 'a -> 'a) -> 'a -> 'a
    (** Fold on each string in the set.
        @since Oxygen-20120901 *)

  val exists: (string -> bool) -> bool
    (** Checks if at least one element of the set satisfies the predicate.
    @since Carbon-20101201 *)

  val set_possible_values: string list -> unit
    (** Set what are the acceptable values for this parameter.
        If the given list is empty, then all values are acceptable.
        @since Oxygen-20120901 *)

  val get_possible_values: unit -> string list
    (** What are the acceptable values for this parameter.
        If the returned list is empty, then all values are acceptable.
        @since Oxygen-20120901 *)

end

(** @plugin development guide *)
module type String_set = String_collection with type t = Datatype.String.Set.t
module type String_list = 
  sig
    include String_collection with type t = string list
    val append_before: string list -> unit
    (** append a list in front of the current state
        @since Neon-20130301
     *)

    val append_after: string list -> unit
    (** append a list at the end of the current state
        @since Neon-20130301 *)
  end

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

(** input signature for [IndexedVal] *)
module type Indexed_val_input = sig
  include Input_with_arg
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val ty: t Type.t
end

(** signature for searching files in a specific directory. *)
module type Specific_dir = sig

  exception No_dir

  val force_dir: bool
  (** For functions below: if [force_dir] is true: if [error] is [false], then
      creates the directory if it does not exist (or raises No_dir if the
      directory cannot be created). Otherwise ([force_dir =
      false]), raise No_dir if [error] is [false]. 
      @since Neon-20130301 *) 

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
(** {2 All the different kinds of command line options as functors} *)
(* ************************************************************************** *)

(** Signatures containing the different functors which may be used to generate
    new command line options.
    @plugin development guide *)
module type Builder = sig

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
  module EmptyString(X: Input_with_arg): String

  (** @plugin development guide *)
  module StringSet(X: Input_with_arg): String_set
  
  module FilledStringSet
    (X: sig 
      include Input_with_arg
      val default: Datatype.String.Set.t 
    end): 
    String_set

  module StringList(X: Input_with_arg): String_list

  module IndexedVal(V: Indexed_val_input): Indexed_val with type value = V.t

  module StringHashtbl
    (X: Input_with_arg)
    (V: sig
      include Datatype.S
      val parse: string -> string * t
      val redefine_binding: string -> old:t -> t -> t
      val no_binding: string -> t
    end) :
    String_hashtbl with type value = V.t

  val parameters: unit -> Typed_parameter.t list    

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
