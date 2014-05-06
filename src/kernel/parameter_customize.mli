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

(** Configuration of command line options.

    You can apply the below functions juste before applying one of the functors
    provided by the functor {!Plugin.Register} and generating a new
    parameter. 

    @plugin development guide *)

val set_cmdline_stage: Cmdline.stage -> unit
(** Set the stage where the option corresponding to the parameter is
    recognized. Default is [Cmdline.Configuring].
    @since Beryllium-20090601-beta1 *)

val do_not_journalize: unit -> unit
(** Prevent journalization of the parameter.
    @since Beryllium-20090601-beta1 *)

val do_not_projectify: unit -> unit
(** Prevent projectification of the parameter: its state is shared by all the
    existing projects. Also imply {!do_not_save} and {!do_not_reset_on_copy}.
    @since Beryllium-20090601-beta1 *)

val do_not_reset_on_copy: unit -> unit
  (** Prevents resetting the parameter to its default value when creating
      a project from a copy visitor.
      @since Neon-20130301 *)

val do_not_save: unit -> unit
(** Prevent serialization of the parameter.
    @since Carbon-20110201 *)

val set_negative_option_name: string -> unit
  (** For boolean parameters, set the name of the negative
      option generating automatically from the positive one (the given option
      name). The default used value prefixes the given option name by "-no".
      Assume that the given string is a valid option name or empty.
      If it is empty, no negative option is created.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

val set_negative_option_help: string -> unit
(** For boolean parameters, set the help message of the negative
      option generating automatically.
      Assume that the given string is non empty.
      @since Beryllium-20090601-beta1 *)

val set_unset_option_name: string -> unit
  (** For string collection parameters, set the name of an option that
      will remove elements from the set. There is no default value: if
      the this function is not called (or if it is the empty string),
      it will only be possible to add elements from the command line.
      @since Fluorine-20130401 *)

val set_unset_option_help: string -> unit
  (** For string collection parameters, gives the help message for
      the corresponding unset option. Useless if [set_unset_option_name]
      has not been called before. No default.
      @since Fluorine-20130401 *)

val set_optional_help: (unit, Format.formatter, unit) format -> unit
  (** Concatenate an additional description just after the default one.
      @since Beryllium-20090601-beta1
      @deprecated since Oxygen-20120901: directly use the help string
      instead. *)

val set_group: Cmdline.Group.t -> unit
(** Affect a group to the parameter.
      @since Beryllium-20090901 *)

val is_invisible: unit -> unit
(** Prevent the help to list the parameter. Also imply {!do_not_iterate}.
    @since Carbon-20101201
    @modify Nitrogen-20111001 does not appear in the help *)

val argument_is_function_name: unit -> unit
(** Indicate that the string argument of the parameter must be a valid function
    name (or a set of valid function names). A valid function name is the name
    of a function defined in the analysed C program.
    Do nothing if the following applied functor has type [String], [String_set]
    or [String_list].
    @since Oxygen-20120901 *)

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
(* ************************************************************************* *)
(** {2 Internal kernel stuff} *)
(* ************************************************************************* *)

val reset: unit -> unit
(** Reset all customizers to their default values. *)

val set_module_name: string -> unit
(** For **kernel** parameters, set the name of the module name corresponding to
    the parameter. Not for casual users. *)

val set_function_names: (unit -> string list) -> unit
(** @since Oxygen-20120901 *)

val set_ast_hook: ((Cil_types.file -> unit) -> unit) -> unit
(** @since Oxygen-20120901 *)

val init_ast_hooks: (Cil_types.file -> unit) list ref
(** @since Oxygen-20120901 *)

val apply_ast_hook: (string list -> unit) -> unit

(* ************************************************************************* *)
(** {3 The customizers themselves} *)
(* ************************************************************************* *)

val cmdline_stage_ref: Cmdline.stage ref
val journalize_ref: bool ref
val negative_option_name_ref: string option ref
val negative_option_help_ref: string ref
val unset_option_name_ref: string ref
val unset_option_help_ref: string ref
val must_save_ref: bool ref
val reset_on_copy_ref: bool ref
val projectify_ref: bool ref
val optional_help_ref: (unit, Format.formatter, unit) format ref
val argument_is_function_name_ref: bool ref
val group_ref: Cmdline.Group.t ref
val do_iterate_ref: bool option ref
val is_visible_ref: bool ref
val module_name_ref: string ref

(**/**)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
