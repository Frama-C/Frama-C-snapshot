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

(** Command line parsing. 
    @plugin development guide *)

(** {2 Stage configurations}

    Frama-C uses several stages for parsing its command line.
    Each of them may be customized. *)

type stage = 
  | Early       (** @plugin development guide *)
  | Extending   (** @plugin development guide *)
  | Extended    (** @plugin development guide *) 
  | Exiting     (** @plugin development guide *)
  | Loading     (** @plugin development guide *)
  | Configuring (** @plugin development guide *)
    (** The different stages, from the first to be executed to the last one.
        @since Beryllium-20090601-beta1 *)

val run_after_early_stage: (unit -> unit) -> unit
  (** Register an action to be executed at the end of the early stage.
      @plugin development guide
      @since Beryllium-20090901 *)

val run_during_extending_stage: (unit -> unit) -> unit
  (** Register an action to be executed during the extending stage.
      @plugin development guide
      @since Beryllium-20090901 *)

val run_after_extended_stage: (unit -> unit) -> unit
  (** Register an action to be executed at the end of the extended stage.
      @plugin development guide
      @since Beryllium-20090901 *)

type exit
  (** @since Beryllium-20090901 *)

val nop : exit
  (** @since Beryllium-20090901
      @plugin development guide *)

exception Exit
  (** @since Beryllium-20090901
      @plugin development guide *)

val run_after_exiting_stage: (unit -> exit) -> unit
  (** Register an action to be executed at the end of the exiting stage.
      The guarded action must finish by [exit n].
      @plugin development guide
      @since Beryllium-20090601-beta1 *)

val run_after_loading_stage: (unit -> unit) -> unit
  (** Register an action to be executed at the end of the loading stage.
      @plugin development guide
      @since Beryllium-20090601-beta1 *)

val is_going_to_load: unit -> unit
  (** To be call if one action is going to run after the loading stage.
      It is not necessary to call this function if the running action is set by
      an option put on the command line.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

val run_after_configuring_stage: (unit -> unit) -> unit
  (** Register an action to be executed at the end of the configuring stage.
      @plugin development guide
      @since Beryllium-20090601-beta1 *)

val run_after_setting_files: (string list -> unit) -> unit
(** Register an action to be executed just after setting the files put on the
    command line. The argument of the function is the list of files.
    @plugin development guide
    @since Carbon-20101201 *)

val protect: exn -> string
  (** Messages for exceptions raised by Frama-C
      @since Boron-20100401 *)

val catch_at_toplevel: exn -> bool
(** @return true iff the given exception is caught by the Frama-C toplevel.
    @since Boron-20100401 *)

val catch_toplevel_run:
  f:(unit -> unit) ->
  quit:bool ->
  at_normal_exit:(unit -> unit) ->
  on_error:(exn -> unit) ->
  unit
    (** Run [f]. When done, either call [at_normal_exit] if running [f] was ok;
        or call [on_error] in other cases.
        Set [quit] to [true] iff Frama-C must stop after running [f].
        @modify Boron-20100401  additional arguments. They are now
        labelled
	@modify Fluorine-20130601+Dev add the exception as argument of
	[on_error]. *)

val at_normal_exit: (unit -> unit) -> unit
  (** Register a hook executed whenever Frama-C exits without error (the exit
      code is 0).
      @since Boron-20100401 *)

val run_normal_exit_hook: unit -> unit
  (** Run all the hooks registered by {!at_normal_exit}.
      @since Boron-20100401 *)

val at_error_exit: (exn -> unit) -> unit
  (** Register a hook executed whenever Frama-C exits with error (the exit
      code is greater than 0). The argument of the hook is the exception at the
      origin of the error.
      @since Boron-20100401
      @modify Neon-20130301 add the exception as argument of the
      hook. *)

val run_error_exit_hook: exn -> unit
  (** Run all the hooks registered by {!at_normal_exit}.
      @since Boron-20100401
      @modify Neon-20130301 add the exception as argument. *)

val error_occurred: exn -> unit
  (** Remember that an error occurred.
      So {!run_error_exit_hook} will be called when Frama-C will exit.
      @since Boron-20100401
      @modify Neon-20130301 add the exception as argument, 
      fix spelling. *)

val bail_out: unit -> 'a
  (** Stop Frama-C with exit 0.
      @since Boron-20100401 *)

(** {2 Special functions}

    These functions should not be used by a standard plug-in developer. *)

val parse_and_boot:
  (string option -> (unit -> unit) -> unit) ->
  (unit -> (unit -> unit) -> unit) ->
  (unit -> unit) -> unit
(** Not for casual users.
    [parse_and_boot on_from_name get_toplevel play] performs the
    parsing of the command line, then play the analysis with the good
    toplevel provided by [get_toplevel]. [on_from_name] is [Project.on] on the
    project corresponding to the given (unique) name (or the default project if
    [None]).
    @since Beryllium-20090901
    @modify Carbon-20101201 *)

val nb_given_options: unit -> int
  (** Number of options provided by the user on the command line.
      Should not be called before the end of the command line parsing.
      @since Beryllium-20090601-beta1 *)

val use_cmdline_files: (string list -> unit) -> unit
  (** What to do with the list of files put on the command lines.
      @since Beryllium-20090601-beta1 *)

(** @since Beryllium-20090901 *)
module Group : sig
  type t (** @since Beryllium-20090901 *)
  val default: t (** @since Beryllium-20090901 *)
  val add: ?memo:bool -> plugin:string -> string -> t * bool
    (** Add a new group of options to the given plugin.
        If [memo] is [true], just return the already registered group if any.
        If [memo] is [false], cannot add twice a group with the same name.
        @return the group corresponding to the given name. Also return [true]
        iff the group has just been created.
        @since Beryllium-20090901 *)
  val name: t -> string
    (** @since Beryllium-20090901 *)
end

val help: unit -> exit
  (** Display the help of Frama-C
      @since Beryllium-20090601-beta1 *)

val plugin_help: string -> exit
  (** Display the help of the given plug-in (given by its shortname).
      @since Beryllium-20090601-beta1 *)

val print_option_help: 
  Format.formatter -> plugin:string -> group:Group.t -> string -> unit
(** Pretty print the help of the option (given by its plug-in, its group and its
    name) in the provided formatter.
    @since Oxygen-20120901 *)

val add_plugin: ?short:string -> string -> help:string -> unit
  (** [add_plugin ~short name ~help] adds a new plug-in recognized by the
      command line of Frama-C. If the shortname is not specified, then the name
      is used as the shortname. By convention, if the name and the shortname
      are equal to "", then the register "plug-in" is the Frama-C kernel
      itself.
      @raise Invalid_argument if the same shortname is registered twice
      @since Beryllium-20090601-beta1 *)

(** @since Beryllium-20090601-beta1 *)
type option_setting =
  | Unit of (unit -> unit)
  | Int of (int -> unit)
  | String of (string -> unit)
  | String_list of (string list -> unit)

val add_option:
  string ->
  plugin:string ->
  group:Group.t ->
  stage ->
  ?argname:string ->
  help:string ->
  visible:bool ->
  ext_help:(unit,Format.formatter,unit) format ->
  option_setting ->
  unit
    (** [add_option name ~plugin stage ~argname ~help setting]
        adds a new option of the given [name] recognized by the command line of
        Frama-C. If the [name] is the empty string, nothing is done.
        [plugin] is the shortname of the plug-in.
        [argname] is the name of the argument which can be used of the
        description [help]. Both of them are used by the help of the
        registered option. If [help] is [None], then the option is not shown
        in the help.
        @since Beryllium-20090601-beta1
        @modify Carbon-20101201 
	@modify Oxygen-20120901 change type of ~help and add ~visible. *)

val add_option_without_action:
  string ->
  plugin:string ->
  group:Group.t ->
  ?argname:string ->
  help:string ->
  visible:bool ->
  ext_help:(unit,Format.formatter,unit) format ->
  unit ->
  unit
(** Equivalent to [add_option] without option setting.
    Thus do not add the option to any stage of the command line...
    Thus should not be used by casual users ;-).
    @since Carbon-20101201 *)

val add_aliases:
  string ->
  plugin:string ->
  group:Group.t ->
  stage ->
  string list ->
  unit
(** [add_aliases orig plugin group aliases] adds a list of aliases to the given
    option name [orig].
    @Invalid_argument if an alias name is the empty string
    @since Carbon-20110201 *)

(** {2 Special parameters}

    Frama-c parameters depending on the command line argument and set at the
    very beginning of the Frama-C initialisation.

    They should not be used directly by a standard plug-in developer. *)

module Kernel_log: Log.Messages
(** @since Neon-20130301 *)

(** @since Fluorine-20130401 *)
module type Level = sig
  val value_if_set: int option ref
  val get: unit -> int
  val set: int -> unit
end

module Debug_level: Level
(** @since Fluorine-20130401 *)

module Verbose_level: Level
(** @since Fluorine-20130401 *)

module Kernel_debug_level: Level
(** @since Fluorine-20130401 *)

module Kernel_verbose_level: Level
(** @since Fluorine-20130401 *)

val kernel_debug_atleast_ref: (int -> bool) ref
  (** @since Boron-20100401 *)

val kernel_verbose_atleast_ref: (int -> bool) ref
  (** @since Boron-20100401 *)

val journal_enable: bool
  (** @since Beryllium-20090601-beta1 *)

val journal_isset: bool
  (** -journal-enable/disable explicitly set on the command line.
      @since Boron-20100401 *)

val use_obj: bool
  (** @since Beryllium-20090601-beta1 *)

val use_type: bool
  (** @since Beryllium-20090601-beta1 *)

val quiet: bool
  (** Must not be used for something else that initializing values
      @since Beryllium-20090601-beta1 *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
