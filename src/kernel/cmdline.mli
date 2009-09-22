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

(** Command line parsing. *)

(** {2 Stage configurations}

    Frama-C uses several stages for parsing its command line. 
    Each of them may be customized. *)
 
type stage = Early | Extending | Extended | Exiting | Loading | Configuring
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
  (** Register an action to be executed at the end of the extending stage. 
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
      It is not necessary to call this function if the running action if set by
      an option put on the command line. 
      @since Beryllium-20090601-beta1 
      @plugin development guide *)

val run_after_configuring_stage: (unit -> unit) -> unit
  (** Register an action to be executed at the end of the configuring stage. 
      @plugin development guide
      @since Beryllium-20090601-beta1 *)

val catch_toplevel_run: (unit -> unit) -> (unit -> unit) -> unit

(** {2 Special functions} 

    These functions should not be used by a standard plug-in developer. *)

val parse_and_boot: (unit -> (unit -> unit) -> unit) -> (unit -> unit) -> unit
  (** Not for casual users. [parse_and_boot get_toplevel play] performs the
      parsing of the command line, then play the analysis with the good
      toplevel provided by [get_toplevel].  
      @since Beryllium-20090901 *)

val nb_given_options: unit -> int
  (** Number of options provided by the user on the command line.
      Should not be called before the end of the command line parsing.
      @since Beryllium-20090601-beta1 *)

val use_cmdline_files: (string list -> unit) -> unit
  (** What to do with the list of files put on the command lines.
      @since Beryllium-20090601-beta1 *)

val help: unit -> exit
  (** Display the help of Frama-C
      @since Beryllium-20090601-beta1 *)

val plugin_help: string -> exit
  (** Display the help of the given plug-in (given by its shortname).
      @since Beryllium-20090601-beta1 *)

val add_plugin: ?short:string -> string -> descr:string -> unit
  (** [add_plugin ~short name ~descr] adds a new plug-in recognized by the
      command line of Frama-C. If the shortname is not specified, then the name
      is used as the shortname. By convention, if the name and the shortname
      are equal to "", then the register "plug-in" is the Frama-C kernel
      itself. 
      @since Beryllium-20090601-beta1 *)

(** @since Beryllium-20090901 *)
module Group : sig
  type t (** @since Beryllium-20090901 *)
  val default: t (** @since Beryllium-20090901 *)
  val add: plugin:string -> string -> t
    (** Add a new group of options to the given plugin
	@since Beryllium-20090901 *)
  val name: t -> string
    (** @since Beryllium-20090901 *)
end

(** @since Beryllium-20090601-beta1 *)
type option_setting =
  | Unit of (unit -> unit)
  | Int of (int -> unit)
  | String of (string -> unit)
  | String_list of (string list -> unit)

val add_option:
  ?prefix:bool ->
  string ->
  plugin:string ->
  group:Group.t ->
  stage ->
  ?argname:string ->
  descr:string option ->
  ext_descr:(unit,Format.formatter,unit) format ->
  option_setting ->
  unit
    (** [add_option ~prefix name ~plugin stage ~argname ~descr setting] 
	adds a new option recognized by the command line of Frama-C.
	The prefix is unused now. [plugin] is the shortname of the plug-in. 
	[argname] is the name of the argument which can be used of the
	description [descr]. Both of them are used by the help of the
	registered option. If [descr] is [None], then the option is not shown
	in the help. 
	@since Beryllium-20090601-beta1 *)

(** {2 Special parameters}

    Frama-c parameters depending on the command line argument and set at the
    very beginning of the Frama-C initialisation. 

    They should not be used directly by a standard plug-in developer. *)

val debug_level: int
  (** @since Beryllium-20090601-beta1 *)

val verbose_level: int
  (** @since Beryllium-20090601-beta1 *)

val debug_isset: bool
  (** @since Beryllium-20090601-beta1 *)

val verbose_isset: bool
  (** @since Beryllium-20090601-beta1 *)

val kernel_debug_level: int
  (** @since Beryllium-20090901 *)

val kernel_verbose_level: int
  (** @since Beryllium-20090901 *)

val quiet: bool
  (** @since Beryllium-20090601-beta1 *)

val journal_enable: bool
  (** @since Beryllium-20090601-beta1 *)

val journal_name: string
  (** @since Beryllium-20090601-beta1 *)

val use_obj: bool
  (** @since Beryllium-20090601-beta1 *)

val use_type: bool
  (** @since Beryllium-20090601-beta1 *)

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)
