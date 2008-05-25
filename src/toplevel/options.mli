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

(** Options and plugins for Frama-C *)

val add_cmdline:
  ?name:string ->
  ?shortname:string ->
  ?debug:(Arg.key * Arg.spec * Arg.doc) list ->
  (Arg.key * Arg.spec * Arg.doc) list ->
  unit
    (** [add_cmdline ~name ~shortname ~debug options] adds general command-line
	options. 

	[name] (if specified) is used to add a new command-line section with
	given options. [debug] are debug options. These options are given on
	the command line as a unique string argument of -[shortname]-debug. If
	[shortname] is not specified, [name] is used instead. In the given
	string, a [;] is interpreted as a blank separator.

	This function should not be used by plugins: they have to use
	{!add_plugin} instead. *)

val add_plugin:
  name:string ->
  descr:string ->
  ?plugin_init: (unit -> unit) ->
  ?init: (unit -> unit) ->
  ?toplevel_init:(unit -> unit) ->
  ?shortname:string ->
  ?debug:(Arg.key * Arg.spec * Arg.doc) list ->
  (Arg.key * Arg.spec * Arg.doc) list ->
  unit
    (** [add_plugin ~name ~descr ~plugin_init ~init ~toplevel_init ~shortname
	~debug options] adds a new plugin.

	[name] is its name. [descr] is a short description used by the help.
	[plugin_init] is an action that is performed during plugin
	initialization, that is after global initialization, and before
	command-line parsing. [init] is an action that is performed right
	after the parsing of the command line argument. [toplevel_init] is an
	action performed after all plugins have been initialized.

	[debug] are debug options. These options are given on
	the command line as a unique string argument of -[shortname]-debug. If
	[shortname] is not specified, [name] is used instead. In the given
	string, a [;] is interpreted as a blank separator.

        A new section of name [name] is automatically added (see {!add_cmdline})
        into the command line description. *)

val has_plugin: string -> bool
  (** [has_plugin name] is [true] iff the plugin [name] has already been
      registered. *)

val usage: unit -> string
  (** Return an usage string, with the list of currently installed plug-ins.*)

val add_file: string -> unit
  (** Add a new file to parse. *)

val parse_cmdline: unit -> unit
  (** Parse the command line arguments and performs the configuration of the
      plugins accordingly. *)

val register_plugin_init: (unit -> unit) -> unit
  (** Register an action to perform at plugin initialization time. *)

val initialize_toplevels: unit -> unit
  (** Function called at startup, i.e. toplevels *)

val init_from_options: unit -> bool
  (** Should be called each time an user may set some options.
      @return true if files given on the cmdline have to be ignored *)
