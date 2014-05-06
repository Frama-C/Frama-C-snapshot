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

(** Provided plug-general services for plug-ins.
    @since Beryllium-20090601-beta1
    @plugin development guide *)

module type S = sig

  include Log.Messages

  val add_group: ?memo:bool -> string -> Cmdline.Group.t
  (** Create a new group inside the plug-in.
      The given string must be different of all the other group names of this
      plug-in if [memo] is [false].
      If [memo] is [true] the function will either create a fresh group or
      return an existing group of the same name in the same plugin.
      [memo] defaults to [false]
      @since Beryllium-20090901 *)

  module Help: Parameter_sig.Bool
  (** @deprecated since Oxygen-20120901 *)

  module Verbose: Parameter_sig.Int
  module Debug: Parameter_sig.Int
  module Debug_category: Parameter_sig.String_set
   (** prints debug messages having the corresponding key.
       @since Oxygen-20120901
       @modify Fluorine-20130401 Set instead of list
    *)

  (** Handle the specific `share' directory of the plug-in.
      @since Oxygen-20120901 *)
  module Share: Parameter_sig.Specific_dir

  (** Handle the specific `session' directory of the plug-in. 
      @since Neon-20130301 *)
  module Session: Parameter_sig.Specific_dir

  (** Handle the specific `config' directory of the plug-in. 
      @since Neon-20130301 *)
  module Config: Parameter_sig.Specific_dir

  val help: Cmdline.Group.t
    (** The group containing option -*-help.
        @since Boron-20100401 *)

  val messages: Cmdline.Group.t
    (** The group containing options -*-debug and -*-verbose.
        @since Boron-20100401 *)

end

type plugin = private
    { p_name: string;
      p_shortname: string;
      p_help: string;
      p_parameters: (string, Typed_parameter.t list) Hashtbl.t }
(** Only iterable parameters (see {!do_iterate} and {!do_not_iterate}) are
    registered in the field [p_parameters].
    @since Beryllium-20090901 *)

module type General_services = sig
  include S
  include Parameter_sig.Builder
end

(**/**)

val register_kernel: unit -> unit
(** Begin to register parameters of the kernel. Not for casual users.
      @since Beryllium-20090601-beta1 *)

(**/**)

(** Functors for registering a new plug-in. It provides access to several
    services.
    @plugin development guide *)
module Register
  (P: sig
     val name: string (** Name of the module. Arbitrary non-empty string. *)
     val shortname: string (** Prefix for plugin options. No space allowed. *)
     val help: string (** description of the module. Free-form text. *)
   end) :
  General_services

val is_share_visible: unit -> unit
(** Make visible to the end-user the -<plug-in>-share option.
    To be called just before applying {!Register} to create plug-in services.
    @since Oxygen-20120901 *)

val is_session_visible: unit -> unit
(** Make visible to the end-user the -<plug-in>-session option.
    To be called just before applying {!Register} to create plug-in services.
    @since Neon-20130301 *)

val is_config_visible: unit -> unit
(** Make visible to the end-user the -<plug-in>-config option.
    To be called just before applying {!Register} to create plug-in services.
    @since Neon-20130301 *)

val plugin_subpath: string -> unit
(** Use the given string as the sub-directory in which the plugin files will
    be installed (ie. [share/frama-c/plugin_subpath]...). Relevant for
    directories [Share], [Session] and [Config] above.
    @since Neon-20130301 *)

(* ************************************************************************* *)
(** {2 Handling plugins} *)
(* ************************************************************************* *)

val get_from_shortname: string -> plugin
(** Get a plug-in from its shortname.
    @since Oxygen-20120901  *)

val get_from_name: string -> plugin
(** Get a plug-in from its name.
    @since Oxygen-20120901 *)

val get: string -> plugin
(** Get a plug-in from its name.
    @deprecated since Oxygen-20120901 *)

val iter_on_plugins: (plugin -> unit) -> unit
  (** Iterate on each registered plug-ins.
      @since Beryllium-20090901 *)

(**/**)
(* ************************************************************************* *)
(** {2 Internal kernel stuff} *)
(* ************************************************************************* *)

val positive_debug_ref: int ref
  (** @since Boron-20100401 *)

val session_is_set_ref: (unit -> bool) ref
val session_ref: (unit -> string) ref

val config_is_set_ref: (unit -> bool) ref
val config_ref: (unit -> string) ref

(**/**)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
