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

(** Dynamic plug-ins: registration and use. *)

val default_path: unit -> string list

(** {2 Registration} *)

val register:
  plugin:string -> string -> 'a Type.t -> journalize:bool -> 'a -> 'a
  (** [register ~plugin name ty v] registers [v] with the name
      [name], the type [ty] and the plug-in [plugin].
      @raise Type.AlreadyExists if [name] already exists. In other words you
      cannot register a value with the same name twice.
      @modify Boron-20100401 add the labeled argument "plugin"
      @plugin development guide *)

(** {2 Access} *)

exception Incompatible_type of string
exception Unbound_value of string

val get: plugin:string -> string -> 'a Type.t -> 'a
  (** [apply ~plugin name ty] returns the value registered with the name
      [name], the type [ty] and the plug-in [plugin]. This plug-in will be
      loaded if required.
      @raise Unbound_value if the name is not registered
      @raise Incompatible_type if the name is not registered
      with a compatible type
      @plugin development guide *)

(** {2 Kernel materials} *)

val object_file_extension: string
  (** Object file extension used when loading a module. See function
      {!load_module}.
      @since Boron-20100401 *)

val add_path: string -> unit
  (** Add a path into the search paths. *)

val load_module: string -> unit
  (** Load the module with the given name. The module is searched in
      search paths. Do nothing if dynamic loading is not available. *)

val load_script: string -> unit
  (** Compile then load the OCaml script with the given name. The file is
      searched in search paths. Do nothing if dynamic loading is not
      available.
      @since Beryllium-20090601-beta1 *)

val set_default: bool -> unit
  (** Search in all the default directories iff the parameter is [true].
      @since Boron-20100401 *)

module Main : sig

  val extend : (unit -> unit) -> unit
    (** Register a function to be called by the Frama-C main entry point.
	@deprecated since Lithium-20081201. Replaced by	{!Db.Main.extend}.
	@deprecated Since Beryllium-20090601-beta1. Replaced by {!Db.Main}. *)

  val apply: unit -> unit
    (** Apply entry points previously registered . *)

end

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
