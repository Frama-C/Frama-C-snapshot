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

(** Dynamic plug-ins: registration and use. 
    @plugin development guide *)

val default_path: unit -> string list

(* ************************************************************************* *)
(** {2 Registration} *)
(* ************************************************************************* *)

val register:
  ?comment:string -> 
  plugin:string -> 
  string -> 'a Type.t -> journalize:bool -> 'a -> 'a
  (** [register ~plugin name ty v] registers [v] with the name
      [name], the type [ty] and the plug-in [plugin].
      @raise Type.AlreadyExists if [name] already exists. In other words you
      cannot register a value with the same name twice.
      @modify Boron-20100401 add the labeled argument "plugin"
      @modify Oxygen-20120901 add the optional labeled argument "comment"
      @plugin development guide *)

(* ************************************************************************* *)
(** {2 Access} *)
(* ************************************************************************* *)

exception Incompatible_type of string
exception Unbound_value of string

exception Unloadable of string
(** Exception that a plug-in can throw if it detects that it
    can't be loaded. It is caught by {!Dynamic.load_module} and
   {!Dynamic.load_script}
    @since Oxygen-20120901 *)

val get: plugin:string -> string -> 'a Type.t -> 'a
  (** [get ~plugin name ty] returns the value registered with the name
      [name], the type [ty] and the plug-in [plugin]. This plug-in will be
      loaded if required.
      @raise Unbound_value if the name is not registered
      @raise Incompatible_type if the name is not registered
      with a compatible type
      @raise Failure _ in the -no-obj mode
      @plugin development guide *)

val iter: (string -> 'a Type.t -> 'a -> unit) -> unit
val iter_comment : (string -> string -> unit) -> unit
(** @since Oxygen-20120901 *)

val is_plugin_present: string -> bool
(** @return true iff the given plug-in is loaded and usable.
    @since Nitrogen-20111001 *)

(* ************************************************************************* *)
(** {2 Dedicated access to plug-in parameters} *)
(* ************************************************************************* *)

(** Module to use for accessing parameters of plug-ins.
    Assume that the plug-in is already loaded. 
    @plugin development guide *)
module Parameter : sig

  (** Set of common operations on parameters. *)
  module type Common = sig
    type t
    val get: string -> unit -> t
    val set: string -> t -> unit
    val clear: string -> unit -> unit
    val is_set: string -> unit -> bool
    val is_default: string -> unit -> bool
  end

  (** retrieve the representation of the corresponding parameter. *)
  val get_parameter: string -> Typed_parameter.t

  (** retrieve the state related to the corresponding parameter.
      @raise Not_found if the option does not correspond to an actual
      parameter
      @since Oxygen-20120901 *)
  val get_state: string -> State.t

  (**/**)
  val get_name: string -> string -> string -> string
  (** Not for casual users *)
  (**/**)

  (** Boolean parameters. 
      @plugin development guide *)
  module Bool: sig
    include Common with type t = bool
    val on: string -> unit -> unit
      (** Set the parameter to [true]. *)
    val off : string -> unit -> unit
      (** Set the parameter to [false]. *)
  end

  (** Integer parameters. *)
  module Int : sig
    include Common with type t = int
    val incr : string -> unit -> unit
  end

  (** String parameters. *)
  module String : Common with type t = string

  (** Set of string parameters. *)
  module StringSet : sig
    include Common with type t = Datatype.String.Set.t
    val add: string -> string  -> unit
    val remove: string -> string -> unit
    val is_empty: string -> unit -> bool
    val iter: string -> (string -> unit) -> unit
  end

  (** List of string parameters. *)
  module StringList : sig
    include Common with type t = string list
    val add: string -> string  -> unit
    val append_before: string -> string list -> unit
      (** @since Neon-20130301 *)
    val append_after: string -> string list -> unit
      (** @since Neon-20130301 *)
    val remove: string -> string -> unit
    val is_empty: string -> unit -> bool
    val iter: string -> (string -> unit) -> unit
  end

(*
    module IndexedVal(X: sig val ty_name: string end) : sig
      include Common with type t = string
      type value
      val add_choice: string -> string -> value -> unit
      val get_val: string -> value
    end
*)

end

(* ************************************************************************* *)
(** {2 Kernel materials} *)
(* ************************************************************************* *)

val object_file_extension: string
  (** Object file extension used when loading a module. See function
      {!load_module}.
      @since Boron-20100401 *)

val add_path: string -> bool
(** Add a path into the search paths, if it is not already in the list.
    @return true iff the path is really added to the list. *)

val load_module: string -> unit
  (** Load the module with the given name. The module is searched in
      search paths if the name is implicit (that is if the file name is relative
      and does not start with an explicit reference to the current directory (./
      or ../ in Unix). Do nothing if dynamic loading is not available.
      @modify Nitrogen-20111001 better strategy for searching modules *)

val load_script: string -> unit
  (** Compile then load the OCaml script with the given name. The file is
      searched in the current directory, next in search paths if the name is
      implicit (that is if the file name is relative and does not start with an
      explicit reference to the current directory (./ or ../ in Unix). Do
      nothing if dynamic loading is not available.
      @since Beryllium-20090601-beta1
      @modify Nitrogen-20111001 better strategy for searching modules *)

val set_default: bool -> unit
  (** Search in all the default directories iff the parameter is [true].
      @since Boron-20100401 *)

val add_dependencies: from:string -> string -> unit
(** [add_dependencies ~from p] indicates that the plugin [from] must be loaded
    before [p].
    @since Neon-20130301 *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
