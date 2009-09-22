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

(* $Id: dynamic.mli,v 1.13 2009-02-06 08:33:56 uid568 Exp $ *)

(** Dynamic plug-ins: registration and use. *)

val default_path: unit -> string list

(** {2 Registration} *)

exception Invalid_Name of string 

val register: string -> 'a Type.t -> journalize:bool -> 'a -> 'a
  (** [register name ty v] registers [v] with the name
      [name] and the type [ty].
      @raise Invalid_Name if the given name is not 
      "name_of_module.name_of_function"
      @raise Type.AlreadyExists if [name] already exists. In other words you
      cannot register a value with the same name twice. 
      @plugin development guide *)

(** {2 Access} *)
  
val get: string -> 'a Type.t -> 'a
  (** [apply name ty] return the value registered with the [name]
      and the type [ty]. 
      @raise Type.Unbound_value if [name] was not previously registered.
      @raise Type.StringTbl.Incompatible_Type if [ty] is not compatible with
      the use of the function
      @plugin development guide *)

val apply: string -> 'a Type.t -> 'a
  (** @deprecated Since Beryllium-20090601-beta1. Replaced by {!get}. *)

(** {2 Kernel materials} *)

val add_path: string -> unit
  (** Add a path into the search paths. *)
  
val load_module: string -> unit
  (** Load the module with the given name. The module is searched in
      search paths. Do nothing if dynamic loading is not available*)

val load_script: string -> unit
  (** Compile then load the ocaml script with the given name. The file is
      searched in search paths. Do nothing if dynamic loading is not
      available.
      @since Beryllium-20090601-beta1 *)

module Main : sig

  val extend : (unit -> unit) -> unit
    (** Register a function to be called by the Frama-C main entry point. 
	@deprecated Since Lithium-20081002+beta1+dev. Replaced by
	{!Db.Main.extend}. 
	@deprecated Since Beryllium-20090601-beta1. Replaced by {!Db.Main}. *)

  val apply: unit -> unit 
    (** Apply entry points previously registered . *)

end

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.. -j"
  End:
*)
