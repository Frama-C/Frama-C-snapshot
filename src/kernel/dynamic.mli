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

(* $Id: dynamic.mli,v 1.9 2008/12/02 11:59:37 uid568 Exp $ *)

(** Dynamic plug-ins: registration and use. *)

val is_dynlink_available: bool

(** {2 Registration} *)

exception Invalid_Name of string 
  
val register: string -> 'a Type.t -> 'a -> unit
  (** [register name sign func] registers [func] with the name
      [name] and the type [sign].
      @raise Invalid_Name if the given name is not 
	"name_of_module.name_of_function"
      @raise FunTbl.AlreadyExists if the given name already exists. In
      other words you cannot register a function with the same name twice.
      @plugin development guide *)

(** {2 Access} *)
  
val apply: string -> 'a Type.t -> 'a
  (** [apply name sign] return a function registered with the [name]
      and the type [sign]. 
      @raise FunTbl.Not_Registered if the given name is not registered.
      @raise FunTbl.Incompatible_Type if the given type is not compatible with
      the use of the function 
      @plugin development guide *)

(** {2 Kernel materials} *)

val add_path: string -> unit
  (** @return add a path in search paths. *)
  
val include_module: string -> unit
  (** @return load module with the name of module. The module is searched in
      search paths. Do nothing if dynamic loading is not available*)

val is_loaded: string -> bool 
  (** @return if a module is already loaded or not.*)
  
(*
val main_error : (unit -> unit) -> unit -> unit
  (** Transform a function in another which catchs all error of apply
      function. Function useful if you have some call to apply in your main
      entry point.*)
*) 
(**/**)
(** Internal functions *)
val trace : unit -> unit
  (** Display all hooked functions by [display] according to debug
      command line level. *) 
  
(* The following functions do nothing if  [Dynlink] is not availbale 
   (Ocaml version lower than 3.11 and native dynlink). *)
val include_all_module : (unit -> unit) ref
  (** This function is equal to [include_all_no_gui_module] for toplevel and
      equal to [include_all_gui_module] for viewer. *)
val include_all_gui_module : unit -> unit
  (** Load all modules whether they finish by "_gui" or not *)
val include_all_no_gui_module : unit -> unit
  (** Load all no gui modules (which are not finished by "_gui") *)
  (**/**)

(** @deprecated Since Lithium-20081002+beta1+dev. Replaced by {!Db.Main}. *)
module Main : sig

  val extend : (unit -> unit) -> unit
    (** Register a function to be called by the Frama-C main entry point. 
	@deprecated Since Lithium-20081002+beta1+dev. Replaced by
	{!Db.Main}. *)

  (**/**)
  val apply: unit -> unit 
    (** Apply entry points previously registered . *)
  (**/**)

end

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.. -j"
  End:
*)
