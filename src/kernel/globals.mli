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

(* $Id: globals.mli,v 1.13 2008/04/10 15:48:06 uid562 Exp $ *)

(** Operations on globals. *)

open Cil_types
open Db_types

(** Globals variables. *)
module Vars: sig

  (** {2 Iterators} *)

  val iter: (varinfo -> initinfo -> unit) -> unit
  val fold: (varinfo -> initinfo -> 'a -> 'a) -> 'a -> 'a
  val find: varinfo -> initinfo

  (** {2 Setters} *)

  exception AlreadyExists of varinfo * initinfo
  val add: varinfo -> initinfo -> unit
    (** @raise AlreadyExists if the given varinfo is already registered. *)
  val add_decl: varinfo -> unit
    (** @raise AlreadyExists if the given varinfo is already registered. *)

end

(* ************************************************************************* *)
(** Functions. *)
module Functions: sig

  (** {2 Getters} *)

  val get: varinfo -> kernel_function
    (** @raise Not_found if the given varinfo has not a function type. *)

  val get_glob_init: ?main_name:string -> file -> kernel_function
    (** Returns the internal function for global initializations. *)

  (** {2 Searching} *)

  val find_by_name : string -> kernel_function
  (** @raise Not_found if there is no function of this name. *)

  val find_def_by_name : string -> kernel_function
    (** @raise Not_found if there is no function definition of this name. *)

  val find_englobing_kf: kinstr -> kernel_function option

  (** {2 Iterators} *)

  val iter: (kernel_function -> unit) -> unit
  val fold: (kernel_function -> 'a -> 'a) -> 'a -> 'a
  val iter_on_fundecs: (fundec -> unit) -> unit

  (** {2 Setters} *)

  val add: cil_function -> unit
  val replace_by_declaration: funspec -> varinfo -> location -> unit
  val replace_by_definition: funspec -> fundec -> location -> unit

end

(* ************************************************************************* *)
(** Globals annotations. *)
module Annotations: sig

  val self: Project.Computation.t
    (** The state kind corresponding to the table of global annotations. *)

  (** {2 Getters} *)
    val get_all: unit -> (global_annotation * bool) list

  (** {2 Iterators} *)

  val iter: (global_annotation -> bool -> unit) -> unit
    (** The boolean parameter of the given function is [true] iff the
	annotation was generated. *)

  (** {2 Setters} *)

  val add_user: global_annotation -> unit
  val add_generated: global_annotation -> unit
  val replace_all:
    (global_annotation -> bool -> global_annotation * bool) -> unit

end

(* ************************************************************************* *)
(** Globals associated to filename. *)
module FileIndex : sig

  val find : filename:string -> global list
    (** Global list for valviewer *)

  val get_globals : filename:string -> (varinfo * initinfo) list
    (** Global variables of the given module for the kernel user interface *)

  val get_functions : filename:string -> kernel_function list
    (** Global variables of the given module for the kernel user interface *)

  val kernel_function_of_local_var_or_param_varinfo :
    varinfo -> (kernel_function * bool)
    (** kernel_function where the local variable or formal parameter is
	declared. The boolean result is true for a formal parameter. *)

  val get_files: unit -> string list
    (** Get the files list containing globals. *)

end

(* ************************************************************************* *)
(** {2 Entry point} *)
(* ************************************************************************* *)

exception No_such_entry_point of string
  (** May be raised by [entry_point] above. *)

val entry_point : unit -> kernel_function * bool
  (** returns the current function entry point and a boolean indicating if it
      is a library entry point.
      @raise No_such_entry_point if the current entrypoint name does not
      exist. *)

val set_entry_point : string -> bool -> unit
  (** [set_entry_point name lib] sets [Cmdline.MainFunction] to [name] if [lib]
      is [false] and [Cmdline.LibEntry] to [name] if [lib] is [true].
      Moreover, clear the results of all the analysis which depend on
      [Cmdline.MainFunction] or [Cmdline.LibEntry]. *)

val has_entry_point: unit -> bool
  (** Returns true if the analysis has an entry-point, false otherwise. *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
