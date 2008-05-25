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

(* $Id: file.mli,v 1.11 2008/04/01 09:25:21 uid568 Exp $ *)

(** Frama-c preprocessing and Cil AST initialization. *)

type t =
  | NeedCPP of string * string 
      (** The first string is the filename of the [.c] to preprocess.
	  The second one is the preprocessor command ([filename.c -o
	  tempfilname.i] will be appended at the end).*) 
  | NoCPP of string 
      (** Filename of a preprocessed [.c] *)
  | CPLUSPLUS of string 
      (** C++ file. Can only be analysed if C++ extension is loaded. *)

val name: t -> string
  (** File name. *)

val get_preprocessor_command: unit -> string
  (** Return the preprocessor command to use. *)

val pre_register: t -> unit
  (** Register some file as source file before command-line files *)

val get_all: unit -> t list
  (** Return the list of toplevel files. *)

val from_filename: ?cpp:string -> string -> t
  (** Build a file from its name. The optional argument is the preprocessor
      command. Default is [!get_preprocessor_command ()]. *)

(* ************************************************************************* *)
(** {2 Initializers} *)
(* ************************************************************************* *)

class check_file: Visitor.frama_c_visitor

val init_from_c_files: t list -> unit
  (** Initialize the cil file representation of the current project.
      Should be called at most once per project.
      @raise File_types.Bad_Initialization if called more than once. *)

val init_project_from_cil_file: Project.t -> Cil_types.file -> unit
  (** Initialize the cil file representation with the given file for the
      given project from the current one.
      Should be called at most once per project.
      @raise File_types.Bad_Initialization if called more than once. *)

val init_project_from_visitor:
  Project.t -> (Project.t -> Visitor.frama_c_visitor) -> unit
  (** Initialize the cil file representation by visiting the current file.
      The visitor is responsible to avoid sharing between old file and new
      file (i.e. it should use {!Cil.copy_visit} at some point.
      @raise File_types.Bad_Initialization if called more than once. *)

val init_from_cmdline: unit -> unit
  (** Initialize the cil file representation with the file given on the
      command line.
      Should be called at most once per project.
      @raise File_types.Bad_Initialization if called more than once. *)

(* ************************************************************************* *)
(** {2 Pretty printing} *)
(* ************************************************************************* *)

val pretty : ?prj:Project.t -> Format.formatter -> unit
  (** Print the project CIL file on the given formatter. The default project
      is the current one.
      @raise File_types.Bad_Initialization if the file is no initialized. *)

(* ************************************************************************* *)
(** {2 Internal delayed functions} 

    Should not be used by casual users. *)
(* ************************************************************************* *)

val cxx_suffixes: string list ref
val parse_cplusplus: (string -> Cil_types.file) ref

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
