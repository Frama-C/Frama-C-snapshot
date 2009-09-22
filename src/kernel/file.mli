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

(* $Id: file.mli,v 1.18 2009-02-05 12:35:06 uid568 Exp $ *)

(** Frama-c preprocessing and Cil AST initialization.
    @plugin development guide *)

type t =
  | NeedCPP of string * string
      (** The first string is the filename of the [.c] to preprocess.
	  The second one is the preprocessor command ([filename.c -o
	  tempfilname.i] will be appended at the end).*)
  | NoCPP of string
      (** Already pre-processed file [.i] *)
  | External of string * (string -> Cil_types.file * Cabs.file)
      (** file that can be translated into a Cil AST through an external
          function. *)

val external_func_type: (string -> Cil_types.file * Cabs.file) Type.t

val new_file_type:
  string -> (string -> Cil_types.file * Cabs.file) -> unit
(** [new_file_type suffix func funcname] registers a new type of files (with
    corresponding suffix) as recognized by Frama-C through [func]. [func] must
    be registered in {!Type.Binding}. *)

val ty: t Type.t

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

val prepare_from_c_files: unit -> unit
  (** Initialize the AST of the current project according to the current
      filename list.
      @raise File_types.Bad_Initialization if called more than once. *)

val init_from_c_files: t list -> unit
  (** Initialize the cil file representation of the current project.
      Should be called at most once per project.
      @raise File_types.Bad_Initialization if called more than once.
      @plugin development guide *)

val init_project_from_cil_file: Project.t -> Cil_types.file -> unit
  (** Initialize the cil file representation with the given file for the
      given project from the current one.
      Should be called at most once per project.
      @raise File_types.Bad_Initialization if called more than once.
      @plugin development guide *)

val create_project_from_visitor:
  string -> (Project.t -> Visitor.frama_c_visitor) -> Project.t
  (** Return a new project with a new cil file representation by visiting the
      file of the current project.
      The visitor is responsible to avoid sharing between old file and new
      file (i.e. it should use {!Cil.copy_visit} at some point.
      @raise File_types.Bad_Initialization if called more than once.
      @since Beryllium-20090601-beta1
      @plugin development guide *)

val init_from_cmdline: unit -> unit
  (** Initialize the cil file representation with the file given on the
      command line.
      Should be called at most once per project.
      @raise File_types.Bad_Initialization if called more than once.
      @plugin development guide *)

(* ************************************************************************* *)
(** {2 Pretty printing} *)
(* ************************************************************************* *)

val pretty : ?prj:Project.t -> ?fmt:Format.formatter -> unit -> unit
  (** Print the project CIL file on the given Formatter.
      The default project is the current one.
      The default formatter is [Parameters.CodeOutput.get_fmt ()].
      @raise File_types.Bad_Initialization if the file is no initialized. *)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
