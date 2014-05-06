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

(** Information about version of Frama-C.

    The body of this module is generated from Makefile.
    @plugin development guide *)

val version: string
  (** Frama-C Version identifier. *)

val date: string
  (** Compilation date. *)

val is_gui: bool ref
  (** Is the Frama-C GUI running?
      @since Beryllium-20090601-beta1 *)

val ocamlc: string
  (** Name of the bytecode compiler.
      @since Boron-20100401 *)

val ocamlopt: string
  (** Name of the native compiler.
      @since Boron-20100401 *)

val datadir: string
  (** Directory where architecture independent files are. *)

val libdir: string
  (** Directory where the Frama-C kernel library is.
      @since Beryllium-20090601-beta1 *)

val plugin_dir: string
  (** Directory where the Frama-C dynamic plug-ins are.
      @since Beryllium-20090601-beta1 *)

val static_plugins: string list
  (** Plug-ins statically linked within Frama-C. *)

val static_gui_plugins: string list
  (** GUI of plug-ins statically linked within Frama-C. *)

val compilation_unit_names: string list
  (** List of names of all kernel compilation units.
      @since Boron-20100401 *)

val preprocessor: string
  (** Name of the default command to call the preprocessor.
      If the CPP environment variable is set, use it
      else use the built-in default from autoconf. Usually this is
      "gcc -C -E -I."
      @since Oxygen-20120901 *)

val preprocessor_keep_comments: bool
  (** [true] if the default preprocessor selected during compilation is
      able to keep comments (hence ACSL annotations) in its output.
      @since Neon-rc3 
   *)
  
val dot: string option
(** Dot command name.
    @return [None] if `dot' is not installed.
    @since Carbon-20101201 *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
