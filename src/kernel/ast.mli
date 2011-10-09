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

(** Access to the CIL AST which must be used from Frama-C.
    @plugin development guide *)

exception Bad_Initialization of string
  (** May be raised by function {!get} below. *)

exception NoUntypedAst
  (** Might be raised by {!UntypedFiles.get} below 
      @since Nitrogen-20111001
   *)

module UntypedFiles: sig

  val get: unit -> Cabs.file list
    (** The list of untyped AST that have been parsed.
        @raise Bad_Initialization if neither {!File.init_from_c_files}
        nor {!File.init_project_from_cil_file} nor {!File.init_from_cmdline}
        was called before. 
        @raise NoUntypedAst if no untyped AST is available. This is in 
        particular the case for projects obtained by code transformation from
        original C files.
        @modify Nitrogen-20111001 raise NoUntypedAst
     *)

  val set: Cabs.file list -> unit
    (** Should not be used by casual users. *)

  val self: State.t

end

val get: unit -> Cil_types.file
  (** Get the cil file representation.
      One of the initialisation function of module {!File} has to be called
      before using this function.
      @raise Bad_Initialization if neither {!File.init_from_c_files}
      nor {!File.init_project_from_cil_file} nor {!File.init_from_cmdline} was
      called before.
      @plugin development guide *)

val compute: unit -> unit
  (** Enforce the computation of the AST.
      @raise Bad_Initialization if neither {!File.init_from_c_files}
      nor {!File.init_project_from_cil_file} nor {!File.init_from_cmdline} was
      called before. *)

val is_computed: unit -> bool
  (** @return true if the AST has been computed. *)

val self: State.t
  (** The state kind associated to the cil AST.
      @plugin development guide *)

(*****************************************************************************)
(** {2 Internals}

    Functions below should not be called by casual users. *)
(*****************************************************************************)

val set_file: Cil_types.file -> unit
val set_default_initialization: (unit -> unit) -> unit
val mark_as_computed: unit -> unit
  (** @since Beryllium-20090901 *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
