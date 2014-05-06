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

val mark_as_changed: unit -> unit
  (** call this function whenever you've made some changes in place
      inside the AST
      @since Oxygen-20120901
      @plugin development guide
  *)

val mark_as_grown: unit -> unit
  (** call this function whenever you have added something to the AST,
      without modifying the existing nodes
      @since Oxygen-20120901
      @plugin development guide
  *)

val add_monotonic_state: State.t -> unit
(** indicates that the given state (which must depend on Ast.self) is robust
    against additions to the AST, that is, it will be able to compute
    information on the new nodes whenever needed. {!Ast.mark_as_grown} will
    not erase such states, while {!Ast.mark_as_changed} and clearing Ast.self
    itself will.
    @since Oxygen-20120901
    @plugin development guide
 *)

val self: State.t
  (** The state kind associated to the cil AST.
      @plugin development guide *)

val apply_after_computed: (Cil_types.file -> unit) -> unit
(** Apply the given hook just after building the AST. 
    @since Oxygen-20120901 *)


(*****************************************************************************)
(** {2 Internals}

    Functions below should not be called by casual users. *)
(*****************************************************************************)

val is_last_decl: Cil_types.global -> bool
  (** [true] if the global is the last one in the AST to introduce a given
      variable. Used by visitor and printer to relate funspec with appropriate
      global.
      @since Oxygen-20120901
   *)

val clear_last_decl : unit -> unit
  (** reset the mapping between a varinfo and the last global introducing it.
      @since Oxygen-20120901
   *)

val set_file: Cil_types.file -> unit
val set_default_initialization: (unit -> unit) -> unit
val mark_as_computed: unit -> unit
  (** @since Beryllium-20090901 *)

val add_hook_on_update: (unit -> unit) -> unit
(** Apply the given hook each time the reference to the AST is updated,
    including on a project switch. 
    @since Fluorine-20130401 *)


(**/**)
val add_linked_state: State.t -> unit



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
