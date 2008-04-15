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

(* $Id: cil_state.mli,v 1.15 2008/12/10 12:53:14 uid568 Exp $ *)

(** Access to the Cil AST dealing with projects. 
    @plugin development guide *)

module UntypedFiles: 
  Computation.OPTION_REF_OUTPUT with type data = Cabs.file list
  (** The list of untyped AST that have been parsed. *)

exception Bad_Initialisation of string
  (** May be raised by {!file} above. *)

val file: unit -> Cil_types.file
  (** Get the cil file representation.
      {!File.init_from_c_files}, {!File.init_project_from_cil_file} or
      {!File.init_from_cmdline} has to be called before using this function.
      @raise Bad_Initialization if neither {!File.init_from_c_files}
      nor {!File.init_project_from_cil_file} nor {!File.init_from_cmdline} was
      called before. 
      @plugin development guide *)

val compute: unit -> unit
  (** Enforce the computation of the AST
  *)

val is_computed: unit -> bool
  (** @return true if the AST has been computed. *)

val self: Project.Computation.t
  (** The state kind associated to the cil AST. 
      @plugin development guide *)

val depend: Project.Computation.t -> unit
  (** Add a dependency toward [self] *)

val set_file: Cil_types.file -> unit
  (** Should not be used by casual users. *)

val set_default_initialization: (unit -> unit) -> unit
  
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
