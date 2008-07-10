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

(* $Id: visitor.mli,v 1.14 2008/05/30 08:29:49 uid568 Exp $ *)

open Cil_types
open Db_types

(** Frama-C visitors dealing with projects. 
    @plugin developer guide *)

(** Class type for a Db-aware visitor.
    This is done by defining auxiliary methods that can be
    redefined in inherited classes, while the corresponding ones from
    {!Cil.cilVisitor} {b must} retain their values as defined here. Otherwise,
    annotations may not be visited properly. The replaced functions are
    - {t vfile} (use {t vfile_aux} instead)
    - {t vstmt} (use {t vstmt_aux} instead)
    - {t vglob} (use {t vglob_aux} instead)

    {b A few hints on how to use correctly this visitor}

    - when initializing a new project with it
    (see {!File.init_project_from_visitor}), use a visitor with copy behavior

    - [SkipChildren] and [ChangeTo] must be used with extreme care in a visitor
    with copy behavior, or some nodes may be shared between the original and
    the copy.

    - Do not erase a statement during the visit, as there might be
    annotations attached to it. Change it to Skip instead, the
    [generic_frama_c_visitor] will know what to do.

    - Be careful if you change the [vid] or [sid]: this must be done before
    anything has been attached to the corresponding variable or
    statement in the new project, which means
       - for statements, in [vstmt], for the current statement only
       - for variables, at their declaration point. *)
class type frama_c_visitor = object

  inherit Cil.cilVisitor

  method vstmt_aux: stmt -> stmt Cil.visitAction
    (** Replacement of vstmt. 
	@plugin developer guide*)

  method vglob_aux: global -> global list Cil.visitAction
    (** Replacement of vglob. 
	@plugin developer guide*)

  method vrooted_code_annotation:
    rooted_code_annotation ->
    rooted_code_annotation list Cil.visitAction
      (** visiting a rooted code annotation. *)

  method is_annot_before: bool
    (** Used to tell if we're visiting an annotation placed
	before current statement.
	@raise Error if not called while visiting a statement. *)

end

class generic_frama_c_visitor:
  Project.t -> Cil.visitor_behavior ->  frama_c_visitor
  (** Generic visitor. The tables are filled on the project given in argument.
      See also {!File.init_project_from_visitor}. 
      @plugin developer guide *)

class frama_c_copy: Project.t -> frama_c_visitor
  (** Copying visitor *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
