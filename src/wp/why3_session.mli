(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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


(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2013   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(** From the original file we kept only the reading of a session.
    We also discard all the information about how that have been proved
    (metas, transformation, proof_attempts) or the order of the goals
*)

(** {2 Proof attempts} *)

type goal = private
  {
    goal_name : string;
    goal_parent : theory;
    mutable goal_verified : bool;
  }


and theory = private
  {
    theory_name : string;
    theory_parent : file;
    theory_goals : goal Datatype.String.Hashtbl.t;
    mutable theory_verified : bool;
    }

and file = private
    {
      file_name : string;
      file_format : string option;
      file_parent : session;
      file_theories: theory Datatype.String.Hashtbl.t;
      (** Not mutated after the creation *)
      mutable file_verified : bool;
    }

and session = private
    { session_files : file Datatype.String.Hashtbl.t;
      session_dir   : string;
    }

(** {2 Read/Write} *)
exception LoadError

val read_session : string -> session
(** Read a session stored on the disk. It returns a session without any
    task attached to goals *)
