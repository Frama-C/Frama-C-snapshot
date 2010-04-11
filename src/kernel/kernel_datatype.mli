(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Datatypes of some useful kernel types.
    @plugin development guide *)

open Db_types

module Rooted_Code_Annotation : 
  Project.Datatype.S with type t = rooted_code_annotation

module Before_After(A:Project.Datatype.S) : 
  Project.Datatype.S with type t = A.t before_after

module Rooted_Code_Annotation_Before_After : 
  Project.Datatype.S with type t = rooted_code_annotation before_after

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
