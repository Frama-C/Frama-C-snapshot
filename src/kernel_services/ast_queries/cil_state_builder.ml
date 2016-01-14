(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

open State_builder
open Cil_datatype

module Stmt_set_ref = Set_ref(Stmt.Set)
module Kinstr_hashtbl = Hashtbl(Kinstr.Hashtbl)
module Stmt_hashtbl = Hashtbl(Stmt.Hashtbl)
module Varinfo_hashtbl = Hashtbl(Varinfo.Hashtbl)
(*
module Code_annotation_hashtbl =
  State_builder.Hashtbl(Cil_datatype.Code_Annotation)
 *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
