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

(** Functors for building computations which use kernel datatypes.
    @plugin development guide *)

module Stmt_set_ref(Info: State_builder.Info) :
  State_builder.Set_ref with type elt = Cil_types.stmt

module Kinstr_hashtbl(Data:Datatype.S)(Info: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.kinstr and type data = Data.t

(** @plugin development guide *)
module Stmt_hashtbl(Data:Datatype.S)(Info: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.stmt and type data = Data.t

module Varinfo_hashtbl(Data:Datatype.S)(Info: State_builder.Info_with_size) :
  State_builder.Hashtbl with type key = Cil_types.varinfo
			and type data = Data.t
(*
module Code_annotation_hashtbl
  (Data:Project.Datatype.S)(Info:State_builder.Info_with_size) :
  State_builder.Hashtbl
  with type key = Cil_types.code_annotation and type data = Data.t
 *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
