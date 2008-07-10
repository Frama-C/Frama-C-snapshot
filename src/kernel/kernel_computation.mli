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

(* $Id: kernel_computation.mli,v 1.8 2008/05/30 08:29:48 uid568 Exp $ *)

(** Functors for building computations which use kernel datatypes. 
    @plugin developer guide *)

open Computation

module StmtSetRef(Info:Signature.NAME_DPDS) : 
  SET_REF_OUTPUT with type elt = Cil_types.stmt

module IntHashtbl(Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = int and type data = Data.t

module InstrHashtbl(Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = Cil_types.kinstr and type data = Data.t

(** @plugin developer guide *)
module StmtHashtbl(Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = Cil_types.stmt and type data = Data.t

module VarinfoHashtbl(Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = Cil_types.varinfo and type data = Data.t

module CodeAnnotationHashtbl
  (Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = Cil_types.code_annotation 
		 and type data = Data.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
