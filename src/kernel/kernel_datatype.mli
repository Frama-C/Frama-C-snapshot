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

(* $Id: kernel_datatype.mli,v 1.14 2008/05/30 08:29:48 uid568 Exp $ *)

open Cil_types
open Db_types

(** Datatypes of some useful kernel types. 
    @plugin developer guide *)

(** @plugin developer guide *)
module Varinfo : Project.Datatype.OUTPUT with type t = varinfo
module Location : Project.Datatype.OUTPUT with type t = location

(** @plugin developer guide *)
module Stmt: sig
  include Project.Datatype.OUTPUT with type t = stmt
  val compare: t -> t -> int
end

module Kinstr: Project.Datatype.OUTPUT with type t = kinstr
module Lval: Project.Datatype.OUTPUT with type t = lval

(** Datatype for a cil file. *)
module File: Project.Datatype.OUTPUT with type t = file
module InitInfo: Project.Datatype.OUTPUT with type t = initinfo

(** Datatype for a kernel function. 
    @plugin developer guide *)
module KernelFunction: Project.Datatype.OUTPUT with type t = kernel_function

(** Datatype for a queue of kernel functions. *)
module KF_Queue: Project.Datatype.OUTPUT with type t = kernel_function Queue.t

(** {3 Hashtables for Cil types} *)

module IntHashtbl(Data:Datatype.INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t Inthash.t

module InstrHashtbl(Data:Datatype.INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t Cil.InstrHashtbl.t

module StmtHashtbl(Data:Datatype.INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t Cilutil.StmtHashtbl.t

(** @plugin developer guide *)
module VarinfoHashtbl(Data:Datatype.INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t Cilutil.VarinfoHashtbl.t

(** {3 Sets} *)

(** Datatype for a set of statements. *)
module StmtSet: Project.Datatype.OUTPUT with type t = Cilutil.StmtSet.t

(** Datatype for a reference to a set of statements. *)
module StmtSetRef: Project.Datatype.OUTPUT with type t = Cilutil.StmtSet.t ref

(** {3 Annotations} *)

module CodeAnnotation: Project.Datatype.OUTPUT with type t = code_annotation

module RootedCodeAnnotation:
  Project.Datatype.OUTPUT with type t = rooted_code_annotation

module Annotation:
  Project.Datatype.OUTPUT with type t = rooted_code_annotation before_after

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
