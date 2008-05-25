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

(* $Id: kernel_datatype.ml,v 1.14 2008/04/07 17:40:58 uid528 Exp $ *)

open Cil_types

module IntHashtbl = Datatype.Make_Hashtbl(Inthash)
module InstrHashtbl = Datatype.Make_Hashtbl(Cil.InstrHashtbl)
module StmtHashtbl = Datatype.Make_Hashtbl(Cilutil.StmtHashtbl)
module VarinfoHashtbl = Datatype.Make_Hashtbl(Cilutil.VarinfoHashtbl)

module Stmt = struct
  include Project.Datatype.Imperative
    (struct type t = stmt let copy _ = assert false end)
  let compare = Cilutil.StmtComparable.compare
end

module StmtSet = Datatype.Make_Set(Cilutil.StmtSet)(Stmt)

module StmtSetRef = Datatype.Make_SetRef(Cilutil.StmtSet)(Stmt)

module Varinfo =
  Project.Datatype.Imperative
    (struct type t = varinfo let copy _ = assert false (* TODO *) end)

module Location =
  Project.Datatype.Imperative
    (struct
       type t = location
       let copy _ = assert false (* TODO *)
     end)

module File = Project.Datatype.Imperative(struct type t = file let copy _ = assert false end)

module InitInfo =
  Project.Datatype.Imperative
    (struct
       type t = initinfo
       let copy _ = assert false (* TODO: deep copy *)
     end)

open Db_types

module KernelFunction =
  Project.Datatype.Register
    (struct
       type t = kernel_function
       let rehash x =
         match x.fundec with
         | Definition _ | Declaration (_,_,None,_)-> x
         | Declaration (_,v,Some args,_) ->
             Cil.unsafeSetFormalsDecl v.vid args;
	     x
       let copy _ = assert false (* TODO: deep copy *)
       include Datatype.Nop
       let name = Project.Datatype.Name.make "Kernel_function"
       let dependencies = [ File.self ]
     end)

module KF_Queue = Datatype.Queue(KernelFunction)

(** {3 Annotations} *)

module CodeAnnotation =
  Project.Datatype.Imperative
    (struct
       type t = code_annotation
       let copy _ = assert false (* TODO *)
     end)

module RootedCodeAnnotation =
  Project.Datatype.Imperative
    (struct
       type t = rooted_code_annotation
       let copy _ = assert false (* TODO *)
     end)

module Annotation =
  Project.Datatype.Imperative
    (struct
       type t = rooted_code_annotation before_after
       let copy _ = assert false (* TODO *)
     end)

module Lval =
  Project.Datatype.Imperative
    (struct type t = lval let copy _ = assert false (* todo *) end)

module Kinstr =
  Project.Datatype.Imperative
    (struct type t = kinstr let copy _ = assert false (* todo *) end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
