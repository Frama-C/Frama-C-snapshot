(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: annotations.ml,v 1.43 2008-12-17 15:37:56 uid530 Exp $ *)

open Cil_types
open Db_types
open Cil

module AnnotState =
  Cil_computation.StmtHashtbl
    (Datatype.Ref(Datatype.List(Ast_info.Datatype_Annotation)))
    (struct
       let name = "Annotations"
       let size = 17
       let dependencies = [ Ast.self ]
     end)

let add stmt a =
  try
    let l = AnnotState.find stmt in
    l := a :: !l;
  with Not_found -> 
    AnnotState.add stmt (ref [ a ])

let add_assert stmt ~before a =
  let a = 
    User (Logic_const.new_code_annotation (AAssert ([],a,{status=Unknown}))) 
  in
  add stmt (if before then Before a else After a)

let add_alarm stmt ~before alarm a =
  let a = 
    AI (alarm,
	Logic_const.new_code_annotation
	  (AAssert ([], a, { status = Unknown }))) 
  in
  add stmt (if before then Before a else After a)

let reset_stmt = AnnotState.remove

let replace stmt a = AnnotState.replace stmt (ref [ a ])

let get stmt = try List.rev !(AnnotState.find stmt) with Not_found -> []

let get_filter f stmt =
  List.filter
    (function
         Before (User ca) | After (User ca)
       | Before (AI (_,ca)) | After(AI(_,ca)) -> f ca)
    (get stmt)

let iter = AnnotState.iter

let filter f = iter (fun stmt l -> l := List.filter (f stmt) !l)

let self = AnnotState.self

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
