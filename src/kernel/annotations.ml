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

open Cil_types
open Db_types
open Cil

module AnnotState =
  Computation.Dashtbl
    (Cilutil.StmtComparable)
    (Ast_info.Datatype_Annotation)
    (struct
       let name = "Annotations"
       let size = 17
       let dependencies = [ Ast.self ]
     end)

let add = AnnotState.add

let add_assert stmt states ~before a =
  let a =
    User (Logic_const.new_code_annotation (AAssert ([],a,{status=Unknown})))
  in
  let v = if before then Before a else After a in
  if not (List.for_all
	    (fun s -> List.memq v (AnnotState.find_all_local_data stmt s))
	    states)
  then add stmt states v

let add_alarm stmt states ~before alarm a =
  let a =
    AI (alarm,
	Logic_const.new_code_annotation
	  (AAssert ([], a, { status = Unknown })))
  in
  add stmt states (if before then Before a else After a)

let reset_stmt = AnnotState.remove_all
let replace = AnnotState.replace

let get = AnnotState.find_all_local
let get_annotations = AnnotState.find_all_local_data
let get_state = AnnotState.find_all_local_state

let get_all = AnnotState.find_all
let get_all_annotations = AnnotState.find_all_data
let get_all_states = AnnotState.find_all_states

let get_by_state stmt =
  List.map (fun s -> s, get_annotations stmt s) (get_all_states stmt)

let get_filter f stmt =
  List.filter
    (function
     | Before (User ca) | After (User ca)
     | Before (AI (_,ca)) | After(AI(_,ca)) -> f ca)
    (get_all_annotations stmt)

let iter = AnnotState.iter
let iter_stmt = AnnotState.iter_key
let single_iter_stmt f s = List.iter f (get_all_annotations s)
let fold = AnnotState.fold
let fold_stmt = AnnotState.fold_key
let single_fold_stmt f s acc =
  List.fold_left (fun acc a -> f a acc) acc (get_all_annotations s)

let filter = AnnotState.filter

let self = AnnotState.self

let add_dependency stmt state =
  List.iter
    (fun s -> AnnotState.add_dependency s state)
    (get_all_states stmt)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
