(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Extlib
open Cil_types
open Db_types
open Cil

let get_code_annotation = function
  | Before (User ca) | After (User ca)
  | Before (AI (_,ca)) | After(AI(_,ca)) -> ca

module AnnotState =
  State_builder.Dashtbl
    (Dashtbl.Default_key_marshaler(Cil_datatype.Stmt))
    (Dashtbl.Default_data_marshaler
       (Kernel_datatype.Rooted_code_annotation_before_after))
    (struct
       let name = "Annotations"
       let size = 17
       let dependencies = [ Ast.self ]
       let kind = `Internal
       let internal_kind = `Correctness
     end)

let get_name a =
  let old = Parameters.UseUnicode.get () in
  Parameters.UseUnicode.set false;
  let s =
    Pretty_utils.sfprintf
      "%a" !Ast_printer.d_rooted_code_annotation_before_after a
  in
  Parameters.UseUnicode.set old;
  s

let add stmt states a = AnnotState.add (get_name a) stmt states a

let add_assert stmt states ~before a =
  let a = User (Logic_const.new_code_annotation (AAssert ([],a))) in
  add stmt states (if before then Before a else After a)

let add_alarm stmt states ~before alarm a =
  let a = AI (alarm, Logic_const.new_code_annotation (AAssert ([], a))) in
  add stmt states (if before then Before a else After a)

let reset_stmt = AnnotState.remove_all
let replace ~reset stmt states a =
  AnnotState.replace (get_name a) ~reset stmt states a

let get = AnnotState.find_all_local
let get_annotations = AnnotState.find_all_local_data
let get_state = AnnotState.find_all_local_states

let get_all = AnnotState.find_all
let get_all_annotations = AnnotState.find_all_data
let get_all_states = AnnotState.find_all_states

let get_by_state stmt =
  List.map (fun s -> s, get_annotations stmt s) (get_all_states stmt)

let get_filter f stmt =
  List.filter (f $ get_code_annotation) (get_all_annotations stmt)

let iter = AnnotState.iter
let iter_stmt = AnnotState.iter_key
let single_iter_stmt f s = List.iter f (get_all_annotations s)
let fold = AnnotState.fold
let fold_stmt = AnnotState.fold_key
let single_fold_stmt f s acc =
  List.fold_left (fun acc a -> f a acc) acc (get_all_annotations s)

let filter = AnnotState.filter

let self = AnnotState.self

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
