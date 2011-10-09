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
open Cil

let get_code_annotation = function | User ca | AI (_,ca) -> ca

let get_annot_properties kf stmt a =
  Property.ip_of_code_annot kf stmt (get_code_annotation a)

module AnnotState =
  State_builder.Dashtbl
    (Dashtbl.Default_key_marshaler(Cil_datatype.Stmt))
    (Dashtbl.Default_data_marshaler
       (Cil_datatype.Rooted_code_annotation))
    (struct
       let name = "Annotations"
       let size = 17
       let dependencies = [ Ast.self ]
       let kind = `Internal
       let internal_kind = `Correctness
     end)

let () = 
  State_dependency_graph.Static.add_dependencies
    ~from:AnnotState.self 
    [ Property_status.self ]

let get_name a =
  Kernel.Unicode.without_unicode
    (Pretty_utils.sfprintf "%a" !Ast_printer.d_rooted_code_annotation) a

let add kf stmt states a = 
(*  Kernel.feedback "registering code annotation %a" 
    !Ast_printer.d_rooted_code_annotation a;*)
  let p = get_annot_properties kf stmt a in
  List.iter Property_status.register p;
  AnnotState.add (get_name a) stmt states a

let add_assert kf stmt states a =
  let a = User (Logic_const.new_code_annotation (AAssert ([],a))) in
  add kf stmt states a

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

let reset_stmt ~reset kf stmt =
  (* Kernel.feedback "reset stmt"; *)
  List.iter
    (fun a -> 
      let l = get_annot_properties kf stmt a in
      List.iter Property_status.remove l)
    (get_all_annotations stmt);
  AnnotState.remove_all ~reset stmt
  
let iter = AnnotState.iter
let iter_stmt = AnnotState.iter_key
let single_iter_stmt f s = List.iter f (get_all_annotations s)
let fold = AnnotState.fold
let fold_stmt = AnnotState.fold_key
let single_fold_stmt f s acc =
  List.fold_left (fun acc a -> f a acc) acc (get_all_annotations s)

let filter ~reset f kf stmt = 
  let f stmt s a =
    let keep = f stmt s a in
    if not keep then begin
      let l = get_annot_properties kf stmt a in
      List.iter Property_status.remove l
    end;
    keep 
  in
  AnnotState.filter ~reset f stmt

let set_annot ?(reset=true) kf stmt states f =
  let l = 
    single_fold_stmt 
      (fun a l -> 
	let old = get_annot_properties kf stmt a in
	let a = f a in
	let ppts = get_annot_properties kf stmt a in
	Property_status.merge ~old ppts;
	a :: l)
      stmt
      []
  in
  AnnotState.remove_all ~reset stmt;
  List.iter (fun a -> AnnotState.add (get_name a) stmt states a) l

let self = AnnotState.self

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
