(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Cvalue

exception Invalid_nb_of_args of int

(* 'Always' means the builtin will always be used to replace a function
   with its name. 'OnAuto' means that the function will be replaced only
   if -val-builtins-auto is set. *)
type use_builtin = Always | OnAuto

let table = Hashtbl.create 17

let register_builtin name ?replace f =
  Hashtbl.add table name (f, Always);
  match replace with
  | None -> ()
  | Some name -> Hashtbl.add table name (f, OnAuto)

let () = Db.Value.register_builtin := register_builtin

(* The functions in _builtin must only return the 'Always' builtins *)

let registered_builtins () =
  let l =
    Hashtbl.fold
      (fun name (f, u) acc -> if u = Always then (name, f) :: acc else acc)
      table []
  in
  List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2) l

let () = Db.Value.registered_builtins := registered_builtins

let find_builtin name =
  let f, u = Hashtbl.find table name in
  if u = Always then f
  else raise Not_found

let mem_builtin name =
  try snd (Hashtbl.find table name) = Always
  with Not_found -> false

let () = Db.Value.mem_builtin := mem_builtin


let find_builtin_override kf =
  let name =
    try Value_parameters.BuiltinsOverrides.find kf
    with Not_found -> Kernel_function.get_name kf
  in
  try
    let f, u = Hashtbl.find table name in
    if u = Always || Value_parameters.BuiltinsAuto.get () then Some f
    else None
  with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Returning a clobbered set                                          --- *)
(* -------------------------------------------------------------------------- *)

let clobbered_set_from_ret state ret =
  let aux b _ acc =
    match Model.find_base_or_default b state with
    | `Top -> Base.SetLattice.top
    | `Bottom -> acc
    | `Value m ->
      if Locals_scoping.offsetmap_contains_local m then
        Base.SetLattice.(join (inject_singleton b) acc)
      else acc
  in
  try V.fold_topset_ok aux ret Base.SetLattice.bottom
  with V.Error_Top -> Base.SetLattice.top


(* -------------------------------------------------------------------------- *)
(* --- "Alarms" emitted by builtins for their preconditions               --- *)
(* -------------------------------------------------------------------------- *)

module BuiltinWarning =
  Datatype.Triple_with_collections
    (Cil_datatype.Stmt)(Datatype.String)(Datatype.String)
    (struct
      let module_name = "Value.Builtins.BuiltinWarning"
    end)

module CodeAnnotOfWarning =
  State_builder.Hashtbl(BuiltinWarning.Hashtbl)(Cil_datatype.Code_annotation)
    (struct
      let dependencies = [Db.Value.self]
      let name = "Value.Builtins.CodeAnnotOfWarnings"
      let size = 16
    end)

module EmittedWarnings =
  State_builder.Hashtbl(Cil_datatype.Stmt.Hashtbl)(Cil_datatype.Code_annotation.Set)
    (struct
      let dependencies = [Db.Value.self]
      let name = "Value.Builtins.EmittedWarnings"
      let size = 16
    end)
(* TODO: it would be nice to be able to remove the annotations registered
   here when this state is cleared (i.e. when Db.Value.self changes, but
   this is not currently possible. *)

let warning_gen stmt ~kind ~text =
  let loc = Cil_datatype.Stmt.loc stmt in
  let pred = List.hd (Logic_env.find_all_logic_functions "\\warning") in
  let s = Logic_const.tstring ~loc text in
  let np = Logic_const.unamed ~loc (Papp (pred, [], [s])) in
  let np = { np with pred_name = [kind] } in
  let ca = Logic_const.new_code_annotation (AAssert([], np)) in
  ca


let warning stmt ~kind ~text =
  CodeAnnotOfWarning.memo
    (fun (stmt, kind, text) -> warning_gen stmt ~kind ~text) (stmt, kind, text)

let emit_alarm ~kind ~text =
  let stack = Value_util.call_stack () in
  let kf, stmt = match stack with
    | (kf, Kstmt stmt) :: _ -> kf, stmt
    | _ -> assert false
  in
  let ca = warning stmt ~kind ~text in
  let to_add, cur =
    try
      let s = EmittedWarnings.find stmt in
      not (Cil_datatype.Code_annotation.Set.mem ca s), s
    with Not_found -> true, Cil_datatype.Code_annotation.Set.empty
  in
  if to_add then begin
    EmittedWarnings.replace stmt (Cil_datatype.Code_annotation.Set.add ca cur);
    Annotations.add_code_annot Value_util.emitter stmt ca;
    let ip = Property.ip_of_code_annot_single kf stmt ca in
    Property_status.emit ~distinct:true Value_util.emitter ~hyps:[]
      ip Property_status.Dont_know;
    true (* new alarm emitted *)
  end else false (* alarm had already been emitted *)

let fold_emitted_alarms = EmittedWarnings.fold

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
