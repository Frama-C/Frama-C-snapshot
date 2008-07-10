(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* fwd ref to Cil.currentLoc... See cil.ml *)
let dloc = ref (ref (Lexing.dummy_pos,Lexing.dummy_pos))
let error (b,e) fstring =
  let f fmt =
    Format.kfprintf (fun _ -> raise Errormsg.Error) fmt (fstring ^^ "@\n@]")
  in
  Format.kfprintf f Format.err_formatter
    "@[File %s, line %d, characters %d-%d:@\n\
     Error during analysis of annotation: "
    b.Lexing.pos_fname b.Lexing.pos_lnum
    (b.Lexing.pos_cnum - b.Lexing.pos_bol)
    (e.Lexing.pos_cnum - b.Lexing.pos_bol)

module LogicInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.logic_info let copy _ = assert false end))
    (struct
       let name = Project.Computation.Name.make "logic functions table"
       let dependencies = []
       let size = 17
     end)

module PredicateInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.predicate_info let copy _ = assert false end))
    (struct
       let name = Project.Computation.Name.make "predicate table"
       let dependencies = []
       let size = 17
     end)

module LogicTypeInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.logic_type_info
               let copy _ = assert false
        end))
    (struct
       let name = Project.Computation.Name.make "logic types table"
       let dependencies = []
       let size = 17
     end)

module LogicCtorInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.logic_ctor_info
               let copy _ = assert false
        end))
    (struct
       let name = Project.Computation.Name.make "logic contructors table"
       let dependencies = []
       let size = 17
     end)

(* We depend from Cil_state, but it is initialized after Logic_typing... *)
let init_dependencies comp =
  LogicInfo.depend comp; PredicateInfo.depend comp;
  LogicTypeInfo.depend comp; LogicCtorInfo.depend comp

let is_logic_function = LogicInfo.mem
let find_logic_function = LogicInfo.find
let add_logic_function l =
  if is_logic_function l.l_name then
    error !(!dloc) "logic function %s already declared" l.l_name;
  LogicInfo.add l.l_name l

let remove_logic_function = LogicInfo.remove

let is_logic_type = LogicTypeInfo.mem
let find_logic_type = LogicTypeInfo.find
let add_logic_type t infos =
  if is_logic_type t
    (* type variables hide type definitions on their scope *)
  then error !(!dloc) "logic type %s already declared" t
  else LogicTypeInfo.add t infos

let is_logic_ctor = LogicCtorInfo.mem
let find_logic_ctor = LogicCtorInfo.find
let add_logic_ctor c infos =
  if is_logic_ctor c
  then error !(!dloc) "logic constructor %s already declared" c
  else LogicCtorInfo.add c infos

let is_predicate = PredicateInfo.mem
let find_predicate = PredicateInfo.find
let add_predicate pred_info =
  if is_predicate pred_info.p_name then
    error !(!dloc) "predicate %s already declared" pred_info.p_name;
  PredicateInfo.add pred_info.p_name pred_info

module LogicBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.logic_info
               let copy c = { c with l_name = c.l_name}
        end))
    (struct
       let name = Project.Computation.Name.make "builtin logic functions table"
       let dependencies = []
       let size = 17
     end)

module PredicateBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.predicate_info
               let copy c = { c with p_name = c.p_name }  end))
    (struct
       let name = Project.Computation.Name.make "builtin predicate table"
       let dependencies = []
       let size = 17
     end)

module LogicTypeBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.logic_type_info
               let copy c = {nb_params = c.nb_params}
        end))
    (struct
       let name = Project.Computation.Name.make "builtin logic types table"
       let dependencies = []
       let size = 17
     end)

module LogicCtorBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Project.Datatype.Imperative
       (struct type t = Cil_types.logic_ctor_info
               let copy c = { c with ctor_name = c.ctor_name}
        end))
    (struct
       let name = Project.Computation.Name.make
         "builtin logic contructors table"
       let dependencies = []
       let size = 17
     end)

let add_builtin_logic_function li =
  try
    add_logic_function (LogicBuiltin.find li.l_name)
  with Not_found ->
    LogicBuiltin.add li.l_name li;
    add_logic_function li

let add_builtin_predicate pi =
  try
    add_predicate (PredicateBuiltin.find pi.p_name)
  with Not_found ->
    PredicateBuiltin.add pi.p_name pi;
    add_predicate pi

let add_builtin_logic_type name infos =
  try
    add_logic_type name (LogicTypeBuiltin.find name)
  with Not_found ->
    LogicTypeBuiltin.add name infos;
    add_logic_type name infos

let add_builtin_logic_ctor name infos =
  try
    add_logic_ctor name (LogicCtorBuiltin.find name)
  with Not_found ->
    LogicCtorBuiltin.add name infos;
    add_logic_ctor name infos

module Builtins=Hook.Make(struct type t = unit end)

let reset_all_tables () =
  LogicCtorInfo.clear ();
  LogicTypeInfo.clear ();
  PredicateInfo.clear ();
  LogicInfo.clear ();
  Builtins.apply ()

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../../.."
  End:
*)
