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

(* fwd ref to [Cil.CurrentLoc]... See cil.ml *)
module CurrentLoc = 
  Computation.Ref
    (struct
       include Cil_datatype.Location
       let default () = Lexing.dummy_pos, Lexing.dummy_pos
     end)
    (struct 
       let dependencies = [] 
       let name = "CurrentLoc"
     end)

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
    (Cil_datatype.Logic_Info)
    (struct
       let name = "logic functions table"
       let dependencies = []
       let size = 17
     end)

(*
module PredicateInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Predicate_Info)
    (struct
       let name = "predicate table"
       let dependencies = []
       let size = 17
     end)
*)

module LogicTypeInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Logic_Type_Info)
    (struct
       let name = "logic types table"
       let dependencies = []
       let size = 17
     end)

module LogicCtorInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Logic_Ctor_Info)
    (struct
       let name = "logic contructors table"
       let dependencies = []
       let size = 17
     end)

(* We depend from Cil_state, but it is initialized after Logic_typing... *)
let init_dependencies comp =
  LogicInfo.depend comp; (* PredicateInfo.depend comp; *)
  LogicTypeInfo.depend comp; LogicCtorInfo.depend comp

let is_logic_function = LogicInfo.mem
let find_logic_function = LogicInfo.find
let add_logic_function l =
  if is_logic_function l.l_name then
    error (CurrentLoc.get ()) "predicate or logic function %s already declared" l.l_name;
  LogicInfo.add l.l_name l

let remove_logic_function = LogicInfo.remove

let is_logic_type = LogicTypeInfo.mem
let find_logic_type = LogicTypeInfo.find
let add_logic_type t infos =
  if is_logic_type t
    (* type variables hide type definitions on their scope *)
  then error (CurrentLoc.get ()) "logic type %s already declared" t
  else LogicTypeInfo.add t infos

let is_logic_ctor = LogicCtorInfo.mem
let find_logic_ctor = LogicCtorInfo.find
let add_logic_ctor c infos =
  if is_logic_ctor c
  then error (CurrentLoc.get ()) "logic constructor %s already declared" c
  else LogicCtorInfo.add c infos

(*
let is_predicate = PredicateInfo.mem
let find_predicate = PredicateInfo.find
let add_predicate pred_info =
  if is_predicate pred_info.p_name then
    error (CurrentLoc.get ()) "predicate %s already declared" pred_info.p_name;
  PredicateInfo.add pred_info.p_name pred_info
let remove_predicate = PredicateInfo.remove
*)

module LogicBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Logic_Info)
    (struct
       let name = "builtin logic functions table"
       let dependencies = []
       let size = 17
     end)
let () = LogicInfo.depend LogicBuiltin.self

(*
module PredicateBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Predicate_Info)
    (struct
       let name = "builtin predicate table"
       let dependencies = []
       let size = 17
     end)
let () = PredicateInfo.depend PredicateBuiltin.self
*)

module LogicTypeBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Logic_Type_Info)
    (struct
       let name = "builtin logic types table"
       let dependencies = []
       let size = 17
     end)
let () = LogicTypeInfo.depend LogicTypeBuiltin.self

module LogicCtorBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Logic_Ctor_Info)
    (struct
       let name = "builtin logic contructors table"
       let dependencies = []
       let size = 17
     end)
let () = LogicCtorInfo.depend LogicCtorBuiltin.self

let builtin_states =
  let add x = Project.Selection.add x Kind.Do_Not_Select_Dependencies in
  add LogicBuiltin.self 
(*
    (add PredicateBuiltin.self
*)
       (add LogicTypeBuiltin.self 
	  (add LogicCtorBuiltin.self Project.Selection.empty))

let add_builtin_logic_function li =
  if not (LogicBuiltin.mem li.l_name) then begin
    LogicBuiltin.add li.l_name li;
    add_logic_function li
  end

(*
let add_builtin_predicate pi =
  if not (PredicateBuiltin.mem pi.p_name) then begin
    PredicateBuiltin.add pi.p_name pi;
    add_predicate pi
  end
*)

let add_builtin_logic_type name infos =
  if not (LogicTypeBuiltin.mem name) then begin
    LogicTypeBuiltin.add name infos;
    add_logic_type name infos
  end

let add_builtin_logic_ctor name infos =
  if not (LogicCtorBuiltin.mem name) then begin
    LogicCtorBuiltin.add name infos;
    add_logic_ctor name infos
  end

module Builtins=Hook.Make(struct type t = unit end)

let prepare_tables () =
  LogicCtorInfo.clear ();
  LogicTypeInfo.clear ();
(*
  PredicateInfo.clear ();
*)
  LogicInfo.clear ();
  LogicCtorBuiltin.iter LogicCtorInfo.add;
  LogicTypeBuiltin.iter LogicTypeInfo.add;
(*
  PredicateBuiltin.iter PredicateInfo.add;
*)
  LogicBuiltin.iter LogicInfo.add

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../../.."
  End:
*)
