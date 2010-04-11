(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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

module CurrentLoc = Cil_const.CurrentLoc

let error (b,_e) fstring =
  Cilmsg.abort
    ~source:{
      Log.src_file = b.Lexing.pos_fname ;
      Log.src_line = b.Lexing.pos_lnum ;
    }
    ("In annotation: " ^^ fstring)

module LogicInfo =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Logic_Info)
    (struct
       let name = "logic functions table"
       let dependencies = []
       let size = 17
     end)

module LogicBuiltin =
  Computation.Hashtbl
    (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
    (Cil_datatype.Builtin_Logic_Info)
    (struct
       let name = "builtin logic functions table"
       let dependencies = []
       let size = 17
     end)
let () = LogicInfo.depend LogicBuiltin.self

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

(* We depend from ast, but it is initialized after Logic_typing... *)
let init_dependencies comp =
  LogicInfo.depend comp;
  LogicTypeInfo.depend comp;
  LogicCtorInfo.depend comp

(* keep track of logic functions builtins that have been used
   across various input files *)
module LogicInfoUsedBuiltin = Hook.Make(struct type t = unit end)

let builtin_to_logic b =
  let params =
    List.map (fun (x,t) -> Cil_const.make_logic_var x t) b.bl_profile in
  let li = Cil_const.make_logic_info b.bl_name in
  li.l_type <- b.bl_type;
  li.l_tparams <- b.bl_params;
  li.l_profile <- params;
  li.l_labels <- b.bl_labels;
  let add () =
    LogicInfo.add b.bl_name li
  in
  LogicInfoUsedBuiltin.extend add;
  add ();
  li

let is_builtin_logic_function = LogicBuiltin.mem

let is_logic_function s = is_builtin_logic_function s || LogicInfo.mem s
(*
let find_logic_function = LogicInfo.find
*)
let find_all_logic_functions s =
  match
    LogicInfo.find_all s
  with
      [] ->
        let builtins = LogicBuiltin.find_all s in
        List.map builtin_to_logic builtins
    | l -> l

let find_logic_cons vi =
  List.find (fun x -> x.l_var_info.lv_id = vi.lv_id)
    (LogicInfo.find_all vi.lv_name)

(* add_logic_function takes as argument a function eq_logic_info which
   decides whether two logic_info are identical. It is intended to be
   Logic_utils.is_same_logic_profile, but this one can not be called
   from here since it will cause a circular dependency Logic_env <-
   Logic_utils <- Cil <- Logic_env
*)

let add_logic_function_gen is_same_profile l =
  if is_builtin_logic_function l.l_var_info.lv_name then
    error (CurrentLoc.get())
      "logic function or predicate %s is built-in. You can not redefine it"
      l.l_var_info.lv_name
  ;
  List.iter
    (fun li ->
       if is_same_profile li l then
	 error (CurrentLoc.get ())
	   "already declared logic function or predicate %s with same profile"
	   l.l_var_info.lv_name)
    (LogicInfo.find_all l.l_var_info.lv_name);
  LogicInfo.add l.l_var_info.lv_name l

let remove_logic_function = LogicInfo.remove

let is_logic_type = LogicTypeInfo.mem
let find_logic_type = LogicTypeInfo.find
let add_logic_type t infos =
  if is_logic_type t
    (* type variables hide type definitions on their scope *)
  then error (CurrentLoc.get ()) "logic type %s already declared" t
  else LogicTypeInfo.add t infos
let remove_logic_type = LogicTypeInfo.remove

let is_logic_ctor = LogicCtorInfo.mem
let find_logic_ctor = LogicCtorInfo.find
let add_logic_ctor c infos =
  if is_logic_ctor c
  then error (CurrentLoc.get ()) "logic constructor %s already declared" c
  else LogicCtorInfo.add c infos
let remove_logic_ctor = LogicCtorInfo.remove

module LogicTypeBuiltin =
  Computation.Hashtbl
    (Datatype.String)
    (Cil_datatype.Logic_Type_Info)
    (struct
       let name = "builtin logic types table"
       let dependencies = []
       let size = 17
     end)
let () = LogicTypeInfo.depend LogicTypeBuiltin.self

let is_builtin_logic_type = LogicTypeBuiltin.mem

module LogicCtorBuiltin =
  Computation.Hashtbl
    (Datatype.String)
    (Cil_datatype.Logic_Ctor_Info)
    (struct
       let name = "builtin logic contructors table"
       let dependencies = []
       let size = 17
     end)
let () = LogicCtorInfo.depend LogicCtorBuiltin.self

let is_builtin_logic_ctor = LogicCtorBuiltin.mem

let builtin_states =
  let add x = Project.Selection.add x Kind.Do_Not_Select_Dependencies in
  add LogicBuiltin.self
    (add LogicTypeBuiltin.self
       (add LogicCtorBuiltin.self Project.Selection.empty))

module Builtins= struct
  include Hook.Make(struct type t = unit end)
    (* ensures we do not apply the hooks twice *)
  module Applied =
    Computation.Ref(struct include Datatype.Bool let default () = false end)
      (struct
         let name="Application of logic built-ins hook"
         let dependencies= [LogicBuiltin.self; LogicTypeBuiltin.self;
                            LogicCtorBuiltin.self]
           (* if the built-in states are not kept,
              hooks must be replayed.
            *)
       end)
  let apply () =
    Cilmsg.feedback ~level:5 "Applying logic built-ins hooks for project %s"
      (Project.name (Project.current()))
    ;
    if not (Applied.get ()) then begin Applied.set true; apply () end
    else Cilmsg.feedback ~level:5 "Already applied"
end

let called = ref false

let prepare_tables () =
  LogicCtorInfo.clear ();
  LogicTypeInfo.clear ();
  LogicInfo.clear ();
  LogicInfoUsedBuiltin.apply();
  LogicTypeBuiltin.iter LogicTypeInfo.add;
  LogicCtorBuiltin.iter LogicCtorInfo.add (*;*)
(*  LogicBuiltin.iter LogicInfo.add *)

(** C typedefs *)
(**
  -  true => identifier is a type name
  -  false => identifier is a plain identifier
*)
let typenames: (string, bool) Hashtbl.t = Hashtbl.create 13

let add_typename t = Hashtbl.add typenames t true

let hide_typename t = Hashtbl.add typenames t false

let remove_typename t = Hashtbl.remove typenames t

let reset_typenames () = Hashtbl.clear typenames

let typename_status t =
  try Hashtbl.find typenames t with Not_found -> false

let builtin_types_as_typenames () =
  LogicTypeBuiltin.iter (fun x _ -> add_typename x)

let add_builtin_logic_function_gen is_same_profile l =
  List.iter
    (fun li ->
       if is_same_profile li l then
	 error (CurrentLoc.get ())
	   "already declared builtin logic function or predicate \
            %s with same profile"
	   l.bl_name)
    (LogicBuiltin.find_all l.bl_name);
    LogicBuiltin.add l.bl_name l

let add_builtin_logic_type name infos =
  if not (LogicTypeBuiltin.mem name) then begin
    LogicTypeBuiltin.add name infos;
    add_typename name;
    add_logic_type name infos
  end

let add_builtin_logic_ctor name infos =
  if not (LogicCtorBuiltin.mem name) then begin
    LogicCtorBuiltin.add name infos;
    add_logic_ctor name infos
  end

let iter_builtin_logic_function f =
  LogicBuiltin.iter (fun _ info -> f info)

let iter_builtin_logic_type f =
  LogicTypeBuiltin.iter (fun _ info -> f info)

let iter_builtin_logic_ctor f =
  LogicCtorBuiltin.iter (fun _ info -> f info)

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../../.."
  End:
*)
