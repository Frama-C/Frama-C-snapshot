(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module CurrentLoc = Cil_const.CurrentLoc

let error (b,_e) fstring =
  Kernel.abort
    ~source:b
    ("In annotation: " ^^ fstring)

module Logic_builtin =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Builtin_logic_info)
    (struct
       let name = "built-in logic functions table"
       let dependencies = []
       let size = 17
     end)

module Logic_info =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Logic_info)
    (struct
       let name = "logic functions table"
       let dependencies = [ Logic_builtin.self ]
       let size = 17
     end)

module Logic_builtin_used = struct
  include State_builder.Ref
    (Cil_datatype.Logic_info.Set)
    (struct
      let name = "used built-in logic functions"
      let dependencies = [ Logic_builtin.self; Logic_info.self ]
      let default () = Cil_datatype.Logic_info.Set.empty
     end)
  let add li = set (Cil_datatype.Logic_info.Set.add li (get()))
  let mem li = Cil_datatype.Logic_info.Set.mem li (get())
  let iter f = Cil_datatype.Logic_info.Set.iter f (get())
end

module Logic_type_builtin =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Logic_type_info)
    (struct
       let name = "built-in logic types table"
       let dependencies = []
       let size = 17
     end)


let is_builtin_logic_type = Logic_type_builtin.mem

module Logic_type_info =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Logic_type_info)
    (struct
       let name = "logic types table"
       let dependencies = [ Logic_type_builtin.self ]
       let size = 17
     end)

module Logic_ctor_builtin =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Logic_ctor_info)
    (struct
       let name = "built-in logic contructors table"
       let dependencies = []
       let size = 17
     end)

module Logic_ctor_info =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Logic_ctor_info)
    (struct
       let name = "logic contructors table"
       let dependencies = [ Logic_ctor_builtin.self ]
       let size = 17
     end)

module Lemmas =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Global_annotation)
    (struct
        let name = "lemmas"
        let dependencies = []
        let size = 17
     end)

module Model_info =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Cil_datatype.Model_info)
    (struct
      let name = "model fields table"
      let dependencies = []
      let size = 17
     end)

(* We depend from ast, but it is initialized after Logic_typing... *)
let init_dependencies from =
  State_dependency_graph.add_dependencies
    ~from
    [ Logic_info.self; 
      Logic_type_info.self;
      Logic_ctor_info.self;
      Lemmas.self;
      Model_info.self;
    ]

let builtin_to_logic b =
  let params =
    List.map (fun (x, t) -> Cil_const.make_logic_var_formal x t) b.bl_profile
  in
  let li = Cil_const.make_logic_info b.bl_name in
  li.l_type <- b.bl_type;
  li.l_tparams <- b.bl_params;
  li.l_profile <- params;
  li.l_labels <- b.bl_labels;
  Logic_builtin_used.add li;
  Logic_info.add b.bl_name li;
  li

let is_builtin_logic_function = Logic_builtin.mem

let is_logic_function s = is_builtin_logic_function s || Logic_info.mem s

let find_all_logic_functions s =
  match Logic_info.find_all s with
  | [] ->
    let builtins = Logic_builtin.find_all s in
    let res = List.map builtin_to_logic builtins in
	(*        Format.printf "builtin func:@.";
		  List.iter (fun x -> Format.printf "%s#%d@." x.l_var_info.lv_name x.l_var_info.lv_id) res;
	 *)
    res
  | l ->
	(*        Format.printf "func in env:@.";
		  List.iter (fun x -> Format.printf "%s#%d@." x.l_var_info.lv_name x.l_var_info.lv_id) l; *)
    l

let find_logic_cons vi =
  List.find
    (fun x -> Cil_datatype.Logic_var.equal x.l_var_info vi)
    (Logic_info.find_all vi.lv_name)

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
    (Logic_info.find_all l.l_var_info.lv_name);
  Logic_info.add l.l_var_info.lv_name l

let remove_logic_function = Logic_info.remove

let is_logic_type = Logic_type_info.mem
let find_logic_type = Logic_type_info.find
let add_logic_type t infos =
  if is_logic_type t
    (* type variables hide type definitions on their scope *)
  then error (CurrentLoc.get ()) "logic type %s already declared" t
  else Logic_type_info.add t infos
let remove_logic_type = Logic_type_info.remove

let is_logic_ctor = Logic_ctor_info.mem
let find_logic_ctor = Logic_ctor_info.find
let add_logic_ctor c infos =
  if is_logic_ctor c
  then error (CurrentLoc.get ()) "logic constructor %s already declared" c
  else Logic_ctor_info.add c infos
let remove_logic_ctor = Logic_ctor_info.remove

let is_model_field = Model_info.mem

let find_all_model_fields s = Model_info.find_all s

let find_model_field s typ =
  let l = Model_info.find_all s in
  let rec find_cons typ =
    try
      List.find (fun x -> Cil_datatype.Typ.equal x.mi_base_type typ) l
    with Not_found as e ->
      (* Don't use Cil.unrollType here:
         unrollType will unroll until it finds something other
         than TNamed. We want to go step by step.
      *)
      (match typ with
        | TNamed(ti,_) -> find_cons ti.ttype
        | _ -> raise e)
  in find_cons typ

let add_model_field m = 
  try
    ignore (find_model_field m.mi_name m.mi_base_type);
    error (CurrentLoc.get())
      "Cannot add model field %s to type %a: it already exists."
      m.mi_name Cil_datatype.Typ.pretty m.mi_base_type
  with Not_found -> Model_info.add m.mi_name m

let remove_model_field = Model_info.remove

let is_builtin_logic_ctor = Logic_ctor_builtin.mem

let builtin_states =
  [ Logic_builtin.self; Logic_type_builtin.self; Logic_ctor_builtin.self ]

module Builtins= struct
  include Hook.Make(struct end)
    (* ensures we do not apply the hooks twice *)
  module Applied =
    State_builder.False_ref
      (struct
        let name = "Application of logic built-ins hook"
        let dependencies = builtin_states
         (* if the built-in states are not kept, hooks must be replayed. *)
       end)

  let apply () =
    Kernel.feedback ~level:5 "Applying logic built-ins hooks for project %s"
      (Project.get_name (Project.current()));
    if Applied.get () then Kernel.feedback ~level:5 "Already applied"
    else begin Applied.set true; apply () end
end

let prepare_tables () =
  Logic_ctor_info.clear ();
  Logic_type_info.clear ();
  Logic_info.clear ();
  Lemmas.clear ();
  Model_info.clear ();
  Logic_type_builtin.iter Logic_type_info.add;
  Logic_ctor_builtin.iter Logic_ctor_info.add;
  Logic_builtin_used.iter (fun x -> Logic_info.add x.l_var_info.lv_name x)

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
  Logic_type_builtin.iter (fun x _ -> add_typename x)

let add_builtin_logic_function_gen is_same_profile l =
  List.iter
    (fun li ->
       if is_same_profile li l then
	 error (CurrentLoc.get ())
	   "already declared builtin logic function or predicate \
            %s with same profile"
	   l.bl_name)
    (Logic_builtin.find_all l.bl_name);
  Logic_builtin.add l.bl_name l

let add_builtin_logic_type name infos =
  if not (Logic_type_builtin.mem name) then begin
    Logic_type_builtin.add name infos;
    add_typename name;
    add_logic_type name infos
  end

let add_builtin_logic_ctor name infos =
  if not (Logic_ctor_builtin.mem name) then begin
    Logic_ctor_builtin.add name infos;
    add_logic_ctor name infos
  end

let iter_builtin_logic_function f = Logic_builtin.iter (fun _ info -> f info)
let iter_builtin_logic_type f = Logic_type_builtin.iter (fun _ info -> f info)
let iter_builtin_logic_ctor f = Logic_ctor_builtin.iter (fun _ info -> f info)

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)
