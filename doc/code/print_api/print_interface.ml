(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Register the new plugin. *)
module Self =
  Plugin.Register
  (struct
    let name = "Print interface"
    let shortname = "print_api"
    let help = "This plugin creates a file containing all\
 the registered signatures of the dynamic plugins"
   end)

(** Register the new Frama-C option "-print_api". *)
module Enabled =
  Self.String
  (struct
     let option_name = "-print_api"
     let help = "creates a .mli file for the dynamic plugins"
     let arg_name = " the absolute path for the .mli to be created"
     let default = ""
  end)

type function_element = 
    { name: string; 
      type_string: string; 
      datatype_string: string }

(** Each object of the table is going to be composed of :
    (function_name, type_string) 
    and its corresponding key is "plugin_name" *)
let functions_tbl = Hashtbl.create 97

(** [type_to_add] contains
    types not referenced in [reference] and to be added in the interface.
    The list [reference] contains the names of the regular types of OCaml
    and the registered types of static plugins and kernel *)
let type_to_add = Hashtbl.create 97
let reference = Config.compilation_unit_names 

(** Comments are registered appart in the module Dynamic *)
let comment_tbl:(string,string) Hashtbl.t = Hashtbl.create 97

let add_comment s1 s2 = match s2 with
  | "" -> ()
  | _  -> Hashtbl.add comment_tbl s1 s2

(**returns a list of the substrings *)
let split_dot s = Str.split (Str.regexp_string ".") s

let get_name i s =
  let li = split_dot s in
  let rec get_name_aux i j l =
    if i < j then match i,l with
      | _,[]   -> ""
      | 0,h::_ -> h
      | _,_::q -> get_name_aux (i-1) (j-1) q
    else
      ""
  in
  get_name_aux i (List.length li) li
  
let sub_string_dot i s = 
  let rec sub_string_dot_aux j st =  
    if j < i then get_name j st ^ "." ^ sub_string_dot_aux (j+1) st
    else get_name i st
  in
  sub_string_dot_aux 0 s
    
(** when considering s= "plugin_name_0.plugin_name_1.function_name",
    [function_name s] = "function_name" *)
let function_name s = 
  let rec function_name_aux i s =
    match i , get_name (i+2) s , get_name (i+1) s with
      | 0,"","" -> ""
      | _,"",f  -> f
      | _,_,_   -> function_name_aux (i+1) s
  in
  function_name_aux 0 s
  
(** when considering s = "plugin_name_0.plugin_name_1.function_name",
    [plugin_name s] ="plugin_name_0.plugin_name_1"  *)
let plugin_name s =
  let rec plugin_name_aux i s =
    match i , get_name (i+2) s , get_name (i+1) s with
      | 0,"","" -> get_name 0 s
      | _,"",_  -> sub_string_dot i s
      | _,_,_   -> plugin_name_aux (i+1) s
  in
  plugin_name_aux 0 s
  
let sub_string_dot_compare i s1 s2 =
  sub_string_dot i s1 = sub_string_dot i s2
  
(** [analyse_type] is called each time a new value is
    added to [functions_tbl] in the function [fill_tbl].
    It considers what is given by
    [Type.get_embedded_type_name type_string],
    tests if the type to analyse is not already recorded 
    in the [reference] list or creates the corresponding type 
    in the Hashtable [type_to add] where the key is the module
    name of this type. *)
let analyse_type name l = 
  let add_type tbl name module_name typ = 
    let add_type_aux t s ty =
      let temp = 
	try Hashtbl.find_all t s
	with Not_found ->[]
      in
      if not(List.mem ty temp) then Hashtbl.add t s ty
    in
    if function_name name = module_name
    then add_type_aux tbl name typ
    else add_type_aux tbl module_name typ
  in
  let analyse_type_aux s =
    if not (String.contains s '>') && (String.contains s '.') then
      if not (String.contains s ' ') then begin
	let s_name = get_name 0 s in
        if not (List.mem s_name reference)
          && not (List.mem (String.lowercase s_name) reference)
	then
	  let typ_n = function_name s in 
	  let module_name = plugin_name s in
      	  add_type type_to_add name module_name typ_n 
      end else
	let lexbuf = Lexing.from_string s in
	let param,type_name =  
	  match Str.split (Str.regexp_string " ")
	    (Grammar.main Lexer.token lexbuf) with
	    | h::[]      -> "",h
	    | h1::h2::[] -> h1,h2
	    | _          -> "",""          
	in 
	let ty_name = get_name 0 type_name in
	if String.contains type_name '.'
	  && not (List.mem ty_name reference)
	  && not (List.mem (String.lowercase ty_name) reference)
	then
	  let typ_n =param^" "^(function_name type_name) in 
	  let module_name = plugin_name type_name in
	  add_type type_to_add name module_name typ_n 
  in
  List.iter analyse_type_aux (List.rev l) 

let is_option key = 
  String.length key > 1 && String.rcontains_from key 1 '-'

(** It fills [function_tbl] with the content of
    [dynamic_values] which is a  Hashtable
    recorded in the module Dynamic. 
    This Hashtable also contains options 
    like: "-hello-help" or "-hello-debug".
    The 'if' is taking away this useless strings
    and the module named "Dynamic"
    and fills the table with the suitable names. *)
let fill_tbl key typ _ =
  if not (is_option key || get_name 0 key = "Dynamic") then
    let type_list = Type.get_embedded_type_names typ in
    let func_elem = 
      { name = function_name key ; 
	type_string = Type.name typ ;
	datatype_string = Type.ml_name typ }
    in
    Hashtbl.add functions_tbl (plugin_name key) func_elem;
    analyse_type (plugin_name key) type_list
	 
(** It replaces the sub-strings "Plugin.type"
    of all the string [type_string] used in the module
    named "Plugin" by "type".
    It also removes the option stucture
    (ex : "~gid:string" is replaced by
    "string"). *)
let repair_type module_name type_string =
  let rec remove_param_name s =
    try
      let c= String.index s ':' in
      try 
	let n = String.index s '~' in
	if n < c then 
	  match n with
	  | 0 -> remove_param_name (Str.string_after s (c+1))
	  | _ -> 
	    remove_param_name (Str.string_before s n)
	    ^ remove_param_name (Str.string_after s (c+1))
	else s
      with Not_found ->
	(match c with
	| 0 -> remove_param_name (Str.string_after s (c+1))
	| _ -> 
	  let sp = String.rindex (Str.string_before s c) ' ' in
	  remove_param_name (Str.string_before s (sp + 1)) 
	  ^ remove_param_name (Str.string_after s (c + 1)))
	  
    with Not_found -> 
      s 
  in 
  let remove_name_module s module_n =
    Str.global_replace (Str.regexp (module_n ^ "\\.")) "" s
  in 
  match split_dot module_name with
  | [] -> type_string
  | _ as l -> 
    List.fold_left remove_name_module (remove_param_name type_string) l
      
(** For each key of the table [functions_tbl], [print_plugin] takes all 
    the pieces of information found in the Hashtable [dynamic_values]
    of the module Dynamic and stored in the 3 Hashtables
    ([functions_tb]l, [type_to_add], [comment_tbl]) and builds up a string 
    in order to write the signature of this module in the .mli file *)
let print_plugin chan =
  let module_list = ref [] in
  let rec space i = match i with 
    | 0 -> ""
    | _ -> space (i-1) ^ "  " 
  in
  let rec print_type c sp l = match l with
    | [] -> ()
    | h :: q -> 
      output_string c ("\n" ^ sp ^ "type " ^ h); 
      print_type c sp q
  in
  let rec print_one_plugin channel i key1 =
    if not (get_name i key1 = "") then
      let module_name = sub_string_dot i key1 in
      if not (List.mem  module_name !module_list) then begin
	module_list := module_name::!module_list ; 
	let s = get_name i key1 in
	output_string channel
	  ("\n \n" ^ space i ^ "module " ^ String.capitalize s ^
	      ":\n" ^ space i ^ "sig ") ;
	let module_types =
	  try Hashtbl.find_all type_to_add module_name
	  with Not_found -> []
	in 
	print_type channel (space i) module_types ;
	let print_one_plugin_aux key elem = 
	  if sub_string_dot i key = module_name then
	    let succ_i = succ i in
	    if get_name succ_i key = "" then begin
	      let typ_name = elem.type_string in
	      let fct_name = elem.name in
	      let message = 
		"\n" ^ space i ^ "  val "^ fct_name ^ " : "
		^ repair_type module_name typ_name
	      in
	      let standard_comment =
		"@call Dynamic.get ~plugin:\"" ^ module_name ^ "\" \"" 
		^ fct_name ^ "\" " ^ elem.datatype_string
	      in
	      let found_comment =
		try Hashtbl.find comment_tbl (key ^ "." ^ fct_name)
		with Not_found -> ""
	      in
	      let comment = 
		"\n" ^ space i ^ "  (**" ^ standard_comment
		^ "\n" ^ found_comment ^ " *)\n"
	      in
	      output_string channel (message ^ comment);
	      Hashtbl.remove functions_tbl key
	    end else
	      print_one_plugin channel succ_i key
	in
	Hashtbl.iter print_one_plugin_aux functions_tbl ;
	output_string channel ("\n" ^ space i ^ "end")
      end
  in
  let print_all i key _ = print_one_plugin chan i key in
  Hashtbl.iter (print_all 0) functions_tbl 
    
(** [print] is the main function of this module.
    It takes one argument which is the path and opens the
    file path/dynamic_plugins.mli. It fills [functions_tbl],
    [comment_tbl] and [type_to_add]
    using the functions [fill_tbl] and [add_comment] and then 
    prints the plugins in the file with [print_plugin] *)
let print path =  try 
     let channel = open_out (path^"/dynamic_plugins.mli") in 
      Dynamic.iter fill_tbl; Dynamic.iter_comment add_comment; 
     output_string channel "(**This module contains all the dynamically \
registered plugins *)" ;
     print_plugin channel; close_out channel
   with Sys_error _ as e -> Self.error "%s" (Printexc.to_string e)

(** register [print (path : string)] *)
let print =
  Dynamic.register
    ~comment: "this creates a .mli file used in the Makefile (make doc) \
to create (with ocamldoc) a html documentation.\
 It takes the path where to create this file as an argument."
    ~plugin:"Print_api"
    "run"
    ~journalize:true
    (Datatype.func Datatype.string Datatype.unit)
    print

let run () = if not (Enabled.is_default ()) then print (Enabled.get ())

let () = Db.Main.extend run
