(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** [type_to_add] contains types not referenced in [reference] and to be added
    in the interface. The list [reference] contains the names of the regular
    types of OCaml and the registered types of static plugins and kernel *)
let type_to_add: (string, string * string) Hashtbl.t = Hashtbl.create 97

let clash_with_compilation_unit = 
  let h = Hashtbl.create 97 in
  List.iter (fun s -> Hashtbl.add h s ()) Config.compilation_unit_names;
  fun s -> Hashtbl.mem h s || Hashtbl.mem h (String.lowercase s)

(** Modules can depend on each other, when a value of a given module depend
    on a type of another. It is then important to print them in an appropriate
    order. *)
module Module_deps = Graph.Imperative.Digraph.Concrete(Datatype.String)
let module_deps = Module_deps.create ()

(** Comments are registered appart in the module Dynamic *)
module Comment: sig 
  val add: string -> string -> unit
  val find: string -> string
end = struct
  let tbl: (string, string) Hashtbl.t = Hashtbl.create 97
  let add k v = if v <> "" then Hashtbl.add tbl k v
  let find k = try Hashtbl.find tbl k with Not_found -> ""
end

(**returns a list of the substrings *)
let split_dot s = Str.split (Str.regexp_string ".") s

let get_name i s =
  let li = split_dot s in
  let rec get_name_aux i j l =
    if i < j then match i, l with
    | _, []   -> ""
    | 0, h :: _ -> h
    | _ , _ :: q -> get_name_aux (i-1) (j-1) q
    else
      ""
  in
  get_name_aux i (List.length li) li
  
let sub_string_dot i s = 
  let rec sub_string_dot_aux j =  
    if j < i then get_name j s ^ "." ^ sub_string_dot_aux (j+1)
    else get_name i s
  in
  sub_string_dot_aux 0
    
(** If s = "module1.module2 ... .fname", then [function_name s] = "fname" *)
let function_name s = 
  let rec function_name_aux i s =
    match i , get_name (i+2) s , get_name (i+1) s with
      | 0,"","" -> ""
      | _,"",f  -> f
      | _,_,_   -> function_name_aux (i+1) s
  in
  function_name_aux 0 s
    
(** If s = "module1.module2 ... .fname", 
    then [long_function_name s] = "module2 ... .fname" *)
let long_function_name s = 
  let pt_idx = ref 0 in
  try
    for i = 0 to String.length s - 1 do
      if s.[i] = '.' then begin 
	pt_idx := i;
	raise Exit
      end
    done;
    s
  with Exit ->
    Str.string_after s (!pt_idx + 1)
  
(** when considering s = "plugin_name_0.plugin_name_1.function_name",
    [plugin_name s] ="plugin_name_0.plugin_name_1"  *)
let plugin_name s =
  let rec plugin_name_aux i s =
    match i , get_name (i+2) s , get_name (i+1) s with
    | 0, "", "" -> get_name 0 s
    | _, "", _ -> sub_string_dot i s
    | _, _, _   -> plugin_name_aux (i+1) s
  in
  plugin_name_aux 0 s
  
let sub_string_dot_compare i s1 s2 = sub_string_dot i s1 = sub_string_dot i s2

let first_divergence m1 m2 =
  let rec aux i = if sub_string_dot_compare i m1 m2 then aux (i+1) else i in
  sub_string_dot (aux 0) m1

(* m1 depends on m2 *)
let add_module_dep m1 m2 =
  Module_deps.add_edge module_deps m2 (first_divergence m1 m2)

let find_module_deps m1 =
  (* add the vertex in order to avoid OCamlGraph crashing on a non-existent
     vertex. *)
  Module_deps.add_vertex module_deps m1;
  let deps = Module_deps.pred module_deps m1 in
  let rec find_real_module m1 m =
    let complete_name = m1 ^ "." ^ m in
    if Hashtbl.mem type_to_add complete_name ||
      Hashtbl.mem functions_tbl complete_name
    then complete_name
    else
      let pre_m1 = plugin_name m1 in
      if m1 = pre_m1 then m else find_real_module m1 m
  in
  List.map (find_real_module m1) deps

(** true if m2 is a sub-module of m1 *)
let is_submodule m1 m2 = let m1' = first_divergence m1 m2 in m1 = m1'

(** [analyse_type] is called each time a new value is added to [functions_tbl]
    in the function [fill_tbl].  It considers what is given by
    [Type.get_embedded_type_name type_string], tests if the type to analyse is
    not already recorded in the [reference] list or creates the corresponding
    type in the Hashtable [type_to add] where the key is the module name of this
    type. *)
let analyse_type name l = 
  let add_type tbl name module_name typ = 
    let add_type_aux t s ty =
      let temp = try Hashtbl.find_all t s with Not_found -> [] in
      if not (List.mem ty temp) then Hashtbl.add t s ty
    in
    if function_name name = module_name
    then add_type_aux tbl name typ
    else begin
      if name <> module_name then add_module_dep name module_name;
      add_type_aux tbl module_name typ
    end
  in
  let analyse_type_aux s =
    if not (String.contains s '>') && (String.contains s '.') then
      if not (String.contains s ' ') then begin
	let s_name = get_name 0 s in
        if not (clash_with_compilation_unit s_name) then
	  let typ_n = function_name s in 
	  let module_name = plugin_name s in
      	  add_type type_to_add name module_name (typ_n, s)
      end else
	let lexbuf = Lexing.from_string s in
	let param, type_name = 
	  let l = 
	    Str.split (Str.regexp_string " ") (Grammar.main Lexer.token lexbuf) 
	  in
	  match l with
	  | [ h ] -> "", h
	  | [h1; h2 ] -> h1, h2
	  | _ -> "", ""          
	in 
	let ty_name = get_name 0 type_name in
	if String.contains type_name '.'
	  && not (clash_with_compilation_unit ty_name)
	then
	  let typ_n = param ^ " " ^ function_name type_name in 
	  let module_name = plugin_name type_name in
	  add_type type_to_add name module_name (typ_n, type_name)
  in
  List.iter analyse_type_aux (List.rev l) 

let is_option key = String.length key > 1 && String.rcontains_from key 1 '-'

(** It fills [function_tbl] with the content of [dynamic_values] which is a
    Hashtable recorded in the module Dynamic.  This Hashtable also contains
    options like: "-hello-help" or "-hello-debug".  The 'if' is taking away this
    useless strings and the module named "Dynamic" and fills the table with the
    suitable names. *)
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
      
(** It replaces the sub-strings "Plugin.type" of all the string [type_string]
    used in the module named "Plugin" by "type".
    It also removes the option stucture (e.g. "~gid:string" is replaced by
    "string"). *) 
let repair_type module_name type_string =
  let rec remove_param_name s =
    try
      let c = String.index s ':' in
      let after = remove_param_name (Str.string_after s (c+1)) in
      try 
	let n = String.index s '~' in
	if n < c then 
	  if n = 0 then after
	  else remove_param_name (Str.string_before s n) ^ after
	else
	  s
      with Not_found ->
	if c = 0 then after
	else
	  let sp = String.rindex (Str.string_before s c) ' ' in
	  remove_param_name (Str.string_before s (sp + 1)) ^ after
    with Not_found -> 
      s 
  in 
  let remove_name_module s module_n =
    Str.global_replace (Str.regexp (module_n ^ "\\.")) "" s
  in 
  match split_dot module_name with
  | [] -> type_string
  | l -> List.fold_left remove_name_module (remove_param_name type_string) l
      
(** For each key of the table [functions_tbl], [print_plugin] takes all 
    the pieces of information found in the Hashtable [dynamic_values]
    of the module Dynamic and stored in the 3 Hashtables
    ([functions_tb]l, [type_to_add], [comment_tbl]) and builds up a string 
    in order to write the signature of this module in the .mli file *)
let print_plugin fmt =
  let modules_list: (string, unit) Hashtbl.t = Hashtbl.create 7 in
  let rec space i = match i with 
    | 0 -> ""
    | _ -> space (i-1) ^ "  " 
  in
  let rec print_types fmt sp = function
    | [] -> ()
    | (h, long_h) :: q -> 
      Format.fprintf fmt "@\n%stype %s@\n%s  \
(** @@call by writing [T.ty] where [T] has previously been defined by: \
[let module T = Type.Make(struct let name = %s end)] *)" 
	sp h sp long_h; 
      print_types fmt sp q
  in
  let rec print_one_plugin fmt i key1 =
    if not (get_name i key1 = "") then
      let module_name = sub_string_dot i key1 in
      if not (Hashtbl.mem modules_list module_name) then begin
	Hashtbl.add modules_list module_name ();
        (* Check whether there are some modules to be treated before us. *)
        let deps = find_module_deps key1 in
        let extern, sub_modules = List.partition (is_submodule key1) deps in
        List.iter (print_one_plugin fmt i) extern;
	let short_module_name = String.capitalize (get_name i key1) in
	let space_i = space i in
	Format.fprintf fmt "\n \n%smodule %s:\n%ssig "
	  space_i
	  short_module_name
	  space_i;
        List.iter (print_one_plugin fmt (succ i)) sub_modules;
	let module_types =
	  try Hashtbl.find_all type_to_add module_name
	  with Not_found -> []
	in 
	print_types fmt (space i) module_types ;
	let print_one_plugin_aux fmt key elem = 
	  if sub_string_dot i key = module_name then
	    let succ_i = succ i in
	    if get_name succ_i key = "" then begin
	      let plugin_name = sub_string_dot 0 key1 in
	      let found_comment = Comment.find (key ^ "." ^ elem.name) in
	      Format.fprintf fmt
		"@\n%s@[  @[val %s:@ %s@]@\n%s@[  (** %s\n\
@@call Dynamic.get ~plugin:\"%s\" \"%s\" %s *)@]@]@\n"
		space_i
		elem.name
		(repair_type module_name elem.type_string)
		space_i
		found_comment
		plugin_name
		(long_function_name (key ^ "." ^ elem.name))
		elem.datatype_string;
	      Hashtbl.remove functions_tbl key
	    end else
	      print_one_plugin fmt succ_i key
	in
	Hashtbl.iter (print_one_plugin_aux fmt) functions_tbl ;
	Format.fprintf fmt "\n%send" (space i)
      end
  in
  let print_all fmt i key _ = print_one_plugin fmt i key in
  Format.fprintf fmt "@[%t@]"
    (fun fmt -> Hashtbl.iter (print_all fmt 0) functions_tbl)
    
(** [print] is the main function of this module.
    It takes one argument which is the path and opens the
    file path/dynamic_plugins.mli. It fills [functions_tbl],
    [comment_tbl] and [type_to_add]
    using the functions [fill_tbl] and [add_comment] and then 
    prints the plugins in the file with [print_plugin] *)
let print path =  
  try 
    Dynamic.iter fill_tbl; 
    Dynamic.iter_comment Comment.add; 
    let channel = open_out (path ^ "/dynamic_plugins.mli") in 
    let fmt = Format.formatter_of_out_channel channel in
    Format.fprintf fmt
      "@[@[(** This@ module@ contains@ all@ the@ dynamically@ \
registered@ plugins *)@]@ %t@]" 
      print_plugin; 
    close_out channel
  with Sys_error _ as e -> 
    Self.error "%s" (Printexc.to_string e)

(** register [print (path : string)] *)
let print =
  Dynamic.register
    ~comment: "Create a .mli file used by 'make doc' \
to generate the html documentation of dynamic plug-ins.\
 It takes the path where to create this file as an argument."
    ~plugin:"Print_api"
    "run"
    ~journalize:true
    (Datatype.func Datatype.string Datatype.unit)
    print

let run () = if not (Enabled.is_default ()) then print (Enabled.get ())

let () = Db.Main.extend run
