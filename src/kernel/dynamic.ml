(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: dynamic.ml,v 1.15 2008/12/03 08:53:39 uid568 Exp $ *)

(* ************************************************************************* *)
(** {2 Gobal variables for paths} *)
(* ************************************************************************* *)

let default_dyn_path = 
  [ Filename.concat "lib" "plugins";
    Filename.concat Version.dataroot "plugins" ]

let all_dyn_path = ref []

(* ************************************************************************* *)
(** {2 Debugging} *)
(* ************************************************************************* *)
  
(* All debug messages raise before command line parsing, are hidden when I
   use Dynamic.Debug.get. To escape this problem, I use a hook on debug messages 
   and after command line parsing I display them if debug command line is
   enable.
*)
  
(** Catch all level 1 debug messages *)
module Trace_level1 = Hook.Make (struct end)
  
(** Catch all level 2 and higher debug messages *)
module Trace_level2 = Hook.Make (struct end)

(** [display lvl fun] If debug command line level is enable, [fun] is applied,
    else [fun] is hooked. [fun] is applied or hooked according to debug
    level and [lvl]   *)  
let display level message=
  match Cmdline.Dynamic.Debug.get () with
  | n when n <=0 -> begin
      match level with
      | n when n<=0 -> ()
      | 1 -> Trace_level1.extend message; Trace_level2.extend message
      | _ -> Trace_level2.extend message
    end
  | 1 when level = 1 -> message ()
  | 1 -> ()
  | _ -> message ()

      
let trace ()=
  let trace f = 
    Format.printf "Traceback Db.Dynamic ...@."; 
    f ();
    Format.printf "... End Traceback @."
  in
  match Cmdline.Dynamic.Debug.get () with
  |n when n <=0 -> ()
  |1  -> trace Trace_level1.apply
  |_  -> trace Trace_level2.apply
     
(* Useful to display error for Sys.readdir function used to determine if a path
 * is correct.
 * Don't display error when default path is not created (use frama-c whitout
 * install) 
 *)
(** @return catch all Sys_error which have "%s: %s", where %s are string,
    profile.*)
let catch_sysreaddir s = 
  let list_arg = Str.split (Str.regexp ": ") s in 
  if List.length list_arg = 2 then
    let dir,error = List.hd(list_arg),List.hd(List.tl(list_arg)) in begin
      if not (List.mem dir default_dyn_path) then
	Format.eprintf "[Dynamic] Warning: cannot search dynamic plugins inside \
directory %s (%s).@." 
	  dir error
    end
  else 
    raise (Sys_error s)  
      
(* Used to display step of dynamic loading*)
let debug_include s = Format.printf "@[<v 0>Registering %s" s
 
(* To determine if we have a ocaml version lower than 3.11 or not *) 
let is_lower_than_311 =
  try ignore (MyDynlink.init ()); false
  with MyDynlink.Unsupported_Feature _ -> true

(* Function do nothing if *)
let do_nothing _ = ()
let is_dynlink_available = not (is_lower_than_311 && MyDynlink.is_native)
let function_available f = if is_dynlink_available then f else do_nothing
 
    
(* ************************************************************************* *)
(** {2 Path functions} *)
(* ************************************************************************* *)

(* The following function is only available for OCaML 3.10 and higher:
 * let check_path path = if Sys.file_exists path && Sys.is_directory path
 * But the following function is more expensive in time but is available for 
 * all supported OCaml version of frama-c 
 *)
(** @return true iff [path] is a readable directory *)
let check_path path = 
  try ignore (Sys.readdir path); true
  with Sys_error s -> catch_sysreaddir s; false

(** Display debug message and add a path to list of search path *)
let add_path_list path =
  display 1 (fun () -> Format.printf "Path %s added@." path);
  all_dyn_path := path :: !all_dyn_path

let add_path path =
  if check_path path then add_path_list path

(** @return retrieve all paths contains by environment variable "FRAMAC_DYN_PATH", check
   them and registered them *)
let init_paths () =
  let list_path =
    try
      let env_paths = Sys.getenv "FRAMAC_DYN_PATH" in
      Str.split (Str.regexp ":") env_paths
    with Not_found -> 
      default_dyn_path
  in
  List.iter add_path list_path

(* read_path is very similar to check_path but to check a path you must use
 * Sys.readdir and use Sys.readdir after. To prevent two use of Sys.readdir, I
 * introduce this function to check path and read path at the same time.
*)
(** @return read path and check error in the same times *)
let read_path path= 
  try Array.to_list (Sys.readdir path)
  with Sys_error s -> catch_sysreaddir s; []

(* ************************************************************************* *)
(** {2 Registering module} *)
(* ************************************************************************* *)

let hashtbl_module : (string, unit) Hashtbl.t = Hashtbl.create 97  

(** @return register name of module to avoid loops with recursive functions *)
let register_module name =
  Hashtbl.add hashtbl_module name () 

let is_loaded name =
  Hashtbl.mem hashtbl_module name
 
(* ************************************************************************* *)
(** {2 Loading of dynamic modules} *)
(* ************************************************************************* *)

(* Distinction between native and bytecode versions *)
let name_extension = if MyDynlink.is_native then ".cmxs" else ".cmo"

(** @return say if a module, which have the given name, is in the given path. A
    module is consider in a path if you can retrieve his .cmo or .cmxs *)
let is_module_in_path name path=
  let list_of_files= read_path path in
  let regexp_file= Str.regexp (name ^ "\\"^name_extension^"$") in 
  List.exists (fun file -> Str.string_match regexp_file file 0) list_of_files
 
(** Loads the module [name_module] in [path] if it is not already loaded. *) 
let include_module_path path name_module = 
  if not(is_loaded name_module) then
    let file = Filename.concat path (name_module ^ name_extension ) in
    try 
      register_module name_module;
      display 1 (fun () -> Format.printf "@;<1 2>@[<v 0>%s..." name_module);
      MyDynlink.loadfile file; 
      display 1 (fun () -> Format.printf "@;<0 0>... dynamically loaded@]")
    with 
    | MyDynlink.Error e ->
        Hashtbl.remove hashtbl_module name_module;
	CilE.log_once "[Dynamic] Warning: cannot load %s (%s)" file (MyDynlink.error_message e)
    | Sys_error s ->
        Hashtbl.remove hashtbl_module name_module;
	CilE.log_once "[Dynamic] Warning: cannot load %s (%s)" file s 
    | End_of_file ->
        Hashtbl.remove hashtbl_module name_module;
	CilE.log_once "[Dynamic] Warning: cannot load %s (unexpected end of file)" file
	  

(** @return loads a module with the given name *)
let include_module name_module = 
  List.iter 
    (fun path -> 
       if is_module_in_path name_module path then 
         include_module_path path name_module)
    !all_dyn_path

(** @return filter files,with the given filter, in the given path. Give all
    files with their path. *)
let filter_files filter path =
    let list_of_files = read_path path in
    let files_matched = List.filter filter list_of_files in
    let files_matched_sorted = List.sort (String.compare) files_matched in
    List.map (fun m -> (Filename.chop_extension m,path)) files_matched_sorted

(**@return load all module which match with the given filter *) 
let include_modules_filtered filter =
  let add_module liste path = filter_files filter path @ liste in 
  let list_module = List.fold_left add_module [] !all_dyn_path  in 
  List.iter (fun (n,p) -> include_module_path p n) list_module

let filter_gui x = 
  Str.string_match (Str.regexp (".+_gui\\"^name_extension^"$")) x 0 

let filter_no_gui x = 
  Str.string_match (Str.regexp (".+\\"^name_extension^"$")) x 0 
  && not (filter_gui x)

let include_all_no_gui_module () =
  display 1 (fun () -> debug_include "dynamic modules");  
  include_modules_filtered filter_no_gui;
  display 1 (fun () -> Format.printf "@;<0 0>@]")
 
(* Load  all gui module and after all no gui module *)
let include_all_gui_module () =
  display 1 (fun () -> debug_include "dynamic gui modules");
  include_modules_filtered filter_gui;
  display 1 (fun () -> Format.printf "@;<0 0>@]");
  include_all_no_gui_module ()

(* ************************************************************************* *)
(** {2 Registering of static modules} *)
(* ************************************************************************* *)
 
(* To prevent the loading of static modules already linked, I register all name
 * of modules. So, those modules are considered by the system as already loaded
 * and the system cannot allow to loading a module twice *) 
(** @return registered all static and static gui modules *) 
let register_all_static_module () =
  let register_static name =
    if not (is_loaded name) then begin
      register_module name;
      display 2 (fun  () -> Format.printf "@;<1 2>%s registered" name) 
    end
  in
  display 1 (fun () -> debug_include "static modules");
  List.iter register_static Version.static_plugins;
  display 1 (fun () -> Format.printf "@;<0 0>@]");
  display 1 (fun () -> debug_include "static gui modules");
  List.iter register_static Version.static_gui_plugins;
  display 1 (fun () -> Format.printf "@;<0 0>@]")
  

(* ************************************************************************* *)
(** {2 Registering functions} *)
(* ************************************************************************* *)

exception Invalid_Name of string

let tbl = FunTbl.create 97
    
let register name p f =
  display 2 (fun () -> Format.printf "@;<1 2>registering function: %s" name);
  FunTbl.register tbl name p f
    
(** @ return "name_of_module" with the given name which must have the form
    "name_of_module.name_of_function"
    @ raise Invalid_Name  if the given name is not
    "name_of_module.name_of_function" *)
let get_module = 
    let point_regexp = Str.regexp "\\." in 
    fun name -> 
      let mod_and_func_list = Str.bounded_split point_regexp name 2
      in if List.length (mod_and_func_list) <> 2
      then raise (Invalid_Name name)
      else List.hd mod_and_func_list
 
let apply name sign=
  let name_module = get_module name in
  include_module name_module;
  FunTbl.apply tbl name sign
    
(* 
(** Catch Registrering exception and Dynamic exception*)
let main_error f = fun () -> 
  try f () 
  with
  | FunTbl.Not_Registered s -> Format.eprintf "Not_registered : %s@." s 
  | FunTbl.Incompatible_Type s  -> Format.eprintf "Incompatible_Type : %s@." s
  | Invalid_Name s ->  Format.eprintf "Invalid_Name : %s@." s    
*)
    
let include_all_module = ref do_nothing
let include_module = function_available include_module
let include_all_gui_module = function_available include_all_gui_module
let include_all_no_gui_module = function_available include_all_no_gui_module 

(* ************************************************************************* *)
(** {2 Command line options} *)
(* ************************************************************************* *)

let () =
  if is_dynlink_available then begin
    (* Initialisation *)
    init_paths ();
    register_all_static_module ();
    MyDynlink.init ();
    MyDynlink.allow_unsafe_modules true;
    (* Registering *)
    include_all_module:= include_all_no_gui_module;
  end

module Main = struct
  module Old = Hook.Make(struct end)
  let extend = Extlib.deprecated "Dynamic.Main.extend" Old.extend
  let apply =  Old.apply
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
