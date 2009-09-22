(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: dynamic.ml,v 1.20 2009-02-06 08:33:56 uid568 Exp $ *)

(* ************************************************************************* *)
(** {2 Gobal variables for paths} *)
(* ************************************************************************* *)

let default_path () =
  (*    (* [JS 2009/05/29] hack from a Benjamin's idea:
	check the binary name in order to choose the default path *)
	let bin = Sys.argv.(0) in
	let contain s target =
	List.length (Str.bounded_split (Str.regexp_string s) target 2) = 2
	in
	if contain "toplevel." bin || contain "viewer." bin then
	if !Config.is_gui () then Filename.concat "lib" "gui"
	else Filename.concat "lib" "plugins"
	else*)
  if !Config.is_gui then
    (* The order is important: plugins are loaded 
       in reverse order of this list. *)
    [ Config.plugin_dir ; Filename.concat Config.plugin_dir "gui"]
  else
    [ Config.plugin_dir ]

let all_path = ref []
let bad_path : string list ref = ref []

(* ************************************************************************* *)
(** {2 Debugging} *)
(* ************************************************************************* *)

include Log.Register
  (struct
     let channel = Log.kernel_channel_name
     let label = Log.kernel_label_name
     let verbose_atleast n = Cmdline.kernel_verbose_level >= n
     let debug_atleast n = Cmdline.kernel_debug_level >= n
   end)

(* Useful to display error for Sys.readdir function used to determine if a path
 * is correct.
 * Don't display error when default path is not created (use frama-c whitout
 * install)
 *)
(** catch all Sys_error which have "%s: %s", where %s are string, profile.*)
let catch_sysreaddir s =
  let list_arg = Str.split (Str.regexp ": ") s in
  if List.length list_arg = 2 then
    match list_arg with
    | dir :: error :: _ ->
	if not (List.mem dir (default_path ())) then begin
	  warning
	    "cannot search dynamic plugins inside directory \"%s\" (%s)."
	    dir error;
	  bad_path := dir :: !bad_path;
	end
    | _ -> assert false
  else
    raise (Sys_error s)

(* To determine if we have a ocaml version lower than 3.11 or not *)
let is_lower_than_311 =
  try ignore (Dynlink_common_interface.init ()); false
  with Dynlink_common_interface.Unsupported_Feature _ -> true

(* Function do nothing if *)
let do_nothing _ = ()
let is_dynlink_available =
  not (is_lower_than_311 && Dynlink_common_interface.is_native)
let dynlink_available f =
  if is_dynlink_available then f else do_nothing

(* ************************************************************************* *)
(** {2 Paths} *)
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
  feedback ~level:2
    "dynamic plug-ins are now also searched inside directory \"%s\"" path;
  all_path := path :: !all_path

let add_path path =
  (* the lazyness of && is used below *)
  if not (List.mem path !all_path)
    && not (List.mem path !bad_path)
    && check_path path
  then
    add_path_list path

let init_paths () = List.iter add_path (default_path ())

(* read_path is very similar to check_path but to check a path you must use
 * Sys.readdir and use Sys.readdir after. To prevent two use of Sys.readdir, I
 * introduce this function to check path and read path at the same time. *)
(** @return read path and check error in the same times *)
let read_path path=
  (* sorting ensures a deterministic results of listing directories *)
  try List.sort Pervasives.compare (Array.to_list (Sys.readdir path))
  with Sys_error s -> catch_sysreaddir s; []

(* ************************************************************************* *)
(** {2 Loading files} *)
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
let name_extension =
  if Dynlink_common_interface.is_native then ".cmxs" else ".cm[oa]"

(** @return say if a module, which have the given name, is in the given path. A
    module is consider in a path if you can retrieve his .cmo or .cmxs *)
let is_module_in_path name path=
  let list_of_files= read_path path in
  let regexp_file= Str.regexp (name ^ "\\"^name_extension^"$") in
  List.exists (fun file -> Str.string_match regexp_file file 0) list_of_files

(** Loads the module [name_module] in [path] if it is not already loaded. *)
let include_module_path path name_module =
  if not (is_loaded name_module) then
    let file = if Dynlink_common_interface.is_native then
      Filename.concat path (name_module ^ name_extension )
    else begin
      let cmo = Filename.concat path (name_module ^ ".cmo") in
      let cma = Filename.concat path (name_module ^ ".cma") in
      if Sys.file_exists cma then cma else cmo
    end
    in
    try
      register_module name_module;
      feedback ~level:3 "loading file \"%s\"" file;
      Dynlink_common_interface.loadfile file;
    with
    | Dynlink_common_interface.Error e ->
        Hashtbl.remove hashtbl_module name_module;
	warning "cannot load file \"%s\" (dynlink error \"%s\")"
	  file (Dynlink_common_interface.error_message e)
    | Sys_error s ->
        Hashtbl.remove hashtbl_module name_module;
	warning "cannot load file \"%s\" (system error \"%s\")" file s
    | End_of_file ->
        Hashtbl.remove hashtbl_module name_module;
	warning "cannot load file \"%s\" (unexpected end of file)" file

(** @return loads a module with the given name *)
let include_module name_module =
  List.iter
    (fun path ->
       if is_module_in_path name_module path then
         include_module_path path name_module)
    !all_path

let extract_filename f =
  try Filename.chop_extension f with Invalid_argument _ -> f

let load_module =
  let load f =
    let name = Filename.basename (extract_filename f) in
    let dir = Filename.dirname f in
    include_module_path dir name
  in
  dynlink_available load

let load_script =
  let load f =
    let name = extract_filename f in
    let dir = Filename.dirname f in
    let ml_name = name ^ ".ml" in
    let cmd =
      Format.sprintf "%s -w Ayz -warn-error A -I %s%t -I %s %s"
	(if Dynlink_common_interface.is_native then
	   "ocamlopt -shared -o " ^ name ^ ".cmxs"
	 else
	   "ocamlc -c")
	Config.libdir
	(fun () ->
	   List.fold_left (fun acc s -> " -I " ^ s ^ acc) "" !all_path)
	dir
	ml_name
    in
    feedback ~level:2 "executing command `%s'" cmd;
    let code = Sys.command cmd in
    if code <> 0 then error "command `%s' failed" cmd
    else load_module name
  in
  dynlink_available load

let load_all_modules =
  let filter f =
    Str.string_match (Str.regexp (".+\\" ^ name_extension ^ "$")) f 0
  in
  let load_dir d =
    let files = read_path d in
    let files = List.filter filter files in
    let modules = List.map Filename.chop_extension files in
    List.iter (fun m -> include_module_path d m) modules
  in
  let load_all () =
    init_paths ();
    List.iter load_dir !all_path
  in
  dynlink_available load_all

(* ************************************************************************* *)
(** {2 Registering static modules} *)
(* ************************************************************************* *)

(* To prevent the loading of static modules already linked, register them. *)
let register_all_static_module () =
  let register name = if not (is_loaded name) then register_module name in
  List.iter register Config.static_plugins;
  List.iter register (List.map (fun s -> s^"_gui") Config.static_gui_plugins)

(* ************************************************************************* *)
(** {2 Registering and accessing functions} *)
(* ************************************************************************* *)

exception Invalid_Name of string

let tbl = Type.StringTbl.create 97

let register name ty ~journalize f =
  if Cmdline.use_type then begin
    debug ~level:5 "registering dynamic function %s" name;
    let f =
      if journalize then
	let comment fmt =
	  Format.fprintf fmt
	    "@[<hov 2>Applying@;dynamic@;functions@;%S@;of@;type@;%s@]"
	    name
	    (Type.name ty)
	in
	let jname =
	  Format.fprintf Format.str_formatter "Dynamic.get %S %t"
	    name (Type.pp_value_name ty Type.Call);
	  Format.flush_str_formatter ()
	in
	Journal.register jname ty ~is_dyn:true ~comment f
      else
	f
    in
    Type.StringTbl.add tbl name ty f
  end else
    f

let get_module =
  let point_regexp = Str.regexp "\\." in
  fun name ->
    let mod_and_func_list = Str.bounded_split point_regexp name 2 in
    if List.length (mod_and_func_list) <> 2
    then raise (Invalid_Name name)
    else List.hd mod_and_func_list

let get name ty =
  if Cmdline.use_type then begin
    let name_module = get_module name in
    include_module name_module;
    Type.StringTbl.find tbl name ty
  end else
    abort "cannot access to the dynamic function %s in the 'no obj' mode" name

let apply x = deprecated "Dynamic.apply" ~now:"Dynamic.get" get x

(* ************************************************************************* *)
(** {2 Initialisation} *)
(* ************************************************************************* *)

let () =
  if is_dynlink_available then begin
    register_all_static_module ();
    Dynlink_common_interface.init ();
    Dynlink_common_interface.allow_unsafe_modules true;
    Cmdline.run_during_extending_stage load_all_modules
  end;

module Main = struct
  module Old = Hook.Make(struct end)
  let extend =
    deprecated "Dynamic.Main.extend" ~now:"Db.Main.extend" Old.extend
  let apply =  Old.apply
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
