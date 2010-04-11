(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* ************************************************************************* *)
(** {2 Gobal variables for paths} *)
(* ************************************************************************* *)

let no_default = ref false

let set_default b = no_default := not b

let default_path () =
  match !no_default, !Config.is_gui with
  | true, _ -> []
  | false, true ->
      (* The order is relevant: plugins are loaded
	 in reverse order of this list. *)
      [ Config.plugin_dir ; Filename.concat Config.plugin_dir "gui"]
  | false, false ->
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
     let verbose_atleast n = !Cmdline.kernel_verbose_atleast_ref n
     let debug_atleast n = !Cmdline.kernel_debug_atleast_ref n
   end)

(* Try to display an error message from the argument of a [Sys_error] exception
   raised whenever a path is incorrect.  Don't display any error for default
   path since it occurs whenever Frama-C is not installed. *)
let catch_sysreaddir s =
  (* [Sys_error] messages for path get usually the form "%s: %s" *)
  let list_arg = Str.split (Str.regexp ": ") s in
  match list_arg with
  | [ dir; error ] ->
      if not (List.mem dir (default_path ())) then begin
	warning
	  "cannot search dynamic plugins inside directory `%s' (%s)."
	  dir error;
	bad_path := dir :: !bad_path;
      end
  | [] | [ _ ] | _ :: _ :: _ :: _ ->
      raise (Sys_error s)

(* To determine if we have a ocaml version lower than 3.11 or not *)
let is_lower_than_311 =
  try
    ignore (Dynlink_common_interface.init ());
    false
  with Dynlink_common_interface.Unsupported_Feature _ ->
    true

let is_dynlink_available =
  not (is_lower_than_311 && Dynlink_common_interface.is_native)

(* apply [f] to [x] iff dynlink is available *)
let dynlink_available f x = if is_dynlink_available then f x else ()

(* ************************************************************************* *)
(** {2 Paths} *)
(* ************************************************************************* *)

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
   Sys.readdir and use Sys.readdir after. To prevent two use of Sys.readdir, I
   introduce this function to check path and read path at the same time. *)
(** @return read path and check error in the same times *)
let read_path path=
  (* sorting ensures a deterministic results of listing directories *)
  try List.sort Pervasives.compare (Array.to_list (Sys.readdir path))
  with Sys_error s -> catch_sysreaddir s; []

(* ************************************************************************* *)
(** {2 Loaded files} *)
(* ************************************************************************* *)

module Loading_error_messages: sig
  val add: string -> string -> string -> unit
  val print: unit -> unit
end = struct

  let tbl = Hashtbl.create 7

  let add name msg details =
    let t =
      try Hashtbl.find tbl msg
      with Not_found ->
	let t = Hashtbl.create 7 in
	Hashtbl.add tbl msg t;
	t
    in
    Hashtbl.replace t name details

  let print () =
    Hashtbl.iter
      (fun msg tbl ->
	 let len = Hashtbl.length tbl in
	 assert (len > 0);
	 if len = 1 then
	   Hashtbl.iter
	     (fun name details ->
		let append fmt =
		  if verbose_atleast 2 then
		    Format.fprintf fmt "The exact failure is: %s." details
		in
		warning ~append "cannot load plug-in `%s' (%s)." name msg)
	     tbl
	 else
	   let append fmt =
	     let first = ref true in
	     let print (name, details) =
	       if verbose_atleast 2 then
		 Format.fprintf fmt "%s@;(%s)@\n" name details
	       else begin
		 if !first then Format.fprintf fmt "%s" name
		 else Format.fprintf fmt ";@;%s" name;
		 first := false
	       end
	     in
	     let l = Hashtbl.fold (fun n d acc -> (n, d) :: acc) tbl [] in
	     List.iter print (List.sort Pervasives.compare l)
	   in
	   warning ~append "cannot load %d plug-ins (%s).@\n" len msg)
      tbl;
    Hashtbl.clear tbl

end

module Modules : sig
  val register: string -> bool
  val unregister: string -> unit
end = struct

  module Names = Set.Make(String)

  let module_names = ref Names.empty
  let forbidden_names = ref Names.empty

  let add s = module_names := Names.add s !module_names
  let disable s = forbidden_names := Names.add s !forbidden_names

  let () =
    List.iter add Config.static_plugins;
    List.iter (fun s -> add (s ^ "_gui")) Config.static_gui_plugins;
    List.iter disable Config.compilation_unit_names

  let register s =
    if Names.mem s !module_names then
      false
    else begin
      if Names.mem s !forbidden_names then begin
	Loading_error_messages.add
	  (String.capitalize s)
	  "forbidden plug-in name"
	  "name already used by a Frama-C kernel file";
	false
      end else begin
	add s;
	true
      end
    end

  let unregister s = module_names := Names.remove s !module_names

end

(* ************************************************************************* *)
(** {2 Loading of dynamic modules} *)
(* ************************************************************************* *)

(* Distinction between native and bytecode versions *)
let object_file_extension =
  if Dynlink_common_interface.is_native then ".cmxs" else ".cm[oa]"

(** @return say if a module, which have the given name, is in the given path. A
    module is consider in a path if you can retrieve his .cmo or .cmxs *)
let is_module_in_path name path=
  let list_of_files= read_path path in
  let regexp_file=
    Str.regexp (name ^ "\\" ^ object_file_extension ^ "$")
  in
  List.exists (fun file -> Str.string_match regexp_file file 0) list_of_files

(** Loads the module [name_module] in [path] if it is not already loaded. *)
let include_module_path path module_name =
  let file =
    if Dynlink_common_interface.is_native then
      Filename.concat path (module_name ^ object_file_extension)
    else begin
      let cmo = Filename.concat path (module_name ^ ".cmo") in
      let cma = Filename.concat path (module_name ^ ".cma") in
      if Sys.file_exists cma then cma else cmo
    end
  in
  let error msg details =
    Modules.unregister module_name;
    Loading_error_messages.add (String.capitalize module_name) msg details
  in
  if Modules.register module_name then begin
    feedback ~level:3 "loading file %S" file;
    try
      Dynlink_common_interface.loadfile file;
    with
    | Dynlink_common_interface.Error e ->
	(match e with
	 | Dynlink_common_interface.Not_a_bytecode_file s ->
	     error "not a bytecode file" s
	 | Dynlink_common_interface.File_not_found s ->
	     error "file not found" s
	 | Dynlink_common_interface.Inconsistent_import _
	 | Dynlink_common_interface.Linking_error _
	 | Dynlink_common_interface.Corrupted_interface _
	 | Dynlink_common_interface.Cannot_open_dll _
	 | Dynlink_common_interface.Inconsistent_implementation _
	 | Dynlink_common_interface.Unavailable_unit _ ->
	     error
	       (Format.sprintf "incompatible with %s" Config.version)
	       (Dynlink_common_interface.error_message e)
	 | Dynlink_common_interface.Unsafe_file -> assert false)
    | Sys_error _ as e ->
	error "system error" (Printexc.to_string e)
    | e ->
	fatal "unexpected exception %S" (Printexc.to_string e)
  end

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
    include_module_path dir name;
    Loading_error_messages.print ()
  in
  dynlink_available load

let load_script =
  let load f =
    let name = extract_filename f in
    let dir = Filename.dirname f in
    let ml_name = name ^ ".ml" in
    let gen_name =
      if Dynlink_common_interface.is_native then name ^ ".cmxs"
      else name ^ ".cmo"
    in
    let cmd =
      Format.sprintf "%s -w Ly -warn-error A -I %s%t -I %s %s"
	(if Dynlink_common_interface.is_native then
	   Config.ocamlopt ^ " -shared -o " ^ gen_name
	 else
	   Config.ocamlc ^ " -c")
	Config.libdir
	(fun () -> List.fold_left (fun acc s -> " -I " ^ s ^ acc) "" !all_path)
	dir
	ml_name
    in
    feedback ~level:2 "executing command `%s'" cmd;
    let code = Sys.command cmd in
    if code <> 0 then abort "command `%s' failed" cmd
    else begin
      load_module name;
      let cleanup () =
	feedback ~level:2 "Removing files generated when compiling %S" ml_name;
	Extlib.safe_remove gen_name (* .cmo or .cmxs *);
	Extlib.safe_remove (name ^ ".cmi");
	if Dynlink_common_interface.is_native then begin
	  Extlib.safe_remove (name ^ ".o");
	  Extlib.safe_remove (name ^ ".cmx")
	end
      in
      at_exit cleanup
    end
  in
  dynlink_available load

let load_all_modules =
  let filter f =
    Str.string_match (Str.regexp (".+\\" ^ object_file_extension ^ "$")) f 0
  in
  let load_dir d =
    let files = read_path d in
    let files = List.filter filter files in
    let modules = List.map Filename.chop_extension files in
    (* order of loading inside a directory remains system-independent *)
    List.iter (include_module_path d) (List.sort String.compare modules)
  in
  let load_all () =
    init_paths ();
    (* order of directory matters for the GUI *)
    List.iter load_dir !all_path;
    Loading_error_messages.print ()
  in
  dynlink_available load_all

(* ************************************************************************* *)
(** {2 Registering and accessing dynamic values} *)
(* ************************************************************************* *)

let dynamic_values = Type.StringTbl.create 97

let register ~plugin name ty ~journalize f =
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
	  Format.fprintf Format.str_formatter "Dynamic.get ~plugin:%S %S %t"
	    plugin name (Type.pp_value_name ty Type.Call);
	  Format.flush_str_formatter ()
	in
	Journal.register jname ty ~is_dyn:true ~comment f
      else
	f
    in
    Type.StringTbl.add dynamic_values (plugin ^ "." ^ name) ty f
  end else
    f

let get ~plugin name ty =
  if Cmdline.use_type then begin
    include_module plugin;
    include_module (String.uncapitalize plugin);
    Type.StringTbl.find dynamic_values (plugin ^ "." ^ name) ty
  end else
    abort "cannot access value %s in the 'no obj' mode" name

(* ************************************************************************* *)
(** {2 Initialisation} *)
(* ************************************************************************* *)

let () =
  if is_dynlink_available then begin
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
compile-command: "LC_ALL=C make -C ../.."
End:
*)
