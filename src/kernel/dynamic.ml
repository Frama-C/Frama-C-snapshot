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

(* ************************************************************************* *)
(** {2 Global variables for paths} *)
(* ************************************************************************* *)

let no_default = ref false

let set_default b = no_default := not b

let plugin_dirs = Str.split (Str.regexp ",[ ]*") Config.plugin_dir

let default_path () =
  match !no_default, !Config.is_gui with
  | true, _ -> []
  | false, true ->
    List.fold_left (fun acc d -> d :: (d ^ "/gui") :: acc) [] plugin_dirs
  | false, false -> plugin_dirs

let all_path = ref []
let bad_path : string list ref = ref []

(* ************************************************************************* *)
(** {2 Debugging} *)
(* ************************************************************************* *)

open Cmdline.Kernel_log
let dkey = register_category "dynamic_loading"

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

(* To determine if Dynlink works on this OCaml version  *)
let too_old_for_dynlink =
  try
    ignore (Dynlink_common_interface.init ());
    false
  with Dynlink_common_interface.Unsupported_Feature _ ->
    true

let is_dynlink_available =
  not (too_old_for_dynlink && Dynlink_common_interface.is_native)

(* apply [f] to [x] iff dynlink is available *)
let dynlink_available f x = if is_dynlink_available then f x

(* ************************************************************************* *)
(** {2 Dependency graph} *)
(* ************************************************************************* *)

module Dep_graph = Graph.Imperative.Digraph.Concrete(Datatype.String)
let plugin_dependencies = Dep_graph.create ()
let add_dependencies ~from p = Dep_graph.add_edge plugin_dependencies from p

(* debugging purpose only *)
let print_graph fmt =
  let module G = 
	Graph.Graphviz.Dot
	  (struct
	    include Dep_graph
	    let graph_attributes _ = []
	    let default_vertex_attributes _ = []
	    let vertex_name s = s
	    let vertex_attributes _ = []
	    let get_subgraph _ = None
	    let default_edge_attributes _ = []
	    let edge_attributes _ = []
	   end)
  in
  G.fprint_graph fmt plugin_dependencies

(* ************************************************************************* *)
(** {2 Paths} *)
(* ************************************************************************* *)

(** @return true iff [path] is a readable directory *)
let check_path ?(error=true) path =
  try ignore (Sys.readdir path); true
  with Sys_error s -> if error then catch_sysreaddir s; false

let rec init_paths =
  let todo = ref true in
  fun () ->
    if !todo then begin
      todo := false;
      List.iter (fun s -> ignore (add_path s)) (default_path ())
    end

(** Display debug message and add a path to list of search path *)
and add_path_list path =
  feedback ~dkey
    "dynamic plug-ins are now searched inside directory `%s'." path;
  init_paths ();
  all_path := path :: !all_path

and add_path path =
  (* the lazyness of && is used below *)
  if not (List.mem path !all_path)
    && not (List.mem path !bad_path)
    && check_path path
  then begin
    add_path_list path;
    (* in GUI mode, try to load the GUI plug-ins before the standard ones *)
    if !Config.is_gui then begin
      let gui = path ^ "/gui" in
      if check_path gui then add_path_list gui
    end;
    true
  end else
    false

(* read_path is very similar to check_path but to check a path you must use
   Sys.readdir and use Sys.readdir after. To prevent two use of Sys.readdir, I
   introduce this function to check path and read path at the same time. *)
(** @return read path and check error in the same times *)
let read_path path=
  (* sorting ensures a deterministic results of listing directories *)
  try List.sort String.compare (Array.to_list (Sys.readdir path))
  with Sys_error s -> catch_sysreaddir s; []

(* ************************************************************************* *)
(** {2 Loaded files} *)
(* ************************************************************************* *)

module Loading_error_messages: sig
  val add: 
    string (* name *) -> string (* message *) -> string (* detail *) -> unit
  val print: unit -> unit
end = struct

  let tbl = Datatype.String.Hashtbl.create 7

  let add name msg details =
    let t =
      try Datatype.String.Hashtbl.find tbl msg
      with Not_found ->
        let t = Datatype.String.Hashtbl.create 7 in
        Datatype.String.Hashtbl.add tbl msg t;
        t
    in
    Datatype.String.Hashtbl.replace t name details

  let print () =
    let once = true in
    Datatype.String.Hashtbl.iter
      (fun msg tbl ->
         let len = Datatype.String.Hashtbl.length tbl in
         assert (len > 0);
         if len = 1 then
           Datatype.String.Hashtbl.iter
             (fun name details ->
                let append fmt =
                  if verbose_atleast 2 then
                    Format.fprintf fmt " The exact failure is: %s." details
                in
                warning ~once ~append "cannot load plug-in `%s' (%s)." name msg)
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
             let l =
               Datatype.String.Hashtbl.fold
                 (fun n d acc -> (n, d) :: acc) tbl []
             in
             List.iter print (List.sort Extlib.compare_basic l)
           in
           warning ~once ~append "cannot load %d plug-ins (%s).@\n" len msg)
      tbl;
    Datatype.String.Hashtbl.clear tbl

end

module Modules : sig
  val register_once: string -> bool
  val unregister: string -> unit
  val mem: string -> bool
end = struct

  let module_names = ref Datatype.String.Set.empty
  let forbidden_names = ref Datatype.String.Set.empty

  let add s = module_names := Datatype.String.Set.add s !module_names
  let disable s = forbidden_names := Datatype.String.Set.add s !forbidden_names

  let () =
    List.iter add Config.static_plugins;
    List.iter (fun s -> add (s ^ "_gui")) Config.static_gui_plugins;
    List.iter disable Config.compilation_unit_names

  let register_once s =
    if Datatype.String.Set.mem s !module_names then
      false
    else begin
      if Datatype.String.Set.mem s !forbidden_names then begin
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

  let unregister s = module_names := Datatype.String.Set.remove s !module_names

  let mem s = Datatype.String.Set.mem s !module_names

end

let is_plugin_present = Modules.mem

(* ************************************************************************* *)
(** {2 Loading of dynamic modules} *)
(* ************************************************************************* *)

exception Unloadable of string

(* Distinction between native and bytecode versions *)
let object_file_extension =
  if Dynlink_common_interface.is_native then ".cmxs" else ".cm[oa]"

let dynlink_file path module_name =
  let error msg details =
    Modules.unregister module_name;
    Loading_error_messages.add (String.capitalize module_name) msg details
  in
  let file =
    if Dynlink_common_interface.is_native then
      Filename.concat path (module_name ^ object_file_extension)
    else begin
      let cmo = Filename.concat path (module_name ^ ".cmo") in
      let cma = Filename.concat path (module_name ^ ".cma") in
      if Sys.file_exists cma then cma else cmo
    end
  in
  try
    feedback ~dkey "loading plug-in `%s'." (String.capitalize module_name);
    Dynlink_common_interface.loadfile file
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
  | Unloadable s ->
    error "incompatible with current set-up" s
  | Log.AbortError _ | Log.AbortFatal _ | Log.FeatureRequest _ as e ->
    raise e
  | e ->
    fatal "unexpected exception %S" (Printexc.to_string e)

let load_module_from_unknown_path name =
  if Modules.register_once name then begin
    Modules.unregister name;
    let regexp =
      Str.regexp_case_fold (name ^ "\\" ^ object_file_extension ^ "$")
    in
    let check_path path =
      let files= read_path path in
      List.exists (fun file -> Str.string_match regexp file 0) files
    in
    let paths = !all_path in
    let tried = ref false in
    List.iter
      (fun p -> 
	if check_path p then begin 
	  tried := true;
	  if Modules.register_once name then dynlink_file p name
	end) 
      paths;
    if not !tried then begin
      Modules.unregister name;
      Loading_error_messages.add 
	name
	"plug-in not found" 
	(match paths with
	| [] -> "no specified directory"
	| [ p ] ->
	  Pretty_utils.sfprintf "plug-in not found in directory %s" p
	| _ :: _ ->
	  Pretty_utils.sfprintf "plug-in not found in directories %a"
	    (Pretty_utils.pp_list Format.pp_print_string)
	    paths);
    end;
    Loading_error_messages.print ();
  end

let extract_filename f =
  try Filename.chop_extension f with Invalid_argument _ -> f

let load_module f =
  init_paths ();
  let load f =
    let name = String.capitalize (Filename.basename (extract_filename f)) in
    let dir = Filename.dirname f in
    if dir = Filename.current_dir_name && Filename.is_implicit f then
      load_module_from_unknown_path (String.capitalize f)
    else
      if Modules.register_once name then dynlink_file dir name;
    Loading_error_messages.print ()
  in
  dynlink_available load f

let load_script =
  let load f =
    let name = extract_filename f in
    let dir = Filename.dirname f in
    let ml_name = name ^ ".ml" in
    let mk_name ext = 
      dir ^ "/" ^ String.capitalize (Filename.basename name) ^ ext 
    in
    let gen_name = 
      mk_name (if Dynlink_common_interface.is_native then ".cmxs" else ".cmo")
    in
    let cmd =
      Format.sprintf "%s -w Ly -warn-error A -I %s%s%t -I %s %s"
        (if Dynlink_common_interface.is_native then
           Config.ocamlopt ^ " -shared -o " ^ gen_name
         else
            Config.ocamlc ^ " -c -o " ^ gen_name)
        Config.libdir
        (if !Config.is_gui then " -I +lablgtk2"
         else "")
        (fun () -> List.fold_left (fun acc s -> " -I " ^ s ^ acc) "" !all_path)
        dir
        ml_name
    in
    feedback ~dkey "executing command `%s'." cmd;
    let code = Sys.command cmd in
    if code <> 0 then abort "command `%s' failed." cmd
    else begin
      let extended = add_path "." in
      load_module name;
      if extended then begin
	match !all_path with
	| [] -> assert false (* contains at least '.', see def of [extended] *)
	| _ :: l -> all_path := l
      end;
      let cleanup () =
        feedback ~dkey "removing files generated when compiling `%s'." ml_name;
        Extlib.safe_remove gen_name (* .cmo or .cmxs *);
        Extlib.safe_remove (mk_name ".cmi");
        if Dynlink_common_interface.is_native then begin
          Extlib.safe_remove (mk_name ".o");
          Extlib.safe_remove (mk_name ".cmx")
        end
      in
      at_exit cleanup
    end
  in
  dynlink_available load

let plugins_of_dir d =
  let filter f =
    Str.string_match (Str.regexp (".+\\" ^ object_file_extension ^ "$")) f 0
  in
  let files = read_path d in
  let files = List.filter filter files in
  List.map Filename.chop_extension files

let load_dir d =
  let load f = if Modules.register_once f then dynlink_file d f in
  let modules = plugins_of_dir d in
  (* order of loading inside a directory remains system-independent *)
  List.iter load (List.sort String.compare modules)

let build_dependency_graph () =
  List.iter
    (fun d ->
      let dir = d ^ "/dependencies" in
      if Sys.file_exists dir && Sys.is_directory dir then begin
	load_dir dir;
	Loading_error_messages.print ()
      end)
    plugin_dirs

let load_all_modules () =
  init_paths ();
  (* build the plug-in dependency graph *)
  let add_vertex dir = 
    let modules = plugins_of_dir dir in
    List.iter (Dep_graph.add_vertex plugin_dependencies) modules
  in
  List.iter add_vertex !all_path;
  if not !no_default then build_dependency_graph ();
  debug ~level:2 ~dkey "@[plug-in dependency graph:@ %t@]" print_graph;
  (* load the plug-ins by following the dependencies *)
  let module T = Graph.Topological.Make_stable(Dep_graph) in
  T.iter load_module_from_unknown_path plugin_dependencies;
  Loading_error_messages.print ()

(* ************************************************************************* *)
(** {2 Registering and accessing dynamic values} *)
(* ************************************************************************* *)

module Tbl = Type.String_tbl(struct type 'a t = 'a end)
let dynamic_values = Tbl.create 97
let comments_fordoc = Hashtbl.create 97

let register ?(comment="") ~plugin name ty ~journalize f =
  if Cmdline.use_type then begin
    debug ~level:5 "registering dynamic function %s" name;
    let f =
      if journalize then
        let comment fmt =
          Format.fprintf fmt
            "@[<hov>Applying@;dynamic@;functions@;%S@;of@;type@;%s@]"
            name
            (Type.name ty)
        in
        let jname =
          Format.fprintf
            Format.str_formatter
            "@[<hv 2>Dynamic.get@;~plugin:%S@;%S@;%t@]"
            plugin name
            (Type.pp_ml_name ty Type.Call);
          Format.flush_str_formatter ()
        in
        Journal.register jname ty ~is_dyn:true ~comment f
      else
        f
    in
    let key = plugin ^ "." ^ name in
    Tbl.add dynamic_values key ty f;
    if comment <> "" then Hashtbl.add comments_fordoc key comment ;
    f
  end else
    f

exception Incompatible_type = Tbl.Incompatible_type
exception Unbound_value = Tbl.Unbound_value

let get ~plugin name ty =
  if Cmdline.use_type then begin
    if plugin <> "" then load_module_from_unknown_path plugin;
    Tbl.find dynamic_values (plugin ^ "." ^ name) ty
  end else
    failwith 
      (Pretty_utils.sfprintf "cannot access value %s in the 'no obj' mode" name)

let iter f = Tbl.iter f dynamic_values
let iter_comment f = Hashtbl.iter f comments_fordoc 

(* ************************************************************************* *)
(** {2 Specialised interface for parameters} *)
(* ************************************************************************* *)

module Parameter = struct

  module type Common = sig
    type t
    val get: string -> unit -> t
    val set: string -> t  -> unit
    val clear: string -> unit -> unit
    val is_set: string -> unit -> bool
    val is_default: string  -> unit -> bool
  end

  let get_name functor_name fct_name option_name =
    Format.sprintf "Dynamic.Parameter.%s.%s %S"
      functor_name fct_name option_name

  let get_parameter option_name = 
    get ~plugin:"" option_name Typed_parameter.ty

  let get_state option_name =
    let prm = get ~plugin:"" option_name Typed_parameter.ty in
    State.get prm.Typed_parameter.name

  let apply modname name s ty1 ty2 =
    get ~plugin:""  (get_name modname s name) (Datatype.func ty1 ty2)

  module Common(X: sig type t val modname:string val ty: t Type.t end ) = struct
    type t = X.t
    let ty = X.ty
    let get name = apply X.modname name "get" Datatype.unit ty
    let set name = apply X.modname name "set" ty Datatype.unit
    let clear name = apply X.modname name "clear" Datatype.unit Datatype.unit
    let is_set name = apply X.modname name "is_set" Datatype.unit Datatype.bool
    let is_default name =
      apply X.modname name "is_default" Datatype.unit Datatype.bool
  end

  module Bool = struct
    include Common
      (struct type t = bool let ty = Datatype.bool let modname = "Bool"end )
    let on name = apply "Bool" name "on" Datatype.unit Datatype.unit
    let off name = apply "Bool" name "off" Datatype.unit Datatype.unit
  end

  module Int = struct
    include Common
      (struct type t = int let ty = Datatype.int let modname = "Int" end )
    let incr name = apply "Int" name "incr" Datatype.unit Datatype.unit
  end

  module String =
    Common
      (struct
        type t = string
        let ty = Datatype.string
        let modname = "String"
       end)

  module StringSet = struct
    include Common
      (struct include Datatype.String.Set let modname = "StringSet" end)
    let add name = apply "StringSet" name "add" Datatype.string Datatype.unit
    let remove name =
      apply "StringSet" name "remove" Datatype.string Datatype.unit
    let is_empty name =
      apply "StringSet" name "is_empty" Datatype.unit Datatype.bool
    let iter name =
      apply "StringSet" name "iter"
        (Datatype.func Datatype.string Datatype.unit) Datatype.unit
  end

  module StringList = struct
    include Common
      (struct
        include Datatype.List(Datatype.String)
        let modname = "StringList"
       end)
    let add name = apply "StringList" name "add" Datatype.string Datatype.unit
    let append_before name = apply "StringList" name "append_before"
      (Datatype.list Datatype.string) Datatype.unit
    let append_after name = apply "StringList" name "append_after"
      (Datatype.list Datatype.string) Datatype.unit
    let remove name =
      apply "StringList" name "remove" Datatype.string Datatype.unit
    let is_empty name =
      apply "StringList" name "is_empty" Datatype.unit Datatype.bool
    let iter name =
      apply "StringList" name "iter"
        (Datatype.func Datatype.string Datatype.unit) Datatype.unit
  end

(*
    module IndexedVal(X: sig val ty_name: string end) = struct
      include Common(struct type t = string let ty = string end)
      type value = Type.ty
      let ty = Type.get_abstract X.ty_name
      let add_choice name =
        StringTbl.find tbl (name ^ ".add_choice") (func string (func ty unit))
      let get_val name =
        StringTbl.find tbl (name ^ ".get_val") (func unit ty) ()
    end
      *)

end

(* ************************************************************************* *)
(** {2 Initialisation} *)
(* ************************************************************************* *)

let () =
  if is_dynlink_available then begin
    Dynlink_common_interface.init ();
    Dynlink_common_interface.allow_unsafe_modules true;
    Cmdline.run_during_extending_stage load_all_modules
  end;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
