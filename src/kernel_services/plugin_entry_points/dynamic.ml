(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Debugging                                                          --- *)
(* -------------------------------------------------------------------------- *)

module Klog = Cmdline.Kernel_log
let dkey = Klog.register_category "dynlink"

let error ~name ~message ~details =
  Klog.error "cannot load plug-in '%s': %s%t" name message
    (fun fmt ->
       if details <> "" && Klog.verbose_atleast 2 then
         Format.fprintf fmt "@\nDetails: %s" details)
    
(* -------------------------------------------------------------------------- *)
(* --- Dynlink Common Interface & Dynamic Library                         --- *)
(* -------------------------------------------------------------------------- *)

exception Unloadable of string

module Tbl = Type.String_tbl(struct type 'a t = 'a end)
module Dynlib = FCDynlink

let dynlib_init = ref false
let dynlib_init () =
  if not !dynlib_init then
    begin
      dynlib_init := true ;
      Dynlib.init () ;
      Dynlib.allow_unsafe_modules true ;
    end

exception Incompatible_type = Tbl.Incompatible_type
exception Unbound_value = Tbl.Unbound_value

let dynlib_error name = function
  | Dynlib.Unsupported_Feature s ->
      error ~name ~message:"dynamic loading not supported" ~details:s ;
  | Dynlib.Error e ->
      error ~name ~message:"cannot load module" ~details:(Dynlib.error_message e) ;
  | Sys_error _ as e ->
      error ~name ~message:"system error" ~details:(Printexc.to_string e)
  | Unloadable details ->
      error ~name ~message:"incompatible with current set-up" ~details
  (* the three next errors may be raised in case of incompatibilites with
     another plug-in *)
  | Incompatible_type s ->
      error ~name ~message:"code incompatibility" ~details:s
  | Unbound_value s ->
      error ~name ~message:"code incompatibility" ~details:("unbound value " ^ s)
  | Type.No_abstract_type s ->
      error ~name ~message:"code incompatibility" ~details:("unbound abstract type " ^ s)
  | Log.AbortError _ | Log.AbortFatal _ | Log.FeatureRequest _ as e ->
      raise e
  | e ->
      error ~name ~message:("unexpected exception: " ^ Printexc.to_string e)
        ~details:(Printexc.get_backtrace ())

let dynlib_module name file =
  Klog.feedback ~dkey "Loading module '%s' from '%s'." name file ;
  try
    dynlib_init () ;
    Dynlib.loadfile file ;
  with error ->
    dynlib_error name error

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let split_word = Str.(split (regexp ":"))
let split_ext p =
  try
    let k = String.rindex p '.' in
    let d = try String.rindex p '/' with Not_found -> 0 in
    (* check for '.' belonging to directory or not *)
    if d <= k then
      let n = String.length p in
      String.sub p 0 k , String.sub p k (n-k)
    else p , ""
  with Not_found -> p , ""

let is_package = 
  let pkg = Str.regexp "[a-z-_]+$" in
  fun name -> Str.string_match pkg name 0

let is_meta =
  let meta = Str.regexp "META.frama-c-[a-z-_]+$" in
  fun name -> Str.string_match meta name 0

let is_dir d = Sys.file_exists d && Sys.is_directory d 

let is_file base ext =
  let file = base ^ ext in
  if Sys.file_exists file then Some file else None

let is_object base =
  if Dynlib.is_native then is_file base ".cmxs" else
    match is_file base ".cma" with
    | Some _ as file -> file
    | None -> is_file base ".cmo"

(* -------------------------------------------------------------------------- *)
(* --- Package Loading                                                    --- *)
(* -------------------------------------------------------------------------- *)

let packages = Hashtbl.create 64

let () = List.iter (fun p -> Hashtbl.add packages p ()) Config.library_names

let missing pkg = not (Hashtbl.mem packages pkg)

let once pkg =
  if Hashtbl.mem packages pkg then false
  else ( Hashtbl.add packages pkg () ; true )

exception ArchiveError of string

let predicates = if Dynlib.is_native then [ "plugin" ] else [ "byte" ]
let load_archive pkg base file =
  let path =
    try Findlib.resolve_path ~base file
    with Not_found ->
      let msg = Printf.sprintf "archive '%s' not found in '%s'" file base in
      raise (ArchiveError msg)
  in dynlib_module pkg path

let mem_package pkg =
  try ignore (Findlib.package_directory pkg) ; true
  with Findlib.No_such_package _ -> false

let load_packages pkgs =
  Klog.debug ~dkey "trying to load %a"
    (Pretty_utils.pp_list ~sep:"@, " Format.pp_print_string) pkgs;
  try
    let pkgs = List.filter missing pkgs in
    List.iter
      begin fun pkg ->
        if once pkg then
          let base = Findlib.package_directory pkg in
          let predicates =
            if !Config.is_gui then "gui"::predicates else predicates in
          let archive = Findlib.package_property predicates pkg "archive" in
          let archives = split_word archive in
          if archives = [] then
            Klog.warning "no archive to load for package '%s'" pkg
          else
            List.iter (load_archive pkg base) archives
      end
      (Findlib.package_deep_ancestors predicates pkgs)
  with
  | Findlib.No_such_package(pkg,details) ->
      Klog.error "[findlib] package '%s' not found (%s)" pkg details
  | Findlib.Package_loop pkg ->
      Klog.error "[findlib] cyclic dependencies for package '%s'" pkg
  | ArchiveError msg ->
      Klog.error "[findlib] %s" msg

(* -------------------------------------------------------------------------- *)
(* --- Load Objects                                                       --- *)
(* -------------------------------------------------------------------------- *)

let load_path = ref [] (* initialized by load_modules *)

let load_script base =
  Klog.feedback ~dkey "compiling script '%s.ml'" base ;
  let cmd = Buffer.create 80 in
  let fmt = Format.formatter_of_buffer cmd in
  begin
    if Dynlib.is_native then
      Format.fprintf fmt "%s -shared -o %s.cmxs" Config.ocamlopt base
    else
      Format.fprintf fmt "%s -c" Config.ocamlc ;
    Format.fprintf fmt " -w Ly -warn-error A -I %s" Config.libdir ;
    if !Config.is_gui then Format.pp_print_string fmt " -I +lablgtk" ;
    List.iter (fun p -> Format.fprintf fmt " -I %s" p) !load_path ;
    Format.fprintf fmt " %s.ml" base ;
    Format.pp_print_flush fmt () ;
    let cmd = Buffer.contents cmd in
    Klog.feedback ~dkey "running '%s'" cmd ;
    begin
      let res = Sys.command cmd in
      if res <> 0
      then Klog.error "compilation of '%s.ml' failed" base
      else
        let pkg = Filename.basename base in
        if Dynlib.is_native then
          dynlib_module pkg (base ^ ".cmxs")
        else
          dynlib_module pkg (base ^ ".cmo") ;
    end ;
    let erase = Printf.sprintf "rm -f %s.cm* %s.o" base base in
    Klog.feedback ~dkey "running '%s'" erase ;
    let st = Sys.command erase in
    if st <> 0 then
      Klog.warning "Error when cleaning '%s.[o|cm*]' files" base ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Command-Line Entry Points                                          --- *)
(* -------------------------------------------------------------------------- *)

let scan_directory pkgs dir =
  Klog.feedback ~dkey "Loading directory '%s'" dir ;
  try
    let content = Sys.readdir dir in
    Array.sort String.compare content ;
    Array.iter
      (fun name ->
         if is_meta name then
           (* name starts with "META.frama-c-" *)
           let pkg = String.sub name 5 (String.length name - 5) in
           pkgs := pkg :: !pkgs
      ) content ;
  with Sys_error error ->
    Klog.error "impossible to read '%s' (%s)" dir error
      
let load_plugin_path path =
  begin
    let add_dir ~user d ps =
      if is_dir d then d::ps else
        ( if user then Klog.warning "cannot load '%s' (not a directory)" d
        ; ps ) in
    Klog.debug ~dkey "plugin_dir: %s" (String.concat ":" Config.plugin_dir);
    load_path :=
      List.fold_right (add_dir ~user:true) path
        (List.fold_right (add_dir ~user:false) Config.plugin_dir []) ;
    let pkgs = ref [] in
    List.iter (scan_directory pkgs) !load_path ;
    let findlib_path = String.concat ":" !load_path in
    Klog.debug ~dkey "setting findlib path to %s" findlib_path;
    Findlib.init ~env_ocamlpath:findlib_path ();
    load_packages (List.rev !pkgs) ;
  end

let load_module m =
  let base,ext = split_ext m in
  match ext with
  | ".ml" ->
      begin
        (* force script compilation *)
        match is_file base ".ml" with
        | Some _ -> load_script base
        | None -> Klog.error "Missing source file '%s'" m
      end
  | "" | "." | ".cmo" | ".cma" | ".cmxs" ->
      begin
        (* load object or compile script or find package *)
        match is_object base with
        | Some file -> dynlib_module (Filename.basename base) file
        | None ->
            match is_file base ".ml" with
            | Some _ -> load_script base
            | None -> 
                if is_package m && mem_package m then load_packages [m]
                else
                  let fc = "frama-c-" ^ String.lowercase m in
                  if mem_package fc then load_packages [fc]
                  else Klog.error "package or module '%s' not found" m
      end
  | _ ->
      Klog.error "don't know what to do with '%s' (unexpected %s)" m ext

(* ************************************************************************* *)
(** {2 Registering and accessing dynamic values} *)
(* ************************************************************************* *)

let dynamic_values = Tbl.create 97
let comments_fordoc = Hashtbl.create 97

let register ?(comment="") ~plugin name ty ~journalize f =
  if Cmdline.use_type then begin
    Klog.debug ~level:5 "registering dynamic function %s" name;
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

let get ~plugin name ty =
  if Cmdline.use_type then
    Tbl.find dynamic_values (plugin ^ "." ^ name) ty
  else
    failwith
      (Printf.sprintf "cannot access value %s in the 'no obj' mode" name)

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

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
