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

open Odoc_module
open Odoc_info
open Odoc_html

let doc_path = ref "."
let lib_files = ref []

let add_libfiles analyse s =
  let f = Odoc_args.Intf_file s in
  lib_files :=
    (String.capitalize (Filename.chop_extension (Filename.basename s))) ::
      !lib_files;
  if analyse then Odoc_args.files := f :: !Odoc_args.files

let rec root_name s =
  let simple = Odoc_info.Name.simple s in
  let father = Odoc_info.Name.father s in
  if father = "" then simple else root_name father

let equal_module_name m s =
  let n = m.m_name in
  n = s && n = root_name n

let equal_module m1 m2 = equal_module_name m1 m2.m_name

type chapter = Chapter of int * string * string | Directory of string
let compare_chapter c1 c2 = match c1 , c2 with
    | Chapter(a,_,_) , Chapter(b,_,_) -> a-b
    | Directory a , Directory b -> compare a b
    | Chapter _ , Directory _ -> (-1)
    | Directory _ , Chapter _ -> 1

let merge3
    (s1 : 'a -> 'a -> int)
    (s2 : 'b -> 'b -> int)
    (s3 : 'c -> 'c -> int)
    (triplets : ('a * 'b * 'c) list)
    : ('a * ('b * 'c list) list) list =
  let sort3_rev s1 s2 s3 (x,y,z) (x',y',z') =
    let c = s1 x' x in
    if c <> 0 then c else
      let c = s2 y' y in
      if c <> 0 then c else
	s3 z' z
  in
  let rec merge3_rev acc triplets =
    match triplets , acc with
      | [] , _ -> acc
      | (a,b,c)::tail , (dir_a,all_a)::a_merged when a = dir_a ->
	  begin
	    match all_a with
	      | (dir_b,all_b)::b_merged when b = dir_b ->
		  merge3_rev ((dir_a,(dir_b,c::all_b)::b_merged)::a_merged) tail
	      | _ ->
		  merge3_rev ((dir_a,(b,[c])::all_a)::a_merged) tail
	  end
      | (a,b,c)::tail , merged ->
	  merge3_rev (( a , [b,[c]] )::merged) tail
  in
  merge3_rev [] (List.sort (sort3_rev s1 s2 s3) triplets)

class gen = object (self)

  inherit html as super

  val mutable memo = []

  method loaded_modules =
    match memo with
    | [] ->
      let l = List.flatten
	(List.map
	   (fun f ->
	     Odoc_info.verbose (Odoc_messages.loading f);
	     try
	       let l = Odoc_analyse.load_modules f in
	       Odoc_info.verbose Odoc_messages.ok;
	       l
	     with Failure s ->
	       prerr_endline s ;
	       incr Odoc_global.errors ;
	       []
	   )
	   !Odoc_args.load
	)
      in
      memo <- l;
      l
    | (_ :: _) as l ->
      l

  method path s =
    let name = root_name s in
    if List.exists (fun m -> m = name) !lib_files then
      "http://caml.inria.fr/pub/docs/manual-ocaml/libref/"
    else
      if List.exists (fun m -> m.m_name = name) self#loaded_modules
      then !doc_path ^ "/"
      else "./"

  method create_fully_qualified_idents_links m_name s =
    let f str_t =
      let match_s = Str.matched_string str_t in
      let rel = Odoc_info.Name.get_relative m_name match_s in
      let s_final = Odoc_info.apply_if_equal
	Odoc_info.use_hidden_modules
	match_s
	rel
      in
      if StringSet.mem match_s known_types_names then
	"<a href=\"" ^ self#path match_s ^ Naming.complete_target Naming.mark_type
	  match_s ^"\">" ^ s_final ^ "</a>"
      else
	if StringSet.mem match_s known_classes_names then
          let (html_file, _) = Naming.html_files match_s in
          "<a href=\""^ self#path html_file ^ html_file^"\">"^s_final^"</a>"
	else
          s_final
    in
    let s2 = Str.global_substitute
      (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
      f
      s
    in
    s2

  (** Take a string and return the string where fully qualified module idents
      have been replaced by links to the module referenced by the ident.*)
  method create_fully_qualified_module_idents_links m_name s =
    let f str_t =
      let match_s = Str.matched_string str_t in
      let rel = Odoc_info.Name.get_relative m_name match_s in
      let s_final = Odoc_info.apply_if_equal
	Odoc_info.use_hidden_modules
	match_s
	rel
      in
      if StringSet.mem match_s known_modules_names then
	let (html_file, _) = Naming.html_files match_s in
	"<a href=\"" ^ self#path match_s ^ html_file^"\">"^s_final^"</a>"
      else
	s_final
    in
    let s2 = Str.global_substitute
      (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([A-Z][a-zA-Z_'0-9]*\\)")
      f
      s
    in
    s2

  (** redefine from file odoc_html.ml *)
  method html_of_Module_list b l =
    let dir f = (* <dir> , <name> *)
      let chop dir f =
	let n = Str.search_forward (Str.regexp dir) f 0 in
	let f = String.sub f n (String.length f - n) in
	let d = Filename.dirname f in
	String.capitalize (Filename.basename d)
      in
      try Chapter(2,"C & ACSL","cil") , chop "cil/" f
      with Not_found ->
	try Chapter(1,"Frama-C","src") , chop "src/" f
	with Not_found ->
	  let d = Filename.dirname f in
	  Directory (Filename.basename (Filename.dirname d)) ,
	  String.capitalize (Filename.basename d)
    in
    let structured_modules (* chapter, section, module *) =
      (List.map
	 (fun name ->
	   let m = List.find (fun m -> m.m_name = name) self#list_modules
	   in let dir,name = dir m.m_file in
	      dir,name,m) l)
    in
    let toc_modules (* chapter/section/modules *) =
      merge3 compare_chapter compare compare structured_modules
    in
    List.iter
      (fun (chapter, subdirs) ->
	let dir =
	  ( match chapter with
	  | Chapter (n,a,d) ->
	    bp b "<h1 class=\"chapter\">Chapter %d. %s</h1>" n a ; d
	  | Directory d ->
	    bp b "<h1>Directory %s</h1>" d ; d)
	in
	List.iter
	  (fun (subdir,modules) ->
	    bp b "<h2 class=\"section\">Section %s <span class=\"directory\">(in %s/%s)</span></h2>\n"
	      subdir dir (String.lowercase subdir) ;
	    bs b "<br>\n<table class=\"indextable\">\n";
	    List.iter
	      (fun m ->
		bs b "<tr><td>";
		(try
		   let (html, _) = Naming.html_files m.m_name in
		   bp b "<a href=\"%s\">%s</a></td>" html m.m_name;
		   bs b "<td>";
		   self#html_of_info_first_sentence b m.m_info;
		 with Not_found ->
		   Odoc_messages.pwarning
		     (Odoc_messages.cross_module_not_found m.m_name);
		   bp b "%s</td><td>" m.m_name);
		bs b "</td></tr>\n")
	      modules;
	    bs b "</table>\n")
	  subdirs)
      toc_modules

  (** Print html code for an included module. *)
  method html_of_included_module b im =
    bs b "<pre>";
    bs b ((self#keyword "include")^" ");
    (
      match im.im_module with
        None ->
          bs b im.im_name
      | Some mmt ->
        let (file, name) =
          match mmt with
            Mod m ->
              let (html_file, _) = Naming.html_files m.m_name in
              (html_file, m.m_name)
          | Modtype mt ->
            let (html_file, _) =
	      Naming.html_files mt.mt_name
	    in
            (html_file, mt.mt_name)
        in
        bp b "<a href=\"%s%s\">%s</a>" (self#path name) file name
    );
    bs b "</pre>\n";
    self#html_of_info b im.im_info

  (** Generate all the html files from a module list. The main
      file is [<index_prefix>.html]. *)
  method generate module_list =

    (* init the style *)
    self#init_style ;

    (* init the lists of elements *)
    list_values <- Odoc_info.Search.values module_list ;
    list_exceptions <- Odoc_info.Search.exceptions module_list ;
    list_types <- Odoc_info.Search.types module_list ;
    list_attributes <- Odoc_info.Search.attributes module_list ;
    list_methods <- Odoc_info.Search.methods module_list ;
    list_classes <- Odoc_info.Search.classes module_list ;
    list_class_types <- Odoc_info.Search.class_types module_list ;
    list_modules <- Odoc_info.Search.modules module_list ;
    list_module_types <- Odoc_info.Search.module_types module_list ;

    (* prepare the page header *)
    self#prepare_header module_list ;

    (* Get the names of all known types. *)
    let types = Odoc_info.Search.types module_list in
    known_types_names <-
      List.fold_left
      (fun acc t -> StringSet.add t.Odoc_type.ty_name acc)
      known_types_names
      types ;

    (* Get the names of all class and class types. *)
    let classes = Odoc_info.Search.classes module_list in
    let class_types = Odoc_info.Search.class_types module_list in
    known_classes_names <-
      List.fold_left
      (fun acc c -> StringSet.add c.Odoc_class.cl_name acc)
      known_classes_names
      classes ;
    known_classes_names <-
      List.fold_left
      (fun acc ct -> StringSet.add ct.Odoc_class.clt_name acc)
      known_classes_names
      class_types ;

    (* Get the names of all known modules and module types. *)
    let module_types = Odoc_info.Search.module_types module_list in
    let modules = Odoc_info.Search.modules module_list in
    known_modules_names <-
      List.fold_left
      (fun acc m -> StringSet.add m.m_name acc)
      known_modules_names
      modules ;
    known_modules_names <-
      List.fold_left
      (fun acc mt -> StringSet.add mt.mt_name acc)
      known_modules_names
      module_types ;

    (* generate html for each module *)
    let keep_list =
      let keep m =
	not (List.exists (equal_module m) self#loaded_modules) &&
	  not (List.exists (equal_module_name m) !lib_files)
      in
      List.filter keep module_list
    in

    if not !Odoc_args.index_only then
      self#generate_elements self#generate_for_module keep_list ;

    (* reinit the lists of elements *)
    list_values <- Odoc_info.Search.values keep_list ;
    list_exceptions <- Odoc_info.Search.exceptions keep_list ;
    list_types <- Odoc_info.Search.types keep_list ;
    list_attributes <- Odoc_info.Search.attributes keep_list ;
    list_methods <- Odoc_info.Search.methods keep_list ;
    list_classes <- Odoc_info.Search.classes keep_list ;
    list_class_types <- Odoc_info.Search.class_types keep_list ;
    list_modules <- Odoc_info.Search.modules keep_list ;
    list_module_types <- Odoc_info.Search.module_types keep_list ;

    try
      self#generate_index keep_list;
      self#generate_values_index keep_list ;
      self#generate_exceptions_index keep_list ;
      self#generate_types_index keep_list ;
      self#generate_attributes_index keep_list ;
      self#generate_methods_index keep_list ;
      self#generate_classes_index keep_list ;
      self#generate_class_types_index keep_list ;
      self#generate_modules_index keep_list ;
      self#generate_module_types_index keep_list ;
    with Failure s ->
      prerr_endline s ;
      incr Odoc_info.errors

  method private html_of_plugin_developer_guide _t =
    "<b>Consult the <a href=\"http://www.frama-c.com/download/frama-c-plugin-development-guide.pdf\">Plugin Development Guide</a></b> for additional details.<br>\n"

  method private html_of_ignore _t = ""

  method private html_of_modify t = match t with
  | [] -> 
    Odoc_info.warning "Found an empty @modify tag"; 
    ""
  | Raw s :: l ->
    let time, explanation =
      try
	let idx = String.index s ' ' in
	String.sub s 0 idx,
	":" ^ String.sub s idx (String.length s - idx)
      with Not_found ->
	s, ""
    in
    let text =
      Bold [ Raw "Change in "; Raw time ] :: Raw explanation :: l
    in
    let buf = Buffer.create 7 in
    self#html_of_text buf text;
    Buffer.add_string buf "<br>\n";
    Buffer.contents buf
  | _ :: _ ->
    assert false

  method private html_of_call t = match t with
  | [] -> 
    Odoc_info.warning "Found an empty @call tag"; 
    ""
  | l ->
    let buf = Buffer.create 97 in
    Buffer.add_string buf "<b>Access it by</b> <code class=\"code\">";
    self#html_of_text buf l;
    Buffer.add_string buf "</code>\n";
    Buffer.contents buf

  (* Write the subtitle (eg. "Frama-C Kernel" after the main title
     instead of before, for users that use many tabs in their browser *)
  method inner_title s =
    match self#title with "" -> "" | t -> self#escape s ^ " - " ^ t

  initializer
    tag_functions <-
      ("modify", self#html_of_modify) ::
      ("ignore", self#html_of_ignore) ::
      ("call", self#html_of_call) ::
      ("plugin", self#html_of_plugin_developer_guide) :: tag_functions

end

let () =
  Odoc_args.set_doc_generator (Some (new gen :> Odoc_args.doc_generator));
  Odoc_args.add_option
    ("-docpath", Arg.Set_string doc_path, "Frama-C documentation directory");
  Odoc_args.add_option
    ("-stdlib", Arg.String (add_libfiles true), "Standard library files");
  Odoc_args.add_option
    ("-stdlib-path", Arg.String (add_libfiles false), "Standard library files")
