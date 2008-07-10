(* $Id: docgen.ml,v 1.6 2008/05/30 08:29:48 uid568 Exp $ *)

open Odoc_module

let doc_path = ref "."

let lib_files = ref []

let add_libfiles analyse s = 
  let f = Odoc_args.Intf_file s in
  lib_files :=
    (String.capitalize (Filename.chop_extension (Filename.basename s))) :: 
      !lib_files;
  if analyse then Odoc_args.files := f :: !Odoc_args.files

open Odoc_html

let rec root_name s =
  let simple = Odoc_info.Name.simple s in
  let father = Odoc_info.Name.father s in
  if father = "" then simple else root_name father

let equal_module_name m s =
  let n = m.m_name in
  n = s && n = root_name n

let equal_module m1 m2 = equal_module_name m1 m2.m_name

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
    let dir f = 
      let chop dir f =
	let n = Str.search_forward (Str.regexp dir) f 0 in
	let f = String.sub f n (String.length f - n) in
	let d = Filename.dirname f in
	String.capitalize (Filename.basename d) 
	^ " (in " ^ String.sub dir 0 (String.length dir - 1) ^ ")"
      in
      try chop "cil/" f
      with Not_found -> 
	try chop "src/" f
	with Not_found ->
	  let d = Filename.dirname f in
	  String.capitalize (Filename.basename d) ^ " (in " 
	  ^ Filename.basename (Filename.dirname d) ^ ")"
    in
    let modules =
      List.fold_left
	(fun acc name ->
	   (let m = List.find (fun m -> m.m_name = name) self#list_modules in
	    dir m.m_file, m) 
	   :: acc)
	[]
	l
    in
    let modules = 
      List.sort (fun (f1, _) (f2, _) -> String.compare f1 f2) modules 
    in
    let l =
      let rec merge acc l = 
	match l, acc with
	| [], _ -> acc
	| (f, m) :: tl, (f', l) :: tl' when f = f' -> 
	    merge ((f, m :: l) :: tl') tl
	| (f, m) :: tl, acc -> merge ((f, [ m ]) :: acc) tl
      in
      List.rev (merge [] modules)
    in
    List.iter 
      (fun (dir, l) ->
	 bp b "<h2>%s</h2>\n" dir;
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
           l;
	 bs b "</table>\n")
      l

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
    with
      Failure s ->
        prerr_endline s ;
        incr Odoc_info.errors

  method html_of_plugin_developer_guide _t = 
    "<b>Consult the <a href=\"http://www.frama-c.cea.fr/download/plugin-developer-guide.pdf\">Plugin Developer Guide</a></b> for additional details."

  initializer 
    tag_functions <- 
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
