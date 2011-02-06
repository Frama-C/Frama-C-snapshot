(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Cil_types
open Cil_datatype
open Extlib
open Gtk_helper

class type t =  object
  method model : GTree.model
  method set_file_attribute:
    ?strikethrough:bool -> ?text:string -> string -> unit
  method set_global_attribute:
    ?strikethrough:bool -> ?text:string -> varinfo -> unit
  method get_file_globals:
    string -> (string * bool) list
  method add_select_function :
    (was_activated:bool -> activating:bool -> global list -> unit) -> unit
  method append_pixbuf_column:
    title:string -> (global list -> GTree.cell_properties_pixbuf list) -> unit
  method select_global : varinfo -> unit
  method selected_globals : Cil_types.global list
  method view : GTree.view
  method reset : unit -> unit
  method reset_dynamic_columns :
    (GTree.view -> global list GTree.column -> unit) list -> unit
    (** Internal use only for legacy filetree mode *)

end

module MAKE(TREE:sig type t
		     val sons: t -> t array
                     val custom_value: Gobject.g_type -> t -> column:int -> Gobject.basic
                     val column_list:GTree.column_list
	    end) =
struct
  type custom_tree =
      {finfo: TREE.t;
       mutable sons: custom_tree array;
       mutable parent: custom_tree option;
       fidx: int (* invariant: parent.(fidx)==myself *) }

  let inbound i a = i>=0 && i<Array.length a

  (** The custom model itself *)
  class custom_tree_class column_list =
  object(self)
    inherit
      [custom_tree,custom_tree,unit,unit] GTree.custom_tree_model column_list as parent

    method custom_encode_iter cr = cr, (), ()
    method custom_decode_iter cr () () = cr

    val mutable num_roots : int = 0
    val mutable roots :  custom_tree array = [||]

    method custom_get_iter (path:Gtk.tree_path) : custom_tree option =
      let indices: int array  = GTree.Path.get_indices path in
      match indices with
      | [||] ->
          None
      | _ ->
          if inbound indices.(0) roots then
            let result = ref (roots.(indices.(0))) in
            try
              for depth=1 to Array.length indices - 1 do
                let index = indices.(depth) in
                if inbound index !result.sons then
                  result:=!result.sons.(index)
                else raise Not_found
              done;
              Some !result
            with Not_found ->
              None
          else None

    method custom_get_path (row:custom_tree) : Gtk.tree_path =
      let current_row = ref row in
      let path = ref [] in
      while !current_row.parent <> None do
        path := !current_row.fidx::!path;
        current_row := match !current_row.parent with Some p -> p
        | None -> assert false
      done;
      GTree.Path.create ((!current_row.fidx)::!path)

    method custom_value (t:Gobject.g_type) (row:custom_tree) ~column =
      TREE.custom_value t row.finfo ~column

    method custom_iter_next (row:custom_tree) : custom_tree option =
      let nidx = succ row.fidx in
      match row.parent with
      | None -> if inbound nidx roots then Some roots.(nidx)
        else None
      | Some parent ->
          if inbound nidx parent.sons then
            Some parent.sons.(nidx)
          else None

    method custom_iter_children (rowopt:custom_tree option) :custom_tree option =
      match rowopt with
      | None -> if inbound 0 roots then Some roots.(0) else None
      | Some row -> if inbound 0 row.sons then Some row.sons.(0) else None

    method custom_iter_has_child (row:custom_tree) : bool =
      Array.length row.sons  > 0

    method custom_iter_n_children (rowopt:custom_tree option) : int =
      match rowopt with
      | None -> Array.length roots
      | Some row -> Array.length row.sons

    method custom_iter_nth_child (rowopt:custom_tree option) (n:int)
      : custom_tree option =
      match rowopt with
      | None when inbound n roots -> Some roots.(n)
      | Some row when inbound n row.sons -> Some (row.sons.(n))
      | _ -> None

    method custom_iter_parent (row:custom_tree) : custom_tree option =
      row.parent

    method custom_foreach f =
    let f p _ = f p (match self#custom_get_iter p with
                     | Some v -> v
                     | None -> assert false)
    in
    parent#foreach f

    method append_tree (t:TREE.t) =
      let rec make_forest root sons =
        Array.mapi
          (fun i t -> let result = {finfo=t; fidx=i; parent = Some root;
                                    sons = [||] }
           in
           let sons = make_forest result (TREE.sons t) in
           result.sons<-sons;
           result)
          sons
      in
      let pos = num_roots in
      num_roots <- num_roots+1;
      let root = { finfo = t; sons = [||];
                   parent = None;
                   fidx = pos }
      in

      let sons = make_forest root (TREE.sons t)
      in
      root.sons <- sons;
      roots <-
        Array.init num_roots (fun n -> if n = num_roots - 1 then root
                              else roots.(n))
    method clear () =
      self#custom_foreach (fun p _ ->
                             self#custom_row_deleted p;
                             false)
  end

  let custom_tree () =
    new custom_tree_class TREE.column_list
end


module MYTREE = struct
type storage = { mutable name : string;
                 mutable globals: global array;
                 mutable strikethrough: bool}


type t = File of storage*t list | Global of storage

let sons t = match t with
| File (_,s) -> Array.of_list s
| Global _ -> [| |]


let sons_info = function
  | File (_, l) ->
      List.map (function
                  | Global { name = n; strikethrough = st } -> (n, st)
                  | File _ -> assert false (* should not happen, a file is
                                             never under a file in the tree *)
               ) l
  | Global _ -> []

let get_storage t = match t with
| File (s,_) -> s
| Global s -> s

let default_storage s globals =
 {
  name = s;
  globals = globals;
  strikethrough = false;
 }

(* Set up the columns *)
let column_list = new GTree.column_list
let filename_col = column_list#add Gobject.Data.string
let (glob_col:global list GTree.column) = column_list#add Gobject.Data.caml
let strikethrough_col = column_list#add Gobject.Data.boolean
let is_function_col = column_list#add Gobject.Data.boolean

let custom_value _ t ~column =
    match column with
    | 0 -> (* filename_col *) `STRING (Some (get_storage t).name)
    | 1 -> (* glob_col *) `CAML (Obj.repr ((get_storage t).globals))
    | 2 -> (* strikethrough_col *) `BOOL (get_storage t).strikethrough
    | 3 -> (* is_function_col *) `BOOL (match t with
                                         | File _ -> false
                                         | Global g -> match g.globals with
                                             | [| GFun _ |] -> true
                                             | _ -> false)
    | _ -> assert false

let make_file (display_name, globs) : t =
 let storage = default_storage display_name (Array.of_list globs) in
 let sons_funs, sons_vars =  List.fold_left
 (* Correct the function sons_info above if a [File] constructor can
    appear in [sons] *)
   (fun (accf, accv as acc) glob -> match glob with
    | GFun ({svar={vname=name}},_) ->
        (Global(default_storage name [|glob|]))::accf, accv

    | GVar({vname=name},_,_) ->
        accf, (Global(default_storage name [|glob|]))::accv

    | GVarDecl(_,({vname=name} as vi),_) ->
        if Cil.isFunctionType vi.vtype then
	  if Kernel_function.is_definition (Globals.Functions.get vi) then
            (* there is a prototype somewhere else *)
            acc
          else
            (Global(default_storage name [|glob|]))::accf, accv
        else
          accf, (Global(default_storage name [|glob|]))::accv

    | _ -> acc)
    ([], [])
    globs
 in
 (* We display first all the functions sorted by their names,
    then all the global variables *)
 let name g = String.lowercase ((get_storage g).name) in
 let sort = List.sort (fun g1 g2 -> String.compare (name g1) (name g2)) in
 File (storage, (sort sons_funs) @ (sort sons_vars))

end

module MODEL=MAKE(MYTREE)

module State = struct

  let default_filetree () =
    let m1 = MODEL.custom_tree () in
    m1,
    Varinfo.Hashtbl.create 17,
    Hashtbl.create 17,GTree.Path.create []

  include State_builder.Ref
  (Datatype.Make
     (struct
	 include Datatype.Undefined
	 type t =
             MODEL.custom_tree_class
             * (Gtk.tree_path * MODEL.custom_tree) Varinfo.Hashtbl.t
             * (string, (Gtk.tree_path * MODEL.custom_tree)) Hashtbl.t
             * Gtk.tree_path
	 let name = "Filetree.FileTree_Datatype"
	   (**  Prevent serialization of this state containing closures *)
	 let reprs = [ default_filetree () ]
	 let mem_project = Datatype.never_any_project
	end))
    (struct
       let name = "Filetree.State"
       let dependencies = [ Ast.self ]
       let kind = `Internal
       let default = default_filetree
     end)

  (** Make and fill the custom model. *)
  let get () =
    if is_computed () then get ()
    else
      let model,_,_,_ = get () in
      (** Let's fill up the model with all files and functions. *)
      let files = Globals.FileIndex.get_files () in
      let files' = List.map (fun s -> Globals.FileIndex.find s) files in
      List.iter
        (fun v -> model#append_tree (MYTREE.make_file v))
        (List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) files');

      (** Let's build the table from cil standard types to rows in the model *)

      (** These tables contain the path (in the treeview of file names)
          to the global (reps. filename) *)
      let global_path_tbl = Varinfo.Hashtbl.create 17 in
      let file_path_tbl = Hashtbl.create 17 in

      let cache path row =
        (match row.MODEL.finfo with
         | MYTREE.File (storage,_) ->
             Hashtbl.add file_path_tbl storage.MYTREE.name (path,row)
         | MYTREE.Global storage ->
	     match storage.MYTREE.globals with
               (* Only one element in this array by invariant: this is a leaf*)
	     | [| GFun ({svar=vi},_)
               | GVar(vi,_,_) | GVarDecl(_,vi,_) |] ->
                 Varinfo.Hashtbl.add global_path_tbl vi (path,row)
             | _ -> (* no cache for other globals yet *) ());
        false
      in
      model#custom_foreach cache; (* fills up the cache *)

      (* These must be put in a global variable. *)
      let r = model,global_path_tbl, file_path_tbl,GTree.Path.create [] in
      set r;
      mark_as_computed ();
      r
end

let make (tree_view:GTree.view) =

  let model,global_path_tbl, file_path_tbl, activated_path = State.get () in

  (** View part *)
  let source_column = GTree.view_column ~title:"Source file" () in
  source_column#set_resizable true;
  let str_renderer = GTree.cell_renderer_text [] in
  source_column#pack str_renderer;
  source_column#add_attribute str_renderer "text" MYTREE.filename_col;
  source_column#add_attribute str_renderer "strikethrough"
    MYTREE.strikethrough_col;
  source_column#add_attribute str_renderer "underline"
    MYTREE.is_function_col;

  let _ = tree_view#append_column source_column in

  tree_view#set_model (Some (model:>GTree.model));

  let set_row model ?strikethrough ?text (path,raw_row) =
    let row = raw_row.MODEL.finfo in
    may
      (fun b -> (MYTREE.get_storage row).MYTREE.strikethrough <- b)
      strikethrough;
    may (fun b -> (MYTREE.get_storage row).MYTREE.name <- b) text;
    model#custom_row_changed path raw_row

  in

  let set_file_attribute file_path_tbl model ?strikethrough ?text filename =
    set_row model ?strikethrough ?text (Hashtbl.find file_path_tbl filename)
  and set_global_attribute global_path_tbl model ?strikethrough ?text global =
    set_row model ?strikethrough ?text
      (Varinfo.Hashtbl.find global_path_tbl global)
  and get_file_globals file_path_tbl file =
    try let _, raw_row =  Hashtbl.find file_path_tbl file
    in MYTREE.sons_info raw_row.MODEL.finfo
    with Not_found -> Gui_parameters.error "%s" file; []

  in
  let myself = object(self)

    val mutable select_functions = []
    val mutable file_path_tbl = file_path_tbl
    val mutable global_path_tbl = global_path_tbl
    val mutable model_custom = model
    val mutable model = model
    val mutable activated_path = activated_path
      (* prevent double selection and restore activated path *)

    method append_pixbuf_column
      ~title (f:(global list -> GTree.cell_properties_pixbuf list)) =
      let column = GTree.view_column ~title () in
      column#set_resizable true;
      (*      column#set_sizing `FIXED;       column#set_fixed_width 70;*)
      let renderer = GTree.cell_renderer_pixbuf [] in
      column#pack renderer;
      column#set_cell_data_func renderer
        (fun model row ->
           let (path:Gtk.tree_path) = model#get_path row  in
           match model_custom#custom_get_iter path with
           | Some {MODEL.finfo=v} ->
               renderer#set_properties (f (Array.to_list((MYTREE.get_storage v).MYTREE.globals)))
           | None -> ());
      ignore (tree_view#append_column column)

    method view = tree_view
    method private model_custom = model_custom
    method model = model
    method private get_select_functions = select_functions
    method set_file_attribute =
      set_file_attribute file_path_tbl self#model_custom
    method set_global_attribute =
      set_global_attribute global_path_tbl self#model_custom
    method get_file_globals =
      get_file_globals file_path_tbl
    method private set_row_attribute = set_row self#model_custom
    method reset () = self#reset_internal ()

    val mutable force_selection = false

    method private enable_select_functions () =
      let select path deactivating =
	let fail e =
          Gui_parameters.error
            "selector handler got an internal error, please report: %s"
            (Printexc.to_string e)
	in
	try
	  let path_s = GTree.Path.to_string path in
          let was_activated =
            (Array.length (GTree.Path.get_indices activated_path) > 0) &&
              GTree.Path.to_string activated_path = path_s
          in
	  if (force_selection || not was_activated) && not deactivating
          then begin
            activated_path <- path;
            State.set (model_custom,global_path_tbl,file_path_tbl, path);
            let {MODEL.finfo=t} =
              match self#model_custom#custom_get_iter path  with
              | Some s ->s | None -> assert false
            in
	    let globs = (MYTREE.get_storage t).MYTREE.globals in
	    (*Format.printf "Select function %b on path %s@."
              (not deactivating) path_s;*)
            let globs = Array.to_list globs in
	    List.iter
	      (fun f ->
		 try
		   f ~was_activated:(not force_selection && was_activated)
                     ~activating:(not deactivating) globs
		 with e-> fail e)
	      select_functions
          end;
          force_selection <- false;
	  true
	with e ->
          Gui_parameters.error
	    "gui could not select row in filetree, please report: %s"
	    (Printexc.to_string e);
          true
      in
      tree_view#selection#set_select_function select

    method add_select_function f =
      select_functions <- select_functions@[f];

    method reset_dynamic_columns
      (_:(GTree.view -> global list GTree.column -> unit) list) =
      ignore (assert false)

    method select_global vi =
      try
        let path, _ = Varinfo.Hashtbl.find global_path_tbl vi in
        expand_to_path tree_view path;
        tree_view#selection#select_path path;
      with Not_found ->
	()

    method selected_globals =
      let (model,_,_,path) = State.get () in
      match model#custom_get_iter path with
      | None -> []
      | Some {MODEL.finfo=f } ->
          Array.to_list (MYTREE.get_storage f).MYTREE.globals

    method private reset_internal () =
      let mc,gc,fc,path = State.get () in
      tree_view#set_model (Some (mc:>GTree.model));
      model_custom <- mc;
      model <- mc;
      global_path_tbl<-gc;
      file_path_tbl<-fc;
      activated_path <- path;
      expand_to_path tree_view path;
      force_selection <- true;
      tree_view#selection#select_path path

    initializer
      self#enable_select_functions ()

  end
  in
  (myself:>t)


(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
