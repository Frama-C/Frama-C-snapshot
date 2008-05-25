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

open Extlib
open Ast_info
open Cil_types
open Cil
open CilE
open Format
open Db
open Db_types
open Pretty_source
open Gtk_helper
open Cilutil

class type t =  object 
  method model : GTree.model_filter
  method set_file_attribute: ?strikethrough:bool -> ?visible:bool -> ?text:string -> string -> unit
  method set_global_attribute: ?strikethrough:bool -> ?visible:bool -> ?text:string -> Cil_types.varinfo -> unit
  method add_select_function : 
    (was_activated:bool -> activating:bool -> Cil_types.global list -> unit) -> unit
  method select_global : Cil_types.varinfo -> unit
  method view : GTree.view
  method reset : unit -> t
end

let rec make (tree_view:GTree.view) = 

  (** These tables contain the path (in the treeview of file names)
      to the global (reps. filename) *)
  let global_path_tbl = Cilutil.VarinfoHashtbl.create 17 in
  let file_path_tbl = Hashtbl.create 17 in

  (** Model Part *)
  (* Set up the columns *)
  let cols = new GTree.column_list in
  let filename_col = cols#add Gobject.Data.string in
  let (glob_col:global list GTree.column) = cols#add Gobject.Data.caml in
  let strikethrough_col = cols#add Gobject.Data.boolean in
  let visible_col = cols#add Gobject.Data.boolean in

  let treestore = GTree.tree_store cols in

  (** Add a global as son of [parent] in the model *)
  let add_global ?parent global = 
    let row = treestore#append ?parent () in
    
    (match global with (* Only GFun are in the tree view *)
     | GFun ({svar=vid } as fd,_) -> 
         let name,visible = 
	   fprintf_to_string "%a" 
	     pretty_vname fd.svar,true 
	 in
         treestore#set ~row ~column:filename_col name;
         treestore#set ~row ~column:glob_col [global];
         treestore#set ~row ~column:visible_col visible;
         treestore#set ~row ~column:strikethrough_col false;
         
         let row_ref = 
	   treestore#get_row_reference (treestore#get_path row) 
	 in
	 Cilutil.VarinfoHashtbl.add global_path_tbl vid row_ref
     | _ -> ())
  in
  (** Fill-up the model with a file and its globals. *)
  let add_file s = 
    (* The file name is added at toplevel. *)
    let toplevel = treestore#append () in
    (* Cache the path to [s] *)
    let row_ref = treestore#get_row_reference (treestore#get_path toplevel) in
    Hashtbl.add file_path_tbl s row_ref;

    (* filling up the tree with file name *)
    treestore#set ~row:toplevel ~column:filename_col s;

    (* All row are visible by default. This may be changed by later hooks. *)
    treestore#set ~row:toplevel ~column:visible_col true;

    (* The list of [global] defined in this file *)
    let globs = Globals.FileIndex.find s in
    treestore#set ~row:toplevel ~column:glob_col globs;

    (* Row are not striked by default *)
    treestore#set ~row:toplevel ~column:strikethrough_col false;
    
    List.iter (add_global ~parent:toplevel) globs
  in
  
  (** Let's fill the model with all files. *) 
  List.iter add_file 
    (List.sort Pervasives.compare (Globals.FileIndex.get_files ()));

  let model_filtered = GTree.model_filter (treestore:>GTree.model) in
  model_filtered#set_visible_column visible_col;

  (* kills performance?
     let sort_by_name (model:#GTree.model) row1 row2 =
     let name1 = model#get ~row:row1 ~column:filename_col in
     let name2 = model#get ~row:row2 ~column:filename_col in
     compare name1 name2 
     in 
     treestore#set_sort_func 0 sort_by_name;
     treestore#set_sort_column_id 0 `ASCENDING;
  *)

  (** View part *)
  let source_column = GTree.view_column ~title:"Source file" () in
  let str_renderer = GTree.cell_renderer_text [] in
  source_column#pack str_renderer;
  source_column#add_attribute str_renderer "text" filename_col;
  source_column#add_attribute str_renderer "strikethrough" strikethrough_col;
  
  let _ = tree_view#append_column source_column in
  tree_view#set_model (Some (model_filtered:>GTree.model));
  let set_row_ref  ?strikethrough ?visible ?text row_ref = 
    assert (row_ref#valid);
    let row = row_ref#iter in
    may (treestore#set ~row ~column:visible_col) visible;
    may (treestore#set ~row ~column:strikethrough_col) strikethrough;
    may (treestore#set ~row ~column:filename_col) text
      
  in
  let set_file_attribute ?strikethrough ?visible ?text filename = 
    set_row_ref ?strikethrough ?visible ?text 
      (Hashtbl.find file_path_tbl filename)
  in
  let set_global_attribute ?strikethrough ?visible ?text global = 
    set_row_ref ?strikethrough ?visible ?text 
      (Cilutil.VarinfoHashtbl.find global_path_tbl global)
  in
  let myself = object
    
    val mutable select_functions = []

    method view = tree_view
    method model = model_filtered
    method set_file_attribute = set_file_attribute
    method set_global_attribute = set_global_attribute
    method set_row_attribute = set_row_ref
    method reset () = 
      (** Cleanup the existing tree view *)
      tree_view#set_model None;
      (try
         ignore (tree_view#remove_column (tree_view#get_column 0));
         ignore (tree_view#remove_column (tree_view#get_column 0))
       with Gpointer.Null -> ());
      let myself = make tree_view in
      List.iter myself#add_select_function select_functions;
      myself

    val mutable activated_path = "" (* prevent double selection *)
    method enable_select_functions = 
      let fail e = Format.eprintf 
        "selector handler got an internal error, please report: %s@." 
        (Printexc.to_string e)
      in
      let select path deactivating = 
        try 
          if !Gtk_helper.gui_unlocked then 
	    let path_s = GTree.Path.to_string path in
            let was_activated = activated_path = path_s in
	    if not was_activated && not deactivating then activated_path <- path_s; 
            let iter = model_filtered#get_iter path  in
            let globs = model_filtered#get ~row:iter ~column:glob_col in
            (*        	  prerr_endline 
	                  ("Select function "
	                  ^ (if (not deactivating) then "true" else "false") 
	                  ^ " on "^path_s);*)
            List.iter 
              (fun f -> 
                 try 
                   f ~was_activated ~activating:(not deactivating) globs
                 with e -> fail e)
              select_functions;
            true
          else false
        with e ->
          Format.eprintf "gui could not select row in filetree, please report: %s@."
            (Printexc.to_string e);
          true
      in
      tree_view#selection#set_select_function select

    method add_select_function f = 
      select_functions <- select_functions@[f];

    method select_global vi = 
      try 
        let path = 
          model_filtered#convert_child_path_to_path 
            (Cilutil.VarinfoHashtbl.find global_path_tbl vi)#path 
        in
        expand_to_path tree_view path;
        tree_view#selection#select_path path
      with Not_found -> ()
  end
  in 
  myself#enable_select_functions;
  (myself:>t)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
