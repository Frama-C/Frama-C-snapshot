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

type filetree_node =
  | File of string * Cil_types.global list
  | Global of Cil_types.global

class type t =  object
  method model : GTree.model
  method flat_mode: bool
  method set_file_attribute:
    ?strikethrough:bool -> ?text:string -> string -> unit
  method set_global_attribute:
    ?strikethrough:bool -> ?text:string -> varinfo -> unit
  method add_global_filter:
    text:string -> key:string -> (Cil_types.varinfo -> bool) -> (unit -> bool)
  method get_file_globals:
    string -> (string * bool) list
  method add_select_function :
    (was_activated:bool -> activating:bool -> filetree_node -> unit) -> unit
  method append_pixbuf_column:
    title:string -> (global list -> GTree.cell_properties_pixbuf list) ->
      (unit -> bool) -> (unit -> unit)
  method select_global : Cil_types.global -> bool
  method selected_globals : Cil_types.global list
  method view : GTree.view
  method reset : unit -> unit
  method register_reset_extension : (t -> unit) -> unit
  method refresh_columns : unit -> unit
end

module MAKE(TREE:sig type t val sons: t -> t array end) =
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

    method custom_value (_t:Gobject.g_type) (_row:custom_tree) ~column:_ =
      assert false

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

  let custom_tree () = new custom_tree_class (new GTree.column_list)
end


module MYTREE = struct
  type storage = { mutable name : string;
                   mutable globals: global array;
                   mutable strikethrough: bool}


  type t = MFile of storage*t list | MGlobal of storage

  let storage_type = function
    | MFile (s, _) -> File (s.name, Array.to_list s.globals)
    | MGlobal { globals = [| g |] } -> Global g
    | MGlobal _ -> assert false

  let sons t = match t with
  | MFile (_,s) -> Array.of_list s
  | MGlobal _ -> [| |]


  let sons_info = function
    | MFile (_, l) ->
      List.map (function
        | MGlobal { name = n; strikethrough = st } -> (n, st)
        | MFile _ -> assert false (* should not happen, a file is
                                    never under a file in the tree *)
      ) l
    | MGlobal _ -> []

  let get_storage t = match t with
  | MFile (s,_) -> s
  | MGlobal s -> s

  let is_function_vi vi = Cil.isFunctionType vi.vtype

  let is_function t = match t with
  | MFile _ -> false
  | MGlobal g -> match g.globals with
    | [| GFun _ |] -> true
    | [| GVarDecl (_, vi, _) |] -> is_function_vi vi
    | _ -> false

  let default_storage s globals =
    {
      name = s;
      globals = globals;
      strikethrough = false;
    }

  let make_list_globals hide globs =
    let l = List.fold_left
      (* Correct the function sons_info above if a [File] constructor can
         appear in [sons] *)
      (fun acc glob ->
        match glob with
          | GFun ({svar=({vname=name} as vi)},_)
          | GVar(({vname=name} as vi),_,_) ->
              if hide vi then acc
              else MGlobal(default_storage name [|glob|]) :: acc

          | GVarDecl(_, vi,_) ->
             (* we have a found the prototype, but there is a definition
                somewhere else. Skip the prototype. *)
              if hide vi ||
                (Cil.isFunctionType vi.vtype &&
                 Kernel_function.is_definition (Globals.Functions.get vi))
              then acc
              else MGlobal(default_storage vi.vname [|glob|]) :: acc

          | _ -> acc)
      []
      globs
    in
    let name g = String.lowercase ((get_storage g).name) in
    let sort = List.sort (fun g1 g2 -> String.compare (name g1) (name g2)) in
    sort l

  let make_file hide (display_name, globs) =
    let storage = default_storage display_name (Array.of_list globs) in
    let sons = make_list_globals hide globs in
    storage, sons
end

module MODEL=MAKE(MYTREE)

(* Primitives to handle the filetree menu (which allows to hide some
   entries) *)
module MenusHide = struct
  let hide key () = Configuration.find_bool ~default:false key

  let menu_item (menu: GMenu.menu) ~label ~key =
    let mi = GMenu.check_menu_item ~label () in
    mi#set_active (hide key ());
    menu#add (mi :> GMenu.menu_item);
    mi

  let mi_set_callback (mi: GMenu.check_menu_item) ~key reset =
    mi#connect#toggled ~callback:
      (fun () ->
         let v = mi#active in
         Configuration.set key (Configuration.ConfBool v);
         reset ())

end

let key_flat_mode = "filetree_flat_mode"
let flat_mode = MenusHide.hide key_flat_mode

module State = struct

  let default_filetree () =
    let m1 = MODEL.custom_tree () in
    m1,
    Varinfo.Hashtbl.create 17,
    Hashtbl.create 17,
    GTree.Path.create []

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

  (** Make and fill the custom model with default values. *)
  let compute hide_filters =
    Gui_parameters.debug "Resetting GUI filetree";
    let hide g = List.exists (fun filter -> filter g) hide_filters in
    clear ();
    let model, global_path_tbl, file_path_tbl,_ = get () in
    (* Let's fill up the model with all files and functions. *)
    let files = Globals.FileIndex.get_files () in
    let files' = List.map (fun s -> Globals.FileIndex.find s) files in
    if flat_mode () then
      let files =
        MYTREE.make_list_globals hide (List.concat (List.map snd files'))
      in
      List.iter model#append_tree files
    else
      List.iter
        (fun v ->
           let name, globals = MYTREE.make_file hide v in
           model#append_tree (MYTREE.MFile (name, globals)))
        (List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) files');

    (* Let's build the table from globals to rows in the model *)
    let cache path row =
      (match row.MODEL.finfo with
        | MYTREE.MFile (storage,_) ->
            Hashtbl.add file_path_tbl storage.MYTREE.name (path,row)
        | MYTREE.MGlobal storage ->
          match storage.MYTREE.globals with
            (* Only one element in this array by invariant: this is a leaf*)
            | [| GFun ({svar=vi},_) | GVar(vi,_,_) | GVarDecl(_,vi,_) |] ->
                Varinfo.Hashtbl.add global_path_tbl vi (path,row)
            | _ -> (* no cache for other globals yet *) ()
      );
      false
    in
    model#custom_foreach cache; (* fills up the cache *)

    mark_as_computed ()

  let get () =
    if is_computed () then get ()
    else (compute [] (* Failsafe: everything is shown *);
          get ())

end

let make (tree_view:GTree.view) =

  (* Menu for configuring the filetree *)
  let menu = GMenu.menu () in
  let button_menu = GButton.button ~relief:`HALF  ~label:"Source file" () in

  (* Buttons to show/hide variables and/or functions *)
  let key_hide_variables = "filetree_hide_variables" in
  let key_hide_functions = "filetree_hide_functions" in
  let hide_variables = MenusHide.hide key_hide_variables in
  let hide_functions = MenusHide.hide key_hide_functions in
  let initial_filter vi =
    let is_fun = MYTREE.is_function_vi vi in
    if is_fun then hide_functions ()
    else hide_variables ()
  in
  let mhide_variables =
    MenusHide.menu_item menu ~label:"Hide variables" ~key:key_hide_variables in
  let mhide_functions =
    MenusHide.menu_item menu ~label:"Hide functions" ~key:key_hide_functions in
  let mflat_mode =
    MenusHide.menu_item menu ~label:"Flat mode" ~key:key_flat_mode in

  (* Initial filetree nodes to display *)
  State.compute [initial_filter];
  let init_model, init_global_path_tbl, init_file_path_tbl, init_activated_path=
    State.get () in

  let set_row model ?strikethrough ?text (path,raw_row) =
    let row = raw_row.MODEL.finfo in
    may
      (fun b -> (MYTREE.get_storage row).MYTREE.strikethrough <- b)
      strikethrough;
    may (fun b -> (MYTREE.get_storage row).MYTREE.name <- b) text;
    model#custom_row_changed path raw_row
  in

  let myself = object(self)

    val mutable reset_extensions = []

    val mutable select_functions = []
    val mutable file_path_tbl = init_file_path_tbl
    val mutable global_path_tbl = init_global_path_tbl
    val mutable model_custom = init_model
    val mutable activated_path = init_activated_path
      (* prevent double selection and restore activated path *)
    val mutable hide_globals_filters = [initial_filter]

    val mutable force_selection = false

    (* Forward reference to the first column. Always set *)
    val mutable source_column = None

    val mutable columns_visibility = []

    method refresh_columns () =
      List.iter (fun f -> f ()) columns_visibility

    method append_pixbuf_column
      ~title (f:(global list -> GTree.cell_properties_pixbuf list)) visible =
      let column = GTree.view_column ~title () in
      column#set_resizable true;
      let renderer = GTree.cell_renderer_pixbuf [] in
      column#pack renderer;
      column#set_cell_data_func renderer
        (fun model row ->
           let (path:Gtk.tree_path) = model#get_path row  in
           match model_custom#custom_get_iter path with
           | Some {MODEL.finfo=v} ->
               renderer#set_properties (f (Array.to_list((MYTREE.get_storage v).MYTREE.globals)))
           | None -> ());
      ignore (tree_view#append_column column);
      (* We return a function showing or masking the column*)
      let refresh () = column#set_visible (visible ()) in
      refresh ();
      columns_visibility <- refresh :: columns_visibility;
      refresh

    method view = tree_view
    method model = model_custom

    method reset () =
      self#reset_internal ();
      self#refresh_columns ();
      List.iter (fun f -> f (self :> t)) reset_extensions;

    method register_reset_extension f =
      reset_extensions <- f :: reset_extensions

    method set_file_attribute ?strikethrough ?text filename =
      try
        set_row model_custom ?strikethrough ?text
          (Hashtbl.find file_path_tbl filename)
      with Not_found -> () (* Some files might not be in the list because
                             of our filters. Ignore *)

    method set_global_attribute ?strikethrough ?text global =
      try
        set_row model_custom ?strikethrough ?text
          (Varinfo.Hashtbl.find global_path_tbl global)
      with Not_found -> () (* Some globals might not be in the list because of
                             our filters. Ignore *)

    method flat_mode = flat_mode ()

    method get_file_globals file =
      try let _, raw_row =  Hashtbl.find file_path_tbl file in
          MYTREE.sons_info raw_row.MODEL.finfo
      with Not_found -> Gui_parameters.error "%s" file; []


    method private enable_select_functions () =
      let select path path_currently_selected =
        let fail e =
          Gui_parameters.error
            "selector handler got an internal error, please report: %s"
            (Printexc.to_string e)
        in
        try
          let path_s = GTree.Path.to_string path in
          (*Format.printf "Select function activating:%b on path %s
            was @."  (not path_currently_selected) path_s;*)
          let was_activated =
            (Array.length (GTree.Path.get_indices activated_path) > 0) &&
              GTree.Path.to_string activated_path = path_s
          in
          if (force_selection || not was_activated)
            && not path_currently_selected
          then begin
            activated_path <- path;
            State.set (model_custom,global_path_tbl,file_path_tbl, path);
            let {MODEL.finfo=t} =
              match model_custom#custom_get_iter path  with
              | Some s ->s | None -> assert false
            in
            let arg = MYTREE.storage_type t in
            List.iter
              (fun f ->
                 try
                   f ~was_activated:(not force_selection && was_activated)
                     ~activating:true
                     arg
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

    method private varinfo_of_global g =
      match g with
        | GVar (vi, _, _)
        | GVarDecl (_, vi, _)
        | GFun ({svar = vi}, _) -> Some vi
        | _ -> None


    method select_global g =
      match self#varinfo_of_global g with
        | None -> false
        | Some vi ->
            try
              let path, _ = Varinfo.Hashtbl.find global_path_tbl vi in
              expand_to_path tree_view path;
              tree_view#selection#select_path path;
              tree_view#scroll_to_cell
                ~align:(0., 0.5) path (Extlib.the source_column);
              tree_view#misc#grab_focus ();
              true
            with Not_found ->
              false

    method selected_globals =
      match model_custom#custom_get_iter activated_path with
      | None -> []
      | Some {MODEL.finfo=f } ->
          Array.to_list (MYTREE.get_storage f).MYTREE.globals


    method add_global_filter ~text ~key f =
      hide_globals_filters <- f :: hide_globals_filters;
      let mi = MenusHide.menu_item menu ~label:text ~key in
      ignore (MenusHide.mi_set_callback mi ~key self#reset);
      MenusHide.hide key

    method private reset_internal () =
      (* We force a full recomputation using our filters for globals *)
      State.compute hide_globals_filters;
      let mc,gc,fc,path = State.get () in
      tree_view#set_model (Some (mc:>GTree.model));
      model_custom <- mc;
      global_path_tbl<-gc;
      file_path_tbl<-fc;
      tree_view#selection#unselect_path path;
      activated_path <- path;
      expand_to_path tree_view path;
      force_selection <- true;
      tree_view#selection#select_path path;

    initializer
    (* Source column *)
    let source_renderer = GTree.cell_renderer_text [`YALIGN 0.0] in
    let m_source_renderer renderer (lmodel:GTree.model) iter =
      let (path:Gtk.tree_path) = lmodel#get_path iter in
      match self#model#custom_get_iter path with
        | Some ({MODEL.finfo=MYTREE.MFile({MYTREE.name=m;
                                        strikethrough=strike},_) as s}
               |{MODEL.finfo=MYTREE.MGlobal ({MYTREE.name=m;
                                        strikethrough=strike}) as s})
          ->
          renderer#set_properties
            [`TEXT m;`STRIKETHROUGH strike;
             `UNDERLINE (if MYTREE.is_function s then `LOW else `NONE)];

        | None -> ()
    in
    let column = GTree.view_column
      ~title:"Source file"
      ~renderer:((source_renderer:>GTree.cell_renderer),[]) ()
    in
    source_column <- Some column;
    column#set_cell_data_func
      source_renderer (m_source_renderer source_renderer);
    column#set_resizable true;
    column#set_clickable true;
    column#set_widget (Some button_menu#coerce);

    ignore (column#connect#clicked ~callback:
              (fun () -> menu#popup
                 ~button:0
                 ~time:(GtkMain.Main.get_current_event_time ());
              ));

    ignore (MenusHide.mi_set_callback
              mhide_functions key_hide_functions self#reset);
    ignore (MenusHide.mi_set_callback
              mhide_variables key_hide_variables self#reset);
    ignore (MenusHide.mi_set_callback
              mflat_mode key_flat_mode self#reset);
    menu#add (GMenu.separator_item () :> GMenu.menu_item);

    let _ = tree_view#append_column column in
    tree_view#set_model (Some (init_model:>GTree.model));
    self#enable_select_functions ();

  end
  in
  (myself:>t)


(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
