(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type ('a,'b) column =
  ?title:string -> 'b list -> ('a -> 'b list) -> GTree.view_column

class type virtual ['a] custom =
  object
    inherit ['a,'a,unit,unit] GTree.custom_tree_model
    method reload : unit
  end

class type ['a] columns =
  object
    method view : GTree.view (** the tree *)
    method scroll : GBin.scrolled_window (** scrolled tree (build on demand) *)
    method coerce : GObj.widget (** widget of the scroll *)
    method pack : (GObj.widget -> unit) -> unit (** packs the scroll *)
    method reload : unit (** Structure has changed *)
    method update_all : unit (** (only) Content of rows has changed *)
    method update_row : 'a -> unit
    method insert_row : 'a -> unit
    method set_focus : 'a -> GTree.view_column -> unit
    method on_click : ('a -> GTree.view_column -> unit) -> unit
    method on_right_click : ('a -> GTree.view_column -> unit) -> unit
    method on_double_click : ('a -> GTree.view_column -> unit) -> unit
    method set_selection_mode : Gtk.Tags.selection_mode -> unit
    method on_selection : (unit -> unit) -> unit
    method count_selected : int
    method iter_selected : ('a -> unit) -> unit
    method is_selected : 'a -> bool
    method add_column_text   : ('a,GTree.cell_properties_text) column
    method add_column_pixbuf : ('a,GTree.cell_properties_pixbuf) column
    method add_column_toggle : ('a,GTree.cell_properties_toggle) column
    method add_column_empty : GTree.view_column
    (** Add an empty column that always appears after the columns created
        by the other [add_column] methods. *)
  end

class type ['a] listmodel =
  object
    method reload : unit
    method size : int
    method index : 'a -> int
    method get : int -> 'a
  end

class type ['a] treemodel =
  object
    method reload : unit
    method has_child : 'a -> bool
    method children : 'a option -> int
    method child_at : 'a option -> int -> 'a
    method parent : 'a -> 'a option
    method index : 'a -> int
  end

(* -------------------------------------------------------------------------- *)
(* --- Columns                                                            --- *)
(* -------------------------------------------------------------------------- *)

let add_column (view:GTree.view) empty data ?title renderer render =
  begin
    let column = GTree.view_column ?title ~renderer:(renderer,[]) () in
    column#set_resizable true ;
    (* column#set_sizing `FIXED ;  *)
    column#set_cell_data_func renderer
      (fun model iter ->
         let props = match data (model#get_path iter) with
           | None -> []
           | Some e -> render e in
         renderer#set_properties props) ;
    ignore (view#append_column column);
    begin
      match empty with
      | None -> ()
      | Some e -> ignore (view#move_column e ~after:column)
    end ;
    column
  end

class ['a] makecolumns ?packing ?width ?height
    (view:GTree.view) (model : 'a #custom) =
  object(self)

    val mutable scroll = None

    initializer match packing with
      | Some packing -> self#pack packing
      | None -> ()

    method scroll =
      match scroll with
      | None ->
          let s = GBin.scrolled_window ?width ?height () in
          s#add view#coerce ; scroll <- Some s ; s
      | Some s -> s

    method pack packing = packing self#scroll#coerce
    method view = view
    method coerce = self#scroll#coerce

    method update_all = GtkBase.Widget.queue_draw view#as_tree_view

    method update_row x =
      try
        (*TODO : get the rectangle for raw and use queue_draw_area
          See  : http://www.gtkforums.com/viewtopic.php?t=1716
          Sadly this is not available in LablGtk2 yet...*)
        model#custom_row_changed (model#custom_get_path x) x
      with Not_found -> ()

    method insert_row x =
      try
        let path = model#custom_get_path x in
        model#custom_row_inserted path x
      with Not_found -> ()

    method reload =
      begin
        (* Delete all nodes in view *)
        let root = GTree.Path.create [0] in
        model#foreach
          (fun _p _i ->
             (* Do not use p since the path is changed by the call
                  to custom_row_deleted*)
             model#custom_row_deleted root;
             false) ;
        (* Then call model *)
        model#reload ;
      end

    method on_right_click f =
      let callback evt =
        let open GdkEvent in
        if Button.button evt = 3 then
          begin
            let x = int_of_float (Button.x evt) in
            let y = int_of_float (Button.y evt) in
            match view#get_path_at_pos ~x ~y with
            | Some (path,col,_,_) ->
                begin
                  match model#custom_get_iter path with
                  | None -> false
                  | Some item ->
                      let () = f item col in false
                end
            | _ -> false
          end
        else false
      in ignore (view#event#connect#button_release ~callback)

    method on_click f =
      let callback () =
        match view#get_cursor () with
        | Some path , Some col ->
            begin
              match model#custom_get_iter path with
              | None -> ()
              | Some item -> f item col
            end
        | _ -> ()
      in ignore (view#connect#cursor_changed ~callback)

    method on_double_click f =
      let callback path col =
        match model#custom_get_iter path with
        | None -> ()
        | Some item -> f item col
      in ignore (view#connect#row_activated ~callback)

    method is_selected item =
      try view#selection#path_is_selected (model#custom_get_path item)
      with Not_found -> false

    method on_selection f =
      ignore (view#selection#connect#changed ~callback:f)

    method set_selection_mode = view#selection#set_mode

    method count_selected = view#selection#count_selected_rows

    method iter_selected f =
      List.iter
        (fun p ->
           match model#custom_get_iter p with
           | None -> ()
           | Some item -> f item)
        view#selection#get_selected_rows

    method set_focus item col =
      try
        let path = model#custom_get_path item in
        view#scroll_to_cell path col ;
        view#selection#select_path path ;
      with Not_found -> ()

    val mutable empty : GTree.view_column option = None

    method add_column_text ?title props render =
      let cell = GTree.cell_renderer_text props in
      add_column view empty model#custom_get_iter ?title cell render

    method add_column_pixbuf ?title props render =
      let cell = GTree.cell_renderer_pixbuf props in
      add_column view empty model#custom_get_iter ?title cell render

    method add_column_toggle ?title props render =
      let cell = GTree.cell_renderer_toggle props in
      add_column view empty model#custom_get_iter ?title cell render

    method add_column_empty =
      let column = GTree.view_column ~title:"" () in
      empty <- Some column ;
      ignore (view#append_column column);
      column

  end

(* -------------------------------------------------------------------------- *)
(* --- Gtk List Model                                                     --- *)
(* -------------------------------------------------------------------------- *)

class ['a] glist_model (m : 'a listmodel) =
  object
    method reload = m#reload
    inherit ['a,'a,unit,unit] GTree.custom_tree_model (new GTree.column_list)
    method! custom_flags = [`LIST_ONLY]
    method custom_decode_iter a () () = a
    method custom_encode_iter a = (a,(),())

    method custom_get_iter path =
      let idx:int array = GtkTree.TreePath.get_indices path in
      match idx with
      | [||] -> None
      | [|i|] -> (try let e = m#get i in
                    Some e
                  with Not_found -> None)
      | _ -> failwith "Invalid path of depth>1 in a list"

    method custom_get_path e =
      GtkTree.TreePath.create [m#index e]

    method custom_value (_:Gobject.g_type) (_:'a) ~column:_ =
      failwith "GwList: empty columns"

    method custom_iter_children e = match e with
      | None when (m#size > 0) ->
          Some(m#get 0)
      | _ ->
          None

    method custom_iter_has_child (_:'a) =
      false

    method custom_iter_n_children = function
      | Some _ -> failwith "GwList: no children"
      | None -> m#size

    method custom_iter_nth_child r k = match r with
      | Some _ -> failwith "GwList: no nth-child"
      | None ->
          if k < m#size then Some (m#get k) else None

    method custom_iter_parent (_:'a) = None

    method custom_iter_next e =
      let r =
        try
          let k = succ (m#index e) in
          if k < m#size then Some (m#get k) else None
        with Not_found -> None
      in
      r
  end

(* -------------------------------------------------------------------------- *)
(* --- Gtk List View                                                      --- *)
(* -------------------------------------------------------------------------- *)

class ['a] list
    ?packing ?width ?height
    ?(headers=true) ?(rules=true)
    (m : 'a listmodel) =
  let model = new glist_model m in
  let view = GTree.view ~model
      ~headers_visible:headers
      ~rules_hint:rules
      ~show:true () in
  object
    inherit ['a] makecolumns ?packing ?width ?height view model
  end

(* -------------------------------------------------------------------------- *)
(* --- Gtk Tree Model                                                     --- *)
(* -------------------------------------------------------------------------- *)

let rec get_iter m r idx k =
  if k >= Array.length idx then r else
    let a = m#child_at r idx.(k) in
    get_iter m (Some a) idx (succ k)

let rec get_path ks m a =
  let ks = m#index a :: ks in
  match m#parent a with
  | None -> ks
  | Some b -> get_path ks m b

class ['a] gtree_model (m : 'a treemodel) =
  object
    method reload = m#reload
    inherit ['a,'a,unit,unit] GTree.custom_tree_model (new GTree.column_list)
    method custom_decode_iter a () () = a
    method custom_encode_iter a = (a,(),())

    method custom_get_iter path =
      let idx = GtkTree.TreePath.get_indices path in
      if Array.length idx = 0 then None else
        let a = m#child_at None idx.(0) in
        get_iter m (Some a) idx 1

    method custom_get_path e =
      let ks = get_path [] m e in
      GtkTree.TreePath.create ks

    method custom_value (_:Gobject.g_type) (_:'a) ~column:(_:int) : Gobject.basic
      = Format.eprintf "Wtable.custom_value@." ; assert false

    method custom_iter_children r =
      let node = match r with None -> true | Some f -> m#has_child f in
      if node && m#children r > 0 then Some (m#child_at r 0) else None

    method custom_iter_has_child r =
      m#has_child r && m#children (Some r) > 0
    method custom_iter_n_children = m#children
    method custom_iter_nth_child r k =
      if k < m#children r then Some (m#child_at r k) else None
    method custom_iter_parent r = m#parent r
    method custom_iter_next e =
      let p = m#parent e in
      let k = succ (m#index e) in
      if k < m#children p then Some (m#child_at p k) else None

  end

(* -------------------------------------------------------------------------- *)
(* --- Gtk Tree View                                                      --- *)
(* -------------------------------------------------------------------------- *)

class ['a] tree
    ?packing ?width ?height
    ?(headers=true) ?(rules=true) (m : 'a treemodel) =
  let model = new gtree_model m in
  let view = GTree.view ~model
      ~headers_visible:headers
      ~rules_hint:rules
      ~show:true ()
  in
  object
    inherit ['a] makecolumns ?packing ?width ?height view model
  end
