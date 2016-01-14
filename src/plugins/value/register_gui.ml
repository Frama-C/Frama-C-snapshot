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

open Cil_types
open Cil
open Pretty_source
open Gtk_helper
open Gui_types

type main_ui = Design.main_window_extension_points
type menu = GMenu.menu GMenu.factory

let rec list_assoc f e = function
  | [] -> raise Not_found
  | (e', v) :: q -> if f e e' then v else list_assoc f e q

let rec list_mem f e = function
  | [] -> false
  | e' :: q -> f e e' || list_mem f e q

let rec list_remove f e = function
  | [] -> []
  | e' :: q -> if f e e' then list_remove f e q else e' :: list_remove f e q

let rec list_mem_assoc f e = function
  | [] -> false
  | (e', _) :: q -> f e e' || list_mem_assoc f e q

module Callstacks_manager = struct

  (* Selection of a row; [RSelectedCallstackCol] corresponds to the fact
     that both the row _and_ the 'Callstacks' column are selected. *)
  type row_selected = RUnselected | RSelected | RSelectedCallstackCol

  (* Information shown in a single row *)
  type row = {
    callstack: gui_callstack;
    rev_callstack: Gui_callstacks_filters.rcallstack (* cache *);
    mutable exprs: (gui_selection * Gui_eval.gui_selection_data) list
      (* if a column exists in the view, it expects to find some data for
         itself in each row here. *);
    mutable selected: row_selected;
  }

  let row_unfocused () = {
    callstack = GC_Filtered;
    rev_callstack = Gui_callstacks_filters.empty;
    exprs = [];
    selected = RUnselected;
  }

  type rows = row GCallstackMap.t

  let find_data row expr =
    try list_assoc gui_selection_equal expr row.exprs
    with Not_found ->
      (* should happen only for the "results hidden" special row, and
         in case of an evaluation error *)
      Gui_eval.gui_selection_data_empty

  let (!!) = Lazy.force

  let add_expr_to_row row expr data =
    if not (list_mem_assoc gui_selection_equal expr row.exprs) then
      row.exprs <- (expr, data) :: row.exprs

  let add_data_to_rows rows callstack expr data =
    try
      let cur_row = GCallstackMap.find callstack rows in
      add_expr_to_row cur_row expr data;
      rows
    with Not_found ->
      let row = {
        callstack; exprs = []; selected = RUnselected;
        rev_callstack = (match callstack with
            | GC_Consolidated | GC_Filtered -> Gui_callstacks_filters.empty
            | GC_Single s | GC_Callstack s ->
              Gui_callstacks_filters.from_callstack s);
      } in
      add_expr_to_row row expr data;
      GCallstackMap.add callstack row rows

  type filter_column =
      FilterAlarm of bool | FilterBefore of gui_res | FilterAfter of gui_after
  type filter = gui_selection * bool * filter_column

  let rec remove_filter e : filter list -> _ = function
    | [] -> []
    | (e', _, _ as hd) :: q as l ->
      let q' = remove_filter e q in
      if gui_selection_equal e e' then q' else if q == q' then l else hd :: q'

  module GColumn = struct
    type t = GTree.view_column
    let hash c = c#misc#get_oid
    let equal c1 c2 = (c1#misc#get_oid = c2#misc#get_oid)
  end

  (* Hash tables indexed by GTree columns *)
  module HColumns = FCHashtbl.Make(GColumn)

  (* Description of the columns of the widget. The [gui_selection] information
     refers to what the column is displaying *)
  type column_type =
    | CCallstack
    | CBefore of gui_selection
    | CAfter of gui_selection
    | CAlarm of gui_selection
    | CEmpty (* empty column at the end, for aesthetic purposes *)

  let equal_column_type ct1 ct2 = match ct1, ct2 with
    | CCallstack, CCallstack | CEmpty, CEmpty -> true
    | CBefore e1, CBefore e2 | CAfter e1, CAfter e2 | CAlarm e1, CAlarm e2 ->
      gui_selection_equal e1 e2
    | _ -> false

  (* This is an hybrid between the model and the view. *)
  type model = {
    mutable loc: gui_loc option (* model: loc which is being visualized *);
    mutable all_exprs: gui_selection list (* G expressions that are currently
                                       being displayed *);
    mutable columns_type: (column_type * (filtered:bool -> unit)) HColumns.t
      (* mapping from GTK columns to the data they display, plus a function
         whose argument indicates whether the column is filtered *);
    mutable rows: rows (* model: rows to display. Sorted, unfiltered
                              (the view does the filtering *);
    mutable row_selected: (int * row) option (* view: row currently selected *);
    mutable focused_rev_callstacks: Gui_callstacks_filters.filter
      (* reverse of the callstacks currently being focused. On all tabs,
         the focused callstacks are the only ones that are shown. On the
         'Selection' tab, they are also used to refine the states that
         are being shown, as well as 'go to callers', etc. *);
    mutable filters: filter list;
    mutable full_callstacks_height: bool (* Set to 'true' to expand rows so
                                       that the entire callstacks are shown *);
    mutable show_consolidated: bool (* show results in consolidated state *);
    mutable show_by_callstacks: bool (* show results by callstacks *);
    mutable hidden_columns: column_type list (* columns hidden by the user *);
  }

  (* Is there a filter on the column? *)
  let column_has_filter model col_type =
    match col_type with
    | CEmpty -> false
    | CCallstack -> model.focused_rev_callstacks <> None
    | CBefore e | CAfter e | CAlarm e ->
      let has (e', _, f) =
        gui_selection_equal e e' &&
        (match f, col_type with
         | FilterBefore _, CBefore _ | FilterAfter _, CAfter _
         | FilterAlarm _, CAlarm _ -> true
         | _ -> false)
      in
      List.exists has model.filters

  let data_matches_filter data pos col =
    let ok =
      match col with
      | FilterAlarm a -> data.Gui_eval.alarm = a
      | FilterBefore r -> equal_gui_res r data.Gui_eval.before
      | FilterAfter r -> equal_gui_after r data.Gui_eval.after
    in
    if pos then ok else not ok

  let row_matches_filter row (expr, pos, col: filter) =
    try
      let data = list_assoc gui_selection_equal expr row.exprs in
      data_matches_filter data pos col
    with Not_found -> (* should not happen *) false

  let filters_match row filters =
    List.for_all (row_matches_filter row) filters

  class type cm_panel = object
    method model: model

    method start_session: gui_loc -> multiple:bool -> unit
     (* clear the model, except in "multiple" view, in which case multiple
        localizable on the same location are stacked *)

    method add_data:
      gui_selection -> gui_callstack -> Gui_eval.gui_selection_data -> unit

    method render_session: unit -> unit
    (* display the current model, taking current filter settings into account *)

    method clear: unit -> unit (* reset both model and widget *)

    method expand_row_for_callstacks: bool -> unit
    method show_consolidated: bool -> unit
    method show_by_callstacks: bool -> unit
    (* These three methods are called by the 'Values' panel when the
       corresponding checkboxes are set or unset *)

    method clone: model -> unit
  end

  (* Fetch the internal (hidden) GtkButton of the column header. *)
  let get_column_header_button (col: GTree.view_column) =
    let rec get_button = function
      | None -> None
      | Some w ->
        if w#misc#get_type = "GtkButton"
        then
          let but_props = GtkButtonProps.Button.cast w#as_widget in
          Some (new GButton.button but_props)
        else get_button w#misc#parent
    in
    get_button col#widget

  let show_icon (icon: GMisc.image) = fun ~filtered -> match filtered with
    | true -> icon#misc#show ()
    | false -> icon#misc#hide ()


  module Data = Indexer.Make(
    struct
      type t = int*row
      let compare (x,_) (y,_) = Pervasives.compare x y
    end)

  (* This function creates a single GTree that displays per-callstack
     results *)
  let make_panel (main_ui:main_ui) ~callback_focus_callstack ~show_consolidated ~show_by_callstacks ~full_callstacks_height =
    let gtk_model =
      object(self)
        val mutable m = Data.empty
        val mutable age = 0
        method data = m 
        method size =  Data.size m
        method index i = Data.index i m
        method get i = Data.get i m
        method add i = age<-age+1; m <- Data.add (age,i) m;age,i
        method reload = age<-0; m <- Data.empty
        method coerce = (self:> (int*row) Gtk_helper.Custom.List.model)
      end
    in
    let frame = GBin.frame ~shadow_type:`ETCHED_OUT () in
    let w = new Gtk_helper.Custom.List.view 
      ~packing:frame#add ~headers:true ~rules:true gtk_model#coerce
    in
    w#view#set_fixed_height_mode false;
    let model = {
      loc = None; rows = GCallstackMap.empty;
      row_selected = None; focused_rev_callstacks = None; filters = [];
      full_callstacks_height; all_exprs = [];
      columns_type = HColumns.create 8; show_by_callstacks;
      show_consolidated; hidden_columns = [];
    } in
    let row_is_visible row  =
      match row.callstack,
            model.show_consolidated,
            model.show_by_callstacks,
            model.focused_rev_callstacks
      with
        | GC_Consolidated, false, _, _ 
        | GC_Callstack _, _, false, _
        | GC_Single _, false, false, _
        | GC_Consolidated, _, _, Some _ -> false
        | (GC_Single _ | GC_Callstack _), _, _, (Some _ as cs') ->
          Gui_callstacks_filters.callstack_matches cs' row.rev_callstack &&
          filters_match row model.filters
        | _ -> filters_match row model.filters
    in
    (* Context menu to hide and show columns *)
    let filter_menu (menu: GMenu.menu Lazy.t) =
      let process column (col_type, _icon) =
        try
          let txt = match col_type with
            | CBefore e ->
              Pretty_utils.sfprintf "'%a'  (before)" pretty_gui_selection e
            | CAfter e ->
              Pretty_utils.sfprintf "'%a'  (after)" pretty_gui_selection e
            | CAlarm e ->
              Pretty_utils.sfprintf "'%a'  (alarms)" pretty_gui_selection e
            | CCallstack | CEmpty -> raise Not_found
          in
          if column#visible ||
             list_mem equal_column_type col_type model.hidden_columns
          then
            let show = GMenu.check_menu_item ~label:txt () in
            show#set_show_toggle true;
            show#set_active column#visible;
            (* Hide this column. Keep it alive for filters and co. *)
            let callback_show_hide () =
              if column#visible then begin
                model.hidden_columns <- col_type :: model.hidden_columns;
                column#set_visible false
              end else begin
                model.hidden_columns <-
                  list_remove equal_column_type col_type model.hidden_columns;
                column#set_visible true
              end;
            in
            ignore (show#connect#activate callback_show_hide);
            (!!menu)#add (show :> GMenu.menu_item);
        with Not_found -> ()
      in
      HColumns.iter process model.columns_type;
    in
    (* Add a custom title to the column: a text, an icon indicating that
       the column is filtered, and a tooltip. Returns the filter icon *)
    let add_column_header (col: GTree.view_column) text tooltip =
      col#set_min_width 25;
      col#set_clickable true;
      let h = GPack.hbox () in
      let _lbl = GMisc.label ~text ~packing:h#pack () in
      let icon = GMisc.image ~xpad:10 ~stock:`COLOR_PICKER ~packing:h#pack () in
      icon#misc#hide ();
      let tooltip_before = GData.tooltips () in
      tooltip_before#set_tip ~text:tooltip h#coerce;
      (* set_widget forces Gtk to create a header button for the view_column. *)
      col#set_widget (Some h#coerce);
      icon
    in
    let add_column_header_callback col mk_menu =
      let pop_menu () =
        let menu = lazy (GMenu.menu ()) in
        List.iter (fun elem -> elem menu) mk_menu;
        if Lazy.is_val menu then begin
          let time = GtkMain.Main.get_current_event_time () in
          (!!menu)#popup ~button:3 ~time
        end
      in
      match get_column_header_button col with
      | None -> ignore (col#connect#clicked pop_menu) (* TODO: warn *)
      | Some button ->
        (* Connect the callback to a right-click *)
        let callback evt =
          if GdkEvent.Button.button evt = 3 then begin
            pop_menu (); true
          end else false
        in
        ignore (button#event#connect#button_release ~callback)
    in
    let col_callstack =
      w#add_column_text ~title:"Callstack" [`YALIGN 0.0]
        (fun (_,{callstack=stack; selected}) ->
           let height =
             if selected <> RUnselected || model.full_callstacks_height
             then -1 else 1
           in
           let text = match stack with
             | GC_Filtered -> [`TEXT "filters active"; `STYLE `ITALIC]
             | GC_Consolidated -> [`TEXT "all"; `STYLE `ITALIC]
             | GC_Single stack | GC_Callstack stack ->
               let pp_text = if selected = RSelectedCallstackCol
                 then Pretty_utils.to_string pretty_callstack
                 else Pretty_utils.to_string ~margin:50 pretty_callstack_short
               in
               [`TEXT (pp_text stack); `STYLE `NORMAL]
           in
           [`HEIGHT height] @ text)
    in
    let col_empty = w#add_column_empty in
    let clear_widget remove_columns =
      Extlib.may (fun (_, r) -> r.selected <- RUnselected) model.row_selected;
      model.row_selected <- None;
      if remove_columns then begin
        model.all_exprs <- [];
        (* Clear out all columns except 'Callstacks' and "empty": clear
           everything, then restore those two. *)
        let data_col_cs = HColumns.find model.columns_type col_callstack in
        let data_col_empty = HColumns.find model.columns_type col_empty in
        HColumns.iter (fun column (col_typ, _) ->
            if col_typ <> CCallstack && col_typ <> CEmpty then
              ignore (w#view#remove_column column)
          ) model.columns_type;
        HColumns.clear model.columns_type;
        HColumns.add model.columns_type col_callstack data_col_cs;
        HColumns.add model.columns_type col_empty data_col_empty;
      end;
      (* Post a reload request before clearing.
         The current model is used to know how many rows
         must be deleted. *)
      w#reload ;
    in
    let clear_model () =
      clear_widget true;
      model.loc <- None;
      model.rows <- GCallstackMap.empty;
      model.filters <- [];
      model.hidden_columns <- [];
    in
    let start_session loc ~multiple =
      if not (multiple && Extlib.opt_equal gui_loc_equal (Some loc) model.loc)
      then begin
        clear_model ();
        model.loc <- Some loc;
      end
    in
    let rec add_columns expr =
      let expr_string = Pretty_utils.to_string pretty_gui_selection expr in
      let _expr_string_short =
        if String.length expr_string >= 15 then
          String.sub expr_string 0 15 ^ ".."
        else expr_string
      in
      (* 'Before' column *)
      let col_before = w#add_column_text [`YALIGN 0.0]
          (fun (_, row) ->
             let data = find_data row expr in
             [`TEXT !!(data.Gui_eval.before_string)])
      in
      let tip_before =
        Printf.sprintf "Value of '%s' before the current point" expr_string
      in
      (* 'Alarm column *)
      let show_alarm_col = ref (fun () -> ()) in
      let col_alarm =
        w#add_column_pixbuf [`YALIGN 0.0;`XALIGN 0.5]
          (fun (_, row) ->
             let data = find_data row expr in
             if data.Gui_eval.alarm then begin
               !show_alarm_col ();
               [`STOCK_ID  "gtk-dialog-warning"]
             end else [])
      in
      show_alarm_col := (fun () ->
          let ct = CAlarm expr in
          if not (list_mem equal_column_type ct model.hidden_columns) then
            col_alarm#set_visible true);
      col_alarm#set_visible false;
      let tip_alarm =
        Printf.sprintf "Does evaluation of '%s' always succeed?" expr_string
      in
      (* 'After column *)
      let show_after_col = ref (fun () -> ()) in
      let col_after = w#add_column_text [`YALIGN 0.0]
          (fun (_, row) ->
             let data = find_data row expr in
             match data.Gui_eval.after with
             | GA_NA -> [`TEXT "n/a"; `STYLE `ITALIC]
             | GA_Unchanged -> [`TEXT "unchanged"; `STYLE `ITALIC]
             | GA_After _ ->
               !show_after_col ();
               [`TEXT !!(data.Gui_eval.after_string); `STYLE `NORMAL])
      in
      col_after#set_visible false;
      let title_after = expr_string ^ " (after)" in
      let tip_after =
        Printf.sprintf "Value of '%s' after the current point" expr_string
      in
      show_after_col := (fun () ->
          let ct = CAfter expr in
          if not (list_mem equal_column_type ct model.hidden_columns) then
            col_after#set_visible true);
      (* This is the menu displayed when the user left-clicks on the header of
         one of the three columns *)
      let menu_on_expr col_type (icon: GMisc.image)  (menu: GMenu.menu Lazy.t)=
        let has_filters = column_has_filter model col_type in
        let txt_remove_col =
          Pretty_utils.sfprintf "Remove all columns for '%a'%s"
            pretty_gui_selection expr
            (if has_filters then " (including filters)" else "")
        in
        let remove = GMenu.menu_item ~label:txt_remove_col () in
        (!!menu)#add remove;
        let callback_remove_filters () =
          icon#misc#hide ();
          let filters' = remove_filter expr model.filters in
          let filters_changed = filters' != model.filters in
          model.filters <- filters';
          if filters_changed then render_session ()
        in
        (* Remove all the columns related to 'expr' *)
        let callback_remove () =
          model.all_exprs <-
            list_remove gui_selection_equal expr model.all_exprs;
          HColumns.iter
            (fun col (col_type, _) ->
               match col_type with
               | CBefore e | CAfter e | CAlarm e
                 when gui_selection_equal expr e ->
                 ignore (w#view#remove_column col);
                 HColumns.remove model.columns_type col;
               | _ -> ()
            ) model.columns_type;
          callback_remove_filters ();
        in
        ignore (remove#connect#activate callback_remove);
        if has_filters then begin
          let txt_unfilter = "Remove filters on this column" in
          let unfilter = GMenu.menu_item ~label:txt_unfilter () in
          (!!menu)#add unfilter;
          ignore (unfilter#connect#activate callback_remove_filters);
        end;
      in
      let aux_expr_column (col: GTree.view_column) coltype txt tip =
        let icon = add_column_header col txt tip in
        let mk_menu = [
          menu_on_expr coltype icon;
          filter_menu
        ] in
        add_column_header_callback col mk_menu;
        HColumns.add model.columns_type col (coltype, show_icon icon);
      in
      aux_expr_column col_before (CBefore expr) expr_string tip_before;
      aux_expr_column col_alarm (CAlarm expr) "  " tip_alarm;
      aux_expr_column col_after (CAfter expr) title_after tip_after;
    and add_data expr callstack data =
      (* If the expression has never been displayed before, create the
         columns *)
      if not (list_mem gui_selection_equal expr model.all_exprs) then begin
        add_columns expr;
        model.all_exprs <- expr :: model.all_exprs
      end;
      model.rows <- add_data_to_rows model.rows callstack expr data
    and render_session () =
      clear_widget false;
      let has_visible_row = ref false in
      GCallstackMap.iter (fun _cs row ->
          if row_is_visible row then begin
            has_visible_row := true;
            w#insert_row (gtk_model#add row);
          end;
        ) model.rows;
      if not !has_visible_row && not (GCallstackMap.is_empty model.rows) then
        (* Add a special row to indicate that some things are hidden by
           filters. This row is intentionnaly only added to the view, but
           not to the model *)
        w#insert_row (gtk_model#add (row_unfocused ()));
      GtkTree.TreeView.columns_autosize w#view#as_tree_view;
    in
    (* Callback called when a callstack is focused or unfocused *)
    let callback_focus_unfocus lcs icon () =
      let conv = List.map Gui_callstacks_filters.from_callstack in
      let lrcs = Extlib.opt_map conv lcs in
      callback_focus_callstack lrcs;
      icon ~filtered:(lcs <> None);
      model.focused_rev_callstacks <- lrcs;
      render_session ();
    in
    (* Add 'Unfocus callstacks' option to menu. *)
    let add_unfocus_callstacks menu icon =
      if Extlib.has_some model.focused_rev_callstacks then begin
        let unfocus = GMenu.menu_item ~label:"Unfocus callstack(s)" () in
        (!!menu)#add unfocus;
        ignore (unfocus#connect#activate (callback_focus_unfocus None icon))
      end;
    in
    (* Add 'Focus on all displayed callstacks' to menu *)
    let add_focus_all_callstacks menu icon =
      let visible_callstack cs row acc =
        match cs with
        | GC_Single cs | GC_Callstack cs ->
          if row_is_visible row then cs :: acc else acc
        | _ -> acc
      in
      let callstacks = GCallstackMap.fold visible_callstack model.rows [] in
      if List.length callstacks > 1 then
        let focus_all = GMenu.menu_item ~label:"Focus on all \
                                                displayed callstacks" () in
        (!!menu)#add focus_all;
        ignore (focus_all#connect#activate
                  (callback_focus_unfocus (Some callstacks) icon));
    in
    let tip_callstack = "Callstacks at which the selection was analyzed" in
    let icon_callstack =
      add_column_header col_callstack "Callstack" tip_callstack
    in
    let mk_menu_header_callstack menu =
      add_unfocus_callstacks menu (show_icon icon_callstack);
      add_focus_all_callstacks menu (show_icon icon_callstack);
      filter_menu menu;
    in
    add_column_header_callback col_callstack [mk_menu_header_callstack];
    HColumns.add model.columns_type col_callstack
      (CCallstack, show_icon icon_callstack);
    HColumns.add model.columns_type col_empty
      (CEmpty, (fun ~filtered:_ -> ()));
    let clone model' =
      clear_model () (* resets row_selected + the widget itself *);
      model.loc <- model'.loc;
      model.all_exprs <- model'.all_exprs;
      (* Recreate the columns, in particular the field 'columns_type' *)
      List.iter add_columns (List.rev model'.all_exprs);
      model.rows <-
        GCallstackMap.map
          (fun r -> { r with selected = RUnselected}) model'.rows;
      model.focused_rev_callstacks <- model'.focused_rev_callstacks;
      model.filters <- model'.filters;
      HColumns.iter (fun _col (coltype, icon) ->
          icon ~filtered:(column_has_filter model' coltype)
        ) model.columns_type;
      model.full_callstacks_height <- model'.full_callstacks_height;
      model.show_consolidated <- model'.show_consolidated;
      model.show_by_callstacks <- model'.show_by_callstacks;
      render_session ()
    in
    (* This is the menu which is displayed when the user right-clicks
       on a data column. It can be used to filter lines *)
    let popup_menu_filter expr v icon =
      let menu = GMenu.menu () in
      let callback_only_except oe () =
        let filter = expr, oe, v in
        model.filters <- filter :: model.filters;
        icon ~filtered:true;
        render_session ();
      in
      let equal = GMenu.menu_item ~label:"Only equal" () in
      let different = GMenu.menu_item ~label:"Only different" () in
      menu#add equal;
      menu#add different;
      ignore (equal#connect#activate (callback_only_except true));
      ignore (different#connect#activate (callback_only_except false));
      let time = GtkMain.Main.get_current_event_time () in
      menu#popup ~button:3 ~time
    in
    (* Updates the selection state of the given row. *)
    let update_selected select (_, row as irow) =
      row.selected <- select;
      w#update_row irow;
    in
    w#on_click
      (fun (_, row as irow) column ->
         (* Update the height of the selected and deselected rows. *)
         (* First, unselect the previous row, if it was not 'row' itself
            (avoids flickering *)
         begin match model.row_selected with
         | Some (_, row_old as irow_old) when row_old != row ->
           update_selected RUnselected irow_old;
         | _ -> ()
         end;
         (* Newt, update 'row' *)
         update_selected (if GColumn.equal column col_callstack
                          then RSelectedCallstackCol
                          else RSelected) irow;
         model.row_selected <- Some irow;
         (* Dump the clicked cell on the "Information" tab, for copy-pasting
            and/or selection *)
         let dump s pp v = main_ui#pretty_information "@.%s:@.%a@." s pp v in
         match HColumns.find model.columns_type column with
         | CCallstack, _ -> begin
             match row.callstack with
             | GC_Single stack | GC_Callstack stack ->
               dump "Stack" pretty_callstack stack
             | GC_Consolidated | GC_Filtered -> ()
           end
         | CBefore expr, _ ->
           let data = find_data row expr in
           dump "Value before" pretty_gui_res data.Gui_eval.before
         | CAfter expr, _ -> begin
             let data = find_data row expr in
             match data.Gui_eval.after with
             | GA_After after -> dump "Value after" pretty_gui_res after
             | GA_NA | GA_Unchanged -> ()
           end
         | CAlarm _, _ | CEmpty, _ -> ()
      );
    w#on_right_click
      (fun (_, row) column ->
         match HColumns.find model.columns_type column with
         | CCallstack, icon -> begin
             let menu = lazy (GMenu.menu ()) in
             (* Add 'Focus' option when a callstack is selected *)
             begin match row.callstack with
             | GC_Single cs | GC_Callstack cs ->
               let focus = GMenu.menu_item ~label:"Focus on this callstack"() in
               (!!menu)#add focus;
               ignore (focus#connect#activate
                         (callback_focus_unfocus (Some [cs]) icon));
             | GC_Filtered | GC_Consolidated -> ()
             end;
             add_focus_all_callstacks menu icon;
             add_unfocus_callstacks menu icon;
             (* Popup the menu only if something as been added *)
             if Lazy.is_val menu then
               let time = GtkMain.Main.get_current_event_time () in
               (!!menu)#popup ~button:3 ~time
           end
         | CBefore expr, icon ->
           let data = find_data row expr in
           if data.Gui_eval.before <> GR_Empty then
             popup_menu_filter expr (FilterBefore data.Gui_eval.before) icon
         | CAfter expr, icon ->
           let data = find_data row expr in
           if data.Gui_eval.before <> GR_Empty then
             popup_menu_filter expr (FilterAfter data.Gui_eval.after) icon
         | CAlarm expr, icon ->
           let data = find_data row expr in
           if data.Gui_eval.before <> GR_Empty then
             popup_menu_filter expr (FilterAlarm data.Gui_eval.alarm) icon
         | CEmpty, _ -> ()
      );
    frame,
    (object
      method model = model
      method add_data = add_data
      method render_session = render_session
      method start_session = start_session
      method clear = clear_model
      method clone = clone

      method expand_row_for_callstacks expand =
        if model.full_callstacks_height <> expand then begin
          model.full_callstacks_height <- expand;
          render_session ()
        end

      method show_by_callstacks show =
        if model.show_by_callstacks <> show then begin
          model.show_by_callstacks <- show;
          render_session ()
        end

      method show_consolidated show =
        if model.show_consolidated <> show then begin
          model.show_consolidated <- show;
          render_session ()
        end
    end: cm_panel)

  class type t = object
    (* Add something to view on the 'Selection' tab for the given statement  *)
    method display_at_loc:
      gui_loc -> (* append * display *) 
      (gui_selection -> gui_callstack -> Gui_eval.gui_selection_data -> unit) *
      (unit -> unit)

    (* Full reset, that should be called on project change *)
    method reset: unit -> unit

    (* Clear the 'default' tab, for example on selection change. *)
    method clear_default: unit -> unit

    method focused_rev_callstacks: Gui_callstacks_filters.filter

    (* Set focus on the 'Selection' tab, itself in the 'Values tab *)
    method focus_selection_tab: unit -> unit
  end

  module HWidget = Hashtbl.Make(struct
      type t = GObj.widget
      let hash w = Gobject.get_oid w#as_widget
      let equal w1 w2 =
        Gobject.get_oid w1#as_widget = Gobject.get_oid w2#as_widget
    end)

  (* This function creates the buttons at the top of "Values" tab, plus
     a tab control suitable for displaying multiple cm_panel *)
  let make (main_ui:main_ui) ~packing =
    let vpaned = GPack.vbox ~packing ~homogeneous:false () in
    let hbox_filters = GPack.hbox ~packing:(vpaned#pack ~expand:false) () in
    let chk_multiple = new Toolbox.checkbox ~label:"Multiple selections"
      ~tooltip:"Allow the selection of multiple expressions on the same \
                statement" ()
    in
    let chk_consolidated = new Toolbox.checkbox ~label:"Consolidated value"
      ~tooltip:"Show values consolidated accross all callstacks" ()
    in
    let chk_callstacks = new Toolbox.checkbox ~label:"Per callstack"
      ~tooltip:"Show values per callstack" ()
    in
    let chk_rows_height = new Toolbox.checkbox ~label:"Expand rows"
      ~tooltip:"Expand rows to fit the 'Callstack' column" ()
    in
    let key_multiple = "Value.multiple_selections" in
    let key_consolidated = "Value.show_consolidated" in
    let key_by_callstacks = "Value.show_by_callstacks" in
    let key_rows_height = "Value.expand_rows" in
    let get_bool k default = Gtk_helper.Configuration.find_bool ~default k in
    let save_bool k v = Gtk_helper.Configuration.(set k (ConfBool v)) in
    chk_multiple#set (get_bool key_multiple false);
    chk_consolidated#set (get_bool key_consolidated true);
    chk_callstacks#set (get_bool key_by_callstacks true);
    chk_rows_height#set (get_bool key_rows_height false);
    hbox_filters#pack chk_multiple#coerce;
    hbox_filters#pack ~from:`END chk_callstacks#coerce;
    hbox_filters#pack ~from:`END chk_consolidated#coerce;
(*    let lbl_filters = GMisc.label ~markup:"Filters: " () in
    hbox_filters#pack ~from:`END lbl_filters#coerce; *)
    hbox_filters#pack ~from:`END chk_rows_height#coerce;
    let tabs =
      GPack.notebook ~scrollable:true ~packing:(vpaned#pack ~expand:true) ()
    in
    vpaned#misc#set_sensitive (Db.Value.is_computed ());
    let pack_tab ?lbl w = ignore (tabs#insert_page ?tab_label:lbl w) in
    let lbl_pane_default = GPack.hbox () in
    let clear_button = new Toolbox.button ~icon:`CLEAR ~tooltip:"Clear" () in
    clear_button#set_enabled false;
    let save_button = new Toolbox.button ~icon:`SAVE ~tooltip:"Save" () in
    save_button#set_enabled false;
    let lbl = GMisc.label ~markup:"Selection" () in
    lbl_pane_default#pack lbl#coerce;
    lbl_pane_default#pack clear_button#coerce;
    lbl_pane_default#pack save_button#coerce;
    let focus_selection_tab () =
      let n = main_ui#lower_notebook#page_num vpaned#coerce in
      main_ui#lower_notebook#goto_page n;
      tabs#goto_page 0
    in
    let focused_rev_callstacks = ref None in
    (* Callback for the "Selection" tab: set the selected callstacks as filters,
       plus rehighlight the source text (for dead code, etc) *)
    let callback_focus_callstack lrcs =
      Gui_callstacks_filters.set_callstacks_filter lrcs;
      focused_rev_callstacks := lrcs;
      main_ui#rehighlight ()
    in
    let make_panel ?(callback_focus_callstack=fun _ -> ()) () =
      make_panel main_ui
        ~callback_focus_callstack
        ~show_consolidated:chk_consolidated#get
        ~show_by_callstacks:chk_callstacks#get
        ~full_callstacks_height:chk_rows_height#get
    in
    let pane_default, model_default = make_panel ~callback_focus_callstack () in
    pack_tab ~lbl:lbl_pane_default#coerce pane_default#coerce;
    let hash_tabs = HWidget.create 16 in
    let notify_switch_page n =
      (* Hide the buttons of all the tabs not focused *)
      HWidget.iter (fun _ (_, bt1, bt2) -> bt1#hide (); bt2#hide ()) hash_tabs;
      if n = 0 then begin (* 'Selection' tab is focused *)
        clear_button#coerce#misc#show ();
        save_button#coerce#misc#show ();
      end else
        let w = tabs#get_nth_page n in
        clear_button#coerce#misc#hide ();
        save_button#coerce#misc#hide ();
        try
          (* Show the buttons of the current tab *)
          let _, bt1, bt2 = HWidget.find hash_tabs w in
          bt1#show (); bt2#show ();
        with Not_found -> () (* should not happen *)
    in
    ignore (tabs#connect#switch_page ~callback:notify_switch_page);
    let callback_save_button () =
      let model = model_default#model in
      match model.loc with
      | Some loc ->
        let txt = match loc with
          | GL_Stmt (kf, stmt) ->
            Pretty_utils.sfprintf "%a:%d"
              Kernel_function.pretty kf
              (fst (Cil_datatype.Stmt.loc stmt)).Lexing.pos_lnum
          | GL_Pre kf ->
            Pretty_utils.sfprintf "pre %a" Kernel_function.pretty kf
          | GL_Post kf ->
            Pretty_utils.sfprintf "post %a" Kernel_function.pretty kf
        in
        let hb = GPack.hbox () in
        ignore (GMisc.label ~packing:hb#pack ~markup:txt ());
        let button_delete =
          new Toolbox.button ~icon:`DELETE ~tooltip:"Delete" ()
        in
        hb#pack button_delete#coerce;
        button_delete#coerce#misc#hide ();
        let button_edit =
          new Toolbox.button ~icon:`EDIT ~tooltip:"Edit" ()
        in
        hb#pack button_edit#coerce;
        button_edit#coerce#misc#hide ();
        let tab, model_tab = make_panel () in
        button_delete#connect (fun () ->
            let n = tabs#page_num tab#coerce in
            tabs#remove_page n
          );
        button_edit#connect (fun () ->
            let loc = match loc with
              | GL_Stmt (kf, stmt) ->
                Pretty_source.PStmt (kf, stmt)
              | GL_Pre kf | GL_Post kf ->
                let vi = Kernel_function.get_vi kf in
                Pretty_source.PVDecl (Some kf, vi)
            in
            main_ui#scroll loc;
            model_default#clone model_tab#model;
            tabs#goto_page 0;
            callback_focus_callstack model_default#model.focused_rev_callstacks;
          );
        HWidget.add hash_tabs tab#coerce
          (model_tab, button_edit#coerce#misc, button_delete#coerce#misc);
        pack_tab ~lbl:hb#coerce tab#coerce;
        model_tab#clone model_default#model
      | None -> ()
    in
    save_button#connect callback_save_button;
    let on_all f =
      f model_default;
      HWidget.iter (fun _ (model, _, _) -> f model) hash_tabs;
    in
    (* Clear the 'default' tab *)
    let clear_default () =
      clear_button#set_enabled false;
      save_button#set_enabled false;
      model_default#clear ()
    in
    clear_button#connect clear_default;
    chk_multiple#connect (fun b -> save_bool key_multiple b);
    chk_consolidated#connect (fun b ->
        save_bool key_consolidated b;
        on_all (fun model -> model#show_consolidated b));
    chk_callstacks#connect (fun b ->
        save_bool key_by_callstacks b;
        on_all (fun model -> model#show_by_callstacks b));
    chk_rows_height#connect (fun b ->
        save_bool key_rows_height b;
        on_all (fun model -> model#expand_row_for_callstacks b));
    (* Clear all the tabs *)
    let reset () =
      (* the method get_n_pages is missing in Lablgtk. Instead, we iterate
         over the hash tables of panels to remove the non-'Selection' ones *)
      HWidget.iter (fun w _ -> tabs#remove_page (tabs#page_num w)) hash_tabs;
      HWidget.clear hash_tabs;
      clear_default ();
      vpaned#misc#set_sensitive (Db.Value.is_computed ());
    in
    (object
      method display_at_loc loc =
        clear_button#set_enabled true;
        save_button#set_enabled true;
        model_default#start_session loc ~multiple:chk_multiple#get;
        model_default#add_data, (fun () -> model_default#render_session ())

      method reset = reset

      method clear_default = clear_default

      method focused_rev_callstacks = !focused_rev_callstacks

      method focus_selection_tab = focus_selection_tab
    end : t)

end

let display_eval_errors (main_ui:main_ui) l =
  let pp = function
    | Eval_terms.LogicEvalError ee ->
      main_ui#pretty_information "Cannot evaluate: %a@."
        Eval_terms.pretty_logic_evaluation_error ee
    | e ->
      main_ui#pretty_information "Unknown error during evaluation (%s)@."
        (Printexc.to_string e)
  in
  List.iter pp l

let select_loc main_ui (cm: Callstacks_manager.t) ev loc v =
  match Gui_eval.callstacks_at_gui_loc loc with
  | None -> ()
  | Some { Gui_eval.states_before = before; states_after = after } ->
    let append, display = cm#display_at_loc loc in
    let errors = Gui_eval.make_data_all_callstacks append ev ~before ~after v in
    display_eval_errors main_ui errors;
    display ();
;;

let select_lv main_ui cm loc lv =
  select_loc main_ui cm Gui_eval.lval_ev loc lv
let select_null main_ui cm loc =
  select_loc main_ui cm Gui_eval.null_ev loc ()
let select_exp main_ui cm loc exp =
  select_loc main_ui cm Gui_eval.exp_ev loc exp
let select_term main_ui cm loc t =
  select_loc main_ui cm (Gui_eval.term_ev loc) loc t
let select_tlv main_ui cm loc tlv =
  select_loc main_ui cm (Gui_eval.tlval_ev loc) loc tlv
let select_predicate main_ui cm loc p =
  select_loc main_ui cm (Gui_eval.predicate_ev loc) loc p

(** Core of the graphical interface. *)

let gui_compute_values (main_ui:main_ui) =
  if not (Db.Value.is_computed ())
  then main_ui#launcher ()

let cleant_outputs kf s =
  let outs = Db.Outputs.kinstr (Kstmt s) in
  let accept =
    Callgraph.Uses.accept_base ~with_formals:true ~with_locals:true kf
  in
  let filter = Locations.Zone.filter_base accept in
  Extlib.opt_map filter outs

type term_or_pred = Term | Pred

let pp_term_or_pred fmt = function
  | Term -> Format.pp_print_string fmt "term"
  | Pred -> Format.pp_print_string fmt "predicate"

(* Evaluate the user-supplied term contained in the string [txt] *)
let eval_user_term_predicate (main_ui:main_ui) cm loc tp txt =
  let kf = kf_of_gui_loc loc in
  Cil.CurrentLoc.set (gui_loc_loc loc);
  try
    cm#focus_selection_tab ();
    let env = Gui_eval.gui_loc_logic_env loc in
    match tp with
    | Term -> begin
      if txt = "NULL" then
        select_null main_ui cm loc
      else
        let term = !Db.Properties.Interp.term ~env kf txt in
        match term.term_node with
        | TLval _ | TStartOf _ -> select_tlv main_ui cm loc term
        | _ -> select_term main_ui cm loc term
      end
    | Pred ->
      let pred = !Db.Properties.Interp.predicate ~env kf txt in
      select_predicate main_ui cm loc pred
  with
  | Logic_interp.Error (_, mess) ->
    main_ui#error "Invalid %a: %s" pp_term_or_pred tp mess
  | Parsing.Parse_error ->
    main_ui#error "Invalid %a: Parse error" pp_term_or_pred tp
  | Eval_terms.LogicEvalError ee ->
    main_ui#error "Cannot evaluate %a (%a)"
      pp_term_or_pred tp Eval_terms.pretty_logic_evaluation_error ee
  | e ->
    main_ui#error "Invalid %a: %s" pp_term_or_pred tp (Cmdline.protect e)

let last_evaluate_acsl_request = ref ""

(* Opens a modal dialog asking for an ACSL expression and evaluates it
   at location [loc]. *)
let eval_acsl_term_pred main_ui cm loc tp () =
  let txt =
    GToolbox.input_string ~title:"Evaluate"
      ~text:!last_evaluate_acsl_request
      (Pretty_utils.sfprintf "  Enter an ACSL %a to evaluate  "
         pp_term_or_pred tp)
      (* the spaces at beginning and end should not be necessary
         but are the quickest fix for an aesthetic GTK problem *)
  in
  match txt with
  | None -> ()
  | Some txt ->
    last_evaluate_acsl_request:=txt;
    eval_user_term_predicate main_ui cm loc tp txt

let pretty_kf_escaped kf =
  Pretty_utils.(escape_underscores (to_string Kernel_function.pretty kf))

(* popup a menu to jump the definitions of the given functions *)
let menu_go_to_fun_definition (main_ui:main_ui) (popup_factory:menu) funs =
  let aux kf =
    try
      let g = Kernel_function.get_global kf in
      ignore
        (popup_factory#add_item
           ("Go to definition of " ^ pretty_kf_escaped kf ^ " (indirect)")
           ~callback:(fun () -> main_ui#select_or_display_global g))
    with Not_found -> ()
  in
  Kernel_function.Hptset.iter aux funs

(* popup a menu to jump to the definitions of the callers *)
let menu_go_to_callers (main_ui:main_ui) (menu:menu) csf kf =
  try
    let aux (menu:menu) (kf, call_sites) =
      let nb_sites = List.length call_sites in
      let label = "Go to caller " ^ pretty_kf_escaped kf in
      let label =
        if nb_sites > 1 then
          label ^ " (" ^ (string_of_int nb_sites) ^ " call sites)"
        else label
      in
      let callback () =
        let g = Kernel_function.get_global kf in
        main_ui#select_or_display_global g;
        (* We put the cursor in the first call site and add the others (if any)
           to the forward history. *)
        match call_sites with
        | first_call_site :: rest ->
          main_ui#view_stmt first_call_site;
          let other_call_sites =
            List.map (fun call ->
                let kf = Kernel_function.find_englobing_kf call in
                History.Localizable (PStmt (kf, call))
              ) rest
          in
          History.set_forward other_call_sites
        | [] -> assert false (* list was not empty *)
      in
      ignore (menu#add_item ~callback label)
    in
    let aux_focus (acc_focus, acc_unfocus) (kf, call_sites) =
      let focus, unfocus =
        List.partition (Gui_callstacks_filters.callsite_matches csf) call_sites
      in
      (if focus <> [] then (kf, focus) :: acc_focus else acc_focus),
      (if unfocus <> [] then (kf, unfocus) :: acc_unfocus else acc_unfocus)
    in
    let focused, unfocused =
      List.fold_left aux_focus ([], []) (!Db.Value.callers kf)
    in
    List.iter (aux menu) focused;
    if unfocused <> [] then
      let submenu = GMenu.menu () in
      let item =
        GMenu.menu_item ~label:"Callers in unselected callstack(s)" ()
      in
      item#set_submenu submenu;
      menu#menu#add item;
      let factory = new GMenu.factory submenu in
      List.iter (aux factory) unfocused
  with Not_found -> ()

let pretty_stmt_info (main_ui:main_ui) kf stmt =
  (* Is it an accessible statement ? *)
  if Db.Value.is_reachable_stmt stmt then begin
    if Value_results.is_non_terminating_instr stmt then
      match stmt.skind with
      | Instr (Call (_lvopt, _, _, _)) ->
        (* This is not 100% accurate: the instr can also fail
           when storing the result in [lvopt] *)
        main_ui#pretty_information "This call never terminates.@."
      | Instr _ ->
        main_ui#pretty_information "This instruction always fail.@."
      | _ -> ()
    else
      (* Out for this statement *)
      let outs = cleant_outputs kf stmt in
      match outs with
      | Some outs ->
        main_ui#pretty_information
          "Modifies @[<hov>%a@]@." Db.Outputs.pretty outs
      | _ -> ()
  end
  else main_ui#pretty_information "This code is dead@."

(* Actions to perform when the user has left-clicked, and Value is computed *)
let left_click_values_computed main_ui cm localizable =
  try
    let open Property in
    match localizable with
    | PStmt (kf,stmt) ->
      if Gui_eval.results_kf_computed kf then
        pretty_stmt_info main_ui kf stmt
    | PLval (Some kf, Kstmt stmt,lv) ->
      if not (isFunctionType (typeOfLval lv)) then
        select_lv main_ui cm (GL_Stmt (kf, stmt)) lv
    | PExp (Some kf, Kstmt stmt,e) ->
      select_exp main_ui cm (GL_Stmt (kf, stmt)) e
    | PTermLval (Some kf, Kstmt stmt, _, tlv) ->
      let term = Logic_const.term (TLval tlv) (Cil.typeOfTermLval tlv) in
      select_tlv main_ui cm (GL_Stmt (kf, stmt)) term
    | PTermLval (Some kf, Kglobal, ip, tlv) -> begin
        match Gui_eval.classify_pre_post kf ip with
        | Some loc ->
          let term = Logic_const.term (TLval tlv) (Cil.typeOfTermLval tlv) in
          select_tlv main_ui cm loc term
        | None -> ()
      end
    | PVDecl (Some kf, vi) when vi.vformal ->
      let lv = (Var vi, NoOffset) in
      select_lv main_ui cm (GL_Pre kf) lv
    | PIP (IPCodeAnnot (kf, stmt,
           {annot_content = AAssert (_, p) | AInvariant (_, true, p)} )) ->
      select_predicate main_ui cm (GL_Stmt (kf, stmt)) p
    | PIP (IPPredicate (_, kf, Kglobal, p) as ip) -> begin
        match Gui_eval.classify_pre_post kf ip with
        | None -> ()
        | Some loc ->
          select_predicate main_ui cm loc (Logic_const.pred_of_id_pred p)
      end
    | PLval ((_ , Kglobal, _) | (None, Kstmt _, _))
    | PExp ((_,Kglobal,_) | (None, Kstmt _, _))
    | PTermLval (None, _, _, _)-> ()
    | PVDecl (_kf,_vi) -> ()
    | PGlobal _  | PIP _ -> ()
  with
  | Eval_terms.LogicEvalError ee ->
    main_ui#pretty_information "Cannot evaluate term: %a@."
      Eval_terms.pretty_logic_evaluation_error ee

(* Actions to perform when the user has right-clicked, and Value is computed *)
let right_click_values_computed main_ui menu (cm: Callstacks_manager.t) localizable =
  match localizable with
  | PVDecl (Some kf, _) ->
    menu_go_to_callers main_ui menu cm#focused_rev_callstacks kf
  | PStmt (kf,stmt) ->
    if Gui_eval.results_kf_computed kf then
      ignore
        (menu#add_item "_Evaluate ACSL term"
           ~callback:(eval_acsl_term_pred main_ui cm (GL_Stmt (kf, stmt)) Term))
  | PLval (_kfopt, ki, lv) ->
    let ty = typeOfLval lv in
    (* Do special actions for functions *)
    begin
      (match lv with
       | Var _,NoOffset when isFunctionType ty ->
         () (* direcl calls are handled by [Design]. *)
       | Mem _, NoOffset when isFunctionType ty -> begin
           (* Function pointers *)
           (* get the list of functions in the values *)
           let e = Cil.dummy_exp (Lval lv) in
           let funs, _ = Eval_exprs.resolv_func_vinfo
               ~with_alarms:CilE.warn_none_mode None
               (Db.Value.get_state ki) e
           in
           menu_go_to_fun_definition main_ui menu funs
         end
       | _ -> ()
      )
    end
  | PVDecl (None, _) | PExp _ | PTermLval _ | PGlobal _ | PIP _ -> ()

let _right_click_value_not_computed (main_ui:main_ui) (menu:menu) localizable =
  match localizable with
  | PVDecl (_,_) -> begin
      ignore
        (menu#add_item "Compute callers"
           ~callback:(fun () -> (gui_compute_values main_ui)))
    end
  | _ -> ()

let to_do_on_select cm (menu:menu) (main_ui:main_ui) ~button selected =
  if Db.Value.is_computed () then
    if button = 1 then 
      left_click_values_computed main_ui cm selected
    else if button = 3 then
      right_click_values_computed main_ui menu cm selected

module UsedVarState =
  Cil_state_builder.Varinfo_hashtbl
    (Datatype.Bool)
    (struct
      let size = 17
      let name = "Value.Gui.UsedVarState"
      let dependencies = [ Db.Value.self ]
      (* [!Db.Inputs.self_external; !Db.Outputs.self_external] would be better
         dependencies, but this introduces a very problematic recursion between
         Value and Inout *)
    end)

let used_var = UsedVarState.memo
    (fun var ->
       Mark_noresults.no_memoization_enabled () ||
       try
         let f = fst (Globals.entry_point ()) in
         let inputs = !Db.Inputs.get_external f in
         let outputs = !Db.Outputs.get_external f in
         let b = Base.of_varinfo var in
         Locations.Zone.mem_base b inputs || Locations.Zone.mem_base b outputs
       with e ->
         Gui_parameters.error ~once:true
           "Exception during usability analysis of var %s: %s"
           var.vname (Printexc.to_string e);
         true (* No really sane value, so in doubt... *)
    )

(* Set when the callback is installed *)
let hide_unused = ref (fun () -> false)

let sync_filetree (filetree:Filetree.t) =
  if not (!hide_unused ()) then
    (Globals.Functions.iter
       (fun kf ->
          try
            let vi = Kernel_function.get_vi kf in
            let strikethrough =
              Db.Value.is_computed () && not (!Db.Value.is_called kf)
            in
            filetree#set_global_attribute ~strikethrough vi
          with Not_found -> ());
     Globals.Vars.iter
       (fun vi _ ->
          if vi.vsource = true then
            filetree#set_global_attribute
              ~strikethrough:(Db.Value.is_computed () && not (used_var vi))
              vi
       );
     if not (filetree#flat_mode) then
       List.iter
         (fun file ->
            (* the display name removes the path *)
            let name, _globals = Globals.FileIndex.find file in
            let globals_state = filetree#get_file_globals name in
            filetree#set_file_attribute
              ~strikethrough:(Db.Value.is_computed () &&
                              List.for_all snd globals_state)
              name
         )
         (Globals.FileIndex.get_files ())
    )
  else
    (* Some lines may have disappeared. We should reset the entire filetree,
       but the method reset of design.ml already does this. *)
    ()


let hide_unused_function_or_var g =
  !hide_unused () && Db.Value.is_computed () &&
  (match g with
    | GFun ({svar = vi}, _) | GFunDecl (_, vi, _) ->
      let kf = Globals.Functions.get vi in
      not (!Db.Value.is_called kf)
    | GVarDecl (vi, _) | GVar (vi, _, _)  ->
      not (used_var vi)
    | _ -> false
  )

module DegeneratedHighlighted =
  State_builder.Option_ref
    (Pretty_source.Localizable)
    (struct
      let name = "Value_gui.DegeneratedHighlightedState"
      let dependencies = [ Ast.self ]
    end)

let value_panel (main_ui:main_ui) =
  let box = GPack.vbox () in
  let run_button = GButton.button ~label:"Run" ~packing:(box#pack) () in
  let w =
    GPack.table ~packing:(box#pack ~expand:true ~fill:true) ~columns:2 ()
  in
  let box_1_1 = GPack.hbox ~packing:(w#attach ~left:1 ~top:1) () in
  let slevel_refresh =
    let tooltip =
      Value_parameters.SemanticUnrollingLevel.parameter.Typed_parameter.help
    in
    Gtk_helper.on_int ~lower:0 ~upper:1000000 ~tooltip
      box_1_1 "slevel"
      Value_parameters.SemanticUnrollingLevel.get
      Value_parameters.SemanticUnrollingLevel.set
  in
  let box_1_2 = GPack.hbox ~packing:(w#attach ~left:1 ~top:2) () in
  let validator s =
    not
      (Kernel_function.Set.is_empty
         (Parameter_customize.get_c_ified_functions s))
  in
  let main_refresh = Gtk_helper.on_string
      ~tooltip:Kernel.MainFunction.parameter.Typed_parameter.help
      ~validator box_1_2 "main" Kernel.MainFunction.get Kernel.MainFunction.set
  in
  let refresh () = slevel_refresh (); main_refresh() in
  ignore (run_button#connect#pressed
            (fun () ->
               main_ui#protect ~cancelable:true
                 (fun () -> refresh (); !Db.Value.compute (); main_ui#reset ());
            ));
  "Value", box#coerce, Some refresh

(* Find a location in which to evaluate things, when the given block is
   selected. *)
let find_loc kf fdec block =
  if block == fdec.sbody then
    Some (GL_Pre kf)
  else
    match block.bstmts with
    | [] -> None
    | s :: _ -> Some (GL_Stmt (kf, s))

let add_keybord_shortcut_evaluate (main_ui:main_ui) cm =
  (* The currently selected statement is stored to enable a keyboard shortcut
     to activate it. [None] means that there is no selection or the selected
     element is not part of a statement. *)
  let selected_loc_for_acsl = ref None in
  (* We add a selector to enable a keyboard shortcut for evaluating ACSL
     expressions. This selector listens to modification events and
     updates selected_loc_for_acsl to the stmt of the selected element. *)
  let clear () = cm#clear_default () in
  let select new_loc =
    begin
      match new_loc, !selected_loc_for_acsl with
      | None, None -> ()
      | None, Some _ | Some _, None -> clear ()
      | Some new_loc, Some old_loc ->
        if not (gui_loc_equal new_loc old_loc) then clear ();
    end;
    selected_loc_for_acsl := new_loc
  in
  (* This function must be maintained synchronized with
     [left_click_values_computed] above. *)
  let can_eval_acsl_expr_selector _menu (_main_ui:main_ui) ~button:_ selected =
    match selected with
    | PStmt (kf, stmt)
    | PLval (Some kf, Kstmt stmt, _)
    | PExp (Some kf, Kstmt stmt, _)
    | PTermLval (Some kf, Kstmt stmt, _, _) ->
      if Gui_eval.results_kf_computed kf
      then select (Some (GL_Stmt (kf, stmt)))
      else select None
    | PTermLval (Some kf, Kglobal, ip, _) ->
      select (Gui_eval.classify_pre_post kf ip)
    | PVDecl (Some kf, vi) when vi.vformal ->
      select (Some (GL_Pre kf))
    | PVDecl (Some kf, vi) when not (vi.vformal || vi.vglob) (* local *) ->
      (* Notice that Pretty_source focuses on the statement containing the block
         itself most of the time. This case only happens when you directly
         select the declaration of a variable, between the type and the name *)
      let fdec = Kernel_function.get_definition kf in
      let bl = Ast_info.block_of_local fdec vi in
      select (find_loc kf fdec bl)
    | PIP (Property.IPCodeAnnot (kf, stmt,
           {annot_content = AAssert (_, _) | AInvariant (_, true, _)} )) ->
      select (Some (GL_Stmt (kf, stmt)))
    | PIP (Property.IPPredicate (_, kf, Kglobal, _) as ip) ->
      select (Gui_eval.classify_pre_post kf ip)
    | _ -> select None
  in
  main_ui#register_source_selector can_eval_acsl_expr_selector;
  (* We add a keyboard shortcut (Ctrl+E) to open the "Evaluate ACSL expression"
     popup. This only works if the current selection is on a statement,
     otherwise it does nothing. *)
  let accel_group = GtkData.AccelGroup.create () in
  let register_accel modi kind =
    GtkData.AccelGroup.connect accel_group
      ~key:GdkKeysyms._E ~modi
      ~callback:(fun _ ->
          match !selected_loc_for_acsl with
          | None -> ()
          | Some loc -> eval_acsl_term_pred main_ui cm loc kind ()
        );
  in
  register_accel [`CONTROL] Term;
  register_accel [`CONTROL; `SHIFT] Pred;
  main_ui#main_window#add_accel_group accel_group
;;

let add_callstacks_manager (main_ui:main_ui) =
  let callstacks_tab_label = (GMisc.label ~text:"Values" ())#coerce in
  let packing w =
    ignore
      (main_ui#lower_notebook#insert_page ~tab_label:callstacks_tab_label w);
  in
  Callstacks_manager.make main_ui ~packing

let highlighter csf (buffer:GSourceView2.source_buffer) localizable ~start ~stop=
  (* highlight dead code areas, non-terminating calls, and degeneration
     points if Value has run.*)
  if Db.Value.is_computed () then
    match localizable with
    | PStmt (kf, stmt) -> begin
        let degenerate =
          try
            Some (
              if Value_util.DegenerationPoints.find stmt
              then (make_tag buffer ~name:"degeneration" [`BACKGROUND "orange"])
              else (make_tag buffer ~name:"unpropagated" [`BACKGROUND "yellow"])
            )
          with Not_found -> None
        in
        match degenerate with
        | Some color_area ->
          apply_tag buffer color_area start stop
        | None ->
          if Gui_eval.results_kf_computed kf then begin
            let csf = csf () in
            if Gui_callstacks_filters.is_reachable_stmt csf stmt then begin
              if Gui_callstacks_filters.is_non_terminating_instr csf stmt then
                let non_terminating =
                  Gtk_helper.make_tag
                    buffer ~name:"value_non_terminating"
                    [`BACKGROUND "tomato"]
                in
                apply_tag buffer non_terminating (stop-1) stop
            end
            else
              let dead_code_area =
                make_tag buffer "deadcode" [`BACKGROUND "tomato";`STYLE `ITALIC]
              in
              apply_tag buffer dead_code_area start stop
          end
      end
    | _ -> ()

let main (main_ui:main_ui) =
  (* Hide unused functions and variables. Must be registered only once *)
  let hide, _filter_menu =
    main_ui#file_tree#add_global_filter
      ~text:"Analyzed by Value only"
      ~key:"value_hide_unused" hide_unused_function_or_var
  in
  hide_unused := hide;
  main_ui#file_tree#register_reset_extension sync_filetree;
  (* Very first display, we need to do a few things by hand *)
  if !hide_unused () then
    main_ui#file_tree#reset ()
  else
    sync_filetree main_ui#file_tree;
  let cm = add_callstacks_manager main_ui in
  Design.register_reset_extension (fun _ -> cm#reset ());
  main_ui#register_source_selector (to_do_on_select cm);
  let callstacks_filter () = cm#focused_rev_callstacks in
  main_ui#register_source_highlighter (highlighter callstacks_filter);
  main_ui#register_panel value_panel;
  add_keybord_shortcut_evaluate main_ui cm;
;;

let () = Design.register_extension main
;;


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
