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

open Gui_types

type main_ui = Design.main_window_extension_points

type 'v data_by_callstack =
  (gui_callstack * 'v Gui_eval.gui_selection_data) list

type 'v display_data_by_callstack =
  gui_loc -> gui_selection -> 'v data_by_callstack -> unit

module type Input = sig
  include Gui_types.S

  val make_data_for_lvalue :
    Cil_types.lval -> gui_loc -> value data_by_callstack
end


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


(* Selection of a row; [RSelectedCallstackCol] corresponds to the fact
   that both the row _and_ the 'Callstacks' column are selected. *)
type row_selected = RUnselected | RSelected | RSelectedCallstackCol

(* Information shown in a single row *)
type 'value row = {
  callstack: gui_callstack;
  rev_callstack: Gui_callstacks_filters.rcallstack (* cache *);
  mutable exprs: (gui_selection * 'value Gui_eval.gui_selection_data) list
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

type 'value rows = 'value row GCallstackMap.t

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

type 'value filter_column =
  | FilterAlarm of bool
  | FilterBefore of 'value gui_res
  | FilterAfter of 'value gui_after

type 'value filter = gui_selection * bool * 'value filter_column

let rec remove_filter e : 'v filter list -> _ = function
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
type 'value model = {
  mutable loc: gui_loc option (* model: loc which is being visualized *);
  mutable all_exprs: gui_selection list (* G expressions that are currently
                                           being displayed *);
  mutable columns_type: (column_type * (filtered:bool -> unit)) HColumns.t
(* mapping from GTK columns to the data they display, plus a function
   whose argument indicates whether the column is filtered *);
  mutable rows: 'value rows (* model: rows to display. Sorted, unfiltered
                               (the view does the filtering *);
  mutable row_selected: (int * 'value row) option (* view: row currently selected *);
  mutable focused_rev_callstacks: Gui_callstacks_filters.filter
(* reverse of the callstacks currently being focused. On all tabs,
   the focused callstacks are the only ones that are shown. On the
   'Selection' tab, they are also used to refine the states that
   are being shown, as well as 'go to callers', etc. *);
  mutable filters: 'value filter list;
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


class type ['value] cm_panel = object
  method model: 'value model

  method start_session: gui_loc -> multiple:bool -> unit
  (* clear the model, except in "multiple" view, in which case multiple
     localizable on the same location are stacked *)

  method add_data:
    gui_selection -> gui_callstack -> 'value Gui_eval.gui_selection_data -> unit

  method render_session: unit -> unit
  (* display the current model, taking current filter settings into account *)

  method clear: unit -> unit (* reset both model and widget *)

  method expand_row_for_callstacks: bool -> unit
  method show_consolidated: bool -> unit
  method show_by_callstacks: bool -> unit
  (* These three methods are called by the 'Values' panel when the
     corresponding checkboxes are set or unset *)

  method clone: 'value model -> unit
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


module Make (Input: Input) = struct
  type value = Input.value

  let pretty_filter_column fmt = function
    | FilterAlarm b -> Format.fprintf fmt "%s" (if b then "!" else " ")
    | FilterBefore r -> Format.fprintf fmt "%a" Input.pretty_gui_res r
    | FilterAfter r -> Format.fprintf fmt "%a" Input.pretty_gui_after r

  let data_matches_filter data pos col =
    let ok =
      match col with
      | FilterAlarm a -> data.Gui_eval.alarm = a
      | FilterBefore r -> Input.equal_gui_res r data.Gui_eval.before
      | FilterAfter r -> Input.equal_gui_after r data.Gui_eval.after
    in
    if pos then ok else not ok

  let row_matches_filter row (expr, pos, col: value filter) =
    try
      let data = list_assoc gui_selection_equal expr row.exprs in
      data_matches_filter data pos col
    with Not_found -> (* should not happen *) false

  let filters_match row filters =
    List.for_all (row_matches_filter row) filters

  module Data = Indexer.Make(
    struct
      type t = int * value row
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
        method coerce = (self:> (int * value row) Wtable.listmodel)
      end
    in
    let frame = GBin.frame ~shadow_type:`ETCHED_OUT () in
    let w = new Wtable.list
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
              Format.asprintf "'%a'  (before)" pretty_gui_selection e
            | CAfter e ->
              Format.asprintf "'%a'  (after)" pretty_gui_selection e
            | CAlarm e ->
              Format.asprintf "'%a'  (alarms)" pretty_gui_selection e
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
             end else [`STOCK_ID  ""])
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
             | GA_Bottom -> [`TEXT "BOTTOM"; `STYLE `NORMAL]
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
          Format.asprintf "Remove all columns for '%a'%s"
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
           filters. This row is intentionally only added to the view, but
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
    let popup_menu_filter expr v icon vars_to_display =
      let menu = GMenu.menu () in
      let callback_copy () =
        (* we copy to both PRIMARY and CLIPBOARD clipboards,
           for easier pasting *)
        (* for a more readable result, add a separator between
           the expression and its value when necessary *)
        let value_str = Format.asprintf "%a" pretty_filter_column v in
        let text =
          Format.asprintf "%a%s%a"
            pretty_gui_selection expr
            (if String.get value_str 0 = ' ' then "" else " -> ")
            pretty_filter_column v
        in
        let clipboard = GtkBase.Clipboard.get Gdk.Atom.clipboard in
        GtkBase.Clipboard.set_text clipboard text;
        let primary = GtkBase.Clipboard.get Gdk.Atom.primary in
        GtkBase.Clipboard.set_text primary text
      in
      let callback_only_except oe () =
        let filter = expr, oe, v in
        model.filters <- filter :: model.filters;
        icon ~filtered:true;
        render_session ();
      in
      let copy = GMenu.menu_item ~label:"Copy to clipboard" () in
      let equal = GMenu.menu_item ~label:"Only equal" () in
      let different = GMenu.menu_item ~label:"Only different" () in
      menu#add copy;
      menu#add (GMenu.separator_item ());
      menu#add equal;
      menu#add different;
      ignore (copy#connect#activate callback_copy);
      ignore (equal#connect#activate (callback_only_except true));
      ignore (different#connect#activate (callback_only_except false));
      (* add menu items for variables present in the selected expression *)
      let callback_display_var vi () =
        Extlib.may (fun loc ->
            let lval = Cil.var vi in
            let selection = GS_LVal lval in
            let list = Input.make_data_for_lvalue lval loc in
            let append (callstack, data) = add_data selection callstack data in
            List.iter append list;
            render_session ()
          ) model.loc
      in
      List.iter (fun vi ->
          let label = Format.asprintf "Display values for '%a'"
              Printer.pp_varinfo vi in
          let varmenuitem = GMenu.menu_item ~label () in
          menu#add varmenuitem;
          ignore (varmenuitem#connect#activate (callback_display_var vi));
        ) vars_to_display;
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
           dump "Value before" Input.pretty_gui_res data.Gui_eval.before
         | CAfter expr, _ -> begin
             let data = find_data row expr in
             match data.Gui_eval.after with
             | GA_After after -> dump "Value after" Input.pretty_gui_res after
             | GA_NA | GA_Unchanged | GA_Bottom -> ()
           end
         | CAlarm _, _ | CEmpty, _ -> ()
      );
    let gui_res_of_after f after =
      match after with
      | GA_After r -> f r
      | GA_NA | GA_Unchanged | GA_Bottom -> []
    in
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
               (Input.vars_in_gui_res data.Gui_eval.before);
         | CAfter expr, icon ->
           let data = find_data row expr in
           if data.Gui_eval.before <> GR_Empty then
             popup_menu_filter expr (FilterAfter data.Gui_eval.after) icon
               (gui_res_of_after Input.vars_in_gui_res data.Gui_eval.after)
         | CAlarm expr, icon ->
           let data = find_data row expr in
           if data.Gui_eval.before <> GR_Empty then
             popup_menu_filter expr (FilterAlarm data.Gui_eval.alarm) icon []
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
    end: value cm_panel)

end


module HWidget = Hashtbl.Make(struct
    type t = GObj.widget
    let hash w = Gobject.get_oid w#as_widget
    let equal w1 w2 =
      Gobject.get_oid w1#as_widget = Gobject.get_oid w2#as_widget
  end)

type 'v result =
  { widget: GObj.widget;
    reset: unit -> unit;
    clear_default: unit -> unit;
    focus_selection_tab: unit -> unit;
    display_data_by_callstack: 'v display_data_by_callstack
  }

(* This function creates the buttons at the top of "Values" tab, plus
   a tab control suitable for displaying multiple cm_panel *)
let make_widget (main_ui:main_ui) ~packing make_panel =
  let vpaned = GPack.vbox ~homogeneous:false () ~packing in
  let hbox_filters = GPack.hbox ~packing:(vpaned#pack ~expand:false) () in
  let chk_multiple = new Widget.checkbox ~label:"Multiple selections"
    ~tooltip:"Allow the selection of multiple expressions on the same \
              statement" ()
  in
  let chk_consolidated = new Widget.checkbox ~label:"Consolidated value"
    ~tooltip:"Show values consolidated across all callstacks" ()
  in
  let chk_callstacks = new Widget.checkbox ~label:"Per callstack"
    ~tooltip:"Show values per callstack" ()
  in
  let chk_rows_height = new Widget.checkbox ~label:"Expand rows"
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
  let clear_button = new Widget.button ~icon:`CLEAR ~tooltip:"Clear" () in
  clear_button#set_enabled false;
  let save_button = new Widget.button ~icon:`SAVE ~tooltip:"Save" () in
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
  (* Callback for the "Selection" tab: set the selected callstacks as filters,
     plus rehighlight the source text (for dead code, etc) *)
  let callback_focus_callstack lrcs =
    Gui_callstacks_filters.focus_on_callstacks lrcs;
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
          Format.asprintf "%a:%d"
            Kernel_function.pretty kf
            (fst (Cil_datatype.Stmt.loc stmt)).Lexing.pos_lnum
        | GL_Pre kf ->
          Format.asprintf "pre %a" Kernel_function.pretty kf
        | GL_Post kf ->
          Format.asprintf "post %a" Kernel_function.pretty kf
      in
      let hb = GPack.hbox () in
      ignore (GMisc.label ~packing:hb#pack ~markup:txt ());
      let button_delete =
        new Widget.button ~icon:`DELETE ~tooltip:"Delete" ()
      in
      hb#pack button_delete#coerce;
      button_delete#coerce#misc#hide ();
      let button_edit =
        new Widget.button ~icon:`EDIT ~tooltip:"Edit" ()
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
              Pretty_source.PVDecl (Some kf, Cil_types.Kglobal, vi)
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
  let display_data_by_callstack loc selection content =
    clear_button#set_enabled true;
    save_button#set_enabled true;
    model_default#start_session loc ~multiple:chk_multiple#get;
    let append (callstack, data) =
      model_default#add_data selection callstack data
    in
    List.iter append content;
    model_default#render_session ()
  in
  { widget = vpaned#coerce;
    reset;
    clear_default;
    focus_selection_tab;
    display_data_by_callstack }

(* Reference to the final widget. Used to remove it properly when rebuilding
   the panel for a new analysis. *)
let widget_ref: GObj.widget option ref = ref None

let reset_ref = ref (fun () -> ())
let clear_default_ref = ref (fun () -> ())
let focus_selection_tab_ref = ref (fun _ -> ())

let reset () = !reset_ref ()
let clear_default () = !clear_default_ref ()
let focus_selection_tab () = !focus_selection_tab_ref ()

(* Removes the previous panel, if any. Returns the position of the panel
   in the lower notebook, and a boolean indicating whether the panel had the
   focus before being removed. Used to keep the same position and focus when
   renewing the panel. *)
let remove main_ui =
  match !widget_ref with
  | None -> -1, false
  | Some widget ->
    let num = main_ui#lower_notebook#page_num widget in
    let focused = main_ui#lower_notebook#current_page = num in
    if num <> -1 then main_ui#lower_notebook#remove_page num;
    num, focused

(* Creates the panel, sets the references to widget, clear_default and
   focus_selection_tab, and returns the display_by_callstack function. *)
let create (type v) (main_ui: main_ui) (module I: Input with type value = v) =
  let num, focused = remove main_ui in
  let module CM = Make (I) in
  let packing w =
    let tab_label = (GMisc.label ~text:"Values" ())#coerce in
    ignore (main_ui#lower_notebook#insert_page ~pos:num ~tab_label w#coerce)
  in
  let result = make_widget main_ui ~packing CM.make_panel in
  if focused then main_ui#lower_notebook#goto_page num;
  widget_ref := Some result.widget;
  reset_ref := result.reset;
  clear_default_ref := result.clear_default;
  focus_selection_tab_ref := result.focus_selection_tab;
  result.display_data_by_callstack
