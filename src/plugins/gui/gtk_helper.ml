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

(** Generic Gtk helpers. *)

let () =
  begin
    Wutil.share := Config.datadir;
    Wutil.flush := (fun msg -> Gui_parameters.warning "%s" msg);
  end

let framac_logo, framac_icon =
  try
    let img ext =
      Some (GdkPixbuf.from_file (Config.datadir ^ "/frama-c." ^ ext))
    in
    img "png", img "ico"
  with
  | Glib.GError _ ->
    Gui_parameters.warning
      "Frama-C icon/logo not found. Is FRAMAC_SHARE correctly set?";
    None, None
  | GdkPixbuf.GdkPixbufError (_, errmsg) ->
    Gui_parameters.warning
      "Could not load Frama-C icon/logo: %s" errmsg;
    None, None

module Icon = struct

  type kind = Frama_C | Unmark
            | Custom of string
            | Feedback of Property_status.Feedback.t

  module F = Property_status.Feedback

  type theme_file =
    | ThemeSpecific of string
    | ThemeGeneric of string

  let builtins =
    [(Frama_C,                      ThemeGeneric "frama-c.ico");
     (Unmark,                       ThemeGeneric "unmark.png");
     (Feedback F.Never_tried,       ThemeSpecific "never_tried.png");
     (Feedback F.Unknown,           ThemeSpecific "unknown.png");
     (Feedback F.Valid,             ThemeSpecific "surely_valid.png");
     (Feedback F.Invalid,           ThemeSpecific "surely_invalid.png");
     (Feedback F.Considered_valid,  ThemeSpecific "considered_valid.png");
     (Feedback F.Valid_under_hyp,   ThemeSpecific "valid_under_hyp.png");
     (Feedback F.Invalid_under_hyp, ThemeSpecific "invalid_under_hyp.png");
     (Feedback F.Invalid_but_dead,  ThemeSpecific "invalid_but_dead.png");
     (Feedback F.Unknown_but_dead,  ThemeSpecific "unknown_but_dead.png");
     (Feedback F.Valid_but_dead,    ThemeSpecific "valid_but_dead.png");
     (Feedback F.Inconsistent,      ThemeSpecific "inconsistent.png");
    ]

  let get_file_in_theme = function
    | ThemeSpecific x -> "theme/" ^ (Gui_parameters.Theme.get()) ^ "/" ^ x
    | ThemeGeneric x -> x

  type icon = Filename of theme_file | Pixbuf of GdkPixbuf.pixbuf

  let h = Hashtbl.create 7

  let () =
    List.iter
      (fun (k,f) -> Hashtbl.add h k (Filename f))
      builtins

  let get k =
    try match Hashtbl.find h k with
      | Filename f' ->
          let f = get_file_in_theme f' in
          let p = Widget.shared_icon f in
          Hashtbl.replace h k (Pixbuf p); p
      | Pixbuf p -> p
    with Not_found -> assert false

  let default = Widget.default_icon

  let register ~name ~file = Hashtbl.replace h (Custom name)
      (Filename (ThemeGeneric file))

end

module Configuration = struct
  include Cilconfig
  let configuration_file () =
    try Gui_parameters.Config.file ~error:false "frama-c-gui.config"
    with Gui_parameters.Config.No_dir -> ""
  let load () = loadConfiguration (configuration_file ())
  let save () = saveConfiguration (configuration_file ())
  let () = Cmdline.at_normal_exit save

  let set = setConfiguration
  let find = findConfiguration

  let use_int = useConfigurationInt
  let set_int key s = set key (ConfInt s)
  let find_int ?default key =
    try findConfigurationInt key
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v ->
          set key (ConfInt v);
          v

  let use_float = useConfigurationFloat
  let set_float key f = set key (ConfFloat f)
  let find_float ?default key =
    try findConfigurationFloat key
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v ->
          set key (ConfFloat v);
          v

  let use_bool = useConfigurationBool
  let set_bool key b = set key (ConfBool b)
  let find_bool ?default key =
    try findConfigurationBool key
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v ->
          set key (ConfBool v);
          v
    
  let use_string = useConfigurationString
  let set_string key s = set key (ConfString s)
  let find_string ?default s = 
    try findConfigurationString s
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v -> set s (ConfString v);
          v

  let set_list key l = set key (ConfList l)
  let use_list = useConfigurationList
  let find_list = findConfigurationList
  
  class type ['a] selector =
    object
      method set : 'a -> unit
      method connect : ('a -> unit) -> unit
    end
    
  let config_string ~key ~default widget =
    let init = find_string ~default key in
    widget#set init ;
    widget#connect (set_string key)
      
  let config_int ~key ~default widget =
    let init = find_int ~default key in
    widget#set init ;
    widget#connect (set_int key)

  let config_bool ~key ~default widget =
    let init = find_bool ~default key in
    widget#set init ;
    widget#connect (set_bool key)

  let config_values ~key ~default ~values widget =
    begin
      let of_string s = fst (List.find (fun e -> snd e = s) values) in
      let to_string v = snd (List.find (fun e -> fst e = v) values) in
      let init =
        try of_string (find_string key)
      with Not_found -> default
      in
      widget#set init ;
      widget#connect
        (fun v ->
           try set_string key (to_string v)
           with Not_found -> ())
    end

end

let apply_tag b tag pb pe =
  let b = (b:>GText.buffer) in
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#apply_tag ~start ~stop tag

let remove_tag b tag pb pe =
  let b = (b:>GText.buffer) in
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#remove_tag ~start ~stop tag

let cleanup_tag b tag =
  let b = (b:>GText.buffer) in
  b#remove_tag tag ~start:b#start_iter ~stop:b#end_iter

(* This table shall not be projectified: it contains trans-project
   informations *)
module IntHashtbl =
  Hashtbl.Make(struct
    type t = int
    let hash = Hashtbl.hash
    let equal : int -> int -> bool = (=)
  end)

let tag_names = IntHashtbl.create 17
let cleanup_all_tags b =
  let b = (b:>GText.buffer) in
  let start = b#start_iter in
  let stop = b#end_iter in
  try
    let tags = IntHashtbl.find tag_names (Oo.id b) in
    Datatype.String.Set.iter (fun s -> b#remove_tag_by_name s ~start ~stop) tags
  with Not_found -> ()

let make_tag (buffer:< tag_table : Gtk.text_tag_table;
              create_tag : ?name:string -> GText.tag_property list -> GText.tag ; .. >)
    ~name l
  =
  match GtkText.TagTable.lookup buffer#tag_table name with
  | None ->
      let oid = Oo.id buffer in
      let old_set =
        try IntHashtbl.find tag_names oid
        with Not_found -> Datatype.String.Set.empty
      in
      IntHashtbl.replace tag_names oid (Datatype.String.Set.add name old_set);
      buffer#create_tag ~name l
  | Some t -> new GText.tag t

let expand_to_path (treeview:GTree.view) path = treeview#expand_to_path path

let make_formatter ?(flush= fun () -> ()) t =
  let t = (t:>GText.buffer) in
  let fmt_emit s start length =
    let subs = String.sub s start length in
    t#insert ~iter:t#end_iter subs
  in
  Format.make_formatter fmt_emit flush

let redirect fmt (t:#GText.buffer) =
  let fmt_emit s start length =
    let subs = String.sub s start length in
    t#insert subs
  in
  let fmt_flush () = () in
  Format.pp_set_formatter_output_functions
    fmt
    fmt_emit fmt_flush

let gui_unlocked = ref false
module Lock = struct
  let last = ref (fun _ -> ())
  module H = Hook.Build(struct type t = bool end)
  let extend is_last f = if is_last then last := f else H.extend f
  let apply b = H.apply b; !last b
end
module Unlock = struct
  let first = ref (fun () -> ())
  module H = Hook.Make(struct end)
  let extend is_first f = if is_first then first := f else H.extend f
  let apply () = !first (); H.apply ()
end
let register_locking_machinery ?(lock_last=false) ~lock ~unlock () =
  if lock_last then begin
    Lock.extend true lock;
    Unlock.extend true unlock
  end else begin
    Lock.extend false lock;
    Unlock.extend false unlock
  end

let log_redirector ?(flush=fun () -> ()) emit_string =
  let output s offset length = emit_string (String.sub s offset length) in
  Log.set_output ~isatty:false output flush

let make_string_list ~packing =
  let (model,column) =
    GTree.store_of_list Gobject.Data.string [] in
  let insert s =
    let row = model#append () in
    model#set ~row ~column s
  in
  let get_all () =
    let l = ref [] in
    model#foreach (fun _ row ->
        l := model#get ~row ~column ::!l ;
        false);
    !l
  in
  let view = GTree.view ~model ~reorderable:true ~packing () in
  let view_column = GTree.view_column ~title:"Source file(s)" () in
  let str_renderer = GTree.cell_renderer_text [] in
  view_column#pack str_renderer;
  view_column#add_attribute str_renderer "text" column;
  let _ = view#append_column view_column in
  let remove_selected () =
    let path_list = view#selection#get_selected_rows in
    let row_refs = List.map model#get_row_reference path_list in
    List.iter (fun rr -> ignore (model#remove rr#iter)) row_refs
  in
  insert,remove_selected, get_all

let model_of_list conv l =
  let cols = new GTree.column_list in
  let column = cols#add conv in
  let model = GTree.list_store cols in
  List.iter
    (fun data ->
       let row = model#append () in
       model#set ~row ~column data)
    l ;
  (model, column)

let string_selector completions packing =
  let (model, col) = model_of_list Gobject.Data.string completions in
  let entry = GEdit.entry ~packing () in
  let c = GEdit.entry_completion ~model ~entry () in
  c#set_text_column col ;
  entry
(*  (GEdit.combo ~popdown_strings:completions ~packing ())#entry *)

let mk_label ?(use_markup=false) ?xalign (container:GPack.box) label =
  let l =
    GMisc.label ~xpad:3 ~line_wrap:true ?xalign
      ~packing:(container#pack ~expand:true ~fill:true)
  in
  if use_markup then l ~markup:label () else l ~text:label ()

type 'a chooser =
  GPack.box -> string -> (unit -> 'a) -> ('a -> unit) -> (unit -> unit)

(* ------------------------------------------------------------------------ *)
(* --- Bundle of fields                                                 --- *)
(* ------------------------------------------------------------------------ *)

let do_tooltip ?tooltip obj = match tooltip with
  | None -> ()
  | Some text ->
      let tooltip = GData.tooltips () in
      tooltip#set_tip ~text obj#coerce

let on_bool ?tooltip ?use_markup (container:GPack.box) label get set =
  let result = ref (get ()) in
  let container = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip container;
  let button =
    GButton.check_button ~packing:container#pack ~active:!result () 
  in
  ignore (mk_label ?use_markup container ~xalign:0. label);
  ignore (button#connect#toggled ~callback:(fun () -> set button#active));
  let update () = button#set_active (get()) in
  (fun () -> update ())

let range_selector
    ?tooltip ?use_markup (container:GPack.box) ~label ~lower ~upper
    set get =
  let container = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip container;
  let x =
    GEdit.spin_button ~digits:0 ~packing:(container#pack ~padding:10) ()
  in
  x#adjustment#set_bounds
    ~lower:(float lower) ~upper:(float upper) ~step_incr:1. ();
  x#adjustment#set_value (float (get ()));
  ignore
    (x#connect#value_changed
       ~callback:
         (fun () -> set x#value_as_int));
  ignore (mk_label ?use_markup ~xalign:0. container label);
  (fun () -> x#adjustment#set_value (float (get ())))

let on_int
    ?tooltip ?use_markup ?(lower=0) ?(upper=max_int)
    ?(sensitive=(fun () -> true)) ?width
    (container:GPack.box) label get set =
  let container = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip container;
  let non_fixed = width=None in
  let spin = GEdit.spin_button ~digits:0
      ?width ~packing:(container#pack ~expand:non_fixed ~fill:non_fixed) ()
  in
  spin#adjustment#set_bounds
    ~lower:(float lower) ~upper:(float upper) ~step_incr:1. ();
  spin#adjustment#set_value (float (get()));
  ignore
    (spin#connect#value_changed
       ~callback:
         (fun () -> set spin#value_as_int));
  let label = mk_label ?use_markup ~xalign:0. container label in
  (fun () ->
     label#misc#set_sensitive (sensitive ());
     spin#misc#set_sensitive (sensitive ());
     spin#adjustment#set_value (float (get())))

let on_string ?tooltip ?use_markup ?(validator=(fun _ -> true)) ?width
    (container:GPack.box) label get set =
  let container = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip container;
  let entry = GEdit.entry ~packing:container#pack ~text:(get()) ?width () in
  let callback _ =
    let text = entry#text in
    if validator text then set text
    else entry#set_text (get ());
    false
  in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (entry#connect#activate ~callback:(fun () -> ignore (callback ())));
  ignore (mk_label ?use_markup ~xalign:0. container label);
  (fun () -> 
     if not (Gobject.Property.get entry#as_widget GtkBase.Widget.P.has_focus)
     then entry#set_text (get ()))

let on_string_set ?tooltip ?use_markup ?width (container:GPack.box) label get set =
  let container = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip container;
  let entry = GEdit.entry ~packing:container#pack ~text:(get()) ?width () in
  let callback _ = set entry#text; false in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (entry#connect#activate ~callback:(fun () -> ignore (callback ())));
  ignore (mk_label ?use_markup ~xalign:0. container (label ^ " (list)"));
  (fun () -> 
     if not (Gobject.Property.get entry#as_widget GtkBase.Widget.P.has_focus)
     then entry#set_text (get()))

let on_string_completion
    ?tooltip ?use_markup ?(validator=(fun _ -> true)) completions
    (container:GPack.box) label get set =
  let box = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip box;
  let entry = string_selector completions box#pack in
  ignore (mk_label ?use_markup ~xalign:0. box label);
  let () = entry#set_text (get()) in
  let callback _ =
    let text =  entry#text in
    if validator text then set text else entry#set_text (get());
    false
  in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (entry#connect#activate ~callback:(fun () -> ignore (callback ())));
  (fun () -> entry#set_text (get()))

let on_combo
    values ?tooltip ?(use_markup=false) ?width (container:GPack.box)
    label get set =
  let rec select i (x:string) = function
    | [] -> (-1)
    | y::ys -> if x=y then i else select (succ i) x ys
  in
  let container = GPack.hbox ~packing:container#pack () in
  do_tooltip ?tooltip container;
  let non_fixed = width=None in
  let combo_box, (_model, column) =
    GEdit.combo_box_text
      ~strings:values ?width
      ~wrap_width:3 ~use_markup
      ~packing:(container#pack ~expand:non_fixed ~fill:non_fixed)
      ()
  in
  let callback () =
    match combo_box#active_iter with
    | None -> ()
    | Some row -> set (combo_box#model#get ~row ~column)
  in
  let update () =
    let result = ref (get ()) in
    let k = select 0 !result values in
    if k >= 0 then combo_box#set_active k
  in
  ignore (combo_box#connect#changed callback) ;
  ignore (mk_label ~use_markup ~xalign:0. container label) ;
  (fun () -> update ())

(* ------------------------------------------------------------------------ *)
(* --- Misc                                                             --- *)
(* ------------------------------------------------------------------------ *)

let save_paned_ratio key (paned:GPack.paned) =
  let paned_min_pos = paned#min_position in
  let paned_max_pos = paned#max_position in
  let length = paned_max_pos - paned_min_pos in
  let ratio = if length = 0 then 0.5
    else (float_of_int paned#position)/.(float_of_int length)
  in
  Configuration.set key (Configuration.ConfFloat ratio)

let place_paned (paned:GPack.paned) factor =
  let paned_min_pos = paned#min_position in
  let offset =
    int_of_float (float (paned#max_position - paned_min_pos)*.factor)
  in
  paned#set_position (paned_min_pos + offset)

let old_gtk_compat f x = try f x with Not_found -> ()

let trace_event (w:GObj.event_ops) =
  let string_of_event x =
    match GdkEvent.get_type x with
    | `NOTHING -> "nothing"
    | `DELETE -> "delete"
    | `DESTROY -> "destroy"
    | `EXPOSE -> "expose"
    | `MOTION_NOTIFY -> "motion-notify"
    | `BUTTON_PRESS -> "button-press"
    | `TWO_BUTTON_PRESS -> "2 button-press"
    | `THREE_BUTTON_PRESS -> "3 button-press"
    | `BUTTON_RELEASE -> "button-release"
    | `KEY_PRESS -> "key-press"
    | `KEY_RELEASE  -> "key-release"
    | `ENTER_NOTIFY  -> "enter-notfiy"
    | `LEAVE_NOTIFY -> "leave-notify"
    | `FOCUS_CHANGE  -> "focus-change"
    | `CONFIGURE -> "configure"
    | `MAP -> "map"
    | `UNMAP -> "unmap"
    | `PROPERTY_NOTIFY -> "property-notify"
    | `SELECTION_CLEAR -> "selection-clear"
    | `SELECTION_REQUEST -> "selection-request"
    | `SELECTION_NOTIFY -> "selection-notify"
    | `PROXIMITY_IN -> "proximity-in"
    | `PROXIMITY_OUT -> "proximiy-out"
    | `DRAG_ENTER -> "drag-enter"
    | `DRAG_LEAVE -> "drag-leave"
    | `DRAG_MOTION -> "drag-motion"
    | `DRAG_STATUS -> "drag-status"
    | `DROP_START -> "drop-start"
    | `DROP_FINISHED -> "drop-finish"
    | `CLIENT_EVENT -> "client-event"
    | `VISIBILITY_NOTIFY -> "visibility-notify"
    | `NO_EXPOSE-> "no-expose"
    | `SCROLL -> "scroll"
    | `WINDOW_STATE -> "window-state"
    | `SETTING -> "setting"
  in
  ignore (w#connect#any
            ~callback:(fun e ->
                Format.eprintf "TRACING event: %s@." (string_of_event e);
                false))

module MAKE_CUSTOM_LIST(A:sig type t end) =
struct
  type custom_list =
    {finfo: A.t;
     fidx: int (* invariant: root.(fidx)==myself *) }

  module H = Hashtbl

  let inbound i a = i>=0 && i<Array.length a

  (** The custom model itself *)
  class custom_list_class column_list =
    object (self)
      inherit
        [custom_list,custom_list,unit,unit] GTree.custom_tree_model column_list

      method custom_encode_iter cr = cr, (), ()
      method custom_decode_iter cr () () = cr

      val mutable last_idx = 0
      val mutable roots : (int,custom_list) H.t = H.create 19
      method private find_opt i =
        try Some (H.find roots i) with Not_found -> None
      method! custom_flags = [`LIST_ONLY]
      method custom_get_iter (path:Gtk.tree_path) : custom_list option =
        let indices: int array  = GTree.Path.get_indices path in
        match indices with
        | [||] ->
            None
        | [|i|] -> self#find_opt i
        | _ -> failwith "Invalid Path of depth > 1 in a list"

      method custom_get_path (row:custom_list) : Gtk.tree_path =
        GTree.Path.create [row.fidx]

      method custom_value (_t:Gobject.g_type) (_row:custom_list) ~column:_ =
        assert false

      method custom_iter_next (row:custom_list) : custom_list option =
        let nidx = succ row.fidx in
        self#find_opt nidx

      method custom_iter_children (rowopt:custom_list option):custom_list option =
        match rowopt with
        | None -> self#find_opt 0
        | Some _ -> None

      method custom_iter_has_child (_:custom_list) : bool = false

      method custom_iter_n_children (rowopt:custom_list option) : int =
        match rowopt with
        | None -> H.length roots
        | Some _ -> assert false

      method custom_iter_nth_child (rowopt:custom_list option) (n:int)
        : custom_list option =
        match rowopt with
        | None -> self#find_opt n
        | _ -> None

      method custom_iter_parent (_:custom_list) : custom_list option = None

      method insert (t:A.t) =
        let e = {finfo=t; fidx= last_idx } in
        self#custom_row_inserted (GTree.Path.create [last_idx]) e;
        H.add roots last_idx e;
        last_idx <- last_idx+1;

      method clear () =
        for i=last_idx-1 downto 0 do
          self#custom_row_deleted (GTree.Path.create [i]);
        done;
        last_idx <- 0;
        H.clear roots;
    end

  let custom_list () =
    new custom_list_class (new GTree.column_list)

  let make_view_column model renderer properties ~title =
    let m_renderer renderer (lmodel:GTree.model) iter =
      let (path:Gtk.tree_path) = lmodel#get_path iter  in
      let props = match model#custom_get_iter path with
        | Some {finfo=v} -> properties v
        | None -> []
      in
      renderer#set_properties props
    in
    let cview = GTree.view_column ~title ~renderer:(renderer,[]) () in
    cview#set_cell_data_func renderer (m_renderer renderer);
    cview
end

(* NOTE: this code has been copied from lablgtk's gToolbox.ml to
   allow binding the behavior of "keypad enter" to the "return" key *)
let input_widget ~widget ~event ~get_text ~bind_ok ~expand
    ~title ?(ok="Ok") ?(cancel="Cancel") message =
  let retour = ref None in
  let window = GWindow.dialog ~title ~modal:true () in
  ignore (window#connect#destroy ~callback: GMain.Main.quit);
  let main_box = window#vbox in
  let hbox_boutons = window#action_area in
  let vbox_saisie = GPack.vbox ~packing: (main_box#pack ~expand: true) () in
  ignore (GMisc.label ~text:message ~packing:(vbox_saisie#pack ~padding:3) ());
  vbox_saisie#pack widget ~expand ~padding: 3;
  let wb_ok = GButton.button ~label: ok
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  wb_ok#grab_default ();
  let wb_cancel = GButton.button ~label: cancel
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  let f_ok () =
    retour := Some (get_text ()) ;
    window#destroy ()
  in
  let f_cancel () =
    retour := None;
    window#destroy ()
  in
  ignore (wb_ok#connect#clicked f_ok);
  ignore (wb_cancel#connect#clicked f_cancel);
  (* the enter key is linked to the ok action *)
  (* the escape key is linked to the cancel action *)
  ignore (event#connect#key_press ~callback:
    begin fun ev ->
      if (GdkEvent.Key.keyval ev = GdkKeysyms._Return ||
          GdkEvent.Key.keyval ev = GdkKeysyms._KP_Enter) && bind_ok
      then f_ok ();
      if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then f_cancel ();
      false
    end);
  widget#misc#grab_focus ();
  window#show ();
  GMain.Main.main ();
  !retour

(* NOTE: this code has been copied from lablgtk's gToolbox.ml to
   allow binding the behavior of "keypad enter" to the "return" key *)
let input_string ~title ?ok ?cancel ?(text="") message =
  let we_chaine = GEdit.entry ~text () in
  if text <> "" then
    we_chaine#select_region 0 (we_chaine#text_length);
  input_widget ~widget:we_chaine#coerce ~event:we_chaine#event
    ~get_text:(fun () -> we_chaine#text) ~bind_ok:true
    ~expand: false
    ~title ?ok ?cancel message

(* ************************************************************************** *)
(** {2 Error manager} *)
(* ************************************************************************** *)

(**  A utility class to catch exceptions and report proper error messages. *)
class type host = object
  method error:
    'a. ?parent:GWindow.window_skel -> ?reset:bool -> ('a, Format.formatter, unit) format -> 'a
  method full_protect :
    'a. cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> 'a) ->
    'a option
  method protect :
    cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> unit) -> unit
  method private set_reset: (unit -> unit) -> unit
end

class error_manager ?reset (o_parent:GWindow.window_skel) : host = 
  object (self: #host)

    val mutable f_reset = match reset with
      | None -> fun () -> ()
      | Some f -> f

    method private set_reset f = f_reset <- f

    method private error_string ?parent ~reset message =
      let w = GWindow.message_dialog
          ~message
          ~message_type:`ERROR
          ~parent:(Extlib.opt_conv o_parent parent)
          ~buttons:GWindow.Buttons.ok
          ~title:"Error"
          ~position:`CENTER_ALWAYS
          ~modal:true
          ()
      in
      w#show ();
      w#present ();
      ignore (w#run ());
      w#destroy ();
      if reset then f_reset ()

    method error ?parent ?(reset=false) fmt =
      let b = Buffer.create 80 in
      let bfmt = Format.formatter_of_buffer b in
      Format.kfprintf
        (function fmt ->
          Format.pp_print_flush fmt ();
          let content = Buffer.contents b in
          self#error_string ?parent ~reset content)
        bfmt
        fmt

    method private display_toplevel_error ?parent ~cancelable e =
      Cmdline.error_occurred e;
      if cancelable then Project.Undo.restore ();
      self#error ?parent ~reset:true "%s" (Cmdline.protect e);

    method protect ~cancelable ?(parent:GWindow.window_skel option) f =
      ignore (self#full_protect ~cancelable ?parent f)

    method full_protect ~cancelable ?(parent:GWindow.window_skel option) f =
      let cancelable = cancelable && Gui_parameters.Undo.get () in
      try
        if cancelable then Project.Undo.breakpoint ();
        let old_gui_unlocked = !gui_unlocked in
        let res =
          Extlib.try_finally
            ~finally:(fun () -> if old_gui_unlocked then begin
                Unlock.apply ();
                gui_unlocked := true
              end)
            (fun () ->
               if old_gui_unlocked then begin
                 Lock.apply cancelable;
                 gui_unlocked := false;
               end;
               f ())
            ()
        in
        if cancelable then Project.Undo.clear_breakpoint ();
        Some res
      with
      | Cmdline.Exit ->
          if cancelable then Project.Undo.clear_breakpoint ();
          None
      | Sys.Break | Db.Cancel ->
          if cancelable then Project.Undo.restore ();
          self#error ?parent ~reset:true
            "Stopping current computation on user request.";
          None
      | Globals.No_such_entry_point msg ->
          (try Gui_parameters.abort "%s" msg
           with
           | Log.AbortError _ as e ->
               self#display_toplevel_error ?parent ~cancelable e;
               None)
      | e when Cmdline.catch_at_toplevel e ->
          self#display_toplevel_error ?parent ~cancelable e;
          None
      | e ->
          if Gui_parameters.debug_atleast 1 then begin
            Cmdline.error_occurred e;
            raise e
          end else begin
            self#display_toplevel_error ?parent ~cancelable e;
            None
          end

  end

let make_text_page ?pos (notebook:GPack.notebook) title =
  let make_tab_label (notebook:GPack.notebook) =
    let flash_title = Format.sprintf "<i>%s</i>" title in
    let tab_label = GMisc.label ~markup:title () in
    let sw = GBin.scrolled_window
        ~vpolicy:`AUTOMATIC
        ~hpolicy:`AUTOMATIC
        ~packing:
          (fun w -> ignore (notebook#insert_page ?pos ~tab_label:tab_label#coerce w))
        ()
    in
    let flash b =
      tab_label#set_text (if b then flash_title else title);
      ignore(tab_label#set_use_markup true)
    in
    flash, sw
  in
  let flash,sw = make_tab_label notebook in
  let flash_ref = ref flash in
  let w = GText.view ~packing:sw#add () in
  let _ = w#set_editable false in
  let _ = w#misc#connect#map (fun () -> !flash_ref false) in
  let _ = w#event#connect#focus_in (fun _ -> !flash_ref false; false) in
  let _ = w#buffer#connect#changed (fun () -> !flash_ref true)
  in
  let reparent_page (notebook:GPack.notebook) =
    let flash, sw = make_tab_label notebook in
    flash_ref := flash;
    w#misc#reparent sw#coerce
  in
  reparent_page, w

(* If [EDITOR] is undefined, uses [emacsclient -n] *)
let open_in_external_viewer ?(line=1) file =
  let viewer =
    try Sys.getenv "EDITOR" with | Not_found -> "emacsclient -n"
  in
  let cmd_line = Format.sprintf "%s +%d %s" viewer line file in
  Gui_parameters.feedback "opening external viewer, running command:\n%s"
    cmd_line;
  ignore (Sys.command cmd_line)

exception Too_many_events
let refresh_gui () = 
  let counter = ref 0 in
  try 
    while Glib.Main.iteration false do 
      if !counter >= 10 then raise Too_many_events 
      else incr counter
    done
  with Too_many_events -> ()

(* ************************************************************************* *)
(** {2 Source File Chooser} *)
(* ************************************************************************* *)

class type source_files_chooser_host = object
  inherit host
  method main_window: GWindow.window_skel
  method reset: unit -> unit
end

let accepted_source_files () =
  let f = GFile.filter ~name:"Source files" () in
  List.iter (fun s -> f#add_pattern ("*" ^ s)) (File.get_suffixes ());
  f

let all_files () =
  let f = GFile.filter ~name:"All files" () in
  f#add_pattern "*.*";
  f

let source_files_chooser (main_ui: source_files_chooser_host) defaults f =
  let dialog = GWindow.dialog
      ~width:800
      ~height:400
      ~modal:true
      ~title:"Select C source files"
      ~parent:main_ui#main_window
      ~destroy_with_parent:true
      ()
  in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_button_stock `OK `OPEN;
  let hbox = GPack.box `HORIZONTAL ~packing:dialog#vbox#add () in
  let filechooser = GFile.chooser_widget
      ~action:`OPEN
      ~packing:(hbox#pack ~expand:true ~fill:true)
      ()
  in
  Configuration.use_string "last_opened_dir" 
    (fun s -> ignore (filechooser#set_current_folder s));
  filechooser#set_select_multiple true;
  filechooser#add_filter (accepted_source_files ());
  filechooser#add_filter (all_files ());
  let bbox =
    GPack.button_box
      ~layout:`START
      `VERTICAL ~packing:(hbox#pack ~expand:false ~fill:false) () in
  let add_button = GButton.button ~stock:`ADD ~packing:bbox#add () in
  let remove_button = GButton.button ~stock:`REMOVE ~packing:bbox#add () in
  let w =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:(hbox#pack ~expand:true ~fill:true)
      ()
  in
  let add,remove,get_all = make_string_list ~packing:w#add in
  let add_selected_files () =
    let f = filechooser#get_filenames in
    List.iter add f
  in
  List.iter add defaults;
  ignore (add_button#connect#pressed ~callback:add_selected_files);
  ignore (remove_button#connect#pressed ~callback:remove);
  ignore (filechooser#connect#file_activated ~callback:add_selected_files);
  let response r = (match r with
      | `OPEN ->
          main_ui#protect
            ~cancelable:true
            ~parent:(dialog :> GWindow.window_skel)
            (fun () -> f (get_all ()))
      | `DELETE_EVENT | `CANCEL ->
          ());
    Extlib.may (fun f ->
        Configuration.set "last_opened_dir"
          (Configuration.ConfString f)) filechooser#current_folder;
    dialog#destroy ()
  in
  let (_:GtkSignal.id) = dialog#connect#response ~callback:response in
  dialog#show ();
  ()

let spawn_command ?(timeout=0) ?stdout ?stderr s args f =
  let check_result = Command.command_async s ?stdout ?stderr args in
  let has_timeout = timeout > 0 in
  let hang_on = float_of_int timeout in
  let starting_time = if has_timeout then Unix.time () else 0. in
  let for_idle () =
    match check_result () with
    | Command.Not_ready kill ->
        if has_timeout && Unix.time () -. starting_time >= hang_on then
          begin
            kill ();
            f (Unix.WSIGNALED Sys.sigalrm);
            false
          end
        else true
    | Command.Result p -> f p; false
  in
  let prio = Glib.int_of_priority `LOW in
  ignore (Glib.Idle.add ~prio for_idle)

let graph_window ~parent ~title make_view =
  let height = int_of_float (float parent#default_height *. 3. /. 4.) in
  let width = int_of_float (float parent#default_width *. 3. /. 4.) in
  let graph_window =
    GWindow.window
      ~width ~height ~title ~allow_shrink:true ~allow_grow:true
      ~position:`CENTER () in
  let view = make_view ~packing:graph_window#add () in
  graph_window#show();
  view#adapt_zoom();
  ()
;;

let graph_window_through_dot ~parent ~title dot_formatter =
  let make_view ~packing () =
    let temp_file =
      try
        Extlib.temp_file_cleanup_at_exit
          "framac_property_status_navigator_graph" "dot"
      with Extlib.Temp_file_error s ->
        Gui_parameters.abort "cannot create temporary file: %s" s in
    let fmt = Format.formatter_of_out_channel (open_out temp_file) in
    dot_formatter fmt;
    Format.pp_print_flush fmt ();
    let view = 
      snd 
        (Dgraph.DGraphContainer.Dot.from_dot_with_commands ~packing temp_file)
    in
    view
  in
  try
    graph_window ~parent ~title make_view
  with Dgraph.DGraphModel.DotError _ as exn ->
    Gui_parameters.error
      "@[cannot display dot graph:@ %s@]"
      (Printexc.to_string exn)
;;

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
