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

(** Generic Gtk helpers. *)

let framac_logo, framac_icon =
  try
    let img ext =
      Some (GdkPixbuf.from_file (Config.datadir ^ "/frama-c." ^ ext))
    in
    img "gif", img "ico"
  with Glib.GError _ ->
    Gui_parameters.warning
      "Frama-C images not found. Is FRAMAC_SHARE correctly set?";
    None, None

module Icon = struct

  type kind = Frama_C | Unmark
              | Custom of string
	      | Feedback of Property_status.Feedback.t

  let default_icon =
    [| "12 12 2 1";
       ". c #ffffff";
       "# c #000000";
       "############";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "############"|]

  module F = Property_status.Feedback

  let builtins =
    [(Frama_C,"frama-c.ico");
     (Unmark,"unmark.png");
     (Feedback F.Never_tried,"feedback/never_tried.png");
     (Feedback F.Unknown,"feedback/unknown.png");
     (Feedback F.Valid,"feedback/surely_valid.png");
     (Feedback F.Invalid,"feedback/surely_invalid.png");
     (Feedback F.Considered_valid,"feedback/considered_valid.png");
     (Feedback F.Valid_under_hyp,"feedback/valid_under_hyp.png");
     (Feedback F.Invalid_under_hyp,"feedback/invalid_under_hyp.png");
     (Feedback F.Invalid_but_dead,"feedback/invalid_but_dead.png");
     (Feedback F.Unknown_but_dead,"feedback/unknown_but_dead.png");
     (Feedback F.Valid_but_dead,"feedback/valid_but_dead.png");
     (Feedback F.Inconsistent,"feedback/inconsistent.png");
    ]

  type icon = Filename of string | Pixbuf of GdkPixbuf.pixbuf

  let h = Hashtbl.create 7

  let () =
    List.iter
      (fun (k,f) -> Hashtbl.add h k (Filename f))
      builtins

  let default () = GdkPixbuf.from_xpm_data default_icon

  let get k =
    try match Hashtbl.find h k with
      | Filename f ->
	  let p =
	    try GdkPixbuf.from_file (Config.datadir ^ "/" ^ f)
	    with Glib.GError _ ->
              Gui_parameters.warning ~once:true
		"Frama-C images not found. Is FRAMAC_SHARE correctly set?";
	      default ()
	  in
	  Hashtbl.replace h k (Pixbuf p); p
      | Pixbuf p -> p
    with Not_found -> assert false
      
  let register ~name ~file = Hashtbl.replace h (Custom name) (Filename file)

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
  let find_int ?default key =
    try findConfigurationInt key
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v ->
          set key (ConfInt v);
          v

  let use_int = useConfigurationInt
  let find_float ?default key =
    try findConfigurationFloat key
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v ->
          set key (ConfFloat v);
          v
  let use_float = useConfigurationFloat

  let find_bool ?default key =
    try findConfigurationBool key
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v ->
          set key (ConfBool v);
          v

  let use_bool = useConfigurationBool
  let find_string ?default s = 
    try findConfigurationString s
    with Not_found -> match default with
      | None -> raise Not_found
      | Some v -> set s (ConfString v);
	v
  let use_string = useConfigurationString

  let find_list = findConfigurationList
  let use_list = useConfigurationList

end

let to_utf8 s =
  try
    if Glib.Utf8.validate s then s else Glib.Convert.locale_to_utf8 s
  with Glib.Convert.Error _ ->
    try
      Glib.Convert.convert_with_fallback
        ~fallback:"#neither UTF-8 nor locale nor ISO-8859-15#"
        ~to_codeset:"UTF-8"
        ~from_codeset:"ISO_8859-15"
        s
    with Glib.Convert.Error _ as e -> Printexc.to_string e


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

exception Found of string * string
let splitting_for_utf8 s =
  let idx = ref 0 in
  let buggy_string = "(* not a valid utf8 string *)" in
  let len = String.length s in
  try
    try
      for i = 1 to 6 do
        idx := i;
        if len = i then raise Exit;
        let pre = String.sub s 0 (len - i) in
        let suf = String.sub s (len - i) i in
        if Glib.Utf8.validate pre then raise (Found (pre, suf))
      done;
      buggy_string, ""
    with Exit ->
      buggy_string, ""
  with Found(pre, suf) ->
    pre, suf

let channel_redirector channel callback =
  let cout,cin = Unix.pipe () in
  Unix.dup2 cin channel ;
  let channel = Glib.Io.channel_of_descr cout in
  let len = 80 in
  let current_partial = ref "" in
  let buf = String.create len in
  ignore (Glib.Io.add_watch channel ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    begin fun cond ->
      try if List.mem `IN cond then begin
        (* On Windows, you must use Io.read *)
        let len = Glib.Io.read channel ~buf ~pos:0 ~len in
        len >= 1 &&
          (let full_string = !current_partial ^ String.sub buf 0 len in
           let to_emit, c = splitting_for_utf8 full_string in
           current_partial := c;
           callback to_emit)
      end
      else false
      with e ->
        ignore
          (callback
             ("Channel redirector got an exception: "
              ^ (Printexc.to_string e)));
       false
    end)

let log_redirector ?(flush=fun () -> ()) emit_string =
  let output s offset length = emit_string (String.sub s offset length) in
  Log.set_output output flush

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

(* ************************************************************************** *)
(** {2 Error manager} *)
(* ************************************************************************** *)

(**  A utility class to catch exceptions and report proper error messages. *)
class type host = object
  method error:
    'a. ?parent:GWindow.window_skel -> ('a, Format.formatter, unit) format -> 'a
  method full_protect :
    'a. cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> 'a) ->
    'a option
  method protect :
    cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> unit) -> unit
  method private set_reset: (unit -> unit) -> unit
end

class error_manager ?reset (o_parent:GWindow.window_skel) : host = 
object (self: #host)

  val mutable reset = match reset with 
  | None -> fun () -> ()
  | Some f -> f

  method private set_reset f = reset <- f

  method private error_string ?parent message =
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
    reset ()

  method error ?parent fmt =
    let b = Buffer.create 80 in
    let bfmt = Format.formatter_of_buffer b in
    Format.kfprintf
      (function fmt ->
        Format.pp_print_flush fmt ();
        let content = Buffer.contents b in
        self#error_string ?parent content)
      bfmt
      fmt

  method private display_toplevel_error ?parent ~cancelable e =
    Cmdline.error_occurred e;
    if cancelable then Project.Undo.restore ();
    self#error ?parent "%s" (Cmdline.protect e)

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
      self#error ?parent "Stopping current computation on user request.";
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
      if Kernel.debug_atleast 1 then begin
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

let refresh_gui () = while Glib.Main.iteration false do () done

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
  (match dialog#run () with
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

let later f =
  let for_idle () = f () ; false in
  let prio = Glib.int_of_priority `LOW in
  ignore (Glib.Idle.add ~prio for_idle)

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

(* Simple custom model interfaces *)
module Custom =struct

  type ('a,'b) column = 
      ?title:string -> 'b list -> ('a -> 'b list) -> GTree.view_column
      
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
    method add_column_empty : unit
  end

  class ['a] makecolumns ?packing ?width ?height (view:GTree.view) 
    (model : 'a #custom) 
    =
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
      (*TODO : get the rectangle for raw and use queue_draw_area 
	See  : http://www.gtkforums.com/viewtopic.php?t=1716
	Sadly this is not available in LablGtk2 yet...*)
      model#custom_row_changed (model#custom_get_path x) x

    method insert_row x = 
      let path = model#custom_get_path x in
      model#custom_row_inserted path x

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
      view#selection#path_is_selected (model#custom_get_path item)

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
      begin
	let path = model#custom_get_path item in
	view#scroll_to_cell path col ;
	view#selection#select_path path ;
      end

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
      ignore (view#append_column column)

  end

  (* Helper for GTK Lists *)
  module List = struct
    class type ['a] model =
    object
      method reload : unit
      method size : int
      method index : 'a -> int
      method get : int -> 'a
    end
      
    class ['a] list_model (m : 'a model) =
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

    class ['a] view 
      ?packing ?width ?height
      ?(headers=true) ?(rules=true)
      (m : 'a model) =
      let model = new list_model m in
      let view = GTree.view ~model 
	~headers_visible:headers 
	~rules_hint:rules 
	~show:true () 
      in
    object
      inherit ['a] makecolumns ?packing ?width ?height view model
    end
  end 
    
  module Tree=struct
    class type ['a] model =
    object
      method reload : unit
      method has_child : 'a -> bool
      method children : 'a option -> int
      method child_at : 'a option -> int -> 'a
      method parent : 'a -> 'a option
      method index : 'a -> int
    end

    let rec get_iter m r idx k =
      if k >= Array.length idx then r else
	let a = m#child_at r idx.(k) in
	get_iter m (Some a) idx (succ k)

    let rec get_path ks m a =
      let ks = m#index a :: ks in
      match m#parent a with
	| None -> ks 
	| Some b -> get_path ks m b
	    
    class ['a] tree_model (m : 'a model) =
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
	= Format.eprintf "Value ?@." ; assert false
	  
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

    class ['a] view 
      ?packing ?width ?height
      ?(headers=true) ?(rules=true) (m : 'a model) =
      let model = new tree_model m in
      let view = GTree.view ~model
	~headers_visible:headers
	~rules_hint:rules
	~show:true () 
      in
    object
      inherit ['a] makecolumns ?packing ?width ?height view model
    end

  end
end

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
compile-command: "make -C ../.."
End:
*)
