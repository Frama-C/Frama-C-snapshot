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

(* -------------------------------------------------------------------------- *)
(* ---  Utils                                                             --- *)
(* -------------------------------------------------------------------------- *)

let on x f = match x with None -> () | Some x -> f x
let apply fs x = List.iter (fun f -> f x) fs

module Prop :
sig
  val wrap : ('a -> 'b) -> 'a -> 'b
end =
struct
  type ('a,'b) cell = Value of 'b | Fun of ('a -> 'b)
  let make f = ref (Fun f)
  let get p x = 
    match !p with 
      | Value y -> y
      | Fun f -> let y = f x in p := Value y ; y
  let wrap f = get (make f)
end

(* -------------------------------------------------------------------------- *)
(* ---  Pango Properties                                                  --- *)
(* -------------------------------------------------------------------------- *)

let small_font = 
  Prop.wrap
    (fun f ->
       let f = Pango.Font.copy f in
       let s = Pango.Font.get_size f in
       Pango.Font.set_size f (s-2) ; f)

let bold_font =
  Prop.wrap
    (fun f ->
       let f = Pango.Font.copy f in
       Pango.Font.set_weight f `BOLD ; f)

let modify_font widget phi =
  widget#misc#modify_font (phi widget#misc#pango_context#font_description)

(* -------------------------------------------------------------------------- *)
(* ---  Gui ToolBox                                                       --- *)
(* -------------------------------------------------------------------------- *)

let set_tooltip obj tooltip = on tooltip obj#misc#set_tooltip_text

class type widget = 
object
  method set_enabled : bool -> unit
  method coerce : GObj.widget
end

class virtual ['a] handler =
object(self)
  method virtual connect : ('a -> unit) -> unit
  method on_check v f = self#connect (fun e -> f (e=v))
  method on_value v f = self#connect (fun e -> if e=v then f ())
  method on_event f = self#connect (fun _ -> f ())
end    
	  
class ['a] signal =
object
  val mutable enabled = true
  val mutable lock = false
  val mutable demon = []
  inherit ['a] handler
  method fire (x:'a) =
    if enabled && not lock then 
      try lock <- true ; apply demon x ; lock <- false
      with err -> lock <- false ; raise err
  method connect f = demon <- demon @ [f]
  method set_enabled e = enabled <- e
end

class ['a] selector default =
object(self)
  val mutable current : 'a = default
  inherit ['a] signal
  method get = current
  method set x = current <- x ; self#fire x
  method send h () : unit =
    if not lock then
      try lock <- true ; let () = h current in lock <- false
      with err -> lock <- false ; raise err
end

class widget_skel obj =
object
  method coerce : GObj.widget = obj#coerce
  method set_enabled (e:bool) : unit = obj#misc#set_sensitive e
end

(* -------------------------------------------------------------------------- *)
(* ---  Labels                                                            --- *)
(* -------------------------------------------------------------------------- *)

type align = [`Left | `Right | `Center]
type style = [`Label | `Descr | `Title]

let xalign = function `Left -> 0.0 | `Right -> 1.0 | `Center -> 0.5

class label ?(style=`Label) ?text ?(align=`Left) () =
  let w = GMisc.label ?text ~xalign:(xalign align) () in
object
  initializer match style with
    | `Label -> ()
    | `Descr ->
	w#set_line_wrap true ;
	modify_font w small_font
    | `Title ->
	modify_font w bold_font

  method coerce = w#coerce
  method set_text = w#set_text
end

(* -------------------------------------------------------------------------- *)
(* ---  Image                                                             --- *)
(* -------------------------------------------------------------------------- *)

type icon = [ GtkStock.id | `Share of string ]

let pixbufs = Hashtbl.create 63
let pixbuf (f:string) =
  try Hashtbl.find pixbufs f
  with Not_found ->
    let pixbuf =
      try GdkPixbuf.from_file (Config.datadir ^ "/" ^ f)
      with Glib.GError _ ->
        Gui_parameters.warning ~once:true
	  "Frama-C images not found. Is FRAMAC_SHARE correctly set?";
	Gtk_helper.Icon.default () in
    Hashtbl.add pixbufs f pixbuf ; pixbuf

let gimage (icon:icon) = match icon with
  | `Share f -> GMisc.image ~pixbuf:(pixbuf f) ()
  | #GtkStock.id as stock -> GMisc.image ~stock ()

(* -------------------------------------------------------------------------- *)
(* ---  Buttons                                                           --- *)
(* -------------------------------------------------------------------------- *)

class button_skel ?icon ?tooltip (button:GButton.button_skel) =
object(self)
  val mutable images = []
  initializer 
    begin
      self#set_icon icon ;
      set_tooltip button tooltip ;
      button#misc#set_can_focus false ;
      button#set_focus_on_click false ;
    end
  inherit widget_skel button
  method set_label = button#set_label
  method set_relief e = button#set_relief (if e then `NORMAL else `NONE) 
  method set_icon = function
    | None -> button#unset_image ()
    | Some icn ->
	let image =
	  try List.assoc icn images
	  with Not_found -> 
	    let img = gimage icn in 
	    images <- (icn,img)::images ; img
	in button#set_image image#coerce
end

class button ?label ?icon ?tooltip () =
  let button = GButton.button ?label ~show:true () in
object(self)
  inherit [unit] signal as s
  inherit! button_skel ?icon ?tooltip (button :> GButton.button_skel) as b
  method! set_enabled e = s#set_enabled e ; b#set_enabled e
  method default = button#grab_default
  initializer
    ignore (button#connect#clicked self#fire)
end

(* -------------------------------------------------------------------------- *)
(* ---  On/Off Buttons                                                    --- *)
(* -------------------------------------------------------------------------- *)

class checkbox ~label ?tooltip () =
  let button = GButton.check_button ~label ~show:true () in
object
  inherit [bool] selector false as s
  inherit! widget_skel button as b
  method! set_enabled e = s#set_enabled e ; b#set_enabled e
  method! set a = s#set a ; button#set_active a
  initializer 
    begin
      set_tooltip button tooltip ;
      ignore (button#connect#clicked (fun () -> s#set button#active)) ;
    end
end
  
class toggle ?label ?icon ?tooltip () =
  let button = GButton.button ?label ~show:true ~relief:`NONE () in
object
  inherit [bool] selector false as s
  inherit! button_skel ?icon ?tooltip (button :> GButton.button_skel) as b
  method! set_enabled e = s#set_enabled e ; b#set_enabled e
  method! set a = s#set a ; button#set_relief (if a then `NORMAL else `NONE)
  initializer ignore (button#connect#clicked (fun () -> s#set (not s#get)))
end
  
class radio ~label ?tooltip () =
  let button = GButton.radio_button ~label ~show:true () in
object
  inherit [bool] selector false as s
  inherit! widget_skel button
  method! set e = s#set e ; if e then button#set_active true
  method group = function
    | None -> Some button#group 
    | (Some g) as sg -> button#set_group g ; sg
  initializer 
    begin
      set_tooltip button tooltip ;
      ignore (button#connect#clicked (fun () -> s#set button#active)) ;
    end
end

class switchbox ?tooltip () =
  let pix_on = pixbuf "feedback/switch-on.png" in
  let pix_off = pixbuf "feedback/switch-off.png" in
  let evt = GBin.event_box () in
  let img = GMisc.image ~pixbuf:pix_on ~packing:evt#add () in
object(self)
  inherit [bool] selector false as s
  inherit! widget_skel evt as b
  method! set_enabled e = s#set_enabled e ; b#set_enabled e
  method! set a = s#set a ; img#set_pixbuf (if a then pix_on else pix_off)
  initializer 
    begin
      set_tooltip evt tooltip ;
      ignore (evt#event#connect#button_release 
		(fun _evt -> self#set (not s#get) ; false)) ;
    end
end

(* -------------------------------------------------------------------------- *)
(* ---  Spinner                                                           --- *)
(* -------------------------------------------------------------------------- *)

class spinner ?min ?max ?(step=1) ~value ?tooltip () =
  let b = GEdit.spin_button ~digits:0 () in
object
  inherit [int] selector value as s
  inherit! widget_skel b
  method! set_enabled e = s#set_enabled e ; b#misc#set_sensitive e
  method! set a = s#set a ; b#set_value (float value)
  initializer
    begin
      set_tooltip b tooltip ;
      let fmap = function None -> None | Some x -> Some (float x) in
      b#adjustment#set_bounds
	?lower:(fmap min) ?upper:(fmap max)
	~step_incr:(float step) () ;
      b#set_value (float value) ;
      let callback () = s#set b#value_as_int in
      ignore (b#connect#value_changed ~callback) ;
    end
end

(* -------------------------------------------------------------------------- *)
(* ---  Switches                                                          --- *)
(* -------------------------------------------------------------------------- *)

class ['a] switch (default : 'a) =
object(self)
  inherit ['a] selector default
  val mutable cases : (bool selector * 'a) list = []
  val mutable group = None
  initializer self#connect 
    (fun v -> List.iter 
       (fun (w,v0) -> w#set (v=v0)) cases)

  method private add_case (w : bool selector) (v : 'a) =
    begin
      w#set ( v = self#get ) ;
      w#connect (fun e -> if e then self#set v) ;
      cases <- (w,v) :: cases ;
    end

  method add_toggle ?label ?icon ?tooltip ~value () =
    let toggle = new toggle ?label ?icon ?tooltip () in
    self#add_case (toggle :> bool selector) value ;
    (toggle :> widget)

  method add_radio ~label ?tooltip ~value () =
    let radio = new radio ~label ?tooltip () in
    self#add_case (radio :> bool selector) value ;
    group <- radio#group group ;
    (radio :> widget)

  method! set_enabled e = 
    List.iter (fun (w,_) -> w#set_enabled e) cases

end

(* -------------------------------------------------------------------------- *)
(* ---  PopDown                                                           --- *)
(* -------------------------------------------------------------------------- *)

class ['a] menulist ~default ~render ?(items=[]) () =
  let strings = List.map render items in
  let (cmb,(model,_)) as combo = GEdit.combo_box_text ~strings ~wrap_width:1 () in
object(self)

  inherit widget_skel cmb as widget
  inherit! ['a] selector default as select

  val mutable items = Array.of_list items

  method! set_enabled e = 
    select#set_enabled e ; widget#set_enabled e

  method get_items = Array.to_list items
    
  method set_items xs =
    begin
      items <- Array.of_list xs ; model#clear () ;
      Array.iter (fun x -> GEdit.text_combo_add combo (render x)) items ;
      let e = select#get in
      if not lock then
	begin
	  lock <- true ;
	  Array.iteri (fun i x -> if x=e then cmb#set_active i) items ;
	  lock <- false ;
	end ;
    end

  method private clicked () =
    try if not lock then select#set items.(cmb#active)
    with _ -> ()

  method! set x =
    begin
      select#set x ; 
      Array.iteri (fun i e -> if x=e then cmb#set_active i) items ;
    end
 
  initializer
    ignore (cmb#connect#changed self#clicked) ;

end

(* -------------------------------------------------------------------------- *)
(* ---  Popup Menu                                                        --- *)
(* -------------------------------------------------------------------------- *)

class popup () =
  let menu = GMenu.menu () in
object
  
  val mutable empty = true
  val mutable separator = false
  
  method clear =
    List.iter menu#remove menu#children
  
  method add_separator = separator <- true
    
  method add_item ~label ~callback =
    if not empty && separator then
      ignore (GMenu.separator_item ~packing:menu#append ()) ;
    let item = GMenu.menu_item ~label ~packing:menu#append () in
    ignore (item#connect#activate ~callback) ;
    empty <- false ; separator <- false

  method popup () =
    let time = GMain.Event.get_current_time () in
    menu#popup ~button:3 ~time
      
end

(* -------------------------------------------------------------------------- *)
(* ---  Rack                                                              --- *)
(* -------------------------------------------------------------------------- *)
  
class rack (widgets : widget list) =
  let box = GPack.hbox ~homogeneous:true ~spacing:0 ~border_width:0 () in
object
  initializer List.iter (fun w -> box#add w#coerce) widgets
  method set_enabled e = List.iter (fun w -> w#set_enabled e) widgets
  method coerce = box#coerce
end

(* -------------------------------------------------------------------------- *)
(* ---  File Chooser                                                      --- *)
(* -------------------------------------------------------------------------- *)

type filekind = [ `FILE | `DIR ]

class filechooser_dialog 
  ?(kind=`FILE)
  ?(title="Select File") 
  ?(select="Select") 
  ?parent () =
  let dialog = GWindow.dialog ~title ?parent ~modal:true () in 
  let packing = dialog#vbox#pack ~expand:true in
  let action = match kind with `FILE -> `SAVE | `DIR -> `CREATE_FOLDER in
  let chooser = GFile.chooser_widget ~action ~packing () in  
object

  inherit [string] signal as signal

  initializer
    begin
      ignore (dialog#event#connect#delete (fun _ -> true)) ;
      dialog#add_button "Cancel" `DELETE_EVENT ;
      dialog#add_button select `SELECT ;
      ignore (GMisc.label ~packing:(dialog#action_area#pack ~expand:true) ()) ;
    end

  method filter ~descr ~patterns =
    if kind = `FILE then
      chooser#add_filter (GFile.filter ~name:descr ~patterns ())

  method select ?dir ?file () =
    begin
      match dir , file with
	| None , None -> ignore (chooser#set_filename "")
	| None , Some path -> ignore (chooser#set_filename path)
	| Some dir , None -> 
	    ignore (chooser#set_current_folder dir) ;
	    ignore (chooser#set_current_name "")
	| Some dir , Some file -> 
	    ignore (chooser#set_current_folder dir) ;
	    ignore (chooser#set_current_name file)
    end ;
    let result = dialog#run () in
    dialog#misc#hide () ;
    match result with
      | `DELETE_EVENT -> ()
      | `SELECT -> 
	  match chooser#get_filenames with | f::_ -> signal#fire f | _ -> ()

end

class filechooser_button ?kind ?title ?select ?tooltip ?parent () =
  let box = GPack.hbox ~homogeneous:false ~spacing:0 ~border_width:0 () in
  let fld = GMisc.label ~text:"(none)" ~xalign:0.0 
    ~packing:(box#pack ~expand:true) () in
  let _ = GMisc.separator `VERTICAL 
    ~packing:(box#pack ~expand:false ~padding:2) ~show:true () in
  let _ = GMisc.image  ~packing:(box#pack ~expand:false) ~stock:`OPEN () in
  let button = GButton.button () in
  let dialog = new filechooser_dialog ?kind ?title ?select ?parent () in
object(self)

  inherit widget_skel button
  inherit! [string] selector "" as current

  val mutable disptip = fun f ->
    match tooltip , f with
      | None , "" -> "(none)"
      | None , _ -> f
      | Some d , "" -> d
      | Some d , f -> Printf.sprintf "%s: %s" d f 
   
  val mutable display = function
    | "" -> "(none)"
    | path -> Filename.basename path

  initializer 
    begin
      button#add box#coerce ;
      button#set_focus_on_click false ;
      ignore (button#connect#clicked self#select) ;
      dialog#connect current#set ;
      set_tooltip button tooltip ;
      current#connect 
	(fun f ->
	   button#misc#set_tooltip_text (disptip f) ;
	   fld#set_text (display f)) ;
    end

  method tooltip p = disptip <- p ; fld#misc#set_tooltip_text (p current#get)
  method display p = display <- p ; fld#set_text (p current#get)

  method filter = dialog#filter
  method select ?dir ?file () = 
    let file = match file with None -> current#get | Some f -> f in
    dialog#select ?dir ~file ()

end

(* -------------------------------------------------------------------------- *)
(* ---  Forms                                                             --- *)
(* -------------------------------------------------------------------------- *)

type field = [ `Compact | `Field | `Editor ]

let fexpand = function `Compact -> `NONE | `Field -> `X | `Editor -> `BOTH

class form () =
  let box = GPack.table ~columns:2 ~col_spacings:16 ~homogeneous:false () in
object(self)
  val mutable line  = 0
  val mutable left  = false (* left column feeded on current line *)
  val mutable right = false (* right column feeded on current line *)
  val mutable xpadding = 0  (* set with sections *)

  inherit widget_skel box

  method private occupy_left =
    if left || right then line <- succ line ;
    left <- true ; right <- false
      
  method private occupy_right =
    if right then (line <- succ line ; left <- false) ;
    right <- true

  method private occupy_both =
    if left || right then line <- succ line ;
    left <- true ; right <- true
      
  method add_newline =
    self#occupy_both ;
    let w = GMisc.label ~text:"" () in
    box#attach ~left:0 ~right:1 ~top:line ~ypadding:12 ~expand:`Y w#coerce

  method add_section label =
    self#occupy_both ;
    let w = GMisc.label ~text:label ~xalign:0.0 ~yalign:1.0 () in
    modify_font w bold_font ;
    xpadding <- 24 ;
    box#attach 
      ~left:0 ~right:1 ~top:line ~xpadding:0 ~ypadding:12 ~expand:`Y w#coerce

  method add_label_widget w =
    self#occupy_left ;
    box#attach ~left:0 ~top:line ~xpadding ~expand:`NONE w

  method add_label label = 
    let w = GMisc.label ~text:label ~xalign:1.0 () in
    self#add_label_widget w#coerce
      
  method add_field ?label ?(field:field=`Field) w =
    on label self#add_label ;
    self#occupy_right ;
    box#attach ~left:1 ~top:line ~expand:(fexpand field) w
      
  method add_row ?(field:field=`Field) w =
    self#occupy_both ;
    box#attach ~left:0 ~right:1 ~top:line ~expand:(fexpand field) w

end

(* -------------------------------------------------------------------------- *)
(* ---  List                                                              --- *)
(* -------------------------------------------------------------------------- *)

module L = Gtk_helper.Custom.List

let bound a x b = if x < a then a else if x > b then b else x

class ['a] lmodel render =
object
  val mutable store : 'a array = [| |]
  val mutable items : 'a array = [| |]
  method reload = items <- store
  method size = Array.length items
  method index k : int = k
  method get k : int = k
    
  method render k : GTree.cell_properties_text list =
    if 0 <= k && k <= Array.length items 
    then [`TEXT (render items.(k))]
    else [`TEXT ""]

  method set_items xs = store <- Array.of_list xs
  method get_items = Array.to_list items

  method get_item k = items.(k)

  method insert_item k e =
    begin
      let n = Array.length items in
      let k = bound 0 k n in
      store <- Array.create (n+1) e ;
      if k>0 then Array.blit items 0 store 0 (k-1) ;
      if k<n then Array.blit items k store (k+1) (n-k) ;
    end

  method remove_item k =
    let n = Array.length items in
    if 0 <= k && k < n then
      begin
	store <- Array.sub items 0 (n-1) ;
	Array.blit items (k+1) store k (n-k-1) ;
      end
    else
      store <- items

  method move_up k =
    if 1 <= k && k < Array.length items then
      begin
	store <- Array.copy items ;
	store.(k-1) <- items.(k) ;
	store.(k) <- items.(k-1) ;
      end

  method move_down k = 
    if 0 <= k && k+1 < Array.length items then
      begin
	store <- Array.copy items ;
	store.(k) <- items.(k+1) ;
	store.(k+1) <- items.(k) ;
      end

end

class ['a] listbox ~(render : 'a -> string) ?width ?(height=80) () =
  let model = new lmodel render in
  let wlist = new L.view 
    ~headers:false ~rules:false ?width ~height (model :> int L.model) in
  let witem = wlist#add_column_text [] model#render in
  let wbox = GPack.vbox ~homogeneous:false () in
  let hbox = GPack.hbox ~homogeneous:true ~spacing:0 () in
  let b_add = new button ~icon:`ADD () in
  let b_del = new button ~icon:`REMOVE () in
  let b_up  = new button ~icon:`GO_UP () in
  let b_dn  = new button ~icon:`GO_DOWN () in
  let insert = new signal in
  let change = new signal in
object(self)
  val mutable enabled = false
  val mutable selected = (-1)

  initializer
    begin
      wbox#pack ~expand:true wlist#coerce ;
      hbox#pack ~padding:32 ~expand:false b_add#coerce ;
      hbox#pack ~padding:0 ~expand:false b_del#coerce ;
      hbox#pack ~padding:0 ~expand:false b_up#coerce ;
      hbox#pack ~padding:0 ~expand:false b_dn#coerce ;
      wbox#pack ~expand:false hbox#coerce ;
      wlist#on_click (fun k _ -> selected <- k ; self#buttons) ;
      b_up#connect self#up ;
      b_dn#connect self#dn ;
      b_add#connect self#add ;
      b_del#connect self#del ;
    end

  method private up () =
    begin
      model#move_up selected ;
      selected <- pred selected ;
      wlist#reload ;
      wlist#set_focus selected witem ;
      self#buttons ;
      change#fire model#get_items ;
    end

  method private dn () =
    begin
      model#move_down selected ;
      selected <- succ selected ;
      wlist#reload ;
      self#buttons ;
      wlist#set_focus selected witem ;
      self#buttons ;
      change#fire model#get_items ;
    end

  method private add () =
    let n = model#size in
    if 0 <= selected && selected <= n then
      insert#fire selected

  method private del () =
    begin
      model#remove_item selected ;
      wlist#reload ;
      self#buttons ;
      change#fire model#get_items ;
    end
      
  method private buttons =
    begin
      let n = model#size in
      wlist#view#misc#set_sensitive enabled ;
      b_add#set_enabled ( enabled ) ;
      b_del#set_enabled ( enabled && 0 <= selected && selected < n ) ;
      b_up#set_enabled  ( enabled && 0 <  selected && selected < n ) ;
      b_dn#set_enabled  ( enabled && 0 <= selected && selected+1 < n ) ;
    end

  method coerce = wbox#coerce
  method set_enabled e = enabled <- e ; self#buttons

  method get = model#get_items
  method set xs = model#set_items xs ; wlist#reload ; change#fire xs
  method send r () : unit = r model#get_items
  method insert k x = 
    begin
      model#insert_item k x ; 
      wlist#reload ;
      wlist#set_focus k witem ;
      selected <- k ;
      self#buttons ;
    end

  method fire = change#fire
  method on_insert_request = insert#connect
  method connect = change#connect
  inherit [_] handler
    
end

(* -------------------------------------------------------------------------- *)
(* ---  Extensible Array                                                  --- *)
(* -------------------------------------------------------------------------- *)

class type entry = 
object
  method widget : GObj.widget
  method update : unit -> unit
  method delete : unit -> unit
end

class ['a] warray ?(dir=`VERTICAL) () =
  let box = GPack.box dir ~homogeneous:false () in
object(self)

  val mutable rows : ('a * entry) list = []
  val mutable creator : ('a -> entry) = (fun _ -> assert false)
 
  inherit widget_skel box

  method set xs =
    begin
      List.iter
	(fun (y,e) ->
	   if not (List.mem y xs) then
	     begin
	       e#delete () ;
	       let w = e#widget in
	       box#remove w ;
	       w#destroy () ;
	     end)
	rows ;
      rows <- List.map
	(fun x ->
	   let e = 
	     try List.assoc x rows
	     with Not_found ->
	       let e = creator x in
	       box#pack ~expand:false e#widget ; e
	   in x,e) xs ;
      ignore 
	(List.fold_left
	   (fun pos (_,w) -> box#reorder_child w#widget ~pos ; succ pos)
	   0 rows)
    end

  method get = List.map fst rows
  method mem x = List.mem_assoc x rows

  method private others x =
    List.fold_right (fun (y,_) ys -> if x=y then ys else y::ys) rows []
      
  method append x = self#set ( self#others x @ [x] )

  method insert ?after x =
    let ys = self#others x in
    let zs = match after with
      | None -> x :: ys
      | Some z ->
	  let rec hook z x = function
	    | [] -> [x]
	    | y::ys ->
		if y = z then z :: x :: ys
		else y :: hook z x ys
	  in hook z x ys
    in self#set zs
	 
  method remove x = self#set (self#others x)

  method create f = creator <- f

  method update () =
    List.iter (fun (_,e) -> e#update ()) rows

end

(* -------------------------------------------------------------------------- *)
(* ---  Notebook                                                          --- *)
(* -------------------------------------------------------------------------- *)
      
class ['a] notebook ?tabs ~default () =
  let view = GPack.notebook ~enable_popup:false ~show_tabs:false ~show:true () in
object(self)
  val mutable pages : 'a list = []
  inherit ['a] selector default as select
  method add ?label page content =
    let tab_label = match label with
      | None -> None
      | Some text -> Some (GMisc.label ~text ())#coerce
    in
    pages <- pages @ [page] ;
    ignore (view#append_page ?tab_label content) ;
    self#set default
  method! set page =
    let rec scan i p = function
      | q::qs -> if p=q then view#goto_page i else scan (succ i) p qs
      | [] -> ()
    in scan 0 page pages
  method private switched i =
    try select#set (List.nth pages i)
    with Invalid_argument _ -> ()
  method on_focus page f = select#connect (fun p -> f (page = p))
  initializer
    begin
      ignore (view#connect#switch_page self#switched) ;
      on tabs (fun p -> view#set_show_tabs true ; view#set_tab_pos p) ;
    end
  method coerce = view#coerce
  method! set_enabled = view#misc#set_sensitive
end

(* -------------------------------------------------------------------------- *)
(* ---  Dialogs                                                           --- *)
(* -------------------------------------------------------------------------- *)

type 'a action = 
    [ 
    | `CANCEL | `APPLY
    | `DEFAULT of 'a
    | `SELECT of 'a
    | `ALT of 'a
    | `ACTION of (unit -> unit)
    ]
      
class ['a] dialog ~title ~window ?(resize=false) () =

  let shell = GWindow.window 
    ~title ~kind:`TOPLEVEL ~modal:true
    ~show:false ~decorated:true ~position:`CENTER_ON_PARENT 
    ~allow_grow:resize () 
  in

  let hclip = GBin.alignment ~packing:shell#add () in
  let vbox = GPack.vbox ~homogeneous:false ~spacing:6 
    ~packing:hclip#add () in
  let vclip = GBin.alignment 
    ~packing:(vbox#pack ~from:`END ~expand:false) () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:32 
    ~packing:vclip#add () in
  let alt_box = GPack.hbox ~homogeneous:true ~spacing:6 
    ~packing:(hbox#pack ~expand:true ~fill:false) () in
  let main_box = GPack.hbox ~homogeneous:true ~spacing:6 
    ~packing:(hbox#pack ~expand:true ~fill:false) () in

object(self)

  constraint 'a = [> `CANCEL | `APPLY]

  inherit ['a] signal

  val mutable defw = (fun () -> ())
  
  method add_row w = 
    vbox#pack ~from:`START ~expand:false w

  method add_block w = 
    vbox#pack ~from:`START ~expand:true w

  method button ~(action : 'a action) ?label ?icon ?tooltip () =
    let w = new button ?label ?icon ?tooltip () in
    let box = match action with
      | `DEFAULT _ | `APPLY -> defw <- w#default ; main_box
      | `SELECT _ | `CANCEL -> main_box 
      | `ALT _ | `ACTION _ -> alt_box
    in box#pack ~expand:false w#coerce ;
    match action with
      | `ALT r | `SELECT r | `DEFAULT r ->
	  w#connect (fun () -> self#select r)
      | `CANCEL ->
	  w#connect (fun () -> self#select `CANCEL)
      | `APPLY ->
	  w#connect (fun () -> self#select `APPLY)
      | `ACTION f -> 
	  w#connect f
	    
  method select r =
    begin
      window#misc#set_sensitive true ;
      shell#misc#hide () ;
      self#fire r ;
    end

  method run () =
    begin
      window#misc#set_sensitive false ;
      shell#show () ;
      defw () ;
    end

  initializer
    begin
      hclip#set_top_padding 4 ;
      hclip#set_bottom_padding 4 ;
      hclip#set_left_padding 24 ;
      hclip#set_right_padding 24 ;
      ignore (shell#event#connect#delete 
		(fun _ -> self#select `CANCEL ; true)) ;
      (* returning [true] prevent the dialog from being destroyed *)
    end
    
end

(* -------------------------------------------------------------------------- *)
(* --- Text with Tagging Formatter                                        --- *)
(* -------------------------------------------------------------------------- *)
  
type tag =
  | TAG of GText.tag
  | LINK of int * string
  | MARK of int * string
  | PLAIN
      
let split tag =
  let rec lookup tag k n = 
    if k < n then
      if tag.[k] = ':' 
      then String.sub tag 0 k , String.sub tag (k+1) (n-k-1)
	else lookup tag (succ k) n
    else tag,""
  in lookup tag 0 (String.length tag)
       
let rec tags tgs = function
  | [] -> tgs
  | TAG t :: style -> tags (t::tgs) style
  | (LINK _ | MARK _ | PLAIN) :: style -> tags tgs style
      
class text () =
  let buffer = GText.buffer () in
  let view = GText.view ~buffer 
    ~editable:false ~cursor_visible:false
    ~justification:`LEFT
    ~wrap_mode:`NONE
    ~accepts_tab:false
    ~show:true () in
  let scroll = GBin.scrolled_window () in
object(self)
    
  (* -------------------------------------------------------------------------- *)
  (* --- Text Formatter                                                     --- *)
  (* -------------------------------------------------------------------------- *)
  
  val text = Buffer.create 80
  val css = Hashtbl.create 31
  val marks = Hashtbl.create 131
  val links = Hashtbl.create 131
  val mutable printf = false
  val mutable style = []
  val mutable fmtref = None
  val mutable demon = []
    
  method fmt = match fmtref with Some fmt -> fmt | None ->
    let output_string s a b = if b > 0 then Buffer.add_substring text s a b in
    let output_flush () =
      if Buffer.length text > 0 then
	begin
	  let s = Gtk_helper.to_utf8 (Buffer.contents text) in
	  let tags = tags [] style in
	  let iter = buffer#end_iter in
	  Buffer.clear text ;
	  printf <- true ;
	  buffer#insert ~tags ~iter s ;
	  printf <- false ;
	end in
    let output_open_tag t = 
      output_flush () ; style <- self#open_tag t :: style ; "" in
    let output_close_tag _t = 
      output_flush () ; match style with
	| [] -> ""
	| s::sty -> self#close_tag s ; style <- sty ; "" in
    let fmt = Format.make_formatter output_string output_flush in
    let tagger = Format.pp_get_formatter_tag_functions fmt () in
    Format.pp_set_formatter_tag_functions fmt 
      { tagger with
	  Format.mark_open_tag = output_open_tag ;
  	  Format.mark_close_tag = output_close_tag ;
      } ;
    Format.pp_set_print_tags fmt false ;
    Format.pp_set_mark_tags fmt true ;
    fmtref <- Some fmt ; fmt
      
  (* -------------------------------------------------------------------------- *)
  (* --- Tag Marking                                                        --- *)
  (* -------------------------------------------------------------------------- *)
      
  method private css_style t p =
    let sty = TAG (buffer#create_tag p) in
    Hashtbl.replace css t sty ; sty
      
  method private link_tag lnk =
    try Hashtbl.find links lnk
    with Not_found ->
      let tag = buffer#create_tag [] in
      let callback tag lnk ~origin evt iter = 
	ignore origin ; self#cb_link tag lnk evt iter in
      ignore (tag#connect#event ~callback:(callback tag lnk)) ;
      Hashtbl.add links lnk tag ; tag

  method private open_tag t =
    try Hashtbl.find css t
    with Not_found ->
      match t with
	| "ul" -> self#css_style t [ `UNDERLINE `SINGLE ]
	| "st" -> self#css_style t [ `STRIKETHROUGH true ]
	| "bf" -> self#css_style t [ `WEIGHT `BOLD ]
	| "it" -> self#css_style t [ `STYLE `ITALIC ]
	| "red" -> self#css_style t [ `FOREGROUND "red" ]
	| "blue" -> self#css_style t [ `FOREGROUND "blue" ]
	| "green" -> self#css_style t [ `FOREGROUND "darkgreen" ]
	| "orange" -> self#css_style t [ `FOREGROUND "orange" ]
	| _ -> match split t with
	    | "link",url -> LINK(buffer#end_iter#offset,url)
	    | "mark",mrk -> MARK(buffer#end_iter#offset,mrk)
	    | "fg",color -> self#css_style t [ `FOREGROUND color ]
	    | "bg",color -> self#css_style t [ `BACKGROUND color ]
	    | _ -> PLAIN
		
  method private close_tag = function
    | LINK(p,lnk) -> 
	let start = buffer#get_iter (`OFFSET p) in
	let stop = buffer#end_iter in
	let tag = self#link_tag lnk in
	buffer#apply_tag tag ~start ~stop ;
    | MARK(p,mrk) ->
	let start = buffer#create_mark (buffer#get_iter (`OFFSET p)) in
	let stop = buffer#create_mark buffer#end_iter in
	let tag = buffer#create_tag [] in
	Hashtbl.replace marks mrk (tag,start,stop)
    | _ -> ()
	
  (* -------------------------------------------------------------------------- *)
  (* --- Tag Callback                                                       --- *)
  (* -------------------------------------------------------------------------- *)
	
  method private cb_link tag lnk evt _iter =
    match GdkEvent.get_type evt with
      | `BUTTON_PRESS -> 
	  let evt = GdkEvent.Button.cast evt in
	  if GdkEvent.Button.button evt = 1 then apply demon lnk ;
	  true
      | `ENTER_NOTIFY -> 
	  tag#set_property (`FOREGROUND "blue") ;
	  tag#set_property (`UNDERLINE `SINGLE) ;
	  true
      | `LEAVE_NOTIFY -> 
	  tag#set_property (`FOREGROUND_SET false) ;
	  tag#set_property (`UNDERLINE `NONE) ;
	  true
      | _ -> true
	  
  (* -------------------------------------------------------------------------- *)
  (* --- Text Initializer                                                   --- *)
  (* -------------------------------------------------------------------------- *)
	  
  initializer 
    begin
      view#misc#modify_font_by_name "Monospace" ;
      scroll#add view#coerce
    end
      
  (* -------------------------------------------------------------------------- *)
  (* --- User API                                                           --- *)
  (* -------------------------------------------------------------------------- *)
      
  method printf : 'b. ('b,Format.formatter,unit) format -> 'b =
    fun text -> Format.fprintf self#fmt text
      
  method clear = 
    Hashtbl.clear marks ;
    Hashtbl.clear links ;
    Format.pp_print_flush self#fmt () ;
    buffer#delete ~start:buffer#start_iter ~stop:buffer#end_iter 
      
  method on_link f = demon <- demon @ [f]
    
  method focus ~mark =
    try 
      let _,mark,_ = Hashtbl.find marks mark in
      let iter = buffer#get_iter (`MARK mark) in
      ignore (view#scroll_to_iter ~use_align:true ~yalign:0.2 iter)
    with Not_found -> ()
      
  method highlight ~mark properties =
    try
      let tag,_,_ = Hashtbl.find marks mark in
      List.iter tag#set_property properties
    with Not_found -> ()
      
  method coerce = scroll#coerce 
  method set_enabled (_:bool) = () (* ignored *)
    
end


