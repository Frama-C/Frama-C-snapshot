(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: viewer.ml,v 1.15 2008/02/05 12:10:48 marche Exp $ i*)

open Format
open Options
open Ast
open Env

let _ = gui := true

let typed_progs = ref []

let deal_file f =
  Loc.set_file f;
  Main.reset ();
  let cin = open_in f in 
  (*c_file := Filename.check_suffix f ".c";*)
  let parsef = (*if !c_file then Main.c_parser else *) Main.why_parser in
  let p = parsef cin in
  if parse_only then exit 0;
  List.iter Main.interp_decl p;
  typed_progs := (f, List.rev !Main.typed_progs) :: !typed_progs;
  close_in cin

let _ =
  if prelude then Main.load_prelude ();
  if files = [] then begin eprintf "usage: gwhy files@."; exit 1 end;
  try
    List.iter deal_file Options.files;
    typed_progs := List.rev !typed_progs
  with e ->
    Report.explain_exception err_formatter e;
    pp_print_newline err_formatter ();
    exit 1

module Tree = struct

  type t = typed_program
	     
  type info = Loc.t * string

  let info = 
    let buf = Buffer.create 1024 in
    let fmt = formatter_of_buffer buf in
    fun x -> 
      fprintf fmt "%a@." Util.print_typing_info x.info;
      let s = Buffer.contents buf in
      Buffer.reset buf;
      let loc = 
	let (b,e) = x.info.t_loc in b.Lexing.pos_cnum, e.Lexing.pos_cnum 
      in
      loc, s

  let show_info_ref = ref (fun i -> ())
  let show_info i = !show_info_ref i

  let rec statements = function
    | [] -> []
    | Statement x :: bl -> x :: statements bl
    | _ :: bl -> statements bl

  let children x = match x.desc with
    | Lam (_, e)
    | Rec (_, _, _, _, e)
    | Loop (_,_,e)
    | Raise (_, Some e) -> [e]
    | LetIn (_, e1, e2)
    | LetRef (_, e1, e2) -> [e1; e2]
    | App (e1, Term e2, _) -> [e2; e1]
    | App (e, _, _) -> [e]
    | If (e1, e2, e3) -> [e1; e2; e3]
    | Seq bl -> statements bl
    | Try (e, pl) -> e :: List.map snd pl
    | Raise _ 
    | Var _ | Expression _ | Absurd | Any _ -> []

end

module NavTree = Navig.MakeNavTree(Tree)
module Nav = Navig.MakeNavigator(NavTree)
let navigation = ref false

(* load source files and store them in an array *)

type source_file = { 
  filename : string;
  text : Hilight.token list;
  asts : typed_program list
}

open Hilight

let load_source file = 
  try
    let ic = open_in_bin file in 
    let lb = Lexing.from_channel ic in
    let s = 
      if Filename.check_suffix file ".c" 
      then Hilight.next_code_c lb else Hilight.next_code lb
    in
    close_in ic;
    s
  with _ -> 
    eprintf "warning: couldn't load %s\n" file;
    []

let source_files : source_file array =
  let a = Array.create (List.length !typed_progs) 
	    { filename = ""; text = []; asts = [] }
  in
  let rec fill i = function
    | [] -> 
	()
    | (f, al) :: r -> 
	a.(i) <- { filename = f; text = load_source f; asts = al };
	fill (succ i) r
  in
  fill 0 !typed_progs;
  a

let nb_files = Array.length source_files
let current_source = ref 0

(* GTK *)

let window_width = 800
let window_height = 900
let show_discharged = ref false

let monospace_font = ref (Pango.Font.from_string "Monospace 15")
let general_font = ref (Pango.Font.from_string "Monospace 15")

let lower_view_general_font = general_font
let upper_view_general_font = general_font
let statusbar_font = ref !general_font
let proved_lemma_font = ref !monospace_font
let to_prove_lemma_font = ref !monospace_font
let discharged_lemma_font = ref !monospace_font
let display_info = ref (function s -> failwith "not ready")
let flash_info = ref (function s -> failwith "not ready")

let out_some = function None -> assert false | Some f -> f

let try_convert s = 
  try
    if Glib.Utf8.validate s then s else 
      Glib.Convert.locale_to_utf8 s
  with _ -> failwith ("Fatal error: wrong encoding in string \""^s^"\"")

let path_of_int i = 
  GtkTree.TreePath.from_string (string_of_int (if i < 0 then 0 else i))

let to_refresh reference e = 
  let n = Filename.chop_extension reference ^ e in
  not (Sys.file_exists n) ||
  ((Unix.stat reference).Unix.st_mtime > (Unix.stat n).Unix.st_mtime)


let rec insert_user_annotation
  (tb:GText.buffer) (output:GText.buffer) s it 
  (old_tag_props,new_tag_props) (infos,message) =
  let new_tag = tb#create_tag new_tag_props in
  tb#insert ~tags:[new_tag] ~iter:it s

let insert_chunk (tb:GText.buffer) (output:GText.buffer) c = match c with
  | Code s -> tb#insert (try_convert s)
  | Annotation s ->
      let t = tb#end_iter in
      insert_user_annotation tb output (try_convert s) t
	([`BACKGROUND "darkgreen"],
	 [`FOREGROUND "darkgreen"])
	([],"User obligation")
 
let create_mark_at (tb:GText.buffer) pos =
  tb#create_mark (tb#get_iter_at_char pos)

let insert_obligations (tb:GText.buffer) (output:GText.buffer) 
  (obligations:Log.t) =
  let marks =
    List.map 
      (function (pos,_,_) as o ->  `MARK (create_mark_at tb pos),o ) 
      obligations
  in 
  ()

let display_source (b:GText.buffer) (tb2:GText.buffer) =
  b#set_text "";  
  tb2#set_text "";
  let source = source_files.(!current_source) in
  List.iter (insert_chunk b tb2) source.text;
  match source.asts with
    | [] -> navigation := false
    | _ -> navigation := true; Nav.set (NavTree.create source.asts)

let next_source (b:GText.buffer) (tb2:GText.buffer) () =
  if !current_source < nb_files - 1 then begin
    incr current_source; display_source b tb2
  end

let previous_source (b:GText.buffer) (tb2:GText.buffer) () =
  if !current_source > 0 then begin
    decr current_source; display_source b tb2
  end

let main () = 
  let w = GWindow.window 
	    ~allow_grow:true ~allow_shrink:true
	    ~width:window_width ~height:window_height ~title:"Why viewer" ()
  in
  w#misc#modify_font !general_font;
  let accel_group = GtkData.AccelGroup.create () in
  let _ = w#connect#destroy ~callback:(fun () -> exit 0) in
  let vbox = GPack.vbox ~homogeneous:false ~packing:w#add () in

  (* Menu *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let file_factory = new GMenu.factory file_menu ~accel_group in
  let quit_m = file_factory#add_item "Quit" ~key:GdkKeysyms._Q
	       ~callback:(fun () -> exit 0) in
  let refresh_m = file_factory#add_item "Refresh" ~key:GdkKeysyms._R in
  let configuration_menu = factory#add_submenu "Configuration" in
  let configuration_factory = new GMenu.factory configuration_menu ~accel_group
  in
  let show_discharged_m = configuration_factory#add_check_item 
			    "Show discharged proof" 
			  ~key:GdkKeysyms._D
			  ~callback:(fun b -> show_discharged := b) 
  in
  let customize_colors_m =
    configuration_factory#add_item "Customize colors"
    ~callback:(fun () -> !flash_info "Not implemented")
  in
  let customize_fonts_m = 
    configuration_factory#add_item "Customize fonts"
    ~callback:(fun () -> !flash_info "Not implemented")
  in
  let _ = file_factory#add_item "Up" ~key:GdkKeysyms._Up
	  ~callback:(fun () -> if !navigation then Nav.up ()) in
  let _ = file_factory#add_item "Down" ~key:GdkKeysyms._Down
	  ~callback:(fun () -> if !navigation then Nav.down ()) in
  let _ = file_factory#add_item "Left" ~key:GdkKeysyms._Left
	  ~callback:(fun () -> if !navigation then Nav.left ()) in
  let _ = file_factory#add_item "Right" ~key:GdkKeysyms._Right
	  ~callback:(fun () -> if !navigation then Nav.right ()) in
  let _ = file_factory#add_item "Next" ~key:GdkKeysyms._space
	  ~callback:(fun () -> if !navigation then Nav.next ()) in

  (* horizontal paned *)
  let hp = GPack.paned `HORIZONTAL  ~border_width:3 ~packing:vbox#add () in

  (* left list of source files *)
  let cols = new GTree.column_list in
  let filename_col = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  Array.iteri 
    (fun i sf ->     
       let row = model#append () in
       model#set ~row ~column:filename_col (Filename.basename sf.filename)) 
    source_files;
  let view = GTree.view ~model ~packing:hp#add1 () in
  let _ = view#selection#set_mode `SINGLE in
  let _ = view#set_rules_hint true in
  let col = GTree.view_column ~title:"Source files" ()
	      ~renderer:(GTree.cell_renderer_text [], ["text",filename_col]) in
  let _ = view#append_column col in

  (* vertical paned *)
  let hb = GPack.paned `VERTICAL  ~border_width:3 ~packing:hp#add2 () in
  let _ = hb#misc#connect#realize
	  ~callback:(fun _ ->hb#set_position (window_height*6/10 ) ) in
  let _ = hb#set_position (window_height*9/10 ) in

  (* upper frame *)
  let fr1 = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:hb#add1 () in 
  let sw1 = GBin.scrolled_window
	    ~vpolicy:`AUTOMATIC 
	    ~hpolicy:`AUTOMATIC
	    ~packing:fr1#add () 
  in

  (* lower frame *)
  let fr2 = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:hb#add2 () in 
  let sw2 = GBin.scrolled_window 
	    ~vpolicy:`AUTOMATIC 
	    ~hpolicy:`AUTOMATIC
	    ~packing:(fr2#add) () 
  in

  (* status bar (at bottom) *)
  let status_bar = GMisc.statusbar ~packing:vbox#pack () in
  status_bar#misc#modify_font !statusbar_font ;
  let status_context = status_bar#new_context "messages" in
  ignore (status_context#push "Ready");
  ignore (status_context#push "Ready");
  display_info := (fun s -> 
		     status_context#pop ();
		     ignore (status_context#push s));
  flash_info := !display_info (* fun s -> status_context#flash ~delay:10 s *);

  (* lower text view: annotations *)
  let tv2 = GText.view ~packing:(sw2#add) () in
  let _ = tv2#misc#modify_font !lower_view_general_font in
  let _ = tv2#set_editable false in
  let tb2 = tv2#buffer in

  (* upper text view: source code *)
  let buf1 = GText.buffer () in 
  let tv1 = GText.view ~buffer:buf1 ~packing:(sw1#add) () in
  let _ = tv1#misc#modify_font !upper_view_general_font in
  let _ = tv1#set_editable false in

  (* hook for navigator *)
  let bgtag = buf1#create_tag [`BACKGROUND "light blue"] in
  Tree.show_info_ref := 
  (fun ((b,e),s) -> 
     buf1#remove_tag bgtag ~start:buf1#start_iter ~stop:buf1#end_iter;
     buf1#apply_tag bgtag 
     ~start:(buf1#get_iter (`OFFSET b)) 
     ~stop:(buf1#get_iter (`OFFSET e));
     buf1#place_cursor (buf1#get_iter (`OFFSET b));
     tv1#scroll_to_mark ~use_align:true ~yalign:0.25 `INSERT;
     tb2#set_text s
  );
  let _ = file_factory#add_item "Next file" ~key:GdkKeysyms._Page_Down
	  ~callback:(fun () -> view#selection#select_path 
			 (path_of_int (succ !current_source))) in
  let _ = file_factory#add_item "Previous file" ~key:GdkKeysyms._Page_Up
	  ~callback:(fun () -> 
		       view#selection#select_path 
			 (path_of_int (pred !current_source))) in
  w#add_accel_group accel_group;

  let _ = view#selection#connect#changed ~callback:
	    (fun () ->
	       List.iter 
		 (fun path ->
		    let pt = GtkTree.TreePath.to_string path in
		    let i = int_of_string pt in
		    current_source := i;
		    display_source buf1 tb2)
		 view#selection#get_selected_rows)
  in
  
  (* Remove default pango menu for textviews *)
  ignore (tv1#event#connect#button_press ~callback:
	    (fun ev -> GdkEvent.Button.button ev = 3));
  ignore (tv2#event#connect#button_press ~callback:
	    (fun ev -> GdkEvent.Button.button ev = 3));

  (* startup configuration *)
  buf1#place_cursor ~where:buf1#start_iter;
  let _ =  refresh_m#connect#activate 
	   ~callback:(fun () -> display_source buf1 tb2) 
  in
  w#show ();
  display_source buf1 tb2



let _ = 
  ignore (GtkMain.Main.init ());
  main () ;
  GMain.Main.main ()
