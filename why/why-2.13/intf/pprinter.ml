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

open Options
open Misc
open Vcg
open Logic
open Cc
open Format
open Colors
open Tagsplit
open Tags

let obligs = Hashtbl.create 5501
let pprinter = Hashtbl.create 5501
let files = Hashtbl.create 10
let last_fct = ref ""
let last_line = ref 0
let last_file = ref ""
let pwd = Sys.getcwd ()
let source = ref ""
let tv_source = ref (GText.view ())

let active = ref false
let desactivate () = active := false
let activate () = active := true
let swap_active () = active := not !active
let is_active () = !active

let set_tvsource tvs = tv_source := tvs

let reset_last_file () = 
  last_file := ""

let print_loc = function 
  | None -> "\"nowhere\""
  | Some {file=f; line=l; sp=s; ep=e} ->
      let ff = if Filename.is_relative f then Filename.concat pwd f else f in
      ("file \""^ff^"\", line "^l^", characters "^s^" - "^e)

let is_cfile f = 
  Filename.check_suffix f ".c" or Filename.check_suffix f ".h"
  (* otherwise it's .why *)

let read_file = function 
  | None -> ()
  | Some {file=f} ->
      try
	let in_channel = open_in f in
	begin 
	  try
            let lexbuf = Lexing.from_channel in_channel 
	    and hilight = 
	      if is_cfile f then Hilight.token else Whyhilight.scan 
	    in
            while true do hilight !tv_source#buffer lexbuf done
	  with Hilight.Eof | Whyhilight.Eof -> ()
	end;
      with Sys_error s -> 
	begin
	  print_endline ("     [...] Sys_error : "^s); flush stdout;
	  !tv_source#buffer#set_text "" 
	end

let hypo = (* Model : HW_11 *)
  let r_hyp = Str.regexp "\\HW_\\([0-9]+\\)" in
  fun s ->
    if Str.string_match r_hyp s 0 then
      (Str.matched_group 1 s)
    else
      s

let hypothesis s = 
  "H"^(hypo s)

let grab_infos = 
  let r_loc = Str.regexp "File \"\\(.+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" in
  fun s -> 
    if Str.string_match r_loc s 0 then 
      let source = Filename.concat pwd (Str.matched_group 1 s) in
      Some({file=source;
            line=(Str.matched_group 2 s);
            sp=(Str.matched_group 3 s);
            ep=(Str.matched_group 4 s)})
    else None
      
let is_localised = function 
  | None -> false
  | Some _ -> true

let move_to_line line = 
  if line <= !tv_source#buffer#line_count && line <> 0 then begin
    let it = !tv_source#buffer#get_iter (`LINE line) in 
    let mark = `MARK (!tv_source#buffer#create_mark it) 
    (* and y = if !tv_source#buffer#line_count < 20 then 0.23 else 0.1  *)
    in
    !tv_source#scroll_to_mark ~use_align:true ~yalign:0.5 mark;
    if debug then
      begin 
	print_endline ("     [...] Moving to line n "^(string_of_int line)); 
	flush stdout 
      end
  end

let color_tag_table = Hashtbl.create 17

let color_loc file (tv:GText.view) l b e =
  let buf = tv#buffer in
  let orange_bg = 
    try
      Hashtbl.find color_tag_table file
    with Not_found ->
      let t = buf#create_tag ~name:"orange_bg" [`BACKGROUND "orange"] in
      Hashtbl.add color_tag_table file t;
      t
  in
  buf#remove_tag orange_bg ~start:buf#start_iter ~stop:buf#end_iter;
  let top = buf#start_iter in
  let start = top#forward_lines (l-1) in
  let start = start#forward_chars b in
  let stop = start#forward_chars (e-b) in
  buf#apply_tag ~start ~stop orange_bg
  

let banner () =
"

Welcome to GWhy (the Graphical VC viewer for the Why platform)
This is Why version " ^ Version.version ^ 
", compiled on " ^ Version.date ^ "
Copyright (c) 2002-2007 ProVal team, INRIA
This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)"

let move_to_source = function
  | None -> 
      last_file := "";
      begin
	try
	  !tv_source#set_buffer (Hashtbl.find files "")
	with Not_found ->
	  !tv_source#set_buffer (GText.buffer ());
	  let b = !tv_source#buffer in
	  b#set_text (banner());
	  begin
	    try
	      let why_logo_image = Options.lib_file "why-logo-1.png" in
	      let why_logo = GdkPixbuf.from_file why_logo_image in
	      b#insert_pixbuf ~iter:b#start_iter ~pixbuf:why_logo 
	    with _ -> 
	      b#insert ~iter:b#start_iter "(Why logo: image not found)"	      
	  end;
	  b#insert ~iter:b#start_iter "\n\t";
	  Hashtbl.add files "" b
      end
  | Some loc ->
      let line = int_of_string loc.line
      and file = loc.file in
      last_line := line;
      if !last_file = file then 
	move_to_line line
      else begin 
	last_file := file;
	if (Hashtbl.mem files file) && (not (Colors.has_changed ())) then 
	  !tv_source#set_buffer (Hashtbl.find files file)
	else begin
	  !tv_source#set_buffer (GText.buffer ());
	  read_file (Some loc);
	  Hashtbl.add files file !tv_source#buffer;
	  Hashtbl.remove color_tag_table file;
	end;
	move_to_line line
      end

 
let move_to_loc loc =
  move_to_source (Some loc);
  color_loc loc.file !tv_source (int_of_string loc.line) 
    (int_of_string loc.sp) (int_of_string loc.ep)

let rec intros ctx = function 
  | Forall (true, id, n, t, _, p) ->
      let id' = Ident.next_away id (predicate_vars p) in
      let p' = subst_in_predicate (subst_onev n id') p in
      let ctx', concl' = intros (Svar (id', t) :: ctx) p' in
      ctx', concl'
  | Pimplies (true, a, b) -> 
      let h = fresh_hyp () in 
      let ctx', concl' = intros (Spred (h, a) :: ctx) b in
      ctx', concl'
  (* TODO: Pnamed ? *)
  | c -> 
      ctx, c

let create_tag (tbuf:GText.buffer) t loc = 
  if not (Hashtbl.mem gtktags t) then begin
    let (fc, bc) = get_color "lpredicate" in
    let new_tag = tbuf#create_tag [`BACKGROUND bc; `FOREGROUND fc] in
    ignore(
      new_tag#connect#event ~callback:
	(fun ~origin ev it -> 
	   if GdkEvent.get_type ev = `BUTTON_PRESS then 
	     move_to_source (Some loc)
	   else if GdkEvent.get_type ev = `MOTION_NOTIFY then begin
	     Tags.refresh_last_colored [new_tag];
	     new_tag#set_properties 
	       [`BACKGROUND (get_bc "pr_hilight"); 
		`FOREGROUND (get_fc "pr_hilight")]
	   end;
	   false));
    add_gtktag t new_tag
  end

let create_all_tags (tbuf:GText.buffer) = 
  Hashtbl.iter (create_tag tbuf) Tags.loctags

let print_oblig fmt (ctx,concl) = 
  let print_pure_type, print_predicate = 
    if is_active () 
    then Astprinter.print_pure_type, Astpprinter.print_predicate
    else Astprinter.print_pure_type, Astnprinter.print_predicate
  in 
  let ctx, concl = intros ctx concl in
  let rec print_list print = function
    | [] -> ()
    | p::r -> print p; fprintf fmt "@\n"; print_list print r 
  and print_name fmt id =
    let hypo_nb = hypo (Ident.string id) in
    fprintf fmt "%s" (hypothesis hypo_nb)
  and print_hyp fmt = function
    | Svar (id, t) ->
	fprintf fmt "@[@{<var>%a:@}@ @{<cc_type>%a@}@]" 
	  Ident.print id print_pure_type t
    | Spred (id, p) ->
	fprintf fmt "@[@{<hypothesis>%a:@} @{<predicate>%a@}@]" 
	  print_name id print_predicate p
  in
  print_list (print_hyp fmt) ctx

let is_buffer_saved = 
  Hashtbl.mem obligs

let save_buffer s (tbuf:GText.buffer) pprint = 
  Hashtbl.replace obligs s tbuf;
  Hashtbl.replace pprinter s pprint

let get_buffer = 
  Hashtbl.find obligs

let color_lines tags =
  let buf = !tv_source#buffer in
  let top = buf#start_iter in
  let orange_bg = buf#create_tag ~name:"orange_bg" [`BACKGROUND "orange"] in
  (*
  buf#remove_tag_by_name ~start:buf#start_iter ~stop:buf#end_iter "orange_bg";
  *)
  let color_one t = 
    let loc = Hashtbl.find loctags t in
    let l = int_of_string loc.line in
    let start = top#forward_lines l in
    let stop = start#forward_line in
    buf#apply_tag ~start ~stop orange_bg
  in
  List.iter color_one tags
  
let print_all (tbuf:GText.buffer) s p = 
  insert_text tbuf "title" (s^"\n\n");
  (* 1. we print the text in a string, which fills the table loctags *)
  let fmt = Format.str_formatter in
  pp_set_tags fmt true;
  let str = 
    fprintf fmt "@[%a@]" print_oblig p;
    flush_str_formatter ()
  in
  (* 2. then we create the GTK tags and map them to the tag names *)
  create_all_tags tbuf;
  (* 3. then we fill the GTK text buffer using Tagsplit.split *)
  let _utags = split tbuf (Lexing.from_string str) in
  let (_,concl) = p in
  let mytag = 
    tbuf#create_tag
      [`UNDERLINE `SINGLE;
       `FOREGROUND (get_fc "separator"); 
       `BACKGROUND (get_bc "separator")] 
  in
  tbuf#insert ~tags:[mytag] ~iter:tbuf#end_iter 
    "              \n\n";
  let conclusion = 
    let print_predicate = 
      if is_active () 
      then Astpprinter.print_predicate
      else Astnprinter.print_predicate
    in 
    fprintf fmt "@[@{<conclusion>%a@}@]" print_predicate concl;
    create_all_tags tbuf;
    flush_str_formatter () 
  in
  let _utags' = split tbuf (Lexing.from_string conclusion) in
  (*color_lines (utags @ utags');*)
  ()

let unchanged s pprint = 
  (not (Colors.has_changed ())) &&
  (is_buffer_saved s) 
  && (try 
	(Hashtbl.find pprinter s) = pprint
      with Not_found -> pprint)

let is_ident_char s = String.length s = 1 && match s.[0] with
  | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> true
  | _ -> false

let show_definition (tv:GText.view) (tv_s:GText.view) = 
  let buf = tv#buffer in
  let find_backward (it:GText.iter) = 
    let rec find start stop = 
      if start = buf#start_iter then
	start 
      else
	let c = buf#get_text ~start ~stop () in
	if is_ident_char c then find (start#backward_char) start else stop
    in 
    find (it#backward_char) it
  in
  let find_forward (it:GText.iter) = 
    let rec find start stop = 
      if stop = buf#end_iter then
	stop
      else
	let c = buf#get_text ~start ~stop () in
	if is_ident_char c then find stop (stop#forward_char) else start
    in 
    find it (it#forward_char)
  in
  let buf = tv#buffer in
  let win = match tv#get_window `WIDGET with
    | None -> assert false
    | Some w -> w
  in
  let x,y = Gdk.Window.get_pointer_location win in
  let b_x,b_y = tv#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
  let it = tv#get_iter_at_location ~x:b_x ~y:b_y in
  let start = if (it = buf#start_iter) then it else find_backward it in
  let stop = if (it = buf#end_iter) then it else find_forward it in
  let text = buf#get_text ~start ~stop () in
  if start <> stop && text <> "" then begin
    try 
      let (f,l,b,e) = Loc.ident text in
      let loc = {file=f; 
		 line=(string_of_int l); 
		 sp=(string_of_int b); 
		 ep=(string_of_int e)} 
      in 
      (*
       * print_endline (text ^ "  " ^ (print_loc (Some(loc)))); 
       * flush stdout;
       *)
      move_to_source (Some loc)
    with Not_found -> ()
  end
    
let text_of_obligation (tv:GText.view) (o,expl,s,p) = 
  let p = p.Env.scheme_type in
  last_fct := s;
  if (unchanged s (is_active ())) then 
    tv#set_buffer (get_buffer s)
  else begin
    let tbuf = GText.buffer () in
    tv#set_buffer tbuf;
    let _ = 
      tv#event#connect#button_release 
	~callback:(fun ev -> 
		     if GdkEvent.Button.button ev = 3 then
		       show_definition tv !tv_source; 
		     false)
    in
    print_all tbuf s p;
    save_buffer s tbuf (is_active ());
  end
