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

module Rangemap = Rgmap

(* -------------------------------------------------------------------------- *)
(* --- Text with Tagging Formatter                                        --- *)
(* -------------------------------------------------------------------------- *)

type tag =
  | TAG of GText.tag
  | MARK of int * string
  | LINK of int * string
  | PLAIN

let rec filter_tags tgs = function
  | [] -> tgs
  | TAG t :: style -> filter_tags (t::tgs) style
  | (MARK _ | LINK _ | PLAIN) :: style -> filter_tags tgs style

let split_tag tag =
  let rec lookup tag k n =
    if k < n then
      if tag.[k] = ':'
      then String.sub tag 0 k , String.sub tag (k+1) (n-k-1)
      else lookup tag (succ k) n
    else tag,""
  in lookup tag 0 (String.length tag)

let css_sheet = [
  "ul" , [ `UNDERLINE `SINGLE ];
  "st" , [ `STRIKETHROUGH true ];
  "bf" , [ `WEIGHT `BOLD ];
  "it" , [ `STYLE `ITALIC ];
  "red" , [ `FOREGROUND "red" ];
  "blue" , [ `FOREGROUND "blue" ];
  "green" , [ `FOREGROUND "darkgreen" ];
  "orange" , [ `FOREGROUND "orange" ];
  "hover" , [ `BACKGROUND "lightblue" ];
  "link" , [ `FOREGROUND "blue" ];
]

type 'a entry = int * int * 'a

let rec fire e = function [] -> () | f::fs -> f e ; fire e fs

class type ['a] marker =
  object
    method set_style : GText.tag_property list -> unit
    method set_hover : GText.tag_property list -> unit
    method connect : (GdkEvent.Button.t -> 'a entry -> unit) -> unit
    method on_click : ('a entry -> unit) -> unit
    method on_double_click : ('a entry -> unit) -> unit
    method on_right_click : ('a entry -> unit) -> unit
    method on_shift_click : ('a entry -> unit) -> unit
    method on_add : ('a entry -> unit) -> unit
    method wrap : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
    method mark : 'b. 'a -> (Format.formatter -> 'b -> unit) -> Format.formatter -> 'b -> unit
    method add : 'a entry -> unit
  end

type style = NoStyle | StyleSet | Style of GText.tag_property list

let configure tag = function
  | NoStyle -> NoStyle
  | StyleSet -> StyleSet
  | Style [] -> NoStyle
  | Style sty -> tag#set_properties sty ; StyleSet
      
(* -------------------------------------------------------------------------- *)
(* --- Monomorphic Marker                                                 --- *)
(* -------------------------------------------------------------------------- *)

type blind = {
  hover : GText.tag ;
  click : (bool -> GdkEvent.Button.t -> unit) ;
}

type registry = blind entry -> unit
type wrapper =
  (int -> int -> unit) ->
  (Format.formatter -> unit) -> Format.formatter -> unit

class ['a] poly_marker
    (buffer : GText.buffer)
    (registry : registry)
    (wrapper : wrapper)
  =
  let style = buffer#create_tag [] in
  let hover = buffer#create_tag [] in
  object(self)

    (*--- Style Configuration ---*)
    
    val mutable style_props = NoStyle
    val mutable hover_props = Style (List.assoc "hover" css_sheet)
    val mutable to_configure = true

    method set_style sty = assert to_configure ; style_props <- Style sty
    method set_hover sty = assert to_configure ; hover_props <- Style sty

    method private configure =
      if to_configure then
        begin
          style_props <- configure style style_props ;
          hover_props <- configure hover hover_props ;
          to_configure <- false ;
        end
        
    val mutable demon : (GdkEvent.Button.t -> 'a entry -> unit) list = []
    val mutable demon_click  : ('a entry -> unit) list = []
    val mutable demon_double : ('a entry -> unit) list = []
    val mutable demon_right  : ('a entry -> unit) list = []
    val mutable demon_shift  : ('a entry -> unit) list = []
    val mutable demon_added  : ('a entry -> unit) list = []

    (*--- Signal Connection ---*)

    method connect f = demon <- demon @ [f]
    method on_click d = demon_click <- demon_click @ [d]
    method on_double_click d = demon_double <- demon_double @ [d]
    method on_right_click d = demon_right <- demon_right @ [d]
    method on_shift_click d = demon_shift <- demon_shift @ [d]
    method on_add d = demon_added <- demon_added @ [d]

    (*--- Adding ---*)

    method add (e : 'a entry) =
      begin
        self#configure ;
        let (p,q,_) = e in
        if style_props = StyleSet then
          begin
            let start = buffer#get_iter (`OFFSET p) in
            let stop = buffer#get_iter (`OFFSET q) in
            buffer#apply_tag style ~start ~stop
          end ;
        let click double evt =
          List.iter (fun f -> f evt e) demon ;
          if double then fire e demon_double else
            let state = GdkEvent.Button.state evt in
            if Gdk.Convert.test_modifier `BUTTON3 state
            then fire e demon_right else
            if Gdk.Convert.test_modifier `BUTTON1 state
            then if Gdk.Convert.test_modifier `SHIFT state
              then fire e demon_shift
              else fire e demon_click
        in
        registry (p,q,{ hover ; click }) ;
        ignore (fire e demon_added) ;
      end
      
    method wrap pp (fmt:Format.formatter) (w:'a) : unit =
      self#mark w pp fmt w

    method mark :
      'b. 'a ->
      (Format.formatter -> 'b -> unit) -> Format.formatter -> 'b -> unit =
      fun e pp fmt w ->
        wrapper (fun p q -> self#add (p,q,e)) (fun fmt -> pp fmt w) fmt

  end

(* -------------------------------------------------------------------------- *)
(* --- Text Widget                                                        --- *)
(* -------------------------------------------------------------------------- *)

class text ?(autoscroll=false) ?(width=80) ?(indent=60) () =
  let buffer = GText.buffer () in
  let react = buffer#create_tag [] in
  let view = GText.view ~buffer
      ~editable:false ~cursor_visible:false
      ~justification:`LEFT
      ~wrap_mode:`NONE
      ~accepts_tab:false
      ~show:true () in
  let scroll = GBin.scrolled_window () in
  object(self)

    val text = Buffer.create 80
    val css = Hashtbl.create 32
    val marks : (string,int -> int -> unit) Hashtbl.t = Hashtbl.create 32
    val mutable links : string marker option = None
    val mutable width = width
    val mutable hrule = ""
    val mutable ruled = false
    val mutable indent = indent
    val mutable hid = 0
    val mutable autoscroll = autoscroll
    val mutable style = []
    val mutable fmtref = None
    val mutable reactive = false
    val mutable index : blind Rangemap.t = Rangemap.empty
    val mutable hovered = None
    val mutable double = false
    
    (* -------------------------------------------------------------------------- *)
    (* --- Text Initializer                                                   --- *)
    (* -------------------------------------------------------------------------- *)

    initializer
      begin
        (* Ignore default pango contextual menu (copy/cut/paste etc...), as this
           widget is read-only *)
        ignore (view#event#connect#button_press
                  ~callback:(fun ev -> GdkEvent.Button.button ev = 3));
        scroll#add view#coerce
      end

    (* -------------------------------------------------------------------------- *)
    (* --- Text Formatter                                                     --- *)
    (* -------------------------------------------------------------------------- *)

    method private flush () =
      if Buffer.length text > 0 then
        begin
          let s = Wutil.to_utf8 (Buffer.contents text) in
          Buffer.clear text ;
          let tags = filter_tags [] style in
          let iter = buffer#end_iter in
          buffer#insert ~tags ~iter s ;
          if reactive then
            let start,stop = buffer#bounds in
            buffer#apply_tag react ~start ~stop ;
        end

    method private open_tag name =
      self#flush () ; style <- self#tag name :: style ; ""

    method private close_tag _name =
      self#flush () ; match style with
      | [] -> ""
      | MARK(p,mrk) :: sty -> style <- sty ; self#mark p mrk ; ""
      | LINK(p,lnk) :: sty -> style <- sty ; self#link p lnk ; ""
      | (TAG _ | PLAIN) :: sty -> style <- sty ; ""

    method fmt = match fmtref with Some fmt -> fmt | None ->
      let output_string s a b = if b > 0 then Buffer.add_substring text s a b in
      let fmt = Format.make_formatter output_string self#flush in
      let tagger = Format.pp_get_formatter_tag_functions fmt () in
      Format.pp_set_formatter_tag_functions fmt
        { tagger with
          Format.mark_open_tag = self#open_tag ;
          Format.mark_close_tag = self#close_tag ;
        } ;
      Format.pp_set_print_tags fmt false ;
      Format.pp_set_mark_tags fmt true ;
      Format.pp_set_margin fmt width ;
      Format.pp_set_max_indent fmt indent ;
      fmtref <- Some fmt ; fmt

    method offset = self#flush () ; buffer#end_iter#offset

    method set_width w =
      width <- w ; hrule <- "" ;
      match fmtref with None -> () | Some fmt ->
        Format.pp_set_margin fmt w

    method set_indent p =
      indent <- p ; match fmtref with None -> () | Some fmt ->
        Format.pp_set_max_indent fmt p

    (* -------------------------------------------------------------------------- *)
    (* --- Link & Marking                                                     --- *)
    (* -------------------------------------------------------------------------- *)

    method links =
      match links with
      | Some marker -> marker
      | None ->
          let marker = self#marker in
          marker#set_style (List.assoc "link" css_sheet) ;
          marker#set_hover (List.assoc "hover" css_sheet) ;
          links <- Some marker ; marker

    method private link p name =
      let q = buffer#end_iter#offset in
      self#links#add (p,q,name)

    method private mark p name =
      let q = buffer#end_iter#offset in
      List.iter (fun f -> f p q) (Hashtbl.find_all marks name)

    method on_link f = self#links#on_click (fun (_,_,lnk) -> f lnk)

    method wrap f pp fmt =
      begin
        let sid = hid <- succ hid ; Printf.sprintf ">%X" hid in
        Hashtbl.add marks sid (fun p q -> Hashtbl.remove marks sid ; f p q) ;
        Format.pp_open_tag fmt sid ;
        let () = pp fmt in
        Format.pp_close_tag fmt () ;
      end

    (* -------------------------------------------------------------------------- *)
    (* --- Tag Marking                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method private css_style name props =
      let sty = TAG(buffer#create_tag ~name props) in
      Hashtbl.replace css name sty ; sty
    
    method private tag name =
      if Hashtbl.mem marks name then MARK(buffer#end_iter#offset,name)
      else
        try Hashtbl.find css name
        with Not_found ->
        try self#css_style name (List.assoc name css_sheet)
        with Not_found ->
          begin
            match split_tag name with
            | "fg",color -> self#css_style name [ `FOREGROUND color ]
            | "bg",color -> self#css_style name [ `BACKGROUND color ]
            | "link",name -> LINK(buffer#end_iter#offset,name)
            | _ -> PLAIN
          end

    method set_css sheet =
      List.iter (fun (name,tags) -> ignore (self#css_style name tags)) sheet

    method set_style name p q =
      match self#tag name with
      | PLAIN | LINK _ | MARK _ -> ()
      | TAG tag ->
          let start = buffer#get_iter (`OFFSET p) in
          let stop = buffer#get_iter (`OFFSET q) in
          buffer#apply_tag tag ~start ~stop

    method remove_style name p q =
      match Hashtbl.find css name with
      | PLAIN | LINK _ | MARK _ -> ()
      | TAG tag ->
          let start = buffer#get_iter (`OFFSET p) in
          let stop = buffer#get_iter (`OFFSET q) in
          buffer#remove_tag tag ~start ~stop

    method remove_all names =
      let start,stop = buffer#bounds in
      List.iter
        (fun name ->
           match Hashtbl.find css name with
           | TAG tag -> buffer#remove_tag tag ~start ~stop
           | PLAIN | LINK _ | MARK _ -> ())
        names

    (* -------------------------------------------------------------------------- *)
    (* --- Hover & Mark Dispatcher                                            --- *)
    (* -------------------------------------------------------------------------- *)

    method private set_reactive =
      if not reactive then
        let callback ~origin:_ evt iter =
          (* return false to propagate events *)
          match GdkEvent.get_type evt with
          | `BUTTON_PRESS -> double <- false ; false
          | `TWO_BUTTON_PRESS -> double <- true ; false
          | `BUTTON_RELEASE ->
              begin
                match hovered with
                | None -> ()
                | Some (_,_,blind) -> 
                    blind.click double (GdkEvent.Button.cast evt)
              end ; false
          | `MOTION_NOTIFY ->
              let offset = GtkText.Iter.get_offset iter in
              let entry =
                try Some(Rangemap.find offset offset index)
                with Not_found -> None
              in self#hover entry ; false
          | _ -> false
        in ( ignore (react#connect#event ~callback) ; reactive <- true )

    method private hover h =
      match hovered , h with
      | Some e0 , Some e when e == e0 -> ()
      | None , None -> ()
      | _ ->
          begin
            (match hovered with None -> () | Some (_,_,{hover}) ->
              let start,stop = buffer#bounds in
              buffer#remove_tag hover ~start ~stop) ;
            (match h with None -> () | Some (a,b,{hover}) ->
              let start = buffer#get_iter (`OFFSET a) in
              let stop = buffer#get_iter (`OFFSET b) in
              self#hover None ;
              buffer#apply_tag hover ~start ~stop) ;
            hovered <- h
          end

    method private register e =
      index <- Rangemap.add e index
    
    (* -------------------------------------------------------------------------- *)
    (* --- User API                                                           --- *)
    (* -------------------------------------------------------------------------- *)

    method set_autoscroll s = autoscroll <- s

    method printf : 'a. ?scroll:bool ->
      ('a,Format.formatter,unit) format -> 'a =
      fun ?(scroll=autoscroll) text ->
        (* Save current number of lines in the buffer *)
        let line = view#buffer#line_count in
        let finally fmt =
          Format.pp_print_flush fmt () ;
          Hashtbl.clear marks ;
          hid <- 0 ;
          ruled <- false ;
          if scroll then
            (* scrolling must be performed asynchronously using Gtk_helper.later,
               otherwise it will not take into account the newly added text. *)
            Wutil.later (self#scroll ~line)
        in
        Format.kfprintf finally self#fmt text

    method hrule =
      if not ruled then
        begin
          if String.length hrule = 0 then
            hrule <- String.make width '-' ;
          Format.pp_print_string self#fmt hrule ;
          Format.pp_print_newline self#fmt () ;
          ruled <- true ;
        end
    
    method lines = view#buffer#line_count

    method scroll ?line () =
      let buf = view#buffer in
      let line = match line with Some l -> l | None -> buf#line_count in
      let iter = buf#get_iter_at_char ~line 0 in
      ignore (view#scroll_to_iter ~use_align:true ~yalign:0.0 iter)

    method select ?(scroll=false) (p:int) (q:int) =
      let buffer = view#buffer in
      let start = buffer#get_iter (`OFFSET p) in
      let stop = buffer#get_iter (`OFFSET q) in
      buffer#select_range start stop ;
      ignore (view#scroll_to_iter ~use_align:scroll ~yalign:0.3 start)

    method clear =
      begin
        Format.pp_print_flush self#fmt () ;
        Hashtbl.clear marks ; hid <- 0 ;
        buffer#delete ~start:buffer#start_iter ~stop:buffer#end_iter ;
        index <- Rangemap.empty ;
        self#hover None ;
      end

    method coerce = scroll#coerce
    method widget = (self :> Widget.t)
    method set_enabled = Wutil.set_enabled scroll
    method set_visible = Wutil.set_visible scroll

    method set_font = Wutil.set_font view
    method set_monospace = Wutil.set_monospace view

    method marker : 'a. 'a marker =
      let h = new poly_marker buffer self#register self#wrap in
      self#set_reactive ;
      (h :> _ marker)

    method get_view = view

  end
