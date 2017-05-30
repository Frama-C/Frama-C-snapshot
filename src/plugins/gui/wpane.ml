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

open Widget

(* -------------------------------------------------------------------------- *)
(* ---  Forms                                                             --- *)
(* -------------------------------------------------------------------------- *)

type field = [ `Compact | `Field | `Panel ]

let fexpand = function `Compact -> `NONE | `Field -> `X | `Panel -> `BOTH

class form () =
  let box = GPack.table ~columns:2 ~col_spacings:16 ~homogeneous:false () in
  object(self)
    val mutable line  = 0
    val mutable left  = false (* left column fed on current line *)
    val mutable right = false (* right column fed on current line *)
    val mutable xpadding = 0  (* set with sections *)

    inherit Wutil.gobj_widget box

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
      box#attach ~left:0 ~right:2 ~top:line ~ypadding:12 ~expand:`Y w#coerce

    method add_section label =
      self#occupy_both ;
      let w = GMisc.label ~text:label ~xalign:0.0 ~yalign:1.0 () in
      Wutil.set_bold_font w ;
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
      Wutil.on label self#add_label ;
      self#occupy_right ;
      box#attach ~left:1 ~top:line ~expand:(fexpand field) w

    method add_row ?(field:field=`Field) ?(xpadding=xpadding) ?ypadding w =
      self#occupy_both ;
      box#attach ~left:0 ~right:2 ~top:line
        ~xpadding ?ypadding ~expand:(fexpand field) w

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

let no_entry = fun _ -> assert false

class ['a] warray ?(dir=`VERTICAL) ?(entry = no_entry) () =
  let box = GPack.box dir ~homogeneous:false () in
  object(self)
    inherit Wutil.gobj_widget box

    val mutable rows : ('a * entry) list = []
    val mutable creator : ('a -> entry) = entry

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

    method set_entry f = creator <- f

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
    inherit ['a] Wutil.selector default as select
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
        Wutil.on tabs (fun p -> view#set_show_tabs true ; view#set_tab_pos p) ;
      end
    method coerce = view#coerce
    method widget = (self :> Widget.t)
    method! set_enabled = Wutil.set_enabled view
    method set_visible = Wutil.set_visible view
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

    inherit ['a] Wutil.signal

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
