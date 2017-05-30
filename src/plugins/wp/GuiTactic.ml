(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Composer                                                           --- *)
(* -------------------------------------------------------------------------- *)

class type composer =
  object
    method title : string
    method descr : string
    method target : selection
    method ranged : bool
    method is_valid : selection -> bool
    method get_value : selection
    method set_value : selection -> unit
  end

class type browser =
  object
    method title : string
    method descr : string
    method target : selection
    method search : (unit named -> unit) -> int -> unit
    method choose : string option -> unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Field Widget                                                       --- *)
(* -------------------------------------------------------------------------- *)

class virtual wfield =
  object(self)
    method wfield = (self :> wfield)
    val mutable target = Empty
    method target = target
    method select tgt = target <- tgt
    method compose_with (_ : composer -> unit) = ()
    method browse_with (_ : browser -> unit) = ()
    method clear = self#reset ; target <- Empty
    method virtual reset : unit
    method virtual connect : (unit -> unit) -> unit
    method virtual update :
      ?enabled:bool -> ?title:string -> ?tooltip:string ->
      ?range:bool -> ?vmin:int -> ?vmax:int ->
      ?filter:(Lang.F.term -> bool) -> string -> unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Checkbox Widget                                                    --- *)
(* -------------------------------------------------------------------------- *)

class checkbox
    (tac : Tactical.t) (form : Wpane.form) (field : bool field) =
  let s = Tactical.signature field in
  let button = new Widget.checkbox ~label:s.title ~tooltip:s.descr () in
  object
    inherit wfield
    initializer
      begin
        form#add_field ~field:`Compact button#coerce ;
        button#connect (tac#set_field field) ;
      end
    method reset = button#set s.value
    method connect = button#on_event
    method update ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter id =
      if id = Tactical.ident field then
        begin
          Wutil.on enabled button#set_visible ;
          Wutil.on tooltip button#set_tooltip ;
          ignore title ;
          ignore filter ;
          ignore vmin ;
          ignore vmax ;
          ignore range ;
        end
  end

(* -------------------------------------------------------------------------- *)
(* --- Spinner Widget                                                     --- *)
(* -------------------------------------------------------------------------- *)

class spinner
    (tac : Tactical.t) (form : Wpane.form) (field : int field)
    (range : int range) =
  let s = Tactical.signature field in
  let spin = new Widget.spinner
    ?min:range.vmin ?max:range.vmax
    ~step:range.vstep
    ~value:s.value
    ~tooltip:s.descr () in
  object
    inherit wfield
    initializer
      begin
        form#add_field ~label:s.title ~field:`Compact spin#coerce ;
        spin#connect (tac#set_field field) ;
      end
    method reset = spin#set s.value
    method connect = spin#on_event
    method update ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter id =
      if id = Tactical.ident field then
        begin
          Wutil.on enabled spin#set_visible ;
          Wutil.on tooltip spin#set_tooltip ;
          Wutil.on vmin spin#set_min ;
          Wutil.on vmax spin#set_max ;
          ignore title ;
          ignore range ;
          ignore filter ;
        end
  end

(* -------------------------------------------------------------------------- *)
(* --- Composer Widget                                                    --- *)
(* -------------------------------------------------------------------------- *)

class mkcomposer
    (tac : Tactical.t) (form : Wpane.form) (field : selection field)
    (accept : Lang.F.term -> bool)
    (pp : Format.formatter -> Tactical.selection -> unit) =
  let s = Tactical.signature field in
  let head = new Widget.label ~style:`Label ~align:`Left () in
  let edit = new Widget.button ~icon:`EDIT ~tooltip:s.descr () in
  let hbox = Wbox.(hbox [ h head ; w ~padding:8 edit ]) in
  object(self)
    inherit wfield
    initializer form#add_row hbox#coerce
    val mutable wtitle = s.title
    val mutable wdescr = s.descr
    val mutable wvalid = accept
    val mutable ranged = false
    val mutable demon = []
    method private updated =
      match tac#get_field field with
      | Empty ->
          Pretty_utils.ksfprintf head#set_text "%s: -" wtitle
      | value ->
          let text =
            Pretty_utils.sfprintf "@[<hov 2>%s: %a@]" wtitle pp value in
          let msg =
            if String.length text <= 20 then text else
              String.sub text 0 17 ^ "..." in
          head#set_text msg

    (* --- Composer API ---- *)
    method composer = (self :> composer)
    method title = wtitle
    method descr = wdescr
    method ranged = ranged
    method is_valid = function
      | Empty -> false
      | Compose(Range(a,b)) -> ranged && (a <= b)
      | _ as s ->
          try wvalid (Tactical.selected s)
          with _ -> false
    method get_value = tac#get_field field
    method set_value v =
      tac#set_field field v ;
      self#updated ;
      List.iter (fun f -> f ()) demon
    method! compose_with f = edit#connect (fun () -> f self#composer)
    (* --- Wfield API ---- *)
    method reset =
      wtitle <- s.title ;
      wdescr <- s.descr ;
      wvalid <- accept ;
      tac#set_field field Tactical.Empty ;
      self#updated
    method connect f = demon <- demon @ [f]
    method update ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter id =
      if id = Tactical.ident field then
        begin
          Wutil.on enabled hbox#set_visible ;
          Wutil.on title (fun s -> wtitle <- s) ;
          Wutil.on tooltip (fun d -> wdescr <- d) ;
          Wutil.on filter (fun f -> wvalid <- f) ;
          Wutil.on range (fun r -> ranged <- r) ;
          ignore vmin ; ignore vmax ;
        end
  end

(* -------------------------------------------------------------------------- *)
(* --- Search Widget                                                      --- *)
(* -------------------------------------------------------------------------- *)

exception StopLookup

class ['a] search
    (tac : Tactical.t) (form : Wpane.form) (field : 'a named option field)
    (browser : 'a Tactical.browser)
  =
  let s = Tactical.signature field in
  let head = new Widget.label ~style:`Label ~align:`Left () in
  let edit = new Widget.button ~icon:`FIND ~tooltip:s.descr () in
  let hbox = Wbox.(hbox [ h head ; w ~padding:8 edit ]) in
  object(self)
    inherit wfield
    initializer form#add_row hbox#coerce ;
    val mutable wtitle = s.title
    val mutable wdescr = s.descr
    val items : (string,'a named) Hashtbl.t = Hashtbl.create 7
    val mutable demon = []
    method private updated =
      match tac#get_field field with
      | None ->
          Pretty_utils.ksfprintf head#set_text "%s: -" wtitle
      | Some item ->
          begin
            let text = item.title in
            let msg =
              if String.length text <= 20 then text else
                String.sub text 0 17 ^ "..." in
            head#set_text msg ;
            head#set_tooltip item.descr ;
          end

    (* --- Browser API --- *)
    method browser = (self :> browser)
    method choose item =
      let value = match item with
        | Some id ->
            (try Some(Hashtbl.find items id)
             with Not_found -> None)
        | None -> None in
      tac#set_field field value ;
      self#updated ;
      List.iter (fun f -> f ()) demon
    method search f n =
      let count = ref n in
      Hashtbl.clear items ;
      try
        browser
          (fun item ->
             Hashtbl.add items item.vid item ;
             f { item with value = () } ;
             decr count ;
             if !count <= 0 then raise StopLookup ;
          ) target
      with StopLookup -> ()

    method! browse_with f = edit#connect (fun () -> f self#browser)

    (* --- Wfield API --- *)
    method title = wtitle
    method descr = wdescr
    method reset =
      wtitle <- s.title ;
      wdescr <- s.descr ;
      tac#set_field field None ;
      Hashtbl.clear items ;
      self#updated ;
    method connect f = demon <- demon @ [f]
    method update ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter id =
      if id = Tactical.ident field then
        begin
          Wutil.on enabled hbox#set_visible ;
          Wutil.on title (fun s -> wtitle <- s) ;
          Wutil.on tooltip (fun d -> wdescr <- d) ;
          ignore filter ; ignore range ;
          ignore vmin ; ignore vmax ;
        end
  end

(* -------------------------------------------------------------------------- *)
(* --- Selector Widget                                                    --- *)
(* -------------------------------------------------------------------------- *)

class ['a] selector
    (tac : Tactical.t) (form : Wpane.form) (field : 'a field)
    (options : 'a Tactical.named list) (equal : 'a -> 'a -> bool) =
  let s = Tactical.signature field in
  let lookup a =
    try List.find (fun v -> equal v.value a) options
    with Not_found ->
      { title = "" ; descr = "(unknown item)" ; vid = "unknown" ; value=a }
  in
  let default = lookup s.value in
  let render item = item.title in
  let combo = new Widget.menu ~default ~render ~items:options () in
  object
    inherit wfield
    initializer
      begin
        form#add_field ~label:s.title ~field:`Compact combo#coerce ;
        combo#connect (fun opt -> tac#set_field field opt.value) ;
      end
    method reset = combo#set default
    method connect = combo#on_event
    method update ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter id =
      if id = Tactical.ident field then
        begin
          Wutil.on enabled combo#widget#set_visible ;
          Wutil.on tooltip combo#set_tooltip ;
          ignore filter ;
          ignore title ;
          ignore vmin ;
          ignore vmax ;
          ignore range ;
        end
  end

(* -------------------------------------------------------------------------- *)
(* --- Dispatcher                                                         --- *)
(* -------------------------------------------------------------------------- *)

let wfield tac form pp = function
  | Checkbox fd -> (new checkbox tac form fd)#wfield
  | Spinner(fd,r) -> (new spinner tac form fd r)#wfield
  | Composer(fd,f) -> (new mkcomposer tac form fd f pp)#wfield
  | Selector(fd,opt,eq) -> (new selector tac form fd opt eq)#wfield
  | Search(fd,browser,_) -> (new search tac form fd browser)#wfield

(* -------------------------------------------------------------------------- *)
(* --- Tactic Widget                                                      --- *)
(* -------------------------------------------------------------------------- *)

type edited = {
  target : selection ;
  browser : (browser -> unit) ;
  composer : (composer -> unit) ;
  process : (tactical -> selection -> process -> unit) ;
}

class tactic
    (tac : tactical)
    (pp : Format.formatter -> Tactical.selection -> unit) =
  let form = new Wpane.form () in
  let descr = new Widget.label ~style:`Descr ~width:24 ~align:`Left () in
  object(self)

    val mutable title = tac#title
    val mutable wfields : wfield list = []
    val mutable edited : edited option = None
    val mutable hints = Fmap.create ()
    val mutable error = false

    inherit Wpalette.tool ~content:form#widget () as dongle

    initializer
      begin
        form#add_row ~xpadding:4 ~ypadding:2 ~field:`Compact descr#coerce ;
        self#set_action ~tooltip:"Apply Tactic" ~icon:`MEDIA_PLAY () ;
        wfields <- List.map (wfield tac form pp) tac#params ;
        List.iter (fun fd -> fd#connect self#updated) wfields ;
        List.iter (fun fd -> fd#compose_with self#compose) wfields ;
        List.iter (fun fd -> fd#browse_with self#browse) wfields ;
        self#set_tooltip
          (if wfields = [] then "Tactic Details" else "Configure Tactic") ;
      end

    (* -------------------------------------------------------------------------- *)
    (* --- Panel API                                                          --- *)
    (* -------------------------------------------------------------------------- *)

    method! set_label =
      fun msg -> title <- msg ; dongle#set_label msg
    method set_title : 'a. 'a formatter =
      fun msg -> Pretty_utils.ksfprintf self#set_label msg
    method set_descr : 'a. 'a formatter =
      fun msg -> Pretty_utils.ksfprintf descr#set_text msg

    (* -------------------------------------------------------------------------- *)
    (* --- Feedback API                                                       --- *)
    (* -------------------------------------------------------------------------- *)

    method interactive = self#is_active
    method get_title = title
    method has_error = error
    method set_error : 'a. 'a formatter =
      begin fun msg ->
        error <- true ;
        descr#set_fg (`NAME "red") ;
        Pretty_utils.ksfprintf descr#set_text msg ;
      end

    method update_field :
      'a. ?enabled:bool -> ?title:string -> ?tooltip:string ->
      ?range:bool -> ?vmin:int -> ?vmax:int ->
      ?filter:(Lang.F.term -> bool) -> 'a field -> unit =
      fun ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter field ->
        let id = Tactical.ident field in
        List.iter (fun (fd : wfield) ->
            fd#update ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter id
          ) wfields

    (* -------------------------------------------------------------------------- *)
    (* --- Widget Behavior                                                    --- *)
    (* -------------------------------------------------------------------------- *)

    method private reset_dongle =
      begin
        self#set_label tac#title ;
        descr#set_text tac#descr ;
        if error then descr#set_fg `NORMAL ;
        error <- false ;
        edited <- None ;
      end

    method private reset_fields =
      List.iter (fun fd -> fd#clear) wfields

    method private compose widget =
      match edited with
      | None -> ()
      | Some edited -> self#set_action () ; edited.composer widget

    method private browse widget =
      match edited with
      | None -> ()
      | Some edited -> self#set_action () ; edited.browser widget

    method private updated () =
      match edited with
      | None -> ()
      | Some { process ; composer ; browser ; target } ->
          self#select ~process ~composer ~browser target

    method clear =
      begin
        self#reset_dongle ;
        self#reset_fields ;
        self#set_status `FIND ;
        self#set_action () ;
      end

    method targeted = match edited with None -> false | Some _ -> true

    method select ~process ~browser ~composer (target : selection) =
      begin
        self#reset_dongle ;
        List.iter (fun fd -> fd#select target) wfields ;
        let status =
          try tac#select (self :> feedback) target
          with Not_found | Exit -> Not_applicable
        in
        match status , error with
        | Not_applicable , _ ->
            self#set_visible false ;
            self#set_status `FIND ;
            self#set_action () ;
        | Not_configured , _ | Applicable _ , true ->
            self#set_visible true ;
            edited <- Some { process ; composer ; browser ; target } ;
            self#set_status `DIALOG_WARNING ;
            self#set_action () ;
        | Applicable proc , false ->
            self#set_visible true ;
            edited <- Some { process ; composer ; browser ; target } ;
            self#set_status `APPLY ;
            let callback () = process tac target proc in
            self#set_action ~callback () ;
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- Strategies                                                         --- *)
(* -------------------------------------------------------------------------- *)

type hform = {
  search : Strategy.heuristic ;
  widget : Widget.checkbox ;
}

let compare f g = String.compare f.search#title g.search#title

class strategies () =
  let form = new Wpane.form () in
  object(self)
    inherit Wpalette.tool
        ~content:form#widget
        ~label:"Strategies"
        ~tooltip:"Apply Custom Strategies" ()
    val mutable hforms : hform list = []
    val mutable demon : (Strategy.heuristic list -> unit) option = None

    method register (search : Strategy.heuristic) =
      begin
        let widget = new Widget.checkbox
          ~label:search#title ~tooltip:search#descr () in
        let config = "wp.strategies." ^ search#id in
        let default = Gtk_helper.Configuration.find_bool ~default:true config in
        widget#set default ;
        widget#connect (Gtk_helper.Configuration.set_bool config) ;
        widget#on_event self#update ;
        form#add_row widget#coerce ;
        let hform = { search ; widget } in
        hforms <- List.merge compare [hform] hforms
      end

    method private update () =
      match demon with
      | None ->
          self#set_visible false
      | Some _ ->
          self#set_visible true ;
          if List.exists (fun h -> h.widget#get) hforms then
            begin
              self#set_status `APPLY ;
              self#set_action ~callback:self#callback () ;
            end
          else
            begin
              self#set_status `INDEX ;
              self#set_action ()
            end

    method private callback () =
      match demon with
      | Some f ->
          let hs =
            List.fold_right
              (fun h hs -> if h.widget#get then h.search :: hs else hs)
              hforms [] in
          f hs
      | None -> ()

    method connect f = demon <- f ; self#update ()
  end

(* -------------------------------------------------------------------------- *)
