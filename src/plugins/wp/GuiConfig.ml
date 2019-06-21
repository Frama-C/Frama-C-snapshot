(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open VCS

(* ------------------------------------------------------------------------ *)
(* ---  Prover List in Configuration                                    --- *)
(* ------------------------------------------------------------------------ *)

class available () =
  object(self)
    val mutable dps = []
    method get = dps
    method detect =
      try dps <- ProverDetect.detect ()
      with exn ->
        Wp_parameters.error "Why3 detection error:@\n%s"
          (Printexc.to_string exn)
    initializer self#detect
  end

class enabled key =
  object(self)
    inherit [string list] Wutil.selector []

    method private load () =
      let open Gtk_helper.Configuration in
      let rec collect w = function
        | ConfString s -> s :: w
        | ConfList fs -> List.fold_left collect w fs
        | _ -> w in
      try
        let data = Gtk_helper.Configuration.find key in
        List.rev (collect [] data)
      with Not_found -> []

    method private save () =
      let open Gtk_helper.Configuration in
      Gtk_helper.Configuration.set key
        (ConfList (List.map (fun s -> ConfString s) self#get))

    initializer
      begin
        let settings = self#load () in
        let cmdline = Wp_parameters.Provers.get () in
        let selection = List.sort_uniq String.compare (settings @ cmdline) in
        self#set selection ;
        self#on_event self#save ;
      end

  end

(* ------------------------------------------------------------------------ *)
(* ---  WP Provers Configuration Panel                                  --- *)
(* ------------------------------------------------------------------------ *)

class dp_chooser
    ~(main:Design.main_window_extension_points)
    ~(available:available)
    ~(enabled:enabled)
  =
  let dialog = new Wpane.dialog
    ~title:"Why3 Provers"
    ~window:main#main_window
    ~resize:false () in
  let array = new Wpane.warray () in
  object(self)

    val mutable selected = []

    method private enable dp e =
      let rec hook dp e = function
        | [] -> [dp,e]
        | head :: tail ->
            if fst head = dp then (dp,e) :: tail
            else head :: hook dp e tail
      in selected <- hook dp e selected

    method private lookup dp =
      try List.assoc dp selected
      with Not_found -> false

    method private entry dp =
      let text = Pretty_utils.to_string VCS.pretty dp in
      let sw = new Widget.switch () in
      let lb = new Widget.label ~align:`Left ~text () in
      sw#set (self#lookup dp) ;
      sw#connect (self#enable dp) ;
      let hbox = GPack.hbox ~spacing:10 ~homogeneous:false () in
      hbox#pack ~expand:false sw#coerce ;
      hbox#pack ~expand:true lb#coerce ;
      (object
        method widget = hbox#coerce
        method update () = sw#set (self#lookup dp)
        method delete () = ()
      end)

    method private configure dps =
      begin
        array#set dps ;
        array#update () ;
      end

    method private detect () =
      begin
        available#detect ;
        self#configure available#get ;
      end

    method private apply () =
      let rec choose = function
        | ({dp_shortcuts=key::_},true)::dps -> key :: choose dps
        | _::dps -> choose dps
        | [] -> []
      in enabled#set (choose selected)

    method run () =
      let dps = available#get in
      let sel = enabled#get in
      selected <- List.map
          (fun dp -> dp,List.exists (fun k -> List.mem k sel) dp.dp_shortcuts)
          dps ;
      self#configure dps ;
      dialog#run ()

    initializer
      begin
        dialog#button ~action:(`ACTION self#detect) ~label:"Detect Provers" () ;
        dialog#button ~action:(`CANCEL) ~label:"Cancel" () ;
        dialog#button ~action:(`APPLY) ~label:"Apply" () ;
        array#set_entry self#entry ;
        dialog#add_block array#coerce ;
        dialog#on_value `APPLY self#apply ;
      end

  end

(* ------------------------------------------------------------------------ *)
(* ---  WP Prover Switch Panel                                          --- *)
(* ------------------------------------------------------------------------ *)

type mprover =
  | NONE
  | ERGO
  | COQ
  | WHY of VCS.dp

class dp_button ~(available:available) =
  let render = function
    | NONE -> "(none)"
    | ERGO -> "Alt-Ergo (native)"
    | COQ -> "Coq (native)"
    | WHY { dp_shortcuts = keys } when List.mem "alt-ergo" keys ->
        "Alt-Ergo (why3)"
    | WHY dp -> Pretty_utils.to_string VCS.pretty dp in
  let select = function
    | ERGO -> "alt-ergo"
    | COQ -> "coq"
    | WHY { dp_shortcuts=[] } | NONE -> "none"
    | WHY { dp_shortcuts=key::_ } -> "why3:"^key in
  let rec import = function
    | [] -> ERGO
    | spec::others ->
        match VCS.prover_of_name spec with
        | None | Some Qed -> NONE
        | Some (AltErgo|Tactical) -> ERGO
        | Some Coq -> COQ
        | Some (Why3 s) ->
            try
              let dps = available#get in
              WHY (List.find (fun dp -> List.mem s dp.dp_shortcuts) dps)
            with Not_found -> import others
  in
  let items = [ NONE ; ERGO ; COQ ] in
  let button = new Widget.menu ~default:ERGO ~render ~items () in
  object(self)
    method coerce = button#coerce
    method widget = (self :> Widget.t)
    method set_enabled = button#set_enabled
    method set_visible = button#set_visible

    val mutable dps = []

    method update () =
      (* called in polling mode *)
      begin
        let avl = available#get in
        if avl != dps then
          begin
            dps <- avl ;
            let items = [NONE;ERGO] @ List.map (fun p -> WHY p) dps @ [COQ] in
            button#set_items items
          end ;
        let cur = Wp_parameters.Provers.get () |> import in
        if cur <> button#get then button#set cur ;
      end

    initializer button#connect
        (fun s -> Wp_parameters.Provers.set [select s])
  end

(* ------------------------------------------------------------------------ *)
