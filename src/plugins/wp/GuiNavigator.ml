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

(* -------------------------------------------------------------------------- *)
(* --- WP Lower Panel                                                     --- *)
(* -------------------------------------------------------------------------- *)

open Design
open Widget
open Property
open GuiSource

(* -------------------------------------------------------------------------- *)
(* --- Build the Reactive Behavior of GUI                                 --- *)
(* -------------------------------------------------------------------------- *)

type scope = [ `All | `Module | `Select ]
type filter = [ `ToProve | `Scripts | `All ]
type card = [ `List | `Goal ]
type focus =
  [ `All
  | `Index of Wpo.index
  | `Call of GuiSource.call
  | `Property of Property.t ]

let index_of_lemma (l,_,_,_,_) =
  match LogicUsage.section_of_lemma l with
  | LogicUsage.Toplevel _ -> Wpo.Axiomatic None
  | LogicUsage.Axiomatic a -> Wpo.Axiomatic (Some a.LogicUsage.ax_name)

let focus_of_selection selection scope =
  match selection , scope with
  | S_none , _  | _ , `All -> `All
  | S_call c , `Select -> `Call c
  | S_call c , `Module -> `Index (Wpo.Function(c.s_caller,None))
  | S_fun kf , (`Select | `Module) -> `Index(Wpo.Function(kf,None))
  | S_prop (IPLemma ilem) , `Module -> `Index(index_of_lemma ilem)
  | S_prop (IPAxiomatic(name,_)) , _ -> `Index(Wpo.Axiomatic (Some name))
  | S_prop ip , `Select -> `Property ip
  | S_prop ip , `Module ->
      begin
        match Property.get_kf ip with
        | None -> `All
        | Some kf -> `Index(Wpo.Function(kf,None))
      end

exception FIRST of Wpo.t

let first iter =
  try iter (fun w -> raise (FIRST w)) ; None
  with FIRST w -> Some w

let iter_kf kf f = Wpo.iter ~index:(Wpo.Function(kf,None)) ~on_goal:f ()
let iter_ip ip f = Wpo.iter ~ip ~on_goal:f ()
let iter_ips ips f = List.iter (fun ip -> Wpo.iter ~ip ~on_goal:f ()) ips
let calls c = List.map snd (Statuses_by_call.all_call_preconditions_at
                              ~warn_missing:false c.s_caller c.s_stmt)

let goal_of_selection = function
  | S_none -> None
  | S_prop ip -> first (iter_ip ip)
  | S_call c -> first (iter_ips (calls c))
  | S_fun kf -> first (iter_kf kf)

class behavior
    ~(main : Design.main_window_extension_points)
    ~(scope : scope Widget.selector)
    ~(filter : filter Widget.selector)
    ~(next : Widget.button)
    ~(prev : Widget.button)
    ~(index : Widget.button)
    ~(clear : Widget.button)
    ~(card : card Widget.selector)
    ~(list : GuiList.pane)
    ~(goal : GuiGoal.pane)
    ~(source : GuiSource.highlighter)
    ~(popup : GuiSource.popup)
  =
  object(self)

    initializer
      let module Cfg = Gtk_helper.Configuration in
      begin
        Cfg.config_values ~key:"wp.navigator.scope"
          ~values:[`All,"all" ; `Module,"module" ; `Select,"select"]
          ~default:`Module scope ;
        Cfg.config_values ~key:"wp.navigator.filter"
          ~values:[`All,"all" ; `Scripts,"scripts" ;
                   `ToProve,"toprove"]
          ~default:`ToProve filter ;
        filter#on_event self#reload ;
      end

    val mutable focus : focus = `All
    val mutable currentgoal : Wpo.t option = None

    method update () =
      begin
        list#update_all ;
        source#update ;
        goal#update ;
      end

    method reload () =
      begin
        list#reload ;
        let to_prove g = not (Wpo.is_trivial g || Wpo.is_proved g) in
        let has_proof g =
          match ProofEngine.get g with
          | `None -> false
          | `Proof | `Script | `Saved -> true in
        let on_goal g =
          let ok =
            match filter#get with
            | `All -> true
            | `Scripts -> has_proof g
            | `ToProve -> to_prove g && (Wpo.is_unknown g || has_proof g)
          in if ok then list#add g
        in
        begin
          match focus with
          | `All -> Wpo.iter ~on_goal ()
          | `Index index -> Wpo.iter ~index ~on_goal ()
          | `Property ip -> Wpo.iter ~ip ~on_goal ()
          | `Call c -> iter_ips (calls c) on_goal
        end ;
        let n = list#size in
        let k = match currentgoal with
          | None -> (-1)
          | Some w ->
              try list#index w
              with Not_found -> (-1)
        in
        index#set_enabled (n>0) ;
        if n=0 then card#set `List ;
        let src = if n=1 && k=0
          then (card#set `Goal ; clear#set_enabled false ; true)
          else false in
        if k<0 then self#navigator false None
        else self#navigator src (Some (list#get k)) ;
      end

    method private set_focus f =
      focus <- f ; self#reload ()

    method private set_scope f =
      match f , currentgoal with
      | `Module , Some w -> self#set_focus (`Index (Wpo.get_index w))
      | `Select , Some w -> self#set_focus (`Property (Wpo.get_property w))
      | _ , _ -> self#set_focus `All

    method private set_selection s =
      let f = scope#get in
      currentgoal <- goal_of_selection s ;
      self#set_focus (focus_of_selection s f)

    (* -------------------------------------------------------------------------- *)
    (* --- Navigation from Next/Prev/List                                     --- *)
    (* -------------------------------------------------------------------------- *)

    method private details =
      match card#get , currentgoal with
      | `List , Some w -> list#show w
      | `List , None -> ()
      | `Goal , sw -> goal#select sw

    method private navigator src = function
      | None ->
          begin
            currentgoal <- None ;
            next#set_enabled false ;
            prev#set_enabled false ;
            source#set None ;
            self#details ;
          end
      | (Some w) as sw ->
          try
            currentgoal <- sw ;
            let n = list#size in
            let k = list#index w in
            prev#set_enabled (k > 0) ;
            next#set_enabled (succ k < n) ;
            source#set (if src then sw else None) ;
            self#details ;
          with Not_found ->
            self#navigator false None

    method private next () = self#move succ
    method private prev () = self#move pred
    method private move dir =
      try
        match currentgoal with
        | None -> ()
        | Some w ->
            begin
              self#navigator true None ;
              let k = list#index w in
              let w = list#get (dir k) in
              self#navigator true (Some w) ;
            end
      with Not_found ->
        self#navigator true None

    method private prove ?mode w prover =
      begin
        let refresh w =
          match card#get with
          | `List -> list#update w
          | `Goal -> goal#update in
        let callback w prover result =
          Wpo.set_result w prover result ; refresh w in
        let success w = function
          | None -> callback w prover VCS.no_result
          | Some _ -> refresh w in
        let schedule task =
          let thread = Task.thread task in
          let kill () =
            Wpo.set_result w prover VCS.no_result ;
            Task.cancel thread ;
          in
          Wpo.set_result w prover (VCS.computing kill) ;
          let server = ProverTask.server () in
          Task.spawn server thread ;
          Task.launch server in
        match prover with
        | VCS.Why3ide ->
            let iter f = Wpo.iter ~on_goal:f () in
            schedule (ProverWhy3ide.prove ~callback ~iter)
        | VCS.Tactical ->
            begin
              match mode , ProverScript.get w with
              | (None | Some VCS.BatchMode) , `Script ->
                  schedule (ProverScript.prove ~callback ~success w)
              | _ ->
                  card#set `Goal ;
                  clear#set_enabled false ;
                  self#navigator true (Some w) ;
            end
        | _ ->
            let open VCS in
            let mode = match mode , prover with
              | Some m , _ -> m
              | None , Coq -> EditMode
              | None , AltErgo -> FixMode
              | _ -> BatchMode in
            schedule (Prover.prove w ~mode ~callback prover)
      end

    method private clear () =
      begin
        let title = "Delete Proof Obligations" in
        let text = Printf.sprintf
            "Confirm deletion of %d proof obligation(s)" list#count_selected in
        let icon = GMisc.image ~stock:`DELETE () in
        let response = GToolbox.question_box
            ~title ~buttons:["Delete POs" ; "Cancel"] ~default:1 ~icon text in
        if response = 1 then
          begin
            list#iter_selected Wpo.remove ;
            self#reload () ;
          end
      end

    (* -------------------------------------------------------------------------- *)
    (* --- Popup on Goals                                                     --- *)
    (* -------------------------------------------------------------------------- *)

    val popup_qed  = new Widget.popup ()
    val popup_tip  = new Widget.popup ()
    val popup_ergo = new Widget.popup ()
    val popup_coq  = new Widget.popup ()
    val popup_why3 = new Widget.popup ()
    val mutable popup_target = None

    method private popup_delete () =
      match popup_target with
      | Some(w,_) -> (popup_target <- None ; Wpo.remove w ; self#reload ())
      | None -> ()

    method private popup_delete_script () =
      match popup_target with
      | Some(w,_) -> ProofEngine.remove w ; ProofSession.remove w
      | None -> ()

    method private popup_run mode () =
      match popup_target with
      | Some(w,Some p) -> (popup_target <- None ; self#prove ~mode w p)
      | _ -> popup_target <- None

    method private popup_why3ide () =
      match popup_target with
      | Some(w,_) -> (popup_target <- None ; self#prove w VCS.Why3ide)
      | _ -> popup_target <- None

    method private add_popup_delete popup =
      begin
        popup#add_separator ;
        popup#add_item ~label:"Delete Goal" ~callback:self#popup_delete ;
      end

    method private add_popup_proofmodes popup modes =
      List.iter
        (fun (label,mode) ->
           popup#add_item ~label ~callback:(self#popup_run mode))
        modes

    initializer
      let open VCS in
      begin
        popup_tip#add_item ~label:"Run Script" ~callback:(self#popup_run BatchMode) ;
        popup_tip#add_item ~label:"Edit Proof" ~callback:(self#popup_run EditMode) ;
        popup_tip#add_item ~label:"Delete Script" ~callback:(self#popup_delete_script) ;
        self#add_popup_proofmodes popup_why3
          [ "Run",BatchMode ] ;
        self#add_popup_proofmodes popup_ergo
          [ "Run",BatchMode ; "Open Altgr-Ergo on Fail",EditMode ; "Open Altgr-Ergo",EditMode ] ;
        self#add_popup_proofmodes popup_coq
          [ "Check Proof",BatchMode ; "Edit on Fail",EditMode ; "Edit Proof",EditMode ] ;
        List.iter
          (fun menu ->
             menu#add_item ~label:"Open Why3ide" ~callback:self#popup_why3ide ;
             self#add_popup_delete menu ;
          ) [ popup_qed ; popup_why3 ; popup_ergo ; popup_coq ] ;
      end

    method private popup w p =
      let open VCS in
      begin
        popup_target <- Some (w,p) ;
        match p with
        | None | Some Tactical -> popup_tip#run ()
        | Some (Qed|Why3ide) -> popup_qed#run ()
        | Some Coq -> popup_coq#run ()
        | Some AltErgo -> popup_ergo#run ()
        | Some (Why3 _) -> popup_why3#run ()
      end

    method private action w p =
      match p with
      | None ->
          begin
            card#set `Goal ;
            clear#set_enabled false ;
            self#navigator true (Some w) ;
          end
      | Some p ->
          begin
            self#navigator true (Some w) ;
            self#prove w p ;
            list#update w ;
          end

    (* -------------------------------------------------------------------------- *)
    (* --- Popup on Goals                                                     --- *)
    (* -------------------------------------------------------------------------- *)

    initializer
      begin
        clear#set_enabled false ;
        next#connect self#next ;
        prev#connect self#prev ;
        index#connect (fun () -> card#set `List) ;
        list#on_click (fun w _p -> self#navigator true (Some w)) ;
        list#on_right_click
          (fun w p ->
             begin
               self#navigator true (Some w) ;
               self#popup w p ;
               list#update w ;
             end
          ) ;
        list#on_double_click self#action ;
        list#on_selection (fun n -> clear#set_enabled (n>0)) ;
        card#connect (fun _ -> self#details) ;
        scope#connect self#set_scope ;
        popup#on_click self#set_selection ;
        popup#on_prove (GuiPanel.run_and_prove main) ;
        clear#connect self#clear ;
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- Make Panel and Extend Frama-C GUI                                  --- *)
(* -------------------------------------------------------------------------- *)

let make (main : main_window_extension_points) =
  begin

    (* -------------------------------------------------------------------------- *)
    (* --- Provers                                                            --- *)
    (* -------------------------------------------------------------------------- *)

    let available = new GuiConfig.provers "wp.available" in
    let enabled = new GuiConfig.provers "wp.enabled" in
    if Wp_parameters.Detect.get () then ProverWhy3.detect_provers available#set ;

    let dp_chooser = new GuiConfig.dp_chooser ~main ~available ~enabled in

    (* -------------------------------------------------------------------------- *)
    (* --- Focus Bar                                                          --- *)
    (* -------------------------------------------------------------------------- *)

    let scope = new Widget.menu ~default:`Module ~options:[
      `All, "Global" ;
      `Module, "Module" ;
      `Select , "Property" ;
    ] () in
    let filter = new Widget.menu ~default:`ToProve ~options:[
      `ToProve , "Not Proved (yet)" ;
      `Scripts , "All Scripts" ;
      `All , "All Goals" ;
    ] () in 
    let prev = new Widget.button ~icon:`GO_BACK ~tooltip:"Previous goal" () in
    let next = new Widget.button ~icon:`GO_FORWARD ~tooltip:"Next goal" () in
    let index = new Widget.button ~icon:`INDEX ~tooltip:"List of goals" () in
    let navigation = Wbox.hgroup [
        (prev :> widget) ;
        (index :> widget) ;
        (next :> widget) ;
      ] in
    let provers = new Widget.button ~label:"Provers..." () in
    let clear = new Widget.button ~label:"Clear" ~icon:`DELETE () in
    let focusbar = GPack.hbox ~spacing:0 () in
    begin
      focusbar#pack ~padding:0 ~expand:false navigation#coerce ;
      focusbar#pack ~padding:20 ~expand:false scope#coerce ;
      focusbar#pack ~padding:20 ~expand:false filter#coerce ;
      focusbar#pack ~from:`END ~expand:false clear#coerce ;
      focusbar#pack ~from:`END ~expand:false provers#coerce ;
      provers#connect dp_chooser#run ;
    end ;

    (* -------------------------------------------------------------------------- *)
    (* --- Filter Popup                                                       --- *)
    (* -------------------------------------------------------------------------- *)

    begin
      filter#set_render (function
          | `All -> "All Results"
          | `Scripts -> "All Scripts"
          | `ToProve -> "Not Proved") ;
      filter#set_items [ `ToProve ; `Scripts ; `All ] ;
    end ;

    (* -------------------------------------------------------------------------- *)
    (* --- List/Goal view                                                     --- *)
    (* -------------------------------------------------------------------------- *)

    let book = new Wpane.notebook ~default:`List () in
    let list = new GuiList.pane enabled in
    let goal = new GuiGoal.pane enabled in
    begin
      book#add `List list#coerce ;
      book#add `Goal goal#coerce ;
    end ;

    (* -------------------------------------------------------------------------- *)
    (* --- Source Feedback                                                    --- *)
    (* -------------------------------------------------------------------------- *)

    let source = new GuiSource.highlighter main in
    let popup = new GuiSource.popup () in

    (* -------------------------------------------------------------------------- *)
    (* --- Panel Behavior                                                     --- *)
    (* -------------------------------------------------------------------------- *)

    let card = (book :> _ Widget.selector) in
    let scope = (scope :> _ Widget.selector) in
    let filter = (filter :> _ Widget.selector) in
    let behavior = new behavior ~main
      ~next ~prev ~index ~scope ~filter ~clear
      ~list ~card ~goal ~source ~popup in
    GuiPanel.on_reload behavior#reload ;
    GuiPanel.on_update behavior#update ;

    (* -------------------------------------------------------------------------- *)
    (* --- Panel view                                                         --- *)
    (* -------------------------------------------------------------------------- *)

    let panel = GPack.vbox ~homogeneous:false () in
    panel#pack ~expand:false focusbar#coerce ;
    panel#pack ~expand:true ~fill:true book#coerce ;
    let tab_label = (GMisc.label ~text:"WP Goals" ())#coerce in
    ignore (panel#misc#connect#after#realize behavior#reload) ;
    ignore (main#lower_notebook#append_page ~tab_label panel#coerce) ;
    main#register_source_highlighter source#highlight ;
    main#register_source_selector popup#register ;
    GuiPanel.register ~main
      ~available_provers:available
      ~enabled_provers:enabled
      ~configure_provers:dp_chooser#run ;
  end

let () = Design.register_extension make
let () = Design.register_reset_extension
    (fun main -> main#protect ~cancelable:false GuiPanel.reload)
