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

open Factory
open GuiSource

(* ------------------------------------------------------------------------ *)
(* ---  RUN WP                                                          --- *)
(* ------------------------------------------------------------------------ *)

exception Stop

let update_callback = ref (fun () -> ())
let on_update f = update_callback := f
let update () = !update_callback ()

let reload_callback = ref (fun () -> ())
let on_reload f = reload_callback := f
let reload () = !reload_callback ()

module Wp_rte_generated =
  Kernel_function.Make_Table
    (Datatype.Unit)
    (struct
      let name = "GuiSource.Rte_generated"
      let size = 8
      let dependencies = [ Ast.self ]
    end)

let kf_of_selection = function
  | S_none -> None
  | S_fun kf -> Some kf
  | S_prop ip -> Property.get_kf ip
  | S_call s -> Some s.s_caller

let wp_rte_generated s =
  match kf_of_selection s with
  | None -> false
  | Some kf ->
      if Wp_parameters.RTE.get () then
        let mem = Wp_rte_generated.mem kf in
        if not mem then
          Wp_rte_generated.add kf () ;
        not mem
      else false

let spawn provers vcs =
  if not (Bag.is_empty vcs) then
    let provers = Why3.Whyconf.Sprover.elements provers#get in
    VC.command ~provers ~tip:true vcs

let run_and_prove
    (main:Design.main_window_extension_points)
    (provers:GuiConfig.provers)
    (selection:GuiSource.selection)
  =
  begin
    try
      begin
        match selection with
        | S_none -> raise Stop
        | S_fun kf -> spawn provers (VC.generate_kf kf)
        | S_prop ip -> spawn provers (VC.generate_ip ip)
        | S_call s -> spawn provers (VC.generate_call s.s_stmt)
      end ;
      if wp_rte_generated selection then
        main#redisplay ()
      else
        reload ()
    with Stop -> ()
  end

(* ------------------------------------------------------------------------ *)
(* ---  Model Panel                                                     --- *)
(* ------------------------------------------------------------------------ *)

type memory = TREE | HOARE | TYPED | REGION

class model_selector (main : Design.main_window_extension_points) =
  let dialog = new Wpane.dialog
    ~title:"WP Memory Model" ~window:main#main_window () in
  let memory = new Widget.group HOARE in
  let r_hoare  = memory#add_radio ~label:"Hoare Memory Model" ~value:HOARE () in
  let r_typed  = memory#add_radio ~label:"Typed Memory Model" ~value:TYPED () in
  let c_casts  = new Widget.checkbox ~label:"Unsafe casts" () in
  let c_byref  = new Widget.checkbox ~label:"Reference Arguments" () in
  let c_ctxt   = new Widget.checkbox ~label:"Context Arguments (Caveat)" () in
  let c_cint   = new Widget.checkbox ~label:"Machine Integers" () in
  let c_cfloat = new Widget.checkbox ~label:"Floating Points" () in
  let m_label  = new Widget.label ~style:`Title () in
  object(self)

    initializer
      begin
        dialog#add_row r_hoare#coerce ;
        dialog#add_row r_typed#coerce ;
        dialog#add_row c_casts#coerce ;
        dialog#add_row c_byref#coerce ;
        dialog#add_row c_ctxt#coerce ;
        dialog#add_row c_cint#coerce ;
        dialog#add_row c_cfloat#coerce ;
        dialog#add_row m_label#coerce ;
        dialog#button ~label:"Cancel" ~icon:`CANCEL ~action:(`CANCEL) () ;
        dialog#button ~label:"Apply"  ~icon:`APPLY  ~action:(`APPLY) () ;
        memory#on_check TYPED c_casts#set_enabled ;
        memory#on_event self#connect ;
        c_casts#on_event self#connect ;
        c_byref#on_event self#connect ;
        c_ctxt#on_event self#connect ;
        c_cint#on_event self#connect ;
        c_cfloat#on_event self#connect ;
        dialog#on_value `APPLY self#update ;
      end

    method update () = Wp_parameters.Model.set [Factory.ident self#get]

    method set (s:setup) =
      begin
        (match s.mheap with
         | ZeroAlias -> memory#set TREE
         | Region -> memory#set REGION
         | Hoare -> memory#set HOARE
         | Typed m -> memory#set TYPED ; c_casts#set (m = MemTyped.Unsafe)) ;
        c_byref#set (s.mvar = Ref) ;
        c_ctxt#set (s.mvar = Caveat) ;
        c_cint#set (s.cint = Cint.Machine) ;
        c_cfloat#set (s.cfloat = Cfloat.Float) ;
      end

    method get : setup =
      let m = match memory#get with
        | TREE -> ZeroAlias
        | REGION -> Region
        | HOARE -> Hoare
        | TYPED -> Typed
                     (if c_casts#get then MemTyped.Unsafe else MemTyped.Fits)
      in {
        mheap = m ;
        mvar = if c_ctxt#get then Caveat else if c_byref#get then Ref else Var ;
        cint = if c_cint#get then Cint.Machine else Cint.Natural ;
        cfloat = if c_cfloat#get then Cfloat.Float else Cfloat.Real ;
      }

    method connect () =
      begin
        m_label#set_text (Factory.descr self#get) ;
        c_byref#set_enabled (not c_ctxt#get) ;
      end

    method run =
      begin
        let s = Factory.parse (Wp_parameters.Model.get ()) in
        self#set s ;
        self#connect () ;
        dialog#run () ;
      end

  end

(* ------------------------------------------------------------------------ *)
(* ---  WP Panel                                                        --- *)
(* ------------------------------------------------------------------------ *)

let wp_configure_model main () =
  (new model_selector main)#run

let wp_panel
    ~(main:Design.main_window_extension_points)
    ~(configure_provers:unit -> unit)
  =
  let vbox = GPack.vbox () in
  let demon = Gtk_form.demon () in
  let packing = vbox#pack in

  let control = GPack.table ~columns:2 ~col_spacings:8 ~rows:2 ~packing () in
  let addcontrol line col w = control#attach ~left:(col-1) ~top:(line-1) ~expand:`NONE w in

  Gtk_form.label ~text:"timeout" ~packing:(addcontrol 1 2) () ;
  Gtk_form.spinner ~lower:0 ~upper:100000
    ~tooltip:"Timeout for proving one proof obligation"
    ~packing:(addcontrol 1 1)
    Wp_parameters.Timeout.get Wp_parameters.Timeout.set demon ;

  Gtk_form.label ~text:"process" ~packing:(addcontrol 2 2) () ;
  Gtk_form.spinner ~lower:1 ~upper:32
    ~tooltip:"Maximum number of parallel running provers"
    ~packing:(addcontrol 2 1)
    Wp_parameters.Procs.get
    (fun n ->
       Wp_parameters.Procs.set n ;
       ignore (ProverTask.server ())
       (* to make server procs updated is server exists *)
    ) demon ;

  let hbox = GPack.hbox ~packing () in

  let model_cfg = new Widget.button
    ~label:"Model..." ~tooltip:"Configure WP Model" () in
  model_cfg#connect (wp_configure_model main) ;
  hbox#pack model_cfg#coerce ;

  let prover_cfg = new Widget.button
    ~label:"Provers..." ~tooltip:"Detect WP Provers" () in
  prover_cfg#connect configure_provers ;
  hbox#pack prover_cfg#coerce ;

  Gtk_form.menu [
    "No Cache" ,    ProverWhy3.NoCache ;
    "Update" ,  ProverWhy3.Update ;
    "Cleanup" , ProverWhy3.Cleanup ;
    "Rebuild" , ProverWhy3.Rebuild ;
    "Replay" ,  ProverWhy3.Replay ;
    "Offline" , ProverWhy3.Offline ;
  ] ~packing:hbox#pack ~tooltip:"Proof Cache Mode"
    ProverWhy3.get_mode ProverWhy3.set_mode demon ;

  let pbox = GPack.hbox ~packing ~show:false () in
  let progress = GRange.progress_bar ~packing:(pbox#pack ~expand:true ~fill:true) () in
  let cancel = GButton.button ~packing:(pbox#pack ~expand:false) ~stock:`STOP () in
  cancel#misc#set_sensitive false ;
  let server = ProverTask.server () in
  ignore (cancel#connect#released (fun () -> Task.cancel_all server)) ;
  let inactive = (0,0) in
  let state = ref inactive in
  Task.on_server_activity server
    (fun () ->
       let scheduled = Task.scheduled server in
       let terminated = Task.terminated server in
       let remaining = scheduled - terminated in

       if remaining <= 0 then
         ( pbox#misc#hide () ; state := inactive ; cancel#misc#set_sensitive false )
       else
         begin
           if !state = inactive then
             ( pbox#misc#show () ; cancel#misc#set_sensitive true ) ;

           let s_term , s_sched = !state in

           if s_term <> terminated then update () ;
           if s_sched <> scheduled || s_term <> terminated then
             begin
               progress#set_text (Printf.sprintf "%d / %d" terminated scheduled) ;
               progress#set_fraction
                 (if scheduled = 0 then 1.0 else (float terminated /. float scheduled)) ;
             end ;

           state := (terminated,remaining) ;
         end) ;
  Task.on_server_stop server update ;
  begin
    "WP" , vbox#coerce , Some (Gtk_form.refresh demon) ;
  end

let register ~main ~configure_provers =
  main#register_panel
    (fun main -> wp_panel ~main ~configure_provers)

(* -------------------------------------------------------------------------- *)
