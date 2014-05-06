(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

module Rte_generated =
  Kernel_function.Make_Table
    (Datatype.Unit)
    (struct
      let name = "GuiSource.Rte_generated"
      let size = 7
      let dependencies = [ Ast.self ]
     end)

let kf_of_selection = function
  | S_none -> None
  | S_fun kf -> Some kf
  | S_prop ip -> Property.get_kf ip
  | S_call s -> Some s.s_caller

let rte_generated s =
  match kf_of_selection s with
    | None -> false
    | Some kf -> 
	if Wp_parameters.RTE.get () then
	  let mem = Rte_generated.mem kf in
	  if not mem then
	    Rte_generated.add kf () ;
	  not mem
	else false

let run_and_prove 
    (main:Design.main_window_extension_points) 
    (selection:GuiSource.selection)
    =
  begin
    try
      begin
	match selection with
          | S_none -> raise Stop
	  | S_fun kf -> Register.wp_compute_kf (Some kf) [] []
          | S_prop ip -> Register.wp_compute_ip ip
          | S_call s -> Register.wp_compute_call s.s_stmt
      end ;
      if rte_generated selection then
	main#redisplay ()
      else
	reload ()
    with Stop -> ()
  end

(* ------------------------------------------------------------------------ *)
(* ---  Model Panel                                                     --- *)
(* ------------------------------------------------------------------------ *)

type memory = HOARE | TYPED

class model_selector (main : Design.main_window_extension_points) =
  let dialog = new Toolbox.dialog 
    ~title:"WP Memory Model" ~window:main#main_window () in
  let memory = new Toolbox.switch HOARE in
  let r_hoare  = memory#add_radio ~label:"Hoare Memory Model" ~value:HOARE () in
  let r_typed  = memory#add_radio ~label:"Typed Memory Model" ~value:TYPED () in
  let c_casts  = new Toolbox.checkbox ~label:"Unsafe casts" () in
  let c_byref  = new Toolbox.checkbox ~label:"Reference Arguments" () in
  let c_cint   = new Toolbox.checkbox ~label:"Machine Integers" () in
  let c_cfloat = new Toolbox.checkbox ~label:"Floating Points" () in
  let m_label  = new Toolbox.label ~style:`Title () in
object(self)
  
  initializer
    begin
      dialog#add_row r_hoare#coerce ;
      dialog#add_row r_typed#coerce ;
      dialog#add_row c_casts#coerce ;
      dialog#add_row c_byref#coerce ;
      dialog#add_row c_cint#coerce ;
      dialog#add_row c_cfloat#coerce ;
      dialog#add_row m_label#coerce ;
      dialog#button ~label:"Cancel" ~icon:`CANCEL ~action:(`CANCEL) () ;
      dialog#button ~label:"Apply"  ~icon:`APPLY  ~action:(`APPLY) () ;
      memory#on_check TYPED c_casts#set_enabled ;
      memory#on_event self#connect ;
      c_casts#on_event self#connect ;
      c_byref#on_event self#connect ;
      c_cint#on_event self#connect ;
      c_cfloat#on_event self#connect ;
      dialog#on_value `APPLY self#update ;
    end

  method update () = Wp_parameters.Model.set [Factory.ident self#get]

  method set (s:setup) =
    begin
      (match s.mheap with 
	 | Hoare -> memory#set HOARE
	 | Typed m -> memory#set TYPED ; c_casts#set (m = MemTyped.Unsafe)) ;
      c_byref#set (s.mvar = Ref) ;
      c_cint#set (s.cint = Cint.Machine) ;
      c_cfloat#set (s.cfloat = Cfloat.Float) ;
    end

  method get : setup =
    let m = match memory#get with 
      | HOARE -> Hoare 
      | TYPED -> Typed
	  (if c_casts#get then MemTyped.Unsafe else MemTyped.Fits) 
    in {
      mheap = m ;
      mvar = if c_byref#get then Ref else Var ;
      cint = if c_cint#get then Cint.Machine else Cint.Natural ;
      cfloat = if c_cfloat#get then Cfloat.Float else Cfloat.Real ;
    }

  method connect () = m_label#set_text (Factory.descr self#get)

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

let wp_dir = ref (Sys.getcwd())

let wp_script () =
  let file = GToolbox.select_file
    ~title:"Script File for Coq proofs"
    ~dir:wp_dir ~filename:"wp.script" ()
  in
  match file with
    | Some f -> Wp_parameters.Script.set f
    | None -> ()

let wp_update_model label () =
  let s = Factory.parse (Wp_parameters.Model.get ()) in
  label#set_text (Factory.descr s)

let wp_configure_model main label () =
  begin
    (new model_selector main)#run ;
    wp_update_model label () ;
  end

let wp_update_script label () =
  let file = Wp_parameters.Script.get () in
  let text = if file = "" then "(None)" else Filename.basename file in
  label#set_text text

let wp_panel 
    ~(main:Design.main_window_extension_points) 
    ~(available_provers:GuiConfig.provers)
    ~(enabled_provers:GuiConfig.provers)
    ~(configure_provers:unit -> unit) 
    =
  let vbox = GPack.vbox () in
  let demon = Gtk_form.demon () in
  let packing = vbox#pack in

  let form = new Toolbox.form () in
  (* Model Row *)
  let model_cfg = new Toolbox.button 
    ~label:"Model..." ~tooltip:"Configure WP Model" () in
  let model_lbl = GMisc.label ~xalign:0.0 () in
  Gtk_form.register demon (wp_update_model model_lbl) ;
  model_cfg#connect (wp_configure_model main model_lbl) ;
  form#add_label_widget model_cfg#coerce ;
  form#add_field model_lbl#coerce ;
  (* Script Row *)
  let script_cfg = new Toolbox.button
    ~label:"Script..." ~tooltip:"Load/Save User Scripts file" () in
  let script_lbl = GMisc.label ~xalign:0.0 () in
  Gtk_form.register demon (wp_update_script script_lbl) ;
  script_cfg#connect wp_script ;
  form#add_label_widget script_cfg#coerce ;
  form#add_field script_lbl#coerce ;
  (* Prover Row *)
  let prover_cfg = new Toolbox.button 
    ~label:"Provers..." ~tooltip:"Detect WP Provers" () in
  prover_cfg#connect configure_provers ;
  form#add_label_widget prover_cfg#coerce ;
  let prover_menu = new GuiConfig.dp_button
    ~available:available_provers
    ~enabled:enabled_provers in
  form#add_field prover_menu#coerce ;
  Gtk_form.register demon prover_menu#update ;
  (* End Form *)
  packing form#coerce ;

  let options = GPack.hbox ~spacing:16 ~packing () in

  Gtk_form.check ~label:"RTE"
    ~tooltip:"Generates RTE guards for WP"
    ~packing:options#pack
    Wp_parameters.RTE.get Wp_parameters.RTE.set demon ;

  Gtk_form.check ~label:"Split"
    ~tooltip:"Splits conjunctions into sub-goals"
    ~packing:options#pack
    Wp_parameters.Split.get Wp_parameters.Split.set demon ;

  Gtk_form.check ~label:"Trace"
    ~tooltip:"Reports proof information from provers"
    ~packing:options#pack
    Wp_parameters.ProofTrace.get Wp_parameters.ProofTrace.set demon ;

  let options = GPack.hbox ~spacing:8 ~packing () in

  Gtk_form.check ~label:"Invariants"
    ~tooltip:"Alternative WP for loop with arbitrary invariants"
    ~packing:options#pack
    Wp_parameters.Invariants.get Wp_parameters.Invariants.set demon ;

  let control = GPack.table ~columns:4 ~col_spacings:8 ~rows:2 ~packing () in
  let addcontrol line col w = control#attach ~left:(col-1) ~top:(line-1) ~expand:`NONE w in

  Gtk_form.label ~text:"Steps" ~packing:(addcontrol 1 1) () ;
  Gtk_form.spinner ~lower:0 ~upper:100000
    ~tooltip:"Search steps for alt-ergo prover"
    ~packing:(addcontrol 1 2)
    Wp_parameters.Steps.get Wp_parameters.Steps.set demon ;

  Gtk_form.label ~text:"Depth" ~packing:(addcontrol 1 3) () ;
  Gtk_form.spinner ~lower:0 ~upper:100000
    ~tooltip:"Search space bound for alt-ergo prover"
    ~packing:(addcontrol 1 4)
    Wp_parameters.Depth.get Wp_parameters.Depth.set demon ;

  Gtk_form.label ~text:"Timeout" ~packing:(addcontrol 2 1) () ;
  Gtk_form.spinner ~lower:0 ~upper:100000
    ~tooltip:"Timeout for proving one proof obligation"
    ~packing:(addcontrol 2 2)
    Wp_parameters.Timeout.get Wp_parameters.Timeout.set demon ;

  Gtk_form.label ~text:"Process" ~packing:(addcontrol 2 3) () ;
  Gtk_form.spinner ~lower:1 ~upper:32
    ~tooltip:"Maximum number of parallel running provers"
    ~packing:(addcontrol 2 4)
    Wp_parameters.Procs.get
    (fun n ->
       Wp_parameters.Procs.set n ;
       ignore (ProverTask.server ()) 
	 (* to make server procs updated is server exists *)
    ) demon ;

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

let register ~main ~available_provers ~enabled_provers ~configure_provers = 
  main#register_panel 
    (fun main -> wp_panel ~main ~available_provers ~enabled_provers ~configure_provers)

(* -------------------------------------------------------------------------- *)
