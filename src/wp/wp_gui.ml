(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open Cil_types

(* ------------------------------------------------------------------------ *)
(* ---  RUN WP                                                          --- *)
(* ------------------------------------------------------------------------ *)

exception Stop

type strategy_code = {
  sp_target : string ;
  sp_kf : Kernel_function.t option ;
  sp_bhv : string list ;
  sp_ip : Property.t option ;
}

type strategy_call = {
  sc_caller : Kernel_function.t ;
  sc_called : Kernel_function.t ;
  sc_callat : stmt ;
}

type strategy =
  | Snone
  | Scode of strategy_code
  | Scall of strategy_call

let kind_of_property = function
  | Property.IPCodeAnnot _ -> "annotation"
  | Property.IPPredicate( Property.PKRequires _ , _ , Kglobal , _ ) -> "preconditions for callers"
  | _ -> "property"

let get_strategy localizable : strategy =
  match localizable with
    | Pretty_source.PStmt( kf , stmt )
    | Pretty_source.PLval( Some kf , Kstmt stmt , _ )
    | Pretty_source.PTermLval( Some kf , Kstmt stmt , _ )
      ->
        begin
          match stmt with
            | { skind=Instr(Call(_,e,_,_)) } ->
                begin
                  match WpStrategy.get_called_kf e with
                    | None -> Snone
                    | Some called ->
                        Scall {
                          sc_caller = kf ;
                          sc_called = called ;
                          sc_callat = stmt ;
                        }
                end
            | _ -> Snone
(*
                Scode {
                  sp_target = "function contract" ;
                  sp_kf = Some kf ;
                  sp_bhv = [] ;
                  sp_ip = None ;
                }
*)
        end

    | Pretty_source.PVDecl (Some kf,{vglob=true}) ->
        Scode {
          sp_target = "function contract" ;
          sp_kf   = Some kf ;
          sp_bhv  = [] ;
          sp_ip   = None ;
        }

    | Pretty_source.PIP ip ->
        Scode {
          sp_target = kind_of_property ip ;
          sp_kf   = Property.get_kf ip ;
          sp_bhv  = 
	    Extlib.may_map
	      ~dft:[] 
	      (fun x -> [ x.b_name ]) 
	      (Property.get_behavior ip) ;
          sp_ip   = Some ip ;
        }

    | Pretty_source.PVDecl _
    | Pretty_source.PLval _ 
    | Pretty_source.PTermLval _
    | Pretty_source.PGlobal _ ->
        Snone

let run_and_prove (main_ui:Design.main_window_extension_points) strategy =
  try
    begin
      match strategy with
        | Snone -> raise Stop
        | Scode s ->
            Register.wp_compute
              s.sp_kf s.sp_bhv s.sp_ip
        | Scall s ->
            Register.wp_compute_call
              ~kf_caller:s.sc_caller
              ~kf_called:s.sc_called
              s.sc_callat
    end ;
    main_ui#rehighlight () ;
    Po_navigator.refresh_panel () ;
    Task.on_server_stop
      (Prover.server ())
      (fun () -> 
	 Po_navigator.refresh_status () ; 
	 if Wp_parameters.RTE.get () (* TODO[LC] can be optimized *)
	 then main_ui#redisplay ()
	 else main_ui#rehighlight () ) ;
  with Stop -> ()

(* ------------------------------------------------------------------------ *)
(* ---  Source Highlighter                                              --- *)
(* ------------------------------------------------------------------------ *)

let wp_highlight
    (buffer:GSourceView2.source_buffer)
    (localizable:Pretty_source.localizable)
    ~(start:int) ~(stop:int) =
  match localizable with
    | Pretty_source.PStmt(_,({ skind=Instr(Call(_,e,_,_)) } as stmt)) ->
	(match WpStrategy.get_called_kf e with
	   | Some kg ->
	       let ips = WpAnnot.lookup_called_preconditions_at kg stmt in
	       if ips <> [] then
		 let validity = Property_status.Feedback.get_conjunction ips in
		 Design.Feedback.mark buffer ~start ~stop validity
	   | None -> ())
    | _ -> ()

(* ------------------------------------------------------------------------ *)
(* ---  Source Callback                                                 --- *)
(* ------------------------------------------------------------------------ *)

let is_rte_generated kf =
  List.for_all
    (fun (_,_,lookup,_) -> lookup kf)
    (!Db.RteGen.get_all_status ())
  
let is_rte_precond kf =
  let (_,_,lookup,_) = !Db.RteGen.get_precond_status () in (lookup kf)

let add_rte_menu
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) localizable =
  begin
    match localizable with
      | Pretty_source.PVDecl (Some kf,{vglob=true}) ->
	  if not (is_rte_generated kf) then
	    ignore (popup_factory#add_item "Insert WP-safety guards"
		      ~callback:(fun () -> !Db.RteGen.do_all_rte kf ; main_ui#redisplay ())) ;
	  if not (is_rte_precond kf) then
	    ignore (popup_factory#add_item "Insert all callees contract"
		      ~callback:(fun () -> !Db.RteGen.do_precond kf ; main_ui#redisplay ())) ;
      | Pretty_source.PStmt(kf,({ skind=Instr(Call _) })) ->
	  if not (is_rte_precond kf) then
	    ignore (popup_factory#add_item "Insert callees contract (all calls)"
		      ~callback:(fun () -> !Db.RteGen.do_precond kf ; main_ui#redisplay ())) ;
      | _ -> ()
  end

let add_wp_menu
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) 
    localizable =
  let strategy = get_strategy localizable in
  let add_wp_run descr =
    ignore (popup_factory#add_item
              (Printf.sprintf "Prove %s by WP" descr)
              ~callback:(fun () -> run_and_prove main_ui strategy))
  in
  match strategy with
  | Snone -> ()
  | Scall _ -> add_wp_run "call preconditions"
  | Scode { sp_target = descr } -> add_wp_run descr

let wp_select
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points)
    ~button localizable =
  match button with
    | 3 -> (* Popup Menu: *)
        add_wp_menu  popup_factory main_ui localizable ;
	add_rte_menu popup_factory main_ui localizable ;
    | _ -> (* Other buttons... *) ()

(* ------------------------------------------------------------------------ *)
(* ---  WP Panel                                                        --- *)
(* ------------------------------------------------------------------------ *)

let wp_model_menu = [
  "Hoare" , "Hoare" ;
  "Store" , "Store" ;
  "Runtime" , "Runtime" ;
]

type p_select =
  | NoProver
  | Prover of string

let wp_prover_menu = [
  "None"     , NoProver ;
  "Alt-Ergo" , Prover "alt-ergo" ;
  "Coq"      , Prover "coqide" ;
  "Z3"       , Prover "z3" ;
  "Simplify" , Prover "simplify" ;
  "Vampire"  , Prover "vampire";
  "CVC3"     , Prover "cvc3" ;
  "Yices"    , Prover "yices" ;
  "Zenon"    , Prover "zenon" ;
]

let wp_prover_get () =
  match Wp_parameters.Prover.get () with
    | "alt-ergo" -> Prover "alt-ergo"
    | "coqide"   -> Prover "coqide"
    | "coq"      -> Wp_parameters.Prover.set "coqide" ; Prover "coqide"
    | "simplify" -> Prover "simplify"
    | "vampire"  -> Prover "vampire"
    | "z3"       -> Prover "z3"
    | "cvc3"     -> Prover "cvc3"
    | "yices"    -> Prover "yices"
    | "zenon"    -> Prover "zenon"
    | _ -> NoProver

let wp_dir = ref
  (try
     let home = Sys.getenv "HOME" in
     if Sys.file_exists home && Sys.is_directory home
     then home else raise Not_found
   with Not_found -> Sys.getcwd ())

let wp_script () =
  let file = GToolbox.select_file
    ~title:"Script File for Coq proofs"
    ~dir:wp_dir ~filename:"wp.script" ()
  in
  match file with
    | Some f -> Wp_parameters.Script.set f
    | None -> ()

let wp_prover_set = function
  | Prover p ->
      if p="coqide" && Wp_parameters.Script.get() = ""
      then wp_script () ;
      Wp_parameters.Prover.set p
  | NoProver ->
      Wp_parameters.Prover.set "none"

let wp_panel (main_ui:Design.main_window_extension_points) =
  let vbox = GPack.vbox () in
  let demon = Gtk_form.demon () in
  let packing = vbox#pack in
  let form = new Gtk_form.form ~packing in

  form#label "Model" ;
  Gtk_form.menu wp_model_menu
    ~tooltip:"Memory model selection" ~packing:form#item
    Wp_parameters.Model.get Wp_parameters.Model.set demon ;

  form#label "Prover" ;
  Gtk_form.menu wp_prover_menu
    ~tooltip:"Prover selection"
    ~packing:form#item
    wp_prover_get wp_prover_set demon ;

  let options = GPack.hbox ~spacing:8 ~packing () in

  Gtk_form.check ~label:"RTE"
    ~tooltip:"Generates RTE guards for WP"
    ~packing:options#pack
    Wp_parameters.RTE.get Wp_parameters.RTE.set demon ;

  Gtk_form.check ~label:"Split"
    ~tooltip:"Split cunjunctions into sub-goals"
    ~packing:options#pack
    Wp_parameters.Split.get Wp_parameters.Split.set
    demon ;

  Gtk_form.check ~label:"Invariants"
    ~tooltip:"Alternative WP for loop with arbitrary invariants"
    ~packing:options#pack
    Wp_parameters.Invariants.get Wp_parameters.Invariants.set demon ;

  Gtk_form.check ~label:"Trace"
    ~tooltip:"Report proof information from the provers"
    ~packing:options#pack
    Wp_parameters.ProofTrace.get Wp_parameters.ProofTrace.set demon ;

  let control = GPack.hbox ~packing () in

  Gtk_form.button ~label:"Scripts"
    ~tooltip:"Script file for saving Coq proofs"
    ~callback:wp_script ~packing:control#pack () ;

  Gtk_form.label ~text:"Timeout" ~packing:control#pack () ;
  Gtk_form.spinner ~lower:0 ~upper:100000
    ~tooltip:"Timeout for proving one proof obligation"
    ~packing:control#pack
    Wp_parameters.Timeout.get Wp_parameters.Timeout.set demon ;

  Gtk_form.label ~text:"Process" ~packing:control#pack () ;
  Gtk_form.spinner ~lower:1 ~upper:32
    ~tooltip:"Maximum number of parallel running provers"
    ~packing:control#pack
    Wp_parameters.Procs.get
    (fun n ->
       Wp_parameters.Procs.set n ;
       ignore (Prover.server ()) (* to make server procs updated is server exists *)
    ) demon ;

  let pbox = GPack.hbox ~packing ~show:false () in
  let progress = GRange.progress_bar ~packing:(pbox#pack ~expand:true ~fill:true) () in
  let cancel = GButton.button ~packing:(pbox#pack ~expand:false) ~stock:`STOP () in
  cancel#misc#set_sensitive false ;
  let server = Prover.server () in
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

           if s_term <> terminated
           then ( Po_navigator.refresh_status () ; main_ui#rehighlight () ) ;
           if s_sched <> scheduled || s_term <> terminated then
             begin
               progress#set_text (Printf.sprintf "%d / %d" terminated scheduled) ;
               progress#set_fraction
                 (if scheduled = 0 then 1.0 else (float terminated /. float scheduled)) ;
             end ;

           state := (terminated,remaining) ;
         end) ;
  "WP" , vbox#coerce , Some (Gtk_form.refresh demon)

(* ------------------------------------------------------------------------ *)
(* --- Registering WP GUI                                               --- *)
(* ------------------------------------------------------------------------ *)

let main main_ui =
  begin
    main_ui#register_source_highlighter wp_highlight ;
    main_ui#register_source_selector wp_select ;
    main_ui#register_panel wp_panel ;
  end

let () = Design.register_extension main

(* ------------------------------------------------------------------------ *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
