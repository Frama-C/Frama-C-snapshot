(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
  | Property.IPLemma _ -> "lemma"
  | Property.IPCodeAnnot _ -> "annotation"
  | Property.IPPredicate( Property.PKRequires _ , _ , Kglobal , _ ) -> "precondition for callers"
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
        end

    | Pretty_source.PVDecl (Some kf,{vglob=true}) ->
        Scode {
          sp_target = "function contract" ;
          sp_kf   = Some kf ;
          sp_bhv  = [] ;
          sp_ip   = None ;
        }

    | Pretty_source.PIP ip ->
	let bhvs = Extlib.may_map ~dft:[] 
	  (fun x -> [ x.b_name ]) 
	  (Property.get_behavior ip) 
	in
        Scode {
          sp_target = kind_of_property ip ;
          sp_kf = Property.get_kf ip ;
          sp_bhv = bhvs ;
          sp_ip = Some ip ;
        }

    | Pretty_source.PVDecl _
    | Pretty_source.PLval _ 
    | Pretty_source.PTermLval _
    | Pretty_source.PGlobal _ ->
        Snone

module Rte_generated =
  Kernel_function.Make_Table
    (Datatype.Unit)
    (struct
      let name = "Wp_gui.Rte_generated"
      let size = 7
      let dependencies = [ Ast.self ]
     end)

let run_and_prove (main_ui:Design.main_window_extension_points) strategy =
  try
    let rehighlight = ref main_ui#rehighlight in
    let kf = ref None in
    begin
      match strategy with
        | Snone -> raise Stop
        | Scode s ->
	    begin match main_ui#reactive_buffer,s.sp_ip with 
	      | Some b,Some p -> 
		  rehighlight:=
		    (fun () -> Design.Feedback.update b p)
	      | _ -> ()
	    end;
	    kf := s.sp_kf;
            Register.wp_compute s.sp_kf s.sp_bhv s.sp_ip
        | Scall s ->
	    kf := Some s.sc_caller;
            Register.wp_compute_call s.sc_callat
    end ;
    Po_navigator.refresh_panel () ; (* To display the "computing..." state *)
    if Wp_parameters.RTE.get () then 
      begin
	match !kf with
	  | Some kf when not(Rte_generated.mem kf) ->
	      (* Redisplay only if wp's rte are generated for 
		 the first time *)
	      Rte_generated.add kf ();
	      main_ui#redisplay ()
	  | _ ->
	      !rehighlight () ;
      end
    else 
      !rehighlight ()
  with Stop -> () (* the strategy is Snone *)

(* ------------------------------------------------------------------------ *)
(* ---  Source Callback                                                 --- *)
(* ------------------------------------------------------------------------ *)

let is_rte_generated kf =
  List.for_all (fun (_, _, lookup) -> lookup kf) (!Db.RteGen.get_all_status ())
  
let is_rte_precond kf =
  let _, _, lookup = !Db.RteGen.get_precond_status () in 
  lookup kf

let add_rte_menu
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) localizable =
  match localizable with
  | Pretty_source.PVDecl (Some kf,{vglob=true}) ->
    if not (is_rte_generated kf) then
      ignore 
	(popup_factory#add_item "Insert WP-safety guards"
	   ~callback:(fun () -> 
	     !Db.RteGen.do_all_rte kf; 
	     main_ui#redisplay ())) ;
    if not (is_rte_precond kf) then
      ignore 
	(popup_factory#add_item "Insert all callees contract"
	   ~callback:(fun () -> 
	     !Db.RteGen.do_precond kf; 
	     main_ui#redisplay ())) ;
  | Pretty_source.PStmt(kf,({ skind=Instr(Call _) })) ->
    if not (is_rte_precond kf) then
      ignore 
	(popup_factory#add_item "Insert callees contract (all calls)"
	   ~callback:(fun () -> 
	     !Db.RteGen.do_precond kf; 
	     main_ui#redisplay ())) ;
  | _ -> ()

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
  "Logic" , "Logic" ;
  "Runtime" , "Runtime" ;
  "Pure" , "Pure" ;
  "Typed" , "Typed" ;
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
  match Datatype.String.Set.elements 
    (Wp_parameters.Provers.get ()) 
  with
    | [] -> Prover "alt-ergo"
    | pname :: _ ->
	match pname with
	  | "alt-ergo" -> Prover "alt-ergo"
	  | "coqide"   -> Prover "coqide"
	  | "coq"      -> 
	      let u = Datatype.String.Set.singleton "coqide" in
	      Wp_parameters.Provers.set u ; Prover "coqide"
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

let wp_prover_set pi =
  let p = match pi with Prover p -> p | NoProver -> "none" in
    Wp_parameters.Provers.set (Datatype.String.Set.singleton p)

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
    ~tooltip:"Split conjunctions into sub-goals"
    ~packing:options#pack
    Wp_parameters.Split.get Wp_parameters.Split.set
    demon ;

  Gtk_form.check ~label:"Trace"
    ~tooltip:"Report proof information from the provers"
    ~packing:options#pack
    Wp_parameters.ProofTrace.get Wp_parameters.ProofTrace.set demon ;

  Gtk_form.check ~label:"Model"
    ~tooltip:"Report model information from the provers"
    ~packing:options#pack
    Wp_parameters.UnsatModel.get Wp_parameters.UnsatModel.set demon ;

  let options = GPack.hbox ~spacing:8 ~packing () in

  Gtk_form.check ~label:"Invariants"
    ~tooltip:"Alternative WP for loop with arbitrary invariants"
    ~packing:options#pack
    Wp_parameters.Invariants.get Wp_parameters.Invariants.set demon ;

  Gtk_form.check ~label:"References"
    ~tooltip:"Detection of by reference parameters"
    ~packing:options#pack
    Wp_parameters.RefVar.get Wp_parameters.RefVar.set demon ;

  Gtk_form.button ~label:"Scripts..."
    ~tooltip:"Script file for saving Coq proofs"
    ~callback:wp_script ~packing:options#pack () ;

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
  Task.on_server_stop server
    (fun () ->
       Po_navigator.refresh_status () ; 
       main_ui#rehighlight () ; (* refresh consolidation of status *)
    ) ;
  "WP" , vbox#coerce , Some (Gtk_form.refresh demon)

(* ------------------------------------------------------------------------ *)
(* --- Registering WP GUI                                               --- *)
(* ------------------------------------------------------------------------ *)

let main main_ui =
  begin
    main_ui#register_source_selector wp_select ;
    main_ui#register_panel wp_panel ;
  end

let () = 
  begin
    Design.register_extension main ;
    Design.register_reset_extension (fun _ -> Po_navigator.refresh_panel ()) ;
  end

(* ------------------------------------------------------------------------ *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
