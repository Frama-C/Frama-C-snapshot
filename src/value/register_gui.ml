(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(*i $Id: register_gui.ml,v 1.11 2008/10/23 07:59:52 uid527 Exp $ i*)

open Cil_types
open Cil
open CilE
open Format
open Db
open Db_types
open Pretty_source
open Gtk_helper


let select_kf tree_view kf =
  let vi = Kernel_function.get_vi kf in
  tree_view#select_global vi

let pretty_offsetmap lv fmt offsetmap =
  begin match offsetmap with
  | None ->  Format.fprintf fmt "<BOTTOM>"
  | Some off ->
      Format.fprintf fmt "%a%a"   
	!Ast_printer.d_lval lv
	(Cvalue_type.V_Offsetmap.pretty_typ
	    (Some (typeOfLval lv)))
	off
  end

let gui_annot_action (main_ui:Design.main_window_extension_points) txt =
  let tag_style_italic = Gtk_helper.make_tag main_ui#annot_window#buffer ~name:"slicing:style italic" [`STYLE `ITALIC] in
  main_ui#annot_window#buffer#insert ~tags:[tag_style_italic] ((txt ()) ^ "\n")


let gui_compute_values what (main_ui:Design.main_window_extension_points) =
  if not (Db.Value.is_computed ()) then
    begin
      gui_annot_action main_ui 
	(fun () -> "Activating " ^ what ^ " by running value analysis first");
      let already_locked = main_ui#lock () in
        !Db.Value.compute ();
        main_ui#unlock already_locked
    end

let rec to_do_on_select
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) button_nb selected
    =
  let inset_utf8 = Unicode.inset_string () in
  
  let annot = main_ui#annot_window#buffer in
  if button_nb = 1 then
    begin
      if Db.Value.is_computed () 
      then begin 
	  match selected with
	  | PStmt (_kf,ki) -> begin
		(* Find kinstr and kf *)
		try
		  (* Is it an accessible statement ? *)
		  if Db.Value.is_accessible (Kstmt ki) then begin
		      (* Out for this statement *)
		      let outs = Db.Outputs.kinstr (Kstmt ki) in
		      let n = ( match outs with
                      | Some outs ->
                          fprintf_to_string "Modifies %a@\n"
                            Db.Outputs.pretty outs
                      | _ -> "\n");
		      in annot#insert n
		    end else annot#insert "This code is dead\n";

		with e ->
		  main_ui#error 
		    "error in Db.KernelFunction.find :%s. Please report"
		    (Printexc.to_string e)
            end
	  | PLval (_kf, ki,lv) ->
              if not (isFunctionType (typeOfLval lv))
              then begin
		  (try
		      let offsetmap =
			!Db.Value.lval_to_offsetmap ~with_alarms:CilE.warn_none_mode ki lv
		      in
		      annot#insert (fprintf_to_string
				       "Before statement:@\n%a@\n"
				       (pretty_offsetmap lv ) offsetmap);
		    with Lmap.Cannot_copy ->
		      let value = !Db.Value.access ki lv in
		      annot#insert (fprintf_to_string "Before statement:@\n%a %s %a@\n" 	
				       !Ast_printer.d_lval lv inset_utf8 Db.Value.pretty value));
		  (try
		      let offsetmap_after = !Db.Value.lval_to_offsetmap_after ki lv in
		      annot#insert "At next statement:\n";
		      annot#insert (fprintf_to_string "%a\n" (pretty_offsetmap lv)  offsetmap_after);
		    with Not_found -> ());
		end
	  | PTermLval _ -> () (* JS: TODO (?) *)
	  | PVDecl (_kf,_vi) -> ()
	end
    end
  else if button_nb = 3
  then begin
      match selected with
      | PVDecl (_,vi) -> 
	  begin
	    try
	      let kfun = Globals.Functions.get vi in
	      if Db.Value.is_computed () 
	      then
		let callers = !Value.callers kfun in
		(* popup a menu to jump to the definitions of the callers *)
		let do_menu l =
		  try
		    List.iter
		      (fun (v,call_sites) ->
			let v = Kernel_function.get_vi v in
			let nb_sites = List.length call_sites in
			let label = "Go to caller " ^
			  (Pretty_utils.escape_underscores
			      (fprintf_to_string "%a"
				  Ast_info.pretty_vname v))
			in
			let label = 
			  if nb_sites > 1 
			  then
			    label ^ " (" ^ (string_of_int nb_sites) ^" call sites)"
			  else label
			in
			ignore
			  (popup_factory#add_item
			      label
			      ~callback:
			      (fun () -> main_ui#file_tree#select_global v)))
		      l;	     
		  with Not_found -> ()
		in
		do_menu callers
	      else
		ignore
		  (popup_factory#add_item
		      "Activate callers"
		      ~callback:
		      (fun () -> (gui_compute_values "callers" main_ui)))
		  
	    with Not_found ->
	      ()
          end
	    
      | PStmt (kf,ki) ->
	  if Db.Value.is_computed () 
	  then begin
              let eval_expr () =
		let txt =
		  GToolbox.input_string 
                    ~title:"Evaluate"
                    "  Enter an ACSL expression to evaluate  "
		    (* the spaces at beginning and end should not be necessary
		       but are the quickest fix for an aesthetic GTK problem *)
		in
		match txt with
		| None -> ()
		| Some txt -> try
	              let exp =
			!Db.Properties.Interp.term_to_exp
			  (!Db.Properties.Interp.expr kf ki txt)
	              in
                      begin match exp with
                      | Lval lv | StartOf lv ->
			  to_do_on_select popup_factory main_ui 1 (PLval((Some kf),Kstmt ki,lv))
                      | _ ->
			  let loc =
			    !Db.Value.access_expr
                              (Kstmt ki)
                              exp
			  in
			  let txt =
			    Format.sprintf
                              "Before the selected statement, all the values taken by the expression %s are contained in %s@\n"
		              (fprintf_to_string "%a" !Ast_printer.d_exp exp)
                              (fprintf_to_string "%a" Cvalue_type.V.pretty loc)
			  in
			  annot#insert txt
                      end
		  with e ->
	            main_ui#error "Invalid expression: %s" (Printexc.to_string e)
              in
              begin
		try
		  ignore
		    (popup_factory#add_item "_Evaluate expression"
			~callback:eval_expr)
		with Not_found -> ()
              end
	    end
	  else
	    ignore
	      (popup_factory#add_item
		  "Activate Evaluate"
		  ~callback:
		  (fun () -> (gui_compute_values "Activate _Evaluate" main_ui)))
      | PLval (_kf, ki, lv) ->
          let ty = typeOfLval lv in
          (* Do special actions for functions *)
          begin
	    (* popup a menu to jump the definitions of the given varinfos *)
	    let do_menu l =
	      match l with
	      | [] -> ()
	      | _ ->
                  try
		    List.iter
		      (fun v ->
                        ignore
                          (popup_factory#add_item
			      ("Go to definition of " ^
                                  (Pretty_utils.escape_underscores
				      (fprintf_to_string "%a"
					  Ast_info.pretty_vname v))
				^ " (indirect)")
			      ~callback:
			      (fun () ->
                                main_ui#file_tree#select_global v)))
		      l ;
                  with Not_found -> ()
	    in
	    (match lv with
	    | Var _,NoOffset when isFunctionType ty ->
                (* simple literal calls are done by [Design]. *)
                ()
	    | _  ->
                if isFunctionType ty then
                  (* Function pointers *)
	          begin try
		      (* get the list of exact bases in the values *)
		      let value,_exact =
			Cvalue_type.V.find_exact_base_without_offset
                          (!Db.Value.access ki lv)
		      in
		      let functions =
			List.fold_left
                          (fun acc ->
			    (function
			      | Base.Var (vi,_) -> vi::acc
			      | _ -> acc))
                          []
                          value
		      in
		      do_menu functions

		    with Not_found -> ()
                  end)
          end
      | PTermLval _ -> () (* No C function calls in logic *)

    end


let reset (main_ui:Design.main_window_extension_points) =
    Globals.Functions.iter
      (fun kf ->
         try
           let vi = Kernel_function.get_vi kf in
           main_ui#file_tree#set_global_attribute
             ~strikethrough:(Value.is_computed () && not (!Value.is_called kf))
             vi
         with Not_found -> ())

module DegeneratedHighlighted =
  Computation.Ref
    (struct
       include Datatype.Option(Pretty_source.Localizable_Datatype)
       let default () = None
     end)
    (struct
       let name = "Value_gui.DegeneratedHighlightedState"
       let dependencies = [ Cil_state.self ]
     end)

let launcher () = 
  let module Hook = Hook.Make(struct end) in
  let enabled = ref (Cmdline.ForceValues.get ()) in
  let w = GPack.vbox () in
  Hook.extend (on_bool w "Enable" 
                 Cmdline.ForceValues.get 
                 (fun b -> 
                    Cmdline.ForceValues.set b;
                    enabled := b));

  (** Entry point chooser *)
  Hook.extend begin
    let all_function_names = 
      List.sort
        Pervasives.compare
        (Globals.Functions.fold 
	   (fun kf acc -> (Kernel_function.get_name kf)::acc)
	   [])
    in
    let get_entrypoint () = 
      try 
        let kf, _ = Globals.entry_point () in
        Kernel_function.get_name kf
      with Globals.No_such_entry_point _ -> 
        ""
    in
    let valid_entrypoint r = 
      if r <> "" then 
        try 
          ignore (Globals.Functions.find_def_by_name r);
          true
        with Not_found -> 
          Format.printf "%s is not a function definition.@." r;
          false
      else true
    in
    on_string_completion 
      ~validator:valid_entrypoint
      all_function_names
      w 
      "Entry point"
      get_entrypoint
      Cmdline.MainFunction.set
  end;

  Hook.extend (on_bool_radio w "Library" "Full application"
                 Cmdline.LibEntry.get 
                 Cmdline.LibEntry.set);

  
  (*****************************************************************************)
  let context =
    let expander = GBin.expander ~label:"Initial Context" ~packing:w#pack () in
    let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
    GPack.vbox ~packing:frame#add ()
  in
  Hook.extend (on_int context "Maximal depth for chained pointers of the initial context"
                 Cmdline.AutomaticContextMaxDepth.get
                 Cmdline.AutomaticContextMaxDepth.set);
  Hook.extend (on_int context "Maximal width for pointers of the initial context"
                 Cmdline.AutomaticContextMaxWidth.get
                 Cmdline.AutomaticContextMaxWidth.set);
  Hook.extend (on_bool context "Initial context contains only valid pointers"
                 Cmdline.AllocatedContextValid.get
                 Cmdline.AllocatedContextValid.set);

  (*****************************************************************************)
  let platform =
    let expander = GBin.expander ~label:"Platform" ~packing:w#pack () in
    let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
    GPack.vbox ~packing:frame#add ()
  in
  Hook.extend (on_bool platform "Pretend integers cannot overflow"
                 Cmdline.IgnoreOverflow.get
                 Cmdline.IgnoreOverflow.set);

  Hook.extend (on_bool platform "Do not assume array accesses inside structures are valid"
                 Cmdline.UnsafeArrays.get
                 Cmdline.UnsafeArrays.set);
  let valid_int64 s = 
    try 
      Scanf.sscanf s "%Li%!" (fun _ -> ());
      true
    with End_of_file | Scanf.Scan_failure _ | Failure _ -> false
  in
  Hook.extend (on_string ~validator:valid_int64 
                 platform 
                 "Minimal valid absolute address in bytes" 
                 (fun () -> 
                    Format.sprintf
                      "%Li"
                      (Abstract_interp.Int.to_int64 
                         (Abstract_interp.Int.div 
                            (Cmdline.MinValidAbsoluteAddress.get ())
                            Abstract_interp.Int.eight)))
                 (fun s -> 
                    try 
                      Scanf.sscanf s "%Li"
                        (fun v ->
                           Cmdline.MinValidAbsoluteAddress.set
                             (Abstract_interp.Int.mul 
                                Abstract_interp.Int.eight
                                (Abstract_interp.Int.of_int64 v)))
                    with End_of_file | Scanf.Scan_failure _ | Failure _ -> assert false));

  Hook.extend (on_string ~validator:valid_int64 
                 platform  
                 "Maximal valid absolute address in bytes" 
                 (fun () -> 
                    Format.sprintf
                      "%Li"
                      (Abstract_interp.Int.to_int64 
                         (Abstract_interp.Int.pred
                            (Abstract_interp.Int.div 
                               (Abstract_interp.Int.succ (Cmdline.MaxValidAbsoluteAddress.get ()))
                               (Abstract_interp.Int.of_int 8)))))
                 (fun s -> 
                    try 
                      Scanf.sscanf s "%Li"
                        (fun v ->
                           Cmdline.MaxValidAbsoluteAddress.set
                             (Abstract_interp.Int.pred
                                (Abstract_interp.Int.mul 
                                   (Abstract_interp.Int.of_int 8) 
                                   (Abstract_interp.Int.succ 
                                      (Abstract_interp.Int.of_int64 v)))))
                    with End_of_file | Scanf.Scan_failure _ | Failure _ -> assert false));


  (*****************************************************************************)
  let tune =
    let expander = GBin.expander ~label:"Precision tuning" ~packing:w#pack () in
    let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
    GPack.vbox ~packing:frame#add ()
  in
  Hook.extend (on_int tune "Semantic branch unrolling"
                 Cmdline.SemanticUnrollingLevel.get
                 Cmdline.SemanticUnrollingLevel.set);
  Hook.extend (on_int tune "Precision level for arrays" 
                 Cmdline.ArrayPrecisionLevel.get
                 Cmdline.ArrayPrecisionLevel.set);
  Hook.extend (on_int tune "Threshold for widening" 
                 Cmdline.WideningLevel.get
                 Cmdline.WideningLevel.set);
  Hook.extend (on_bool tune "Continue even after degeneration"
                 Cmdline.PropagateTop.get
                 Cmdline.PropagateTop.set);

  let compute () = 
    if !enabled then !Db.Value.compute ()
  in
  "Value",Some w#coerce,Some Hook.apply,Some compute

let main (main_ui:Design.main_window_extension_points) =

  (* reset attributes for the list of source files *)
  reset main_ui;

  let value_selector
      menu (main_ui:Design.main_window_extension_points) ~button localizable =
    to_do_on_select
      menu
      main_ui
      button
      localizable
  in
  main_ui#register_source_selector value_selector;

  let highlighter (buffer:GSourceView.source_buffer) localizable ~start ~stop =
    (* highlight the degeneration point *)
    Extlib.may 
      (fun loc -> 
         if Cilutil.equals localizable loc then 
           let orange_area = make_tag 
             buffer 
             ~name:"degeneration" 
             [`BACKGROUND "orange" ] 
           in
           apply_tag buffer orange_area start stop)
      (DegeneratedHighlighted.get ());
    
    (* highlight dead code areas if values were computed.*)
    if Db.Value.is_computed () then
      let ki = match localizable with
      | PStmt (_,stmt) -> Kstmt stmt
      | PLval (_,ki,_) | PTermLval(_,ki,_) -> ki
      | PVDecl _ -> Kglobal
      in
      if not (Value.is_accessible ki) then
        let dead_code_area =
          make_tag
            buffer
            "deadcode"
            [`BACKGROUND "tomato";
             `STRIKETHROUGH true;
             `STYLE `ITALIC;]
        in
        apply_tag buffer dead_code_area start stop
  in
  main_ui#register_source_highlighter highlighter;

  let _ = main_ui#source_viewer#misc#modify_font main_ui#monospace in
  Source_manager.set_font
    main_ui#original_source_viewer
    main_ui#monospace;

  let filetree_selector ~was_activated ~activating globals = 
    if Value.is_computed () then begin 
      if not was_activated && activating then begin match globals with
      | [GFun ({svar=v},_)] -> 
          begin try 
            let kf = Globals.Functions.get v in
            let s = fprintf_to_string "@[%a@]" Value.display kf in
            main_ui#annot_window#buffer#insert s
          with Not_found -> () 
          end
      | _ -> ();
      end;
    end
  in
  main_ui#file_tree#add_select_function filetree_selector;

  main_ui#register_launcher launcher


let degeneration_occurred ki lv =
  Cilutil.flush_all ();
  Db.Value.mark_as_computed ();
  ignore (GtkMain.Main.init ());
  let app = new Design.main_window () in
  app#main_window#set_title "Degeneration Occurred";
  ignore
    (Glib.Idle.add ~prio:1000
       (fun () ->
          let localizable =
	    (match ki,lv with
             | Kstmt st, Some lv ->
                 let kf = snd (Kernel_function.find_from_sid st.sid) in
                 select_kf app#file_tree kf;
                 PLval(Some kf,ki,lv)
             | Kstmt st, None ->
                 let kf = snd (Kernel_function.find_from_sid st.sid) in
                 select_kf app#file_tree kf;
                 PStmt(kf,st)
             | Kglobal, Some lv ->
                 PLval(None,ki,lv)
             | Kglobal, None -> assert false)
          in
          to_do_on_select
            (new GMenu.factory (GMenu.menu ()))
            app
            1
	    localizable;
          DegeneratedHighlighted.set (Some localizable);
          app#rehighlight ();
          app#scroll localizable
          (*match ki with
	    | Kstmt st ->
            let l =  (get_stmtLoc st.skind) in
            select_locs ~file:l.file ~line:l.line app#source_viewer
	    | _ -> ()*);
             false(*do it once only*)));
  GMain.Main.main ();
  ignore (raise Db.Value.Aborted)

let () =
  Design.register_extension main;
  Design.register_reset_extension reset;
  Db.Value.degeneration_occurred := degeneration_occurred

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
    
