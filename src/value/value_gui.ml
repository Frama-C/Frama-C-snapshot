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

(*i $Id: value_gui.ml,v 1.36 2008/04/18 09:36:06 uid528 Exp $ i*)

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

let rec to_do_on_select
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) button_nb selected
    =
    let inset_utf8 = Unicode.inset_string () in

  if Db.Value.is_computed () then
    let annot = main_ui#annot_window#buffer in
    if  button_nb = 1 then
      begin match selected with
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
	    main_ui#info 
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
    else if button_nb = 3 then begin
      match selected with
      | PVDecl (_,vi) -> begin
	  try
            let kfun = Globals.Functions.get vi in
            let callers = !Value.callers kfun in
            (* popup a menu to jump to the definitions of the callers *)
            let do_menu l =
              match l with
              | [] -> ()
              | _ ->
                  try
                    List.iter
                      (fun (v,call_sites) ->
			 let v = Kernel_function.get_vi v in
			 ignore
                           (popup_factory#add_item
                              ("Go to caller " ^
				 (Pretty_utils.escape_underscores
				    (fprintf_to_string "%a"
				       Ast_info.pretty_vname v)) ^
				 let nb_sites = List.length call_sites in
				 if nb_sites > 1 then
                                   " (" ^ (string_of_int nb_sites) ^" call sites)"
				 else "")
                              ~callback:
                              (fun () -> main_ui#file_tree#select_global v)))
                      l;
                  with Not_found -> ()
            in
            do_menu callers
	  with Not_found ->
	    ()
        end

      | PStmt (kf,ki) ->
          let eval_expr () =
            let txt =
              GToolbox.input_string
                ~title:"Input a pure C expression to evaluate before current statement"
                ""
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
                        "Before the statement, all the values taken by the expression %s are contained in %s@\n"
		        (fprintf_to_string "%a" !Ast_printer.d_exp exp)
                        (fprintf_to_string "%a" Cvalue_type.V.pretty loc)
                    in
                    annot#insert txt
                end
	      with e ->
	        main_ui#info "invalid expression: %s" (Printexc.to_string e)
          in
          begin
            try
              ignore
		(popup_factory#add_item "_Evaluate expression"
		   ~callback:eval_expr)
            with Not_found -> ()
          end
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

  let highlighter (buffer:GText.buffer) localizable ~start ~stop =
    (* highlight dead code areas if values were computed *)
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
  main_ui#file_tree#add_select_function filetree_selector

let degeneration_occurred ki lv =
  Cilutil.flush_all ();
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
          let orange_area = make_tag app#source_viewer#buffer ~name:"degeneration" [`BACKGROUND "orange" ] in
          to_do_on_select
            (new GMenu.factory (GMenu.menu ()))
            app
            1
	    localizable;
          app#highlight ~scroll:true orange_area localizable;
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
