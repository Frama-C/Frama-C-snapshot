(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Cil_types
open Cil
open CilE
open Format
open Db
open Pretty_source
open Gtk_helper


let select_kf tree_view kf =
  let vi = Kernel_function.get_vi kf in
  tree_view#select_global vi


type lval_or_absolute = LVal of lval | AbsoluteMem

let pretty_lval_or_absolute fmt = function
  | LVal lv -> !Ast_printer.d_lval fmt lv
  | AbsoluteMem -> Format.pp_print_string fmt "[MEMORY]"

let pretty_offsetmap lva fmt offsetmap =
  begin match offsetmap with
  | None ->  Format.fprintf fmt "<BOTTOM>"
  | Some off ->
      let typ = match lva with
        | LVal lv -> Some (typeOfLval lv)
        | AbsoluteMem -> None
      in
      Format.fprintf fmt "%a%a"
        pretty_lval_or_absolute lva
        (Cvalue.V_Offsetmap.pretty_typ typ) off
  end

let lval_or_absolute_to_offsetmap state lva =
  match lva with
    | LVal lv -> !Db.Value.lval_to_offsetmap_state state lv
    | AbsoluteMem ->
        try Some (Cvalue.Model.find_base Base.null state)
        with Not_found -> None

let pretty_lval_or_absolute (annot : GText.buffer) ki lva =
  begin (* State before ki *)
    annot#insert "Before statement:\n";
    let state = Value.get_state ki in
    try
      let offsetmap = lval_or_absolute_to_offsetmap state lva in
      annot#insert (Pretty_utils.sfprintf "%a@\n"
                      (pretty_offsetmap lva) offsetmap);
    with Lmap.Cannot_copy ->
      match lva with
        | LVal lv ->
            let value = !Db.Value.access ki lv in
            let inset_utf8 = Unicode.inset_string () in
            annot#insert (Pretty_utils.sfprintf "%a %s %a@\n"
                            !Ast_printer.d_lval lv
                            inset_utf8
                            Db.Value.pretty value)
        | AbsoluteMem -> annot#insert "<>"
  end;
  begin (* State after ki *)
    if Value_parameters.ResultsAfter.get () then
      match ki with
        | Kstmt ({ skind = Instr _} as stmt) ->
          let state =
            try Value.AfterTable.find stmt
            with Not_found -> Cvalue.Model.bottom
          in
          let offsetmap_after = lval_or_absolute_to_offsetmap state lva in
          annot#insert (Pretty_utils.sfprintf "After statement:\n%a\n"
                          (pretty_offsetmap lva) offsetmap_after);
        | Kglobal | Kstmt _ -> ()
    else
      try
        (match lva with
          | LVal lv ->
              let offsetmap_after = !Db.Value.lval_to_offsetmap_after ki lv in
              annot#insert (Pretty_utils.sfprintf "At next statement:@\n%a@\n"
                              (pretty_offsetmap lva) offsetmap_after)
          | AbsoluteMem -> ())
      with Not_found -> ()
  end

let gui_annot_action (main_ui:Design.main_window_extension_points) txt =
  let tag_style_italic = Gtk_helper.make_tag main_ui#annot_window#buffer ~name:"slicing:style italic" [`STYLE `ITALIC] in
  main_ui#annot_window#buffer#insert ~tags:[tag_style_italic] ((txt ()) ^ "\n")


let gui_compute_values  (main_ui:Design.main_window_extension_points) =
  if not (Db.Value.is_computed ())
  then main_ui#launcher ()

let cleant_outputs kf s =
  let outs = Db.Outputs.kinstr (Kstmt s) in
  let filter = Locations.Zone.filter_base
    (Db.accept_base ~with_formals:true ~with_locals:true kf) in
  Extlib.opt_map filter outs

let rec to_do_on_select
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) button_nb selected
    =
  let annot = main_ui#annot_window#buffer in
  if button_nb = 1 then
    begin
      if Db.Value.is_computed ()
      then begin
          match selected with
          | PStmt (kf,stmt) -> begin
            (* Is it an accessible statement ? *)
            if Db.Value.is_reachable_stmt stmt then
              (* Out for this statement *)
              let outs = cleant_outputs kf stmt in
              let n = ( match outs with
                | Some outs ->
                  Pretty_utils.sfprintf
                    "Modifies @[<hov>%a@]@\n" Db.Outputs.pretty outs
                | _ -> "\n");
              in annot#insert n
            else annot#insert "This code is dead\n";
          end
          | PLval (_kf, ki,lv) ->
              if not (isFunctionType (typeOfLval lv)) then
                pretty_lval_or_absolute annot ki (LVal lv)
          | PTermLval _ -> () (* JS: TODO (?) *)
          | PVDecl (_kf,_vi) -> ()
          | PGlobal _  | PIP _ -> ()
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
                      (fun (kf,call_sites) ->
                        let nb_sites = List.length call_sites in
                        let label = "Go to caller " ^
                          (Pretty_utils.escape_underscores
                              (Pretty_utils.sfprintf "%a"
                                 Kernel_function.pretty kf))
                        in
                        let label =
                          if nb_sites > 1 then
                            label ^ " (" ^ (string_of_int nb_sites)
                            ^ " call sites)"
                          else label
                        in
                        ignore
                          (popup_factory#add_item
                              label
                              ~callback:
                              (fun () -> main_ui#select_or_display_global
                                 (Kernel_function.get_global kf))))
                      l;
                  with Not_found -> ()
                in
                do_menu callers
              else
                ignore
                  (popup_factory#add_item
                      "Callers ..."
                      ~callback:
                      (fun () -> (gui_compute_values main_ui)))

            with Not_found ->
              ()
          end

      | PStmt (kf,stmt) ->
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
                | Some txt ->
                  try
                    if txt = "[MEM]" then
                      pretty_lval_or_absolute annot (Kstmt stmt) AbsoluteMem
                    else
                      let exp =
                        !Db.Properties.Interp.term_to_exp ~result:None
                          (!Db.Properties.Interp.expr kf stmt txt)
                      in
                      begin match exp.enode with
                      | Lval lv | StartOf lv ->
                          pretty_lval_or_absolute annot (Kstmt stmt) (LVal lv)
                      | _ ->
                          let loc = !Db.Value.access_expr (Kstmt stmt) exp in
                          let txt =
                            Format.sprintf
                              "Before the selected statement, all the values taken by the expression %s are contained in %s@\n"
                              (Pretty_utils.sfprintf "%a" !Ast_printer.d_exp exp)
                              (Pretty_utils.sfprintf "%a" Cvalue.V.pretty loc)
                          in
                          annot#insert txt
                      end
                  with
                    | Logic_interp.Error (_, mess) ->
                      main_ui#error "Invalid expression: %s" mess
                    | Parsing.Parse_error ->
                      main_ui#error "Invalid expression: %s" "Parse error"
                    | e ->
                      main_ui#error "Invalid expression: %s" (Cmdline.protect e)
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
                  "_Evaluate expression ..."
                  ~callback:
                  (fun () -> (gui_compute_values main_ui)))
      | PLval (_kf, ki, lv) ->
          if Db.Value.is_computed () then
          let ty = typeOfLval lv in
          (* Do special actions for functions *)
          begin
            (* popup a menu to jump the definitions of the given varinfos *)
            let do_menu l =
              match l with
              | [] -> ()
              | _ ->
                  List.iter
                    (fun v ->
                       try
                         let kf = Globals.Functions.get v in
                         let g = Kernel_function.get_global kf in
                         ignore
                           (popup_factory#add_item
                              ("Go to definition of " ^
                                 (Pretty_utils.escape_underscores
                                    (Pretty_utils.sfprintf "%a"
                                       Cil_datatype.Varinfo.pretty_vname v))
                               ^ " (indirect)")
                              ~callback:
                              (fun () ->
                                 main_ui#select_or_display_global g))
                       with Not_found -> ())
                    l;
            in
            (match lv with
            | Var _,NoOffset when isFunctionType ty ->
                (* simple literal calls are done by [Design]. *)
                ()
            | Mem ({ enode = Lval lv}), NoOffset  ->
                if isFunctionType ty then
                  (* Function pointers *)
                  begin try
                      (* get the list of exact bases in the values *)
                      let value,_exact =
                        Cvalue.V.find_exact_base_without_offset
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
                  end
            | _ -> ()
            )
          end
      | PTermLval _ -> () (* No C function calls in logic *)
      | PGlobal _ -> ()
      | PIP _ -> ()
    end

module UsedVarState =
  Cil_state_builder.Varinfo_hashtbl
    (Datatype.Bool)
    (struct
       let size = 17
       let name = "Value.Gui.UsedVarState"
       let dependencies = [ !Db.Inputs.self_external;
                            !Db.Outputs.self_external; ]
       let kind = `Internal
     end)

let no_memoization_enabled () =
  Value_parameters.NoResultsAll.get() ||
  Value_parameters.ObviouslyTerminatesAll.get() ||
  not (Value_parameters.NoResultsFunctions.is_empty ()) ||
  not (Value_parameters.ObviouslyTerminatesFunctions.is_empty ())


let used_var = UsedVarState.memo
  (fun var ->
    no_memoization_enabled () ||
     try
       let f = fst (Globals.entry_point ()) in
       let inputs = !Db.Inputs.get_external f in
       let outputs = !Db.Outputs.get_external f in
       let b = Base.create_varinfo var in
       Locations.Zone.mem_base b inputs || Locations.Zone.mem_base b outputs
     with e ->
       Gui_parameters.error ~once:true
         "Exception during usability analysis of var %s: %s"
         var.vname (Printexc.to_string e);
       true (* No really sane value, so in doubt... *)
  )


(* Set when the callback is installed *)
let hide_unused = ref (fun () -> false)

let sync_filetree (filetree:Filetree.t) =
  if not (!hide_unused ()) then
    (Globals.Functions.iter
       (fun kf ->
         try
           let vi = Kernel_function.get_vi kf in

           filetree#set_global_attribute
             ~strikethrough:(Value.is_computed () && not (!Value.is_called kf))
             vi
         with Not_found -> ());
     Globals.Vars.iter
       (fun vi _ ->
         if vi.vlogic = false then
           filetree#set_global_attribute
             ~strikethrough:(Value.is_computed () && not (used_var vi))
             vi
       );
     if not (filetree#flat_mode) then
       List.iter
         (fun file ->
         (* the display name removes the path *)
           let name, _globals = Globals.FileIndex.find file in
           let globals_state = filetree#get_file_globals name in
           filetree#set_file_attribute
             ~strikethrough:(Value.is_computed () &&
                               List.for_all snd globals_state)
             name
         )
         (Globals.FileIndex.get_files ())
    )
  else
   (* Some lines may have disappeared. We should reset the entire filetree,
      but the method reset of design.ml already does this. *)
    ()


let hide_unused_function_or_var vi =
  !hide_unused () && Value.is_computed () &&
  (try
     let kf = Globals.Functions.get vi in
     not (!Value.is_called kf)
   with Not_found ->
     not (used_var vi)
  )

module DegeneratedHighlighted =
  State_builder.Option_ref
    (Pretty_source.Localizable)
    (struct
       let name = "Value_gui.DegeneratedHighlightedState"
       let dependencies = [ Ast.self ]
       let kind = `Internal
     end)

let main (main_ui:Design.main_window_extension_points) =
  (* Hide unused functions and variables. Must be registered only once *)
  hide_unused :=
    main_ui#file_tree#add_global_filter
      ~text:"Hide unused according to\nvalue analysis"
      ~key:"value_hide_unused" hide_unused_function_or_var;

  main_ui#file_tree#register_reset_extension sync_filetree;

  (* Very first display, we need to do a few things by hand *)
  if !hide_unused () then
    main_ui#file_tree#reset ()
  else
    sync_filetree main_ui#file_tree;

  let value_selector
      menu (main_ui:Design.main_window_extension_points) ~button localizable =
    to_do_on_select
      menu
      main_ui
      button
      localizable
  in
  main_ui#register_source_selector value_selector;

  let highlighter (buffer:GSourceView2.source_buffer) localizable ~start ~stop =
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
      (DegeneratedHighlighted.get_option ());

    (* highlight dead code areas if values were computed.*)
    if Db.Value.is_computed () then
      let ki = ki_of_localizable localizable in
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
  main_ui#register_source_highlighter highlighter

let degeneration_occurred _ki _lv =
(*
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
                 let kf = Kernel_function.find_englobing_kf st in
                 select_kf app#file_tree kf;
                 PLval(Some kf,ki,lv)
             | Kstmt st, None ->
                 let kf = Kernel_function.find_englobing_kf st in
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
          DegeneratedHighlighted.set localizable;
          app#rehighlight ();
          app#scroll localizable
          (*match ki with
            | Kstmt st ->
            let l =  (Cil_datatype.Stmt.loc st.skind) in
            select_locs ~file:l.file ~line:l.line app#source_viewer
            | _ -> ()*);
             false(*do it once only*)));
  GMain.Main.main ();
*)
  ignore (raise Db.Value.Aborted)

let () =
  Design.register_extension main;
  Db.Value.degeneration_occurred := degeneration_occurred;
;;


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
