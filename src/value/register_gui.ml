(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Db
open Pretty_source
open Gtk_helper


type lval_or_absolute = TLVal of term | LVal of lval | AbsoluteMem

let pretty_lval_or_absolute fmt = function
  | TLVal tlv -> !Ast_printer.d_term fmt tlv
  | LVal lv -> !Ast_printer.d_lval fmt lv
  | AbsoluteMem -> Format.pp_print_string fmt "[MEMORY]"

let pretty_offsetmap lv fmt offsetmap =
  begin match offsetmap with
  | None ->  Format.fprintf fmt "<BOTTOM>"
  | Some off ->
      let typ = match lv with
        | LVal lv -> Some (typeOfLval lv)
        | TLVal tlv -> Some (Logic_utils.logicCType tlv.term_type)
        | AbsoluteMem -> None
      in
      Format.fprintf fmt "%a%a"
        pretty_lval_or_absolute lv
        (Cvalue.V_Offsetmap.pretty_typ typ) off
  end

(* special [with_alarms] value that log important alarms, but allow execution
   to continue *)
let log_alarms () =
  let ok = ref true in
  let not_ok () = ok := false in
  let with_alarms = {
    CilE.others = CilE.Acall not_ok;
    unspecified = CilE.Acall not_ok;
    defined_logic =       CilE.Aignore;
    imprecision_tracing = CilE.Aignore;
  } in
  with_alarms, ok

let pp_eval_ok fmt ok =
  if not ok then
    Format.fprintf fmt " (evaluation may have failed in some cases) "


let lval_or_absolute_to_offsetmap state lv =
  let with_alarms, ok = log_alarms () in
  let r = match lv with
    | LVal lv ->
        let loc = Eval_exprs.lval_to_loc ~with_alarms state lv in
        Cvalue.Model.copy_offsetmap ~with_alarms loc state
    | TLVal tlv ->
        let env = Eval_terms.env_annot ~pre:Cvalue.Model.top ~here:state in
        let loc = Eval_terms.eval_tlval_as_location env ~with_alarms None tlv in
        Cvalue.Model.copy_offsetmap ~with_alarms loc state
    | AbsoluteMem ->
        try Some (Cvalue.Model.find_base Base.null state)
        with Not_found -> None
  in
  r, !ok


let pretty_lval_or_absolute (main_ui: Design.main_window_extension_points) ki lva =
  (match ki with
    | Kstmt stmt -> Cil.CurrentLoc.set (Cil_datatype.Stmt.loc stmt)
    | Kglobal -> ());
  let pp fmt = main_ui#pretty_information fmt in
  begin (* State before ki *)
    let state = Value.get_state ki in
    let offsetmap, ok = lval_or_absolute_to_offsetmap state lva in
    pp "Before statement%a:@." pp_eval_ok ok;
    pp "%a@." (pretty_offsetmap lva) offsetmap;
  end;
  begin (* State after ki *)
    if Value_parameters.ResultsAfter.get () then
      match ki with
        | Kstmt ({ skind = Instr _} as stmt) ->
          let state =
            try Value.AfterTable.find stmt
            with Not_found -> Cvalue.Model.bottom
          in
          let offsetmap_after, ok = lval_or_absolute_to_offsetmap state lva in
          pp "After statement%a:@." pp_eval_ok ok;
          pp "%a@." (pretty_offsetmap lva) offsetmap_after;
        | Kglobal | Kstmt _ -> ()
    else
      try
        (match lva with
          | LVal lv ->
              let offsetmap_after = !Db.Value.lval_to_offsetmap_after ki lv in
              pp "At next statement:@.";
              pp "%a@." (pretty_offsetmap lva) offsetmap_after;
          | TLVal _ | AbsoluteMem ->
  	      () (*TODO, but this needs an equivalent to l_val_to_offsetmap
                   for the NULL base and for terms. *))
      with Not_found -> ()
  end


let gui_compute_values  (main_ui:Design.main_window_extension_points) =
  if not (Db.Value.is_computed ())
  then main_ui#launcher ()

let cleant_outputs kf s =
  let outs = Db.Outputs.kinstr (Kstmt s) in
  let filter = Locations.Zone.filter_base
    (Value_aux.accept_base ~with_formals:true ~with_locals:true kf) in
  Extlib.opt_map filter outs

(* Evaluate the user-supplied term contained in the string [txt] *)
let eval_user_term main_ui kf stmt txt =
  Cil.CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
  let ki = Kstmt stmt in
  try
    if txt = "[MEM]" then
      pretty_lval_or_absolute main_ui (Kstmt stmt) AbsoluteMem
    else
      let term = !Db.Properties.Interp.expr kf stmt txt in
      let env = Eval_terms.env_annot ~pre:(Db.Value.get_initial_state kf)
	~here:(Db.Value.get_stmt_state stmt)
      in
      begin match term.term_node with
        | TLval _ | TStartOf _ ->
            pretty_lval_or_absolute main_ui ki (TLVal term)
        | _ ->
            let with_alarms, ok = log_alarms () in
            let evaled =
              Eval_terms.eval_term ~with_alarms env None term in
	    let v = List.fold_left
	      (fun acc (_,v) -> Cvalue.V.join acc v)
	      Cvalue.V.bottom evaled
	    in
            main_ui#pretty_information
              "Before the selected statement, all the values \
              taken by the term %a are contained in %a%a@."
              !Ast_printer.d_term term Cvalue.V.pretty v pp_eval_ok (!ok)
      end
  with
    | Logic_interp.Error (_, mess) ->
        main_ui#error "Invalid expression: %s" mess
    | Parsing.Parse_error ->
        main_ui#error "Invalid term: Parse error"
    | Eval_terms.LogicEvalError ee ->
        main_ui#error "Cannot evaluate term (%a)"
          Eval_terms.pretty_logic_evaluation_error ee
    | e ->
        main_ui#error "Invalid expression: %s" (Cmdline.protect e)


let to_do_on_select
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) button_nb selected
    =
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
              match outs with
                | Some outs ->
                  main_ui#pretty_information
                    "Modifies @[<hov>%a@]@." Db.Outputs.pretty outs
                | _ -> ()
            else main_ui#pretty_information "This code is dead@.";
          end
          | PLval (_kf, ki,lv) ->
              if not (isFunctionType (typeOfLval lv)) then
                pretty_lval_or_absolute main_ui ki (LVal lv)
          | PTermLval (_kf, ki, tlv) ->
              let ltyp = Cil.typeOfTermLval tlv in
              let term = Logic_const.term (TLval tlv) ltyp in
              pretty_lval_or_absolute main_ui ki (TLVal term)
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
          then
            let eval_expr () =
              let txt =
                GToolbox.input_string ~title:"Evaluate"
                  "  Enter an ACSL expression to evaluate  "
                  (* the spaces at beginning and end should not be necessary
                     but are the quickest fix for an aesthetic GTK problem *)
              in
              match txt with
                | None -> ()
                | Some txt -> eval_user_term main_ui kf stmt txt
            in
            begin
              try
                ignore
                  (popup_factory#add_item "_Evaluate ACSL term"
                     ~callback:eval_expr)
              with Not_found -> ()
            end
          else
            ignore
              (popup_factory#add_item
                  "_Evaluate ACSL term ..."
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


let hide_unused_function_or_var g =
  !hide_unused () && Value.is_computed () &&
  (match g with
    | GFun ({svar = vi}, _)
    | GVarDecl (_, vi, _) ->
      (try
         let kf = Globals.Functions.get vi in
         not (!Value.is_called kf)
       with Not_found ->
         not (used_var vi))
    | _ -> false
  )

module DegeneratedHighlighted =
  State_builder.Option_ref
    (Pretty_source.Localizable)
    (struct
       let name = "Value_gui.DegeneratedHighlightedState"
       let dependencies = [ Ast.self ]
     end)

let value_panel (main_ui:Design.main_window_extension_points) =
  let box = GPack.vbox () in

  let run_button =
    GButton.button ~label:"Run" ~packing:(box#pack) ()
  in
  let w =
    GPack.table ~packing:(box#pack  ~expand:true ~fill:true) ~columns:2 () 
  in
  let box_1_1 = GPack.hbox ~packing:(w#attach ~left:1 ~top:1) () in
  let slevel_refresh = Gtk_helper.on_int ~lower:0 ~upper:1000000
    ~tooltip:(Pretty_utils.sfprintf "%s"
               Value_parameters.SemanticUnrollingLevel.parameter.Parameter.help)
    box_1_1
    "slevel"
     Value_parameters.SemanticUnrollingLevel.get
     Value_parameters.SemanticUnrollingLevel.set
  in
  let box_1_2 = GPack.hbox ~packing:(w#attach ~left:1 ~top:2) () in
  let main_refresh = Gtk_helper.on_string
    ~tooltip:(Pretty_utils.sfprintf "%s"
                Kernel.MainFunction.parameter.Parameter.help) 
    ~validator:(fun s->List.mem s (Kernel.MainFunction.get_possible_values ()))
    box_1_2
    "main"
    Kernel.MainFunction.get
    Kernel.MainFunction.set
  in

  let refresh () = slevel_refresh (); main_refresh()
  in
  ignore (run_button#connect#pressed 
	    (fun () ->
              main_ui#protect ~cancelable:true
                (fun () -> refresh (); !Db.Value.compute (); main_ui#reset ());
              ));
  "Value", box#coerce, Some refresh


let main (main_ui:Design.main_window_extension_points) =
  (* Hide unused functions and variables. Must be registered only once *)
  let hide, _filter_menu =
    main_ui#file_tree#add_global_filter
      ~text:"Analyzed by Value only"
      ~key:"value_hide_unused" hide_unused_function_or_var
  in
  hide_unused := hide;

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
         if Pretty_source.Localizable.equal localizable loc then
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
  main_ui#register_source_highlighter highlighter;
  main_ui#register_panel value_panel
  

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
