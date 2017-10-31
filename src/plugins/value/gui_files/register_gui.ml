(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
open Pretty_source
open Gui_types

type main_ui = Design.main_window_extension_points
type menu = GMenu.menu GMenu.factory

(* ------------------------ Eva panel and filetree -------------------------- *)

module UsedVarState =
  Cil_state_builder.Varinfo_hashtbl
    (Datatype.Bool)
    (struct
      let size = 17
      let name = "Value.Gui.UsedVarState"
      let dependencies = [ Db.Value.self ]
      (* [!Db.Inputs.self_external; !Db.Outputs.self_external] would be better
         dependencies, but this introduces a very problematic recursion between
         Value and Inout *)
    end)

let used_var = UsedVarState.memo
    (fun var ->
       Mark_noresults.no_memoization_enabled () ||
       try
         let f = fst (Globals.entry_point ()) in
         let inputs = !Db.Inputs.get_external f in
         let outputs = !Db.Outputs.get_external f in
         let b = Base.of_varinfo var in
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
            let strikethrough =
              Db.Value.is_computed () && not (!Db.Value.is_called kf)
            in
            filetree#set_global_attribute ~strikethrough vi
          with Not_found -> ());
     Globals.Vars.iter
       (fun vi _ ->
          if vi.vsource = true then
            filetree#set_global_attribute
              ~strikethrough:(Db.Value.is_computed () && not (used_var vi))
              vi
       );
     if not (filetree#flat_mode) then
       List.iter
         (fun file ->
            (* the display name removes the path *)
            let name, _globals = Globals.FileIndex.find file in
            let globals_state = filetree#get_file_globals name in
            filetree#set_file_attribute
              ~strikethrough:(Db.Value.is_computed () &&
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
  !hide_unused () && Db.Value.is_computed () &&
  (match g with
   | GFun ({svar = vi}, _) | GFunDecl (_, vi, _) ->
     let kf = Globals.Functions.get vi in
     not (!Db.Value.is_called kf)
   | GVarDecl (vi, _) | GVar (vi, _, _)  ->
     not (used_var vi)
   | _ -> false
  )

let value_panel (main_ui:main_ui) =
  let box = GPack.vbox () in
  let run_button = GButton.button ~label:"Run" ~packing:(box#pack) () in
  let w =
    GPack.table ~packing:(box#pack ~expand:true ~fill:true) ~columns:2 ()
  in
  let box_1_1 = GPack.hbox ~packing:(w#attach ~left:1 ~top:1) () in
  let slevel_refresh =
    let tooltip =
      Value_parameters.SemanticUnrollingLevel.parameter.Typed_parameter.help
    in
    Gtk_helper.on_int ~lower:0 ~upper:1000000 ~tooltip
      box_1_1 "slevel"
      Value_parameters.SemanticUnrollingLevel.get
      Value_parameters.SemanticUnrollingLevel.set
  in
  let box_1_2 = GPack.hbox ~packing:(w#attach ~left:1 ~top:2) () in
  let validator s =
    not
      (Kernel_function.Set.is_empty
         (Parameter_customize.get_c_ified_functions s))
  in
  let main_refresh = Gtk_helper.on_string
      ~tooltip:Kernel.MainFunction.parameter.Typed_parameter.help
      ~validator box_1_2 "main" Kernel.MainFunction.get Kernel.MainFunction.set
  in
  let refresh () = slevel_refresh (); main_refresh() in
  ignore (run_button#connect#pressed
            (fun () ->
               main_ui#protect ~cancelable:true
                 (fun () -> refresh (); !Db.Value.compute (); main_ui#reset ());
            ));
  "Value", box#coerce, Some refresh

(* ---------------------------- Highlighter --------------------------------- *)

let active_highlighter buffer localizable ~start ~stop =
  let open Gtk_helper in
  let buffer = buffer#buffer in
  (* highlight dead code areas, non-terminating calls, and degeneration
     points if Value has run.*)
  if Db.Value.is_computed () then
    match localizable with
    | PStmt (kf, stmt) -> begin
        let degenerate =
          try
            Some (
              if Value_util.DegenerationPoints.find stmt
              then (make_tag buffer ~name:"degeneration" [`BACKGROUND "orange"])
              else (make_tag buffer ~name:"unpropagated" [`BACKGROUND "yellow"])
            )
          with Not_found -> None
        in
        match degenerate with
        | Some color_area ->
          apply_tag buffer color_area start stop
        | None ->
          if Gui_eval.results_kf_computed kf then begin
            let csf = Gui_callstacks_filters.focused_callstacks () in
            if Gui_callstacks_filters.is_reachable_stmt csf stmt then begin
              if Gui_callstacks_filters.is_non_terminating_instr csf stmt then
                let non_terminating =
                  Gtk_helper.make_tag
                    buffer ~name:"value_non_terminating"
                    [`BACKGROUND "tomato"]
                in
                apply_tag buffer non_terminating (stop-1) stop
            end
            else
              let dead_code_area =
                make_tag buffer "deadcode" [`BACKGROUND "tomato";`STYLE `ITALIC]
              in
              apply_tag buffer dead_code_area start stop
          end
      end
    | _ -> ()

(* ------------------------ Responses to selections ------------------------- *)

let display_eval_errors (main_ui:main_ui) l =
  let pp = function
    | Eval_terms.LogicEvalError ee ->
      main_ui#pretty_information "Cannot evaluate: %a@."
        Eval_terms.pretty_logic_evaluation_error ee
    | e ->
      main_ui#pretty_information "Unknown error during evaluation (%s)@."
        (Printexc.to_string e)
  in
  List.iter pp l

let pretty_kf_escaped kf =
  Pretty_utils.(escape_underscores (to_string Kernel_function.pretty kf))

(* popup a menu to jump the definitions of the given functions *)
let menu_go_to_fun_definition (main_ui:main_ui) (popup_factory:menu) funs =
  let aux kf =
    try
      let g = Kernel_function.get_global kf in
      ignore
        (popup_factory#add_item
           ("Go to definition of " ^ pretty_kf_escaped kf ^ " (indirect)")
           ~callback:(fun () -> main_ui#select_or_display_global g))
    with Not_found -> ()
  in
  List.iter aux funs

let gui_compute_values (main_ui:main_ui) =
  if not (Db.Value.is_computed ())
  then main_ui#launcher ()

let cleaned_outputs kf s =
  let outs = Db.Outputs.kinstr (Kstmt s) in
  let accept =
    Callgraph.Uses.accept_base ~with_formals:true ~with_locals:true kf
  in
  let filter = Locations.Zone.filter_base accept in
  Extlib.opt_map filter outs

let pretty_stmt_info (main_ui:main_ui) kf stmt =
  (* Is it an accessible statement ? *)
  if Db.Value.is_reachable_stmt stmt then begin
    if Value_results.is_non_terminating_instr stmt then
      match stmt.skind with
      | Instr (Call (_, _, _, _)
              | Local_init (_, ConsInit _, _)) ->
        (* This is not 100% accurate: the instr can also fail
           when storing the result in [lvopt] *)
        main_ui#pretty_information "This call never terminates.@."
      | Instr _ ->
        main_ui#pretty_information "This instruction always fail.@."
      | _ -> ()
    else
      (* Out for this statement *)
      let outs = cleaned_outputs kf stmt in
      match outs with
      | Some outs ->
        main_ui#pretty_information
          "Modifies @[<hov>%a@]@." Db.Outputs.pretty outs
      | _ -> ()
  end
  else main_ui#pretty_information "This code is dead@."

type term_or_pred = Term | Pred

let pp_term_or_pred fmt = function
  | Term -> Format.pp_print_string fmt "term"
  | Pred -> Format.pp_print_string fmt "predicate"

let last_evaluate_acsl_request = ref ""

(* ------- Make the responses from the abstractions used in analysis  ------- *)

(** Responses of the GUI to user actions. Built by the Select functor. *)
module type Responses = sig
  val eval_acsl_term_pred: main_ui -> gui_loc -> term_or_pred -> unit -> unit
  val left_click_values_computed: main_ui -> localizable -> unit
  val right_click_values_computed: main_ui -> menu -> localizable -> unit
end

(** A "no response" module, when the GUI has not been built. *)
module No_Response = struct
  let eval_acsl_term_pred _ _ _ () = ()
  let left_click_values_computed _ _ = ()
  let right_click_values_computed _ _ _ = ()
end

(* Module argument of the Select functor: it is the module resulting
   from Gui_eval.A, plus the function display_at_loc coming from
   gui_callstacks_manager. *)
module type Eval = sig
  include Gui_eval.S
  val display_data_by_callstack:
    Analysis.Val.t Gui_callstacks_manager.display_data_by_callstack
end

(* Builds the responses of the GUI to user actions. *)
module Select (Eval: Eval) = struct

  let select_loc main_ui ev loc v =
    let data, errors = Eval.make_data_all_callstacks ev loc v in
    display_eval_errors main_ui errors;
    let selection = ev.Eval.expr_to_gui_selection v in
    Eval.display_data_by_callstack loc selection data

  let is_scalar typ =
    match Cil.unrollType typ with
    | TInt _ | TEnum _ | TPtr _ | TFloat _ -> true
    | _ -> false

  let select_lv main_ui loc lv =
    if is_scalar (Cil.typeOfLval lv)
    then select_loc main_ui Eval.lval_ev loc lv
    else select_loc main_ui Eval.lval_as_offsm_ev loc lv
  let select_null main_ui loc =
    select_loc main_ui Eval.null_ev loc ()
  let select_exp main_ui loc exp =
    select_loc main_ui Eval.exp_ev loc exp
  let select_term main_ui loc t =
    select_loc main_ui (Eval.term_ev loc) loc t
  let select_tlv main_ui loc tlv =
    select_loc main_ui (Eval.tlval_ev loc) loc tlv
  let select_predicate main_ui loc p =
    select_loc main_ui (Eval.predicate_ev loc) loc p

  (* Evaluate the user-supplied term contained in the string [txt] *)
  let eval_user_term_predicate (main_ui:main_ui) loc tp txt =
    let kf = kf_of_gui_loc loc in
    try
      Gui_callstacks_manager.focus_selection_tab ();
      let env = Gui_eval.gui_loc_logic_env loc in
      match tp with
      | Term -> begin
          if txt = "NULL" then
            select_null main_ui loc
          else
            let term = !Db.Properties.Interp.term ~env kf txt in
            match term.term_node with
            | TLval _ | TStartOf _ -> select_tlv main_ui loc term
            | _ -> select_term main_ui loc term
        end
      | Pred ->
        let pred = !Db.Properties.Interp.predicate ~env kf txt in
        select_predicate main_ui loc pred
    with
    | Logic_interp.Error (_, mess) ->
      main_ui#error "Invalid %a: %s" pp_term_or_pred tp mess
    | Parsing.Parse_error ->
      main_ui#error "Invalid %a: Parse error" pp_term_or_pred tp
    | Eval_terms.LogicEvalError ee ->
      main_ui#error "Cannot evaluate %a (%a)"
        pp_term_or_pred tp Eval_terms.pretty_logic_evaluation_error ee
    | Log.AbortFatal s when s = "kernel" ->
      let bt = Printexc.get_backtrace () in
      (* possibly a typing error, avoid an error message too drastic *)
      main_ui#error "Invalid %a (see the 'Console' tab for more details)."
        pp_term_or_pred tp;
      (* print the backtrace only if in debugging mode *)
      Gui_parameters.debug "%s" bt
    | e ->
      main_ui#error "Invalid %a: %s" pp_term_or_pred tp (Cmdline.protect e)

  (* Opens a modal dialog asking for an ACSL expression and evaluates it
     at location [loc]. *)
  let eval_acsl_term_pred main_ui loc tp () =
    let txt =
      Gtk_helper.input_string ~title:"Evaluate"
        ~text:!last_evaluate_acsl_request
        (Format.asprintf "  Enter an ACSL %a to evaluate  "
           pp_term_or_pred tp)
        (* the spaces at beginning and end should not be necessary
           but are the quickest fix for an aesthetic GTK problem *)
    in
    match txt with
    | None -> ()
    | Some txt ->
      last_evaluate_acsl_request:=txt;
      eval_user_term_predicate main_ui loc tp txt

  (* popup a menu to jump to the definitions of the callers *)
  let menu_go_to_callers (main_ui:main_ui) (menu:menu) csf kf =
    try
      let aux (menu:menu) (kf, call_sites) =
        let nb_sites = List.length call_sites in
        let label = "Go to caller " ^ pretty_kf_escaped kf in
        let label =
          if nb_sites > 1 then
            label ^ " (" ^ (string_of_int nb_sites) ^ " call sites)"
          else label
        in
        let callback () =
          let g = Kernel_function.get_global kf in
          main_ui#select_or_display_global g;
          (* We put the cursor in the first call site and add the others (if any)
             to the forward history. *)
          match call_sites with
          | first_call_site :: rest ->
            main_ui#view_stmt first_call_site;
            let other_call_sites =
              List.map (fun call ->
                  let kf = Kernel_function.find_englobing_kf call in
                  History.Localizable (PStmt (kf, call))
                ) rest
            in
            History.set_forward other_call_sites
          | [] -> assert false (* list was not empty *)
        in
        ignore (menu#add_item ~callback label)
      in
      let aux_focus (acc_focus, acc_unfocus) (kf, call_sites) =
        let focus, unfocus =
          List.partition (Gui_callstacks_filters.callsite_matches csf) call_sites
        in
        (if focus <> [] then (kf, focus) :: acc_focus else acc_focus),
        (if unfocus <> [] then (kf, unfocus) :: acc_unfocus else acc_unfocus)
      in
      let focused, unfocused =
        List.fold_left aux_focus ([], []) (!Db.Value.callers kf)
      in
      List.iter (aux menu) focused;
      if unfocused <> [] then
        let submenu = GMenu.menu () in
        let item =
          GMenu.menu_item ~label:"Callers in unselected callstack(s)" ()
        in
        item#set_submenu submenu;
        menu#menu#add item;
        let factory = new GMenu.factory submenu in
        List.iter (aux factory) unfocused
    with Not_found -> ()

  (* Actions to perform when the user has left-clicked, and Value is computed.
     Maintain synchronized with [can_eval_acsl_expr_selector] later in this file.*)
  let left_click_values_computed main_ui localizable =
    try
      let open Property in
      match localizable with
      | PStmt (kf,stmt) ->
        if Gui_eval.results_kf_computed kf then
          pretty_stmt_info main_ui kf stmt
      | PLval (Some kf, Kstmt stmt,lv) ->
        if not (Cil.isFunctionType (Cil.typeOfLval lv)) then
          select_lv main_ui (GL_Stmt (kf, stmt)) lv
      | PLval (Some kf, Kglobal, lv) -> (* see can_eval_acsl_expr_selector *)
        if not (Cil.isFunctionType (Cil.typeOfLval lv)) then
          select_lv main_ui (GL_Pre kf) lv
      | PExp (Some kf, Kstmt stmt,e) ->
        select_exp main_ui (GL_Stmt (kf, stmt)) e
      | PTermLval (Some kf, Kstmt stmt, _, tlv) ->
        let term = Logic_const.term (TLval tlv) (Cil.typeOfTermLval tlv) in
        select_tlv main_ui (GL_Stmt (kf, stmt)) term
      | PTermLval (Some kf, Kglobal, ip, tlv) -> begin
          match Gui_eval.classify_pre_post kf ip with
          | Some loc ->
            let term = Logic_const.term (TLval tlv) (Cil.typeOfTermLval tlv) in
            select_tlv main_ui loc term
          | None -> ()
        end
      | PVDecl (Some kf, _, vi) when vi.vformal ->
        let lv = (Var vi, NoOffset) in
        select_lv main_ui (GL_Pre kf) lv
      | PVDecl (Some kf, Kstmt stmt, vi) ->
        let lv = (Var vi, NoOffset) in
        select_lv main_ui (GL_Stmt (kf, stmt)) lv
      | PIP (IPCodeAnnot (kf, stmt,
                          {annot_content = AAssert (_, p) | AInvariant (_, true, p)} )) ->
        select_predicate main_ui (GL_Stmt (kf, stmt)) p
      | PIP (IPPredicate (_, kf, Kglobal, p) as ip) -> begin
          match Gui_eval.classify_pre_post kf ip with
          | None -> ()
          | Some loc ->
            select_predicate main_ui loc (Logic_const.pred_of_id_pred p)
        end
      | PLval (None , _, _)
      | PExp ((_,Kglobal,_) | (None, Kstmt _, _))
      | PTermLval (None, _, _, _)-> ()
      | PVDecl (_kf,_ki,_vi) -> ()
      | PGlobal _  | PIP _ -> ()
    with
    | Eval_terms.LogicEvalError ee ->
      main_ui#pretty_information "Cannot evaluate term: %a@."
        Eval_terms.pretty_logic_evaluation_error ee

  (* Actions to perform when the user has right-clicked, and Value is computed *)
  let right_click_values_computed main_ui menu localizable =
    match localizable with
    | PVDecl (Some kf, _, _) ->
      let filter = Gui_callstacks_filters.focused_callstacks () in
      menu_go_to_callers main_ui menu filter kf
    | PStmt (kf,stmt) ->
      if Gui_eval.results_kf_computed kf then
        ignore
          (menu#add_item "_Evaluate ACSL term"
             ~callback:(eval_acsl_term_pred main_ui (GL_Stmt (kf, stmt)) Term))
    | PLval (_kfopt, ki, lv) ->
      let ty = Cil.typeOfLval lv in
      (* Do special actions for functions *)
      begin
        (match lv with
         | Var _,NoOffset when Cil.isFunctionType ty ->
           () (* direct calls are handled by [Design]. *)
         | Mem _, NoOffset when Cil.isFunctionType ty -> begin
             (* Function pointers *)
             (* get the list of functions in the values *)
             let e = Value_util.lval_to_exp lv in
             match Eval.Analysis.get_kinstr_state ki with
             | `Bottom -> ()
             | `Value state ->
               let funs, _ = Eval.Analysis.eval_function_exp state e in
               match funs with
               | `Bottom -> ()
               | `Value funs ->
                 menu_go_to_fun_definition main_ui menu funs
           end
         | _ -> ()
        )
      end
    | PVDecl (None, _, _) | PExp _ | PTermLval _ | PGlobal _ | PIP _ -> ()

  let _right_click_value_not_computed (main_ui:main_ui) (menu:menu) localizable =
    match localizable with
    | PVDecl (_,_,_) -> begin
        ignore
          (menu#add_item "Compute callers"
             ~callback:(fun () -> (gui_compute_values main_ui)))
      end
    | _ -> ()

end

(* ----------------- Reference to responses, and use it  -------------------- *)

(* This reference contains the responses of the GUI built by the Select
   functor. It is updated each time the abstractions used in Eva are changed. *)
let responses_ref = ref (module No_Response: Responses)

let to_do_on_select (menu:menu) (main_ui:main_ui) ~button selected =
  let module Responses = (val !responses_ref) in
  if Db.Value.is_computed () then
    if button = 1 then
      Responses.left_click_values_computed main_ui selected
    else if button = 3 then
      Responses.right_click_values_computed main_ui menu selected

(* Find a location in which to evaluate things, when the given block is
   selected. *)
let find_loc kf fdec block =
  if block == fdec.sbody then
    Some (GL_Pre kf)
  else
    match block.bstmts with
    | [] -> None
    | s :: _ -> Some (GL_Stmt (kf, s))

let add_keybord_shortcut_evaluate main_ui =
  (* The currently selected statement is stored to enable a keyboard shortcut
     to activate it. [None] means that there is no selection or the selected
     element is not part of a statement. *)
  let selected_loc_for_acsl = ref None in
  (* This function must be maintained synchronized with
     [left_click_values_computed] above. *)
  let can_eval_acsl_expr_selector _menu _main ~button:_ selected =
    (* We add a selector to enable a keyboard shortcut for evaluating ACSL
       expressions. This selector listens to modification events and
       updates selected_loc_for_acsl to the stmt of the selected element. *)
    let clear () = Gui_callstacks_manager.clear_default () in
    let select new_loc =
      begin
        match new_loc, !selected_loc_for_acsl with
        | None, None -> ()
        | None, Some _ | Some _, None -> clear ()
        | Some new_loc, Some old_loc ->
          if not (gui_loc_equal new_loc old_loc) then clear ();
      end;
      selected_loc_for_acsl := new_loc
    in
    match selected with
    | PStmt (kf, stmt)
    | PLval (Some kf, Kstmt stmt, _)
    | PExp (Some kf, Kstmt stmt, _)
    | PTermLval (Some kf, Kstmt stmt, _, _) ->
      if Gui_eval.results_kf_computed kf
      then select (Some (GL_Stmt (kf, stmt)))
      else select None
    | PLval (Some kf, Kglobal, _) ->
      (* We are either on a formal, or on the declaration of the variables of
         [kf] at body scope. *)
      if Gui_eval.results_kf_computed kf
      then select (Some (GL_Pre kf))
      else select None
    | PTermLval (Some kf, Kglobal, ip, _) ->
      select (Gui_eval.classify_pre_post kf ip)
    | PVDecl (Some kf, _, vi) when vi.vformal ->
      select (Some (GL_Pre kf))
    | PVDecl (Some kf, ki, vi) when not (vi.vformal || vi.vglob) (* local *) ->
      begin
        match ki with
        | Kstmt stmt -> (* local with initializers *)
          select (Some (GL_Stmt (kf, stmt)))
        | Kglobal -> (* no initializer. Find the declaration block *)
          (* Notice that Pretty_source focuses on the statement containing the
             block itself most of the time. The case handled here happens only
             when you directly select the declaration of a variable, between
             the type and the name *)
          let fdec = Kernel_function.get_definition kf in
          let bl = Ast_info.block_of_local fdec vi in
          select (find_loc kf fdec bl)
      end
    | PIP (Property.IPCodeAnnot (kf, stmt,
                                 {annot_content = AAssert (_, _) | AInvariant (_, true, _)} )) ->
      select (Some (GL_Stmt (kf, stmt)))
    | PIP (Property.IPPredicate (_, kf, Kglobal, _) as ip) ->
      select (Gui_eval.classify_pre_post kf ip)
    | _ -> select None
  in
  main_ui#register_source_selector can_eval_acsl_expr_selector;
  (* We add a keyboard shortcut (Ctrl+E) to open the "Evaluate ACSL expression"
     popup. This only works if the current selection is on a statement,
     otherwise it does nothing. *)
  let accel_group = GtkData.AccelGroup.create () in
  let register_accel modi kind =
    GtkData.AccelGroup.connect accel_group
      ~key:GdkKeysyms._E ~modi
      ~callback:(fun _ ->
          match !selected_loc_for_acsl with
          | None -> ()
          | Some loc ->
            let module Responses = (val !responses_ref) in
            Responses.eval_acsl_term_pred main_ui loc kind ()
        );
  in
  register_accel [`CONTROL] Term;
  register_accel [`CONTROL; `SHIFT] Pred;
  main_ui#main_window#add_accel_group accel_group
;;

(* ----------------------------- Build the GUI ------------------------------ *)

(* Resets the GUI parts that depend on the abstractions used for the Eva
   analysis. This needs to be done each time the abstractions are changed.
   The module [A] is the current analysis module; it contains the
   abstractions used by Eva for the current analysis. *)
let reset (main_ui:main_ui) (module A: Analysis.S) =
  (* Types of the GUI depending on the abstractions used for the analysis. *)
  let module Gui_Types = Gui_types.Make (A.Val) in
  (* Evaluation functions for the GUI. *)
  let module Gui_Eval = Gui_eval.Make (A) in
  (* Mandatory: registers the functions that perform an evaluation by
     callstack. *)
  Gui_callstacks_filters.register_to_zone_functions (module Gui_Eval);
  (* Input module for building the callstack manager. *)
  let module Input = struct
    type value = A.Val.t
    include Gui_Types
    let make_data_for_lvalue lval loc =
      fst (Gui_Eval.make_data_all_callstacks Gui_Eval.lval_as_offsm_ev loc lval)
  end in
  (* Builds the "Values" panel on the lower notebook of the GUI. The resulting
     function is used to display data by callstacks on the user demand. *)
  let display_data_by_callstack =
    Gui_callstacks_manager.create main_ui (module Input)
  in
  (* Input module for builting the responses of the GUI. *)
  let module Eval : Eval = struct
    include Gui_Eval
    let display_data_by_callstack = display_data_by_callstack
  end in
  let module Responses = Select (Eval) in
  (* Stores the Responses module as a reference. *)
  responses_ref := (module Responses)

let main (main_ui:main_ui) =
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
  reset main_ui (Analysis.current_analyzer ());
  Analysis.register_hook (reset main_ui);
  Design.register_reset_extension (fun _ -> Gui_callstacks_manager.reset ());
  main_ui#register_source_selector (to_do_on_select );
  main_ui#register_source_highlighter active_highlighter;
  main_ui#register_panel value_panel;
  add_keybord_shortcut_evaluate main_ui;
;;

let () = Design.register_extension main
;;

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
