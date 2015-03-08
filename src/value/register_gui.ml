(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
open Pretty_source
open Gtk_helper

(* Catch the fact that we are in a function for which [-no-results] or one
   of its variants is set. Without this check, we would display 
   much non-sensical information. *)
let results_kf_computed = function
  | { fundec = Definition (fundec, _) } ->
    Mark_noresults.should_memorize_function fundec
  | { fundec = Declaration _ } -> true (* This value is not really used *)


(** Generic functions working for lvalues and expressions. *)

(* special [with_alarms] value that logs important alarms, but allows execution
   to continue *)
let log_alarms () =
  let ok = ref true in
  let not_ok () = ok := false in
  let with_alarms = {
    CilE.others = {CilE.a_ignore with CilE.a_call=not_ok};
    unspecified = {CilE.a_ignore with CilE.a_call=not_ok};
    defined_logic =       CilE.a_ignore;
    imprecision_tracing = CilE.a_ignore;
  } in
  with_alarms, ok

let pp_eval_ok fmt ok =
  if not ok then
    Format.fprintf fmt " (evaluation may have failed in some cases) "

type ('states, 'expr, 'v) evaluation_functions = {
  eval_and_warn: 'states -> 'expr -> 'v * bool;
  equal: 'v -> 'v -> bool;
  bottom: 'v;
  join: 'v -> 'v -> 'v;
  pretty: 'expr -> Format.formatter -> 'v -> unit
}

let pretty_before_after ev (main_ui: Design.main_window) ~before ~after expr =
  let pp fmt = main_ui#pretty_information fmt in
  try
    let vbefore, okbef = ev.eval_and_warn before expr in
    let res_after = Extlib.opt_map (fun a -> ev.eval_and_warn a expr) after in
    let pretty = ev.pretty expr in
    match res_after with
      | Some (vafter, okafter)
          when ev.equal vbefore vafter ->
        pp "Before this statement / after this statement%a:@. %a@."
          pp_eval_ok (okbef && okafter) pretty vbefore
      | Some (vafter, okafter) ->
        pp "Before this statement%a:@. %a@."
          pp_eval_ok okbef pretty vbefore;
        pp "After this statement%a:@. %a@."
          pp_eval_ok okafter pretty vafter
      | None ->
        pp "Before this statement%a:@. %a@."
          pp_eval_ok okbef pretty vbefore;
  with Eval_terms.LogicEvalError ee ->
    Value_parameters.debug "Cannot evaluate term (%a)"
      Eval_terms.pretty_logic_evaluation_error ee

let pretty_per_callstacks ev (main_ui: Design.main_window) ~before ~after v =
  let pp_info fmt = main_ui#pretty_information fmt in
  let aux callstack before =
    let after =
      Extlib.opt_map
        (fun cafter ->
          try Value_types.Callstack.Hashtbl.find cafter callstack
          with Not_found -> Cvalue.Model.bottom)
        after
    in
    pp_info "@.For callstack [%a]@." Value_util.pretty_call_stack callstack;
    pretty_before_after ev main_ui ~before ~after v
  in
  (* TODO: we should sort the callstacks by prefix *)
  Value_types.Callstack.Hashtbl.iter aux before

let callstack_fold ev h a =
  let f _callstack state (acc_res, acc_w) =
    let res, w = ev.eval_and_warn state a in
    ev.join res acc_res, w || acc_w
  in
  Value_types.Callstack.Hashtbl.fold f h (ev.bottom, false)

let pretty_callstack_summary ev main_ui ~before ~after v =
  let ev = { ev with eval_and_warn = callstack_fold ev } in
  pretty_before_after ev main_ui ~before ~after v

let pretty_at_stmt ev main_ui stmt v =
  (* Display 'after' states only in instructions. On blocks and if/switch
     statements, the notion of 'after' is counter-intuitive. *)
  let is_instr = match stmt.skind with Instr _ -> true | _ -> false in
  (* Standard printing, without callstacks *)
  let before =
    match Db.Value.get_stmt_state_callstack ~after:false stmt with
    | None -> Value_types.Callstack.Hashtbl.create 1
    | Some x -> x
  in
  let after =
    if is_instr
    then Db.Value.get_stmt_state_callstack ~after:true stmt
    else None
  in
  if Value_types.Callstack.Hashtbl.length before > 1
  then pretty_callstack_summary ev main_ui ~before ~after v;
  pretty_per_callstacks ev main_ui ~before ~after v

(** lvalues-related functions *)

type lval_or_absolute = TLVal of term | LVal of lval | AbsoluteMem

let pretty_lval_or_absolute fmt = function
  | TLVal tlv -> Printer.pp_term fmt tlv
  | LVal lv -> Printer.pp_lval fmt lv
  | AbsoluteMem -> Format.pp_print_string fmt "[MEMORY]"

type offsetmap_result =
  | Bottom (* Bottom memory state *)
  | Top (* State or size was Top *)
  | InvalidLoc (* Location is always invalid *)
  | Offsetmap of Cvalue.V_Offsetmap.t (* Normal result *)

let equal_offsetmap_result r1 r2 = match r1, r2 with
  | Bottom, Bottom -> true
  | Top, Top -> true
  | InvalidLoc, InvalidLoc -> true
  | Offsetmap o1, Offsetmap o2 -> Cvalue.V_Offsetmap.equal o1 o2
  | (Bottom | Top | InvalidLoc | Offsetmap _), _ -> false

let join_offsetmap_result r1 r2 = match r1, r2 with
  | Top, _ | _, Top -> Top
  | (Bottom | InvalidLoc), x | x, (Bottom | InvalidLoc) -> x
  | Offsetmap o1, Offsetmap o2 -> Offsetmap (Cvalue.V_Offsetmap.join o1 o2)

(* Display [o] as a single value, when this is more readable and more precise
   than the standard display. *)
let pretty_stitched_offsetmap fmt typ o =
  if Cil.isArithmeticOrPointerType typ &&
    not (Cvalue.V_Offsetmap.is_single_interval o)
  then
    let v =
      Eval_op.v_uninit_of_offsetmap ~with_alarms:CilE.warn_none_mode ~typ o
    in
    if not (Cvalue.V_Or_Uninitialized.is_isotropic v)
    then
      Format.fprintf fmt "@\nThis amounts to: %a"
        Cvalue.V_Or_Uninitialized.pretty v

let pretty_offsetmap_result lv fmt r =
  begin match r with
  | Bottom ->  Format.pp_print_string fmt "<BOTTOM>"
  | InvalidLoc -> Format.pp_print_string fmt "<INVALID LOCATION>"
  | Top -> Format.pp_print_string fmt "<NO INFORMATION>"
  | Offsetmap off ->
      let typ = match lv with
        | LVal lv -> Some (Cil.unrollType (typeOfLval lv))
        | TLVal tlv ->
          Some (Cil.unrollType (Logic_utils.logicCType tlv.term_type))
        | AbsoluteMem -> None
      in
      pretty_lval_or_absolute fmt lv;
      Cvalue.V_Offsetmap.pretty_generic ?typ () fmt off;
      match typ with
      | None -> ()
      | Some typ -> pretty_stitched_offsetmap fmt typ off
  end

let lval_or_absolute_to_offsetmap state lv =
  let with_alarms, ok = log_alarms () in
  (* Evaluate the given location in [state]. Catch an unreachable state, an
     invalid location, or another error during the evaluation. *)
  let reduce_loc_and_eval loc =
    if Cvalue.Model.is_top state then
      Top, true
    else if Cvalue.Model.is_reachable state then
      let loc' = Locations.valid_part ~for_writing:false loc in
      if Locations.is_bottom_loc loc' then
        InvalidLoc, false
      else
        try
          match Eval_op.copy_offsetmap ~with_alarms
           loc'.Locations.loc (Int_Base.project loc'.Locations.size) state
          with
          | `Bottom -> Bottom, false
          | `Top -> Top, true
          | `Map offsm ->
            let ok = !ok && (Locations.loc_equal loc loc') in
            Offsetmap offsm, ok
        with Int_Base.Error_Top -> Top, true
    else
      Bottom, true
  in
  match lv with
    | LVal lv ->
        let ploc = Eval_exprs.lval_to_precise_loc ~with_alarms state lv in
        let aux loc (acc_res, acc_ok) =
          let res, ok = reduce_loc_and_eval loc in
          match acc_res, res with
            | Offsetmap o1, Offsetmap o2 ->
              Offsetmap (Cvalue.V_Offsetmap.join o1 o2), acc_ok && ok
            | Bottom, v | v, Bottom -> v, acc_ok && ok
            | Top, Top -> Top, acc_ok && ok
            | InvalidLoc, InvalidLoc -> InvalidLoc, false
            | InvalidLoc, Offsetmap _ -> res, false
            | Offsetmap _, InvalidLoc -> acc_res, false
            | Top, (InvalidLoc | Offsetmap _ as r)
            | (InvalidLoc | Offsetmap _ as r), Top ->
              r, acc_ok && ok (* cannot happen, we should get Top everywhere *)
        in
        Precise_locs.fold aux ploc (Bottom, true)
    | TLVal tlv ->
        let env = Eval_terms.env_annot ~pre:Cvalue.Model.top ~here:state () in
        let loc = Eval_terms.eval_tlval_as_location env ~with_alarms tlv in
        reduce_loc_and_eval loc
    | AbsoluteMem ->
      match Cvalue.Model.find_base_or_default Base.null state with
      | `Bottom -> InvalidLoc, true
      | `Top -> Top, true
      | `Map m -> Offsetmap m, true

let lval_ev = 
  {eval_and_warn=lval_or_absolute_to_offsetmap;
   equal=equal_offsetmap_result;
   bottom=Bottom;
   pretty=pretty_offsetmap_result;
   join=join_offsetmap_result}

let pretty_lva_at_stmt = pretty_at_stmt lval_ev

(** Expressions-related functions *)

let eval_exp_and_warn state e =
  let with_alarms,ok = log_alarms () in
  Eval_exprs.eval_expr ~with_alarms state e,
  !ok

let pretty_exp_result e fmt v =
  Format.fprintf fmt
    "%a @[%s %a@]"
    Printer.pp_exp e
    (Unicode.inset_string ())
    (Cvalue.V.pretty_typ (Some (Cil.typeOf e))) v

let exp_ev =     
  {eval_and_warn=eval_exp_and_warn;
   equal=Cvalue.V.equal;
   bottom=Cvalue.V.bottom;
   pretty=pretty_exp_result;
   join=Cvalue.V.join}

let pretty_exp_at_stmt = pretty_at_stmt exp_ev 

(** Special case for formals *)
let pretty_formal_initial_state
    (main_ui: Design.main_window_extension_points) lvoa state =
  (* Callstack information not available yet *)
  let offsm,_ = lval_or_absolute_to_offsetmap state lvoa in
  let pp fmt = main_ui#pretty_information fmt in
  pp "Initial value (before preconditions):@.%a@."
    (pretty_offsetmap_result lvoa) offsm

(** Core of the graphical interface. *)

let gui_compute_values  (main_ui:Design.main_window_extension_points) =
  if not (Db.Value.is_computed ())
  then main_ui#launcher ()

let cleant_outputs kf s =
  let outs = Db.Outputs.kinstr (Kstmt s) in
  let accept =
    !Db.Semantic_Callgraph.accept_base ~with_formals:true ~with_locals:true kf
  in
  let filter = Locations.Zone.filter_base accept in
  Extlib.opt_map filter outs

module C_labels =
  State_builder.Ref(Datatype.Option(Cil_datatype.Logic_label.Map.Make(Cvalue.Model)))
    (struct
      let name = "GUI.c_labels"
      let dependencies = [ Db.Value.Table_By_Callstack.self ]
      let default () = None
     end)

let c_labels () =
  match C_labels.get () with
  | Some h -> h
  | None ->
    let h =
      Db.Value.Table_By_Callstack.fold
        (fun stmt _ acc ->
          if stmt.labels != []
          then Cil_datatype.Logic_label.Map.add
            (StmtLabel (ref stmt))
            (Db.Value.get_stmt_state stmt) acc
          else acc)
        Cil_datatype.Logic_label.Map.empty
    in
    C_labels.set (Some h);
    h

(* Evaluate the user-supplied term contained in the string [txt] *)
let eval_user_term main_ui kf stmt txt =
  Cil.CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
  try
    if txt = "[MEM]" then
      pretty_lva_at_stmt main_ui stmt AbsoluteMem
    else
      let term = !Db.Properties.Interp.expr kf stmt txt in
      let pre = Db.Value.get_initial_state kf in
      let here = Db.Value.get_stmt_state stmt in
      let env = Eval_terms.env_annot ~c_labels:(c_labels ()) ~pre ~here ()
      in
      begin match term.term_node with
        | TLval _ | TStartOf _ ->
            pretty_lva_at_stmt main_ui stmt (TLVal term)
        | _ ->
            let with_alarms, ok = log_alarms () in
            let evaled = Eval_terms.eval_term ~with_alarms env term in
            let v = evaled.Eval_terms.eover in
            let typ = match Logic_utils.unroll_type term.term_type with
              | Ctype typ -> Some typ
              | _ -> None
            in
            main_ui#pretty_information
              "Before the selected statement, all the values \
              taken by the term %a are contained in %a%a@."
              Printer.pp_term term (Cvalue.V.pretty_typ typ) v pp_eval_ok (!ok)
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

let last_evaluate_acsl_request = ref ""

let to_do_on_select
    (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) button_nb selected
    =
  if button_nb = 1 then
    begin
      if Db.Value.is_computed () then begin
        try
          match selected with
          | PStmt (kf,stmt) -> begin
            if results_kf_computed kf then begin
              (* Is it an accessible statement ? *)
              if Db.Value.is_reachable_stmt stmt then begin
                if Value_results.is_non_terminating_instr stmt then
                  match stmt.skind with
                  | Instr (Call (_lvopt, _, _, _)) ->
                    (* This is not 100% accurate: the instr can also fail
                       when storing the result in [lvopt] *)
                    main_ui#pretty_information "This call never terminates.@."
                  | Instr _ ->
                    main_ui#pretty_information "This instruction always fail.@."
                  | _ -> ()
                else
                  (* Out for this statement *)
                  let outs = cleant_outputs kf stmt in
                  match outs with
                  | Some outs ->
                    main_ui#pretty_information
                      "Modifies @[<hov>%a@]@." Db.Outputs.pretty outs
                  | _ -> ()
              end
              else main_ui#pretty_information "This code is dead@.";
            end
          end
          | PLval (_kf, Kstmt stmt,lv) ->
              if Db.Value.is_reachable_stmt stmt &&
                not (isFunctionType (typeOfLval lv))
              then pretty_lva_at_stmt main_ui stmt (LVal lv)
          | PExp (_kf, Kstmt stmt,e) ->
              if Db.Value.is_reachable_stmt stmt
              then
                pretty_exp_at_stmt main_ui stmt e
          | PTermLval (_kf, Kstmt stmt, tlv) ->
              if Db.Value.is_reachable_stmt stmt then
                let ltyp = Cil.typeOfTermLval tlv in
                let term = Logic_const.term (TLval tlv) ltyp in
                pretty_lva_at_stmt main_ui stmt (TLVal term)
          | PTermLval (Some kf, Kglobal, tlv) ->
              let state = Db.Value.get_initial_state kf in
              if Cvalue.Model.is_reachable state then
                let ltyp = Cil.typeOfTermLval tlv in
                let term = Logic_const.term (TLval tlv) ltyp in
                let lvoa = TLVal term in
                pretty_formal_initial_state main_ui lvoa state
          | PVDecl (Some kf, vi) when vi.vformal ->
              let state = Db.Value.get_initial_state kf in
              if Cvalue.Model.is_reachable state then
                let lvoa = LVal (Var vi, NoOffset) in
                pretty_formal_initial_state main_ui lvoa state
          | PExp (_,Kglobal,_)| PLval (_, Kglobal, _)
          | PTermLval (None, Kglobal, _)-> ()
          | PVDecl (_kf,_vi) -> ()
          | PGlobal _  | PIP _ -> ()
        with
        | Eval_terms.LogicEvalError ee ->
          main_ui#pretty_information "Cannot evaluate term: %a@."
            Eval_terms.pretty_logic_evaluation_error ee
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
                let callers = !Db.Value.callers kfun in
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
          if Db.Value.is_computed () && results_kf_computed kf
          then
            let eval_expr () =
              let txt =
                GToolbox.input_string ~title:"Evaluate"
		  ~text:!last_evaluate_acsl_request
                  "  Enter an ACSL expression to evaluate  "
                  (* the spaces at beginning and end should not be necessary
                     but are the quickest fix for an aesthetic GTK problem *)
              in
              match txt with
                | None -> ()
                | Some txt -> 
		  last_evaluate_acsl_request:=txt;
		  eval_user_term main_ui kf stmt txt
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
            let do_menu funs =
              if not (Kernel_function.Hptset.is_empty funs) then
                Kernel_function.Hptset.iter
                  (fun kf ->
                     try
                       let g = Kernel_function.get_global kf in
                       ignore
                         (popup_factory#add_item
                            ("Go to definition of " ^
                               (Pretty_utils.escape_underscores
                                  (Pretty_utils.sfprintf "%a"
                                     Kernel_function.pretty kf))
                             ^ " (indirect)")
                            ~callback:
                            (fun () ->
                               main_ui#select_or_display_global g))
                     with Not_found -> ())
                  funs;
            in
            (match lv with
            | Var _,NoOffset when isFunctionType ty ->
                (* simple literal calls are done by [Design]. *)
                ()
            | Mem _, NoOffset when isFunctionType ty ->
                  (* Function pointers *)
                  begin
                    (* get the list of functions in the values *)
                    let e = Cil.dummy_exp (Lval lv) in
                    let funs, _ = Eval_exprs.resolv_func_vinfo
                      ~with_alarms:CilE.warn_none_mode None
                      (Db.Value.get_state ki) e
                    in
                    do_menu funs
                  end
            | _ -> ()
            )
          end
      | PExp _ -> () (* No C function only in exp *)
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
    | GFun ({svar = vi}, _)
    | GVarDecl (_, vi, _) ->
      (try
         let kf = Globals.Functions.get vi in
         not (!Db.Value.is_called kf)
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
    GPack.table ~packing:(box#pack ~expand:true ~fill:true) ~columns:2 ()
  in
  let box_1_1 = GPack.hbox ~packing:(w#attach ~left:1 ~top:1) () in
  let slevel_refresh =
    let tooltip =
      Pretty_utils.sfprintf "%s"
        Value_parameters.SemanticUnrollingLevel.parameter.Typed_parameter.help
    in
    Gtk_helper.on_int ~lower:0 ~upper:1000000 ~tooltip
      box_1_1
      "slevel"
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
    ~tooltip:(Pretty_utils.sfprintf "%s"
                Kernel.MainFunction.parameter.Typed_parameter.help)
    ~validator
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
          if results_kf_computed kf then begin
            if Db.Value.is_reachable_stmt stmt then begin
              if Value_results.is_non_terminating_instr stmt then
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
  in
  main_ui#register_source_highlighter highlighter;
  main_ui#register_panel value_panel


let () = Design.register_extension main
;;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
