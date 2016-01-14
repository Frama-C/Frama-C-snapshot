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

open Design
open Cil_types
open Property_status

(* Collect all properties that have a status *)
let all_properties () =
  let globals = ref Property.Set.empty in
  let functions = ref Kernel_function.Map.empty in
  (* Dispatch properties into globals and per-function map *)
  Property_status.iter
    (fun ip ->
       match Property.get_kf ip with
	 | None -> globals := Property.Set.add ip !globals
	 | Some kf ->
	     if not (Ast_info.is_frama_c_builtin (Kernel_function.get_name kf))
	     then try
 	       let fips = Kernel_function.Map.find kf !functions in
	       fips := Property.Set.add ip !fips
	     with Not_found ->
	       let ips = Property.Set.singleton ip in
	       functions := Kernel_function.Map.add kf (ref ips) !functions
      );
  !functions, !globals


type property = {
  module_name:string;
  function_name:string;
  kind:string;
  status_name:string;
  consolidated_status:Consolidation.consolidated_status option;
  consolidated_status_name:string;
  status_icon:Gtk_helper.Icon.kind;
  visible:bool;
  ip: Property.t;
}

let kf_name_and_module kf =
  let name = Kernel_function.get_name kf in
  let loc = Kernel_function.get_location kf in
  let file = Filename.basename (fst loc).Lexing.pos_fname in
  name, file

let make_property ip =
  let status = Property_status.get ip in
  let status_name = Pretty_utils.sfprintf "%a" Property_status.pretty status in
  let con_status = Consolidation.get ip in
  let consolidated_status_name =
    Pretty_utils.sfprintf "%a" Consolidation.pretty con_status
  in
  let function_name, module_name = match Property.get_kf ip with
    | None -> "", "" (* TODO: it would be great to find the location
                        of global invariants or lemmas, but there isn't
                        enough information in the ast *)
    | Some kf -> kf_name_and_module kf
  in
  let kind =
    Pretty_utils.sfprintf "@[<hov>%a@]" Property.pretty ip
  in
  let status_icon = Gtk_helper.Icon.Feedback (Feedback.get ip) in
  {
    module_name = module_name;
    function_name = function_name;
    visible = true;
    ip=ip; kind=kind;
    status_name = status_name ;
    consolidated_status = Some con_status ;
    consolidated_status_name = consolidated_status_name ;
    status_icon = status_icon ;
  }

module Refreshers: sig

  type check = {
    id: int (* used to compare checks *);
    add: GPack.box -> unit;
    get: unit -> bool;
    set: bool -> unit;
    reset: unit -> unit (* change to default state if needed *);
  }

  val onlyCurrent: check

  val ensures: check
  val preconditions: check
  val behaviors: check
  val complete_disjoint: check
  val allocations: check
  val assigns: check
  val from: check
  val assertions: check
  val invariant: check
  val variant: check
  val terminates: check
  val stmtSpec: check
  val reachable: check
  val other: check
  val instances: check
  val lemmas: check
  val axiomatic: check
  val typeInvariants: check
  val globalInvariants: check

  val rteNotGenerated: check
  val rteGenerated: check

  val valid: check
  val validHyp: check
  val unknown: check
  val invalid: check
  val invalidHyp: check
  val considered_valid: check
  val untried: check
  val dead: check
  val inconsistent: check

  val active_alarm: Alarms.t -> bool

  val pack: GPack.box -> unit
  val set_refresh_needed : (bool -> unit) ref
  val apply: unit -> unit

  val all_checks : unit -> check list
end
=
struct
  (* Function to be called during the idle time of the GUI *)
  let refreshers = ref []
  let add_refresher f = refreshers := f::!refreshers
  let set_refresh_needed = ref (fun _ -> (*will be defined later*)())
  let apply () =
    List.iter (fun f -> f ()) !refreshers

  type check = {
    id: int (* unique ID, used to compare checks *);
    add: GPack.box -> unit (* pack the corresponding checkbox in the argument*);
    get: unit -> bool (* state of the checkbox (set/unset *) ;
    set: bool -> unit (* change checkbox state *) ;
    reset: unit -> unit (* change to default state if needed *);
  }

  let last_id = ref 0
  let next_id () = incr last_id; !last_id
  let checks : check list ref = ref []
  let all_checks () = List.rev !checks

  (* ref below used by [add] to set the configuration, since it must be delayed
     until the session directory has been set *)
  let first_extended_ref = ref true

  (* This function must always be called at OCaml toplevel, because it registers
     a new Frama-C state.
     [resettable] indicates whether it is affected by the "reset checkboxes"
     menu. *)
  let add ~name ~hint ?(default=true) ?(set=(fun _b -> ())) ?(resettable=true) () =
    let open Gtk_helper in
    let key_name =
      let s = String.copy name in
      for i = 0 to String.length s - 1 do
        let c = s.[i] in
        if c < 'A' || c > 'z' || (c > 'Z' && c < 'a') then
          s.[i] <- '_'
      done;
      "property_panel." ^ s
    in
    let module M = State_builder.Ref
      (Datatype.Bool)
      (struct
        let name = "show " ^ name
        let dependencies = []
        let default () = default
      end)
    in
    let get = M.get in
    let () =
      Cmdline.run_after_extended_stage
        (fun () ->
           (* avoid loading the configuration file several times *)
           if !first_extended_ref then begin
             first_extended_ref := false;
             Configuration.load ()
           end);
      Cmdline.run_after_loading_stage
        (fun () ->
           let v = Configuration.find_bool ~default key_name in
           M.set v)
    in
    let set v =
      Configuration.set key_name (Configuration.ConfBool v);
      set v;
      !set_refresh_needed true;
      M.set v
    in
    let add hb = add_refresher
        (Gtk_helper.on_bool ~tooltip:hint hb name get set)
    in
    let reset () = if get () <> default then set default in
    let chk = { id = next_id(); get; set; add; reset } in
    if resettable then checks := chk :: !checks;
    chk

  let onlyCurrent = add ~name:"Current function" ~resettable:false
      ~hint:"Only show properties related to current function" ()

  let preconditions = add ~name:"Preconditions"
      ~hint:"Show function preconditions" ()
  let ensures = add ~name:"Postconditions"
      ~hint:"Show function postconditions" ()
  let behaviors = add ~name:"Behaviors" ~default:false
      ~hint:"Show function behaviors" ()
  let complete_disjoint = add ~name:"Complete/disjoint"
      ~hint:"Show complete/disjoint behaviors" ()
  let allocations = add ~name:"Allocations"
      ~hint:"Show function allocations" ()
  let assigns = add ~name:"Assigns"
      ~hint:"Show function assigns" ()
  let from = add ~name:"From" ()
      ~hint:"Show functional dependencies in function assigns"
  (* Function called when assertions are enabled or disabled. *)
  let set_assertions = ref (fun _b -> ())
  let assertions = add ~set:(fun b -> !set_assertions b) ~name:"Assert"
      ~hint:"Show assertions" ()
  let invariant = add ~name:"Invariant"
      ~hint:"Show loop invariants" ()
  let variant = add ~name:"Variant"
      ~hint:"Show loop termination argument" ()
  let terminates = add ~name:"Terminates"
      ~hint:"Show function termination clauses" ()
  let stmtSpec = add ~name:"Stmt contract"
      ~hint:"Show statement contracts" ()
  let lemmas = add ~name:"Lemmas"
      ~hint:"Show lemmas" ()
  let axiomatic = add ~name:"Axiomatic" ~default:false
      ~hint:"Show global axiomatics" ()
  let instances = add ~name:"Instances"
      ~hint:"Show properties that are instances of root properties" ()
  let typeInvariants = add ~name:"Type invariants"
      ~hint:"Show type invariants" ()
  let globalInvariants = add ~name:"Global invariants"
      ~hint:"Show global invariants" ()
  let other = add ~name:"Other"
      ~hint:"Show other properties" ()
  let reachable = add ~default:false ~name:"Reachable"
      ~hint:"Show 'reachable' hypotheses" ()

  let rteNotGenerated = add ~default:false ~name:"Non generated"
      ~hint:"Show RTEs assertions that remain to generate" ()
  let rteGenerated = add ~default:false ~name:"Generated"
      ~hint:"Show RTEs assertions that have been generated" ()

  let valid = add ~name:"Valid"
      ~hint:"Show properties that are proven valid" ()
  let validHyp = add ~name:"Valid under hyp."
      ~hint:"Show properties that are are valid, but depend on some hypotheses"
      ()
  let unknown = add ~name:"Unknown"
      ~hint:"Show properties with an 'unknown' status" ()
  let invalid = add ~name:"Invalid"
      ~hint:"Show properties that are proven invalid" ()
  let invalidHyp = add ~name:"Invalid under hyp."
      ~hint:"Show properties that are are invalid, but depend on \
             some hypotheses" ()
  let considered_valid = add ~name:"Considered valid" ~default:false
      ~hint:"Show properties that are considered valid because \
             the platform has no way to prove them" ()
  let untried = add ~name:"Untried" ~default:false
      ~hint:"Show properties whose proof has not been attempted" ()
  let dead = add ~name:"Dead" ~default:false
      ~hint:"Show properties on unreachable code" ()
  let inconsistent = add ~name:"Inconsistent"
      ~hint:"Show properties that have an inconsistent status" ()

  let make_expand (box:GPack.box) ?tooltip text =
    let key_config = "Properties." ^ text in
    let expanded =Gtk_helper.Configuration.find_bool ~default:true key_config in
    let expander = GBin.expander ~expanded ~packing:box#pack () in
    ignore (expander#connect#activate
              (fun () -> (* Save expansion of panels*)
                 Gtk_helper.Configuration.set key_config
                  (Gtk_helper.Configuration.ConfBool (not expander#expanded))));
    let hb = GPack.vbox ~packing:expander#add () in
    let markup = Printf.sprintf "<span font_weight=\"bold\">%s</span>" text in
    let label = GMisc.label ~markup () in
    Gtk_helper.do_tooltip ?tooltip label;
    expander#set_label_widget label#coerce;
    hb, expander

  (* [list_alarms] is the instantiation of [add] for all the various kind
     of alarms. It is computed by ad hoc introspection on the reprs field of the
     datatype. [active_alarm] finds the category of the alarm, and returns
     whether it should be shown according to the corresponding checkbox. *)
  let list_alarms, active_alarm =
    (*[h] maps alarms hints to the corresponding [get] checkbox. *)
    let h = Datatype.String.Hashtbl.create 16 in
    let aux alarm = (* instantiates [add] for the category of [alarm] *)
      let name = Alarms.get_name alarm in
      let hint = Alarms.get_description alarm in
      let ({get} as check) = add ~name ~hint () in
      Datatype.String.Hashtbl.add h (Alarms.get_name alarm) get;
      check
    in
    let active_alarm alarm =
      try (Datatype.String.Hashtbl.find h (Alarms.get_name alarm)) ()
      with Not_found ->
        Gui_parameters.warning "Unregistered alarm type";
        true (* should not happen *)
    in
    List.map aux Alarms.reprs, active_alarm

  let pack (box:GPack.box) =
(*    let hb = make_expand box
        ~tooltip:"Locations of the properties that are shown" "Where"
      in *)
    onlyCurrent.add (*hb*) box;
    let hb, _ = make_expand box
        ~tooltip:"Which properties (precondition, assertion, etc) are shown"
        "Kind"
    in
    preconditions.add hb;
    ensures.add hb;
    behaviors.add hb;
    complete_disjoint.add hb;
    allocations.add hb;
    assigns.add hb;
    from.add hb;
    assertions.add hb;
    invariant.add hb;
    variant.add hb;
    terminates.add hb;
    stmtSpec.add hb;
    axiomatic.add hb;
    lemmas.add hb;
    typeInvariants.add hb;
    globalInvariants.add hb;
    instances.add hb;
    other.add hb;
    reachable.add hb;
    (*Pragma.add hb;*)
    let hb, _ = make_expand box
        ~tooltip:"Validity status of the properties that are shown" "Status"
    in
    valid.add hb;
    validHyp.add hb;
    unknown.add hb;
    invalid.add hb;
    invalidHyp.add hb;
    considered_valid.add hb;
    untried.add hb;
    dead.add hb;
    inconsistent.add hb;
    let hb_category, expand_category = make_expand box
        ~tooltip:"Category of runtime errors leading to the emission of an \
                  assertion. Enabled only when assertions are displayed."
        "RTE category"
    in
    List.iter (fun check_alarm -> check_alarm.add hb_category) list_alarms;
    let hb, _ = make_expand box
        ~tooltip:"Whether assertions against runtime errors of a certain class \
                  have been generated"
        "RTE emission"
    in
    rteNotGenerated.add hb;
    rteGenerated.add hb;
    (* Register additional callbacks *)
    set_assertions :=
      (fun b ->
         hb_category#misc#set_sensitive b;
         if not b then expand_category#set_expanded false);
    !set_assertions (assertions.get ()) (* For the initial state *);
  ;;

end

open Refreshers

(* Process the rte statuses for the given kf, and add the result in the
   accumulator. Filter the statuses according to user-selected filters*)
let aux_rte kf acc (name, _, rte_status_get: Db.RteGen.status_accessor) =
  let st = rte_status_get kf in
  match st, rteGenerated.get (), rteNotGenerated.get () with
    | true, true, _
    | false, _, true ->
        (* Considered that leaf functions are not verified internally *)
	let status_name, status =
	  if st then
            if Kernel_function.is_definition kf
            then "Generated", Feedback.Valid
            else "Considered generated", Feedback.Considered_valid
          else "Not generated", Feedback.Invalid
	in
        let function_name, module_name = kf_name_and_module kf in
        let status_icon = Gtk_helper.Icon.Feedback status in
        let ip = Property.ip_other name None Kglobal in {
          module_name = module_name;
          function_name = function_name;
          visible = true;
          ip=ip;
          kind=Pretty_utils.sfprintf "@[<hov>%a@]" Property.pretty ip;
          status_name = status_name ;
          consolidated_status = None ;
          consolidated_status_name = status_name ;
          status_icon = status_icon ;
        } :: acc
    | true, false, _
    | false, _, false -> acc

let properties_tab_label = ref None
(* Used to change dynamically the label of the "Properties" tab. *)

(* Lists of checkboxes (used by popup menus) *)
(* all checks (except onlyCurrent):
   preconditions; ensures; behaviors; allocations; assigns; from;
   assertions; invariant; variant; terminates; stmtSpec; axiomatic;
   typeInvariants; globalInvariants; instances; other; reachable; valid;
   validHyp; unknown; invalid; invalidHyp; considered_valid; untried; dead;
   inconsistent; rteNotGenerated; rteGenerated *)

(* [reset_checks to_check to_uncheck] sets all checks in [to_check] and
   unsets all checks in [to_uncheck], then refreshes the view. *)
let reset_checks to_check to_uncheck =
  List.iter (fun chk -> chk.set true) to_check;
  List.iter (fun chk -> chk.set false) to_uncheck;
  Refreshers.apply ()

let reset_checks_default () =
  List.iter (fun chk -> chk.reset ()) (all_checks ());
  Refreshers.apply ()

let unproven_checks_true = [unknown; invalid; invalidHyp; inconsistent]
let unproven_checks_false = [valid; validHyp; considered_valid; untried; dead]

let check_default () = reset_checks_default ()
let check_unproven () = reset_checks unproven_checks_true unproven_checks_false
let check_all () = reset_checks (all_checks ()) []
let check_none () = reset_checks [] (all_checks ())

let make_panel (main_ui:main_window_extension_points) =
  let container = GPack.hbox () in
  let vb_left = GPack.vbox ~packing:container#pack () in
  let hb_refresh_reset = GPack.hbox ~packing:vb_left#pack () in
  let refresh_button =
    GButton.button ~packing:(hb_refresh_reset#pack ~expand:true) ()
  in
  let refresh_label = GMisc.label ~markup:"<b>Refresh</b>" () in
  refresh_button#add refresh_label#coerce;
  Refreshers.set_refresh_needed :=
    (fun b ->
       if b then refresh_label#set_label "<b>Refresh</b>"
       else refresh_label#set_label "Refresh");

  (* button to modify all checkboxes according to presets *)
  let checks_menu = GMenu.menu () in
  let mi_check_def = GMenu.menu_item ~label:"Reset all filters to default" () in
  checks_menu#add mi_check_def;
  ignore (mi_check_def#connect#activate ~callback:(fun () -> check_default ()));
  let mi_check_unproven =
    GMenu.menu_item ~label:"Reset 'Status' filters to show only unproven/invalid" ()
  in
  checks_menu#add mi_check_unproven;
  ignore (mi_check_unproven#connect#activate
            ~callback:(fun () -> check_unproven ()));
  let mi_check_all =
    GMenu.menu_item ~label:"Select all" ()
  in
  checks_menu#add mi_check_all;
  ignore (mi_check_all#connect#activate
            ~callback:(fun () -> check_all ()));
  let mi_check_none =
    GMenu.menu_item ~label:"Unselect all" ()
  in
  checks_menu#add mi_check_none;
  ignore (mi_check_none#connect#activate
            ~callback:(fun () -> check_none ()));
  let icon = GMisc.image ~stock:`INDEX () in
  let reset_menu_button =
    GButton.button ~packing:(hb_refresh_reset#pack ~expand:false) ()
  in
  reset_menu_button#misc#set_tooltip_text "Reconfigure filters according to presets";
  reset_menu_button#add icon#coerce;
  ignore (reset_menu_button#connect#clicked
            (fun () -> checks_menu#popup ~button:0
                ~time:(GtkMain.Main.get_current_event_time ())));
  let sc_buttons =
    GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`NEVER ()
  in
  let vb = GPack.vbox () in
  Refreshers.pack vb;
  sc_buttons#add_with_viewport vb#coerce;
  vb_left#add sc_buttons#coerce;
  let module MODEL =
	Gtk_helper.MAKE_CUSTOM_LIST(struct type t = property end)
  in
  let model = MODEL.custom_list () in
  let append m = if m.visible then model#insert m in
  let clear () = model#clear () in
  (* TODO: this avoids some problems when changing projects, where
     the property navigator displays outdated information. A better solution
     would be to projectify what is being displayed *)
  Design.register_reset_extension (fun _ -> clear ();
                                    match !properties_tab_label with
                                    | None -> ()
                                    | Some label ->
                                      GtkMisc.Label.set_text label "Properties"
                                  );
  let sc =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:(container#pack ~expand:true ~fill:true)
      ()
  in
  let view = GTree.view
    ~rules_hint:true
    ~headers_visible:true
    ~packing:sc#add ()
  in
  ignore
    (view#connect#row_activated
       ~callback:(fun path _col ->
	 match model#custom_get_iter path with
	 | Some { MODEL.finfo = { ip = ip } } ->
	   let format_graph ppf =
	     Consolidation_graph.dump (Consolidation_graph.get ip) ppf in
	   Gtk_helper.graph_window_through_dot main_ui#main_window "Dependencies" format_graph
	 | None -> ()));
  view#selection#set_select_function
    (fun path currently_selected ->
      if not currently_selected then
        begin match model#custom_get_iter path with
        | Some {MODEL.finfo={ip = ip;}} ->
          ignore (main_ui#scroll (Pretty_source.PIP ip))
        | None -> ()
        end;
      true);

  let top = `YALIGN 0.0 in

  let make_view_column renderer properties ~title =
    let cview = MODEL.make_view_column model renderer properties ~title in
    cview#set_resizable true;
    ignore (view#append_column cview)
  in

  (* Function name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{function_name=m} -> [`TEXT m])
    ~title:"Function";

  (* Module name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{module_name=m} -> [`TEXT m])
    ~title:"File";

  (* Kind name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{kind=k} -> [`TEXT k])
    ~title:"Kind";

  (* Status colored column viewer *)
  make_view_column (GTree.cell_renderer_pixbuf [top])
    (function {status_icon=status_icon} ->
      [`PIXBUF (Gtk_helper.Icon.get status_icon)])
    ~title:"Status";

  (* Consolidated status name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{consolidated_status_name=k}-> [`TEXT k])
    ~title:"Consolidated Status";

  (* (Local) status name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{status_name=k}-> [`TEXT k])
    ~title:"Local Status";

  view#set_model (Some model#coerce);

  let visible ip = match ip with
    | Property.IPOther _ -> other.get ()
    | Property.IPReachable _ -> reachable.get ()
    | Property.IPBehavior (_,Kglobal,_) -> behaviors.get ()
    | Property.IPBehavior (_,Kstmt _,_) -> behaviors.get () && stmtSpec.get ()
    | Property.IPPredicate(Property.PKRequires _,_,Kglobal,_) ->
        preconditions.get ()
    | Property.IPPredicate(Property.PKRequires _,_,Kstmt _,_) ->
        preconditions.get () && stmtSpec.get ()
    | Property.IPPredicate(Property.PKAssumes _,_,_,_) -> false
    | Property.IPPredicate(Property.PKEnsures _,_,Kglobal,_) -> ensures.get ()
    | Property.IPPredicate(Property.PKEnsures _,_,Kstmt _,_) ->
        ensures.get() && stmtSpec.get()
    | Property.IPPredicate(Property.PKTerminates,_,_,_) -> terminates.get ()
    | Property.IPAxiom _ -> false
    | Property.IPTypeInvariant _ -> typeInvariants.get()
    | Property.IPGlobalInvariant _ -> globalInvariants.get()
    | Property.IPAxiomatic _ -> axiomatic.get () && not (onlyCurrent.get ())
    | Property.IPLemma _ -> lemmas.get ()
    | Property.IPComplete _ -> complete_disjoint.get ()
    | Property.IPDisjoint _ -> complete_disjoint.get ()
    | Property.IPCodeAnnot(_,_,({annot_content = AAssert _} as ca)) ->
      assertions.get () && (match Alarms.find ca with
          | None -> true
          | Some a -> active_alarm a)
    | Property.IPCodeAnnot(_,_,{annot_content = AInvariant _}) ->
        invariant.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = APragma p}) ->
        Logic_utils.is_property_pragma p (* currently always false. *)
    | Property.IPCodeAnnot(_, _, _) -> assert false
    | Property.IPAllocation (_,Kglobal,_,_) -> allocations.get ()
    | Property.IPAllocation (_,Kstmt _,Property.Id_code_annot _,_) ->
        allocations.get ()
    | Property.IPAllocation (_,Kstmt _,Property.Id_behavior _,_) ->
        allocations.get() && stmtSpec.get()
    | Property.IPAssigns (_,Kglobal,_,_) -> assigns.get ()
    | Property.IPAssigns (_,Kstmt _,Property.Id_code_annot _,_) ->
        assigns.get ()
    | Property.IPAssigns (_,Kstmt _,Property.Id_behavior _,_) ->
        assigns.get() && stmtSpec.get()
    | Property.IPFrom _ -> from.get ()
    | Property.IPDecrease _ -> variant.get ()
    | Property.IPPropertyInstance _ -> instances.get ()
  in
  let visible_status_aux = function
    | Consolidation.Never_tried -> untried.get ()
    | Consolidation.Considered_valid -> considered_valid.get ()
    | Consolidation.Valid _ -> valid.get ()
    | Consolidation.Valid_under_hyp _ -> validHyp.get ()
    | Consolidation.Unknown _ -> unknown.get ()
    | Consolidation.Invalid _ -> invalid.get ()
    | Consolidation.Invalid_under_hyp _ -> invalidHyp.get ()
    | Consolidation.Invalid_but_dead _
    | Consolidation.Valid_but_dead _
    | Consolidation.Unknown_but_dead _ -> dead.get ()
    | Consolidation.Inconsistent _ -> inconsistent.get ()
  in
  let visible_status = Extlib.may_map visible_status_aux ~dft:true in
  let fill_model () =
    let add_ip ip =
      if visible ip then
        let p = make_property ip in
        if visible_status p.consolidated_status then append p
    in
    let by_kf, globals = all_properties () in
    (* Add global properties at the top of the list *)
    Property.Set.iter add_ip globals;

    (* Will the results for this kf be ultimately displayed *)
    let display kf =
      not (Cil.is_unused_builtin (Kernel_function.get_vi kf)) &&
       not (onlyCurrent.get ()) ||
         (let kfvi = Kernel_function.get_vi kf in
          List.exists
            (function
               | GFun ({svar = fvi},_) | GFunDecl (_, fvi, _) ->
                   Cil_datatype.Varinfo.equal fvi kfvi
               | _ -> false
            ) main_ui#file_tree#selected_globals)
    in

    let rte_get_all_statuses = !Db.RteGen.get_all_status () in
    (* All non-filtered RTE statuses for a given function *)
    let rte_kf kf = List.fold_left (aux_rte kf) [] rte_get_all_statuses in
    (* Add RTE statuses for all functions. We cannot simply iterate over
       [by_kf], as functions without any property will not be present in it *)
    let with_rte =
      let aux kf acc =
        if display kf then
          let props =
            try !(Kernel_function.Map.find kf by_kf)
            with Not_found -> Property.Set.empty
          in
          (kf, (props, rte_kf kf)) :: acc
        else acc
      in
      Globals.Functions.fold aux []
    in
    (* Sort functions by names *)
    let cmp (k1, _) (k2, _) =
      String.compare (Kernel_function.get_name k1) (Kernel_function.get_name k2)
    in
    let by_kf = List.sort cmp with_rte in

    (* Add the properties for all the relevant functions *)
    List.iter
      (fun (kf, (ips, rtes)) ->
        if display kf then begin
          Property.Set.iter add_ip ips;
          List.iter append rtes;
        end
      ) by_kf;
    match !properties_tab_label with
    | None -> ()
    | Some label ->
      let text = Format.sprintf "Properties (%d)" (model#custom_iter_n_children None) in
      GtkMisc.Label.set_text label text
  in
  ignore
    (let callback _ =
       main_ui#protect ~cancelable:false
         (fun () ->
	   clear ();
           Refreshers.apply ();
           !Refreshers.set_refresh_needed false;
           fill_model ())
     in
     refresh_button#connect#released ~callback);

  (* To fill at startup:
     let (_:GtkSignal.id) = view#misc#connect#after#realize fill_model in *)
  let tab_label = (GMisc.label ~text:"Properties" ())#coerce in
  properties_tab_label := Some (GtkMisc.Label.cast tab_label#as_widget);
  let (_:int) = main_ui#lower_notebook#append_page
    ~tab_label (container#coerce)
  in
  register_reset_extension (fun _ -> Refreshers.apply ())

(* Graphical markers in text showing the status of properties.
   Aka. "bullets" in left margin *)
let highlighter (buffer:GSourceView2.source_buffer) localizable ~start ~stop =
  match localizable with
  | Pretty_source.PIP (Property.IPPredicate (Property.PKAssumes _,_,_,_)) ->
    (* Assumes clause do not get a bullet: there is nothing
       to prove about them.*)
    ()
  | Pretty_source.PIP ppt ->
      Design.Feedback.mark buffer ~start ~stop (Property_status.Feedback.get ppt)
  | Pretty_source.PStmt(_,({ skind=Instr(Call _) } as stmt)) ->
    let kfs = Statuses_by_call.all_functions_with_preconditions stmt in
    (* We separate the consolidated statuses of the preconditions inside
       guarded behaviors from those outside. For guarded behaviors, since we
       do not keep track of the status of 'assumes' clauses, we cannot know
       if they are active. Hence, we must weaken any 'Invalid' status into
       'Unknown'. *)
    let filter (ip_src, _ip_copy) =
      match ip_src with
      | Property.IPPredicate (Property.PKRequires bhv, _, _, _) ->
        bhv.b_assumes = []
      | _ -> false
    in
    let ips_sure, ips_unsure = Kernel_function.Hptset.fold
      (fun kf (ips_sure, ips_unsure) ->
        let ips_kf =
          Statuses_by_call.all_call_preconditions_at ~warn_missing:false kf stmt
        in
        let ips_kf_sure, ips_kf_unsure = List.partition filter ips_kf in
        (List.map snd ips_kf_sure @ ips_sure),
        (List.map snd ips_kf_unsure @ ips_unsure))
      kfs ([], [])
    in
    let ips = ips_sure @ ips_unsure in
    if ips <> [] then
      let validity = Property_status.Feedback.get_conjunction ips in
      let validity =
        match validity with
        | Feedback.Invalid_under_hyp ->
          (* Weaken if the invalidity comes from [ips_unsure]. We do nothing
             for statuses [Invalid] (a path should exist, hence the behavior
             must be active), or [Invalid_but_dead] (equivalent to [True]) *)
          let invalid ip = Feedback.get ip = Feedback.Invalid_under_hyp in
          if List.exists invalid ips_unsure &&
             not (List.exists invalid ips_sure)
          then Feedback.Unknown
          else validity
        | _ -> validity
      in
      (* Use [start=stop] for a call with a statement contract. Without this,
         the bullet is put at the beginning of the spec, instead of in front
         of the call itself *)
      Design.Feedback.mark buffer ~start:stop ~stop validity

  | Pretty_source.PStmt _
  | Pretty_source.PGlobal _| Pretty_source.PVDecl _
  | Pretty_source.PTermLval _| Pretty_source.PLval _
  | Pretty_source.PExp _ -> ()



let extend (main_ui:main_window_extension_points) =
  make_panel main_ui;
  main_ui#register_source_highlighter highlighter

let () = Design.register_extension extend

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)
