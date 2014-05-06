(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
  module OnlyCurrent: State_builder.Ref with type data = bool

  module Ensures: State_builder.Ref with type data = bool
  module Preconditions: State_builder.Ref with type data = bool
  module Behaviors: State_builder.Ref with type data = bool
  module Allocations: State_builder.Ref with type data = bool
  module Assigns: State_builder.Ref with type data = bool
  module From: State_builder.Ref with type data = bool
  module Assert: State_builder.Ref with type data = bool
  module Invariant: State_builder.Ref with type data = bool
  module Variant: State_builder.Ref with type data = bool
  module Terminates: State_builder.Ref with type data = bool
  module StmtSpec: State_builder.Ref with type data = bool
  module Reachable: State_builder.Ref with type data = bool
  module Other: State_builder.Ref with type data = bool
  module Axiomatic: State_builder.Ref with type data = bool
(*module Pragma: State_builder.Ref with type data = bool*)
  module RteNotGenerated: State_builder.Ref with type data = bool
  module RteGenerated: State_builder.Ref with type data = bool

  module Valid: State_builder.Ref with type data = bool
  module ValidHyp: State_builder.Ref with type data = bool
  module Unknown: State_builder.Ref with type data = bool
  module Invalid: State_builder.Ref with type data = bool
  module InvalidHyp: State_builder.Ref with type data = bool
  module Considered_valid: State_builder.Ref with type data = bool
  module Untried: State_builder.Ref with type data = bool
  module Dead: State_builder.Ref with type data = bool
  module Inconsistent: State_builder.Ref with type data = bool

  val pack: GPack.box -> unit
  val apply: unit -> unit
end
=
struct
  (* Function to be called during the idle time of the GUI *)
  let refreshers = ref []
  let add_refresher f = refreshers := f::!refreshers

  module Add (X: sig val name: string val hint: string end) = struct
    open Gtk_helper
    let key_name =
      let s = String.copy X.name in
      for i = 0 to String.length s - 1 do
        let c = s.[i] in
        if c < 'A' || c > 'z' || (c > 'Z' && c < 'a') then
          s.[i] <- '_'
      done;
      "property_panel." ^ s

    include State_builder.Ref
      (Datatype.Bool)
      (struct
        let name = "show " ^ X.name
        let dependencies = []
        let default () = true
       end)

    (* setting the configuration must be delayed until the session directory
       has been set *)
    let first_extended_ref = ref true
    let first_exiting_ref = ref true
    let () =
      Cmdline.run_after_extended_stage 
	(fun () -> 
	  if !first_extended_ref then begin
	    first_extended_ref := false;
	    Configuration.load ()
	  end);
      Cmdline.run_after_loading_stage 
	(fun () -> 
	  if !first_exiting_ref then begin
	    first_exiting_ref := false;
	    let v = Configuration.find_bool ~default:true key_name in
            set v
	  end)

    let set v = 
      Configuration.set key_name (Configuration.ConfBool v);
      set v

    let add hb = add_refresher
      (Gtk_helper.on_bool ~tooltip:X.hint hb X.name get set)

  end

  let apply () = List.iter (fun f -> f ()) !refreshers

  module OnlyCurrent = Add(
    struct let name = "Current function"
           let hint = "Restrict properties to those of current function" end)
  module Preconditions = Add(
    struct let name = "Preconditions"
           let hint = "Show functions preconditions" end)
  module Ensures = Add(
    struct let name = "Postconditions"
           let hint = "Show functions postconditions" end)
  module Behaviors = Add(
    struct let name = "Behaviors"
           let hint = "Show functions behaviors" end)
  module Allocations = Add(
    struct let name = "Allocations"
           let hint = "Show functions assigns" end)
  module Assigns = Add(
    struct let name = "Assigns"
           let hint = "Show functions assigns" end)
  module From = Add(
    struct let name = "From"
           let hint = "Show functional dependencies in functions assigns" end)
  module Assert = Add(
    struct let name = "Assert"
           let hint = "Show assertions" end)
  module Invariant = Add(
    struct let name = "Invariant"
           let hint = "Show loop invariants" end)
  module Variant = Add(
    struct let name = "Variant"
           let hint = "Show loop termination argument" end)
  module Terminates = Add(
    struct let name = "Terminates"
           let hint = "Show functions termination clauses" end)
  module StmtSpec = Add(
    struct let name = "Stmt contract"
           let hint = "Show statements contracts" end)
  module Axiomatic = Add(
    struct let name = "Axiomatic"
           let hint = "Show global axiomatics" end)
  module Reachable = Add(
    struct let name = "Reachable"
           let hint = "Show 'reachable' hypotheses" end)
  module Other = Add(
    struct let name = "Other"
           let hint = "Show other properties" end)
  (*module Pragma = Add(struct let name = "pragma" end) *)
  module RteNotGenerated = Add(
    struct let name = "Non generated"
           let hint = "Show RTEs assertions that remain to generate" end)
  module RteGenerated = Add(
    struct let name = "Generated"
           let hint = "Show RTEs assertions that have been generated" end)


  module Valid = Add(
    struct let name = "Valid"
           let hint = "Show properties that are proven valid" end)
  module ValidHyp = Add(
    struct let name = "Valid under hyp."
           let hint = "Show properties that are are valid, but depend on \
                       some hypotheses" end)
  module Unknown = Add(
    struct let name = "Unknown"
           let hint = "Show properties with an 'unknown' status" end)
  module Invalid = Add(
    struct let name = "Invalid"
           let hint = "Show properties that are proven invalid" end)
  module InvalidHyp = Add(
    struct let name = "Invalid under hyp."
           let hint = "Show properties that are are invalid, but depend on \
                       some hypotheses" end)
  module Considered_valid = Add(
    struct let name = "Considered valid"
           let hint = "Show properties that are considered valid because \
                       the platform has no way to prove them" end)
  module Untried = Add(
    struct let name = "Untried"
           let hint = "Show properties whose proof have not been attempted" end)
  module Dead = Add(
    struct let name = "Dead"
           let hint = "Show properties on unreachable code" end)
  module Inconsistent = Add(
    struct let name = "Inconsistent"
           let hint = "Show properties that have an inconsistent status" end)

  let pack hb =
    OnlyCurrent.add hb;
    Preconditions.add hb;
    Ensures.add hb;
    Behaviors.add hb;
    Allocations.add hb;
    Assigns.add hb;
    From.add hb;
    Assert.add hb;
    Invariant.add hb;
    Variant.add hb;
    Terminates.add hb;
    Reachable.add hb;
    StmtSpec.add hb;
    Axiomatic.add hb;
    Other.add hb;
  (*Pragma.add hb;*)
    RteNotGenerated.add hb;
    RteGenerated.add hb;
    ignore (GMisc.separator ~packing:hb#pack `HORIZONTAL ());
    Valid.add hb;
    ValidHyp.add hb;
    Unknown.add hb;
    Invalid.add hb;
    InvalidHyp.add hb;
    Considered_valid.add hb;
    Untried.add hb;
    Dead.add hb;
    Inconsistent.add hb;

end

open Refreshers

(* Process the rte statuses for the given kf, and add the result in the
   accumulator. Filter the statuses according to user-selected filters*)
let aux_rte kf acc (name, _, rte_status_get: Db.RteGen.status_accessor) =
  let st = rte_status_get kf in
  match st, RteGenerated.get (), RteNotGenerated.get () with
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

let make_panel (main_ui:main_window_extension_points) =
  let container = GPack.hbox () in

  let sc_buttons =
    GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`NEVER ()
  in
  let vb = GPack.vbox () in
  let refresh_button = GButton.button ~label:"Refresh" ~packing:vb#pack () in
  Refreshers.pack vb;
  sc_buttons#add_with_viewport vb#coerce;
  container#pack sc_buttons#coerce;

  let module MODEL =  
	Gtk_helper.MAKE_CUSTOM_LIST(struct type t = property end)
  in
  let model = MODEL.custom_list () in
  let append m = if m.visible then model#insert m in
  let clear () = model#clear () in
  (* TODO: this avoids some problems when changing projects, where
     the property navigator displays outdated information. A better solution
     would be to projectify what is being displayed *)
  Design.register_reset_extension (fun _ -> clear ());
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
    | Property.IPOther _ -> Other.get ()
    | Property.IPReachable _ -> Reachable.get ()
    | Property.IPBehavior (_,Kglobal,_) -> Behaviors.get ()
    | Property.IPBehavior (_,Kstmt _,_) -> Behaviors.get () && StmtSpec.get ()
    | Property.IPPredicate(Property.PKRequires _,_,Kglobal,_) ->
        Preconditions.get ()
    | Property.IPPredicate(Property.PKRequires _,_,Kstmt _,_) ->
        Preconditions.get () && StmtSpec.get ()
    | Property.IPPredicate(Property.PKAssumes _,_,_,_) -> false
    | Property.IPPredicate(Property.PKEnsures _,_,Kglobal,_) -> Ensures.get ()
    | Property.IPPredicate(Property.PKEnsures _,_,Kstmt _,_) ->
        Ensures.get() && StmtSpec.get()
    | Property.IPPredicate(Property.PKTerminates,_,_,_) -> Terminates.get ()
    | Property.IPAxiom _ -> false
    | Property.IPAxiomatic _ -> Axiomatic.get () && not (OnlyCurrent.get ())
    | Property.IPLemma _ -> Axiomatic.get () && not (OnlyCurrent.get ())
    | Property.IPComplete _ -> Behaviors.get ()
    | Property.IPDisjoint _ -> Behaviors.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = AAssert _}) -> Assert.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = AInvariant _}) ->
        Invariant.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = APragma p}) ->
        Logic_utils.is_property_pragma p (* currently always false. *)
    | Property.IPCodeAnnot(_, _, _) -> assert false
    | Property.IPAllocation (_,Kglobal,_,_) -> Allocations.get ()
    | Property.IPAllocation (_,Kstmt _,Property.Id_code_annot _,_) ->
        Allocations.get ()
    | Property.IPAllocation (_,Kstmt _,Property.Id_behavior _,_) ->
        Allocations.get() && StmtSpec.get()

    | Property.IPAssigns (_,Kglobal,_,_) -> Assigns.get ()
    | Property.IPAssigns (_,Kstmt _,Property.Id_code_annot _,_) ->
        Assigns.get ()
    | Property.IPAssigns (_,Kstmt _,Property.Id_behavior _,_) ->
        Assigns.get() && StmtSpec.get()
    | Property.IPFrom _ -> From.get ()
    | Property.IPDecrease _ -> Variant.get ()
  in
  let visible_status_aux = function
    | Consolidation.Never_tried -> Untried.get ()
    | Consolidation.Considered_valid -> Considered_valid.get ()
    | Consolidation.Valid _ -> Valid.get ()
    | Consolidation.Valid_under_hyp _ -> ValidHyp.get ()
    | Consolidation.Unknown _ -> Unknown.get ()
    | Consolidation.Invalid _ -> Invalid.get ()
    | Consolidation.Invalid_under_hyp _ -> InvalidHyp.get ()
    | Consolidation.Invalid_but_dead _
    | Consolidation.Valid_but_dead _
    | Consolidation.Unknown_but_dead _ -> Dead.get ()
    | Consolidation.Inconsistent _ -> Inconsistent.get ()
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
       not (OnlyCurrent.get ()) ||
         (let kfvi = Kernel_function.get_vi kf in
          List.exists
            (function
               | GFun ({svar = fvi},_) | GVarDecl (_, fvi, _) ->
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
      ) by_kf
  in
  ignore
    (let callback _ =
       main_ui#protect ~cancelable:false
         (fun () ->
	   clear ();
           Refreshers.apply ();
           fill_model ())
     in
     refresh_button#connect#released ~callback);

  (* To fill at startup:
     let (_:GtkSignal.id) = view#misc#connect#after#realize fill_model in *)
  let (_:int) = main_ui#lower_notebook#append_page
    ~tab_label:(GMisc.label ~text:"Properties" ())#coerce
    (container#coerce)
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
    let ips = Kernel_function.Hptset.fold
      (fun kf ips ->
        Statuses_by_call.all_call_preconditions_at
          ~warn_missing:false kf stmt @ ips)
      kfs []
    in
    if ips <> [] then
      let ips = List.map snd ips in
      let validity = Property_status.Feedback.get_conjunction ips in
      (* Use [start=stop] for a call with a statement contract. Without this,
         the bullet is put at the beginning of the spec, instead of in front
         of the call itself *)
      Design.Feedback.mark buffer ~start:stop ~stop validity

  | Pretty_source.PStmt _
  | Pretty_source.PGlobal _| Pretty_source.PVDecl _
  | Pretty_source.PTermLval _| Pretty_source.PLval _ -> ()



let extend (main_ui:main_window_extension_points) =
  make_panel main_ui;
  main_ui#register_source_highlighter highlighter

let () = Design.register_extension extend

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
