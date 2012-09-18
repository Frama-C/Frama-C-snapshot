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

open Pretty_source
open Gtk_helper
open Db
open Cil_types

module SelectedStmt = struct
  include State_builder.Option_ref
    (Cil_datatype.Stmt)
    (struct
      let name = "Impact_gui.SelectedStmt"
      let dependencies = [ Ast.self ]
     end)

  let set s =
    set s;
    Project.clear ~selection:(State_selection.only_dependencies self) ();
end

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
      State_dependency_graph.Static.add_codependencies
        ~onto:SelectedStmt.self
        [ !Db.Pdg.self ])

module Highlighted_stmt : sig
  val add: Kernel_function.t -> stmt -> unit
  val mem: Kernel_function.t -> stmt -> bool
  val mem_kf: Kernel_function.t -> bool
end = struct

  open Cil_datatype

  module Tbl =
    Kernel_function.Make_Table
      (Stmt.Set)
      (struct
         let name = "Impact_gui.Highlighted_stmt"
         let size = 7
         let dependencies = [ SelectedStmt.self ]
       end)

  let add kf s =
    ignore
      (Tbl.memo
         ~change:(fun set -> Stmt.Set.add s set)
         (fun _ -> Stmt.Set.singleton s)
         kf)

  let mem kf s =
    try
      let set = Tbl.find kf in
      Stmt.Set.mem s set
    with Not_found ->
      false

  let mem_kf = Tbl.mem

end

module ImpactedNodes =
  State_builder.Ref(Kernel_function.Map.Make(PdgTypes.NodeSet))(struct
    let name = "Impact.Register_gui.ImpactedNodes"
    let dependencies = [SelectedStmt.self]
    let default () = Kernel_function.Map.empty
  end)

module InitialNodes =
  State_builder.Ref(Datatype.List(PdgTypes.Node))(struct
    let name = "Impact.Register_gui.InitialNodes"
    let dependencies = [SelectedStmt.self]
    let default () = []
  end)

let impact_in_kf kf = Compute_impact.impact_in_kf (ImpactedNodes.get ()) kf

(* Update the 'Impact' column of the gui filetree. *)
let update_column = ref (fun _ -> ())

(* Are results shown? *)
module Enabled = struct
  include State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Impact_gui.State"
       let dependencies = []
       let default () = false
     end)
end

(* Should perform slicing after impact? *)
module Slicing =
  State_builder.Ref
    (Datatype.Bool)
    (struct
      let name = "Impact_gui.Slicing"
      let dependencies = []
      let default () = false
     end)

(* Follow Focus mode *)
module FollowFocus =
  State_builder.Ref
    (Datatype.Bool)
    (struct
      let name = "Impact_gui.FollowFocus"
      let dependencies = []
      let default () = false
     end)

let apply_on_stmt f = function
  | PStmt (kf,s) -> f kf s
  | _ -> ()

let impact_highlighter buffer loc ~start ~stop =
  if Enabled.get () then
    let tag name color =
      let t = make_tag buffer name [`BACKGROUND color ] in
      apply_tag buffer t start stop
    in
    let hilight kf s =
      if Highlighted_stmt.mem kf s then
        tag "hilighed_impact" "green"
      else
        SelectedStmt.may
          (fun sel -> if Cil_datatype.Stmt.equal sel s then
             tag "selected_impact" "cyan")
    in
    apply_on_stmt hilight loc


let impact_statement s =
  let kf = Kernel_function.find_englobing_kf s in
  let skip = Compute_impact.skip () in
  let impact = Compute_impact.impacted_nodes ~skip kf [s] in
  let init = Compute_impact.initial_nodes ~skip kf s in
  SelectedStmt.set s;
  ImpactedNodes.set impact;
  InitialNodes.set init;
  let stmts = ref [] in
  Kernel_function.Map.iter
    (fun kf s ->
      let stmts' = Compute_impact.nodes_to_stmts s in
      stmts := stmts' :: !stmts;
      List.iter (Highlighted_stmt.add kf) stmts'
    ) impact;
  let impact = List.concat !stmts in
  if Slicing.get () then !Db.Impact.slice impact;
  Enabled.set true;
  impact

let impact_statement_ui (main_ui:Design.main_window_extension_points) s =
  let val_computed = Db.Value.is_computed () in
  ignore (!Db.Impact.from_stmt s);
  if not val_computed then
    main_ui#reset ()
  else (
    !update_column `Contents;
    main_ui#rehighlight ()
  )

let pretty_info = ref false

let impact_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  match localizable with
    | PStmt (kf, s) ->
       if button = 3 || FollowFocus.get () then (
         let callback () = ignore (impact_statement_ui main_ui s) in
         ignore (popup_factory#add_item "_Impact analysis" ~callback);
         if FollowFocus.get () then
           ignore (Glib.Idle.add (fun () -> callback (); false))
       );
      if button = 1 then
        (* Initial nodes, at the source of the impact *)
        (match SelectedStmt.get_option () with
          | Some s' when Cil_datatype.Stmt.equal s s' ->
            if !pretty_info then
            main_ui#pretty_information "@[Impact initial nodes:@ %a@]@."
              (Pretty_utils.pp_list ~sep:",@ " (!Db.Pdg.pretty_node false))
              (InitialNodes.get ());
          | _ -> ());
        let nodes = impact_in_kf kf in
        let nodes = PdgTypes.NodeSet.filter
          (fun node ->
            match PdgIndex.Key.stmt (!Pdg.node_key node) with
              | None -> false
              | Some s' -> Cil_datatype.Stmt.equal s s')
          nodes
        in
        if not (PdgTypes.NodeSet.is_empty nodes) then
          if !pretty_info then
          main_ui#pretty_information "@[Impact:@ %a@]@."
            (Pretty_utils.pp_iter ~sep:",@ " PdgTypes.NodeSet.iter
               (!Db.Pdg.pretty_node false)) nodes;
    | PVDecl (_, vi) | PGlobal (GFun ({ svar = vi }, _))
        when Cil.isFunctionType vi.vtype ->
       if button = 1 then
         let kf = Globals.Functions.get vi in
         let nodes = impact_in_kf kf in
         let nodes = PdgTypes.NodeSet.filter
          (fun node ->
            match PdgIndex.Key.stmt (!Pdg.node_key node) with
              | None -> true
              | Some _ -> false
          ) nodes
         in
         if not (PdgTypes.NodeSet.is_empty nodes) then
           if !pretty_info then
           main_ui#pretty_information "@[Function global impact:@ %a@]@."
             (Pretty_utils.pp_iter ~sep:",@ " PdgTypes.NodeSet.iter
                (!Db.Pdg.pretty_node false)) nodes
    | _ -> ()

let impact_panel main_ui =
  let w = GPack.vbox () in
  (* button "set_selected" *)
  let bbox = GPack.hbox ~width:120 ~packing:w#pack () in
  let set_selected =
    GButton.button ~label:"Set selected"
      ~packing:(bbox#pack ~fill:false ~expand:true) ()
  in
  let do_select = apply_on_stmt (fun _ -> impact_statement_ui main_ui) in
  ignore (set_selected#connect#pressed
            (fun () -> History.apply_on_selected do_select));
  (* check buttons *)
  let enabled_button =
    on_bool w "Enable" Enabled.get
      (fun b ->
        Enabled.set b;
        !update_column `Visibility;
        main_ui#rehighlight ())
  in
  let slicing_button =
    on_bool w "Slicing after impact" Slicing.get Slicing.set
  in
  let follow_focus_button =
    on_bool w "Follow focus" FollowFocus.get FollowFocus.set
  in
  (* panel refresh *)
  let refresh () =
    let sensitive_set_selected_button = ref false in
    History.apply_on_selected
      (apply_on_stmt (fun _ _ -> sensitive_set_selected_button := true));
    set_selected#misc#set_sensitive !sensitive_set_selected_button;
    enabled_button ();
    slicing_button ();
    follow_focus_button ()
  in
  "Impact", w#coerce, Some refresh

let file_tree_decorate (file_tree:Filetree.t) =
  update_column :=
    file_tree#append_pixbuf_column
      ~title:"Impact"
      (fun globs ->
        let is_hilighted = function
          | GFun ({svar = v }, _) ->
            Highlighted_stmt.mem_kf (Globals.Functions.get v)
          |  _ -> false
        in
        let id =
          (* lazyness of && is used for efficiency *)
          if Enabled.get () && SelectedStmt.get_option () <> None &&
            List.exists is_hilighted globs then "gtk-apply"
          else ""
        in
        [ `STOCK_ID id ])
    (fun () -> Enabled.get () && SelectedStmt.get_option () <> None);
  !update_column `Visibility

let main main_ui =
  main_ui#register_source_selector impact_selector;
  main_ui#register_source_highlighter impact_highlighter;
  main_ui#register_panel impact_panel;
  file_tree_decorate main_ui#file_tree

let () = Design.register_extension main

let () =
  Db.register
    (Db.Journalize
       ("Impact.from_stmt",
        Datatype.func Cil_datatype.Stmt.ty
        (Datatype.list Cil_datatype.Stmt.ty)))
    Impact.from_stmt
    impact_statement


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
