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

open Dgraph

let ($) f x = f x

type service_id = int

module Service_view = DGraphContainer.Make(Services.Graphviz_attributes)

class ['v, 'e, 'c] services_view view = object (self)

  val services:
    (service_id,
     bool ref * Services.G.V.t DGraphViewItem.view_item list ref)
    Hashtbl.t
    = Hashtbl.create 10

  method is_root (n:'v DGraphViewItem.view_item) = n#item.Service_graph.is_root

  method is_deployed id =
    try !(fst (Hashtbl.find services id)) with Not_found -> assert false

  method edge_kind (e: 'e DGraphViewItem.view_item) =
    Services.G.E.label e#item

  method deploy node =
    assert (self#is_root node);
    let service = self#service node in
    let deployed, nodes = Hashtbl.find services service in
    assert (not !deployed);
    deployed := true;
    (* iterating on nodes of the current service *)
    List.iter
      (fun n ->
         n#compute ();
         if not (self#is_root n) then n#show ();
         view#iter_succ_e
           (fun e -> match self#edge_kind e with
            | Service_graph.Inter_functions | Service_graph.Both ->
                e#compute ();
                e#show ()
            | Service_graph.Inter_services ->
                e#hide ())
           n)
      !nodes

  method undeploy node =
    assert (self#is_root node);
    let service = self#service node in
    let deployed, nodes = Hashtbl.find services service in
    assert !deployed;
    deployed := false;
    (* iterating on nodes of the current service *)
    List.iter
      (fun n ->
         if not (self#is_root n) then n#hide ();
         view#iter_succ_e
           (fun e -> match self#edge_kind e with
            | Service_graph.Inter_services | Service_graph.Both -> e#show ()
            | Service_graph.Inter_functions -> e#hide ())
           n)
      !nodes

  method service n =
    Kernel_function.get_id n#item.Service_graph.root.Service_graph.node

  initializer
  let add_in_service n s =
    try
      let _, nodes = Hashtbl.find services s in
      nodes := n :: !nodes
    with Not_found ->
      Hashtbl.add services s (ref false, ref [ n ])
  in
  let connect_trigger_to_node n =
    let callback = function
      | `BUTTON_PRESS _ ->
          if self#is_deployed (self#service n) then self#undeploy n
          else self#deploy n;
          false
      | _ ->
          false
    in
    n#connect_event ~callback
  in
  view#iter_nodes
    (fun n ->
       add_in_service n (self#service n);
       if self#is_root n then connect_trigger_to_node n else n#hide ());
  view#iter_edges_e
    (fun e -> match self#edge_kind e with
     | Service_graph.Inter_services | Service_graph.Both -> e#show ()
     | Service_graph.Inter_functions -> e#hide ())

end

(* Constructor copied from dGraphView *)
let services_view model =
  let delay_node v = not v.Service_graph.is_root in
  let delay_edge e = match Services.G.E.label e with
    | Service_graph.Inter_services | Service_graph.Both -> false
    | Service_graph.Inter_functions -> true
  in
  let view = Service_view.GView.view ~aa:true ~delay_node ~delay_edge model in
  view#set_zoom_padding 0.025;
  (* not very nice *)
  ignore (new services_view view);
  view#connect_highlighting_event ();
  ignore $ view#set_center_scroll_region true;
  view

let make_service_view ~packing () =
  let _, view =
    Service_view.from_graph_with_commands
      ~packing
      ?root:(Services.entry_point ())
      ~mk_global_view:services_view
      (Services.Subgraph.get ())
  in
  view

module Cg_view = DGraphContainer.Make(Cg.Graphviz_attributes)

let make_cg_view ?root ~packing (): Cg_view.view_container =
  let _, view =
    Cg_view.from_graph_with_commands ~packing ?root (Cg.Subgraph.get ())
  in
  view

(* note: root is only used when services are not computed *)
let make_graph_view ?root services ~packing () =
  if services then
    (make_service_view ~packing () :> <adapt_zoom: unit -> unit>)
  else
    (make_cg_view ?root ~packing () :> <adapt_zoom: unit -> unit >)

let has_entry_point () =
  try ignore (Globals.entry_point ()); true
  with Globals.No_such_entry_point _ -> false

let can_show_service_graph () =
   has_entry_point () && Options.Service_roots.is_empty ()

let get_current_function () =
  match History.get_current () with
  | Some (History.Global (Cil_types.GFunDecl (_, vi, _)))
  | Some (History.Global (Cil_types.GFun ({Cil_types.svar = vi}, _))) ->
    let kf =
      try Globals.Functions.get vi
      with Not_found -> Options.fatal "no kf for %a" Printer.pp_varinfo vi
    in
    if Kernel_function.is_definition kf then Some kf else None
  | Some (History.Localizable l) -> Pretty_source.kf_of_localizable l
  | _ -> None

let warn_degrade reason =
  GToolbox.message_box ~title:"Warning"
    ("Services cannot be displayed due to " ^ reason ^
     ".\n\
      View degraded to non-service graph.\n\
      (use -cg-no-services to avoid this warning)")

exception Found_vertex of bool

let main (window: Design.main_window_extension_points) =
  ignore
    ((window#menu_manager ())#add_plugin
       [ Menu_manager.menubar "Show entire callgraph"
           (Menu_manager.Unit_callback (fun () ->
                (* note: if there is no entry point, or if the set of service
                   roots is not empty, we must 'degrade' the view and show a
                   non-service graph *)
                let services, warn =
                  if Options.Services.get () then
                    let degrade = not (can_show_service_graph ()) in
                    not degrade, degrade
                  else false, false
                in
                try
                  (* display the callgraph through its dot output *)
                  Service_graph.frama_c_display true;
                  Gtk_helper.graph_window
                    ~parent:window#main_window ~title:"Callgraph"
                    (make_graph_view services);
                  if warn then
                    warn_degrade
                      (if not (has_entry_point ()) then "absence of entry point"
                       else "set of service roots being non-empty")
                with ex ->
                  GToolbox.message_box ~title:"Error"
                    ("Error loading callgraph: " ^ (Printexc.to_string ex))
              ));
         Menu_manager.menubar "Show callgraph from current function"
           ~sensitive:(fun () -> get_current_function () <> None)
           (Menu_manager.Unit_callback (fun () ->
                match get_current_function () with
                | None ->
                  GToolbox.message_box ~title:"Error" "Error: no current function"
                | Some kf ->
                  try
                    (* save old value, to restore it later *)
                    let old_roots = Options.Roots.get () in
                    Options.Roots.set (Kernel_function.Set.singleton kf);
                    let services, warn =
                      if Options.Services.get () && can_show_service_graph ()
                      then begin
                        ignore (Services.Subgraph.get ()); (* compute subgraph *)
                        let is_root = Services.is_root kf in
                        is_root, not is_root
                      end
                      else false, false
                    in
                    Service_graph.frama_c_display true;
                    Gtk_helper.graph_window
                      ~parent:window#main_window ~title:"Callgraph"
                      (make_graph_view ~root:kf services);
                    (* restore old value *)
                    Options.Roots.set old_roots;
                    if warn then
                      warn_degrade "node not being a service root"
                  with ex ->
                    GToolbox.message_box ~title:"Error"
                      ("Error loading callgraph: " ^ (Printexc.to_string ex))
              ))
       ])

let () = Design.register_extension main

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
