(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Dgraph

let ($) f x = f x

type service_id = int

module View = DGraphContainer.Make(Services.Graphviz_attributes)

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
    (* itering on nodes of the current service *)
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
    (* itering on nodes of the current service *)
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
  let view = View.GView.view ~aa:true ~delay_node ~delay_edge model in
  view#set_zoom_padding 0.025;
  (* not very nice *)
  ignore (new services_view view);
  view#connect_highlighting_event ();
  ignore $ view#set_center_scroll_region true;
  view

let make_graph_view ~packing () = 
  let _, view = 
    View.from_graph_with_commands
      ~packing
      ?root:(Services.entry_point ())
      ~mk_global_view:services_view
      (Services.get ())
  in view

let main (window: Design.main_window_extension_points) =
  ignore
    ((window#menu_manager ())#add_plugin
       [ Menu_manager.menubar "Show callgraph"
           (Menu_manager.Unit_callback (fun () -> 
	     Service_graph.frama_c_display true;
	     Gtk_helper.graph_window
               ~parent:window#main_window ~title:"Callgraph"
	       make_graph_view))
       ])

let () = Design.register_extension main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
