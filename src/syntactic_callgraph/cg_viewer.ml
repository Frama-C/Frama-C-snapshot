(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

open Printf
open Register
open Dgraph

let ($) f x = f x

type service_id = int

module Model = DGraphModel.Make(Service.TP)

class ['v, 'e, 'c] service_view obj model g =
object(self)
  inherit ['v, 'e, 'c] DGraphView.highlight_focus_view obj model

  val deployed : (service_id, bool) Hashtbl.t = Hashtbl.create 10

  method roots =
    let l = ref [] in
    self#iter_nodes (fun n -> if self#is_root n then l := n::!l);
    !l

  method iter_roots f = List.iter f self#roots

  method is_root (n : 'v DGraphViewItem.view_node) = n#vertex.Service.is_root

  method is_deployed i = try Hashtbl.find deployed i with Not_found -> false

  method edge_type (e: 'e DGraphViewItem.view_edge) = 
    Service.CallG.E.label e#edge

  (* For debugging purposes *)
  method private print_edge (e: 'e DGraphViewItem.view_edge) =
    let v1, v2 = Service.CallG.E.src e#edge, Service.CallG.E.dst e#edge in
    Options.debug "edge from \"%s\" (service %d) to \"%s\" (service %d): %s@."
      (Service.TP.vertex_name v1) (self#service (self#src e))
      (Service.TP.vertex_name v2) (self#service (self#dst e))
      (match Service.CallG.E.label e#edge with
       | Service.Inter_services -> "inter services"
       | Service.Inter_functions -> "inter functions"
       | Service.Function_to_service -> "function to service")

  method iter_service_nodes f n =
    let s = self#service n in
    let apply n' =
      if n' <> n && self#service n' = s then
	f n' in
    self#iter_nodes apply

  method deploy n =
    assert (self#is_root n);
    let s = self#service n in
    Hashtbl.add deployed s true;
    (* Show nodes *)
    self#iter_service_nodes (fun n' -> n'#show ()) n;
    (* Show or hide edges *)
    let show_or_hide_edge e =
      let n1, n2 = self#src e, self#dst e in
      (* Edge in the current service *)
      if self#service n1 = s && self#service n2 = s then begin
	match self#edge_type e with
	| Service.Inter_functions -> e#show ()
	| _ -> assert false
      end else 
	(* Edge starting from the current service *)
	if self#service n1 = s then begin
	  match self#edge_type e with
	  | Service.Inter_functions ->
	      if self#is_deployed (self#service n2) then e#show ()
	  | Service.Function_to_service ->
	      if not (self#is_deployed (self#service n2)) then e#show ()
	  | Service.Inter_services ->
	      e#hide ()
	end else 
	  (* Edges going to the current service *)
	  if self#service n2 = s then begin
	    match self#edge_type e with
	    | Service.Inter_functions ->
		if self#is_deployed (self#service n1) then
		  e#show ()
	    | Service.Function_to_service ->
		e#hide ()
	    | Service.Inter_services ->
		if self#is_deployed (self#service n1) then
		  e#hide ()
	  end 
    in
    self#iter_edges_e show_or_hide_edge

  method undeploy n =
    assert (self#is_root n);
    let s = self#service n in
    Hashtbl.add deployed s false;
    (* Hide nodes *)
    self#iter_service_nodes (fun n' -> n'#hide ()) n;
    (* Show or hide edges *)
    let show_or_hide_edge e =
      let n1, n2 = self#src e, self#dst e in
      (* Edge in the current service *)
      if self#service n1 = s && self#service n2 = s then begin
	match self#edge_type e with
	| Service.Inter_functions -> e#hide ()
	| _ -> assert false
      end else 
	(* Edges starting from the current service *)
	if self#service n1 = s then begin
	  match self#edge_type e with
	  | Service.Inter_functions | Service.Function_to_service ->
	      e#hide ()
	  | Service.Inter_services ->
	      e#show ()
	end else
	  (* Edges going to the current service *)
	  if self#service n2 = s then begin
	    match self#edge_type e with
	    | Service.Inter_functions ->
		e#hide ()
	    | Service.Function_to_service ->
		if self#is_deployed (self#service n1) then
		  e#show ()
	    | Service.Inter_services ->
		if not (self#is_deployed (self#service n1)) then
		  e#show ()
	  end 
    in
    self#iter_edges_e show_or_hide_edge
      
  method service n = n#vertex.Service.root.Service.node.Callgraph.cnid
      
  (* Events *)
  method private trigger_deploy_ev n = function
  | `BUTTON_PRESS _ ->
      if self#is_root n then
	if self#is_deployed (self#service n) then self#undeploy n
	else self#deploy n;
      false
  | _ -> 
      false
	
  initializer
    (* Hide non-service nodes *)
    self#iter_roots (fun n -> self#undeploy n);
    (* Deploy or undeploy root nodes when clicked *)
    let connect_trigger_to_node n =
      let callback = self#trigger_deploy_ev n in
      n#iter_shapes (fun s -> ignore $ s#connect#event ~callback);
      n#iter_texts (fun t -> ignore $ t#connect#event ~callback) 
    in
    self#iter_roots connect_trigger_to_node;

end

(* Constructor copied from dGraphView *)
let service_view ?(aa=false) model g =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then GnomeCanvas.Canvas.new_canvas_aa ()
      else GnomeCanvas.Canvas.new_canvas () in
    Gobject.set_params w pl;
    new service_view w model g)

let scrolled_view ~packing model g =
  let frame = GBin.frame ~shadow_type:`IN () in
  let aa = true (* anti-aliasing *) in
  let view = 
    service_view ~aa ~width:1280 ~height:1024 ~packing:frame#add model g () 
  in
  ignore $ view#set_center_scroll_region true;
  let table = GPack.table ~packing
                ~rows:2 ~columns:2 ~row_spacings:4 ~col_spacings:4 () in
  ignore $ table#attach ~left:0 ~right:1 ~top:0 ~bottom:1
           ~expand:`BOTH ~fill:`BOTH ~shrink:`BOTH ~xpadding:0 ~ypadding:0
           frame#coerce;
  let w = GRange.scrollbar `HORIZONTAL ~adjustment:view#hadjustment () in
  ignore $ table#attach ~left:0 ~right:1 ~top:1 ~bottom:2
            ~expand:`X ~fill:`BOTH ~shrink:`X ~xpadding:0 ~ypadding:0
            w#coerce;
  let w = GRange.scrollbar `VERTICAL ~adjustment:view#vadjustment () in
  ignore $ table#attach ~left:1 ~right:2 ~top:0 ~bottom:1
            ~expand:`Y ~fill:`BOTH ~shrink:`Y ~xpadding:0 ~ypadding:0
            w#coerce;
  view, table

let create_graph_win title model g =
  let window = GWindow.window ~title ~allow_shrink:true  ~allow_grow:true () in
  let vbox = GPack.vbox ~border_width:4 ~spacing:4 ~packing:window#add () in
  let packing = vbox#pack ~expand:true ~fill:true in
  let view, _table = scrolled_view ~packing model g in
  window, view

let show_graph_win _a =
  let g = Register.get () in
  try
    let model = Model.from_graph g in
    let window, view = create_graph_win "Call Graph" model g in
    window#show ();
    view#adapt_zoom ()
  with DGraphModel.DotError cmd ->
    GToolbox.message_box "Error: " 
      (Printf.sprintf "%s failed\n" cmd)
    
let main window =
  GAction.add_actions window#actions 
    [ GAction.add_action 
	"CallGraph" ~label:"_Show Call Graph" ~callback:show_graph_win];
  ignore
    (window#ui_manager#add_ui_from_string
       "<ui><menubar name='MenuBar'> 
              <menu action='ViewMenu'>
                 <menuitem action='CallGraph'/> 
              </menu>
           </menubar>
       </ui>")

let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
