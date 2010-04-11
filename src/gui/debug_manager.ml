(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* Require Dgraph included in Ocamlgraph, thus GnomeCanvas *)

open Dgraph

let state_dependency_graph ~packing () =
  let dot_file =
    Extlib.temp_file_cleanup_at_exit "framac_state_dependency_graph" "dot"
  in
  Project.Computation.dump_dynamic_dependencies dot_file;
  let model = DGraphModel.read_dot dot_file in
  let view = DGraphView.view ~aa:true ~packing model in
  view#connect_highlighting_event ();
  view

let graph_window main_window title =
  let height = int_of_float (float main_window#default_height *. 3. /. 4.) in
  let width = int_of_float (float main_window#default_width *. 3. /. 4.) in
  let window =
    GWindow.window
      ~width ~height ~title ~allow_shrink:true ~allow_grow:true
      ~position:`CENTER ()
  in
  let scroll =
    GBin.scrolled_window
      ~packing:window#add ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let view = state_dependency_graph ~packing:scroll#add () in
  ignore (view#set_center_scroll_region true);
  window#show ();
  view#adapt_zoom ()

let () =
  Design.register_extension
    (fun window ->
       ignore
	 (window#menu_manager#add_debug
	    ~show:(fun () -> Kernel.debug_atleast 1)
	    [ let s = "State Dependency Graph" in
	      Menu_manager.Menubar(None, s),
	      fun () -> graph_window window#main_window s ]))

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
