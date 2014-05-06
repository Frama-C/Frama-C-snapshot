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

(* Require Dgraph included in Ocamlgraph, thus GnomeCanvas *)

open Dgraph

let graph_view ~packing mk_dot =
  let f =
    try Extlib.temp_file_cleanup_at_exit "framac_graph_view" "dot"
    with Extlib.Temp_file_error s ->
      Gui_parameters.abort "cannot create temporary file: %s" s
  in
  mk_dot f;
  snd
    (DGraphContainer.Dot.from_dot_with_commands
       ~status:DGraphContainer.Global
       ~packing
       f)

let state_dependency_graph ~packing () =
  graph_view ~packing State_dependency_graph.dump

(* [JS 2011/07/05] to be reimplemented *)
let status_dependency_graph ~packing:_ () = assert false
(*  let g = Properties_status.Consolidation_tree.get_full_graph () in
  graph_view ~packing (Properties_status.Consolidation_tree.dump g)*)

let graph_window main_window title mk_view =
  let height = int_of_float (float main_window#default_height *. 3. /. 4.) in
  let width = int_of_float (float main_window#default_width *. 3. /. 4.) in
  let window =
    GWindow.window
      ~width ~height ~title ~allow_shrink:true ~allow_grow:true
      ~position:`CENTER ()
  in
  let view = mk_view ~packing:window#add () in
  window#show ();
  view#adapt_zoom ()

open Menu_manager

let () =
  Design.register_extension
    (fun window ->
      let mk_graph = graph_window window#main_window in
      ignore
        ((window#menu_manager ())#add_debug
            ~show:(fun () -> Kernel.debug_atleast 1)
            [ (let s = "State Dependency Graph" in
               menubar s
                 (Unit_callback (fun () -> mk_graph s state_dependency_graph)));
              (let s = "Status Graph" in
               menubar s
                (Unit_callback (fun () -> mk_graph s status_dependency_graph)))
            ]))

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
