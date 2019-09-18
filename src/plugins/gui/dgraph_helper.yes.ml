(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

let graph_window ~parent ~title make_view =
  let height = int_of_float (float parent#default_height *. 3. /. 4.) in
  let width = int_of_float (float parent#default_width *. 3. /. 4.) in
  let graph_window =
    GWindow.window
      ~width ~height ~title ~resizable:true ~position:`CENTER ()
  in
  let view = make_view ~packing:graph_window#add () in
  graph_window#show();
  view#adapt_zoom();
  ()
;;

let graph_window_through_dot ~parent ~title dot_formatter =
  let make_view ~packing () =
    let temp_file =
      try
        Extlib.temp_file_cleanup_at_exit
          "framac_property_status_navigator_graph" "dot"
      with Extlib.Temp_file_error s ->
        Gui_parameters.abort "cannot create temporary file: %s" s in
    let fmt = Format.formatter_of_out_channel (open_out temp_file) in
    dot_formatter fmt;
    Format.pp_print_flush fmt ();
    let view =
      snd
        (Dgraph.DGraphContainer.Dot.from_dot_with_commands ~packing temp_file)
    in
    view
  in
  try
    graph_window ~parent ~title make_view
  with Dgraph.DGraphModel.DotError _ as exn ->
    Gui_parameters.error
      "@[cannot display dot graph:@ %s@]"
      (Printexc.to_string exn)
