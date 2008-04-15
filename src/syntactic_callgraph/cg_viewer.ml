(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

let create_scrolled_canvas packing =
  let frame = GBin.frame ~shadow_type:`IN () in
  let canvas =
    let aa = false (* anti-aliasing *) in
    GnoCanvas.canvas ~aa ~width:600 ~height:400 ~packing:frame#add ()
  in
  let _ = canvas#set_center_scroll_region true in
         (* if the graph is too big, show its center *)
  let table = GPack.table ~packing
                ~rows:2 ~columns:2 ~row_spacings:4 ~col_spacings:4 () in
  let _ = table#attach ~left:0 ~right:1 ~top:0 ~bottom:1
            ~expand:`BOTH ~fill:`BOTH ~shrink:`BOTH ~xpadding:0 ~ypadding:0
            frame#coerce in
  let w = GRange.scrollbar `HORIZONTAL ~adjustment:canvas#hadjustment ()  in
  let _ = table#attach ~left:0 ~right:1 ~top:1 ~bottom:2
            ~expand:`X ~fill:`BOTH ~shrink:`X ~xpadding:0 ~ypadding:0
            w#coerce  in
  let w = GRange.scrollbar `VERTICAL ~adjustment:canvas#vadjustment ()  in
  let _ = table#attach ~left:1 ~right:2 ~top:0 ~bottom:1
            ~expand:`Y ~fill:`BOTH ~shrink:`Y ~xpadding:0 ~ypadding:0
            w#coerce  in
    canvas

let create_graph_win () =
  let window = GWindow.window ~title:"Call Graph"
                 ~allow_shrink:true  ~allow_grow:true ()  in
  let vbox = GPack.vbox ~border_width:4 ~spacing:4 ~packing:window#add () in
  let help_but = GButton.button ~label:"Help" 
                   ~packing:(vbox#pack ~expand:false ~fill:true) () in
  let _ = help_but#connect#clicked ~callback:ViewGraph_select.show_help in
  let canvas = create_scrolled_canvas (vbox#pack ~expand:true ~fill:true) in
  let hbox = GPack.hbox ~spacing:4 ~packing:vbox#pack () in
  let select_init_env =
    ViewGraph_select.init ViewGraph_select.default_options
      canvas (hbox#pack ~expand:true ~fill:true) in
    window#show ();
    select_init_env
      
module Cb = struct
  type t_env = unit
  let button_one_press_on_graph _env = ()
  let button_two_press_on_graph _env = ()
  let button_three_press_on_graph _env = ()
  let button_one_press_on_node _env _n = () 
      (* TODO : show fct in source viewer ? *)
  let button_two_press_on_node _env _n = ()
      (* TODO : show calls in source viewer ? *)
  let button_three_press_on_node _env _n = ()
  let enter_node _env _n = ()
  let leave_node _env _n = ()
end

module V = ViewGraph_select.VG (Cb)

let show_cg_cb _a = 
  let cg_name = Cmdline.CallgraphFilename.get () in
  let cg_name = 
    if cg_name <> "" then cg_name
    else Filename.temp_file "framaC_cg_" ".dot"
  in Cmdline.CallgraphFilename.set cg_name;

  let select_init_env = create_graph_win () in
    Format.printf "[call graph] preparing the file %s " cg_name;
    !Db.Syntactic_callgraph.dump ();
    Format.printf "done.";
  try
    let env = () in
    let _graph = V.open_dot_file env select_init_env cg_name in
      ()
  with ViewGraph.DotError _ ->
    GToolbox.message_box "Error"
      (Printf.sprintf "Didn't succed to build graph for %s\nSorry !" cg_name)

let main window =
  GAction.add_actions window#actions 
    [GAction.add_action "CallGraph" ~label:"_Show Call Graph" ~callback:show_cg_cb];
    window#ui_manager#add_ui_from_string
      "<ui><menubar name='MenuBar'> 
              <menu action='ViewMenu'>
                 <menuitem action='CallGraph'/> 
              </menu>
           </menubar>
       </ui>";
    Format.printf "View added";
  ()

let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)

