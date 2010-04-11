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

(* $Id: register_gui.ml,v 1.10 2008-12-22 10:35:15 uid568 Exp $ *)

open Pretty_source
open Gtk_helper
open Db
open Cil_types

module SelectedStmt = struct
  include Computation.OptionRef
    (Cil_datatype.Stmt)
    (struct
       let name = "Impact_gui.SelectedStmt"
       let dependencies = [ Ast.self ]
     end)
  let set s =
    set s;
    let only = 
      Project.Selection.singleton self Kind.Only_Select_Dependencies
    in 
    Project.clear ~only ()
end

module Highlighted_stmt : sig
  val add: Kernel_function.t -> stmt -> unit 
  val mem: Kernel_function.t -> stmt -> bool
  val mem_kf: Kernel_function.t -> bool
end = struct

  module Tbl = 
    Kernel_function.Make_Table
      (Cil_datatype.StmtSet)
      (struct
	 let name = "Impact_gui.Highlighted_stmt"
	 let size = 7
	 let dependencies = [ SelectedStmt.self ]
       end)

  let add kf s = 
    ignore
      (Tbl.memo
	 ~change:(fun set -> Cilutil.StmtSet.add s set) 
	 (fun _ -> Cilutil.StmtSet.singleton s) 
	 kf)

  let mem kf s = 
    try
      let set = Tbl.find kf in
      Cilutil.StmtSet.mem s set
    with Not_found ->
      false

  let mem_kf = Tbl.mem

end
 
(* Are results shown? *)
module Enabled =
  Computation.Ref
    (struct include Datatype.Bool let default () = true end)
    (struct let name = "Impact_gui.Enabled" let dependencies = [] end)

(* Should perform slicing after impact? *)
module Slicing =
  Computation.Ref
    (struct include Datatype.Bool let default () = false end)
    (struct let name = "Impact_gui.Slicing" let dependencies = [] end)

(* Follow Focus mode *)
module FollowFocus = 
  Computation.Ref
    (struct include Datatype.Bool let default () = false end)
    (struct let name = "Impact_gui.FollowFocus" let dependencies = [] end)

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

let compute_impact (main_ui:Design.main_window_extension_points) s =
  let impact = !Db.Impact.from_stmt s in
  SelectedStmt.set s;
  let add s = 
    Highlighted_stmt.add (snd (Kernel_function.find_from_sid s.sid)) s 
  in
  List.iter add impact;
  if Slicing.get () then !Db.Impact.slice impact;
  Enabled.set true;
  main_ui#rehighlight ()

let impact_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  apply_on_stmt
    (fun _ s -> 
       if button = 3 || FollowFocus.get () then
	 let callback () = compute_impact main_ui s in
	 ignore (popup_factory#add_item "_Impact analysis" ~callback);
	 if FollowFocus.get () then
	   ignore (Glib.Idle.add (fun () -> callback (); false)))
    localizable

let impact_panel main_ui = 
  let w = GPack.vbox () in
  (* button "set_selected" *)
  let bbox = GPack.hbox ~width:120 ~packing:w#pack () in
  let set_selected = 
    GButton.button ~label:"Set selected" 
      ~packing:(bbox#pack ~fill:false ~expand:true) () 
  in
  let do_select = apply_on_stmt (fun _ -> compute_impact main_ui) in
  ignore (set_selected#connect#pressed
	    (fun () -> Design.apply_on_selected do_select));
  (* check buttons *)
  let add_check_button label active f =
    let b = GButton.check_button ~label ~active ~packing:w#pack () in
    ignore (b#connect#toggled ~callback:(fun () -> f b));
    b
  in
  let enabled_button = 
    add_check_button "Enable" (Enabled.get ())
      (fun b -> Enabled.set b#active; main_ui#rehighlight ())
  in
  let slicing_button =
    add_check_button "Slicing after impact" (Slicing.get ())
      (fun b -> Slicing.set b#active)
  in
  let follow_focus_button =
    add_check_button "Follow focus" (FollowFocus.get ())
      (fun b -> FollowFocus.set b#active)
  in
  (* panel refresh *)
  let refresh () = 
    let sensitive_set_selected_button = ref false in
    Design.apply_on_selected
      (apply_on_stmt (fun _ _ -> sensitive_set_selected_button := true));
    set_selected#misc#set_sensitive !sensitive_set_selected_button;
    if Enabled.get () <> enabled_button#active then begin
      enabled_button#set_active (Enabled.get ());
      main_ui#rehighlight ()
    end;
    slicing_button#set_active (Slicing.get ());
    follow_focus_button#set_active (FollowFocus.get ())
  in
  "Impact", w#coerce, Some refresh

let file_tree_decorate (file_tree:Filetree.t) =
  file_tree#append_pixbuf_column
    "Impact"
    (fun globs ->
       let is_hilighted = function
         | GFun ({svar = v }, _) -> 
             Highlighted_stmt.mem_kf (Globals.Functions.get v)
         |  _ -> false
       in
       let id =
	 (* lazyness of && is used for efficiency *)
	 if Enabled.get () && List.exists is_hilighted globs then "gtk-apply"
	 else ""
       in
       [ `STOCK_ID id ])

let main main_ui = 
  main_ui#register_source_selector impact_selector;
  main_ui#register_source_highlighter impact_highlighter;
  main_ui#register_panel impact_panel;
  file_tree_decorate main_ui#file_tree
  
let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
