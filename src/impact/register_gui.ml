(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: register_gui.ml,v 1.6 2008/11/04 13:58:03 uid568 Exp $ *)

open Pretty_source
open Gtk_helper
open Db
open Cil_types

module HighlightedStmtState = struct
  include Cil_computation.StmtSetRef
    (struct
       let name = "Impact_gui.HighlightedStmt"
       let dependencies = [ Cil_state.self ]
     end)
  let clear () = 
    let only = Project.Selection.singleton self Kind.Select_Dependencies in
    Project.clear ~only ()
end
 
(* Are results shown? *)
module Enabled=
  Computation.Ref
    (struct include Datatype.Bool let default () = true end)
    (struct let name = "Impact_gui.Enabled" let dependencies = [] end)

let apply_on_stmt f = function
  | PStmt (_kf,s) -> f s
  | _ -> ()

let impact_highlighter buffer loc ~start ~stop =
  if Enabled.get () then
    let hilight s = 
      if HighlightedStmtState.mem s then
	let tag = make_tag buffer "impact" [`BACKGROUND "green" ] in
	apply_tag buffer tag start stop
    in
    apply_on_stmt hilight loc

let compute_impact main_ui s =
  main_ui#lock ();
  HighlightedStmtState.clear ();
  List.iter HighlightedStmtState.add (!Db.Impact.from_stmt s);
  Enabled.set true;
  main_ui#rehighlight ();
  main_ui#unlock ()

module FollowFocus = 
  Computation.Ref
    (struct include Datatype.Bool let default () = false end)
    (struct let name = "Impact_gui.FollowFocus" let dependencies = [] end)

let impact_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  apply_on_stmt
    (fun s -> 
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
  let do_select = apply_on_stmt (compute_impact main_ui) in
  ignore (set_selected#connect#pressed
	    (fun () -> Design.apply_on_selected do_select));
  (* check_button enabled *)
  let enabled = Enabled.get () in
  let enabled_button = GButton.check_button 
    ~label:"Enable"
    ~packing:w#pack
    ~active:enabled
    ()
  in
  ignore 
    (enabled_button#connect#toggled 
       ~callback:
       (fun () -> Enabled.set enabled_button#active; main_ui#rehighlight ()));
  (* check_button followFocus *)
  let followFocus = GButton.check_button 
    ~label:"Follow focus"
    ~packing:w#pack
    ~active:(FollowFocus.get ())
    ()
  in
  ignore 
    (followFocus#connect#toggled 
       ~callback:(fun () -> FollowFocus.set followFocus#active));
  (* panel refresh *)
  let refresh () = 
    let sensitive_set_selected_button = ref false in
    Design.apply_on_selected
      (apply_on_stmt (fun _ -> sensitive_set_selected_button:=true));
    set_selected#misc#set_sensitive !sensitive_set_selected_button;
    enabled_button#set_active (Enabled.get());
  in
  "Impact", w#coerce, Some refresh

let main main_ui = 
  main_ui#register_source_selector impact_selector;
  main_ui#register_source_highlighter impact_highlighter;
  main_ui#register_panel impact_panel
  
let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
