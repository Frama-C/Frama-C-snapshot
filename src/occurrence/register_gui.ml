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

(* $Id: register_gui.ml,v 1.19 2008/11/04 10:35:48 uid568 Exp $ *)

open Pretty_source
open Gtk_helper
open Db
open Cil_types
open Cilutil

module Enabled=
  Computation.Ref
    (struct include Datatype.Bool let default () = true end)
    (struct let name = "Occurrence_gui.Enabled" let dependencies = [] end)

let find_occurence (main_ui:Design.main_window_extension_points) vi () = 
  main_ui#lock ();
  ignore (!Db.Occurrence.get vi);
  main_ui#unlock ();
  Enabled.set true;
  main_ui#rehighlight ()

(* Only these localizable interest this plugin *)
let apply_on_vi f localizable = match localizable with
  | PVDecl(_,vi) 
  | PLval(_, _, (Var vi, NoOffset)) 
  | PTermLval(_, _, (TVar { lv_origin = Some vi }, TNoOffset)) ->
      if not (Cil.isFunctionType vi.vtype) then
        f vi 
  | _ -> ()

let occurrence_highlighter buffer loc ~start ~stop =
  if Enabled.get () then 
    match !Db.Occurrence.get_last_result () with
    | None -> (* occurrence not computed *)
        ()
    | Some (result, vi) ->
        let highlight () =
	  let tag = make_tag buffer "occurrence" [`BACKGROUND "yellow" ] in
          apply_tag buffer tag start stop
        in
        match loc with 
        | PLval (_, ki, lval) -> 
	    let same_lval (k, l) = 
	      KinstrComparable.equal k ki && Cilutil.equals l lval
	    in
	    if List.exists same_lval result then highlight ()
        | PTermLval (_,ki,term_lval) -> 
	    let same_tlval (k, l) =
              Logic_const.is_same_tlval 
	        (Logic_const.lval_to_term_lval l) 
	        term_lval
	      && KinstrComparable.equal k ki 
	    in
	    if List.exists same_tlval result then highlight ()
        | PVDecl(_, vi') when VarinfoComparable.equal vi vi' ->
	    highlight ()
        | PVDecl _ | PStmt _ -> 
	    ()

module FollowFocus = 
  Computation.Ref
    (struct include Datatype.Bool let default () = false end)
    (struct let name = "Occurrence_gui.FollowFocus" let dependencies = [] end)

let occurrence_panel main_ui = 
  let w = GPack.vbox  () in
  (* Selected Var display *)
  let selected_var_box = GPack.hbox ~packing:w#pack () in
  ignore 
    (GMisc.label ~xalign:0.0 ~text:"Current var: " 
       ~packing:(selected_var_box#pack ~expand:false) ());
  let e = GMisc.label ~xalign:0.0 
    ~selectable:true 
    ~packing:(selected_var_box#pack ~expand:true ~fill:true)
    ()
  in
  e#set_use_markup true;
  e#set_single_line_mode true;
  let set_selected = GButton.button ~label:"Set selected" 
    ~packing:selected_var_box#pack () 
  in
  let do_select localizable = 
    apply_on_vi 
      (fun vi -> find_occurence main_ui vi ())
      localizable
  in
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
       (fun () -> 
          Enabled.set enabled_button#active;
          main_ui#rehighlight ()));
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
  let refresh = 
    let old_vi = ref (-2) in 
    fun () -> 
      (let sensitive_set_selected_button = ref false in
       Design.apply_on_selected
         (apply_on_vi (fun _ -> sensitive_set_selected_button:=true));
       set_selected#misc#set_sensitive !sensitive_set_selected_button;
       enabled_button#set_active (Enabled.get());
       let new_result = !Db.Occurrence.get_last_result () in
       (match new_result with 
	| None when !old_vi<> -1 -> 
            old_vi := -1; e#set_label "<i>None</i>"
	| Some (_,vi) when vi.vid<> !old_vi-> 
            old_vi := vi.vid;
            e#set_label vi.vname
	| _ -> ()))
  in
  "Occurrence",w#coerce,Some refresh

let occurrence_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
    apply_on_vi 
     (fun vi ->
       if button = 3 || FollowFocus.get () then begin
         let callback = find_occurence main_ui vi in
         ignore (popup_factory#add_item "_Occurrence" ~callback);
         if FollowFocus.get () then 
	   ignore (Glib.Idle.add (fun () -> callback (); false))
       end)
      localizable

let file_tree_decorate (file_tree:Filetree.t) = 
  file_tree#append_pixbuf_column 
    "Occurence"  
    (fun globs -> 
       match !Db.Occurrence.get_last_result () with
       | None -> (* occurrence not computed *)
           [`STOCK_ID ""]
       | Some (result, _) ->
           let in_globals globs (ki,_) = 
             let kf = Globals.Functions.find_englobing_kf ki in
             match kf with 
             | None -> false
             | Some kf -> 
                 let {vid=v0} = Kernel_function.get_vi kf in
                 List.exists
                   (fun glob -> match glob with 
                    | GFun ({svar ={vid=v1}},_ ) -> v1=v0
                    |  _ -> false) 
                   globs
           in
           if List.exists (in_globals globs) result then [`STOCK_ID "gtk-yes"]
           else [`STOCK_ID ""])

let main main_ui =
  main_ui#register_source_selector occurrence_selector;
  main_ui#register_source_highlighter occurrence_highlighter;
  main_ui#register_panel occurrence_panel;
  file_tree_decorate main_ui#file_tree

let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
