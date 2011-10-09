(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Pretty_source
open Gtk_helper
open Db
open Cil_types
open Cil_datatype


(* Show or hide the 'Occurrence' column of the gui filetree. *)
let show_column = ref (fun () -> ())

(* Are results shown? *)
module Enabled = struct
  include State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Occrrence_gui.State"
       let dependencies = [!Db.Occurrence.self]
       let kind = `Internal
       let default () = false
     end)
end

let _ =
  Dynamic.register
    ~plugin:"Occurrence"
    ~journalize:false
    "Enabled.set"
    (Datatype.func Datatype.bool Datatype.unit)
    Enabled.set

let _ =
  Dynamic.register
    ~plugin:"Occurrence"
    ~journalize:false
    "Enabled.get"
    (Datatype.func Datatype.unit Datatype.bool)
    Enabled.get

let find_occurrence (main_ui:Design.main_window_extension_points) vi () =
  ignore (!Db.Occurrence.get vi);
  Enabled.set true;
  !show_column ();
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
            let same_lval (k, l) = Kinstr.equal k ki && Lval.equal l lval in
            if List.exists same_lval result then highlight ()
        | PTermLval (_,ki,term_lval) ->
            let same_tlval (k, l) =
              Logic_utils.is_same_tlval
                (Logic_utils.lval_to_term_lval ~cast:true l)
                term_lval
              && Kinstr.equal k ki
            in
            if List.exists same_tlval result then highlight ()
        | PVDecl(_, vi') when Varinfo.equal vi vi' ->
            highlight ()
        | PVDecl _ | PStmt _ | PGlobal _ | PIP _ -> ()

module FollowFocus =
  State_builder.Ref
    (Datatype.Bool)
    (struct
      let name = "Occurrence_gui.FollowFocus"
      let dependencies = []
      let kind = `Internal
      let default () = false
     end)

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
  old_gtk_compat e#set_single_line_mode true;
  let set_selected = GButton.button ~label:"Set selected"
    ~packing:selected_var_box#pack ()
  in
  let do_select localizable =
    apply_on_vi
      (fun vi -> find_occurrence main_ui vi ())
      localizable
  in
  ignore (set_selected#connect#pressed
            (fun () -> History.apply_on_selected do_select));
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
         !show_column ();
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
       History.apply_on_selected
         (apply_on_vi (fun _ -> sensitive_set_selected_button:=true));
       set_selected#misc#set_sensitive !sensitive_set_selected_button;
       if Enabled.get () <> enabled_button#active then (
         enabled_button#set_active (Enabled.get ());
         !show_column ();
       );
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
         let callback = find_occurrence main_ui vi in
         ignore (popup_factory#add_item "_Occurrence" ~callback);
         if FollowFocus.get () then
           ignore (Glib.Idle.add (fun () -> callback (); false))
       end)
      localizable

let file_tree_decorate (file_tree:Filetree.t) =
  show_column :=
    file_tree#append_pixbuf_column
      ~title:"Occurrence"
      (fun globs ->
        match !Db.Occurrence.get_last_result () with
          | None -> (* occurrence not computed *)
            [`STOCK_ID ""]
          | Some (result, _) ->
            let in_globals (ki,_) =
              match ki with
                | Kglobal -> false
                | Kstmt stmt ->
                  let kf = Kernel_function.find_englobing_kf stmt in
                  let {vid=v0} = Kernel_function.get_vi kf in
                  List.exists
                    (fun glob -> match glob with
                      | GFun ({svar ={vid=v1}},_ ) -> v1=v0
                      |  _ -> false)
                    globs
            in
            if List.exists in_globals result then [`STOCK_ID "gtk-apply"]
            else [`STOCK_ID ""])
      (fun () -> Enabled.get ());
  !show_column ()

let main main_ui =
  main_ui#register_source_selector occurrence_selector;
  main_ui#register_source_highlighter occurrence_highlighter;
  main_ui#register_panel occurrence_panel;
  file_tree_decorate main_ui#file_tree;
;;

let () = Design.register_extension main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
