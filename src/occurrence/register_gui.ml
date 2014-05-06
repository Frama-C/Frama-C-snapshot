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

open Pretty_source
open Gtk_helper
open Cil_types
open Cil_datatype


(* Update the 'Occurrence' column of the gui filetree. *)
let update_column = ref (fun _ -> ())

(* Are results shown? *)
module Enabled = State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Occrrence_gui.State"
       let dependencies = [!Db.Occurrence.self]
       let default () = false
     end)

module ShowRead = State_builder.Ref
  (Datatype.Bool)
  (struct
    let name = "Occrrence_gui.ShowRead"
    let dependencies = []
    let default () = true
   end)

module ShowWrite = State_builder.Ref
  (Datatype.Bool)
  (struct
    let name = "Occrrence_gui.ShowWrite"
    let dependencies = []
    let default () = true
   end)

let consider_access () =
  match ShowRead.get (), ShowWrite.get () with
    | false, false -> (fun _ -> false)
    | true,  true -> (fun _ -> true)
    | true,  false ->
        (fun ak -> ak = Register.Read  || ak = Register.Both)
    | false, true ->
        (fun ak -> ak = Register.Write || ak = Register.Both)

let filter_accesses l =
  match ShowRead.get (), ShowWrite.get () with
    | false, false -> []
    | true,  true -> l
    | true,  false | false, true ->
        let f = consider_access () in
        List.filter (fun access -> f (Register.classify_accesses access)) l

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
  !update_column `Contents;
  main_ui#rehighlight ()

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
        let result = filter_accesses result in
        let highlight () =
          let tag = make_tag buffer "occurrence" [`BACKGROUND "yellow" ] in
          apply_tag buffer tag start stop
        in
        match loc with
        | PLval (_, ki, lval) ->
            let same_lval (_kf, k, l) = 
              Kinstr.equal k ki && Lval.equal l lval
            in
            if List.exists same_lval result then highlight ()
        | PTermLval (_,ki,term_lval) ->
            let same_tlval (_kf, k, l) =
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
  (* check_button enabled *)
  let refresh_enabled_button = on_bool
    w
    "Enable"
    Enabled.get
    (fun v -> Enabled.set v;
      !update_column `Visibility;
      main_ui#rehighlight ())
  in
  (* check_button followFocus *)
  let refresh_followFocus = on_bool w "Follow focus"
    FollowFocus.get
    FollowFocus.set
  in
  let h_read_write = GPack.hbox ~packing:w#pack () in
  let refresh_rw_aux f v =
    f v;
    main_ui#file_tree#reset();
    main_ui#rehighlight ()
  in
  let refresh_read =
    Gtk_helper.on_bool
    ~tooltip:"Show only occurrences where the zone is read"
      h_read_write "Read" ShowRead.get (refresh_rw_aux ShowRead.set) in
  let refresh_write =
    Gtk_helper.on_bool
    ~tooltip:"Show only occurrences where the zone is written"
      h_read_write "Write" ShowWrite.get (refresh_rw_aux ShowWrite.set) in
  let refresh =
    let old_vi = ref (-2) in
    (fun () ->
       refresh_read();
       refresh_write ();
       refresh_followFocus ();
       refresh_enabled_button ();
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
  update_column :=
    file_tree#append_pixbuf_column
      ~title:"Occurrence"
      (fun globs ->
        match !Db.Occurrence.get_last_result () with
          | None -> (* occurrence not computed *)
            [`STOCK_ID ""]
          | Some (result, _) ->
            let in_globals (kf,ki,_ as access) =
              (let ak = Register.classify_accesses access in
               consider_access () ak)
              &&
              match ki with
                | Kglobal -> false
                | Kstmt _ ->
                  let kf = Extlib.the kf in
                  let v0 = Kernel_function.get_vi kf in
                  List.exists
                    (fun glob -> match glob with
                      | GFun ({svar =v1},_ ) -> Varinfo.equal v1 v0
                      |  _ -> false)
                    globs
            in
            if List.exists in_globals result then [`STOCK_ID "gtk-apply"]
            else [`STOCK_ID ""])
      (fun () -> Enabled.get ());
  !update_column `Visibility

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
