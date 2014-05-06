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

type ('a, 'b, 'c) metrics_panel = {
  top : 'a option;
  bottom : 'b option;
  actions : 'c list;
}
;;

(* The option type for top and bottom GTK objects is compulsory in order not to
   have warnings at runtim.
   Creation of GTK objects cannot be made before the general window is
   initialized.
   The option type with a None value marks the fact that this value is not
   initialized either (it will only be at register time).
*)
let get_panel, set_panel, add_panel_action =
  let panel = ref {
    top = None;
    bottom = None;
    actions = [];
  } in
  (fun () -> !panel),
  (fun top_widget bottom_widget ->
    panel := { top = top_widget; bottom = bottom_widget; actions = []; }
  ),
  (fun action -> panel := { !panel with actions = action :: !panel.actions; })
;;

(** Display the [table_contents] matrix as a GTK table *)
let display_as_table table_contents (parent:GPack.box) =
  let table = GPack.table
    ~columns:(List.length (List.hd table_contents))
    ~rows:(List.length table_contents)
    ~homogeneous:true
    ~packing:parent#pack () in
  Extlib.iteri (fun i row ->
    Extlib.iteri (fun j text ->
      table#attach ~left:j ~top:i
        ((GMisc.label ~justify:`LEFT ~text:text ()):>GObj.widget)) row)
    table_contents ;
;;


(** Remove all sub-elements of a GUI object *)
let clear_container w = List.iter (fun c -> c#destroy ()) w#children ;;

(** The panel of Metrics has two parts:
    - The upper part contains the various choices of the user;
    - The bottom part displays the result.
*)
let init_panel () =
  let v = GPack.vbox () in
  (* Titles, buttons, and headers *)
  let up = GPack.hbox ~width:120 ~packing:(v#pack ~expand:true) () in
  (* Results *)
  let bottom = GPack.vbox ~width:120 ~packing:(v#pack ~expand:true) () in

  let choices = GEdit.combo_box_text ~active:0 ~strings:[] ~packing:(up#pack) ()
  in
  let launch_button = GButton.button ~label:"Launch metrics"
    ~packing:(up#pack) ()
  in
  ignore(launch_button#connect#clicked (fun () ->
    let actions = (get_panel ()).actions in
    let sopt = GEdit.text_combo_get_active choices in
    match sopt with
      | None -> ()
      | Some s ->
        if List.mem_assoc s actions then
          let action = List.assoc s actions in
          clear_container bottom;
          action bottom;
        else ()
  ) );
  set_panel (Some choices) (Some bottom);
  v
;;

let reset_panel _ =
  let metrics_panel = get_panel () in
  match metrics_panel.bottom with
    | None -> ()
    | Some b -> clear_container b;
;;


(** Returning a value to register in Frama-C's GUI *)
let coerce_panel_to_ui panel_box _main_ui = "Metrics", panel_box#coerce, None ;;

(** Add a new metrics to its dedicated panel.
    The text is added to the combox box while the action is added to the
    association lists of possible actions.
*)
let register_metrics name display_function =
  add_panel_action (name, display_function);
  let metrics_panel = get_panel () in
  GEdit.text_combo_add (Extlib.the metrics_panel.top) name;
;;
