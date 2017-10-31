(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
open Cil_types
open Cil_datatype

let update_column = ref (fun _ -> ())

let add_tag buffer (name, tag_prop) start stop =
  let tag = Gtk_helper.make_tag buffer ~name tag_prop in
  Gtk_helper.apply_tag buffer tag start stop

let studia_start_tag = ("startstudia", [`UNDERLINE `DOUBLE])
let show_direct_tag = ("show_direct", [`BACKGROUND "#FFca63"])
let show_indirect_tag = ("show_indirect", [`BACKGROUND "#FFdb74"])
let empty_tag = ("", [])

let ask_for_lval (main_ui:Design.main_window_extension_points) kf =
  let txt = GToolbox.input_string ~title:"Input lvalue expression" "" in
  match txt with None | Some "" -> None
  | Some txt ->
    try
      let term_lval = !Db.Properties.Interp.term_lval kf txt in
      Some (txt, term_lval)
    with e ->
      main_ui#error "[ask for lval] '%s' invalid expression: %s@."
        txt (Printexc.to_string e);
      None

(** [kf_stmt_opt] is used if we want to ask the lval to the user in a popup *)
let get_lval_opt main_ui kf localizable =
  match localizable with
  | Pretty_source.PLval (Some _kf, (Kstmt _stmt), lv) ->
    let lv_txt = Pretty_utils.to_string Printer.pp_lval lv in
    let tlv = Logic_utils.lval_to_term_lval ~cast:false lv in
    Some (lv_txt, tlv)
  | Pretty_source.PTermLval (Some _kf, (Kstmt _stmt), _, tlv) ->
    let tlv_txt = Pretty_utils.to_string Printer.pp_term_lval tlv in
    Some (tlv_txt, tlv)
  | _ ->
    match ask_for_lval main_ui kf with
    | None -> None
    | Some (lv_txt, lv) -> Some (lv_txt, lv)

let eval_tlval =
  let typ_lval_to_zone_gui = Datatype.func2 Stmt.ty Term.ty Locations.Zone.ty in
  Dynamic.get ~plugin:"Value" "tlval_to_zone_gui" typ_lval_to_zone_gui


module Kfs_containing_highlighted_stmt =
  Kernel_function.Make_Table
      (Datatype.String.Set)
      (struct
        let name = "Studia.Kf_containing_highlighted_stmt"
        let size = 7
        let dependencies = 
	  [ (*Dependencies are managed manually by Make_StmtSetState*) ]
       end)

let default_icon_name = "gtk-apply"
let default_icon = Datatype.String.Set.singleton default_icon_name


module Make_StmtMapState (Info:sig val name: string end) =
  struct
    module D = Datatype
    include State_builder.Ref
    (Stmt.Map.Make(Datatype.String.Set))
    (struct
       let name = Info.name
       let dependencies = [ Db.Value.self ]
       let default () = Stmt.Map.empty
     end)

   let set s =
     set s;
     Kfs_containing_highlighted_stmt.clear ();
     Stmt.Map.iter
       (fun stmt s ->
         let kf = Kernel_function.find_englobing_kf stmt in
         let prev =
           try Kfs_containing_highlighted_stmt.find kf
           with Not_found -> D.String.Set.empty
         in
         let union = D.String.Set.union prev s in
	 Kfs_containing_highlighted_stmt.replace kf union)
       s;
     !update_column `Contents

  end

(*
module type StudiaCmdSig = sig
  type input
  val help : string
  val compute : Kernel_function.t -> Cil_types.stmt -> input -> string
  (** Returns a text to be displayed in the GUI  *)
  val tag_stmt : Cil_types.stmt -> (string * GText.tag_property list)
  val clear: unit -> unit
end
 *)

module WritesOrReads =
struct

  (* type input = term_lval *)

  module State =
    Make_StmtMapState
      (struct let name = "Studia.Highlighter.WritesOrRead" end)

  let clear () = State.clear()

  let help_writes = ("[writes] "
      ^"highlight the statements that writes to the location pointed to \
        by D at L")
  let help_reads = ("[reads] "
      ^"highlight the statements that reads the location pointed to \
        by D at L")

  let indirect_icon = Datatype.String.Set.singleton "gtk-jump-to"

  let conv l =
    let aux acc (stmt, effects) =
      let empty = Datatype.String.Set.empty in
      let direct = if effects.Writes.direct then default_icon else empty in
      let indirect = if effects.Writes.indirect then indirect_icon else empty in
      let s = Datatype.String.Set.union direct indirect in
      if Datatype.String.Set.is_empty s then acc else Stmt.Map.add stmt s acc
    in
    List.fold_left aux Stmt.Map.empty l

  let compute op _kf stmt tlv =
    let t = Logic_const.term (TLval tlv) (Cil.typeOfTermLval tlv) in
    let z = eval_tlval stmt t in
    let r, s = match op with
      | `Reads -> Reads.compute z, "Reads"
      | `Writes -> Writes.compute z, "Writes"
    in
    Options.feedback "%s computed" s;
    match r with
      | [] -> clear (); s ^ " computed; no statement found."
      | defs -> State.set (conv defs); s ^ " computed"

  let tag_stmt stmt =
    try
      let s = Stmt.Map.find stmt (State.get()) in
      if Datatype.String.Set.mem default_icon_name s
      then show_direct_tag else show_indirect_tag
    with Not_found -> empty_tag

end

let help (main_ui:Design.main_window_extension_points) =
  main_ui#pretty_information "%s@." WritesOrReads.help_writes;
  main_ui#pretty_information "%s@." WritesOrReads.help_reads;
  main_ui#pretty_information "%s@."
    "Reset : reset the internal state for all the previous commands."

module StudiaState =
  State_builder.Option_ref
    (Stmt)
    (struct
       let name = "Studia.Highlighter.StudiaState"
       let dependencies = [ Db.Value.self ]
     end)

let reset () =
  StudiaState.clear ();
  WritesOrReads.clear ();
  Kfs_containing_highlighted_stmt.clear ();
  !update_column `Contents

let callback op (main_ui:Design.main_window_extension_points) (kf, stmt, localizable) =
  let compute f arg =
    let msg = f kf stmt arg in
    if msg <> "" then main_ui#pretty_information "%s@." msg
  in
  begin
    match get_lval_opt main_ui kf localizable with
    | None -> reset ()
    | Some (lval_txt, lval) ->
      begin
        let txt = Format.asprintf "[studia] query %s" lval_txt in
        StudiaState.set stmt;
        main_ui#pretty_information "%s@." txt;
        compute (WritesOrReads.compute op) lval
      end;
  end;
  main_ui#rehighlight ()

let highlighter (buffer:Design.reactive_buffer) localizable ~start ~stop =
  try
    let start_s = StudiaState.get () in
    let put_tag tag = match tag with
      | ("",[]) -> ()
      | _ -> add_tag buffer#buffer tag start stop
    in
    match localizable with
    | PStmt (_,stmt) ->
      if start_s.sid = stmt.sid then put_tag studia_start_tag;
      put_tag (WritesOrReads.tag_stmt stmt);
    | _ -> ()
  with Not_found -> ()

let check_value (main_ui:Design.main_window_extension_points) =
  Db.Value.is_computed () ||
    let answer = GToolbox.question_box
      ~title:("Need Value Analysis")
      ~buttons:[ "Run"; "Cancel" ]
      ("Value analysis has to be run first.\nThis can take some time.\n"
       ^"Do you want to run the value analysis now ?")
    in
    answer = 1 &&
        match main_ui#full_protect ~cancelable:true !Db.Value.compute with
        | Some _ -> true
        | None -> false


(** To add a sensitive/unsensitive menu item to a [factory].
    The menu item is insensitive when [arg_opt = None],
    else, when the item is selected, the callback is called with the argument.
    If [uses_value] is true, check if the value analysis has been computed.
 *)
let add_item (main_ui:Design.main_window_extension_points) ~uses_value menu name arg_opt callback =
  (* add the menu item *)
  let item = GMenu.menu_item ~label:name () in
  menu#add item;
  match arg_opt with
  | None -> (* item must not be sensitive *)
    item#misc#set_sensitive false
  | Some arg -> (* add callback to the menu item *)
    let callback () =
      if not uses_value || check_value main_ui then callback arg else ()
    in
    ignore (item#connect#activate ~callback);
    (* See http://stackoverflow.com/questions/5221326/submenu-item-does-not-call-function-with-working-solution/15309826 *)
    ignore (item#event#connect#button_press
              (fun evt ->
                 if GdkEvent.Button.button evt = 1
                 then (callback (); true)
                 else false))

let selector (popup_factory:GMenu.menu GMenu.factory)
             (main_ui:Design.main_window_extension_points)
             ~button localizable =
  if button = 3 then begin
    let submenu = popup_factory#add_submenu "Studia" in
    let submenu_factory = new GMenu.factory submenu in
    add_item main_ui ~uses_value:false submenu
      "Help" (Some()) (fun _ -> help main_ui) ;
    ignore (submenu_factory#add_separator ());
    let arg = match (Pretty_source.kf_of_localizable localizable,
                     Pretty_source.ki_of_localizable localizable)
      with
      | Some kf, Kstmt stmt -> Some (kf, stmt, localizable)
      | Some _, Kglobal | None, _ -> None
    in
    let add_menu_item name callback =
      add_item main_ui ~uses_value:true submenu name arg
        (fun arg ->
          main_ui#protect ~cancelable:true (fun () -> callback main_ui arg))
    in
    add_menu_item "Writes" (callback `Writes);
    add_menu_item "Reads" (callback `Reads);
    ignore (submenu_factory#add_separator ());
    add_item main_ui ~uses_value:false submenu "Reset All" (Some())
      (fun _ -> reset () ; main_ui#rehighlight ())
  end

let filetree_decorate main_ui = 
  main_ui#file_tree#append_pixbuf_column
    ~title:"Studia"
    (fun globs ->
      let icons = function
        | GFun ({svar = v }, _) ->
          (try Kfs_containing_highlighted_stmt.find  (Globals.Functions.get v)
           with Not_found -> Datatype.String.Set.empty)
        |  _ -> Datatype.String.Set.empty
      in
      let ids =
        if Kfs_containing_highlighted_stmt.length () <> 0 then
          let icons = List.fold_left
            (fun acc glob -> Datatype.String.Set.union (icons glob) acc)
            Datatype.String.Set.empty globs
          in
          if Datatype.String.Set.is_empty icons
          then Datatype.String.Set.singleton ""
          else icons
        else
          Datatype.String.Set.singleton ""
      in
      let icons =
        if Datatype.String.Set.mem default_icon_name ids then
          [default_icon_name]
        else
          Datatype.String.Set.elements
            (Datatype.String.Set.remove default_icon_name ids)
      in
      List.map (fun icon -> `STOCK_ID icon) icons
    )
    (fun _ -> Kfs_containing_highlighted_stmt.length () <> 0)
    
let main main_ui =
  main_ui#register_source_selector selector;
  main_ui#register_source_highlighter highlighter;
  update_column := (filetree_decorate main_ui)
    
let () = Design.register_extension main
