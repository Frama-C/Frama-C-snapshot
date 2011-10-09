(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Pretty_source
open Cil_types
open Cil_datatype

let update_visibility = ref (fun () -> ())

let add_tag buffer (name, tag_prop) start stop =
  let tag = Gtk_helper.make_tag buffer ~name tag_prop in
  Gtk_helper.apply_tag buffer tag start stop

let scope_start_tag = ("startscope", [`UNDERLINE `DOUBLE])
let zones_used_tag = ("zones",  [`BACKGROUND "#FFeeCC"])
let show_def_tag = ("show_def", [`BACKGROUND "#FFca63"])
let scope_b_tag = ("b_scope",   [`BACKGROUND "#CCFFff"])
let scope_fb_tag = ("fb_scope", [`BACKGROUND "#CCFFee"])
let scope_f_tag = ("f_scope",   [`BACKGROUND "#CCFFbb"])
let scope_p_tag = ("p_scope",   [`BACKGROUND "#FFFFab"])
let scope_p_warn_tag = ("p_warn_scope",   [`BACKGROUND "#D5FFAb"])
let empty_tag = ("", [])

let add_msg (main_ui:Design.main_window_extension_points) txt =
  main_ui#annot_window#buffer#insert (txt ^ "\n")

let pretty_zone fmt z =
  Format.fprintf fmt "@[<h 1>%a@]" Locations.Zone.pretty z

let ask_for_lval (main_ui:Design.main_window_extension_points) kf stmt =
  let txt = GToolbox.input_string ~title:"Input lvalue expression" "" in
    match txt with None | Some "" -> None
      | Some txt ->
          try
            let term_lval = !Db.Properties.Interp.lval kf stmt txt in
            let lval =
              !Db.Properties.Interp.term_lval_to_lval ~result:None term_lval
            in
              Some (txt, lval)
          with e ->
            main_ui#error "[ask for lval] '%s' invalid expression: %s@."
              txt (Printexc.to_string e);
            None

let get_kf_stmt_opt localizable =
  match (Pretty_source.kf_of_localizable localizable,
         Pretty_source.ki_of_localizable localizable)
  with
      Some kf, Kstmt st -> Some(kf,st)
    | Some _, Kglobal | None, _ -> None

let get_annot_opt localizable = match localizable with
  | Pretty_source.PIP(Property.IPCodeAnnot(_,_,annot)) -> Some annot
  | _ -> None


(** [kf_stmt_opt] is used if we want to ask the lval to the user in a popup *)
let get_lval_opt main_ui kf_stmt_opt localizable =
  match localizable with
    | Pretty_source.PLval (Some _kf, (Kstmt _stmt), lv) ->
        let lv_txt = Pretty_utils.sfprintf "%a" Cil.d_lval lv in
        Some (lv_txt, lv)
    | _ ->
       ( match kf_stmt_opt with
         None -> None
       | Some (kf, stmt) ->
              match (ask_for_lval main_ui kf stmt) with
                None -> None
              | Some (lv_txt, lv) -> Some (lv_txt, lv))

module Kf_containing_highlighted_stmt =
  Kernel_function.Make_Table
      (Datatype.Unit)
      (struct
        let name = "Dpds_gui.Kf_containing_highlighted_stmt"
        let size = 7
        let dependencies = 
	  [ (*Dependencies are managed manually by Make_StmtSetState*) ]
        let kind = `Internal
       end)


module Make_StmtSetState (Info:sig val name: string end) =
  struct include State_builder.Ref
    (Stmt.Set)
    (struct
       let name = Info.name
       let dependencies = [ Db.Value.self ]
       let kind = `Internal
       let default () = Stmt.Set.empty
     end)
	
   let set s = 
     set s;
     Kf_containing_highlighted_stmt.clear ();
     Stmt.Set.iter 
       (fun stmt -> 
	 Kf_containing_highlighted_stmt.replace 
	   (Kernel_function.find_englobing_kf stmt) ())
       s;
     !update_visibility ()

  end
module type DpdCmdSig = sig
  type t_in
  val help : string
  val get_info : (Kernel_function.t * Cil_types.stmt) option -> string
  val compute : Kernel_function.t -> Cil_types.stmt -> t_in -> string
  val tag_stmt : Cil_types.stmt -> (string * GText.tag_property list)
  val clear: unit -> unit
end

module DataScope : (DpdCmdSig with type t_in = lval)  = struct

  type t_in = lval

  module Fscope =
    Make_StmtSetState
      (struct let name = "Dpds_gui.Highlighter.Fscope" end)

  module FBscope =
    Make_StmtSetState
      (struct let name = "Dpds_gui.Highlighter.FBscope" end)

  module Bscope =
    Make_StmtSetState
      (struct let name = "Dpds_gui.Highlighter.Bscope" end)

  let clear () = Fscope.clear(); FBscope.clear(); Bscope.clear()

  let help = ("[data_scope] "
      ^"highlight the statements where the value of D is the same "
      ^"than at its value at L.\n\t"
      ^"For more information, please look at the Scope plugin documentation.")

  let get_info _kf_stmt_opt =
    if Stmt.Set.is_empty (Fscope.get ())
      && Stmt.Set.is_empty (FBscope.get ())
      && Stmt.Set.is_empty (Bscope.get ())
    then ""
    else "[scope] selected"

  let compute kf stmt lval =
    let f, (fb, b) = !Db.Scope.get_data_scope_at_stmt kf stmt lval in
    Fscope.set f; FBscope.set fb; Bscope.set b;
    "[scope] computed"

  let tag_stmt stmt =
    if Stmt.Set.mem stmt (Fscope.get()) then scope_f_tag
    else if Stmt.Set.mem stmt (FBscope.get()) then scope_fb_tag
    else if Stmt.Set.mem stmt (Bscope.get()) then scope_b_tag
    else empty_tag

end

module Pscope (* : (DpdCmdSig with type t_in = code_annotation) *) = struct

  type t_in = code_annotation

  module Pscope =
    Make_StmtSetState
      (struct let name = "Dpds_gui.Highlighter.Pscope" end)

  module Pscope_warn =
    State_builder.List_ref
      (Code_annotation)
      (struct
        let name = "Dpds_gui.Highlighter.Pscope_warn"
        let dependencies = [ Db.Value.self ]
        let kind = `Internal
       end)

  let clear () = Pscope.clear(); Pscope_warn.clear()

  let help = ("[prop_scope] "
      ^"highlight the statements where the value of the assertion is also ok\n\t"
      ^"For more information, please look at the Scope plugin documentation.")

  let get_info _kf_stmt_opt =
    if Stmt.Set.is_empty (Pscope.get ())
    then ""
    else "[prop_scope] selected"

  let compute kf stmt annot =
    let s1, s2 = !Db.Scope.get_prop_scope_at_stmt kf stmt annot in
    Pscope.set s1; Pscope_warn.set s2;
    "[prop_scope] computed"

  let tag_stmt stmt =
    (*if Cilutil.StmtSet.mem stmt (Pscope_warn.get()) then scope_p_warn_tag
    else*) if Stmt.Set.mem stmt (Pscope.get()) then scope_p_tag
    else empty_tag

  let tag_annot annot =
    let tag =
      List.exists (fun a -> a.annot_id = annot.annot_id) (Pscope_warn.get())
    in if tag then scope_p_warn_tag else empty_tag
end

module ShowDef : (DpdCmdSig with type t_in = lval) = struct

  type t_in = lval

  module ShowDefState =
    Make_StmtSetState
      (struct let name = "Dpds_gui.Highlighter.ShowDef" end)

  let clear () = ShowDefState.clear()

  let help = ("[show_def] "
      ^"highlight the statements that define the value of D at L,\n\t"
      ^"and print a message if a part of D might be undefined.\n\t"
      ^"Notice that 'undefined' only means here "
      ^"not defined on some path from the beginning of the function.")


  let get_info _kf_stmt_opt =
    if Stmt.Set.is_empty (ShowDefState.get()) then  ""
    else "[show_def] selected"
      
  let compute kf stmt lv =
    match !Db.Scope.get_defs kf stmt lv with
      | None -> clear (); "[show_def] nothing found..."
      | Some (defs, undef) ->
          let msg = match undef with
            | None -> "[show_def] computed"
            | Some undef ->
                Pretty_utils.sfprintf "[show_def] notice that %a %s"
                  pretty_zone undef
                  "can be undefined by this function at this point"
          in
            ShowDefState.set defs; msg

  let tag_stmt stmt =
    if Stmt.Set.mem stmt (ShowDefState.get())
    then show_def_tag else empty_tag

end

module Zones : (DpdCmdSig with type t_in = lval)  = struct

  type t_in = lval

  module ZonesState =
    struct include State_builder.Option_ref
      (Datatype.Pair
         (Stmt.Hashtbl.Make(Locations.Zone))
         (Stmt.Set))
      (struct
         let name = "Dpds_gui.Highlighter.ZonesState"
         let dependencies = [ Db.Value.self ]
         let kind = `Internal
       end)
    let set s = 
      set s;
      Kf_containing_highlighted_stmt.clear ();
      Stmt.Set.iter 
	(fun stmt -> 
	  Kf_containing_highlighted_stmt.replace 
	    (Kernel_function.find_englobing_kf stmt) ())
	(snd s);
     !update_visibility ()
    end
  let clear () = ZonesState.clear ()

  let help =
    ("[zones] computes, for each point Li of the function, "
      ^"the data Di needed to know the value of D at L.\n"
      ^"\tAfter this computation, the result Di will be printed in the "
     ^" information window each time a statement Li is selected.")

  let get_info kf_stmt_opt =
    try
      let zones, _ = ZonesState.get () in
        match kf_stmt_opt with
          | None -> "[zones] no information for this point"
          | Some (_kf, stmt) ->
              let z = !Db.Scope.get_zones zones stmt in
              let txt =
                Pretty_utils.sfprintf "[zones] needed before stmt %d = %a"
                  stmt.sid pretty_zone z
              in txt
    with Not_found -> ""

  let compute kf stmt lval =
    let used_stmts, zones = !Db.Scope.build_zones kf stmt lval in
      ZonesState.set (zones, used_stmts);
      "[zones] computed"

  let tag_stmt stmt =
    let is_used =
      try
        let _zones, used =  ZonesState.get () in
        Stmt.Set.mem stmt used
      with Not_found -> false
    in
      if is_used then zones_used_tag else empty_tag

end

let help (main_ui:Design.main_window_extension_points) =
  let add txt = add_msg main_ui txt in
    add ("General : "
     ^"each of these commands starts from a data D at a program point L.\n\t"
     ^"The program point is the one that is before the selected statement,\n\t"
       ^"and the data is the one that is selected if any, "
       ^"or it can be given via a popup.\n"
       ^"\tIf the text given in the popup is empty, or 'Cancel' is chosen, "
       ^"the selection of the command is reseted.");
    add (ShowDef.help);
    add (Zones.help);
    add (DataScope.help);
    add (Pscope.help);
    add ("All : call the 3 commands on the same D and L.");
    add ("Reset : reset the internal state for all the previous commands.")

module DpdsState =
  State_builder.Option_ref
    (Datatype.Triple(Kernel_function)(Stmt)(Datatype.String))
    (struct
       let name = "Dpds_gui.Highlighter.DpdsState"
       let dependencies = [ Db.Value.self ]
       let kind = `Internal
     end)

let reset () =
  DpdsState.clear ();
  ShowDef.clear ();
  Zones.clear ();
  DataScope.clear ();
  Pscope.clear ();
  Kf_containing_highlighted_stmt.clear ();
  !update_visibility ()


let print_info main_ui kf_stmt_opt =
  try
    let kf_dpds, _s, txt = DpdsState.get () in
      add_msg main_ui txt;
      match kf_stmt_opt with
        | None -> ()
        | Some (kf, _s) ->
            if Kernel_function.equal kf kf_dpds then
              begin
              let get f =
                let msg = f kf_stmt_opt in
                  if msg <> "" then add_msg main_ui msg
              in
                get ShowDef.get_info;
                get Zones.get_info;
                get DataScope.get_info;
                get Pscope.get_info
              end
            else add_msg main_ui
                   "[dependencies] no information in this function"
  with Not_found -> ()

let callbacks ?(defs=false) ?(zones=false) ?(scope=false) ?(pscope=false)
    main_ui (kf, stmt, localizable) =
  let compute f arg =

    let msg = f kf stmt arg in

    if msg <> "" then add_msg main_ui msg
  in
  let set_txt x =
    let txt = Pretty_utils.sfprintf
      "[dependencies] for %s before stmt %d in %a"
      x stmt.sid Kernel_function.pretty kf
    in
    DpdsState.set (kf, stmt, txt);
    add_msg main_ui txt
  in
  let _ =
    if pscope then begin
      reset ();
      match get_annot_opt localizable with
      | Some ({annot_content = (AAssert _)} as annot) ->
          begin
            set_txt ("annotation "^(string_of_int annot.annot_id));
            compute Pscope.compute annot
          end
      | _ -> ()
    end
    else begin
      Pscope.clear ();
      match get_lval_opt main_ui (Some(kf, stmt)) localizable with
      | None -> reset ()
      | Some (lval_txt, lval) ->
          begin
            set_txt lval_txt;
            if defs then compute ShowDef.compute lval else ShowDef.clear ();
            if zones then compute Zones.compute lval else Zones.clear ();
            if scope then compute DataScope.compute lval else DataScope.clear ()
          end
    end
  in main_ui#rehighlight ()

let highlighter (buffer:GSourceView2.source_buffer) localizable ~start ~stop =
  try
    let _kf, start_s, _txt = DpdsState.get () in
    let put_tag tag = match tag with ("",[]) -> ()
      | _ -> add_tag buffer tag start stop
    in
    match localizable with
    | PStmt (_,stmt) ->
      if start_s.sid = stmt.sid then put_tag scope_start_tag;
      put_tag (Pscope.tag_stmt stmt);
      put_tag (DataScope.tag_stmt stmt);
      put_tag (Zones.tag_stmt stmt );
      put_tag (ShowDef.tag_stmt stmt)
    | PIP (Property.IPCodeAnnot (_, _, annot)) ->
      put_tag (Pscope.tag_annot annot)
    | PVDecl _ | PTermLval _ | PLval _ | PGlobal _ | PIP _ -> ()
  with Not_found -> ()

let check_value (main_ui:Design.main_window_extension_points) =
  if Db.Value.is_computed () then true
  else
    let answer = GToolbox.question_box
      ~title:("Need Value Analysis")
      ~buttons:[ "Run"; "Cancel" ]
      ("Value analysis has to be run first.\nThis can take some time.\n"
       ^"Do you want to run the value analysis now ?")
    in
      if answer = 1 then
        match main_ui#full_protect ~cancelable:true !Db.Value.compute with
          | Some _ -> true
          | None -> false
      else false


(** To add a sensitive/unsensitive menu item to a [factory].
* The menu item is insensitive when [arg_opt = None],
* else, when the item is selected, the callback is called with the argument.
* If [~use_values], check if the value analysis has been computed.
 *)
let add_item (main_ui:Design.main_window_extension_points)
      ~use_values (factory:GMenu.menu GMenu.factory) name arg_opt callback =
    match arg_opt with
      | None -> (* add the menu item, but it isn't sensitive *)
          let item = factory#add_item name ~callback: (fun () -> ())
          in item#misc#set_sensitive false
      | Some arg -> (* add the menu item with its callback *)
          let cb arg =
            if use_values then
              if check_value main_ui then callback arg else ()
            else callback arg
          in
          ignore (factory#add_item name ~callback: (fun () -> cb arg))

let selector (popup_factory:GMenu.menu GMenu.factory)
             (main_ui:Design.main_window_extension_points)
             ~button localizable =
  if button = 3 then
    begin
      let submenu = popup_factory#add_submenu "Dependencies" in
      let submenu_factory = new GMenu.factory submenu in

        add_item main_ui ~use_values:false submenu_factory
          "Help" (Some()) (fun _ -> help main_ui) ;

        ignore (submenu_factory#add_separator ());

      let kf_stmt_opt = get_kf_stmt_opt localizable in
      let arg = match kf_stmt_opt with None -> None
        | Some (kf, stmt) -> Some (kf, stmt, localizable)
      in
      let add_zones_item name cb =
        add_item main_ui ~use_values:true
          submenu_factory name arg (cb main_ui) in

        add_zones_item "Show defs" (callbacks ~defs:true);
        add_zones_item "Zones"     (callbacks ~zones:true);
        add_zones_item "DataScope" (callbacks ~scope:true);
        add_zones_item "PropScope" (callbacks ~pscope:true);

        ignore (submenu_factory#add_separator ());
        add_zones_item "Show All"
                       (callbacks ~defs:true ~zones:true  ~scope:true);

        add_item main_ui ~use_values:false submenu_factory "Reset All" (Some())
                        (fun _ -> reset () ; main_ui#rehighlight ())
    end
  else if button = 1 then
      print_info main_ui (get_kf_stmt_opt localizable)

let filetree_decorate main_ui = 
  main_ui#file_tree#append_pixbuf_column
    ~title:"Scope"
    (fun globs -> 
      let is_hilighted = function
        | GFun ({svar = v }, _) ->
          Kf_containing_highlighted_stmt.mem (Globals.Functions.get v)
        |  _ -> false
      in
      let id =
          (* lazyness of && is used for efficiency *)
        if (Kf_containing_highlighted_stmt.length () <> 0)
	  && List.exists is_hilighted globs 
	then "gtk-apply"
        else ""
      in
      [ `STOCK_ID id ])
    (fun _ -> Kf_containing_highlighted_stmt.length () <>0)
    
let main main_ui =
  main_ui#register_source_selector selector;
  main_ui#register_source_highlighter highlighter;
  update_visibility := (filetree_decorate main_ui)
    
let () = Design.register_extension main
