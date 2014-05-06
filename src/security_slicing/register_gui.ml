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

module Make_HighlighterState(Info:sig val name: string end) =
  State_builder.List_ref
    (Cil_datatype.Stmt)
    (struct
       let name = Info.name
       let dependencies = [ Ast.self ]
     end)

module ForwardHighlighterState =
  Make_HighlighterState(struct let name = "Security_gui.Forward" end)

module IndirectBackwardHighlighterState =
  Make_HighlighterState(struct let name = "Security_gui.Indirectb" end)

module DirectHighlighterState =
  Make_HighlighterState(struct let name = "Security_gui.Direct" end)

let security_highlighter buffer loc ~start ~stop =
  match loc with
  | PStmt (_,s) ->
      let f = ForwardHighlighterState.get () in
      if List.exists (fun k -> k.sid=s.sid) f then begin
        let tag = make_tag buffer"forward" [`BACKGROUND "orange" ] in
        apply_tag buffer tag start stop end;
      let i = IndirectBackwardHighlighterState.get () in
      if List.exists (fun k -> k.sid=s.sid) i then begin
        let tag = make_tag buffer"indirect_backward" [`BACKGROUND  "cyan" ] in
        apply_tag buffer tag start stop end;
      let d = DirectHighlighterState.get () in
      if List.exists (fun k -> k.sid=s.sid) d then begin
        let tag = make_tag buffer"direct" [`BACKGROUND  "green" ] in
        apply_tag buffer tag start stop end
  | PVDecl _ | PTermLval _ | PLval _ | PGlobal _ | PIP _ -> ()

let security_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  if button = 3 && Security_slicing_parameters.Slicing.get () then
    match localizable with
    | PStmt (_kf, ki) ->
        ignore
          (popup_factory#add_item "_Security component"
             ~callback:
             (fun () ->
                ForwardHighlighterState.set
                  (Components.get_forward_component ki);
                IndirectBackwardHighlighterState.set
                  (Components.get_indirect_backward_component ki);
                DirectHighlighterState.set
                  (Components.get_direct_component ki);
                main_ui#rehighlight ()))
    | _ -> ()

let main main_ui =
  main_ui#register_source_selector security_selector;
  main_ui#register_source_highlighter security_highlighter

let () = Design.register_extension main

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)
