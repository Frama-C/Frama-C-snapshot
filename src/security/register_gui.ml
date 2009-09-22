(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: register_gui.ml,v 1.5 2008-12-21 17:34:22 uid528 Exp $ *)

open Pretty_source
open Gtk_helper
open Db
open Cil_types

module Make_HighlighterState(Info:sig val name: string end) =
  Computation.Ref
    (struct include Cil_datatype.StmtList let default () = [] end)
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
  | PVDecl _ | PTermLval _ | PLval _ | PCodeAnnot _ | PGlobal _ 
  | PBehavior _ | PPredicate _ -> ()

let security_selector
    (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
  if button = 3 && Options.is_on () then
    match localizable with
    | PStmt (_kf,ki) ->
        ignore
          (popup_factory#add_item "_Security component"
             ~callback:
             (fun () ->
                ForwardHighlighterState.set
		  (!Db.Security.get_forward_component ki);
                IndirectBackwardHighlighterState.set
		  (!Db.Security.get_indirect_backward_component ki);
		DirectHighlighterState.set 
		  (!Db.Security.get_direct_component ki);
		main_ui#rehighlight ()))
    | _ ->
	()

let main main_ui = 
  main_ui#register_source_selector security_selector;
  main_ui#register_source_highlighter security_highlighter    

let () = Design.register_extension main

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.. -j"
  End:
*)
