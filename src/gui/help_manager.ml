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

let show main_ui =
  let authors =
    [ "Patrick Baudin" ;
      "François Bobot" ;
      "Richard Bonichon";
      "Loïc Correnson";
      "Pascal Cuoq";
      "Zaynah Dargaye";
      "Jean-Christophe Filliâtre";
      "Philippe Herrmann";
      "Florent Kirchner";
      "Matthieu Lemerre";
      "Claude Marché";
      "Benjamin Monate";
      "Yannick Moy";
      "Anne Pacalet";
      "Virgile Prévosto";
      "Julien Signoles";
      "Boris Yakobowski" ]
  in
  let copyright (* should be automatically generated *) =
    "\t © CEA and INRIA for the Frama-C kernel\n\
\t © CEA for the GUI and plug-ins constant propagation, from, inout, impact, \
metrics, occurrence pdg, postdominators, scope, security_slicing, \
semantic callgraph, slicing, sparecode, syntactic callgraph, users and value.\n\
\n\
See the particular header of each source file for details."
  in
  let license (* should be automatically generated *) =
    "Licences of the Frama-C kernel and plug-ins are either under LGPL v2.1, \
or BSD.\n\
See the particular header of each source file for details."
  in
  let dialog =
    GWindow.about_dialog
      ~parent:main_ui#main_window
      ?icon:Gtk_helper.framac_icon
      ?logo:Gtk_helper.framac_logo
      ~name:"Frama-C"
      ~authors
      ~copyright
      ~license
      ~website:"http://frama-c.com"
      ~website_label:"Questions and support"
      ~version:(Config.version ^ "\nBuilt on " ^ Config.date)
      ~comments:"Frama-C is a suite of tools dedicated to the analysis of the \
source code of software written in C."
      ()
  in
  (*  Buggy labgtk2 prevents this from working...*)
  ignore
    (dialog#connect#response
       ~callback:(fun _ -> try
                    dialog#coerce#destroy ()
                  with Not_found -> ()));
  try
    ignore (dialog#run ())
  with Not_found | Failure "dialog destroyed" ->
    (* raised because of a buggy lablgtk2 *)
    ()

(** Register this dialog in main window menu bar *)
let () =
  Design.register_extension
    (fun window ->
       let menu_manager = window#menu_manager () in
       let _helpitem, helpmenu =
         menu_manager#add_menu "_Help"
           ~pos:(List.length menu_manager#factory#menu#children)
       in
(*       helpitem#set_right_justified true;*)
       ignore
         (menu_manager#add_entries
            helpmenu
            [ Menu_manager.menubar ~icon:`ABOUT "About"
                (Menu_manager.Unit_callback (fun () -> show window));
            ]);
    )

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
