(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

let show main_ui =
  let authors = [
    "Patrick Baudin";
    "François Bobot";
    "Richard Bonichon";
    "David Bühler";
    "Loïc Correnson";
    "Julien Crétin";
    "Pascal Cuoq";
    "Zaynah Dargaye";
    "Jean-Christophe Filliâtre";
    "Philippe Herrmann";
    "Florent Kirchner";
    "Matthieu Lemerre";
    "David Maison";
    "Claude Marché";
    "André Maroneze";
    "Benjamin Monate";
    "Yannick Moy";
    "Anne Pacalet";
    "Valentin Perrelle";
    "Guillaume Petiot";
    "Virgile Prevosto";
    "Armand Puccetti";
    "Muriel Roger";
    "Julien Signoles";
    "Boris Yakobowski"
  ]
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
    "Licenses of the Frama-C kernel and plug-ins are either under LGPL v2.1, \
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
      ~version:Config.version
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
  with
  | Not_found -> () (* ignore: raised because of a buggy lablgtk2 *)
  | Failure msg as e ->
    if msg = "dialog destroyed" then
      (* ignore: raised because of a buggy lablgtk2 *)
      ()
    else raise e

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
compile-command: "make -C ../../.."
End:
*)
