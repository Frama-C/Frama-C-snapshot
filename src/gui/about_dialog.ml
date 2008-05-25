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

let show () =
  try 
  let dialog = 
    GWindow.about_dialog 
      ~name:"ValViewer" 
      ~authors:["Patrick Baudin" ; 
                "Pascal Cuoq"; 
                "Jean-Christophe FilliÃ¢tre"; 
                "Claude MarchÃ©";
                "Benjamin Monate"; 
                "Yannick Moy";
                "Virgile PrÃ©vosto";
                "Julien Signoles";
               ]
      ~copyright:"Copyright: CEA for value analysis plugin and GUI\n\tCEA/INRIA for the Frama-C kernel\n\tINRIA for the Jessie plugin"
      ~license:"Frama-C kernel is under LGPL v2
Cil is under BSD
Ocamlgraph is under LGPL v2
Analysis plugins are LGPL v2.1"
      ~website:"http://www.frama-c.cea.fr/"
      ~website_label:"Questions and support"
      ~version:(Version.version^" compiled on "^Version.date)
      ()
  in
(*  Buggy labgtk2 prevents this from working...
    ignore 
    (dialog#connect#response 
       ~callback:(fun _ -> try 
                    dialog#coerce#destroy ()
                    with Not_found -> ()));*)

  ignore (dialog#run ())
  with Not_found -> () (* raised because of a buggy lablgtk2 *)

(** Register this dialog in main window menu bar *)
let () = 
  Design.register_extension 
    (fun window -> 
       GAction.add_actions window#actions
         [GAction.add_action "About" ~stock:`ABOUT ~accel:"<control>A" 
            ~tooltip:"About"
	    ~callback:(fun _-> show ());
          GAction.add_action "HelpMenu" ~label:"_Help";];
       let location = 
         "<ui><menubar name='MenuBar'>
          <menu action='HelpMenu'>
          <menuitem action='About'/>
          </menu>
          </menubar></ui>"
       in
       ignore (window#ui_manager#add_ui_from_string location))
  
