(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

open Cil_types

let main (main_ui:Design.main_window_extension_points) =
  let filetree_selector ~was_activated ~activating node =
    (* [JS 2009/30/03] GUI may become too slow if froms are displayed *)
    if false && Db.Value.is_computed () then begin
      if not was_activated && activating then begin
        match node with
        | Filetree.Global (Cil_types.GFun ({svar=v},_)) ->
          begin
            try
              let kf = Globals.Functions.get v in
              if !Db.From.is_computed kf then
                main_ui#pretty_information
                  "@[Functional dependencies:@\n%a@]@." !Db.From.pretty kf
            with Not_found -> ()
          end
        | _ -> ();
      end;
    end
  in
  main_ui#file_tree#add_select_function filetree_selector

let () = Design.register_extension main


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
