(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Format

module DatatypeMessages =
  Datatype.Make
    (struct
       include Datatype.Serializable_undefined
       open Log
       type t = event
       let name = "message"
       let reprs =
         [ { evt_kind = Failure;
             evt_plugin = "";
             evt_source = None;
             evt_message = "" } ]
       let mem_project = Datatype.never_any_project
     end)

module Messages =
  State_builder.List_ref
    (DatatypeMessages)
    (struct
       let name = "message_table"
       let dependencies = [ Ast.self ]
       let kind = `Internal
     end)
module NbMessages =
  State_builder.Zero_ref
    (struct
       let name = "nb_messages"
       let dependencies = [Messages.self]
       let kind = `Internal
     end)

let self = Messages.self

let add_message m =
  NbMessages.set (NbMessages.get () + 1);
  Messages.set (m :: Messages.get ())

let iter f = List.iter f (List.rev (Messages.get ()))
let dump_messages () = iter Log.echo

let enable_collect =
  let not_yet = ref true in
  fun () ->
    if !not_yet then begin
      Kernel.debug "enable collection of error messages.";
      Log.add_listener ~kind:[ Log.Error; Log.Warning ] add_message;
      not_yet := false
    end

let () =
  let run () = if Kernel.Collect_messages.get () then enable_collect () in
  (* Set by the user on the command-line *)
  Cmdline.run_after_early_stage run;
  (* Set by a plugin *)
  Cmdline.run_after_configuring_stage run;
;;


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
