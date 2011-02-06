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

module Messages =
  State_builder.Hashtbl
    (Cil_datatype.Int_hashtbl)
    (Datatype.Make
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
	end))
    (struct
       let name = "message_table"
       let size = 17
       let dependencies = []
       let kind = `Internal
     end)

let self = Messages.self

let iter f = Messages.iter f

let enable_collect =
  let not_yet = ref true in
  fun () ->
    if !not_yet then begin
      Kernel.debug "enable collection of error messages.";
      let emit e =
        let c = Messages.length () in
        Messages.add c e ;
      in
      Log.add_listener ~kind:[ Log.Error; Log.Warning ] emit;
      not_yet := false
    end

let () =
  let run () = if Parameters.Collect_messages.get () then enable_collect () in
  (* Set by the user on the command-line *)
  Cmdline.run_after_early_stage run;
  (* Set by a plugin *)
  Cmdline.run_after_configuring_stage run;
;;

let dump_messages () = Messages.iter (fun _ e -> Log.echo e)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
