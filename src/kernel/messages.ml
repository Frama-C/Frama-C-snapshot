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

module DatatypeMessages =
  Datatype.Make_with_collections
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
       let hash (e: event)= Hashtbl.hash e
       let compare (e1: event) e2 = Extlib.compare_basic e1 e2
       let equal = Datatype.from_compare
     end)

module Messages =
  State_builder.List_ref
    (DatatypeMessages)
    (struct
       let name = "message_table"
       let dependencies = [ Ast.self ]
     end)
let () = Ast.add_monotonic_state Messages.self

module Nb_errors =
  State_builder.Zero_ref
    (struct
       let name = "nb_errors"
       let dependencies = [Messages.self]
     end)

let add_error m =
  Nb_errors.set (Nb_errors.get() + 1);
  Messages.set (m :: Messages.get ());;

let nb_errors() = Nb_errors.get();;

module Nb_warnings =
  State_builder.Zero_ref
    (struct
       let name = "nb_warnings"
       let dependencies = [Messages.self]
     end)

let add_warning m =
  Nb_warnings.set (Nb_warnings.get() + 1);
  Messages.set (m :: Messages.get ());;

let nb_warnings() = Nb_warnings.get();;
let nb_messages() = nb_errors() + nb_warnings();;

let self = Messages.self

let iter f = List.iter f (List.rev (Messages.get ()))
let dump_messages () = iter Log.echo

let enable_collect =
  let not_yet = ref true in
  fun () ->
    if !not_yet then begin
      Kernel.debug "enable collection of error messages.";
      Log.add_listener ~kind:[ Log.Error ] add_error;
      Log.add_listener ~kind:[ Log.Warning ] add_warning;
      not_yet := false
    end

module OnceTable = 
  State_builder.Hashtbl
    (DatatypeMessages.Hashtbl)
    (Datatype.Unit)
    (struct
      let size = 37
      let dependencies = [ Ast.self ]
      let name = "Message.OnceTable"
     end)

let check_not_yet evt =
  if OnceTable.mem evt then false
  else begin
    OnceTable.add evt (); 
    true
  end

let () = Log.check_not_yet := check_not_yet

let reset_once_flag () = OnceTable.clear ()

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
