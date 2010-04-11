(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

module MessageMap =
  Computation.Make_Hashtbl
    (Inthash)
    (Project.Datatype.Persistent
       (struct type t = Log.event let name = "message" end))
    (struct
       let name = "message_table"
       let size = 17
       let dependencies = []
     end)

module MessageCounter =
  Computation.Ref
    (struct include Datatype.Int let default () = 0 end)
    (struct
       let name = "message_counter"
       let dependencies = []
     end)

let depend s = MessageMap.depend s; MessageCounter.depend s

let clear () = MessageCounter.clear () ; MessageMap.clear ()
let iter f = MessageMap.iter f

let enable_collect =
  let enabled = ref false in
  fun () ->
    if !enabled = false then (
      enabled := true;
      Cilmsg.debug "Enable collection of error messages." ;
      let emit e =
        let c = MessageCounter.get () in
        MessageMap.add c e ;
        MessageCounter.set (succ c)
      in
      begin
        Log.add_listener ~kind:[Log.Error;Log.Warning] emit ;
      end)

let disable_echo () =
  begin
    Cilmsg.debug "Disable echo for error messages" ;
    Log.set_echo ~kind:[Log.Error;Log.Warning] false ;
  end

let dump_messages () = MessageMap.iter (fun _ e -> Log.echo e)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
