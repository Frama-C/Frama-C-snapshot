(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* Only Compiled when package Zmq is installed *)
(* No interface, registered via side-effects   *)

(* -------------------------------------------------------------------------- *)
(* --- ZeroMQ Server Options                                              --- *)
(* -------------------------------------------------------------------------- *)

module Senv = Server_parameters

let zmq_group = Senv.add_group "Protocol ZeroMQ"

let () = Parameter_customize.set_group zmq_group
module Enabled = Senv.String
    (struct
      let option_name = "-server-zmq"
      let arg_name = "url"
      let default = ""
      let help = "Establish a ZeroMQ server and listen for connections"
    end)

let _ = Doc.page `Protocol ~title:"ZeroMQ Protocol" ~filename:"server_zmq.md"

(* -------------------------------------------------------------------------- *)
(* --- ZMQ Context                                                        --- *)
(* -------------------------------------------------------------------------- *)

let context =
  let zmq = ref None in
  fun () ->
    match !zmq with
    | Some ctxt -> ctxt
    | None ->
      let major,minor,patch = Zmq.version () in
      Senv.feedback "ZeroMQ %d.%d.%d" major minor patch ;
      let ctxt = Zmq.Context.create () in
      at_exit (fun () -> Zmq.Context.terminate ctxt) ;
      zmq := Some ctxt ; ctxt

(* -------------------------------------------------------------------------- *)
(* --- Decoding Requests                                                  --- *)
(* -------------------------------------------------------------------------- *)

exception WrongEncoding of string

let jdecode txt =
  try Yojson.Basic.from_string txt
  with exn ->
    (* Exception if purely local from Yojson *)
    raise (WrongEncoding (Printexc.to_string exn))

let jencode js =
  try Yojson.Basic.to_string ~std:false js
  with exn ->
    (* Exception if purely local from Yojson *)
    raise (WrongEncoding (Printexc.to_string exn))

let rec decode = function
  | ("GET"|"SET"|"EXEC")::id::request::data :: w ->
    `Request(id,request,jdecode data) :: decode w
  | "KILL"::id:: w -> `Kill id :: decode w
  | "POLL" :: w -> `Poll :: decode w
  | "SHUTDOWN" :: _ -> [`Shutdown]
  | cmd::_ -> raise (WrongEncoding cmd)
  | [] -> []

let rec encode = function
  | `Data(id,data) :: w -> "DATA" :: id :: jencode data :: encode w
  | `Error(id,msg) :: w -> "ERROR" :: id :: msg :: encode w
  | `Killed id :: w -> "KILLED" :: id :: encode w
  | `Rejected id :: w -> "REJECTED" :: id :: encode w
  | [] -> []

(* -------------------------------------------------------------------------- *)
(* --- ZMQ Messages                                                       --- *)
(* -------------------------------------------------------------------------- *)

let callback socket responses =
  try
    let msg = encode responses in
    Zmq.Socket.send_all socket (if msg = [] then ["NONE"] else msg)
  with WrongEncoding msg ->
    Zmq.Socket.send_all socket [ "WRONG" ; msg ]

let fetch socket () =
  try
    let msg = Zmq.Socket.recv_all ~block:false socket in
    try Some Main.{ requests = decode msg ; callback = callback socket }
    with WrongEncoding msg ->
      Zmq.Socket.send_all socket [ "WRONG" ; msg ] ; None
  with
  | Unix.Unix_error( Unix.EAGAIN , _ , _ ) -> None
  | Zmq.ZMQ_exception(_,msg) -> Senv.fatal "ZeroMQ error: %s" msg

(* -------------------------------------------------------------------------- *)
(* --- Establish the Server                                               --- *)
(* -------------------------------------------------------------------------- *)

let establish url =
  if url <> "" then
    begin
      let context = context () in
      let socket = Zmq.Socket.(create context rep) in
      try
        Zmq.Socket.bind socket url ;
        Senv.feedback "ZeroMQ [%s]" url ;
        Main.run ~pretty:Format.pp_print_string ~fetch:(fetch socket) () ;
        Zmq.Socket.close socket ;
      with exn ->
        Zmq.Socket.close socket ;
        raise exn
    end

(* -------------------------------------------------------------------------- *)
(* --- Establish the Server from Command line                             --- *)
(* -------------------------------------------------------------------------- *)

let () = Db.Main.extend (fun () -> establish (Enabled.get ()))

(* -------------------------------------------------------------------------- *)
