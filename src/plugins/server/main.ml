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

(* -------------------------------------------------------------------------- *)
(* --- Server Main Process                                                --- *)
(* -------------------------------------------------------------------------- *)

module Senv = Server_parameters

let option f = function None -> () | Some x -> f x

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

type kind = [ `GET | `SET | `EXEC ]
let string_of_kind = function `GET -> "GET" | `SET -> "SET" | `EXEC -> "EXEC"
let pp_kind fmt kd = Format.pp_print_string fmt (string_of_kind kd)

let registry = Hashtbl.create 32

let register (kind : kind) request handler =
  if Hashtbl.mem registry request then
    Server_parameters.failure "Request '%s' already registered" request
  else
    Hashtbl.add registry request (kind,handler)

let find request =
  try Some (Hashtbl.find registry request)
  with Not_found -> None

let exec request data = (snd (Hashtbl.find registry request)) data

(* -------------------------------------------------------------------------- *)
(* --- Public API                                                         --- *)
(* -------------------------------------------------------------------------- *)

type json = Json.t

type 'a request = [
  | `Poll
  | `Request of 'a * string * json
  | `Kill of 'a
  | `Shutdown
]

type 'a response = [
  | `Data of 'a * json
  | `Error of 'a * string
  | `Killed of 'a
  | `Rejected of 'a
]

type 'a message = {
  requests : 'a request list ;
  callback : 'a response list -> unit ;
}

(* Private API: *)

type 'a exec = {
  id : 'a ;
  request : string ;
  data : json ;
  handler : json -> json ;
  yield : bool ;
  mutable killed : bool ;
}

type 'a server = {
  rate : int ;
  pretty : Format.formatter -> 'a -> unit ;
  equal : 'a -> 'a -> bool ;
  fetch : unit -> 'a message option ;
  q_in : 'a exec Queue.t ;
  q_out : 'a response Stack.t ;
  mutable shutdown : bool ;
  mutable coins : int ;
  mutable running : 'a exec option ;
}

exception Killed

(* -------------------------------------------------------------------------- *)
(* --- Debug                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_request pp fmt (r : _ request) =
  match r with
  | `Poll -> Format.fprintf fmt "Poll"
  | `Shutdown -> Format.fprintf fmt "Shutdown"
  | `Kill id -> Format.fprintf fmt "Kill %a" pp id
  | `Request(id,request,data) ->
    if Senv.debug_atleast 2 then
      Format.fprintf fmt "@[<hov 2>Request %s:%a@ %a@]"
        request pp id Data.pretty data
    else
      Format.fprintf fmt "Request %s:%a" request pp id

let pp_response pp fmt (r : _ response) =
  match r with
  | `Error(id,err) -> Format.fprintf fmt "Error %a: %s" pp id err
  | `Rejected id -> Format.fprintf fmt "Rejected %a" pp id
  | `Killed id -> Format.fprintf fmt "Killed %a" pp id
  | `Data(id,data) ->
    if Senv.debug_atleast 2 then
      Format.fprintf fmt "@[<hov 2>Response %a@ %a@]"
        pp id Data.pretty data
    else
      Format.fprintf fmt "Response %a" pp id

(* -------------------------------------------------------------------------- *)
(* --- Request Handling                                                   --- *)
(* -------------------------------------------------------------------------- *)

let no_yield () = ()

let execute exec : _ response =
  try
    let data = exec.handler exec.data in
    `Data(exec.id,data)
  with
  | Killed -> `Killed exec.id
  | Data.InputError msg -> `Error(exec.id,msg)
  | Sys.Break as exn -> raise exn (* Silently pass the exception *)
  | exn when Cmdline.catch_at_toplevel exn ->
    Senv.warning "[%s] Uncaught exception:@\n%s"
      exec.request (Cmdline.protect exn) ;
    `Error(exec.id,Printexc.to_string exn)

let execute_with_yield yield exec =
  let db = !Db.progress in
  Db.progress := if exec.yield then yield else no_yield ;
  Extlib.try_finally ~finally:(fun () -> Db.progress := db) execute exec

let execute_debug pp yield exec =
  if Senv.debug_atleast 1 then
    Senv.debug "Trigger %s:%a" exec.request pp exec.id ;
  execute_with_yield yield exec

let reply_debug server resp =
  if Senv.debug_atleast 1 then
    Senv.debug "%a" (pp_response server.pretty) resp ;
  Stack.push resp server.q_out

(* -------------------------------------------------------------------------- *)
(* --- Processing Requests                                                --- *)
(* -------------------------------------------------------------------------- *)

let raise_if_killed = function { killed } -> if killed then raise Killed
let kill_exec e = e.killed <- true
let kill_request eq id e = if eq id e.id then e.killed <- true

let process_request (server : 'a server) (request : 'a request) : unit =
  if Senv.debug_atleast 1 then
    Senv.debug "%a" (pp_request server.pretty) request ;
  match request with
  | `Poll -> ()
  | `Shutdown ->
    begin
      option kill_exec server.running ;
      Queue.clear server.q_in ;
      Stack.clear server.q_out ;
      server.shutdown <- true ;
    end
  | `Kill id ->
    begin
      let kill = kill_request server.equal id in
      Queue.iter kill server.q_in ;
      option kill server.running ;
    end
  | `Request(id,request,data) ->
    begin
      match find request with
      | None -> reply_debug server (`Rejected id)
      | Some( `GET , handler ) ->
        let exec = { id ; request ; handler ; data ;
                     yield = false ; killed = false } in
        reply_debug server (execute exec)
      | Some( `SET , handler ) ->
        let exec = { id ; request ; handler ; data ;
                     yield = false ; killed = false } in
        Queue.push exec server.q_in
      | Some( `EXEC , handler ) ->
        let exec = { id ; request ; handler ; data ;
                     yield = true ; killed = false } in
        Queue.push exec server.q_in
    end

(* -------------------------------------------------------------------------- *)
(* --- Fetching a Bunck of Messages                                       --- *)
(* -------------------------------------------------------------------------- *)

let communicate server =
  match server.fetch () with
  | None -> false
  | Some message ->
    let error =
      try List.iter (process_request server) message.requests ; None
      with exn -> Some exn in (* re-raised after message reply *)
    let pool = ref [] in
    Stack.iter (fun r -> pool := r :: !pool) server.q_out ;
    Stack.clear server.q_out ;
    message.callback !pool ;
    option raise error ; true

(* -------------------------------------------------------------------------- *)
(* --- Yielding                                                           --- *)
(* -------------------------------------------------------------------------- *)

let do_yield server () =
  begin
    option raise_if_killed server.running ;
    let n = server.coins in
    if n < server.rate then
      server.coins <- succ n
    else
      ( server.coins <- 0 ; ignore ( communicate server ) ) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- One Step Process                                                   --- *)
(* -------------------------------------------------------------------------- *)

let rec fetch_exec q =
  if Queue.is_empty q then None
  else
    let e = Queue.pop q in
    if e.killed then fetch_exec q else Some e

let process server =
  match fetch_exec server.q_in with
  | None -> communicate server
  | Some exec ->
    server.running <- Some exec ;
    try
      reply_debug server (execute_debug server.pretty (do_yield server) exec) ;
      server.running <- None ;
      true
    with exn ->
      server.running <- None ;
      raise exn

(* -------------------------------------------------------------------------- *)
(* --- Server Main Loop                                                   --- *)
(* -------------------------------------------------------------------------- *)

let in_range ~min:a ~max:b v = min (max a v) b

let kill () = raise Killed
let yield () = !Db.progress ()

let demons = ref []
let on callback = demons := !demons @ [ callback ]
let signal activity =
  List.iter (fun f -> try f activity with _ -> ()) !demons

let run ~pretty ?(equal=(=)) ~fetch () =
  begin
    let rate = in_range ~min:1 ~max:200 (Senv.Rate.get ()) in
    let idle_ms = in_range ~min:1 ~max:2000 (Senv.Idle.get ()) in
    let idle_s = float_of_int idle_ms /. 1000.0 in
    let server = {
      fetch ; coins = 0 ; rate ; equal ; pretty ;
      q_in = Queue.create () ;
      q_out = Stack.create () ;
      running = None ;
      shutdown = false ;
    } in
    try
      (* TODO: remove the following line once the Why3 signal handler is not
         used anymore. *)
      Sys.catch_break true;
      signal true ;
      Senv.feedback "Server running." ;
      begin try
          while not server.shutdown do
            let activity = process server in
            if not activity then Unix.sleepf idle_s ;
          done ;
        with Sys.Break -> () (* Ctr+C, just leave the loop normally *)
      end;
      Senv.feedback "Server shutdown." ;
      signal false ;
    with exn ->
      Senv.feedback "Server interruped (fatal error)." ;
      signal false ;
      raise exn
  end

(* -------------------------------------------------------------------------- *)
