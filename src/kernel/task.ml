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

(* ------------------------------------------------------------------------ *)
(* ---  High Level Interface to Command                                 --- *)
(* ------------------------------------------------------------------------ *)

type 'a status =
  | Canceled
  | Result of 'a
  | Failed of exn

type 'a running =
  | Running of (unit -> unit)
  | Finished of 'a status

module Monad :
sig
  type 'a t
  val result : 'a status -> 'a t
  val async  : (unit -> 'a running) -> 'a t
  val ping   : 'a t -> 'a running
  val cancel : 'a t -> unit
  val bind   : 'a t -> ('a status -> 'b t) -> 'b t
end =
struct

  type 'a process =
    | Ping of (unit -> 'a running)
    | Done of 'a status * 'a running
        (* Invariant : Done(x,y) => y==Finished x *)

  type 'a t = 'a process ref

  let finished r = Done(r,Finished r)
  let result r = ref (finished r)
  let async ping = ref (Ping ping)

  let ping task =
    match !task with
      | Done(_,run) -> run
      | Ping p ->
          let run = try p () with e -> Finished(Failed e) in
          match run with
            | Finished r -> task := Done(r,run) ; run
            | Running _ -> run

  let cancel t =
    match ping t with
      | Running kill ->
          begin
            try kill () ; t := finished Canceled
            with e -> t := finished (Failed e)
          end
      | Finished _ -> ()

  type ('a,'b) seq =
    | Last of 'b t
    | Seq of 'a t * ('a status -> 'b t)

  let bind t k =
    let pinger step () =
      match !step with
        | Last t -> ping t
        | Seq(t,k) ->
            match ping t with
              | Running kill -> Running kill (* 'a conversion *)
              | Finished r ->
                  let t' = try k r with e -> result (Failed e) in
                  if r <> Canceled
                  then ( step := Last t' ; ping t' )
                  else ( cancel t' ; Finished Canceled )
    in async (pinger (ref (Seq(t,k))))

end

type 'a task = 'a Monad.t

(* ------------------------------------------------------------------------ *)
(* ---  Monadic Constructors                                            --- *)
(* ------------------------------------------------------------------------ *)

let status = Monad.result
let return r = Monad.result (Result r)
let raised e = Monad.result (Failed e)
let canceled () = Monad.result Canceled
let failed text =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       Monad.result (Failed(Failure (Buffer.contents buffer))))
    (Format.formatter_of_buffer buffer) text

let bind = Monad.bind
let sequence t f =
  bind t (function
            | Result r -> f r
            | Failed e -> raised e
            | Canceled -> canceled ())

let wait = Running (fun () -> ())
let stop = Finished(Result())
let nop = return ()
let todo job = sequence nop job
let call f x = Monad.async (fun () -> Finished(Result(f x)))

let finally t cb =
  let kill k cb () =
    try k () ; cb Canceled
    with e -> cb (Failed e)
  in
  let pinger t () =
    match Monad.ping t with
      | Finished s as run -> cb s ; run
      | Running k -> Running (kill k cb)
  in Monad.async (pinger t)

let callback t cb =
  let kill k cb () =
    try k () ; cb Canceled
    with e -> cb (Failed e)
  in
  let pinger t () =
    let run = Monad.ping t in
    match run with
      | Finished st -> cb st ; Finished (Result())
      | Running k -> Running (kill k cb)
  in Monad.async (pinger t)

let (>>>) = Monad.bind
let (>>=) = sequence
let (>>?) = finally
let (>>!) = callback

(* ------------------------------------------------------------------------ *)
(* ---  Critical Sections                                               --- *)
(* ------------------------------------------------------------------------ *)

type mutex = bool ref
let mutex () = ref false
let lock m = Monad.async (fun () -> if !m then wait else (m:=true ; stop))
let unlock m = if not !m then Kernel.failure "Suspiscious lock" ; m := false
let sync m t = lock m >>= t >>? fun _ -> unlock m

(* ------------------------------------------------------------------------ *)
(* ---  Run Operations                                                  --- *)
(* ------------------------------------------------------------------------ *)

let step task = ignore (Monad.ping task)
let start = step
let ping = Monad.ping
let cancel = Monad.cancel

let rec wait task =
  let run =
    try !Db.progress () ; Monad.ping task
    with Db.Cancel -> Finished Canceled
  in
  match run with
    | Finished r -> r
    | _ -> Unix.sleep 1 ; wait task

(* ------------------------------------------------------------------------ *)
(* ---  System Commands                                                 --- *)
(* ------------------------------------------------------------------------ *)

let debug = true

let command ?(timeout=0) ?stdout ?stderr cmd args =
  let hang_on =
    if timeout > 0
    then Unix.time () +. float_of_int timeout
    else 0.0 in
  Kernel.debug "exec '@[<hov 4>%t'@]"
    (fun fmt ->
       Format.pp_print_string fmt cmd ;
       Array.iter
         (fun c -> Format.fprintf fmt "@ %s" c) args) ;
  let async = Command.command_async ?stdout ?stderr cmd args in
  let pinger () =
    try
      match async () with
        | Command.Not_ready kill ->
            if timeout > 0 && Unix.time () > hang_on then
              begin
                Kernel.debug "timeout '%s'" cmd ;
                kill () ; Finished Canceled
              end
            else Running kill
        | Command.Result (Unix.WEXITED s) ->
            Kernel.debug "exit '%s' [%d]" cmd s ;
            Finished (Result s)
        | Command.Result (Unix.WSIGNALED s|Unix.WSTOPPED s) ->
            Kernel.debug "signal '%s' [%d]" cmd s ;
            Finished Canceled
    with e ->
      Kernel.debug "failure '%s' [%s]" cmd (Printexc.to_string e) ;
      Finished (Failed e)
  in Monad.async pinger

(* ------------------------------------------------------------------------ *)
(* ---  Server                                                          --- *)
(* ------------------------------------------------------------------------ *)

type callbacks = (unit -> unit) list

(* Invariant:

   terminated + (length running) + Sum ( length queue.(i) ) == scheduled

*)

type server = {
  queue : unit task Queue.t array ;
  mutable scheduled : int ;
  mutable terminated : int ;
  mutable running : unit task list ;
  mutable procs : int ;
  mutable activity : callbacks ;
  mutable start : callbacks ;
  mutable stop : callbacks ;
}

let fire callbacks =
  List.iter (fun f -> try f () with _ -> ()) callbacks

let server ?(stages=1) ?(procs=4) () = {
  queue = Array.init stages (fun _ -> Queue.create ()) ;
  running = [] ;
  procs = procs ;
  scheduled = 0 ; terminated = 0 ;
  activity = [] ; start = [] ; stop = [] ;
}

let on_idle = ref
  (fun f -> try
     while f () do Extlib.usleep 50000 (* wait for 50ms *) done
   with Db.Cancel -> ())

let set_procs s p = s.procs <- p
let on_server_activity s cb  = s.activity <- s.activity @ [cb]
let on_server_start s cb = s.start <- s.start @ [cb]
let on_server_stop s cb  = s.stop <- s.stop @ [cb]

let cancel_all server =
  begin
    Array.iter (Queue.iter cancel) server.queue ;
    List.iter cancel server.running ;
  end

let spawn server ?(stage=0) task =
  begin
    Queue.push task server.queue.(stage) ;      (* queue(i) ++ *)
    server.scheduled <- succ server.scheduled ; (* scheduled ++ *)
  end (* invariant holds *)

let scheduled s = s.scheduled
let terminated s = s.terminated

let alive task =
  match Monad.ping task with
    | Running _ -> true
    | Finished _ -> false

let schedule server q =
  try
    while List.length server.running < server.procs do
      let task = Queue.take q in (* queue ++ *)
      if alive task
      then server.running <- task :: server.running
        (* running++ => invariant holds *)
      else server.terminated <- succ server.terminated
        (* terminated++ => invariant holds *)
    done
  with Queue.Empty -> ()

let rec run server () =
  begin
    server.running <- List.filter
      (fun task ->
         if alive task then true
         else
           ( (* running -- ; terminated ++ => invariant preserved *)
             server.terminated <- succ server.terminated ; false )
      ) server.running ;
    Array.iter (schedule server) server.queue ;
    try
      !Db.progress () ;
      fire server.activity ;
      if server.running <> [] then true else
        begin
          fire server.stop ;
          server.scheduled <- 0 ;
          server.terminated <- 0 ;
          false
        end
    with _ -> (* Db.Cancel ... *)
      cancel_all server ;
      run server ()
  end

let launch server =
  if server.scheduled > server.terminated
  then ( fire server.start ; !on_idle (run server) )
