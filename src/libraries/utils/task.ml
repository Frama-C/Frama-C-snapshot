(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

let dkey = Kernel.register_category "task"

(* -------------------------------------------------------------------------- *)
(* --- Error Messages                                                     --- *)
(* -------------------------------------------------------------------------- *)

let error = function
  | Failure msg -> msg
  | Sys_error msg -> msg
  | Unix.Unix_error(e,_,"") -> Unix.error_message e
  | Unix.Unix_error(e,_,p) -> Printf.sprintf "%s (%s)" (Unix.error_message e) p
  | exn -> Printexc.to_string exn

(* ------------------------------------------------------------------------ *)
(* ---  High Level Interface to Command                                 --- *)
(* ------------------------------------------------------------------------ *)

type 'a status =
  | Timeout of int
  | Canceled
  | Result of 'a
  | Failed of exn

let map f = function
  | Timeout n -> Timeout n
  | Canceled -> Canceled
  | Result x -> Result (f x)
  | Failed e -> Failed e

let pretty pp fmt = function
  | Timeout _ -> Format.pp_print_string fmt "timeout"
  | Canceled -> Format.pp_print_string fmt "canceled"
  | Result x -> Format.fprintf fmt "result %a" pp x
  | Failed (Failure msg) -> Format.fprintf fmt "failed (%s)" msg
  | Failed e -> Format.fprintf fmt "failed (%s)" (Printexc.to_string e)

(* -------------------------------------------------------------------------- *)
(* --- Monadic Engine                                                     --- *)
(* -------------------------------------------------------------------------- *)

type coin = Coin | Kill

type 'a async =
  | Yield
  | Wait of int
  | Return of 'a

module Monad :
sig
  type 'a t
  val unit : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val progress : 'a t -> 'a t
  val cancel : 'a t -> 'a t
  val yield : (coin -> 'a t) -> 'a t
  val async : (coin -> 'a async) -> 'a t
  val wait : 'a t -> 'a
  val finished : 'a t -> 'a option
  val waiting : 'a t -> bool
end =
struct
  
  type 'a t =
    | UNIT of 'a
    | WAIT of int * (coin -> 'a t)
    | YIELD of (coin -> 'a t)

  let unit a = UNIT a
  
  let rec bind m f = match m with
    | UNIT a -> f a
    | WAIT(d,m) -> WAIT (d, fun c -> bind (m c) f)
    | YIELD m -> YIELD (fun c -> bind (m c) f)
  
  let put c m = match m with
    | UNIT _ -> m
    | WAIT(_,f) | YIELD f -> f c
  let progress m = put Coin m
  let cancel m = put Kill m
  let yield f = YIELD f
  let rec ping f coin =
    match f coin with
    | Wait d -> WAIT(d,ping f)
    | Yield -> YIELD(ping f)
    | Return a -> UNIT a
    
  let async f = YIELD (ping f)

  let rec wait = function
    | UNIT a -> a
    | YIELD f -> !Db.progress() ; wait (f Coin)
    | WAIT(ms,f) -> !Db.progress() ; Extlib.usleep ms ; wait (f Coin)

  let finished = function UNIT a -> Some a | YIELD _ | WAIT _ -> None

  let waiting = function UNIT _ -> false | YIELD _ | WAIT _ -> true
  
end

(* ------------------------------------------------------------------------ *)
(* ---  Monadic Constructors                                            --- *)
(* ------------------------------------------------------------------------ *)

type 'a task = 'a status Monad.t

let wait = Monad.wait
let status = Monad.unit
let return r = Monad.unit (Result r)
let raised e = Monad.unit (Failed e)
let canceled () = Monad.unit Canceled
let failed text =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       Monad.unit (Failed(Failure (Buffer.contents buffer))))
    (Format.formatter_of_buffer buffer) text

let bind = Monad.bind

let sequence a k = 
  Monad.bind a (function
      | Result r -> k r
      | Failed e -> Monad.unit (Failed e)
      | Timeout n -> Monad.unit (Timeout n)
      | Canceled -> Monad.unit Canceled)
    
let nop = return ()
let later f x = Monad.yield
    begin function
      | Coin -> (try f x with e -> raised e)
      | Kill -> canceled ()
    end
let call f x = Monad.yield
    begin function
      | Coin -> (try return (f x) with e -> raised e)
      | Kill -> canceled ()
    end

let todo f = later f ()
let job t = sequence t (fun _ -> nop)

let finally t cb =
  Monad.bind t (fun s -> cb s ; Monad.unit s)

let callback t cb =
  Monad.bind t (fun s -> cb s ; nop)

let (>>>) = Monad.bind
let (>>=) = sequence
let (>>?) = finally
let (>>!) = callback

(* ------------------------------------------------------------------------ *)
(* ---  Critical Sections                                               --- *)
(* ------------------------------------------------------------------------ *)

type mutex = bool ref

let mutex () = ref false
let rec lock m =
  if !m
  then later lock m
  else (m := true ; return ())
let unlock m =
  if not !m
  then (invalid_arg "Invalid lock on mutex")
  else m := false

let sync m t = lock m >>= t >>? fun _ -> unlock m

(* ------------------------------------------------------------------------ *)
(* ---  System Commands                                                 --- *)
(* ------------------------------------------------------------------------ *)

type cmd = {
  name : string ;
  timed : bool ;
  timeout : int ;
  time_start : float ;
  time_stop : float ;
  mutable time_killed : bool ;
  chrono : float ref option ;
  async : (unit -> Command.process_result) ;
}

let set_chrono cmd = match cmd.chrono with
  | None -> () | Some r -> r := max !r (Unix.gettimeofday () -. cmd.time_start)

let set_time cmd t = match cmd.chrono with
  | None -> () | Some r -> r := max !r t

let start_command ~timeout ?time ?stdout ?stderr cmd args =
  begin
    Kernel.debug ~dkey "execute task '@[<hov 4>%t'@]"
      (fun fmt ->
         Format.pp_print_string fmt cmd ;
         Array.iter
           (fun c -> Format.fprintf fmt "@ %s" c) args) ;
    let timed = timeout > 0 || time <> None in
    let time_start = if timed then Unix.gettimeofday () else 0.0 in
    let time_stop = if timeout > 0 then time_start +. float_of_int timeout else 0.0 in
    let async = Command.command_async ?stdout ?stderr cmd args in
    {
      name = cmd ;
      timed = timed ;
      timeout = timeout ;
      time_start = time_start ;
      time_stop = time_stop ;
      time_killed = false ;
      chrono = time ;
      async = async ;
    }
  end

let ping_command cmd coin =
  try
    match cmd.async () with
    
    | Command.Not_ready kill ->
        if coin = Kill then (kill () ; Wait 100)
        else
          let time_now = if cmd.timed then Unix.gettimeofday () else 0.0 in
          if cmd.timeout > 0 && time_now > cmd.time_stop then
            begin
              set_time cmd (time_now -. cmd.time_start) ;
              Kernel.debug ~dkey "timeout '%s'" cmd.name ;
              cmd.time_killed <- true ;
              kill () ;
            end ;
          Wait 100

    | Command.Result (Unix.WEXITED s|Unix.WSIGNALED s|Unix.WSTOPPED s)
      when cmd.time_killed ->
        set_chrono cmd ;
        Kernel.debug ~dkey "timeout '%s' [%d]" cmd.name s ;
        Return (Timeout cmd.timeout)

    | Command.Result (Unix.WEXITED s) ->
        set_chrono cmd ;
        Kernel.debug ~dkey "exit '%s' [%d]" cmd.name s ;
        Return (Result s)

    | Command.Result (Unix.WSIGNALED s|Unix.WSTOPPED s) ->
        set_chrono cmd ;
        Kernel.debug ~dkey "signal '%s' [%d]" cmd.name s ;
        Return Canceled

  with e ->
    set_chrono cmd ;
    Kernel.debug ~dkey "failure '%s' [%s]" cmd.name (Printexc.to_string e) ;
    Return (Failed e)

let command ?(timeout=0) ?time ?stdout ?stderr cmd args = todo
    begin fun () ->
      let cmd = start_command ~timeout ?time ?stdout ?stderr cmd args in
      Monad.async (ping_command cmd)
    end

(* ------------------------------------------------------------------------ *)
(* ---  Shared Tasks                                                    --- *)
(* ------------------------------------------------------------------------ *)

type 'a shared =
  { descr : string ;
    retry : bool ;
    mutable builder : (unit -> 'a task) ;
    mutable shared : 'a task ;
    mutable clients : int ;
  }

let shared ~descr ~retry builder =
  { descr ; retry ; builder ; shared = todo builder ; clients = 0 }

let retry_shared sh = function
  | Failed _ -> sh.retry
  | Timeout _ | Canceled -> true
  | Result _ -> false

let ping_shared sh = function
  | Coin ->
      begin match Monad.finished sh.shared with
        | Some r ->
            if retry_shared sh r then sh.shared <- todo sh.builder ;
            Return r
        | None -> sh.shared <- Monad.progress sh.shared ; Yield
      end
  | Kill ->      
      if sh.clients > 1 then
        begin
          sh.clients <- pred sh.clients ;
          Return Canceled
        end
      else
        ( if sh.clients = 1 then
            begin
              sh.clients <- 0 ;
              sh.shared <- Monad.cancel sh.shared ;
            end ;
          Yield )

let share sh = todo
    begin fun () ->
      sh.clients <- succ sh.clients ;
      Monad.async (ping_shared sh)
    end

(* -------------------------------------------------------------------------- *)
(* --- IDLE                                                               --- *)
(* -------------------------------------------------------------------------- *)
    
let on_idle = ref
    (fun f -> try
        while f () do Extlib.usleep 50000 (* wait for 50ms *) done
      with Db.Cancel -> ())

(* -------------------------------------------------------------------------- *)
(* --- Task thread                                                        --- *)
(* -------------------------------------------------------------------------- *)

type thread = {
  mutable task : unit task ;
  mutable lock : bool ;
}

let thread task = { task = (task >>= fun _ -> nop) ; lock = false }
let cancel th = th.task <- Monad.cancel th.task
let running th =
  th.lock ||
  begin
    try
      th.lock <- true ;
      let t = Monad.progress th.task in
      th.task <- t ;
      th.lock <- false ;
      Monad.waiting t
    with e ->
      th.lock <- false ;
      raise e
  end

let run th = !on_idle (fun () -> (running th))

(* -------------------------------------------------------------------------- *)
(* --- Task Pool                                                          --- *)
(* -------------------------------------------------------------------------- *)

type pool = thread list ref

let pool () = ref []

let iter f p =
  let rec walk f = function
    | [] -> []
    | t::ts ->
        if running t then f t ;
        let ts = walk f ts in
        if running t then t :: ts else ts
  in p := walk f !p

let add p t =
  let ps = List.filter running !p in
  p := if running t then t :: ps else ps

let flush p = p := List.filter running !p
let size p = flush p ; List.length !p

(* -------------------------------------------------------------------------- *)
(* --- Task Server                                                        --- *)
(* -------------------------------------------------------------------------- *)

type callbacks = (unit -> unit) list

(* Invariant:
   terminated + (length running) + Sum ( length queue.(i) ) == scheduled
*)

type server = {
  queue : thread Queue.t array ;
  mutable scheduled : int ;
  mutable terminated : int ;
  mutable running : thread list ;
  mutable procs : int ;
  mutable waiting : bool ;
  mutable wait : callbacks ;
  mutable activity : callbacks ;
  mutable start : callbacks ;
  mutable stop : callbacks ;
}

let fire callbacks =
  List.iter (fun f -> f ()) callbacks

let server ?(stages=1) ?(procs=4) () = {
  queue = Array.init stages (fun _ -> Queue.create ()) ;
  running = [] ;
  procs = procs ;
  scheduled = 0 ; terminated = 0 ;
  activity = [] ; start = [] ; stop = [] ; wait = [] ;
  waiting = false ;
}

let set_procs s p = s.procs <- p
let on_server_activity s cb  = s.activity <- s.activity @ [cb]
let on_server_wait s cb = s.wait <- s.wait @ [cb]
let on_server_start s cb = s.start <- s.start @ [cb]
let on_server_stop s cb  = s.stop <- s.stop @ [cb]

let cancel_all server =
  begin
    Array.iter (Queue.iter cancel) server.queue ;
    List.iter cancel server.running ;
  end

let spawn server ?pool ?(stage=0) thread =
  begin
    (match pool with None -> () | Some pool -> add pool thread) ;
    Queue.push thread server.queue.(stage) ;      (* queue(i) ++ *)
    server.scheduled <- succ server.scheduled ; (* scheduled ++ *)
    server.waiting <- false ;
  end (* invariant holds *)

let scheduled s = s.scheduled
let terminated s = s.terminated
let waiting s =
  if s.waiting || s.running = [] then None else Some (List.length s.running)

let is_empty server =
  try Array.iter
        (fun q -> if not (Queue.is_empty q) then raise Exit)
        server.queue ; true
  with Exit -> false

let schedule server q =
  try
    while List.length server.running < server.procs do
      let task = Queue.take q in (* queue ++ *)
      if running task
      then server.running <- task :: server.running
      (* running++ => invariant holds *)
      else server.terminated <- succ server.terminated
      (* terminated++ => invariant holds *)
    done
  with Queue.Empty -> ()

let rec run_server server () =
  begin
    server.running <- List.filter
        (fun task ->
           if running task then true
           else
             ( (* running -- ; terminated ++ => invariant preserved *)
               server.terminated <- succ server.terminated ; false )
        ) server.running ;
    Array.iter (schedule server) server.queue ;
    try
      !Db.progress () ;
      fire server.activity ;
      if server.running <> [] then
        begin
          if not server.waiting && is_empty server then
            begin
              fire server.wait ;
              server.waiting <- true ;
            end ;
          true
        end
      else
        begin
          fire server.stop ;
          server.scheduled <- 0 ;
          server.terminated <- 0 ;
          false
        end
    with _ -> (* Db.Cancel ... *)
      cancel_all server ;
      run_server server ()
  end

let launch server =
  if server.scheduled > server.terminated
  then ( fire server.start ; !on_idle (run_server server) )
    
