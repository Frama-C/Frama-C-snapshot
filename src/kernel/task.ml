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
  | Timeout
  | Canceled
  | Result of 'a
  | Failed of exn

let map f = function
  | Timeout -> Timeout
  | Canceled -> Canceled
  | Result x -> Result (f x)
  | Failed e -> Failed e

let pretty pp fmt = function
  | Timeout -> Format.pp_print_string fmt "timeout"
  | Canceled -> Format.pp_print_string fmt "canceled"
  | Result x -> Format.fprintf fmt "result %a" pp x
  | Failed (Failure msg) -> Format.fprintf fmt "failed (%s)" msg
  | Failed e -> Format.fprintf fmt "failed (%s)" (Printexc.to_string e)

let protect f arg on_fail =
  try f arg 
  with e ->
    if Kernel.debug_atleast 1 then
      begin
        Kernel.debug ~dkey "Current task raised an exception:@\n%s@\n%s"
          (Printexc.to_string e) (Printexc.get_backtrace ())
      end;
    on_fail (Failed e)

type 'a ping = 
  | DONE of 'a status
  | RUN of (unit -> unit)
  | NEXT of (unit -> unit) * (unit -> 'a ping)

type 'a pinger = unit -> 'a ping
      
type 'a running =
  | Waiting
  | Running of (unit -> unit)
  | Finished of 'a status

module Monad :
sig
  type 'a t
  val return : 'a status -> 'a t
  val bind : 'a t -> ('a status -> 'b t) -> 'b t
  val running : 'a pinger -> 'a t
  val waiting : (unit -> 'b pinger) -> 'b t
  val state   : 'a t -> 'a running
  val execute : 'a t -> 'a status option
  val start   : 'a t -> unit
  val cancel  : 'a t -> unit
end =
struct

  type 'a process =
    | Wait of (unit -> 'a pinger)
    | Ping of 'a pinger
    | Done of 'a status

  type 'a t = 'a process ref

  let finished e = DONE e
  let pinger e () = DONE e
  let return r = ref (Done r)
  let waiting starter = ref (Wait starter)
  let running pinger = ref (Ping pinger)

  let run task p =
    let ping = protect p () finished in
    match ping with
      | DONE r -> task := Done r ; ping
      | NEXT(_,f) -> task := Ping f ; ping
      | RUN _ -> ping

  let state_of_ping = function DONE r -> Finished r | NEXT(k,_) | RUN k -> Running k
  let result_of_ping = function DONE r -> Some r | NEXT _ | RUN _ -> None

  let state task =
    match !task with
      | Wait _ -> Waiting
      | Done r -> Finished r
      | Ping p -> state_of_ping (run task p)

  let start task =
    match !task with
      | Wait s -> 
	  let f = protect s () pinger in
	  task := Ping f ; ignore (run task f)
      | Ping f -> ignore (run task f)
      | Done _ -> ()

  let execute task =
    match !task with
      | Wait s -> 
	  let f = protect s () pinger in
	  task := Ping f ; result_of_ping (run task f)
      | Ping f -> result_of_ping (run task f)
      | Done r -> Some r
	  
  let cancel task =
    match state task with
      | Waiting -> task := Done Canceled
      | Running kill ->
          begin
            protect 
              (fun () -> task := Done Canceled ; kill ()) ()
              (fun st -> task := Done st)
          end
      | Finished _ -> ()

  let get_pinger task =
    match !task with
      | Done r -> pinger r
      | Wait s -> protect s () pinger
      | Ping f -> f

  let next_ping s k =
    let b = protect k s return in
    let kill = fun () -> cancel b in
    let ping = get_pinger b in
    NEXT(kill,ping)

  let next_pinger s k () = next_ping s k

  let rec bind_pinger f k () =
    match f () with
      | DONE s -> next_ping s k
      | NEXT(kill,f') -> NEXT(kill,bind_pinger f' k)
      | RUN kill -> RUN kill

  let bind_waiter s k () = bind_pinger (protect s () pinger) k

  let bind a k =
    match !a with
      | Wait s -> ref (Wait(bind_waiter s k))
      | Ping f -> ref (Ping(bind_pinger f k))
      | Done s -> ref (Ping(next_pinger s k))

end

type 'a task = 'a Monad.t

(* ------------------------------------------------------------------------ *)
(* ---  Monadic Constructors                                            --- *)
(* ------------------------------------------------------------------------ *)

let status = Monad.return
let return r = Monad.return (Result r)
let raised e = Monad.return (Failed e)
let canceled () = Monad.return Canceled
let failed text =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       Monad.return (Failed(Failure (Buffer.contents buffer))))
    (Format.formatter_of_buffer buffer) text

let bind a k =
  Monad.bind a (function
		  | Canceled -> Monad.return Canceled
		  | s -> k s)

let sequence a k = 
  Monad.bind a (function
		  | Result r -> k r
		  | Failed e -> Monad.return (Failed e)
		  | Timeout -> Monad.return Timeout
		  | Canceled -> Monad.return Canceled)

let nop = Monad.return (Result())
let call f x = Monad.running (fun () -> DONE (Result(f x)))
let todo f = sequence nop f
let job job = sequence job (fun _ -> nop)

let finally t cb =
  Monad.bind t (fun s -> cb s ; Monad.return s)

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
let wait = RUN (fun () -> ())
let next = DONE (Result ())
let lock m = Monad.running (fun () -> if !m then wait else (m:=true ; next))
let unlock m = if not !m then Kernel.failure "Suspiscious lock" ; m := false
let sync m t = lock m >>= t >>? fun _ -> unlock m

(* ------------------------------------------------------------------------ *)
(* ---  Run Operations                                                  --- *)
(* ------------------------------------------------------------------------ *)

let start = Monad.start
let ping = Monad.state
let cancel = Monad.cancel

let rec wait task =
  (try !Db.progress () with Db.Cancel -> Monad.cancel task) ;
  match Monad.state task with
    | Finished r -> r
    | _ -> Unix.sleep 1 ; wait task

(* ------------------------------------------------------------------------ *)
(* ---  System Commands                                                 --- *)
(* ------------------------------------------------------------------------ *)

type cmd = {
  name : string ;
  timed : bool ;
  timeout : int ;
  time_start : float ;
  time_stop : float ;
  chrono : float ref option ;
  async : (unit -> Command.process_result) ;
}

let set_chrono cmd = match cmd.chrono with
  | None -> () | Some r -> r := max !r (Unix.time () -. cmd.time_start)

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
    let time_start = if timed then Unix.time () else 0.0 in
    let time_stop = if timeout > 0 then time_start +. float_of_int timeout else 0.0 in
    let async = Command.command_async ?stdout ?stderr cmd args in
    {
      name = cmd ;
      timed = timed ;
      timeout = timeout ;
      time_start = time_start ;
      time_stop = time_stop ;
      chrono = time ;
      async = async ;
    }
  end

let ping_command cmd () =
  try
    match cmd.async () with
	
      | Command.Not_ready kill ->
	  let time_now = if cmd.timed then Unix.time () else 0.0 in
          if cmd.timeout > 0 && time_now > cmd.time_stop then
            begin
	      set_time cmd (time_now -. cmd.time_start) ;
              Kernel.debug ~dkey "timeout '%s'" cmd.name ;
              kill () ;
	      DONE Timeout
            end
          else 
	    RUN kill
	      
      | Command.Result (Unix.WEXITED s) ->
	  set_chrono cmd ;
          Kernel.debug ~dkey "exit '%s' [%d]" cmd.name s ;
          DONE (Result s)

      | Command.Result (Unix.WSIGNALED s|Unix.WSTOPPED s) ->
	  set_chrono cmd ;
          Kernel.debug ~dkey "signal '%s' [%d]" cmd.name s ;
	  let err = Failure (Printf.sprintf "Unix.SIGNAL %d" s) in
          DONE (Failed err)

  with e ->
    set_chrono cmd ;
    Kernel.debug ~dkey "failure '%s' [%s]" cmd.name (Printexc.to_string e) ;
    DONE (Failed e)

let command ?(timeout=0) ?time ?stdout ?stderr cmd args =
  Monad.waiting
    begin fun () ->
      ping_command (start_command ~timeout ?time ?stdout ?stderr cmd args)
    end

(* ------------------------------------------------------------------------ *)
(* ---  Shared Tasks                                                    --- *)
(* ------------------------------------------------------------------------ *)

module Shared :
sig
  
  type 'a t
  val make : descr:string -> retry:bool -> (unit -> 'a task) -> 'a t
  val share : 'a t -> 'a task

end =
struct
  
  type 'a t = {
    descr : string ;
    retry : bool ;
    builder : unit -> 'a task ;
    mutable running : 'a task option ;
    mutable clients : int ;
  }

  let make ~descr ~retry cc = 
    { descr=descr ; retry=retry ; builder=cc ; running=None ; clients=0 }

  let kill s () =
    Kernel.debug ~dkey "Cancel instance of task '%s' (over %d)" s.descr s.clients ;
    if s.clients > 0 then
      begin
	s.clients <- pred s.clients ;
	if s.clients = 0 then
	  match s.running with
	    | Some k -> 
		Kernel.debug ~dkey "Kill shared task '%s'" s.descr ;
		Monad.cancel k ; s.running <- None
	    | None -> ()
      end

  let ping s () =
    let task = match s.running with
      | None -> 
	  let t = protect s.builder () Monad.return in 
	  s.running <- Some t ; t
      | Some t -> t
    in
    match Monad.execute task with
      | None -> RUN (kill s)
      | Some r ->
	  let release = match r with
	    | Result _ -> false
	    | Failed _ -> s.retry
	    | Timeout | Canceled -> true
	  in
	  if release then s.running <- None ;
	  (DONE r : 'a ping)
	  
  let share s =
    s.clients <- succ s.clients ; 
    Kernel.debug ~dkey "New instance of task '%s' (%d)" s.descr s.clients ;
    Monad.waiting (fun () -> ping s)

end

type 'a shared = 'a Shared.t
let shared = Shared.make
let share = Shared.share

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
  List.iter (fun f -> protect f () (fun _ -> ())) callbacks

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
  match Monad.state task with
    | Waiting -> true
    | Running _ -> true
    | Finished _ -> false

let running task = 
  match Monad.execute task with
    | Some _ -> false
    | None -> true

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
      run_server server ()
  end

let launch server =
  if server.scheduled > server.terminated
  then ( fire server.start ; !on_idle (run_server server) )

let run t = !on_idle (fun () -> running t)

