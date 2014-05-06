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

(** High Level Interface to Command.
    @since Carbon-20101201
    @plugin development guide *)

(* ************************************************************************* *)
(** {2 Task} *)
(* ************************************************************************* *)

type 'a task
type 'a status =
  | Timeout
  | Canceled
  | Result of 'a
  | Failed of exn
type 'a running =
  | Waiting
  | Running of (unit -> unit)
  | Finished of 'a status

val error  : exn -> string (** Extract error message form exception *)

val start  : 'a task -> unit
val cancel : 'a task -> unit
val wait   : 'a task -> 'a status (** Blocks until termination. *)
val ping   : 'a task -> 'a running

val map : ('a -> 'b) -> 'a status -> 'b status
val pretty : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a status -> unit

(* ************************************************************************* *)
(** {2 Monadic Constructors} *)
(* ************************************************************************* *)

val nop : unit task
  (** The task that immediately returns unit *)

val return : 'a -> 'a task
  (** The task that immediately returns a result *)

val raised : exn -> 'a task
  (** The task that immediately fails with an exception *)

val canceled : unit -> 'a task
  (** The task that is immediately canceled *)

val failed : ('a,Format.formatter,unit,'b task) format4 -> 'a
  (** The task that immediately fails by raising a [Failure] exception.
      Typically: [[let exit d : 'a task = failed "exit status %d" k]] *)

val call : ('a -> 'b) -> 'a -> 'b task
  (** The task that, when started, invokes a function and immediately
      returns the result. *)

val todo : (unit -> 'a task) -> 'a task

val status : 'a status -> 'a task
  (** The task that immediately finishes with provided status *)

val bind : 'a task -> ('a status -> 'b task) -> 'b task
  (** [bind t k] first runs [t]. Then, when [t] exit with status [s],
      it starts task [k s].

      <b>Remark:</b> If [t] was cancelled, [k s] is still evaluated, but
      immediately canceled as well. This allows [finally]-like behaviors to
      be implemented. To evaluate [k r] only when [t] terminates normally,
      make use of the [sequence] operator. *)

val sequence : 'a task -> ('a -> 'b task) -> 'b task
  (** [sequence t k] first runs [t]. If [t] terminates with [Result r],
      then task [k r] is started.
      Otherwise, failure or cancelation of [t] is returned. *)

val job : 'a task -> unit task

val finally : 'a task -> ('a status -> unit) -> 'a task
  (** [finally t cb] runs task [t] and {i always} calls [cb s] when [t] exits
      with status [s]. Then [s] is returned. If the callback [cb]
      raises an exception, the returned status is emitted. *)

val callback : 'a task -> ('a status -> unit) -> unit task
  (** Same as [finally] but the status of the task is discarded. *)

val (>>>) : 'a task -> ('a status -> 'b task) -> 'b task (** [bind] infix. *)
val (>>=) : 'a task -> ('a -> 'b task) -> 'b task  (** [sequence] infix. *)
val (>>?) : 'a task -> ('a status -> unit) -> 'a task (** [finally] infix. *)
val (>>!) : 'a task -> ('a status -> unit) -> unit task (** [callback] infix. *)

(* ************************************************************************* *)
(** {2 Synchroneous Command} *)
(* ************************************************************************* *)

type mutex
val mutex : unit -> mutex
val sync : mutex -> (unit -> 'a task) -> 'a task
(** Schedules a task such that only one can run simultaneously for a
    given mutex. *)

(* ************************************************************************* *)
(** {2 System Command} *)
(* ************************************************************************* *)

val command :
  ?timeout:int ->
  ?time:float ref ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string array -> int task
  (** Immediately launch a system-process.
      Default timeout is [0], which means no-timeout at all.
      Standard outputs are discarded unless optional buffers are provided.
      To make the task start later, simply use [todo (command ...)]. *)

(* ************************************************************************* *)
(** {2 Shared Tasks}

    When two tasks [A] and [B] share a common sub-task [S],
    cancelling [A] will make [B] fail either. To prevent this, it is
    necessary to make [S] {i shareable} and to use two distinct {i
    instances} of [S] in [A] and [B]. 

    Shared tasks manage the number of their instance and actually run
    or cancel a unique task on demand. In particular, shared tasks can
    be canceled and re-started later.

    @since Oxygen-20120901 *)
(* ************************************************************************* *)

type 'a shared 
  (** Shareable tasks. *)

val shared : descr:string -> retry:bool -> (unit -> 'a task) -> 'a shared 
(** Build a shareable task.  The build function is called whenever a new
    instance is required but no shared instance task is actually running.
    Interrupted tasks (by Cancel or Timeout) are retried for further
    instances. If the task failed, it can be re-launch if [retry] is [true].
    Otherwize, further instances will return [Failed] status. *)

val share : 'a shared -> 'a task 
(** New instance of shared task. *)

(* ************************************************************************* *)
(** {2 Task Server} *)
(* ************************************************************************* *)

val run : unit task -> unit
(** Runs one single task in the background. 
    Typically using [on_idle]. *)

type server

val server :
  ?stages:int ->
  ?procs:int ->
  unit -> server
  (** Creates a server of commands.
      @param stages number of queues in the server.
      Stage 0 tasks are issued first. Default is 1.
      @param procs maximum number of running tasks. Default is 4. *)

val spawn : server -> ?stage:int -> unit task -> unit
  (** Schedules a task on the server.
      The task is not immediately started. *)

val launch : server -> unit
  (** Starts the server if not running yet *)

val cancel_all : server -> unit
  (** Cancel all scheduled tasks *)

val set_procs : server -> int -> unit
  (** Adjusts the maximum number of running process. *)

val on_server_activity : server -> (unit -> unit) -> unit
(** Idle server callback *)

val on_server_start    : server -> (unit -> unit) -> unit
(** On-start server callback *)

val on_server_stop     : server -> (unit -> unit) -> unit
(** On-stop server callback *)

val scheduled  : server -> int (** Number of scheduled process *)
val terminated : server -> int (** Number of terminated process *)

(* ************************************************************************* *)
(** {2 GUI Configuration} *)
(* ************************************************************************* *)

val on_idle : ((unit -> bool) -> unit) ref
  (** Typically modified by GUI.
      [!on_idle f] should repeatedly calls [f] until it returns [false].
      Default implementation rely on [Unix.sleep 1] and [Db.progress].
      See also [Gtk_helper] module implementation. *)

