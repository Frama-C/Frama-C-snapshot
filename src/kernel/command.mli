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

(** Useful high-level system operations. 
    @plugin development guide *)

(* ************************************************************************* *)
(** {2 File Utilities} *)
(* ************************************************************************* *)

val filename : string -> string -> string

val pp_to_file : string -> (Format.formatter -> unit) -> unit
  (** [pp_to_file file pp] runs [pp] on a formatter that writes into [file].
      The formatter is always properly flushed and closed on return.
      Exceptions in [pp] are re-raised after closing. *)

val pp_from_file : Format.formatter -> string -> unit
  (** [pp_from_file fmt file] dumps the content of [file] into the [fmt].
      Exceptions in [pp] are re-raised after closing. *)

val bincopy : string -> in_channel -> out_channel -> unit
  (** [copy buffer cin cout] reads [cin] until end-of-file
      and copy it in [cout].
      [buffer] is a temporary string used during the copy.
      Recommanded size is [2048]. *)

val copy : string -> string -> unit
  (** [copy source target] copies source file to target file using [bincopy]. *)

val read_file : string -> (in_channel -> 'a) -> 'a
  (** Properly close the channel and re-raise exceptions *)
val read_lines : string -> (string -> unit) -> unit
  (** Iter over all text lines in the file *)
val write_file : string -> (out_channel -> 'a) -> 'a
  (** Properly close the channel and re-raise exceptions *)
val print_file : string -> (Format.formatter -> 'a) -> 'a
  (** Properly flush and close the channel and re-raise exceptions *)

(* ************************************************************************* *)
(** {2 Timing Utility} *)
(* ************************************************************************* *)

type timer = float ref
type 'a result = Result of 'a | Error of exn
val catch : ('a -> 'b) -> 'a -> 'b result
val return : 'a result -> 'a
val time : ?rmax:timer -> ?radd:timer -> ('a -> 'b) -> 'a -> 'b
  (** Compute the ellapsed time with [Sys.time].
      The [rmax] timer is maximized and the [radd] timer is cumulated.
      Computed result is returned, or exception is re-raised. *)

(* ************************************************************************* *)
(** {2 System commands} *)
(* ************************************************************************* *)

val full_command :
  string -> string array
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> Unix.process_status
  (** Same arguments as {Unix.create_process} but returns only when
      execution is complete.
      @raise Sys_error when a system error occurs *)

type process_result =
  | Not_ready of (unit -> unit)
  | Result of Unix.process_status
      (** [Not_ready f] means that the child process is not yet finished and
          may be terminated manually with [f ()]. *)

val full_command_async :
  string -> string array
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> (unit -> process_result)
  (** Same arguments as {Unix.create_process}.
      @return a function to call to check if the process execution
      is complete.
      You must call this function until it returns a Result
      to prevent Zombie processes.
      @raise Sys_error when a system error occurs *)

val command_async :
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string array
  -> (unit -> process_result)
  (** Same arguments as {Unix.create_process}.
      @return a function to call to check if the process execution
      is complete.
      You must call this function until it returns a Result
      to prevent Zombie processes.
      When this function returns a Result, the stdout and stderr of the child
      process will be filled into the arguments buffer.
      @raise Sys_error when a system error occurs *)

val command :
  ?timeout:int ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string array
  -> Unix.process_status
  (** Same arguments as {Unix.create_process}.
      When this function returns, the stdout and stderr of the child
      process will be filled into the arguments buffer.
      @raise Sys_error when a system error occurs
      @raise Db.Cancel when the computation is interrupted or on timeout *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
