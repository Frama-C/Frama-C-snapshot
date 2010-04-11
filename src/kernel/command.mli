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

(* ************************************************************************* *)
(** System commands *)
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

type process_result = Not_ready | Result of Unix.process_status
  (** [Not_ready] means that the child process is not yet finished. *)

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
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string array
  -> Unix.process_status
  (** Same arguments as {Unix.create_process}.
      When this function returns, the stdout and stderr of the child
      process will be filled into the arguments buffer.
      @raise Sys_error when a system error occurs *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
