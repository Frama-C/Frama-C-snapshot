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
(** Server Main Process *)
(* -------------------------------------------------------------------------- *)

type json = Json.t
type kind = [ `GET | `SET | `EXEC ]
val string_of_kind : kind -> string
val pp_kind : Format.formatter -> kind -> unit

(* -------------------------------------------------------------------------- *)
(** {2 Request Registry} *)
(* -------------------------------------------------------------------------- *)

val register : kind -> string -> (json -> json) -> unit
val find : string -> (kind * (json -> json)) option
val exec : string -> json -> json (** @raises Not_found if not registered *)

(* -------------------------------------------------------------------------- *)
(** {2 Server Main Process} *)
(* -------------------------------------------------------------------------- *)

(** Type of request messages.
    Parametrized by the type of request identifiers. *)
type 'a request = [
  | `Poll
  | `Request of 'a * string * json
  | `Kill of 'a
  | `Shutdown
]

(** Type of response messages.
    Parametrized by the type of request identifiers. *)
type 'a response = [
  | `Data of 'a * json
  | `Error of 'a * string
  | `Killed of 'a
  | `Rejected of 'a
]

(** A paired request-response message.
    The callback will be called exactly once for each received message. *)
type 'a message = {
  requests : 'a request list ;
  callback : 'a response list -> unit ;
}

(**
   Run a server with the provided low-level network primitives to
   actually exchange data.

   The function does not return until the server is explicitely
   Shutdown. Logs are monitored unless [~logs:false] is specified.

   Default equality is the standard `(=)` one.
*)
val run :
  pretty:(Format.formatter -> 'a -> unit) ->
  ?equal:('a -> 'a -> bool) ->
  fetch:(unit -> 'a message option) ->
  unit -> unit

(** Yield the server during the currently running request.
    Actually, calls [!Db.progress()]. *)
val yield : unit -> unit

(** Kills the currently running request. Actually raises an exception. *)
val kill : unit -> 'a

(** Register a callback to listen for server activity.
    All callbacks would be executed in their order of registration.
    They shall {i never} raise any exception. *)
val on : (bool -> unit) -> unit

(* -------------------------------------------------------------------------- *)
