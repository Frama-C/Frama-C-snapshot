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

(** Map from alarms to status.
    Returned by the abstract semantics to report the possible undefined
    behaviors. *)

(** An alarm is a guard against an undesirable behavior. If the status of an
    assertion is true, then its corresponding undesirable behavior never
    occurs. Otherwise, the undesirable behavior may occur (unknown status) or
    definitely happens if the program point is reachable (false status). *)

(** The maps are partial. Missing assertions are implicitly bound to a default
    status. There are two kinds of alarm maps:
    - closed maps [Just s], where all missing assertions are considered as true:
      [s] contains the only alarms that can occur.
    - open maps [AllBut s], where all missing assertions are considered as
      unknown: [s] contains the only alarms whose status is known. *)

type s
type t = private Just of s | AllBut of s
type alarm = Alarms.t
type status = True | False | Unknown

type 'a if_consistent = [ `Value of 'a | `Inconsistent ]

(* Logical status bound to assertions. *)
module Status : sig
  include Datatype.S_with_collections with type t := status
  val join: status -> status -> status
  val join_list: status list -> status
  val inter: status -> status -> status if_consistent
end

(** no alarms: all potential assertions have a True status.
    = Just empty *)
val none : t

(** all alarms: all potential assertions have a Unknown status.
    = AllBut empty *)
val all : t

(** [set alarm status t] binds the [alarm] to the [status] in the map [t]. *)
val set : alarm -> status -> t -> t

(** ! Different semantics according to the kind of the alarm map.
    [add alarm [Just s] = set alarm Unknown (Just s)]
    [add alarm [AllBut s] = set alarm True (AllBut s)] *)
val add : alarm -> t -> t

(** Returns the status of a given alarm. *)
val find : alarm -> t -> status

(** Are two maps equal? *)
val equal : t -> t -> bool

(** Is there an assertion with a non True status ? *)
val is_empty : t -> bool

(** [singleton alarm] creates the map [add alarm none]:
    [alarm] has a Unknown status, and all others have a True status. *)
val singleton : alarm -> t

(** Pointwise union of property status: the least precise status is kept. *)
val union: t -> t -> t

(** Pointwise intersection of property status: the most precise status is kept.
    May return Inconsistent in case of incompatible status bound to an alarm. *)
val inter: t -> t -> t if_consistent

val exists: (alarm -> status -> bool) -> default:(status -> bool) -> t -> bool
val for_all: (alarm -> status -> bool) -> default:(status -> bool) -> t -> bool

val iter: (alarm -> status -> unit) -> t -> unit

(** Emits the alarms according to the given warn mode, at the given
    instruction. *)
val emit: Cil_types.kinstr -> t -> unit

(** Calls the functions registered in the [warn_mode] according to the
    set of alarms. *)
val notify: CilE.warn_mode -> t -> unit

val pretty : Format.formatter -> t -> unit
val pretty_status : Format.formatter -> status -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
