(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
type status = Abstract_interp.Comp.result = True | False | Unknown

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

(** Returns the status of a given alarm. *)
val find : alarm -> t -> status

(** Are two maps equal? *)
val equal : t -> t -> bool

(** Is there an assertion with a non True status ? *)
val is_empty : t -> bool

(** [singleton ?status alarm] creates the map [set alarm status none]:
    [alarm] has a by default an unkown status (which can be overridden through
    [status]), and all others have a True status. *)
val singleton : ?status:status -> alarm -> t

(** Combines two alarm maps carrying different sets of alarms.  If [t1] and [t2]
    are sound alarm maps for the evaluation in the same state of the expressions
    [e1] and [e2] respectively, then [combine t1 t2] is a sound alarm map for
    both evaluations of [e1] and [e2]. *)
val combine: t -> t -> t

(** Pointwise union of property status: the least precise status is kept.
    If [t1] and [t2] are sound alarm maps for a same expression [e] in states
    [s1] and [s2] respectively, then [union t1 t2] is a sound alarm map for [e]
    in states [s1] and [s2]. *)
val union: t -> t -> t

(** Pointwise intersection of property status: the most precise status is kept.
    May return Inconsistent in case of incompatible status bound to an alarm.
    If [t1] and [t2] are both sound alarm maps for a same expression [e] in the
    same state, then [inter t1 t2] is also a sound alarm map for [e]. *)
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
