(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
    Returned by the abstract domains and the evaluation functions of an
    abstract interpretation analysis. *)

(** There are two kinds of alarms map:
    - [Just s], where all missing properties are considered as true:
     [s] contains the only alarms that can occur.
    - [AllBut s], where all missing properties are considered as unknown:
     [s] contains the only alarms whose status is known. *)

type s
type t = private Just of s | AllBut of s
type alarm = Alarms.t
type status = True | False | Unknown

type 'a if_consistent = [ `Value of 'a | `Inconsistent ]

module Status : sig
  include Datatype.S_with_collections with type t := status
  val join: status -> status -> status
  val join_list: status list -> status
  val inter: status -> status -> status if_consistent
end

(** no alarms: all potential assertions have a True status.
    = Just empty *)
val none : t

(** all alarms: all potential assertions have a Dont_know status.
    = AllBut empty *)
val all : t

val equal : t -> t -> bool

(** Is there an assertion with a non True status ? *)
val is_empty : t -> bool

val singleton : alarm -> t
val add' : alarm -> status -> t -> t

(** ! Different semantics according to the kind of alarms map.
    [add alarm [Just s] = add' alarm Dont_know (Just s)]
    [add alarm [AllBut s] = add' alarm True (AllBut s)]*)
val add : alarm -> t -> t

(** Returns the status of a given alarm. *)
val find : alarm -> t -> status

(** Pointwise union of property status: the least precise status is kept. *)
val union: t -> t -> t

(** Pointwise intersection of property status: the most precise status is kept.
    May return Inconsistent in case of incompatible status bound to an alarm. *)
val inter: t -> t -> t if_consistent

val exists: (alarm -> status -> bool) -> default:(status -> bool) -> t -> bool
val for_all: (alarm -> status -> bool) -> default:(status -> bool) -> t -> bool


val iter: (alarm -> status -> unit) -> t -> unit

(** Emits the alarms according to the given warn mode. *)
val emit: CilE.warn_mode -> t -> unit

val start_stmt : Cil_types.kinstr -> unit
val end_stmt : unit -> unit
val current_stmt : unit -> Cil_types.kinstr

val pretty : Format.formatter -> t -> unit


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
