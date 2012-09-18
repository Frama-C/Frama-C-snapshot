(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(** Equality-based Simplifier *)

open Logic

(** Simplification Actions *)
type 'a simp =
  | Keep (** Keep the term as it is *)
  | Equal of 'a list (** The term is equal to these others *)
  | Rewrite of 'a (** The term should be replaced by this one *)
  | Condition of 'a * 'a simp * 'a simp (** The action is conditional *)

(** Timer are usefull for controlling constraints propagation. *)

class timer : int ->
object
  method copy : timer (** with same timeout and callbacks *)
  method timeout : int (** [0] by default *)
  method set_timeout : int -> unit (** [0] means no timeout *)
  method on_timeout : (int -> unit) -> unit (** Callback *)
  method start : unit (** Reset the timer to its timeout *)
  method stop : unit (** Set time out of timeout. *)
  method loop : bool (** Increment the timer. Return [true] if not timeout. *)
  method break : bool (** Increment the timer. Return [true] if time is over. *)
  method check : unit (** Increment the timer. Raise [Exit] if time is over. *)
  method time : int (** Current time. *)
  method is_over : bool (** Current time less than timeout. *)
end

(** A simplifier is used to infer new facts during congruence closure. *)

class type ['a] simplifier =
object
  method copy : 'a simplifier
  method have : 'a -> unit
  method merge : old:'a -> by:'a -> unit
  method simplify : 'a -> 'a simp
end

(** Extensible simplifier algorithm *)
module Make(T : Term) :
sig

  val debug : bool ref

  open T

  type ground

  val cc_pretty : Format.formatter -> ground -> unit
  val cc_make : ?timeout:int -> term simplifier list -> ground
  val cc_assume : ground -> term list -> ground option
  val cc_simplify : ground -> term -> term

  class cc_record : object inherit [term] simplifier end
  class cc_arrays : object inherit [term] simplifier end

end
