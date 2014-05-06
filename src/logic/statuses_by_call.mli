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

(** Statuses of preconditions specialized at a given call-point. *)

open Cil_types

val setup_precondition_proxy: kernel_function -> Property.t -> unit
(** [setup_precondition_proxy kf p] creates a new property for [p]
    at each syntactic call site of [kf], representing the status
    of [p] at this particular call. [p] is considered proven if and
    only if all its instances are themselves proven. *)

val setup_all_preconditions_proxies: kernel_function -> unit
(** [setup_all_preconditions_proxies kf] is equivalent to calling
    [setup_precondition_proxy] on all the requires of [kf]. *)

val precondition_at_call:
  kernel_function -> Property.t -> stmt -> Property.t
(** [property_at_call kf p stmt] returns the property corresponding to the
    status of the precondition [p] at the call [stmt]. If [stmt]  is a call
    through a pointer, the property at this call is created automatically
    if needed. For direct calls, [setup_precondition_proxy] must have been
    called before. *)

val all_call_preconditions_at:
  warn_missing:bool -> kernel_function -> stmt -> (Property.t * Property.t) list
(** [all_call_preconditions_at create kf stmt] returns the copies of all the
    requires of [kf] for the call statement at [stmt].  The first property in
    the tuple is the require,  the second the copy at the call point.
    If [warn_missing] is true and a copy has not yet been created an error
    is raised. *)

val all_functions_with_preconditions: stmt -> Kernel_function.Hptset.t
(** Returns the set of functions that can be called at the given statement
    and for which a precondition has been specialized at this call.
    Those functions are registered when the function {!precondition_at_call}
    is called. *)


val replace_call_precondition: Property.t -> stmt -> Property.t -> unit
(** [replace_for_call pre stmt pre_at_call] states that [pre_at_call]
    is the property corresponding to the status of [pre] at call [stmt].
    The previous property, if any, is removed. Beware that this may also
    remove some already proved statuses *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
