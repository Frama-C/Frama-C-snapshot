(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

open Cil_types

(** This module contains various declarations and functions that
    are useful for plugins written on top of the results of Value. *)


type call_site = kernel_function * kinstr
type callstack = call_site list
(** Value callstacks, as used e.g. in Db.Value hooks *)

module Callsite: Datatype.S_with_collections with type t = call_site
module Callstack: Datatype.S_with_collections with type t = call_site list

type 'a callback_result =
  | Normal of 'a
  | NormalStore of 'a * int
  | Reuse of int


val accept_base :
  with_formals:bool ->
  with_locals:bool ->
  kernel_function -> Base.t -> bool
(** [accept_base formals locals kf b] returns [true] if and only [b] is
    - a global
    - a formal or local of one of the callers of [kf]
    - a formal or local of [kf] and the corresponding argument is [true]
*)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

