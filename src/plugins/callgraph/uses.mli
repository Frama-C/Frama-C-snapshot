(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

val iter_in_order: (Kernel_function.t -> unit) -> unit
(** Iterate over all the functions, in the callgraph order, i.e. from callers
    to callees. In case of cycles (mutual recursive functions), the order is
    unspecified. *)

val iter_in_rev_order: (Kernel_function.t -> unit) -> unit
(** Iterate over all the functions, in the callgraph reverse order, i.e. from
    callees to callers. In case of cycles (mutual recursive functions), the
    order is unspecified. *)

val iter_on_callers : (Kernel_function.t -> unit) -> Kernel_function.t -> unit
(** Iterate over all the callers of a given function in a (reverse) depth-first
    way. Do nothing if the function is not in the callgraph. *)

val iter_on_callees : (Kernel_function.t -> unit) -> Kernel_function.t -> unit
(** Iterate over all the callees of a given function in a (reverse) depth-first
    way. Do nothing if the function is not in the callgraph. *)

val accept_base :
  with_formals:bool ->
  with_locals:bool ->
  Kernel_function.t ->
  Base.t ->
  bool
(** [accept_base formals locals kf b] returns [true] if and only if [b] is
    - a global
    - a formal or local of one of the callers of [kf]
    - a formal or local of [kf] and the corresponding argument is [true]. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
