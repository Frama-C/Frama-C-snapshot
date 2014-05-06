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

(** Sets of Cvalue.Model.t implemented imperatively. Current implementation
    is optimized to detect similarities in the memory states *)

type t

val pretty : Format.formatter -> t -> unit

(** Creation *)
val empty : unit -> t
val singleton : Cvalue.Model.t -> t

(** Information *)
val is_empty : t -> bool
val length : t -> int

(** Adding elements. *)
exception Unchanged (** The three next functions raise [Unchanged] if the
                        element(s) was already present. *)
val merge_set_return_new : State_set.t -> t -> State_set.t

(** Iterators *)
val fold : ( Cvalue.Model.t -> 'a -> 'a) -> t -> 'a  -> 'a
val iter : (Cvalue.Model.t -> unit) -> t -> unit
val exists : (Cvalue.Model.t -> bool) -> t -> bool

(** Export *)
val join : t -> Cvalue.Model.t
val to_set : t -> State_set.t
val to_list : t -> Cvalue.Model.t list


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
