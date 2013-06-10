(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Wrapper for [Hashtbl] compatible with all OCaml versions. *)

module type S = sig

  include Hashtbl.S

  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit
    (** Iter on the hashtbl, but respecting the order induced
	by [cmp]. Use [Pervasives.compare] if [cmp] not given. *)

  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Fold on the hashtbl, but respecting the order induced
      by [cmp]. Use [Pervasives.compare] if [cmp] not given. *)
    
end

module Make(H: Hashtbl.HashedType) : S with type key = H.t

val hash : 'a -> int
val hash_param : int -> int -> 'a -> int

