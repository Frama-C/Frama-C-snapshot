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

(** Sets over ordered types.

    This module implements the set data structure.
    All operations over sets
    are purely applicative (no side-effects). *)


(** Output signature of the functor {!Set.Make}. *)
module type S = sig

    include Datatype.S_with_collections
    include FCSet.S_Basic_Compare with type t := t
    (** The datatype of sets. *)

    val contains_single_elt: t -> elt option

    val intersects: t -> t -> bool
    (** [intersects s1 s2] returns [true] if and only if [s1] and [s2]
        have an element in common *)


    type 'a shape
    (** Shape of the set, ie. the unique shape of its OCaml value. *)

    val shape: t -> unit shape
    (** Export the shape of the set. *)

    val from_shape: 'a shape -> t
    (** Build a set from another [elt]-indexed map or set. *)

    (** Clear all the caches used internally by the functions of this module.
        Those caches are not project-aware, so this function must be called
        at least each a project switch occurs. *)
    val clear_caches: unit -> unit
end

module Make(X: Hptmap.Id_Datatype)
  (Initial_Values : sig val v : X.t list list end)
  (Datatype_deps: sig val l : State.t list end) :
  sig
    include S with type elt = X.t
              and type 'a shape = 'a Hptmap.Shape(X).t
    val self : State.t
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
