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

(** Atom of the predicates. *)

open Cil_types

type atom =
  (* "Living" terms *)
  | Exp of exp (** Expression cil *)
  | Lvalue of lval (** Représentation d'une lvalue *)
  (* "Dead" terms *)
  | Old of atom (** Evaluation of a term at the call point of the current function *)
  | Fix of atom (** Fixed term : not modified by a [subst] operation. *)


module type Atom = sig
  include Datatype.S_with_collections

  val make : atom -> t
  val get : t -> atom

  (* Constructors *)
  val of_exp : exp -> t
  val of_lval : lval -> t
  val old : t -> t
  val reborn : t -> t
  val fix : t -> t
  val release : t -> t

  (** Utilitaries *)
  val is_alive : t -> bool
  val is_old : t -> bool
  val is_fix : t -> bool

  val id : t -> int
  val self : State.t

  val debug : t -> string
  val pretty_debug : t Pretty_utils.formatter

  module Hptset : Hptset.S with type elt = t
  module Lattice_Set : Lattice_type.Lattice_Hashconsed_Set with module O = Hptset

  module Lmap_Bitwise: Lmap_bitwise.Location_map_bitwise with type v = Lattice_Set.t
end


module Atom : Atom

