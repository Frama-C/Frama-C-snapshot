(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Type of the keys of the map. *)
module type Element = sig
  include Datatype.S_with_collections
  val id: t -> int (** Identity of a key. Must verify [id k >= 0] and
                       [equal k1 k2 ==> id k1 = id k2] *)
  val self : State.t
  val pretty_debug: t Pretty_utils.formatter
end

module Make
  (Elt : Element)
  (Set : Hptset.S with type elt = Elt.t)
  (Functor_info : Datatype.Functor_info)
  : Equality_sig.S_with_collections with type elt = Elt.t
