(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
