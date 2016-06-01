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


module type S = sig
  include Abstract_domain.Internal
  val key : t Abstract_domain.key

  val pretty_debug : Format.formatter -> t -> unit

  type equalities
  val project : t -> equalities
end


module Make
    (Atom : Equality_term.Atom)
    (Equality : Equality_sig.S_with_collections with type elt = Atom.t)
    (Value : Abstract_value.External)
  : S with type value = Value.t
       and type location = Precise_locs.precise_location
       and type equalities := Equality.Set.t

