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

