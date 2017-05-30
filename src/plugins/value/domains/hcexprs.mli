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

(** Hash-consed expressions and lvalues. *)

open Cil_types

type unhashconsed_exprs = private E of exp | LV of lval
(** lvalues are never stored under a constructor [E], only [LV] *)

module Datatype_UHCE: Datatype.S with type t = unhashconsed_exprs

(** Hashconsed {!hash_consed_exprs} *)
type hashconsed_exprs

(** Datatype + utilities functions for {!hashconsed_exprs}. *)
module HCE: sig
  include Datatype.S_with_collections with type t = hashconsed_exprs
  val pretty_debug: t Pretty_utils.formatter

  val id: t -> int

  val of_lval: lval -> t
  val of_exp: exp -> t

  val to_exp: t -> exp

  val get: t -> unhashconsed_exprs

  val self: State.t
end


(** Hashconsed sets of symbolic expressions. *)
module HCESet: Hptset.S with type elt = HCE.t


(** Maps from symbolic expressions to their memory dependencies, expressed as a
    {!Locations.Zone.t}. *)
module HCEToZone: sig
  include Datatype.S_with_collections

  val empty: t

  val add: HCE.t -> Locations.Zone.t -> t -> t
  val remove: HCE.t -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val is_included: t -> t -> bool

  val find: HCE.t -> t -> Locations.Zone.t
  (** may raise [Not_found] *)

  val find_default: HCE.t -> t -> Locations.Zone.t
  (** returns the empty set when the key is not bound *)

end


(** Maps froms {!Base.t} to set of {!HCE.t}. *)
module BaseToHCESet: sig
  include Datatype.S_with_collections

  val empty: t

  val add: Base.t -> HCESet.t -> t -> t
  val remove: Base.t -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t

  val find: Base.t -> t -> HCESet.t
  (** may raise [Not_found] *)

  val find_default: Base.t -> t -> HCESet.t
  (** returns the empty set when the key is not bound *)
end
