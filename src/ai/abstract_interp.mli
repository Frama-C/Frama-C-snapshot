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

(** Functors for generic lattices implementations. 
    @plugin developer guide *)

exception Not_less_than
(** Raised by {!Lattice.cardinal_less_than}. *)

open Lattice_type

module Int : sig
  include module type of Integer with type t = Integer.t
  include Lattice_Value with type t := t

  val fold : (t -> 'a -> 'a) -> inf:t -> sup:t -> step:t -> 'a -> 'a
end

(** "Relative" integers. They are subtraction between two absolute integers *)
module Rel : sig
  type t

  val pretty: t Pretty_utils.formatter

  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int

  val zero: t
  val is_zero: t -> bool

  val sub : t -> t -> t
  val add_abs : Int.t -> t -> Int.t 
  val sub_abs : Int.t -> Int.t -> t
  val pos_rem: t -> Int.t -> t

  val check: rem:t -> modu:Int.t -> bool
end

module Make_Lattice_Base (V : Lattice_Value) : Lattice_Base with type l = V.t
module Make_Lattice_Set (V : Lattice_Value) : Lattice_Set with type O.elt=V.t

module Make_Hashconsed_Lattice_Set
  (V : Hptmap.Id_Datatype)
  (O: Hptset.S with type elt = V.t)
  : Lattice_Hashconsed_Set with module O = O
(** See e.g. base.ml and locations.ml to see how this functor should be
    applied. The [O] module passed as argument is the same as [O] in the
    result. It is passed here to avoid having multiple modules calling
    [Hptset.Make] on the same argument (which is forbidden by the datatype
    library, and would cause hashconding problems) *)

module type Collapse = sig val collapse : bool end

(** If [C.collapse] then [L1.bottom,_] = [_,L2.bottom] = [bottom] *)
(* Untested *)
module Make_Lattice_Product (L1:AI_Lattice_with_cardinal_one) (L2:AI_Lattice_with_cardinal_one) (C:Collapse):
  Lattice_Product with type t1 = L1.t and type t2 = L2.t

(** Uncollapsed product. Literally the set of (e1, e2) ordered pairs
    equipped with the order (e1, e2) < (d1, d2) <==> e1 < d1 && e2 < d2. *)
module Make_Lattice_UProduct (L1:AI_Lattice_with_cardinal_one) (L2:AI_Lattice_with_cardinal_one) :
  Lattice_UProduct with type t1 = L1.t and type t2 = L2.t

(* Untested *)
module Make_Lattice_Sum (L1:AI_Lattice_with_cardinal_one) (L2:AI_Lattice_with_cardinal_one):
  (Lattice_Sum with type t1 = L1.t and type t2 = L2.t)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
