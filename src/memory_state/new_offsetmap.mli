(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

module Int :
    sig
      type t = int64
    end

module Make :
  functor (V : Lattice_With_Isotropy.S) ->
    sig
      type t 
      val empty : t
      val equal_vv : Int.t * Int.t * V.t -> Int.t * Int.t * V.t -> bool
      val equal : t -> t -> bool
      val get_vv : t -> Int.t * Int.t * V.t
      val get_max : t -> Int.t
      val is_above : int64 -> int64 -> int64 -> int64 -> bool
      type zipper 
      exception End_reached
      exception Empty_tree
      val pr_zipper : Format.formatter -> zipper -> unit
      val pout_zipper : zipper -> unit
      val rezip : zipper -> Int.t -> t -> Int.t * t
      val leftmost_child : Int.t -> zipper -> t -> Int.t * t * zipper
      val rightmost_child : Int.t -> zipper -> t -> Int.t * t * zipper
      val move_right : Int.t -> t -> zipper -> Int.t * t * zipper
      val fold_offset :
        (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a -> 'a) ->
        Int.t -> t -> 'a -> 'a
      val fold :
        (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a -> 'a) ->
        t -> 'a -> 'a
      val iter_offset :
        (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a) -> Int.t -> t -> unit
      val iter : (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a) -> t -> unit
      val pretty_node :
        Format.formatter -> int64 -> int64 -> int64 -> int64 -> V.t -> unit
      val pretty_offset : int64 -> Format.formatter -> t -> unit
      val pretty : Format.formatter -> t -> unit
      val pretty_debug_offset : int64 -> Format.formatter -> t -> unit
      val pretty_debug : Format.formatter -> t -> unit
      val print_offset : int64 -> t -> unit
      val fprint : Format.formatter -> t -> unit
      val print : t -> unit
      val to_list : t -> (Int.t * int64 * Int.t * Int.t * V.t) list
      val to_offsetmap : 'a -> unit
      exception Interval_not_found of Int.t * Int.t
      val subtree_from_interval : Int.t -> Int.t -> Int.t -> t -> Int.t * t
      val make_node :
        int64 ->
        Int.t ->
        Int.t -> t -> Int.t -> t -> Int.t -> Int.t -> V.t -> int64 * t
      val add_node :
        int64 -> int64 -> int64 -> Int.t -> Int.t -> V.t -> t -> int64 * t
      val check : int64 -> t -> unit

      val is_included_generic_exn :
        (V.t -> V.t -> 'a) -> Int.t -> t -> Int.t -> t -> unit
      val is_included : Int.t -> t -> Int.t -> t -> bool
        val join : t -> t -> int64 * t
      val change_binding :
        int64 -> Int.t -> Int.t -> Int.t -> V.t -> t -> Int.t * t
    end

