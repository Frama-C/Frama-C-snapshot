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

open Cil_types

module Typ : sig
  val attributes_less_equal : typ -> typ -> bool
  val params : typ -> (string * typ * attributes) list
  val params_types : typ -> typ list
  val params_count : typ -> int
  val is_variadic : typ -> bool
end

module Cil : sig
  include module type of Cil

  val ptrType : typ -> typ
  val constPtrType : typ -> typ

  val shortType : typ
  val ushortType : typ

  val shortPtrType : typ
  val ushortPtrType : typ
  val longPtrType : typ
  val ulongPtrType : typ
  val longlongPtrType : typ
  val ulonglongPtrType : typ
  val doublePtrType : typ

  val is_folded_zero : exp -> bool

  (** Standard integer types in C99 (Cf. 6.2.5) *)
  val signedIntegerTypes : typ list
  val unsignedIntegerTypes : typ list
  val signedIntegerPtrTypes : typ list
  val unsignedIntegerPtrTypes : typ list

  val is_signed_integer_type : typ -> bool
  val is_unsigned_integer_type : typ -> bool
  val is_integer_type : typ -> bool
  val is_integer_ptr_type : typ -> bool

  val is_function : varinfo -> bool

  (** @return [true] if varinfo is a variadic function, [false] if it
      is a non-variadic function or if it is not a function. *)
  val is_variadic_function : varinfo -> bool

  (** Does not use {! Globals.Functions.get} nor
      {! Kernel_function.get_return_type}. *)
  val get_fundec_return_type : fundec -> typ

  val get_kf_attributes : kernel_function -> attributes

  (** [integer_ranking_comp t1 t2]
      @return
      [<0] if [t1 < t2]
      [0] if [t1 = t2]
      [>0] if [t1 > t2]
      @raise Invalid_argument if t1 and t2 are not comparable.
  *)
  val integer_ranking_comp : typ -> typ -> int

  (** [integer_promotion t1 t2] returns [true] if [t1 < t2] *)
  val integer_promotion : typ -> typ -> bool

  val get_inst_loc : instr -> location

  val get_stmt_loc : stmt -> location
end


module List : sig
  include module type of List

  (* Constructors *)
  val make : int -> 'a -> 'a list

  exception EmptyList

  (* Get one element *)
  val to_scalar : 'a list -> 'a
  val of_opt : 'a option -> 'a list
  val to_opt : 'a list -> 'a option
  val first : 'a list -> 'a
  val last : 'a list -> 'a (** @raise EmptyList when the list is empty. *)

  (* Sublists *)
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val break : int -> 'a list -> 'a list * 'a list

  (* Iterators *)
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  val iteri2 : (int -> 'a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val mapi2 : (int -> 'a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val reduce_left : ('a -> 'a -> 'a) -> 'a list -> 'a
  val reduce_right : ('a -> 'a -> 'a) -> 'a list -> 'a
  val map_fold_left : ('b -> 'a -> 'c * 'b) -> 'b -> 'a list -> 'c list * 'b

  (* Search *)
  val ifind : ('a -> bool) -> 'a list -> int

  (* Sort *)
  val sort_unique : ('a -> 'a -> int) -> 'a list -> 'a list
  val unique_sorted : ('a -> 'a -> int) -> 'a list -> 'a list
(*
  (** [split k l] when [l] = \[e{_1}; ...; e{_n}\].

      @return
      (\[e{_1}; ...; e{_k}\], \[e{_k+1}; ...; e{_n}\]) if [0 < k < n],

      ([\[\]], [l]) if [k <= 0],

      ([l], [\[\]]) if [k >= n] *)
  val split : int -> 'a list -> 'a list * 'a list *)

  (** [replace i v l] returns a new list where [l.(i)] = [v] *)
  val replace : int -> 'a -> 'a list -> 'a list
end

