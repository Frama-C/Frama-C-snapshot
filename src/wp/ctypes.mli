(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(** C-Types                                                                   *)
(* -------------------------------------------------------------------------- *)

open Cil_types

(** Runtime integers. *)
type c_int =
  | UInt8
  | SInt8
  | UInt16
  | SInt16
  | UInt32
  | SInt32
  | UInt64
  | SInt64

val c_int_all : c_int list

(** Runtime floats. *)
type c_float =
  | Float32
  | Float64

(** Array objects, with both the head view and the flatten view. *)
type arrayflat = {
  arr_size     : int ; (** number of elements in the array *)
  arr_dim      : int ; (** number of dimensions in the array *)
  arr_cell     : typ ; (** type of elementary cells of the flatten array. Never an array. *)
  arr_cell_nbr : int ; (** number of elementary cells in the flatten array *)
}

type arrayinfo = {
  arr_element  : typ ;   (** type of the elements of the array *)
  arr_flat : arrayflat option;
}

(** Type of variable, inits, field or assignable values. *)
type c_object =
  | C_int of c_int
  | C_float of c_float
  | C_pointer of typ
  | C_comp of compinfo
  | C_array of arrayinfo

val object_of_pointed: c_object -> c_object
val object_of_array_elem : c_object -> c_object
val object_of_logic_type : logic_type -> c_object
val object_of_logic_pointed : logic_type -> c_object

(** {2 Utilities} *)

val imemo : (c_int -> 'a) -> c_int -> 'a
val fmemo : (c_float -> 'a) -> c_float -> 'a

val is_char : c_int -> bool
val c_char : unit -> c_int     (** Returns the type of [char] *)
val c_bool : unit -> c_int     (** Returns the type of [int] *)
val c_ptr  : unit -> c_int     (** Returns the type of pointers *)

val c_int    : ikind -> c_int   (** Conforms to {Cil.theMachine} *)
val c_float  : fkind -> c_float (** Conforms to {Cil.theMachine} *)
val object_of : typ -> c_object

val is_void : typ -> bool
val is_pointer : c_object -> bool

val char : char -> int64
val constant : exp -> int64
val get_int : exp -> int64 option

val i_bits : c_int -> int (** size in bits *)
val i_bytes : c_int -> int (** size in bytes *)
val signed : c_int -> bool  (** [true] if signed *)
val c_int_bounds: c_int -> Integer.t * Integer.t

(** All sizes are in bits *)

val sub_c_int: c_int -> c_int -> bool

val sub_c_float : c_float -> c_float -> bool

val sizeof_typ : typ -> int
val sizeof_object : c_object -> int
val field_offset : fieldinfo -> int

val no_infinite_array : c_object -> bool
val array_dim : arrayinfo -> c_object * int
val array_size : typ -> int option
val array_dimensions : arrayinfo -> c_object * int option list
  (** Returns the list of dimensions the array consists of.
      None-dimension means undefined one. *)
val dimension_of_object : c_object -> (int * int) option
  (** Returns None for 1-dimension objects, and Some(d,N) for d-matrix with N cells *)

val i_convert : c_int -> c_int -> c_int
val f_convert : c_float -> c_float -> c_float
val promote : c_object -> c_object -> c_object

val pp_int : Format.formatter -> c_int -> unit
val pp_float : Format.formatter -> c_float -> unit
val pp_object : Format.formatter -> c_object -> unit

val basename : c_object -> string
val compare : c_object -> c_object -> int
val equal : c_object -> c_object -> bool
val merge : c_object -> c_object -> c_object
val hash : c_object -> int
val pretty : Format.formatter -> c_object -> unit

module AinfoComparable :
sig
  type t = arrayinfo
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end
