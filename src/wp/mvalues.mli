(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(** Memory Model Signature                                                    *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Clabels
open Formula
open Cil_types

module type Model =
sig

  module F : Formula.S
  module A : Mint.S with module F = F
  module R : Mfloat.S with module F = F

  type loc

  val tau_of_loc :tau

  val term_of_loc : loc -> F.abstract
  val loc_of_term : c_object -> F.abstract -> loc


  (** {2 Loc arithmetics} *)

  val equal_loc_bool : loc -> loc -> F.boolean
    (** [equal_loc_bool p q] is the boolean that is true when
        locs [p] and [q], that points to elements of type [ty],
        are equal. Returns a term of type [bool]. *)

  val lt_loc_bool : loc -> loc -> F.boolean
    (** [lt_loc_bool p q] is the less than comparison
        of locs [p] and [q], that points to elements of type [ty],
        ie. [p<q]. Returns a term of type [boolean].
    *)

  val le_loc_bool : loc -> loc -> F.boolean
    (** [le_loc_bool p q] is the less than or equal comparison
        of locs [p] and [q], that points to elements of type [ty],
        ie. [p<=q]. Returns a term of type [boolean].
    *)

  val lt_loc : loc -> loc -> F.pred
    (** [lt_loc p q] is the less than comparison
        of locs [p] and [q], that points to elements of type [ty],
        ie. [p<q].
    *)

  val le_loc : loc -> loc -> F.pred
    (** [le_loc p q] is the less than or equal comparison
        of locs [p] and [q], that points to elements of type [ty],
        ie. [p<=q].
    *)

  val equal_loc: loc -> loc -> F.pred

  val minus_loc : loc -> loc -> F.integer
    (** [minus_loc ty p q] is the arithmetics difference of
        locs [p] and [q], that points to elements of type [ty],
        ie. [p-q]. Returns a term of type [integer]. *)

  val is_null : loc -> F.boolean
    (** Take a term representing an [address] and returns a
        term of type [bool]. *)

  (** {2 Special locations} *)

  val null : loc
    (** [null] return the special location of the memory model designing
        the null loc. *)

  (**
   * [cast_loc_to_int t p c_int] :
   * cast [loc] if type [t*] into a term of type [c_int]
   **)

  val cast_loc_to_int : Cil_types.typ -> loc -> Ctypes.c_int -> F.integer
  val cast_int_to_loc : Ctypes.c_int -> F.integer -> Cil_types.typ -> loc

  val pp_loc : Format.formatter -> loc -> unit

end

module type Values =
sig

  include Model

  (** The internal representation of an ACSL value *)
  type value =
    | V_int of Ctypes.c_int * F.integer
    | V_float of Ctypes.c_float * F.real
    | V_pointer of Ctypes.c_object * loc
    | V_record of compinfo * F.record
    | V_union of compinfo * F.urecord
    | V_array of arrayinfo * F.array

  val pp_value : Format.formatter -> value -> unit

  (** Conversion between internal representation of ACSL value and FOL term *)


  val equal : c_object -> F.abstract -> F.abstract -> F.pred
  val eq_array : arrayinfo -> F.array -> F.array -> F.pred
  val eq_record : compinfo -> F.record -> F.record -> F.pred

  val logic_of_value : value -> F.abstract
  val value_of_logic : c_object -> F.abstract -> value
  val tau_of_object : c_object -> tau
  val tau_of_object_array : c_object -> int -> tau
  val tau_of_logic_type : Cil_types.logic_type -> tau
  val pp_tau: Format.formatter -> tau -> unit
  val symb_is_init : c_object -> string option
  val symb_is_init_range : c_object -> string option

end

module type Data =
sig

  include Values

  type m_of_mem
  val tau_of_mem : tau
  val forall_loc : F.pool -> F.var list * loc

  val index : loc -> c_object -> F.integer -> loc
  val field : loc -> fieldinfo -> loc

  val load_mem  : m_of_mem F.term -> c_object -> loc -> value
  val store_mem : m_of_mem F.term -> c_object -> loc -> value -> m_of_mem F.term

end

module type S =
sig

  include Values
  module L:Formula.Logic with module F = F

  (** {2 Memory, Field and Array access} *)

  type mem

  val mem : unit -> mem

  val global : varinfo -> unit

  val cvar  : mem -> varinfo -> loc

  val shift : loc -> c_object -> F.integer -> loc
    (** [shift ptr tau k] computes the location
        of [ptr+k], where [ptr] is a pointer to a value of type [tau]. *)

  val index : loc -> c_object -> F.integer -> loc
  (** [index tab tau k] computes the location
      of [tab[k]], where [tab] is an array with elements of type [tau]. *)

  val startof : loc -> c_object -> loc
  (** [startof] return a pointer to the first element of an array *)

  val field : loc -> fieldinfo -> loc

  val load  : mem -> c_object -> loc -> value

  val cast_loc_to_loc :  typ -> typ -> loc -> loc
  (** [cast_loc_to_loc t1 t2 l] returns the casted location of type [t2]
            from the location [l] of type [t1] *)

end
