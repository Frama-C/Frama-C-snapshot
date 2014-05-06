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

(** Internal representations of OCaml type as first class values. These values
    are called structural descriptors.
    @since Carbon-20101201 *)

(* ********************************************************************** *)
(** {2 Type declarations} *)
(* ********************************************************************** *)

(** Type used for handling (possibly mutually) recursive structural
    descriptors. See module {!Recursive}. *)
type recursive

type single_pack = private Unmarshal.t

(** Structural descriptor used inside structures.
    @modify Nitrogen-20111001 this type is now private. Use smart
    constructors instead. *)
type pack = private
  | Nopack                 (** Was impossible to build a pack. *)
  | Pack of single_pack    (** A standard pack. *)
  | Recursive of recursive (** Pack for a recursive descriptor.
                               See module {!Recursive}. *)

(** Type of internal representations of OCaml type.

    Example: the structural descriptor of [A | B of int * bool | C of string]
    is [Structure (Sum [| [| p_int; p_bool |]; [| p_string |] |])]. Ok, in
    this case, just [Abstract] is valid too. *)
type t = private
  | Unknown
  (** Use it either for unmarshable types or if you don't know its internal
      representation. In any case, values of types with this descriptor
      will never be written on disk. *)

  | Abstract
  (** The data is marshable as an usual OCaml value. No specific processing
      will be applied on any part of such a data. *)

  | Structure of structure
  (** Provide a description of the representation of data. 
      @plugin development guide *)

  | T_pack of single_pack (** Internal use only.
                              Do not use it outside the library *)

(** Description with details. *)
and structure = private
  | Sum of pack array array
  (** [Sum c] describes a non-array type where [c] is an array describing
      the non-constant constructors of the type being described (in the order
      of their declarations in that type).  Each element of this latter array
      is an array of [t] that describes (in order) the fields of the
      corresponding constructor. 
      @plugin development guide *)

  | Array of pack (** The data is an array of values of the same type, each
                      value being described by the pack. *)

(* ********************************************************************** *)
(** {2 Pack builders} *)
(* ********************************************************************** *)

val pack: t -> pack
(** Pack a structural descriptor in order to embed it inside another one. 
    @plugin development guide *)

val recursive_pack: recursive -> pack
(** Pack a recursive descriptor.
    @since Nitrogen-20111001 *)

(** Use this module for handling a (possibly recursive) structural descriptor
    [d]. Call [Recursive.create ()] (returning [r]) before building [d]. Build
    [d] and use [Recursive r] in places where [d] should be put. Call
    [Recursive.update r d] after building [d].

    Here is an example for [type t = A | B of t]:
    [let r = Recursive.create () in
    let d = Structure (Sum [| [| Recursive r |] |]) in
    Recursive.update r d] *)
module Recursive: sig
  val create: unit -> recursive
  val update: recursive -> t -> unit
end

(* ********************************************************************** *)
(** {2 Predefined descriptors} *)
(* ********************************************************************** *)

val t_unknown: t
(** @since Neon-20130301 *)

val t_abstract: t
(** @since Neon-20130301 *)

val t_unit : t
val t_int : t
val t_string : t
val t_float : t
val t_bool : t
val t_int32 : t
val t_int64 : t
val t_nativeint : t

val t_record : pack array -> t
val t_tuple : pack array -> t
val t_list : t -> t
val t_ref : t -> t
val t_option : t -> t
val t_array : t -> t
val t_queue: t -> t
val t_sum: pack array array -> t
(** @since Neon-20130301 *)

(** Use the functions below only if the compare/hash functions cannot change by
    marshalling. *)

val t_set_unchanged_compares: t -> t
val t_map_unchanged_compares: t -> t -> t
val t_hashtbl_unchanged_hashs: t -> t -> t

(** Packed versions of predefined descriptors. *)

val p_abstract: pack
(** Equivalent to [pack Abstract] *)

val p_unit : pack
val p_int : pack
(** @plugin development guide *)

val p_string : pack
val p_float : pack
val p_bool : pack
val p_int32 : pack
val p_int64 : pack
val p_nativeint : pack

(* ********************************************************************** *)
(** {2 Internals}

    These values must be used only inside the Type library. *)
(* ********************************************************************** *)

exception Cannot_pack
val unsafe_pack: Unmarshal.t -> pack
(** @raise Cannot_pack if packing failed. *)

val of_pack: single_pack -> t

val cleanup: t -> t
val are_consistent: t -> t -> bool
(** Not symmetrical: check that the second argument is a correct refinement of
    the first one. *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
