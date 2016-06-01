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

(** Some bit manipulations. *)

open Cil_types

val sizeofchar: unit -> Integer.t
  (** [sizeof(char)] in bits *)

val sizeofpointer: unit -> int
  (** [sizeof(char* )] in bits *)

val sizeof: typ -> Int_Base.t
  (** [sizeof ty] is the size of [ty] in bits. This function may return
      [Int_Base.top]. *)

val osizeof: typ -> Int_Base.t
  (** [osizeof ty] is the size of [ty] in bytes. This function may return
      [Int_Base.top]. *)

exception Neither_Int_Nor_Enum_Nor_Pointer

val is_signed_int_enum_pointer: typ -> bool
  (** [true] means that the type is signed.
      @raise Neither_Int_Nor_Enum_Nor_Pointer if the sign of the type is not
      meaningful. *)

val signof_typeof_lval: lval -> bool
  (** @return the sign of type of the [lval]. [true] means that the type is
      signed. *)

val sizeof_vid: varinfo -> Int_Base.t
  (** @return the size of the type of the variable in bits. *)

val sizeof_lval: lval -> Int_Base.t
  (** @return the size of the type of the left value in bits. *)

val sizeof_pointed: typ -> Int_Base.t
  (** @return the size of the type pointed by a pointer or array type in bits.
      Never call it on a non pointer or non array type . *)

val osizeof_pointed: typ -> Int_Base.t
  (** @return the size of the type pointed by a pointer or array type in bytes.
      Never call it on a non pointer or array type. *)

val sizeof_pointed_lval: lval -> Int_Base.t
  (** @return the size of the type pointed by a pointer type of the [lval] in
      bits. Never call it on a non pointer type [lval]. *)

val max_bit_address : unit -> Integer.t
  (** @return the maximal possible offset in bits of a memory base. *)

val max_bit_size : unit -> Integer.t
  (** @return the maximal possible size in bits of a memory base. *)

val max_byte_address : unit -> Integer.t
  (** @return the maximal possible offset in bytes of a memory base.
      @since Aluminium-20160501 *)

val max_byte_size : unit -> Integer.t
  (** @return the maximal possible size in bits of a memory base.
      @since Aluminium-20160501 *)

(** {2 Pretty printing} *)

val pretty_bits:
  typ ->
  use_align:bool ->
  align:Abstract_interp.Rel.t ->
  rh_size:Integer.t ->
  start:Integer.t ->
  stop:Integer.t -> Format.formatter -> bool * typ option
  (** Pretty prints a range of bits in a type for the user.
      Tries to find field names and array indexes, whenever possible. *)


(** {2 Mapping from numeric offsets to symbolic ones.} *)

(** We want to find a symbolic offset that corresponds to a numeric one, with
    one additional criterion: *)
type offset_match =
| MatchType of typ (** Offset that has this type (modulo attributes) *)
| MatchSize of Integer.t (** Offset that has a type of this size *)
| MatchFirst (** Return first symbolic offset that matches *)

exception NoMatchingOffset

(** [find_offset typ ~offset ~size] finds a subtype [t] of [typ] that describes
    the type of the bits [offset..offset+size-1] in [typ]. May return a subtype
    of [typ], or a type that is a sub-array of an array type in [typ].
    Also returns a {!Cil_types.offset} [off] that corresponds to [offset].
    (But we do not have the guarantee that [typeof(off) == typ], because of
    sub-arrays.) 
    @raise NoMatchingOffset when no offset matches. *)
val find_offset:
  typ -> offset:Integer.t -> offset_match -> Cil_types.offset * Cil_types.typ

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
