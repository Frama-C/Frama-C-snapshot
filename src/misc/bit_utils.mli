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

val max_bit_address : unit -> Abstract_interp.Int.t
  (** @return the maximal possible offset in bits of a memory base. *)

val max_bit_size : unit -> Abstract_interp.Int.t
  (** @return the maximal possible size in bits of a memory base. *)

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

val pretty_offset:
  typ -> Integer.t -> Format.formatter -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
