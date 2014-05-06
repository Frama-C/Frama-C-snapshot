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

(** Type descriptor for safe unmarshalling.

    This module provides a safe API on top of modules {!Unmarshal} of
    {!Structural_descr}, using module {!Type}. This module offers the same
    powerfulness than {!Unmarshal}, but in a safe way. *)

(* ********************************************************************** *)
(** {2 Type declaration} *)
(* ********************************************************************** *)

type 'a t
(** Type of a type descriptor. *)

(* ********************************************************************** *)
(** {2 Predefined type descriptors} *)
(* ********************************************************************** *)

val t_unit: unit t
val t_int : int t
val t_string : string t
val t_float : float t
val t_bool : bool t
val t_int32 : int32 t
val t_int64 : int64 t
val t_nativeint : nativeint t

val unmarshable: 'a t
(** Descriptor for unmarshalable types.
    @since Carbon-20101201 *)

val is_unmarshable: 'a t -> bool
(** @since Carbon-20101201 *)

val is_abstract: 'a t -> bool
(** @since Neon-20130301 *)

(* ********************************************************************** *)
(** {2 Type descriptor builders} *)
(* ********************************************************************** *)

exception Invalid_descriptor
(** @since Carbon-20101201 *)

(** {3 Builders for standard OCaml types} *)

val t_record : Structural_descr.pack array -> 'a -> 'a t
(** Type descriptor for records (the length of the array must be equal to the
    number of fields in the record).
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val t_tuple : Structural_descr.pack array -> 'a -> 'a t
(** Type descriptor for tuples of any range (the length of the array range is
    the range of the tuple).
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val t_pair: 'a t -> 'b t -> ('a * 'b) t
(** Type descriptor for pairs (2-tuples).
    Safer that [t_tuple] for pairs.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val t_list : 'a t -> 'a list t
(** Type descriptor for lists.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val t_ref : 'a t -> 'a ref t
(** Type descriptor for references.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val t_option : 'a t -> 'a option t
(** Type descriptor for options.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val t_queue: 'a t -> 'a Queue.t t
(** Type descriptor for queues.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

(** {3 Builders from others datatypes of the Type library} *)

val of_type: 'a Type.t -> 'a t
(** Type descriptor from the type value.
    @since Carbon-20101201 *)

val of_structural: 'a Type.t -> Structural_descr.t -> 'a t
(** Type descriptor from the structural descriptor. The given type value
    ensures safety.
    @since Carbon-20101201 *)

(** {3 Builders mapping {!Unmarshal}'s transformers} *)

val dependent_pair: 'a t -> ('a -> 'b t) -> ('a * 'b) t
(** Similar to {!Unmarshal.Dependent_pair}, but safe.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val transform: 'a t -> ('a -> 'a) -> 'a t
(** Similar to {!Unmarshal.Transform}, but safe.
    @raise Invalid_descriptor if the given descriptor is incorrect. *)

val return: 'a t -> (unit -> 'a) -> 'a t
(** Similar to {!Unmarshal.Return}, but safe.
    @raise Invalid_descriptor if the descriptor cannot be built. *)

val dynamic: (unit -> 'a t) -> 'a t
  (** Similar to {!Unmarshal.Dynamic}.
      @raise Invalid_descriptor if the descriptor cannot be built. *)

(* ********************************************************************** *)
(** {2 Coercions} *)
(* ********************************************************************** *)

val str: 'a t -> Structural_descr.t
(** @raise Invalid_descriptor if the given descriptor is incorrect.
    @since Carbon-20101201 *)

val pack: 'a t -> Structural_descr.pack
(** @since Carbon-20101201 *)

(* ********************************************************************** *)
(** {2 Safe unmarshaling} *)
(* ********************************************************************** *)

val input_val: in_channel -> 'a t -> 'a
(** @since Carbon-20101201 *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
