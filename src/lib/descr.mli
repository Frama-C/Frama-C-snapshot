(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Type description for safer unmarshalling.

    This module provides a safer API than the library "unmarshal" for
    registering type description.  *)

(** {2 Type declaration} *)

type 'a t
  (** Type of a type description.
      The type variable is for safety only. *)

type packed
  (** Type of an "embeded" type description (that is a type description
      enclosed into one other). *)

(** {2 Smart constructors} *)

val pack: 'a t -> packed
  (** Create an embeded type description from a standard type description. *)

val abstract: 'a Type.t -> 'a t
  (** Similar to the {!Unmarshal.Abstract} constructor. *)

val sum: 'a Type.t -> packed array array -> 'a t
  (** Similar to the {!Unmarshal.Sum} constructor. *)

val dependent_pair: 'b Type.t -> 'a t -> ('a -> 'b t) -> ('a * 'b) t
  (** Similar to the {!Unmarshal.Dependent_pair} constructor. *)

val array: 'a Type.t -> packed -> 'a t
  (** Similar to the {!Unmarshal.Array} constructor. *)

val transform: 'a t -> ('a -> 'a) -> 'a t
  (** Similar to the {!Unmarshal.Transform} constructor with sanity check. *)

val return: 'a t -> (unit -> 'a) -> 'a t
  (** Similar to the {!Unmarshal.Return} constructor with sanity check. *)

val dynamic: 'a Type.t -> (unit -> 'a t) -> 'a t
  (** Similar to the {!Unmarshal.Dynamic} constructor. *)

(** {2 Predefined type description values} *)

val t_int : int t
val t_string : string t
val t_float : float t
val t_bool : bool t
val t_int32 : int32 t
val t_int64 : int64 t
val t_nativeint : nativeint t

(** {2 Convenient functions for building type description} *)

val t_record : 'a Type.t -> packed array -> 'a t
val t_tuple : 'a Type.t -> packed array -> 'a t
val t_couple: 'a t -> 'b t -> ('a * 'b) t
val t_list : 'a t -> 'a list t
val t_ref : 'a t -> 'a ref t
val t_option : 'a t -> 'a option t

val t_hashtbl_unchangedhashs: 'key t -> 'value t -> ('key, 'value) Hashtbl.t t
val t_hashtbl_changedhashs:
  (int -> ('key, 'value) Hashtbl.t) ->
  (('key, 'value) Hashtbl.t -> 'key -> 'value -> unit) ->
  'key t -> 'value t -> ('key, 'value) Hashtbl.t t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
