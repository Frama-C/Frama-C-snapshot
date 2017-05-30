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

(** Abstract reductions on Cvalue.V.t *)

open Cvalue
open Cil_types

(** See !{abstract_value.mli} for details about backward operations. *)

(** This function tries to reduce the argument values of a binary operation,
    given its result.
    [typ_res] is a type of [res_value], and [typ_e1] the type of [v1]. *)
val backward_binop:
  typ_res:typ ->
  res_value: V.t ->
  typ_e1:typ ->
  V.t -> binop -> V.t -> (V.t * V.t) option

(** This function tries to reduce the argument value of an unary operation,
    given its result. [typ_arg] is the type of [arg]. *)
val backward_unop:
  typ_arg:typ ->
  unop ->
  arg: V.t ->
  res: V.t ->
  V.t option

(** This function tries to reduce the argument of a cast, given the result of
    the cast.
    [src_typ] is the type of [src_val], [dst_typ] the type of the cast
    and of [dst_val]. *)
val backward_cast:
  src_typ: typ ->
  dst_typ: typ ->
  src_val: V.t ->
  dst_val: V.t ->
  V.t option


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
