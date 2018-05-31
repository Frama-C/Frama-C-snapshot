(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Forward operations on Cvalue.V.t *)

open Cvalue
open Cil_types

val are_comparable: Abstract_interp.Comp.t -> V.t -> V.t -> bool

val forward_binop_int:
  context: Eval.binop_context ->
  logic: bool ->
  typ: typ ->
  V.t -> binop -> V.t -> V.t * Alarmset.t

val forward_binop_float: Fval.kind -> V.t -> binop -> V.t -> V.t

val forward_unop:
  context: Eval.unop_context ->
  typ -> unop -> V.t -> V.t * Alarmset.t

val truncate_integer: exp -> Eval_typ.integer_range -> V.t -> V.t * Alarmset.t
val rewrap_integer: Eval_typ.integer_range -> V.t -> V.t

val reinterpret: typ -> V.t -> V.t

val cast_float_to_int_alarms:
  Eval_typ.integer_range -> (unit -> exp) -> V.t -> V.t * Alarmset.t
(* Cast from floating-point to integer. The function given in argument
   returns the expression being cast, used to build the alarms. *)

val cast: src_typ: typ -> dst_typ: typ -> exp -> V.t -> V.t * Alarmset.t

(** [make_volatile ?typ v] makes the value [v] more general (to account for
    external modifications), whenever [typ] is [None] or when it has type
    qualifier [volatile]. *)
val make_volatile: ?typ:typ -> V.t -> V.t

val eval_float_constant: float -> fkind -> string option -> V.t

val restrict_float:
  remove_infinite:bool -> fkind -> exp -> V.t -> V.t * Alarmset.t



(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
