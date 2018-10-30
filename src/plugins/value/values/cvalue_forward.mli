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
open Abstract_value

val are_comparable: Abstract_interp.Comp.t -> V.t -> V.t -> bool

val assume_non_zero: V.t -> V.t truth
val assume_bounded: bound_kind -> bound -> V.t -> V.t truth
val assume_not_nan: assume_finite:bool -> fkind -> V.t -> V.t truth
val assume_comparable: pointer_comparison -> V.t -> V.t -> (V.t * V.t) truth

val forward_binop_int: typ: typ -> V.t -> binop -> V.t -> V.t

val forward_binop_float: Fval.kind -> V.t -> binop -> V.t -> V.t

val forward_unop: typ -> unop -> V.t -> V.t

val rewrap_integer: Eval_typ.integer_range -> V.t -> V.t

val reinterpret: typ -> V.t -> V.t

val cast_float_to_int: Eval_typ.integer_range -> V.t -> V.t
(* Cast from floating-point to integer. *)

val forward_cast:
  src_type:Eval_typ.scalar_typ -> dst_type:Eval_typ.scalar_typ -> V.t -> V.t

(** [make_volatile ?typ v] makes the value [v] more general (to account for
    external modifications), whenever [typ] is [None] or when it has type
    qualifier [volatile]. *)
val make_volatile: ?typ:typ -> V.t -> V.t

val eval_float_constant: float -> fkind -> string option -> V.t

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
