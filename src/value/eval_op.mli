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

(** Numeric evaluation. Factored with evaluation in the logic. *)

open Cil_types
open Cvalue

(** Transformation a value into an offsetmap of size [sizeof(typ)] bytes. *)
val offsetmap_of_v: typ:Cil_types.typ -> V.t -> V_Offsetmap.t

(** Specialization of the function above for standard types *)
val wrap_int: V.t -> V_Offsetmap.t option
val wrap_ptr: V.t -> V_Offsetmap.t option
val wrap_double: V.t -> V_Offsetmap.t option

(** Reads the contents of the offsetmap (assuming it contains [sizeof(typ)]
    bytes), and return them as an uninterpreted value. *)
val v_uninit_of_offsetmap:
  with_alarms:CilE.warn_mode ->
  typ:Cil_types.typ -> V_Offsetmap.t -> V_Or_Uninitialized.t

(** Reads the contents of the offsetmap (assuming it contains [sizeof(typ)]
    bytes) as a value of type V.t, then convert the result to type [typ] *)
val v_of_offsetmap:
  with_alarms:CilE.warn_mode ->
  typ:Cil_types.typ -> V_Offsetmap.t -> V.t


(** Bitfields *)
val is_bitfield: typ -> bool

val cast_lval_bitfield : typ -> Int_Base.t -> Cvalue.V.t -> Cvalue.V.t

val sizeof_lval_typ: typ -> Int_Base.t
(** Size of the type of a lval, taking into account that the lval might have
    been a bitfield. *)

val reinterpret_int:
  with_alarms:CilE.warn_mode -> Cil_types.ikind -> V.t -> V.t
(** Read the given value value as an int of the given [ikind]. Warn if the
    value contains an address. *)

val reinterpret_float:
  with_alarms:CilE.warn_mode -> Cil_types.fkind -> V.t -> V.t
(** Read the given value value as a float int of the given [fkind]. Warn if the
    value contains an address, or is not representable as a finite float.  *)

val reinterpret:
  with_alarms:CilE.warn_mode -> Cil_types.typ -> V.t -> V.t


val eval_binop_float :
  with_alarms:CilE.warn_mode ->
  Ival.Float_abstract.rounding_mode ->
  Cil_types.fkind option ->
  Cvalue.V.t -> binop -> Cvalue.V.t -> Cvalue.V.t

val eval_binop_int :
  with_alarms:CilE.warn_mode ->
  ?typ:typ ->
  te1:typ ->
  Cvalue.V.t -> binop -> Cvalue.V.t -> Cvalue.V.t

val eval_unop:
  check_overflow:bool ->
  with_alarms:CilE.warn_mode ->
  Cvalue.V.t ->
  typ (** Type of the expression under the unop *) ->
  Cil_types.unop -> Cvalue.V.t

val handle_overflow:
  with_alarms:CilE.warn_mode ->
  warn_unsigned:bool -> Cil_types.typ -> Cvalue.V.t -> Cvalue.V.t

val do_promotion:
  with_alarms:CilE.warn_mode ->
  Ival.Float_abstract.rounding_mode ->
  src_typ:Cil_types.typ ->
  dst_typ:Cil_types.typ ->
  Cvalue.V.t -> (Format.formatter -> unit) -> Cvalue.V.t

type reduce_rel_int_float = {
  reduce_rel_symmetric :
    bool -> binop -> Cvalue.V.t -> Cvalue.V.t -> Cvalue.V.t;
  reduce_rel_antisymmetric :
    typ_loc:typ ->
    bool -> binop -> Cvalue.V.t -> Cvalue.V.t -> Cvalue.V.t;
}
val reduce_rel_int : reduce_rel_int_float
val reduce_rel_float : bool -> reduce_rel_int_float

val eval_float_constant:
  with_alarms:CilE.warn_mode -> float -> fkind -> string option -> Cvalue.V.t
(** The arguments are the approximate float value computed during parsing, the
    size of the floating-point type, and the string representing the initial
    constant if available. Return an abstract value that may be bottom if the
    constant is outside of the representable range, or that may be imprecise
    if it is not exactly representable. *)


(** Change all offsets to top_int. Currently used to approximate volatile
    values. *)
val light_topify: Cvalue.V.t -> Cvalue.V.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
