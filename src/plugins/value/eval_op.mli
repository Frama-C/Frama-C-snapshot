(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
val wrap_size_t: V.t -> V_Offsetmap.t option
val wrap_int: V.t -> V_Offsetmap.t option
val wrap_ptr: V.t -> V_Offsetmap.t option
val wrap_double: V.t -> V_Offsetmap.t option
val wrap_float: V.t -> V_Offsetmap.t option

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
  Fval.rounding_mode ->
  Cil_types.fkind option ->
  Cvalue.V.t -> binop -> Cvalue.V.t -> Cvalue.V.t

val eval_binop_int :
  with_alarms:CilE.warn_mode ->
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
  Fval.rounding_mode ->
  src_typ:Cil_types.typ ->
  dst_typ:Cil_types.typ ->
  Cvalue.V.t -> (Format.formatter -> unit) -> Cvalue.V.t

val eval_float_constant:
  with_alarms:CilE.warn_mode -> float -> fkind -> string option -> Cvalue.V.t
(** The arguments are the approximate float value computed during parsing, the
    size of the floating-point type, and the string representing the initial
    constant if available. Return an abstract value that may be bottom if the
    constant is outside of the representable range, or that may be imprecise
    if it is not exactly representable. *)

val make_volatile: ?typ:typ -> V.t -> V.t
(** [make_volatile ?typ v] makes the value [v] more general (to account for
    external modifications), whenever [typ] is [None] or when it has type
    qualifier [volatile] *)

val reduce_rel_from_type:
  Cil_types.typ -> (bool -> binop -> Cvalue.V.t -> Cvalue.V.t -> Cvalue.V.t)
(** Reduction of a {!Cvalue.V.t} by [==], [!=], [>=], [>], [<=] and [<].
    [reduce_rel_from_type typ positive op vexpr v] reduces [v]
    so that the relation [v op vexpr] holds. [typ] is the type of the
    expression being reduced. *)

val find:
  with_alarms:CilE.warn_mode ->
  ?conflate_bottom:bool -> Model.t -> Locations.location -> V.t
(** Tempory. Re-export of [Cvalue.Model.find] with a [~with_alarms] argument *)

val add_binding :
  with_alarms:CilE.warn_mode ->
  ?remove_invalid:bool ->
  exact:bool ->
  Model.t ->
  Locations.location ->
  V.t ->
  Model.t
(** Temporary. Re-export of [Cvalue.Model.add_binding] with a [with_alarms]
    argument *)

val add_binding_unspecified :
  with_alarms:CilE.warn_mode ->
  ?remove_invalid:bool ->
  exact:bool ->
  Model.t ->
  Locations.location ->
  V_Or_Uninitialized.t ->
  Model.t
(** Temporary. Re-export of [Cvalue.Model.add_binding_unspecifed] with a
    [with_alarms] argument *)

val copy_offsetmap :
  with_alarms:CilE.warn_mode ->
  Locations.Location_Bits.t -> Integer.t -> Model.t ->
  [ `Bottom | `Map of V_Offsetmap.t | `Top ]
(** Tempory. Re-export of [Cvalue.Model.copy_offsetmap] with a [with_alarms]
    argument *)

val paste_offsetmap:
  with_alarms:CilE.warn_mode ->
  ?remove_invalid:bool ->
  reducing:bool ->
  from:V_Offsetmap.t ->
  dst_loc:Locations.Location_Bits.t ->
  size:Integer.t ->
  exact:bool ->
  Model.t -> Model.t
(** Temporary. Re-exportation of [Cvalue.Model.paste_offsetmap] with a
    [~with_alarms] argument. If [remove_invalid] is set to [true] (default
    is [false], [dst_loc] will be pre-reduced to its valid part. Should be
    set unless you reduce [dst_loc] yourself. *)

val reduce_by_initialized_defined :
  (V_Or_Uninitialized.t -> V_Or_Uninitialized.t) ->
  Locations.location -> Model.t -> Model.t

val apply_on_all_locs:
  (Locations.location -> 'a -> 'a) -> Locations.location -> 'a -> 'a
(** [apply_all_locs f loc state] folds [f] on all the atomic locations
    in [loc], provided there are less than [plevel]. Useful mainly
    when [loc] is exact or an over-approximation. *)

val reduce_by_valid_loc:
  positive:bool ->
  for_writing:bool ->
  Locations.location -> typ -> Model.t -> Model.t
(* [reduce_by_valid_loc positive ~for_writing loc typ state] reduces
   [state] so that [loc] contains a pointer [p] such that [(typ* )p] is
   valid if [positive] holds (or invalid otherwise). *)


(** [write_abstract_value ~with_alarms state lv typ_lv loc_lv v]
   writes [v] at [loc_lv] in [state], casting [v] to respect the type
   [typ_lv] of [lv]. Currently Does 4 things:
   - cast the value to the type of the bitfield it is written into, if needed
   - honor an eventual "volatile" qualifier on [lv]
   - check that [loc_lv] is not catastrophically imprecise.
   - perform the actual abstract write
*)
val write_abstract_value: with_alarms:CilE.warn_mode ->
  Model.t -> lval -> typ -> Locations.Location.t -> V.t -> Model.t

val make_loc_contiguous: Locations.location -> Locations.location
(** 'Simplify' the location if it represents a contiguous zone: instead
    of multiple offsets with a small size, change it into a single offset
    with a size that covers the entire range. *)

val pretty_stitched_offsetmap: Format.formatter -> typ -> V_Offsetmap.t -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
