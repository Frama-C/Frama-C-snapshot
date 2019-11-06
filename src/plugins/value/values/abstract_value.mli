(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Abstract numeric values of the analysis. *)

open Cil_types
open Eval

(** Type for the truth value of an assertion in a value abstraction. The two
    last tags should be used only for a product of value abstractions. *)
type 'v truth =
  [ `False              (* The assertion is always false for the value
                           abstraction, leading to bottom and a red alarm. *)
  | `Unknown of 'v      (* The assertion may be true or false for different
                           concretization of the abstraction. The value is
                           reduced by assuming the assertion. *)
  | `True               (* The assertion is always true for the value
                           abstraction (that cannot be reduced). *)
  | `TrueReduced of 'v  (* The assertion is always true according to a component
                           of a product of values, but other components have
                           been reduced by assuming the assertion. *)
  | `Unreachable ]      (* A product of values was incompatible and has led to
                           inconsistent truth value for the assertion. *)

type bound_kind = Alarms.bound_kind = Lower_bound | Upper_bound
type bound = Int of Integer.t | Float of float * fkind

type pointer_comparison = Equality | Relation | Subtraction

(** Signature of abstract numerical values. *)
module type S = sig
  include Datatype.S

  val pretty_typ: typ option -> t Pretty_utils.formatter
  (** Pretty the abstract value assuming it has the type given as argument. *)

  (** {3 Lattice Structure} *)

  val top : t
  val is_included : t -> t -> bool
  val join : t -> t -> t
  val narrow : t -> t -> t or_bottom

  (** {3 Constructors } *)

  val zero: t
  val one: t
  val top_int : t
  val inject_int : typ -> Integer.t -> t
  (** Abstract address for the given varinfo. (With type "pointer to the type
      of the variable" if the abstract values are typed.) *)

  (** {3 Alarms } *)

  (** These functions are used to create the alarms that report undesirable
      behaviors, when a value abstraction does not meet the prerequisites of an
      operation. Thereafter, the value is assumed to meet them to continue the
      analysis.
      See the documentation of the [truth] type for more details. *)

  (* Assumes that the integer value represents only non zero values. *)
  val assume_non_zero: t -> t truth

  (* [assume_bounded Lower_bound b v] assumes that the value [v] represents
     only values greater or equal to the lower bound [b].
     [assume_bounded Upper_bound b v] assumes that the value [v] represents
     only values lower or equal to the greater bound [b].
     Depending on the bound, [v] is an integer or a floating-point value. *)
  val assume_bounded: bound_kind -> bound -> t -> t truth

  (* Assumes that the floating-point value does not represent NaN.
     If [assume_finite] is true, assumes that the value represents only finite
     floating-point values. *)
  val assume_not_nan: assume_finite:bool -> fkind -> t -> t truth

  (* [assume_comparable cmp v1 v2] assumes that the integer or pointer values
     [v1] and [v2] are comparable for [cmp]. Integers are always comparable.
     If one value is a pointer, then both values should be pointers, and:
     and according to [cmp]:
     - if [cmp] is Equality:
       either one pointer is NULL, or both pointers are valid (pointing into
       their object), or both pointers are nearly valid (pointing into or just
       beyond their object) and point to the same object.
     - if [cmp] is Relation:
       both pointers should point into or just beyond the same object;
     - if [cmp] is Subtraction:
       both pointers should point to the same object, without any restriction on
       their offsets. *)
  val assume_comparable: pointer_comparison -> t -> t -> (t * t) truth

  (** {3 Forward Operations } *)

  (** Embeds C constants into value abstractions: returns an abstract value
      for the given constant. The constant cannot be an enumeration constant. *)
  val constant : exp -> constant -> t

  (** [forward_unop typ unop v] evaluates the value [unop v], resulting from the
      application of the unary operator [unop] to the value [v].  [typ] is the
      type of [v]. *)
  val forward_unop : typ -> unop -> t -> t or_bottom

  (** [forward_binop typ binop v1 v2] evaluates the value [v1 binop v2],
      resulting from the application of the binary operator [binop] to the
      values [v1] and [v2]. [typ] is the type of [v1]. *)
  val forward_binop : typ -> binop -> t -> t -> t or_bottom

  (** [rewrap_integer irange t] wraps around the abstract value [t] to fit the
      integer range [irange], assuming 2's complement. *)
  val rewrap_integer: Eval_typ.integer_range -> t -> t

  (** Abstract evaluation of casts operators from [src_type] to [dst_type]. *)
  val forward_cast :
    src_type: Eval_typ.scalar_typ -> dst_type: Eval_typ.scalar_typ ->
    t -> t or_bottom

  (** {3 Backward Operations } *)

  (** For an unary forward operation F, the inverse backward operator B tries to
      reduce the argument values of the operation, given its result.

      It must satisfy:
        if [B arg res] = v
        then ∀ a ⊆ arg such that [F a] ⊆ res, a ⊆ v

      i.e. [B arg res] returns a value [v] larger than all subvalues of [arg]
      whose result through F is included in [res].

      If [F arg] ∈ [res] is impossible, then [v] should be bottom.
      If the value [arg] cannot be reduced, then [v] should be None.

      Any n-ary operator may be considered as a unary operator on a vector
      of values, the inclusion being lifted pointwise.
  *)

  (** Backward evaluation of the binary operation [left binop right = result];
      tries to reduce the argument [left] and [right] according to [result].
      [input_type] is the type of [left], [resulting_type] the type of [result]. *)
  val backward_binop : input_type:typ -> resulting_type:typ ->
    binop -> left:t -> right:t -> result:t -> (t option * t option) or_bottom

  (** Backward evaluation of the unary operation [unop arg = res];
      tries to reduce the argument [arg] according to [res].
      [typ_arg] is the type of [arg]. *)
  val backward_unop :
    typ_arg:typ -> unop -> arg:t -> res:t -> t option or_bottom

  (** Backward evaluation of the cast of the value [src_val] of type [src_typ]
      into the value [dst_val] of type [dst_typ]. Tries to reduce [scr_val]
      according to [dst_val]. *)
  val backward_cast:
    src_typ: typ ->
    dst_typ: typ ->
    src_val: t ->
    dst_val: t ->
    t option or_bottom

  val resolve_functions : t -> Kernel_function.t list or_top * bool
  (** [resolve_functions v] returns the list of functions that may be pointed to
      by the abstract value [v] (representing a function pointer). The returned
      boolean must be [true] if some of the values represented by [v] do not
      correspond to functions. It is always safe to return [`Top, true]. *)

end

type 'v key = 'v Structure.Key_Value.key

(** Signature for a leaf module of abstract values. *)
module type Leaf = sig
  include S

  (** The key identifies the module and the type [t] of abstract values. *)
  val key: t key
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
