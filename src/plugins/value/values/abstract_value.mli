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

(** Abstract numeric values of the analysis. *)

open Cil_types
open Eval

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
  val float_zeros: t
  val top_int : t
  val inject_int : typ -> Integer.t -> t
  val inject_address: varinfo -> t
  (** Abstract address for the given varinfo. (With type "pointer to the type
      of the variable" if the abstract values are typed.) *)

  (** {3 Forward Operations } *)

  (** Returns the abstract value of a literal constant, and potentially some
      alarms in case of floating point constants overflow. *)
  val constant : exp -> constant -> t evaluated

  (** [forward_unop context typ unop v] evaluates the value [unop v], and the
      alarms resulting from the operations. See {eval.mli} for details on the
      types. [typ] is the type of [v], and [context] contains the expressions
      involved in the operation, needed to create the alarms. *)
  val forward_unop : context:unop_context -> typ -> unop -> t -> t evaluated

  (** [forward_binop context typ binop v1 v2] evaluates the value [v1 binop v2],
      and the alarms resulting from the operations. See {eval.mli} for details
      on the types. [typ] is the type of [v1], and [context] contains the
      expressions involved in the operation, needed to create the alarms. *)
  val forward_binop : context:binop_context -> typ -> binop -> t -> t -> t evaluated

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

  (** {3 Reinterpret and Cast } *)

  (** [truncate_integer expr irange t] truncates the abstract value [t] to the
      integer range [irange]. Produces overflow alarms if [t] does not already
      fit into [irange], attached to the expression [expr]. *)
  val truncate_integer: exp -> Eval_typ.integer_range -> t -> t evaluated

  (** [rewrap_integer irange t] wraps around the abstract value [t] to fit the
      integer range [irange]. Does not produce any alarms. *)
  val rewrap_integer: Eval_typ.integer_range -> t -> t

  (** [cast_float expr fkind t] recasts the abstract value [t] resulting from a
      floating-point operation to the precision type [fkind]. Produces
      is_nan_or_infinite alarms (attached to the expression [expr]) if
      necessary. *)
  val cast_float: exp -> fkind -> t -> t evaluated

  (** Abstract evaluation of casts operators from [scr_typ] to [dst_typ]. *)
  val do_promotion : src_typ:typ -> dst_typ: typ -> exp -> t -> t evaluated

  val resolve_functions :
    typ_pointer:typ -> t -> Kernel_function.Hptset.t Eval.or_top * bool
    (** [resolve_functions ~typ_pointer v] finds within [v] all the functions
        with a type compatible with [typ_pointer]. The returned boolean
        indicates the possibility of an alarm, i.e. that some of the values
        represented by [v] do not correspond to functions, or to functions
        with an incompatible type. It is always safe to return [`Top, true].

        This function is used to resolve pointers calls. For consistency
        between analyses, the function {!Eval_typ.compatible_functions}
        should be used to determine whether the functions [v] may point to
        are compatible with [typ_pointer]. *)

end

(** Key and structure for values. See {structure.mli},
    and {domain.mli} where the mechanism is explained in detail.*)

type 'a key = 'a Structure.Key_Value.k
type 'a structure = 'a Structure.Key_Value.structure

module type Internal = sig
  include S
  val structure : t structure
end

module type External = sig
  include S
  include Structure.External with type t := t
                              and type 'a key := 'a key
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
