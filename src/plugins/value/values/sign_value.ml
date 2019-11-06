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

open Cil_types
open Eval
open Abstract_interp

(** Sign domain: abstraction of integer numerical values by their signs. *)

type signs = {
  pos: bool;  (** true: maybe positive, false: never positive *)
  zero: bool; (** true: maybe zero, false: never zero *)
  neg: bool;  (** true: maybe negative, false: never negative *)
}

let top =         { pos = true;  zero = true;  neg = true  }
let pos_or_zero = { pos = true;  zero = true;  neg = false }
let pos =         { pos = true;  zero = false; neg = false }
let neg_or_zero = { pos = false; zero = true;  neg = true  }
let neg =         { pos = false; zero = false; neg = true  }
let zero =        { pos = false; zero = true;  neg = false }
let one =         { pos = true; zero = false;  neg = false }
let non_zero =    { pos = true;  zero = false; neg = true  }
let ge_zero v = not v.neg
let le_zero v = not v.pos

(* Bottom is a special value (`Bottom) in Eva, and need not be part of
   the lattice. Here, we have a value which is equivalent to it, defined
   there only for commodity. *)
let empty = { pos = false; zero = false; neg = false }

(* Datatypes are Frama-C specific modules used among other things for
   serialization. There is no need to understand them in detail.
   They are created mostly via copy/paste of templates. *)
include Datatype.Make(struct
    type t = signs
    include Datatype.Serializable_undefined
    let compare = Transitioning.Stdlib.compare
    let equal = Datatype.from_compare
    let hash = Hashtbl.hash
    let reprs = [top]
    let name = "Value.Sign_values.signs"
    let pretty fmt v =
      Format.fprintf fmt "%s%s%s"
        (if v.neg  then "-" else "")
        (if v.zero then "0" else "")
        (if v.pos  then "+" else "")
  end)
let pretty_debug = pretty
let pretty_typ _ = pretty

(* Inclusion: test inclusion of each field. *)
let is_included v1 v2 =
  let bincl b1 b2 = (not b1) || b2 in
  bincl v1.pos v2.pos && bincl v1.zero v2.zero && bincl v1.neg v2.neg

(* Join of the lattice: pointwise logical or. *)
let join v1 v2 = {
  pos  = v1.pos  || v2.pos;
  zero = v1.zero || v2.zero;
  neg  = v1.neg  || v2.neg;
}

(* Meet of the lattice (called 'narrow' in Eva for historical reasons).
   We detect the case where the values have incompatible concretization,
   and report this as `Bottom. *)
let narrow v1 v2 =
  let r = {
    pos  = v1.pos  && v2.pos;
    zero = v1.zero && v2.zero;
    neg  = v1.neg  && v2.neg;
  } in
  if r = empty then `Bottom else `Value r

let top_int = top

(* [inject_int] creates an abstract value corresponding to the singleton [i]. *)
let inject_int _ i =
  if Integer.lt i Integer.zero then neg
  else if Integer.gt i Integer.zero then pos
  else zero

let constant _ = function
  | CInt64 (i, _, _) -> inject_int () i
  | _ -> top

(* Extracting function pointers from an abstraction. Not implemented
   precisely *)
let resolve_functions _ = `Top, true

(** {2 Alarms} *)

let assume_non_zero v =
  if equal v zero
  then `False
  else if v.zero
  then `Unknown {v with zero = false}
  else `True

(* TODO: use the bound to reduce the value when possible. *)
let assume_bounded _ _ v = `Unknown v

let assume_not_nan ~assume_finite:_ _ v = `Unknown v
let assume_comparable _ v1 v2 = `Unknown (v1, v2)

(** {2 Forward transfer functions} *)

(* The three functions below are forward transformers for the mathematical
   operations +, *, /, and the unary negation -. The potential overflows for the
   operations on machine integers are taken into account by the functions
   [truncate_integer] and [rewrap_integer]. *)

let neg_unop v = { v with neg = v.pos; pos = v.neg }

let forward_unop _ op v =
  match op with
  | Neg -> `Value (neg_unop v)
  | _ -> `Value top

let plus v1 v2 =
  let neg = v1.neg || v2.neg in
  let pos = v1.pos || v2.pos in
  let same_sign v1 v2 =
    (le_zero v1 && le_zero v2) || (ge_zero v1 && ge_zero v2)
  in
  let zero = not (same_sign v1 v2) || (v1.zero && v2.zero) in
  { neg; pos; zero }

let mul v1 v2 =
  let pos = (v1.pos && v2.pos) || (v1.neg && v2.neg) in
  let neg = (v1.pos && v2.neg) || (v1.neg && v2.pos) in
  let zero = v1.zero || v2.zero in
  { neg; pos; zero }

let div v1 v2 =
  let pos = (v1.pos && v2.pos) || (v1.neg && v2.neg) in
  let neg = (v1.pos && v2.neg) || (v1.neg && v2.pos) in
  let zero = true in (* zero can appear with large enough v2 *)
  { neg; pos; zero }

let forward_binop _ op v1 v2 =
  match op with
  | PlusA  -> `Value (plus v1 v2)
  | MinusA -> `Value (plus v1 (neg_unop v2))
  | Mult   -> `Value (mul v1 v2)
  | Div    -> if equal zero v2 then `Bottom else `Value (div v1 v2)
  | _      -> `Value top

let rewrap_integer range v =
  if equal v zero then v
  else if range.Eval_typ.i_signed then top else pos_or_zero

(* Casts from type [src_typ] to type [dst_typ]. As downcasting can wrap,
   we only handle upcasts precisely *)
let forward_cast ~src_type ~dst_type v =
  let open Eval_typ in
  match src_type, dst_type with
  | TSInt range_src, TSInt range_dst ->
    if equal v zero then `Value v else
    if range_inclusion range_src range_dst then
      `Value v (* upcast *)
    else if range_dst.i_signed then
      `Value top (*dst_typ is signed, return all possible values*)
    else
      `Value pos_or_zero (* dst_typ is unsigned *)
  | _ ->
    (* at least one non-integer type. not handled precisely. *)
    `Value top


(** {2 Backward transfer functions} *)

(* Backward transfer functions are used to reduce the abstraction of a value,
   knowing other information. For example '[0+] > [0]' means that the
   first value can only be [+].

   In the OCaml signatures, 'None' means 'I cannot reduce'. *)

(* Value to return when no reduction is possible *)
let unreduced = `Value None
(* Function to use when a reduction is possible *)
let reduced v = `Value (Some v)

(* This function must reduce the value [right] assuming that the
   comparison [left op right] holds. *)
let backward_comp_right op ~left ~right =
  let open Abstract_interp.Comp in
  match op with
  | Eq ->
    narrow left right >>- reduced
  | Ne ->
    if equal left zero then
      narrow right non_zero >>- reduced
    else unreduced
  | Le ->
    if ge_zero left then
      (* [left] is positive or zero. Hence, [right] is at least also positive
         or zero. *)
      if left.zero then
        (* [left] may be zero, [right] is positive or zero *)
        narrow right pos_or_zero >>- reduced
      else
        (* [left] is strictly positive, hence so is [right] *)
        narrow right pos >>- reduced
    else unreduced
  | Lt ->
    if ge_zero left then
      narrow right pos >>- reduced
    else unreduced
  | Ge ->
    if le_zero left then
      if left.zero then
        narrow right neg_or_zero >>- reduced
      else
        narrow right neg >>- reduced
    else unreduced
  | Gt ->
    if le_zero left then
      narrow right neg >>- reduced
    else unreduced

(* This functions must reduce the values [left] and [right], assuming
   that [left op right == result] holds. Currently, it is only implemented
   for comparison operators. *)
let backward_binop ~input_type:_ ~resulting_type:_ op ~left ~right ~result =
  match op with
  | Ne | Eq | Le | Lt | Ge | Gt ->
    let op = Value_util.conv_comp op in
    if equal zero result then
      (* The comparison is false, as it always evaluate to false. Reduce by the
         fact that the inverse comparison is true.  *)
      let op = Comp.inv op in
      backward_comp_right op ~left ~right >>- fun right' ->
      backward_comp_right (Comp.sym op) ~left:right ~right:left >>- fun left' ->
      `Value (left', right')
    else if not result.zero then
      (* The comparison always hold, as it never evaluates to false. *)
      backward_comp_right op ~left ~right >>- fun right' ->
      backward_comp_right (Comp.sym op) ~left:right ~right:left >>- fun left' ->
      `Value (left', right')
    else
      (* The comparison may or may not hold, it is not possible to reduce *)
      `Value (None, None)
  | _ -> `Value (None, None)

(* Not implemented precisely *)
let backward_unop ~typ_arg:_ _op ~arg:_ ~res:_ = `Value None
(* Not implemented precisely *)
let backward_cast ~src_typ:_ ~dst_typ:_ ~src_val:_ ~dst_val:_ = `Value None


(** {2 Misc} *)

(* Eva boilerplate, used to retrieve the domain. *)
let key = Structure.Key_Value.create_key "sign_values"
