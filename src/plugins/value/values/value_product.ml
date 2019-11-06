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

open Eval

module Make
    (Left: Abstract_value.S)
    (Right: Abstract_value.S)
= struct

  include Datatype.Pair (Left) (Right)

  let pretty_typ typ =
    Pretty_utils.pp_pair ~pre:"@[" ~sep:",@ " ~suf:"@]"
      (Left.pretty_typ typ) (Right.pretty_typ typ)

  let top = Left.top, Right.top
  let is_included (l1, r1) (l2, r2) =
    Left.is_included l1 l2 && Right.is_included r1 r2
  let join (l1, r1) (l2, r2) =
    Left.join l1 l2, Right.join r1 r2
  let narrow (l1, r1) (l2, r2) =
    Left.narrow l1 l2 >>- fun left ->
    Right.narrow r1 r2 >>-: fun right ->
    left, right

  let zero = Left.zero, Right.zero
  let one = Left.one, Right.one
  let top_int = Left.top_int, Right.top_int
  let inject_int typ i = Left.inject_int typ i, Right.inject_int typ i

  (* Intersects the truth values [t1] and [t2] coming from [assume_] functions
     from both abstract values. [v1] and [v2] are the initial values leading to
     these truth values, that may be reduced by the assumption. [combine]
     combines values from both abstract values into values of the product. *)
  let narrow_any_truth combine (v1, t1) (v2, t2) = match t1, t2 with
    | `Unreachable, _ | _, `Unreachable
    | (`True | `TrueReduced _), `False
    | `False, (`True | `TrueReduced _) -> `Unreachable
    | `False, _ | _, `False -> `False
    | `Unknown v1, `Unknown v2 -> `Unknown (combine v1 v2)
    | (`Unknown v1 | `TrueReduced v1), `True -> `TrueReduced (combine v1 v2)
    | `True, (`Unknown v2 | `TrueReduced v2) -> `TrueReduced (combine v1 v2)
    | (`Unknown v1 | `TrueReduced v1),
      (`Unknown v2 | `TrueReduced v2) -> `TrueReduced (combine v1 v2)
    | `True, `True -> `True

  let narrow_truth = narrow_any_truth (fun left right -> left, right)

  let assume_non_zero (left, right) =
    let left_truth = Left.assume_non_zero left
    and right_truth = Right.assume_non_zero right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_bounded kind bound (left, right) =
    let left_truth = Left.assume_bounded kind bound left
    and right_truth = Right.assume_bounded kind bound right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_not_nan ~assume_finite fkind (left, right) =
    let left_truth = Left.assume_not_nan ~assume_finite fkind left
    and right_truth = Right.assume_not_nan ~assume_finite fkind right in
    narrow_truth (left, left_truth) (right, right_truth)

  let assume_comparable op (l1, r1) (l2, r2) =
    let left_truth = Left.assume_comparable op l1 l2
    and right_truth = Right.assume_comparable op r1 r2 in
    let combine (l1, l2) (r1, r2) = (l1, r1), (l2, r2) in
    narrow_any_truth combine ((l1, l2), left_truth) ((r1, r2), right_truth)

  let constant expr constant =
    let left = Left.constant expr constant
    and right = Right.constant expr constant in
    left, right

  let forward_unop typ unop (left, right) =
    Left.forward_unop typ unop left >>- fun left ->
    Right.forward_unop typ unop right >>-: fun right ->
    left, right

  let forward_binop typ binop (l1, r1) (l2, r2) =
    Left.forward_binop typ binop l1 l2 >>- fun left ->
    Right.forward_binop typ binop r1 r2 >>-: fun right ->
    left, right

  let rewrap_integer range (left, right) =
    Left.rewrap_integer range left, Right.rewrap_integer range right

  let forward_cast ~src_type ~dst_type (left, right) =
    Left.forward_cast ~src_type ~dst_type left >>- fun left ->
    Right.forward_cast ~src_type ~dst_type right >>-: fun right ->
    left, right

  let resolve_functions (left, right) =
    let list1, b1 = Left.resolve_functions left
    and list2, b2 = Right.resolve_functions right in
    let list = match list1, list2 with
      | `Top, _ -> list2
      | _, `Top -> list1
      | `Value s1, `Value s2 -> `Value (List.filter (fun f -> List.mem f s1) s2)
    in
    list, b1 && b2

  let reduce (orig_left, orig_right) left right = match left, right with
    | None, None            -> None
    | Some left, None       -> Some (left, orig_right)
    | None, Some right      -> Some (orig_left, right)
    | Some left, Some right -> Some (left, right)

  let backward_unop ~typ_arg unop ~arg:(arg_l, arg_r as arg) ~res:(res_l, res_r) =
    Left.backward_unop ~typ_arg unop ~arg:arg_l ~res:res_l >>- fun left ->
    Right.backward_unop ~typ_arg unop ~arg:arg_r ~res:res_r >>-: fun right ->
    reduce arg left right

  let backward_binop ~input_type ~resulting_type binop ~left ~right ~result =
    let l1, r1 = left and l2, r2 = right and l3, r3 = result in
    Left.backward_binop ~input_type ~resulting_type
      binop ~left:l1 ~right:l2 ~result:l3
    >>- fun (l1, l2) ->
    Right.backward_binop ~input_type ~resulting_type
      binop ~left:r1 ~right:r2 ~result:r3
    >>-: fun (r1, r2) ->
    reduce left l1 r1, reduce right l2 r2

  let backward_cast ~src_typ ~dst_typ ~src_val ~dst_val =
    let l1, r1 = src_val and l2, r2 = dst_val in
    Left.backward_cast ~src_typ ~dst_typ ~src_val:l1 ~dst_val:l2 >>- fun left ->
    Right.backward_cast ~src_typ ~dst_typ ~src_val:r1 ~dst_val:r2 >>-: fun right ->
    reduce src_val left right

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
