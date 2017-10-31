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

open Eval

(* Here we need to intersect the alarms issued for each abstract value. *)
let (>>=) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t ->
    let t', a' = f t in
    match Alarmset.inter a a' with
    | `Inconsistent ->
      Value_parameters.abort ~current:true ~once:true
        "Inconsistent status of alarms between value abstractions: unsound states."
    | `Value alarms -> t', alarms


module Make
    (Left: Abstract_value.Internal)
    (Right: Abstract_value.Internal)
= struct

  include Datatype.Pair (Left) (Right)

  let structure = Structure.Key_Value.Node (Left.structure, Right.structure)

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
  let float_zeros = Left.float_zeros, Right.float_zeros
  let top_int = Left.top_int, Right.top_int
  let inject_int typ i = Left.inject_int typ i, Right.inject_int typ i
  let inject_address vi = Left.inject_address vi, Right.inject_address vi

  let constant expr constant =
    Left.constant expr constant >>= fun left ->
    Right.constant expr constant >>=: fun right ->
    left, right

  let forward_unop ~context typ unop (left, right) =
    Left.forward_unop ~context typ unop left >>= fun left ->
    Right.forward_unop ~context typ unop right >>=: fun right ->
    left, right

  let forward_binop ~context typ binop (l1, r1) (l2, r2) =
    Left.forward_binop ~context typ binop l1 l2 >>= fun left ->
    Right.forward_binop ~context typ binop r1 r2 >>=: fun right ->
    left, right

  let truncate_integer expr range (left, right) =
    Left.truncate_integer expr range left >>= fun left ->
    Right.truncate_integer expr range right >>=: fun right ->
    left, right

  let rewrap_integer range (left, right) =
    Left.rewrap_integer range left, Right.rewrap_integer range right

  let cast_float expr fkind (left, right) =
    Left.cast_float expr fkind left >>= fun left ->
    Right.cast_float expr fkind right >>=: fun right ->
    left, right

  let do_promotion ~src_typ ~dst_typ expr (left, right) =
    Left.do_promotion ~src_typ ~dst_typ expr left >>= fun left ->
    Right.do_promotion ~src_typ ~dst_typ expr right >>=: fun right ->
    left, right

  let resolve_functions ~typ_pointer (left, right) =
    let set1, b1 = Left.resolve_functions ~typ_pointer left
    and set2, b2 = Right.resolve_functions ~typ_pointer right in
    let set = match set1, set2 with
      | `Top, _              -> set2
      | _, `Top              -> set1
      | `Value s1, `Value s2 -> `Value (Kernel_function.Hptset.inter s1 s2)
    in
    set, b1 && b2

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
