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

open Cil_types

module Exp = Cil_datatype.ExpStructEq
module Lval = Cil_datatype.LvalStructEq

(* lvalues are never stored under a constructor [E]. *)
type unhashconsed_exprs = E of Exp.t | LV of Lval.t

(* The replacement of an lvalue by another term in an expression fails
   (raises an exception) if the height of the resulting expression exceeds
   this limit. *)
let height_limit = 8

exception NonExchangeable

type kill_type = Modified | Deleted

module E = struct
  include Datatype.Make (struct
      include Datatype.Serializable_undefined

      type t = unhashconsed_exprs
      let name = "Value.Symbolic_exprs.key"
      let reprs = [ E Cil_datatype.Exp.dummy ]

      let structural_descr =
        Structural_descr.t_sum
          [| [| Exp.packed_descr |] ; [| Lval.packed_descr |] ; |]

      let equal a b = match a, b with
        | E e1, E e2 -> Exp.equal e1 e2
        | LV lv1, LV lv2 -> Lval.equal lv1 lv2
        | (E _ | LV _), _ -> false

      let compare a b = match a, b with
        | E e1, E e2 -> Exp.compare e1 e2
        | LV lv1, LV lv2 -> Lval.compare lv1 lv2
        | LV _, E _  -> -1
        | E _, LV _  -> 1

      let pretty fmt = function
        | E e ->   Format.fprintf fmt "%a" Exp.pretty e
        | LV lv -> Format.fprintf fmt "%a" Lval.pretty lv

      let hash = function
        | E e -> Exp.hash e
        | LV lv -> Lval.hash lv

      let copy c = c
    end)

  let replace_visitor kind ~late ~heir = object
    inherit Visitor.frama_c_copy (Project.current ())

    method! vexpr expr =
      match expr.enode with
      | Lval lval ->
        if Lval.equal lval late then Cil.ChangeTo heir else Cil.JustCopy
      | StartOf lval | AddrOf lval ->
        if kind = Modified
        then Cil.JustCopy
        else if Lval.equal lval late then raise NonExchangeable else Cil.JustCopy
      | AlignOfE _ -> raise NonExchangeable
      | _ -> Cil.DoChildren
  end

  let replace kind ~late ~heir expr =
    let visitor = replace_visitor kind ~late ~heir in
    Visitor.visitFramacExpr visitor expr
end

module HCE = struct
  module S =
    State_builder.Hashcons(E)
      (struct let dependencies = [Ast.self] let name = "" end)

  include S

  let pretty_debug = pretty

  let of_lval lv = hashcons (LV lv)

  let of_exp exp =
    match exp.enode with
    | Lval lv -> of_lval lv
    | _ -> hashcons (E exp)

  let to_exp h = match get h with
    | E e -> e
    | LV lv -> Value_util.lval_to_exp lv

  let to_lval h = match get h with
    | E _ -> None
    | LV lv -> Some lv

  let is_lval h = match get h with
    | E _ -> false
    | LV _ -> true

  let replace kind ~late ~heir h = match get h with
    | E e ->
      let e = E.replace kind ~late ~heir e in
      if Value_util.height_expr e > height_limit
      then raise NonExchangeable
      else of_exp e
    | LV lval -> if Lval.equal lval late then of_exp heir else h
end

module HCESet =
  Hptset.Make (HCE) (struct let v = [] end) (struct let l = [Ast.self] end)

type lvalues = {
  read : HCESet.t;
  addr : HCESet.t;
}

let empty_lvalues = { read = HCESet.empty; addr = HCESet.empty; }

let syntactic_lvalues expr =
  let rec gather expr lvalues =
    match expr.enode with
    | Lval lv ->
      { lvalues with read = HCESet.add (HCE.of_lval lv) lvalues.read }
    | AddrOf lv | StartOf lv ->
      { lvalues with addr = HCESet.add (HCE.of_lval lv) lvalues.addr }
    | AlignOfE e | SizeOfE e ->
      (* The lvalues appearing in [e] are not read, and must all be in addr. *)
      let new_lvalues = gather e empty_lvalues in
      let new_addr = HCESet.union new_lvalues.read new_lvalues.addr in
      { lvalues with addr = HCESet.union new_addr lvalues.addr }
    | UnOp (_, e, _) | CastE (_, e) | Info (e, _) -> gather e lvalues
    | BinOp (_, e1, e2, _) -> gather e1 (gather e2 lvalues)
    | _ -> lvalues
  in
  gather expr empty_lvalues


module HCEToZone = struct

  let cache_prefix = "Value.Symbolic_exprs.K2Z"

  include Hptmap.Make(HCE)(Locations.Zone)(Hptmap.Comp_unused)
      (struct let v = [] end)(struct let l = [Ast.self] end)

  let is_included =
    let cache_name = cache_prefix ^ ".is_included" in
    let decide_fst _b _v1 = true in
    let decide_snd _b _v2 = false in
    let decide_both _ v1 v2 = Locations.Zone.is_included v1 v2 in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate
      (Hptmap_sig.PersistentCache cache_name) UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let inter =
    let cache_name = cache_prefix ^ ".inter" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Some (Locations.Zone.join v1 v2) in
    inter ~cache ~symmetric ~idempotent ~decide

  let union =
    let cache_name = cache_prefix ^ ".union" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Locations.Zone.join v1 v2 in
    join ~cache ~symmetric ~idempotent ~decide

  let find_default k m =
    try find k m
    with Not_found -> Locations.Zone.bottom

end


module BaseToHCESet = struct

  include Hptmap.Make (Base) (HCESet) (Hptmap.Comp_unused)
      (struct let v = [] end)(struct let l = [Ast.self] end)

  let cache_prefix = "Value.Symbolic_exprs.B2K"

  let inter =
    let cache_name = cache_prefix ^ ".inter" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 =
      let s = HCESet.inter v1 v2 in
      if HCESet.is_empty s then None else Some s
    in
    inter ~cache ~symmetric ~idempotent ~decide

  let union =
    let cache_name = cache_prefix ^ ".union" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = HCESet.union v1 v2 in
    join ~cache ~symmetric ~idempotent ~decide

  let find_default b m =
    try find b m
    with Not_found -> HCESet.empty

end
