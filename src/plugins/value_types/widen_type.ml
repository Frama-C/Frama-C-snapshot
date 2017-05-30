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

open Cil_datatype

module Num_hints_stmt = Stmt.Map.Make(Ival.Widen_Hints)
module Num_hints_bases = Base.Map.Make(Ival.Widen_Hints)
module Num_hints_bases_stmt = Stmt.Map.Make(Num_hints_bases)
module Priority_bases_stmt = Stmt.Map.Make(Base.Set)

type widen_hints = {
  priority_bases: Base.Set.t Stmt.Map.t;
  default_hints: Ival.Widen_Hints.t;
  default_hints_by_stmt: Ival.Widen_Hints.t Stmt.Map.t;
  hints_by_addr: Ival.Widen_Hints.t Base.Map.t;
  hints_by_addr_by_stmt: Ival.Widen_Hints.t Base.Map.t Stmt.Map.t;
}

(* an [empty] set of hints *)
let empty = {
  priority_bases = Stmt.Map.empty;
  default_hints = Ival.Widen_Hints.empty;
  default_hints_by_stmt = Stmt.Map.empty;
  hints_by_addr = Base.Map.empty;
  hints_by_addr_by_stmt = Stmt.Map.empty;
}

include Datatype.Make(struct
  include Datatype.Serializable_undefined
  type t = widen_hints
  let name = "Widen_type.widen_hints"
  let structural_descr =
    Structural_descr.t_tuple
      [| Priority_bases_stmt.packed_descr;
         Ival.Widen_Hints.packed_descr;
         Num_hints_stmt.packed_descr;
         Num_hints_bases.packed_descr;
         Num_hints_bases_stmt.packed_descr |]
  let reprs =
    List.map
      (fun wh ->
        { priority_bases = Stmt.Map.empty;
          default_hints = wh;
          default_hints_by_stmt = Stmt.Map.empty;
          hints_by_addr = Base.Map.empty;
          hints_by_addr_by_stmt = Stmt.Map.empty})
      Ival.Widen_Hints.reprs
  let mem_project = Datatype.never_any_project
  end)

let join wh1 wh2 =
  let map_merge s_join os1 os2 =
    match os1, os2 with
    | Some bs1, Some bs2 -> Some (s_join bs1 bs2)
    | Some bs, None | None, Some bs -> Some bs
    | None, None -> None
  in
  { priority_bases =
      Stmt.Map.merge (fun _key -> map_merge Base.Set.union)
        wh1.priority_bases wh2.priority_bases;
    default_hints =
      Ival.Widen_Hints.union wh1.default_hints wh2.default_hints;
    default_hints_by_stmt =
      Stmt.Map.merge (fun _key -> map_merge Ival.Widen_Hints.union)
        wh1.default_hints_by_stmt wh2.default_hints_by_stmt;
    hints_by_addr =
      Base.Map.merge (fun _key -> map_merge Ival.Widen_Hints.union)
        wh1.hints_by_addr wh2.hints_by_addr;
    hints_by_addr_by_stmt =
      Stmt.Map.merge (fun _key ->
          map_merge (Base.Map.merge
                       (fun _key -> map_merge Ival.Widen_Hints.union)))
        wh1.hints_by_addr_by_stmt wh2.hints_by_addr_by_stmt;
  }

let pretty fmt wh =
  let pp_bindings pp_key pp_elt fmt l =
    Format.fprintf fmt "%a"
      (Pretty_utils.pp_list ~sep:",@ "
         (Pretty_utils.pp_pair ~sep:" -> " pp_key pp_elt)) l
  in
  let pp_base_map pp_elt fmt m =
    Format.fprintf fmt "%a" (pp_bindings Base.pretty pp_elt) (Base.Map.bindings m)
  in
  let pp_stmt fmt stmt =
    let stmt_str = Pretty_utils.sfprintf "%a" Stmt.pretty stmt in
    let len = String.length stmt_str in
    Format.fprintf fmt "[sid:%d<%s>]" stmt.Cil_types.sid
      (if len < 10 then stmt_str else String.sub stmt_str 0 10 ^ "...")
  in
  Format.fprintf fmt
    "@[priority bases: %a@\n\
     default_hints: %a@\n\
     default_hints_by_stmt: %a@\n\
     hints_by_addr: %a@\n\
     hints_by_addr_by_stmt: %a@]"
    (pp_bindings pp_stmt Base.Set.pretty) (Stmt.Map.bindings wh.priority_bases)
    Ival.Widen_Hints.pretty wh.default_hints
    (Pretty_utils.pp_list ~sep:",@ "
       (Pretty_utils.pp_pair ~sep:" -> " pp_stmt Ival.Widen_Hints.pretty))
    (Stmt.Map.bindings wh.default_hints_by_stmt)
    (Pretty_utils.pp_list ~sep:",@ "
       (Pretty_utils.pp_pair ~sep:" -> " Base.pretty Ival.Widen_Hints.pretty))
    (Base.Map.bindings wh.hints_by_addr)
    (pp_bindings pp_stmt (pp_base_map Ival.Widen_Hints.pretty))
    (Stmt.Map.bindings wh.hints_by_addr_by_stmt)

let hints_for_base default_hints hints_by_base b =
  let widen_hints_null =
    try Ival.Widen_Hints.union (Base.Map.find b hints_by_base) default_hints
    with Not_found -> default_hints
  in
  let widen_zero = Ival.Widen_Hints.singleton Integer.zero in
  (function
    | Base.Null -> widen_hints_null
    | b ->
      let validity = Base.validity b in
      match validity with
        | Base.Known (_, m)
        | Base.Unknown (_, _, m)
        | Base.Variable { Base.max_alloc = m } ->
          (* Try the frontier of the block: further accesses are invalid
             anyway. This also works great for constant strings (this computes
             the offset of the null terminator). *)
          let bound = Integer.(pred (div (succ m) eight)) in
          Ival.Widen_Hints.add bound widen_zero
        | Base.Empty | Base.Invalid -> widen_zero
  )

let hints_from_keys stmt h =
  let hints_by_base =
    try
      let at_stmt = Stmt.Map.find stmt h.hints_by_addr_by_stmt in
      Base.Map.merge (fun _b os1 os2 ->
          match os1, os2 with
          | Some s1, Some s2 -> Some (Ival.Widen_Hints.union s1 s2)
          | Some s, None | None, Some s -> Some s
          | None, None -> None
        ) at_stmt h.hints_by_addr
    with Not_found -> h.hints_by_addr
  in
  let prio =
    try Stmt.Map.find stmt h.priority_bases
    with Not_found -> Base.Set.empty
  in
  let default =
    try
      let at_stmt = Stmt.Map.find stmt h.default_hints_by_stmt in
      Ival.Widen_Hints.union h.default_hints at_stmt
    with Not_found -> h.default_hints
  in
  prio, (fun b -> hints_for_base default hints_by_base b)

let var_hints stmt prio_bases =
  { empty with priority_bases = Stmt.Map.singleton stmt prio_bases }

let num_hints stmto baseo hints =
  match stmto, baseo with
  | None, Some b -> (* Hints for a base at all statements *)
    { empty with hints_by_addr = Base.Map.singleton b hints }
  | Some stmt, Some b -> (* Hints for a base at a statement *)
    { empty with hints_by_addr_by_stmt = Stmt.Map.singleton stmt
                     (Base.Map.singleton b hints) }
  | Some stmt, None -> (* Hints for all bases and a given statement *)
    { empty with default_hints_by_stmt = Stmt.Map.singleton stmt hints }
  | None, None -> (* Hints for all bases and all statements *)
    { empty with default_hints = hints }

(* default set of hints. Depends on the machdep *)
let default () =
  let default = Ival.Widen_Hints.default_widen_hints in
  num_hints None None default

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
