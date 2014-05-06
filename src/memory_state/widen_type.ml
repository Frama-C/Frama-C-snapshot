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
        | Base.Periodic (_, m, _) ->
          (* Try the frontier of the block: further accesses are invalid
             anyway. This also works great for constant strings (this computes
             the offset of the null terminator). *)
          let bound =
            Integer.pred (Integer.div (Integer.succ m) Integer.eight)
          in
          Ival.Widen_Hints.add bound widen_zero
        | Base.Invalid -> widen_zero
  )

let hints_from_keys stmt h =
  let hints_by_base =
    try Stmt.Map.find stmt h.hints_by_addr_by_stmt
    with Not_found -> h.hints_by_addr (* TODO: we should merge *)
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

let add_var_hints stmt prio_bases h =
  let new_hints =
    try
      let previous = Stmt.Map.find stmt h.priority_bases in
      Base.Set.union prio_bases previous
    with Not_found -> prio_bases
  in
  { h with priority_bases = Stmt.Map.add stmt new_hints h.priority_bases }

let add_num_hints stmto baseo hints h =
  let add_merge b hints hints_by_base =
    let new_hints =
      try
        let previous_hints = Base.Map.find b hints_by_base in
        Ival.Widen_Hints.union hints previous_hints        
      with Not_found -> hints
    in
    Base.Map.add b new_hints hints_by_base
  in
  match stmto, baseo with
    | None, Some b -> (* Hints for a base at all statements *)
      let new_hints =
        let previous_hints =
          try Base.Map.find b h.hints_by_addr
          with Not_found -> Ival.Widen_Hints.empty
        in
        Ival.Widen_Hints.union hints previous_hints
      in
      { h with hints_by_addr = add_merge b new_hints h.hints_by_addr }

    | Some stmt, Some b -> (* Hints for a base at a statement *)
      let hints_addr_for_stmt =
        let previous_var_map =
          try Stmt.Map.find stmt h.hints_by_addr_by_stmt
          with Not_found -> Base.Map.empty
        in
        add_merge b hints previous_var_map
      in
      let hints_addr_stmt =
        Stmt.Map.add stmt hints_addr_for_stmt h.hints_by_addr_by_stmt
      in
      { h with hints_by_addr_by_stmt = hints_addr_stmt }

    | Some stmt, None -> (* Hints for all bases and a given statement *)
      let new_default_for_stmt =
        try
          let previous = Stmt.Map.find stmt h.default_hints_by_stmt in
          Ival.Widen_Hints.union hints previous
        with Not_found -> hints
      in
      { h with default_hints_by_stmt =
          Stmt.Map.add stmt new_default_for_stmt h.default_hints_by_stmt }

    | None, None -> (* Hints for all bases and all statements *)
      { h with default_hints = Ival.Widen_Hints.union hints h.default_hints }

(* an [empty] set of hints *)
let empty = {
  priority_bases = Stmt.Map.empty;
  default_hints = Ival.Widen_Hints.empty;
  default_hints_by_stmt = Stmt.Map.empty;
  hints_by_addr = Base.Map.empty;
  hints_by_addr_by_stmt = Stmt.Map.empty;
}

(* default set of hints. Depends on the machdep *)
let default () =
  (* Add signed types frontiers, but only if signed overflow active. Otherwise,
     the computation will just overflow one iteration later. *)
  let int_types =
    if Kernel.SignedOverflow.get () then
      Ival.Widen_Hints.hints_for_signed_int_types ()
    else
      Ival.Widen_Hints.empty
  in
  let default = Ival.Widen_Hints.(union default_widen_hints int_types) in
  add_num_hints None None default empty

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
