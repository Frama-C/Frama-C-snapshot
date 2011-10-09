(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module Widen_hint_bases = Base.Map.Make(Ival.Widen_Hints)
module Widen_hint_stmts = Stmt.Map.Make(Widen_hint_bases)
module Bases_stmts = Stmt.Map.Make(Base.Set)

include Datatype.Pair
(Bases_stmts)
(Datatype.Make
   (struct
     include Datatype.Serializable_undefined
     type t =
         Ival.Widen_Hints.t
         * Ival.Widen_Hints.t
         * Widen_hint_bases.t
         * Widen_hint_stmts.t
     let name = "widen types"
     let structural_descr =
       Structural_descr.t_tuple
         [| Ival.Widen_Hints.packed_descr;
            Ival.Widen_Hints.packed_descr;
            Widen_hint_bases.packed_descr;
            Widen_hint_stmts.packed_descr |]
     let reprs =
       List.map
         (fun wh -> wh, wh, Base.Map.empty, Stmt.Map.empty)
         Ival.Widen_Hints.reprs
     let mem_project = Datatype.never_any_project
    end))

(* map from Base.t to Ival.Widen_Hints.t *)
type var_key = Default | All | VarKey of Cvalue.V.M.key

let hints_from_key (forced_hints, default_hints, var_map) var_key =
  let widen_hints =
    let hints =
      try Ival.Widen_Hints.union (Base.Map.find var_key var_map) default_hints
      with Not_found -> default_hints
    in
    Ival.Widen_Hints.union forced_hints hints
  in (* Format.printf "WIDEN_HInt widen a var_key %a -> %a @\n"
        Base.pretty var_key
        Ival.Widen_Hints.pretty widen_hints; *)
  Cvalue.V.Top_Param.O.empty, fun _ -> widen_hints

let hints_from_keys
    stmt_key
    (stmt_map1, (forced_hints, default_hints, var_map, stmt_map))
    =
  let var_map =
    try Stmt.Map.find stmt_key stmt_map
    with Not_found -> var_map
  in
  let var_set =
    try Stmt.Map.find stmt_key stmt_map1
    with Not_found -> Base.Set.empty
  in
  var_set, hints_from_key (forced_hints, default_hints, var_map)

let add_var_hints stmt var_hints (stmt_map1, map2) =
  let new_hints =
    let previous_hints =
      try Stmt.Map.find stmt stmt_map1
      with Not_found -> Base.Set.empty
    in
    Base.Set.union var_hints previous_hints
  in
  Stmt.Map.add stmt new_hints stmt_map1, map2

let add_num_hints
    stmt_key
    var_key
    hints
    (stmt_map1, (forced_hints, default_hints, var_map, stmt_map))
    =
  let add_merge var_key hints var_map =
    let new_hints =
      let previous_hints =
        try Base.Map.find var_key var_map
        with Not_found -> Ival.Widen_Hints.empty
      in
      Ival.Widen_Hints.union hints previous_hints
    in
    Base.Map.add var_key new_hints var_map
  in
  let map2 = match (stmt_key, var_key) with
    | (None, VarKey (var_key)) ->
         (* add a set of [hints] for a [var_key] *)
      let new_hints =
        let previous_hints =
          try Base.Map.find var_key var_map
          with Not_found -> Ival.Widen_Hints.empty
        in
        Ival.Widen_Hints.union hints previous_hints
      in
      forced_hints,
      default_hints,
      add_merge var_key new_hints var_map,
      stmt_map
    | (Some(stmt_key), VarKey (var_key)) ->
         (* add a set of [hints] for a [stmt_key, var_key] *)
      let new_var_map =
        let previous_var_map =
          try Stmt.Map.find stmt_key stmt_map
          with Not_found -> Base.Map.empty
        in
        add_merge var_key hints previous_var_map
      in
      forced_hints,
      default_hints,
      var_map,
      Stmt.Map.add stmt_key new_var_map stmt_map
    | (_, All) ->
         (* add a set of [hints] for all var_keys *)
      Ival.Widen_Hints.union hints forced_hints,
      default_hints,
      var_map,
      stmt_map
    | (_, Default) ->
         (* add a set of default [hint] *)
      forced_hints,
      Ival.Widen_Hints.union hints default_hints,
      var_map,
      stmt_map
  in
  stmt_map1, map2

(* an [empty] set of hints *)
let empty =
  Stmt.Map.empty,
  (Ival.Widen_Hints.empty,
   Ival.Widen_Hints.empty,
   Base.Map.empty,
   Stmt.Map.empty)

(* a [default] set of hints *)
let default =
  add_num_hints None Default Ival.Widen_Hints.default_widen_hints empty

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
