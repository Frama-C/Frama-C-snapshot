(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: widen_type.ml,v 1.21 2008-10-03 13:09:17 uid568 Exp $ *)

open Cil
open Cil_types
open Abstract_value
open Cilutil
open BaseUtils
    
type widen_hint = Ival.Widen_Hints.t
    
(* map from Base.t to Ival.Widen_Hints.t *)
type var_key = Default | All | VarKey of Cvalue_type.V.M.key
type widen_hint_map_from_var_key = widen_hint BaseMap.t

(* map from stmt to widen_hint_map_from_var_key *)
type stmt_key = StmtMap.key option
type widen_hint_map_from_stmt_key = widen_hint_map_from_var_key StmtMap.t

type t = 
    BaseSet.t StmtMap.t
    * (widen_hint 
       * widen_hint 
       * widen_hint_map_from_var_key 
       * widen_hint_map_from_stmt_key)

let hints_from_key (forced_hints, default_hints, var_map) var_key =
  let widen_hints =
    let hints =
      try
        Ival.Widen_Hints.union (BaseMap.find var_key var_map) default_hints
      with Not_found -> default_hints
    in Ival.Widen_Hints.union forced_hints hints
  in (* Format.printf "WIDEN_HINT widen a var_key %a -> %a @\n"
        Base.pretty var_key
        Ival.Widen_Hints.pretty widen_hints; *)
    (Cvalue_type.V.Top_Param.O.empty, (fun _ -> widen_hints))

let hints_from_keys stmt_key (stmt_map1, (forced_hints, default_hints, var_map, stmt_map)) =
  let var_map =
    try
      StmtMap.find stmt_key stmt_map
    with Not_found -> var_map
  and var_set =
    try
      StmtMap.find stmt_key stmt_map1
    with Not_found -> BaseSet.empty
  in (var_set, hints_from_key (forced_hints, default_hints, var_map))

let add_var_hints stmt var_hints (stmt_map1, map2) =
    let new_hints = 
      let previous_hints =
        try
          StmtMap.find stmt stmt_map1
        with Not_found -> BaseSet.empty
      in BaseSet.union var_hints previous_hints
    in (StmtMap.add stmt new_hints stmt_map1, map2)
  
let add_num_hints stmt_key var_key hints (stmt_map1, (forced_hints, default_hints, var_map, stmt_map)) =
  let add_merge var_key hints var_map =
    let new_hints = 
      let previous_hints =
        try
          BaseMap.find var_key var_map
        with Not_found -> Ival.Widen_Hints.empty
      in Ival.Widen_Hints.union hints previous_hints
    in BaseMap.add var_key new_hints var_map
  in let map2 =
      match (stmt_key, var_key) with
        | (None, VarKey (var_key)) ->
            (* add a set of [hints] for a [var_key] *)
            let new_hints = 
              let previous_hints =
                try
                  BaseMap.find var_key var_map
                with Not_found -> Ival.Widen_Hints.empty
              in Ival.Widen_Hints.union hints previous_hints
            in (forced_hints, default_hints, add_merge var_key new_hints var_map, stmt_map)
        | (Some(stmt_key), VarKey (var_key)) ->
            (* add a set of [hints] for a [stmt_key, var_key] *)
            let new_var_map = 
              let previous_var_map =
                try
                  StmtMap.find stmt_key stmt_map
                with Not_found -> BaseMap.empty
              in add_merge var_key hints previous_var_map
            in (forced_hints, default_hints, var_map, StmtMap.add stmt_key new_var_map stmt_map)
        | (_, All) ->
            (* add a set of [hints] for all var_keys *)
            (Ival.Widen_Hints.union hints forced_hints, default_hints, var_map, stmt_map)
        | (_, Default) ->
            (* add a set of default [hint] *)
            (forced_hints, Ival.Widen_Hints.union hints default_hints, var_map, stmt_map)
  in (stmt_map1, map2) 
    
(* an [empty] set of hints *)
let empty =
  (StmtMap.empty,
   (Ival.Widen_Hints.empty,
    Ival.Widen_Hints.empty,
    BaseMap.empty,
    StmtMap.empty))
    
(* a [default] set of hints *)
let default =
  add_num_hints None Default Ival.Widen_Hints.default_widen_hints empty
    
module Datatype =
  Project.Datatype.Persistent
    (struct type tt = t type t = tt let name = "widen_type" end)
