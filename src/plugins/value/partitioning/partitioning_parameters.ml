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

open Value_parameters
open Partitioning_annots
open Cil_types

let is_return s = match s.skind with Return _ -> true | _ -> false
let is_loop s =   match s.skind with Loop _ -> true | _ -> false

let warn ?(current = true) = Kernel.warning ~once:true ~current

module Make (Kf : sig val kf: kernel_function end) =
struct
  let kf = Kf.kf

  let widening_delay = WideningDelay.get ()
  let widening_period = WideningPeriod.get ()

  let interpreter_mode = InterpreterMode.get ()

  let slevel stmt =
    if is_return stmt || interpreter_mode then
      max_int
    else match Per_stmt_slevel.local kf with
      | Per_stmt_slevel.Global i -> i
      | Per_stmt_slevel.PerStmt f -> f stmt

  let merge_after_loop = SlevelMergeAfterLoop.mem kf

  let merge stmt =
    is_loop stmt && merge_after_loop
    ||
    match Per_stmt_slevel.merge kf with
    | Per_stmt_slevel.NoMerge -> false
    | Per_stmt_slevel.Merge f -> f stmt

  let term_to_exp term =
    !Db.Properties.Interp.term_to_exp ~result:None term

  let min_loop_unroll = MinLoopUnroll.get ()
  let auto_loop_unroll = AutoLoopUnroll.get ()
  let default_loop_unroll = DefaultLoopUnroll.get ()

  let warn_no_loop_unroll stmt =
    let is_attribute a = Cil.hasAttribute a stmt.sattr in
    match List.filter is_attribute ["for" ; "while" ; "dowhile"] with
    | [] -> ()
    | loop_kind :: _ ->
      let wkey =
        if loop_kind = "for"
        then Value_parameters.wkey_missing_loop_unroll_for
        else Value_parameters.wkey_missing_loop_unroll
      in
      Value_parameters.warning
        ~wkey ~source:(fst (Cil_datatype.Stmt.loc stmt)) ~once:true
        "%s loop without unroll annotation" loop_kind

  let unroll stmt =
    let default =
      if auto_loop_unroll > min_loop_unroll
      then Partition.AutoUnroll (stmt, min_loop_unroll, auto_loop_unroll)
      else Partition.IntLimit min_loop_unroll
    in
    match get_unroll_annot stmt with
    | [] -> warn_no_loop_unroll stmt; default
    | [None] -> Partition.IntLimit default_loop_unroll
    | [(Some t)] -> begin
        (* Inlines the value of const variables in [t]. *)
        let global_init vi =
          try (Globals.Vars.find vi).init with Not_found -> None
        in
        let t =
          Cil.visitCilTerm (new Logic_utils.simplify_const_lval global_init) t
        in
        match Logic_utils.constFoldTermToInt t with
        | Some n -> Partition.IntLimit (Integer.to_int n)
        | None   ->
          try
            Partition.ExpLimit (term_to_exp t)
          with Db.Properties.Interp.No_conversion ->
            warn "loop unrolling parameters must be valid expressions";
            default
      end
    | _ ->
      warn "ignoring invalid unroll annotation";
      default

  let history_size = HistoryPartitioning.get ()

  let split_limit = SplitLimit.get ()

  let universal_splits =
    let add name l =
      try
        let vi = Globals.Vars.find_from_astinfo name VGlobal in
        let monitor = Partition.new_monitor ~split_limit in
        Partition.Split (Cil.evar vi, Partition.Dynamic, monitor) :: l
      with Not_found ->
        warn ~current:false "cannot find the global variable %s for value \
                             partitioning" name;
        l
    in
    ValuePartitioning.fold add []

  let flow_actions stmt =
    let kind = Partition.Static in
    let map_annot acc t =
      try
        let monitor = Partition.new_monitor ~split_limit in
        let action =
          match t with
          | FlowSplit t -> Partition.Split (term_to_exp t, kind, monitor)
          | FlowMerge t -> Partition.Merge (term_to_exp t, kind)
        in
        action :: acc
      with
        Db.Properties.Interp.No_conversion ->
        warn "split/merge expressions must be valid expressions";
        acc (* Impossible to convert term to lval *)
    in
    List.fold_left map_annot [] (get_flow_annot stmt)
end
