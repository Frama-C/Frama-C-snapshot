(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Compute a graph which provide the lexical successor of each statement s,
    ie. the statement which is the next one if 's' is replaced by Nop.
    Notice that if 's' is an If, Loop, ...
    the considered statement is the whole block.

    Example : (1) x = 3;
              (2) if (c) (3) y = 3; (4) goto L; else (5) z = 8;
              (6) while (c--) (7) x++;
              (8) L : return x;

    (1) -> (2) -> (6) -> (8)
    (3) -> (4) -> (6)
           (5) -> (6)
           (7) -> (6)
 *)

open Cil_types

(** Add a link prev -> next in the graph.
    Do nothing if prev or next is Kglobal.
 *)
let add_link graph ~prev ~next =
  match (prev, next) with
  | (Kglobal, _) -> ()
  | (_, Kglobal) -> ()
  | (Kstmt s_prev, Kstmt s_next) ->
      let prev_id = s_prev.sid in
      try
        ignore (Inthash.find graph prev_id)
      with Not_found ->
        Pdg_parameters.debug "[lexical successor] add %d -> %d"
	  prev_id s_next.sid;
        Inthash.add graph prev_id s_next

(** Add links from each prev in prev_list to next. *)
let add_links graph prev_list next =
  let link prev = add_link graph prev next in
  List.iter link prev_list

(** Add links from prev_list to stmt,
    (ie. 'stmt' is the lexical succesor of every statements in prev_list)
    and build the links inside stmt (when it contains blocks)
    @return a list of the last statements in stmt to continue processing
            with the statement that follows.
  *)
let rec process_stmt graph ~prev_list ~stmt =
  let ki_stmt = Kstmt stmt in

  add_links graph prev_list ki_stmt;

  match stmt.skind with
  | If (_,bthen,belse,_) ->
      let last_then = process_block graph bthen in
      let last_else = process_block graph belse in
      last_then @ last_else

  | Switch (_,blk,_,_)
  | Block blk -> process_block graph blk
  | UnspecifiedSequence seq ->
      process_block graph (Cil.block_from_unspecified_sequence seq)
  | Loop (_,body,_,_,_) ->
      let last_list = process_block graph body in
      add_links graph last_list ki_stmt; [ki_stmt]

  | Instr _
  | Return _ | Goto _ | Break _ | Continue _
  | TryFinally _ | TryExcept _
    -> [ki_stmt]

(** Process each statement in tail (statement list)
    knowing that the first element of 'tail'
    is the successor of every statement in prev_list.
    @return a list of the last statements in tail or prev_list if tail=[].
  *)
and process_tail graph prev_list tail =
  match tail with
  | [] -> prev_list
  | s :: tail -> let s_last_stmt = process_stmt graph prev_list s in
                 let tail_last_stmt = process_tail graph s_last_stmt tail
                 in tail_last_stmt

(** Process each statement in blk with no previous statement to begin with *)
and process_block graph blk = process_tail graph [Kglobal]  blk.bstmts

(** Type of the graph *)
type t = Cil_types.stmt Inthash.t

(** Compute the lexical successor graph for function kf *)
let compute kf =
  Pdg_parameters.debug "[lexical successor] computing for function %s@."
    (Kernel_function.get_name kf);
  let graph = Inthash.create 50 in
  match kf.Db_types.fundec with
    | Db_types.Declaration _ -> graph
    | Db_types.Definition (f, _) ->
      let _ = process_block graph  f.sbody in graph

(** @return the lexical successor of stmt in graph.
    @raise Not_found if 'stmt' has no successor in 'graph'.
  *)
let find graph stmt =
  try Inthash.find graph stmt.sid
  with Not_found ->
    assert (match stmt.skind with Return _ -> true | _ -> false);
    raise Not_found
