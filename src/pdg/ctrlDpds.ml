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

let dkey = Pdg_parameters.register_category "ctrl-dpds"

open Cil_types
open Cil_datatype

(*============================================================================*)
(** Lexical successors *)
(*============================================================================*)
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
module Lexical_successors : sig

  type t
  val compute : Cil_types.kernel_function -> t

  (** @return the lexical successor of stmt in graph.
  @raise Not_found if 'stmt' has no successor in 'graph' 
  *)
  val find : t -> Cil_types.stmt -> Cil_types.stmt
end = struct

  let dkey = Pdg_parameters.register_category "lex-succs"

  (** Type of the graph *)
  type t = Cil_types.stmt Stmt.Hashtbl.t

  let pp_stmt fmt s = Format.fprintf fmt "@[sid:%d(%a)@]" s.sid Stmt.pretty s

  (** Add links from each [prev] in [prev_list] to [next]. *)
  let add_links graph prev_list next = match prev_list with
    | [] -> ()
    | _ ->
        let link prev =
          try ignore (Stmt.Hashtbl.find graph prev)
          with Not_found ->
            Pdg_parameters.debug ~dkey "add @[%a@,-> %a@]"
              pp_stmt prev pp_stmt next;
            Stmt.Hashtbl.add graph prev next
        in List.iter link prev_list

  (** Add links from [prev_list] to [stmt].
  * (ie. [stmt] is the lexical successor of every statements in [prev_list])
  * and build the links inside [stmt] (when it contains blocks)
  * @return a list of the last statements in [stmt] to continue processing
  *         with the statement that follows.
  *)
  let rec process_stmt graph ~prev_list ~stmt =
    Pdg_parameters.debug ~dkey "computing for statement %a@."
      pp_stmt stmt;
    match stmt.skind with
      | If (_,bthen,belse,_) ->
          let _ = add_links graph prev_list stmt in
          let last_then = process_block graph bthen in
          let last_else = process_block graph belse in
          let prev_list = match last_then, last_else with
            | [], [] -> [ stmt ]
            | last, [] | [], last -> stmt::last
            | last_then, last_else -> last_then @ last_else
          in prev_list

      | Switch (_,blk,_,_)
      | Block blk -> 
          let _ = add_links graph prev_list stmt in
          process_block graph blk
      | UnspecifiedSequence seq ->
          let _ = add_links graph prev_list stmt in
          process_block graph (Cil.block_from_unspecified_sequence seq)

      | Loop (_,body,_,_,_) ->
          let prev_list = match body.bstmts with
            | [] ->
                let _ = add_links graph prev_list stmt in [ stmt ]
            | head::_ -> 
                let _ = add_links graph prev_list head in
                let last_list = process_block graph body in
                let _ = add_links graph last_list stmt in
                  stmt::[]
          in prev_list

      | Instr _
      | Return _ | Goto _ | Break _ | Continue _
      | TryFinally _ | TryExcept _
        -> let _ = add_links graph prev_list stmt in [stmt]

  (** Process each statement in blk with no previous statement to begin with.
  * Then process each statement in the statement list
   * knowing that the first element of 'tail'
   * is the successor of every statement in prev_list.
   * @return a list of the last statements in tail or prev_list if tail=[].
   *)
  and process_block graph blk = 
    let rec process_stmts prev_list stmts = match stmts with
     | [] -> prev_list
     | s :: tail -> 
         let s_last_stmts = process_stmt graph prev_list s in
           process_stmts s_last_stmts tail
    in process_stmts [] blk.bstmts

  (** Compute the lexical successor graph for function kf *)
  let compute kf =
    Pdg_parameters.debug ~dkey "computing for function %s@."
      (Kernel_function.get_name kf);
    if !Db.Value.use_spec_instead_of_definition kf then Stmt.Hashtbl.create 0
    else let graph = Stmt.Hashtbl.create 17 in
         let f = Kernel_function.get_definition kf in
         let _ = process_block graph  f.sbody in graph

  (** @return the lexical successor of stmt in graph.
  @raise Not_found if 'stmt' has no successor in 'graph' ie when it is [return].
  *)
  let find graph stmt =
    try Stmt.Hashtbl.find graph stmt 
    with Not_found ->
      Pdg_parameters.debug ~dkey ~level:2 "not found for stmt:%d@." stmt.sid;
      raise Not_found
end

(*============================================================================*)
(** Postdominators (with infine path extension) *)
(*============================================================================*)
(** This backward dataflow implements a variant of postdominators that verify
    the property P enunciated in bts 963: a statement postdominates itself
    if and only it is within the main path of a syntactically infinite loop.

    The implementation is as follows:
    - compute postdominators with an additional flag infinite loop/non-infinite
    loop. Every path that may terminate does not have the "infinite loop" flag

    - the implementation verifies property P only for Loop statements. To
    obtain the property, the cfg is locally rewritten. For statements
    --> p --> s:Loop --> h --> ... --> e
              ^                        |
              |                        |
              --------------------------
    the edges p --> s are transformed into p --> h, but _not_ the backward
    edges e --> s. This way, s post-dominates itself if and only if s is
    a syntactically infinite loop, but not if there is an outgoing edge. *)
module PdgPostdom : sig

  type t

  val compute : kernel_function -> t

  (** @param with_s tells if the statement has to be added to its postdom.
  * The returned boolean tells if there is a path to [return] *)
  val get : t -> with_s:bool -> stmt -> bool * Stmt.Hptset.t

end = struct 

  module State = struct
    type t = 
      | ToReturn of Stmt.Hptset.t
      | ToInfinity of Stmt.Hptset.t

    let inter a b = match a,b with
      | ToReturn v, ToReturn v' -> ToReturn ( Stmt.Hptset.inter v v')
      | ToInfinity v, ToInfinity v' -> ToInfinity ( Stmt.Hptset.inter v v')
      | ToReturn v, ToInfinity _ | ToInfinity _, ToReturn v -> ToReturn v

    let equal a b = match a,b with
      | ToReturn v, ToReturn v' ->  Stmt.Hptset.equal v v'
      | ToInfinity v, ToInfinity v' ->  Stmt.Hptset.equal v v'
      | _ -> false

    let add stmt set = match set with
      | ToReturn set -> ToReturn (Stmt.Hptset.add stmt set)
      | ToInfinity set -> ToInfinity (Stmt.Hptset.add stmt set)

    let pretty fmt d =
      match d with
        | ToReturn d -> Format.fprintf fmt "{%a}_ret" Stmt.Hptset.pretty d
        | ToInfinity d -> Format.fprintf fmt "{%a}_oo" Stmt.Hptset.pretty d
  end

  type t = State.t Stmt.Hashtbl.t

  let _pretty fmt infos =
    Stmt.Hashtbl.iter
      (fun k v -> Format.fprintf fmt "Stmt:%d\n%a\n======" k.sid State.pretty v)
      infos

  let is_in_stmts iter s stmts =
    try iter (fun s' -> if s.sid = s'.sid then raise Exit) stmts; false
    with Exit -> true

  (** change [succs] so move the edges [entry -> loop] to [entry -> head] *)
  let succs stmt = 
    let modif acc s = match s.skind with
      | Loop _ -> 
          let head = match s.succs with | [head] -> head | _ -> assert false in
          let entry, _back_edges = Stmts_graph.loop_preds s in
            if is_in_stmts List.iter stmt entry then head::acc else s::acc
      | _ -> s::acc
    in List.fold_left modif [] stmt.succs

  (** change [preds] so remove the edges [entry <- loop] 
  * and to add the edges [entry <- head] *)
  let preds stmt = match stmt.skind with
    | Loop _ -> (* remove edges from entry to loop *)
        let _entry, back_edges = Stmts_graph.loop_preds stmt in back_edges
    | _ -> 
        let modif acc s = match s.skind with
          | Loop _ -> 
              let entry, _back_edges = Stmts_graph.loop_preds s in
               s::entry@acc
          | _ -> s::acc
        in List.fold_left modif [] stmt.preds

  let add_postdom infos start init =
    let get s =
      try Stmt.Hashtbl.find infos s
      with Not_found -> State.ToInfinity Stmt.Hptset.empty
    in
    let do_stmt stmt = match succs stmt with 
      | [] when stmt.sid = start.sid -> 
          Some (State.ToReturn (Stmt.Hptset.empty))
      | [] -> assert false
      | s::tl -> 
          let add_get s = State.add s (get s) in
          let combineSuccessors st s = State.inter st (add_get s) in
          let st = List.fold_left combineSuccessors (add_get s) tl in
          let old = get stmt in
          let new_st = (* don't need to State.inter old *) st in
            if State.equal old new_st then None 
            else Some new_st
    in 
    let todo =  Queue.create () in
    let add_todo p = 
      if is_in_stmts Queue.iter p todo then () else Queue.add p todo 
    in
    let rec do_todo () =
      let s = Queue.take todo in
      begin
        match do_stmt s with
        | None -> (* finished with that one *) ()
        | Some st -> (* store state and add preds *)
        Stmt.Hashtbl.add infos s st; List.iter add_todo (preds s)
      end;
      do_todo ()
    in
    try
      let _ = Stmt.Hashtbl.add infos start init in
      let _ = List.iter (fun p -> Queue.add p todo) (preds start) in
      do_todo () 
    with Queue.Empty -> ()

  let compute kf =
    let infos = Stmt.Hashtbl.create 50 in
    let return  =
      try Kernel_function.find_return kf
      with Kernel_function.No_Statement ->
        Pdg_parameters.fatal "No return statement for a function with body %a"
          Kernel_function.pretty kf
    in 
    let _ = add_postdom infos return (State.ToReturn (Stmt.Hptset.empty)) in
    let stmts =
      if !Db.Value.use_spec_instead_of_definition kf then
        invalid_arg "[traces] cannot compute for a leaf function"
      else
        let f = Kernel_function.get_definition kf in f.sallstmts
    in
    let remove_top s =
      try ignore (Stmt.Hashtbl.find infos s) with Not_found -> 
        Pdg_parameters.debug ~dkey "compute infinite path to sid:%d" s.sid;
        add_postdom infos s (State.ToInfinity (Stmt.Hptset.empty))
    in
    let _ = List.iter remove_top stmts in
      infos

  let get infos ~with_s stmt =
    try
      let stmt_to_ret, postdoms = match Stmt.Hashtbl.find infos stmt with
        | State.ToInfinity postdoms -> false, postdoms
        | State.ToReturn postdoms -> true, postdoms
      in let postdoms =
        if with_s then Stmt.Hptset.add stmt postdoms else postdoms
      in 
        Pdg_parameters.debug ~dkey ~level:2 
          "get_postdoms for sid:%d (%s) = %a (%spath to ret)@." 
          stmt.sid (if with_s then "with" else "without")
          Stmt.Hptset.pretty postdoms (if stmt_to_ret then "" else "no ");
        stmt_to_ret, postdoms
    with Not_found -> assert false

end
(*============================================================================*)
(** Compute information needed for control dependencies *)
(*============================================================================*)

type t = Lexical_successors.t * PdgPostdom.t

let compute kf =
  let lex_succ_graph = Lexical_successors.compute kf in
  let ctrl_dpds_infos = PdgPostdom.compute kf in
    (lex_succ_graph, ctrl_dpds_infos)

(** Compute the PDB(A,B) set used in the control dependencies algorithm.
 * Roughly speaking, it gives {v (\{B\} U postdom(B))-postdom(A) v}.
 * It means that if S is in the result, it postdominates B but not A.
 * As B is usually a successor of A, it means that S is reached if the B-branch
 * is chosen, but not necessary for the other branches. Then, S should depend
 * on A.
  (see the document to know more about the applied algorithm)
 *)
let pd_b_but_not_a infos stmt_a stmt_b =
  if stmt_a.sid = stmt_b.sid then Stmt.Hptset.empty
  else begin
    let a_to_ret, postdom_a = PdgPostdom.get infos ~with_s:false stmt_a in
    let b_to_ret, postdom_b = PdgPostdom.get infos ~with_s:true stmt_b in
    let res = match a_to_ret, b_to_ret with
      | true, true | false, false -> Stmt.Hptset.diff postdom_b postdom_a
      | true, false -> postdom_b
      | false, true -> (* no path [a, ret] but path [b, ret]
                        * possible when a there is a jump, because then we have
                        * either (A=G, B=S) or (A=S, B=L) *)
          Stmt.Hptset.empty (* because we don't want b postdoms
                                 to depend on the jump *)
    in
      Pdg_parameters.debug ~dkey ~level:2 
        "pd_b_but_not_a for a=sid:%d b=sid:%d = %a"
        stmt_a.sid stmt_b.sid Stmt.Hptset.pretty res;
      res
  end

(*============================================================================*)
(** Control dependencies *)
(*============================================================================*)

(** @return the statements which are depending on the condition.
 *
 * {v = U (PDB (if, succs(if)) v}
 * (see the document to know more about the applied algorithm).
 *)
let get_if_controled_stmts ctrl_dpds_infos stmt =
  let _, infos = ctrl_dpds_infos in
  let add_pdb_s set succ =
      Stmt.Hptset.union set (pd_b_but_not_a infos stmt succ)
  in
  let controled_stmts = List.fold_left add_pdb_s Stmt.Hptset.empty stmt.succs in
  Pdg_parameters.debug ~dkey "controled_stmt for cond sid:%d = %a"
    stmt.sid Stmt.Hptset.pretty controled_stmts;
  controled_stmts

let jump_controled_stmts infos jump label lex_suc =
  Pdg_parameters.debug ~dkey ~level:2 
    "lex_succ sid:%d = sid:%d" jump.sid lex_suc.sid;
  Pdg_parameters.debug ~dkey ~level:2 
    "jump succ sid:%d = sid:%d" jump.sid label.sid;
  let controled_stmts =
    if lex_suc.sid = label.sid then begin
      (* the label is the jump lexical successor: no dpds *)
      Pdg_parameters.debug ~dkey "useless jump sid:%d (label = lex_succ = %d)" 
        jump.sid lex_suc.sid;
      Stmt.Hptset.empty
    end else
      let pdb_jump_lex_suc = pd_b_but_not_a infos jump lex_suc in
      let pdb_lex_suc_label = pd_b_but_not_a infos lex_suc label in
      let pdb_lex_suc_label =
        Stmt.Hptset.remove lex_suc pdb_lex_suc_label
      in Stmt.Hptset.union pdb_jump_lex_suc pdb_lex_suc_label
  in
    controled_stmts

(** let's find the statements which are depending on
* the jump statement (goto, break, continue) =
  {v PDB(jump,lex_suc) U (PDB(lex_suc,label) - lex_suc) v}
  (see the document to know more about the applied algorithm).
  *)
let get_jump_controled_stmts ctrl_dpds_infos jump =
  let lex_succ_graph, infos = ctrl_dpds_infos in
  let lex_suc = 
    try Lexical_successors.find lex_succ_graph jump
    with Not_found -> assert false
  in
  let label = match jump.succs with | [label] -> label | _ -> assert false in
  let controled_stmts =  jump_controled_stmts infos jump label lex_suc in
  Pdg_parameters.debug ~dkey "controled_stmt for jump sid:%d = %a"
    jump.sid Stmt.Hptset.pretty controled_stmts;
  controled_stmts

(** Try to process [while(1) S; LS: ] as [L: S; goto L; LS: ] *)
let get_loop_controled_stmts ctrl_dpds_infos loop =
  let lex_succ_graph, infos = ctrl_dpds_infos in
  let lex_suc = 
    try Lexical_successors.find lex_succ_graph loop 
    with Not_found -> (* must have at least a return *) assert false
  in
  let jump = loop in
  let label = match loop.succs with [head] -> head | _ -> assert false in
  let controled_stmts = jump_controled_stmts infos jump label lex_suc in
    Pdg_parameters.debug ~dkey "controled_stmt for loop sid:%d = %a"
      loop.sid Stmt.Hptset.pretty controled_stmts;
    controled_stmts

(*============================================================================*)
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
