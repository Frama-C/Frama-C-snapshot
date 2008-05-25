(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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

open Cil_types

let debug = false

module S = struct
  type t = Cilutil.StmtSet.t
  let empty = Cilutil.StmtSet.empty
  let singleton = Cilutil.StmtSet.singleton

  let add = Cilutil.StmtSet.add
  let remove = Cilutil.StmtSet.remove

  let equal = Cilutil.StmtSet.equal

  let inter = Cilutil.StmtSet.inter
  let diff = Cilutil.StmtSet.diff
  let union = Cilutil.StmtSet.union

  let elements = Cilutil.StmtSet.elements
  let pretty = Cilutil.StmtSet.pretty
end

type t_info =
    | ToReturn of S.t 
    | ToInfinity of S.t 
    | Init

module State = struct
  type t = t_info

  let inter a b = match a,b with
    | Init, Init -> Init
    | ToReturn v, Init | Init, ToReturn v -> ToReturn v
    | ToInfinity v, Init | Init, ToInfinity v -> ToInfinity v
    | ToReturn v, ToReturn v' -> ToReturn ( S.inter v v')
    | ToInfinity v, ToInfinity v' -> ToInfinity ( S.inter v v')
    | ToReturn v, ToInfinity _ | ToInfinity _, ToReturn v -> ToReturn v

  let equal a b = match a,b with
    | Init, Init -> true
    | ToReturn v, ToReturn v' ->  S.equal v v'
    | ToInfinity v, ToInfinity v' ->  S.equal v v'
    | _ -> false

  let add stmt set = match set with
    | Init -> Init
    | ToReturn set -> ToReturn (S.add stmt set)
    | ToInfinity set -> ToInfinity (S.add stmt set)

  let pretty fmt d =
    match d with
    | Init ->
	Format.fprintf fmt "Top"
    | ToReturn d -> Format.fprintf fmt "{%a}_ret" S.pretty d
    | ToInfinity d -> Format.fprintf fmt "{%a}_oo" S.pretty d
end

module States = struct
  type t = State.t Inthash.t
  let create = Inthash.create
  let add = Inthash.add
  let find = Inthash.find
  let pretty fmt infos = 
    Inthash.iter 
      (fun k v -> Format.fprintf fmt "Stmt:%d\n%a\n======" k State.pretty v)
      infos
end

type t = Lexical_successors.t * States.t

module Computer (Param:sig 
                   val states : States.t
                   val end_point : int 
                 end) = struct

  let name = "ctrlDpds"
  let debug = ref false

  type t = State.t
  let pretty = State.pretty

  module StmtStartData = struct
    type data = t
    let clear () = Inthash.clear Param.states
    let mem = Inthash.mem Param.states
    let find = Inthash.find Param.states
    let replace = Inthash.replace Param.states
    let add = Inthash.add Param.states
    let iter f = Inthash.iter f Param.states
  end


  let combineStmtStartData _stmt ~old new_ =
    let result = (* inter old *) new_ in
    if State.equal result old then None else Some result

  let combineSuccessors = State.inter

  let doStmt stmt =
    if stmt.sid = Param.end_point then
      Dataflow.Done (ToInfinity (S.singleton stmt))
    else
      Dataflow.Post (fun data -> State.add stmt data)

  let doInstr _ _ _ = Dataflow.Default

  let filterStmt _stmt _next = true
    (* assert (Db.ToReturn.is_accessible (Kstmt next));
       Db.ToReturn.is_accessible (Kstmt stmt) *)

  let funcExitData = ToReturn S.empty

end

let go infos end_point =
  let module Computer = Computer (struct
                                    let end_point = end_point.sid
                                    let states = infos
                                  end)
  in let module Compute = Dataflow.BackwardsDataFlow(Computer) in
  Compute.compute [end_point]

let compute_on_infinite_traces infos tops =
  let rec remove_top stmts = match stmts with
    | [] -> ()
    | s :: stmts ->
        if States.find infos s.sid = Init then go infos s else ();
        remove_top stmts
  in remove_top tops

let compute_infos kf =
  let stmts =
    try
      let f = Kernel_function.get_definition kf in f.sallstmts
    with Kernel_function.No_Definition -> invalid_arg
                               "[traces] cannot compute for a leaf function"
  in
  let infos = States.create 50 in
  (*List.iter (fun s -> States.add s.sid (ToReturn (S.empty))) stmts;*)
  (*List.iter (fun s -> States.add s.sid (ToReturn (S.singleton s))) stmts;*)
  (* let return = find_return kf in go return; *)
  let init tops s = 
    let tops, postdom = 
      try tops, ToReturn (!Db.Postdominators.stmt_postdominators kf s)
      with Db.Postdominators.Top -> s::tops, Init
    in
    States.add infos s.sid postdom ;
    tops
  in
  let tops = List.fold_left init [] stmts in
  let _ = match tops with
      | [] -> ()
      | _ -> 
          begin
            Cil.log "[traces] computing for function %a@\n" 
	      Kernel_function.pretty_name kf;
            Cil.log "[traces] WARNING : experimental feature...@\n";
            Cil.log "  -> infinite loop processing@\n" ;
            compute_on_infinite_traces infos tops
          end
  in infos

let compute kf =
  let lex_succ_graph = Lexical_successors.compute kf in
  let ctrl_dpds_infos = compute_infos kf in
    (lex_succ_graph, ctrl_dpds_infos)

let get_postdoms infos ~without stmt =
  try
    let stmt_to_ret, postdoms = match States.find infos stmt.sid with
      | ToInfinity postdoms -> false, postdoms
      | ToReturn postdoms -> true, postdoms
      | Init -> assert false 
    in let postdoms = if without then S.remove stmt postdoms else postdoms in
      stmt_to_ret, postdoms
  with Not_found -> assert false

(** Compute the PDB(A,B) set used in the control dependencies algorithm.
 * Roughly speaking, it gives {v (\{B\} U postdom(B))-postdom(A) v}.
 * It means that if S is in the result, it postdominates B but not A.
 * As B is usually a successor of A, it means that S is reached if the B-branch
 * is chosen, but not necessary for the other branches. Then, S should depend
 * on A.
  (see the document to know more about the applied algorithm)
 *)
let pd_b_but_not_a infos stmt_a stmt_b =
  if stmt_a.sid = stmt_b.sid then S.empty
  else begin
    let a_to_ret, postdom_a = get_postdoms infos ~without:true stmt_a in
    let b_to_ret, postdom_b = get_postdoms infos ~without:false stmt_b in
    let res = match a_to_ret, b_to_ret with
      | true, true | false, false -> S.diff postdom_b postdom_a 
      | true, false -> postdom_b
      | false, true -> (* no path [a, ret] but path [b, ret]
                        * possible when a there is a jump, because then we have
                        * either (A=G, B=S) or (A=S, B=L) *)
          S.empty (* because we don't want b postdoms to depend on the jump *)
    in 
      if Macros.debug2 () then
        Format.printf "[pdg] pd_b_but_not_a for a=%d b=%d = %a@\n"
          stmt_a.sid stmt_b.sid S.pretty res;
      res
  end

(** @return the statements which are depending on the condition.
 *
 * {v = U (PDB (if, succs(if)) v}
 * (see the document to know more about the applied algorithm).
 *)
let get_if_controled_stmts ctrl_dpds_infos stmt =
  let _, infos = ctrl_dpds_infos in
  let add_pdb_s set succ = S.union set (pd_b_but_not_a infos stmt succ) in
  let controled_stmts = List.fold_left add_pdb_s S.empty stmt.succs in
  if Macros.debug1 () then
    Format.printf "[pdg] controled_stmt for cond %d = %a@\n" 
      stmt.sid S.pretty controled_stmts;
  let controled_stmts = S.elements controled_stmts in
  controled_stmts

(** let's find the statements which are depending on 
* the jump statement (goto, break, continue, loop) =
  {v PDB(jump,lex_suc) U (PDB(lex_suc,label) - lex_suc) v}
  (see the document to know more about the applied algorithm).
  *)
let get_jump_controled_stmts ctrl_dpds_infos jump =
  let lex_succ_graph, infos = ctrl_dpds_infos in
  let controled_stmts =
  try
    let lex_suc = Lexical_successors.find lex_succ_graph jump in
      if Macros.debug2 ()
      then Format.printf "[pdg] lex_succ %d = %d@\n" jump.sid lex_suc.sid;
    match jump.succs with
    | [label] ->
        if Macros.debug2 ()
        then Format.printf "[pdg] jump succ %d = %d@\n" jump.sid label.sid;
        if lex_suc.sid = label.sid
        then (* the label is the jump lexical successor : no dpds *)
          (if Macros.debug1 () then Format.printf "[pdg] useless jump %d@\n" jump.sid;
          S.empty
          )
        else
            let pdb_jump_lex_suc = pd_b_but_not_a infos jump lex_suc in
            let pdb_lex_suc_label = pd_b_but_not_a infos lex_suc label in
            let pdb_lex_suc_label = S.remove lex_suc pdb_lex_suc_label in
            S.union pdb_jump_lex_suc pdb_lex_suc_label
    | _ -> assert false
  with Not_found ->
    if debug
    then Format.printf "[pdg] lex_succ %d = (none) @\n" jump.sid;
    (* no lexical successor : every postdom (jump) depend on jump. *)
    let _, pd_jump = (get_postdoms infos ~without:false jump) in
    S.remove jump pd_jump
  in
  if Macros.debug1 () then
    Format.printf "[pdg] controled_stmt for jump %d = %a@\n" 
      jump.sid S.pretty controled_stmts;
  let controled_stmt_list = S.elements controled_stmts in
    controled_stmt_list

let display = States.pretty 

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
