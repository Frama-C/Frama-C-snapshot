(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

open Cil_types
open Cil

(** A framework for data flow analysis for CIL code.  Before using
    this framework, you must initialize the Control-flow Graph for your
    program, e.g using {!Cfg.computeFileCFG} *)

type 't action =
    Default (** The default action *)
  | Done of 't (** Do not do the default action. Use this result *)
  | Post of ('t -> 't) (** The default action, followed by the given
                        * transformer *)

type 't stmtaction =
    SDefault   (** The default action *)
  | SDone      (** Do not visit this statement or its successors *)
  | SUse of 't (** Visit the instructions and successors of this statement
                  as usual, but use the specified state instead of the
                  one that was passed to doStmt *)

(* For if statements *)
type 't guardaction =
    GDefault      (** The default state *)
  | GUse of 't    (** Use this data for the branch *)
  | GUnreachable  (** The branch will never be taken. *)

module type StmtStartData = sig
  type data
  val clear: unit -> unit
  val mem: Cil_types.stmt -> bool
  val find: Cil_types.stmt -> data
  val replace: Cil_types.stmt -> data -> unit
  val add: Cil_types.stmt -> data -> unit
  val iter: (Cil_types.stmt -> data -> unit) -> unit
  val length: unit -> int
end

module StartData(X: sig type t val size: int end) = struct
  type data = X.t
  open Cil_datatype.Stmt.Hashtbl
  let stmtStartData = create X.size
  let clear () = clear stmtStartData
  let mem = mem stmtStartData
  let find = find stmtStartData
  let replace = replace stmtStartData
  let add = add stmtStartData
  let iter f = iter f stmtStartData
  let length () = length stmtStartData
end

(** Find which function we are analysing from the set of inital statements *)
let current_kf = function
  | [] -> assert false
  | s :: q ->
    let kf = Kernel_function.find_englobing_kf s in
    let same_kf s' =
      let kf' = Kernel_function.find_englobing_kf s' in
      Kernel_function.equal kf kf'
    in
    assert (List.for_all same_kf q);
    kf

module type WORKLIST = sig
  type t

(** Create a worklist for function [kf], initially populated by the stmt list. *)
  val create: Kernel_function.t -> stmt list -> t

(** Add a statement to the worklist. If the statement is already
    there, it is not added a second time. *)
  val add: t -> stmt -> unit

  (** Retrieve and remove the next element in the worklist. Raise
      [Empty] if the worklist is empty. *)
  val pop_next: t -> stmt
  exception Empty

  val is_empty: t -> bool
  val fold: (stmt -> 'b -> 'b) -> t -> 'b -> 'b
end

module type MAYBE_REVERSE = sig
  (* [maybe_rev_int a b] is [a] in forward dataflow (no reversal), or
     [b+1-a] in reverse dataflow (reversal, i.e. counts from the
     end). *)
  val maybe_rev_int: int -> int -> int
end

module Worklist(MaybeReverse:MAYBE_REVERSE):WORKLIST = struct

  (* The worklist algorithm determines the order of propagation of the
     dataflow. The current strategy is to iterate one strongly
     connected components after another; inside one strongly connected
     component, we reexecute a statement already seen only once all
     the statements in the strongly connected component has been
     executed. This is implemented with a bitvector used as a priority
     queue, which is scanned iteratively, and rolled back (if
     necessary) at the end of a strongly connected component. *)

  type ordered_stmt = int
  type connex_component = int
    
  type t = 
    { 					
      (** Priority queue implemented as a bit vector. Index 0 has the
          highest priority.*)
      bv: Bitvector.t;

      (** Conversions between stmt and ordered_stmt. *)
      order: Ordered_stmt.stmt_to_ordered;
      unorder: Ordered_stmt.ordered_to_stmt;
      connex: connex_component array;

      (** Next stmt to be retrieved.  *)
      mutable next: ordered_stmt;

      (** The connex component for the last call to next(). *)
      mutable current_scc: connex_component;

      (** The first statement changed in the current scc, or None
          if the scc has not changed. *)
      mutable must_restart_cc: ordered_stmt option;
    }

  (* Forward and backward dataflow use the same data structure, but
     the index in the bitvector is reversed: in forward dataflow, 0
     corresponds to the entry point of the function, while in backward
     dataflow, it is one of the sinks. *)
  let maybe_reverse t i =
    let nb_stmts = Array.length t.unorder in
    MaybeReverse.maybe_rev_int i nb_stmts
  ;;

  let stmt_from_ordered t ordered =
    Ordered_stmt.to_stmt t.unorder (maybe_reverse t ordered)
  ;;

  let ordered_from_stmt t stmt =
    maybe_reverse t (Ordered_stmt.to_ordered t.order stmt)
  ;;

  let connex_of_ordered t ordered =
    let k = maybe_reverse t ordered in
    t.connex.(k)
  ;;


  let create kf stmts = 
    let (order,unorder,connex) = Ordered_stmt.get_conversion_tables kf in
    let nb_stmts = Array.length unorder in 
    let bv = Bitvector.create nb_stmts in
    let (min, ordereds) = List.fold_left (fun (cur_min,cur_list) stmt -> 
      let ordered = MaybeReverse.maybe_rev_int (Ordered_stmt.to_ordered order stmt) nb_stmts in
      (min cur_min ordered, ordered::cur_list)) (0,[]) stmts 
    in 
    List.iter (fun ordered -> Bitvector.set bv ordered) ordereds;
    let next = min in
    let current_scc = connex.(next) in
    let must_restart_cc = None in
    { bv; order; unorder; next; current_scc; connex; must_restart_cc }

  let add t stmt = 
    let i = ordered_from_stmt t stmt in
    Bitvector.set t.bv i;
    if i < t.next 
    then t.must_restart_cc <- 
      match t.must_restart_cc with
      | None -> Some(i)
      | Some(j) -> Some(min i j)
  ;;

  let is_empty t = Bitvector.is_empty t.bv

  exception Empty;;

  let pop_next t =

    let restart_from i = 
      (* We should restart in the same connex component. *)
      assert((connex_of_ordered t i) == t.current_scc);
      t.must_restart_cc <- None;
      i
    in

    let real_next = 
      try
        let next_true = Bitvector.find_next_true t.bv t.next in
        let next_true_scc = connex_of_ordered t next_true in
        if next_true_scc == t.current_scc 
        then 
          (* Continue in the same connex component. *)
          next_true 
        else 
          (* We reached the end of the current connex component. The
             trick is that OCamlgraph's topological ordering guarantee
             that elements of the same connex component have
             congiguous indexes, so we know that we have reached the
             end of the current connex component. Check if we should
             start over in the same connex component, or continue to
             the next cc. *)
          ((* assert (next_true_scc < t.current_scc); *)
           match t.must_restart_cc with
           | None -> t.current_scc <- next_true_scc; next_true
           | Some(i) -> restart_from i)
      with Not_found -> 
        (* We found no further work, but it could be because the graph
           ends with a non-trival connex component (e.g. the function
           ends with a loop). *)
        (match t.must_restart_cc with
        | None -> raise Empty
        | Some(i) -> restart_from i)
    in
    Bitvector.clear t.bv real_next;
    t.next <- real_next + 1;
    t.current_scc <- connex_of_ordered t real_next;
    let stmt = stmt_from_ordered t real_next in
    (* Kernel.debug "next: %d\n" stmt.sid; *)
    stmt
  ;;

  let fold f t init = 
    Bitvector.fold_true (fun acc i -> 
      f (stmt_from_ordered t i) acc) init t.bv

end

module ForwardWorklist = Worklist(struct let maybe_rev_int k _ = k end)
module BackwardWorklist = Worklist(struct let maybe_rev_int k n = (n-1) - k end)

(******************************************************************
 **********
 **********         FORWARDS
 **********
 ********************************************************************)

module type ForwardsTransfer = sig
  val name: string
  val debug: bool
  type t
  val copy: t -> t
  val pretty: Format.formatter -> t -> unit
  val computeFirstPredecessor: stmt -> t -> t
  val combinePredecessors: stmt -> old:t -> t -> t option
  val doInstr: stmt -> instr -> t -> t
  val doGuard: stmt -> exp -> t -> t guardaction * t guardaction
  val doStmt: stmt -> t -> t stmtaction
  val doEdge: stmt -> stmt -> t -> t

  module StmtStartData: StmtStartData with type data = t
  (** For each statement id, the data at the start. Not found in the hash
   * table means nothing is known about the state at this point. At the end
   * of the analysis this means that the block is not reachable. *)
end

module Forwards(T : ForwardsTransfer) = struct

    (** We call this function when we have encountered a statement, with some
     * state. *)
    let reachedStatement worklist pred (s: stmt) (d: T.t) : unit =
      (** see if we know about it already *)
      let d = T.doEdge pred s d in
      let newdata: T.t option =
        try
          let old = T.StmtStartData.find s in
          match T.combinePredecessors s ~old:old d with
            None -> (* We are done here *)
              if T.debug then
                Kernel.debug "FF(%s): reached stmt %d with %a\n  implies the old state %a\n"
                  T.name s.sid T.pretty d T.pretty old;
              None
          | Some d' -> begin
              (* We have changed the data *)
              if T.debug then
                Kernel.debug "FF(%s): weaken data for block %d: %a\n"
                  T.name s.sid T.pretty d';
              Some d'
          end
        with Not_found -> (* was bottom before *)
          let d' = T.computeFirstPredecessor s d in
          if T.debug then
            Kernel.debug "FF(%s): set data for block %d: %a\n"
              T.name s.sid T.pretty d';
          Some d'
      in
      match newdata with
        None -> ()
      | Some d' ->
          T.StmtStartData.replace s d';
        ForwardWorklist.add worklist s

    (** Process a statement *)
    let processStmt worklist (s: stmt) : unit =
      CurrentLoc.set (Cil_datatype.Stmt.loc s);
      if T.debug then
        Kernel.debug "FF(%s).stmt %d at %t@\n" T.name s.sid Cil.pp_thisloc;

      (* It must be the case that the block has some data *)
      let init: T.t =
        try T.copy (T.StmtStartData.find s)
        with Not_found ->
          Kernel.fatal ~current:true
	    "FF(%s): processing block without data" T.name
      in

      (** See what the custom says *)
      match T.doStmt s init with
      | SDone  -> ()
      | (SDefault | SUse _) as act -> begin
          let curr = match act with
            | SDefault -> init
            | SUse d -> d
            | SDone -> assert false
          and do_succs state =
            List.iter (fun s' -> reachedStatement worklist s s' state) s.succs
          in

          CurrentLoc.set (Cil_datatype.Stmt.loc s);
          match s.skind with
            | Instr i ->
                CurrentLoc.set (Cil_datatype.Instr.loc i);
                let after = T.doInstr s i curr in
                do_succs after

            | UnspecifiedSequence _
            | Goto _ | Break _ | Continue _
            | TryExcept _ | TryFinally _
            | Loop _ | Return _ | Block _ ->
                do_succs curr

            | If (e, _, _, _) ->
                let thenGuard, elseGuard = T.doGuard s e curr in
                if thenGuard = GDefault && elseGuard = GDefault then
                  (* this is the common case *)
                  do_succs curr
                else begin
                  let doBranch succ guard =
                    match guard with
                      GDefault -> reachedStatement worklist s succ curr
                    | GUse d ->  reachedStatement worklist s succ d
                    | GUnreachable ->
                        if T.debug then
                          (Kernel.debug "FF(%s): Not exploring branch to %d\n"
                             T.name succ.sid)
                  in
                  let thenSucc, elseSucc = Cil.separate_if_succs s  in
                  doBranch thenSucc thenGuard;
                  doBranch elseSucc elseGuard;
                end

            | Switch (exp_sw, _, _, _) ->
                let cases, default = Cil.separate_switch_succs s in
                (* Auxiliary function that iters on all the labels of
                   the switch. The accumulator is the state after the
                   evaluation of the label, and the default case *)
                let iter_all_labels f =
                  List.fold_left
                    (fun rem_state succ ->
                      if rem_state = None then None
                      else
                        List.fold_left
                          (fun rem_state label ->
                            match rem_state with
                              | None -> rem_state
                              | Some state -> f succ label state
                          ) rem_state succ.labels
                    ) (Some curr) cases
                in
                (* Compute a successor of the switch, starting with the state
                   [before], supposing we are considering the label [exp] *)
                let explore_succ before succ exp_case =
                  let exp = match exp_case.enode with
		    (* This helps when switch is used on boolean expressions. *)
		    | Const (CInt64 (z,_,_)) 
                      when Integer.equal z Integer.zero ->
		        new_exp ~loc:exp_sw.eloc (UnOp(LNot,exp_sw,intType))
                    | _ ->
                        Cil.new_exp exp_case.eloc
                          (BinOp (Eq, exp_sw, exp_case, Cil.intType))
                  in
                  let branch_case, branch_not_case = T.doGuard s exp before in
                  (match branch_case with
		    | GDefault -> reachedStatement worklist s succ before;
                    | GUse d ->   reachedStatement worklist s succ d;
		    | GUnreachable ->
                        if T.debug then
                          Kernel.debug "FF(%s): Not exploring branch to %d\n"
                            T.name succ.sid;
                  );
                  (* State corresponding to the negation of [exp], to
                     be used for the remaining labels *)
                  match branch_not_case with
                    | GDefault -> Some before
                    | GUse d -> Some d
                    | GUnreachable -> None
                in
                (* Evaluate all of the labels one after the other, refining
                   the state after each case *)
                let after = iter_all_labels
                  (fun succ label before ->
                    match label with
                      | Label _ (* Label not related to the switch *)
		      | Cil_types.Default _ -> 	(* The default case is handled at the end *)
                        (Some before)

                      | Case (exp_case, _) ->
                        let after = explore_succ before succ exp_case in after

                  ) in
                (* If [after] is different from [None], we must evaluate
                   the default case, be it a default label, or the
                   successor of the switch *)
                (match after with
                  | None -> ()
                  | Some state -> reachedStatement worklist s default state)

      end


    (** Compute the data flow. *)
    let compute (sources: stmt list) =
      let kf = current_kf sources in
      let worklist = ForwardWorklist.create kf sources in

      List.iter (fun s -> ForwardWorklist.add worklist s) sources;

      (** All initial stmts must have non-bottom data *)
      List.iter
	(fun s ->
          if not (T.StmtStartData.mem s) then
            Kernel.fatal ~current:true
	      "FF(%s): initial stmt %d does not have data"
              T.name s.sid)
        sources;
      if T.debug then
        (Kernel.debug "FF(%s): processing" T.name);
      let rec fixedpoint () =
        if T.debug && not (ForwardWorklist.is_empty worklist) then
          (Kernel.debug "FF(%s): worklist= %a"
             T.name
             (Pretty_utils.pp_list (fun fmt s -> Format.fprintf fmt "%d" s.sid))
             (List.rev
                (ForwardWorklist.fold (fun s acc -> s :: acc) worklist [])));
        let s = ForwardWorklist.pop_next worklist in
        processStmt worklist s;
        fixedpoint ()
      in
      (try
         fixedpoint ()
       with ForwardWorklist.Empty ->
         if T.debug then
           (Kernel.debug "FF(%s): done" T.name))

  end


(******************************************************************
 **********
 **********         BACKWARDS
 **********
 ********************************************************************)

module type BackwardsTransfer = sig
  val name: string
  val debug: bool
  type t
  val pretty: Format.formatter -> t -> unit
  val funcExitData: t
  val combineStmtStartData: Cil_types.stmt -> old:t -> t -> t option
  val combineSuccessors: t -> t -> t
  val doStmt: stmt -> t action
  val doInstr: stmt -> instr -> t -> t action
  val filterStmt: stmt -> stmt -> bool

  module StmtStartData: StmtStartData with type data = t
  (** For each block id, the data at the start. This data structure must be
   * initialized with the initial data for each block *)
end

module Backwards(T : BackwardsTransfer) =
struct

    let getStmtStartData (s: stmt) : T.t =
      try T.StmtStartData.find s
      with Not_found ->
        Kernel.fatal ~current:true
	  "BF(%s): stmtStartData is not initialized for %d"
          T.name s.sid

    (** Process a statement and return true if the set of live return
     * addresses on its entry has changed. *)
    let processStmt (s: stmt) : bool =
      if T.debug then
        (Kernel.debug "FF(%s).stmt %d\n" T.name s.sid);


      (* Find the state before the branch *)
      CurrentLoc.set (Cil_datatype.Stmt.loc s);
      let d: T.t =
        match T.doStmt s with
           Done d -> d
         | (Default | Post _) as action -> begin
             (* Compute the default state, by combining the successors *)
             let res =
               (* We restrict ourselves to the successors we are interested in.
                  If T.filterStmt is deterministic, this should not make the
                  list empty if s.succs is not empty, as we would not have
                  reached s otherwise *)
               match List.filter (T.filterStmt s) s.succs with
               | [] -> T.funcExitData
               | fst :: rest ->
                   List.fold_left (fun acc succ ->
                     T.combineSuccessors acc (getStmtStartData succ))
                     (getStmtStartData fst)
                     rest
             in
             (* Now do the instructions *)
             let res' =
               match s.skind with
                 | Instr i ->
                   begin
                     CurrentLoc.set (Cil_datatype.Instr.loc i);
                     let action = T.doInstr s i res in
                     match action with
                     | Done s' -> s'
                     | Default -> res (* do nothing *)
                     | Post f -> f res
                   end
               | _ -> res
             in
             match action with
               Post f -> f res'
             | _ -> res'
         end
      in

      (* See if the state has changed. The only changes are that it may grow.*)
      let s0 = getStmtStartData s in

      match T.combineStmtStartData s ~old:s0 d with
        None -> (* The old data is good enough *)
          false

      | Some d' ->
          (* We have changed the data *)
          if T.debug then
            Kernel.debug "BF(%s): set data for block %d: %a\n"
              T.name s.sid T.pretty d';
          T.StmtStartData.replace s d';
          true

    (** Compute the data flow. Must have the CFG initialized *)
    let compute (sinks: stmt list) =
      let kf = current_kf sinks in
      let worklist = BackwardWorklist.create kf sinks in
      List.iter (fun s -> BackwardWorklist.add worklist s) sinks;
      if T.debug && not (BackwardWorklist.is_empty worklist) then
        (Kernel.debug "\nBF(%s): processing\n"
                  T.name);
      let rec fixedpoint () =
        if T.debug &&  not (BackwardWorklist.is_empty worklist) then
          (Kernel.debug "BF(%s): worklist= %a\n"
                    T.name
                    (Pretty_utils.pp_list (fun fmt s -> Format.fprintf fmt "%d " s.sid))
                    (List.rev
                       (BackwardWorklist.fold (fun s acc -> s :: acc)  worklist [])));
          let s = BackwardWorklist.pop_next worklist in
          let changes = processStmt s in
          if changes then begin
            (* We must add all predecessors of block b, only if not already
             * in and if the filter accepts them. *)
            List.iter
	      (fun p -> if T.filterStmt p s then BackwardWorklist.add worklist p)
              s.preds;
          end;
          fixedpoint ()
      in
      try
        fixedpoint ()
      with BackwardWorklist.Empty ->
        if T.debug then
          (Kernel.debug "BF(%s): done\n\n" T.name)
  end


(** Helper utility that finds all of the statements of a function.
  It also lists the return statments (including statements that
  fall through the end of a void function).  Useful when you need an
  initial set of statements for BackwardsDataFlow.compute. *)
let sinkFinder sink_stmts all_stmts = object
  inherit nopCilVisitor

  method! vstmt s =
    all_stmts := s ::(!all_stmts);
    match s.succs with
      [] -> (sink_stmts := s :: (!sink_stmts);
	     DoChildren)
    | _ -> DoChildren

end

(* returns (all_stmts, return_stmts).   *)
let find_stmts (fdec:fundec) : (stmt list * stmt list) =
  let sink_stmts = ref []
  and all_stmts = ref [] in
  ignore(visitCilFunction (sinkFinder sink_stmts all_stmts) fdec);
  !all_stmts, !sink_stmts


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
