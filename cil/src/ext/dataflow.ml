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

(*module E = Errormsg*)
open Cil_types
open Cil
(* open Pretty *)

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


exception True
let qexists f q =
  try
    Queue.iter (fun v -> if f v then raise True) q;
    false
  with True -> true


(******************************************************************
 **********
 **********         FORWARDS
 **********
 ********************************************************************)

module type ForwardsTransferAux = sig
  val name: string (** For debugging purposes, the name of the analysis *)
  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. May be
           * imperative.  *)

  val copy: t -> t
  (** Make a deep copy of the data *)

  val pretty: Format.formatter -> t -> unit
  (** Pretty-print the state *)

  val computeFirstPredecessor: stmt -> t -> t
  (** Give the first value for a predecessors, compute the value to be set
   * for the block *)

  val combinePredecessors: stmt -> old:t -> t -> t option
  (** Take some old data for the start of a statement, and some new data for
   * the same point. Return None if the combination is identical to the old
   * data. Otherwise, compute the combination, and return it. *)

  val doInstr: stmt -> instr -> t -> t action
  (** The (forwards) transfer function for an instruction. The
   * {!Cil.currentLoc} is set before calling this. The default action is to
   * continue with the state unchanged.
   * [stmt] is the englobing statement *)

 val doGuard: stmt -> exp -> t -> t guardaction * t guardaction
  (** Generate the successors [th, el] to an
    *  If statement assuming the given expression
    * is respectively nonzero and zero.
    * Analyses that don't need guard information can return
    * GDefault, GDefault; this is equivalent to returning GUse of the input.
    * A return value of GUnreachable indicates that this half of the branch
    * will not be taken and should not be explored.  This will be called
    * once per If.
    * [stmt] is the corresponding [If] statement FYI only.
    *)

  val doStmt: stmt -> t -> t stmtaction
  (** The (forwards) transfer function for a statement. The {!Cil.currentLoc}
   * is set before calling this. The default action is to continue with the
   * successors of this block, but only for the ... statements. For other
   * kinds of branches you must handle it, and return {!Dataflow.Done}. *)

  val filterStmt: stmt -> bool
  (** Whether to put this statement in the worklist. This is called when a
   * block would normally be put in the worklist. *)

  val stmt_can_reach : stmt -> stmt -> bool

  val doEdge: stmt -> stmt -> t -> t
    (** what to do when following the edge between the two given statements.
        Can default to identity if nothing special is required.
     *)
end

module type ForwardsTransfer = sig
  include ForwardsTransferAux

  module StmtStartData: StmtStartData with type data = t
  (** For each statement id, the data at the start. Not found in the hash
   * table means nothing is known about the state at this point. At the end
   * of the analysis this means that the block is not reachable. *)

end

module Forwards(T : ForwardsTransfer) = struct

    (** Keep a worklist of statements to process. It is best to keep a queue,
     * because this way it is more likely that we are going to process all
     * predecessors of a statement before the statement itself. *)
    let worklist: stmt Queue.t = Queue.create ()



    (** We call this function when we have encountered a statement, with some
     * state. *)
    let reachedStatement pred (s: stmt) (d: T.t) : unit =
      (** see if we know about it already *)
      let d = T.doEdge pred s d in
      let newdata: T.t option =
        try
          let old = T.StmtStartData.find s in
          match T.combinePredecessors s ~old:old d with
            None -> (* We are done here *)
              if !T.debug then
                Kernel.debug "FF(%s): reached stmt %d with %a\n  implies the old state %a\n"
                  T.name s.sid T.pretty d T.pretty old;
              None
          | Some d' -> begin
              (* We have changed the data *)
              if !T.debug then
                Kernel.debug "FF(%s): weaken data for block %d: %a\n"
                  T.name s.sid T.pretty d';
              Some d'
          end
        with Not_found -> (* was bottom before *)
          let d' = T.computeFirstPredecessor s d in
          if !T.debug then
            Kernel.debug "FF(%s): set data for block %d: %a\n"
              T.name s.sid T.pretty d';
          Some d'
      in
      match newdata with
        None -> ()
      | Some d' ->
          T.StmtStartData.replace s d';
          if T.filterStmt s &&
            not (Queue.fold (fun exists s' -> exists || s'.sid = s.sid)
                            false
                            worklist) then
            Queue.add s worklist


    (** Get the two successors of an If statement *)
    let ifSuccs (s:stmt) : stmt * stmt =
      let fstStmt blk = match blk.bstmts with
          [] -> Cil.dummyStmt
        | fst::_ -> fst
      in
      match s.skind with
        If(_e, b1, b2, _) ->
          let thenSucc = fstStmt b1 in
          let elseSucc = fstStmt b2 in
          let oneFallthrough () =
            let fallthrough =
              List.filter
                (fun s' -> thenSucc != s' && elseSucc != s')
                s.succs
            in
            match fallthrough with
              [] ->
		Kernel.fatal ~current:true
		  "Bad CFG: missing fallthrough for If."
            | [s'] -> s'
            | _ ->
	      Kernel.fatal ~current:true "Bad CFG: multiple fallthrough for If."
          in
          (* If thenSucc or elseSucc is Cil.dummyStmt, it's an empty block.
             So the successor is the statement after the if *)
          let stmtOrFallthrough s' =
            if s' == Cil.dummyStmt then
              oneFallthrough ()
            else
              s'
          in
          (stmtOrFallthrough thenSucc,
           stmtOrFallthrough elseSucc)

      | _-> Kernel.fatal ~current:true "ifSuccs on a non-If Statement."

    (** Process a statement *)
    let processStmt (s: stmt) : unit =
      CurrentLoc.set (Cil_datatype.Stmt.loc s);
      if !T.debug then
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
            List.iter (fun s' -> reachedStatement s s' state) s.succs
          in

          CurrentLoc.set (Cil_datatype.Stmt.loc s);
          match s.skind with
            | Instr i ->
                CurrentLoc.set (Cil_datatype.Instr.loc i);
                let action = T.doInstr s i curr in
                let after = match action with
                  | Done s' -> s'
                  | Default -> curr (* do nothing *)
                  | Post f -> f curr
                in
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
                      GDefault -> reachedStatement s succ curr
                    | GUse d ->  reachedStatement s succ d
                    | GUnreachable ->
                        if !T.debug then
                          (Kernel.debug "FF(%s): Not exploring branch to %d\n"
                             T.name succ.sid)
                  in
                  let thenSucc, elseSucc = ifSuccs s  in
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
		    | Const (CInt64 (z,_,_)) 
                      when Integer.equal z Integer.zero ->
		        new_exp ~loc:exp_sw.eloc (UnOp(LNot,exp_sw,intType))
                    | _ ->
                        Cil.new_exp exp_case.eloc
                          (BinOp (Eq, exp_sw, exp_case, Cil.intType))
                  in
                  let branch_case, branch_not_case = T.doGuard s exp before in
                  (match branch_case with
		    | GDefault -> reachedStatement s succ before;
                    | GUse d ->   reachedStatement s succ d;
		    | GUnreachable ->
                        if !T.debug then
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
                  | Some state -> reachedStatement s default state)
      end

    exception Good of stmt

    let find_next_in_queue worklist =
      let nok_queue = Queue.create () in
      try
        while true do
          let s = Queue.take worklist in
          if
            (let nb_preds = List.length s.preds in
             nb_preds > 1
             || (nb_preds = 1 && List.length (List.hd s.preds).succs > 1))
            &&
              qexists (fun v -> T.stmt_can_reach v s &&
                         not (T.stmt_can_reach s v))
              worklist
          then ((* prerr_endline "REORDER\n" ; *) Queue.add s nok_queue)
          else raise (Good s)
        done;
        assert false
      with
        | Not_found ->
            assert false (*; (* the relation
                           "stmt_can_reach v s && not (stmt_can_reach s v)"
                           is a partial order so this shouldn't happen *)
                           let r = Queue.take nok_queue in
                           Queue.transfer nok_queue worklist;
                           r *)
        | Good r ->
            Queue.transfer nok_queue worklist;
            r

    (** Compute the data flow. Must have the CFG initialized *)
    let compute (sources: stmt list) =
      Queue.clear worklist;
      List.iter (fun s -> Queue.add s worklist) sources;

      (** All initial stmts must have non-bottom data *)
      List.iter
	(fun s ->
          if not (T.StmtStartData.mem s) then
            Kernel.fatal ~current:true
	      "FF(%s): initial stmt %d does not have data"
              T.name s.sid)
        sources;
      if !T.debug then
        (Kernel.debug "FF(%s): processing" T.name);
      let rec fixedpoint () =
        if !T.debug && not (Queue.is_empty worklist) then
          (Kernel.debug "FF(%s): worklist= %a"
             T.name
             (Pretty_utils.pp_list (fun fmt s -> Format.fprintf fmt "%d" s.sid))
             (List.rev
                (Queue.fold (fun acc s -> s :: acc) [] worklist)));
        let s = find_next_in_queue worklist in
        processStmt s;
        fixedpoint ()
      in
      (try
         fixedpoint ()
       with Queue.Empty ->
         if !T.debug then
           (Kernel.debug "FF(%s): done" T.name))

  end


(******************************************************************
 **********
 **********         BACKWARDS
 **********
 ********************************************************************)
module type BackwardsTransferAux = sig
  val name: string (* For debugging purposes, the name of the analysis *)

  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. In many
           * presentations of backwards data flow analysis we maintain the
           * data at the block end. This is not easy to do with JVML because
           * a block has many exceptional ends. So we maintain the data for
           * the statement start. *)

  val pretty: Format.formatter -> t -> unit (** Pretty-print the state *)

  val funcExitData: t
  (** The data at function exit.  Used for statements with no successors.
      This is usually bottom, since we'll also use doStmt on Return
      statements. *)

  val combineStmtStartData: Cil_types.stmt -> old:t -> t -> t option

  (** When the analysis reaches the start of a block, combine the old data
   * with the one we have just computed. Return None if the combination is
   * the same as the old data, otherwise return the combination. In the
   * latter case, the predecessors of the statement are put on the working
   * list. *)


  val combineSuccessors: t -> t -> t
  (** Take the data from two successors and combine it *)


  val doStmt: stmt -> t action
  (** The (backwards) transfer function for a branch. The {!Cil.CurrentLoc} is
   * set before calling this. If it returns None, then we have some default
   * handling. Otherwise, the returned data is the data before the branch
   * (not considering the exception handlers) *)

  val doInstr: stmt -> instr -> t -> t action
  (** The (backwards) transfer function for an instruction. The
   * {!Cil.CurrentLoc} is set before calling this. If it returns None, then we
   * have some default handling. Otherwise, the returned data is the data
   * before the branch (not considering the exception handlers) *)

  val filterStmt: stmt -> stmt -> bool
  (** Whether to put this predecessor block in the worklist. We give the
   * predecessor and the block whose predecessor we are (and whose data has
   * changed)  *)

  val stmt_can_reach : stmt -> stmt -> bool

end

module type BackwardsTransfer = sig
  include BackwardsTransferAux

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
      if !T.debug then
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
          if !T.debug then
            Kernel.debug "BF(%s): set data for block %d: %a\n"
              T.name s.sid T.pretty d';
          T.StmtStartData.replace s d';
          true

    exception Good of stmt
    (* This function is the exact dual to the one in the forward dataflow *)
    let find_next_in_queue worklist =
      let nok_queue = Queue.create () in
      try
        while true do
          let s = Queue.take worklist in
          if
            (let nb_succs = List.length s.succs in
             nb_succs > 1
             || (nb_succs = 1 && List.length (List.hd s.succs).preds > 1))
            &&
              qexists (fun v -> T.stmt_can_reach s v &&
                         not (T.stmt_can_reach v s))
              worklist
          then Queue.add s nok_queue
          else raise (Good s)
        done;
        assert false
      with
        | Not_found ->
            assert false
        | Good r ->
            Queue.transfer nok_queue worklist;
            r


    (** Compute the data flow. Must have the CFG initialized *)
    let compute (sinks: stmt list) =
      let worklist: stmt Queue.t = Queue.create () in
      List.iter (fun s -> Queue.add s worklist) sinks;
      if !T.debug && not (Queue.is_empty worklist) then
        (Kernel.debug "\nBF(%s): processing\n"
                  T.name);
      let rec fixedpoint () =
        if !T.debug &&  not (Queue.is_empty worklist) then
          (Kernel.debug "BF(%s): worklist= %a\n"
                    T.name
                    (Pretty_utils.pp_list (fun fmt s -> Format.fprintf fmt "%d " s.sid))
                    (List.rev
                       (Queue.fold (fun acc s -> s :: acc) [] worklist)));
          let s = find_next_in_queue worklist in
          let changes = processStmt s in
          if changes then begin
            (* We must add all predecessors of block b, only if not already
             * in and if the filter accepts them. *)
            List.iter
              (fun p ->
                if T.filterStmt p s
                  && (try
                        Queue.iter (fun s' -> if p.sid = s'.sid then raise Exit) worklist;
                        true
                      with Exit -> false)
                then
                  Queue.add p worklist)
              s.preds;
          end;
          fixedpoint ()
      in
      try
        fixedpoint ()
      with Queue.Empty ->
        if !T.debug then
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
