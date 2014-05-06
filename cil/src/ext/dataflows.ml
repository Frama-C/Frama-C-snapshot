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

let dkey = Kernel.register_category "dataflows"

(* This file defines a set of dataflow frameworks. Instead of defining
   a single dataflow interface that tries to accomodate with all the
   options, having a set of dataflows allow to keep things simple in
   the general case; specific demands are handled by using more
   general dataflows. Simpler-to-instanciate dataflows are instances
   of the more general dataflows. *)

open Ordered_stmt;;
open Cil_types;;

(****************************************************************)

(* Environment relative to the function being processed. *)
module type FUNCTION_ENV = sig
  val to_ordered: stmt -> ordered_stmt
  val to_stmt: ordered_stmt -> stmt
  val nb_stmts: int
  val kf: Kernel_function.t
end

let function_env kf =
  (module struct
    let (order,unorder,_connex) = Ordered_stmt.get_conversion_tables kf;;
    let nb_stmts = Array.length unorder;;
    let to_stmt ordered = Ordered_stmt.to_stmt unorder ordered;;
    let to_ordered stmt = Ordered_stmt.to_ordered order stmt;;
    let kf = kf
  end : FUNCTION_ENV)

(****************************************************************)
(* Worklists. *)

(* TODO:
   - Retrieve the efficient worklists from Dataflow2.
*)
module type WORKLIST = sig

  (** Add a statement to the worklist. The statement can already be in
      the worklist; in this case it will not appear twice in the
      worklist (i.e. the worklist is a set, not a list). *)
  val insert: ordered_stmt -> unit

  (** Retrieve and remove the next element of the worklist. Returns
      [None] if the worklist is empty. *)
  val extract: unit -> ordered_stmt option
end

(* Worklist for a "rapid" framework. Just iterate over all statements
   until none has changed. *)
module Rapid_forward_worklist(Fenv:FUNCTION_ENV):WORKLIST = struct

  type t = { mutable changed: bool;
	     mutable current_index: ordered_stmt; }
    ;;

  let w = { changed = false; current_index = Fenv.nb_stmts } ;;
  let insert _ord = w.changed <- true ;;
  let extract () =
    if w.current_index >= Fenv.nb_stmts - 1
    then
      if w.changed
      then
	(w.changed <- false;
	 w.current_index <- 0;
	 Some 0)
      else
	None
    else
      (w.current_index <- w.current_index + 1;
       Some w.current_index)
  ;;
end

(* Iterates on all statements in order, but do something only on those
   for which there is a pending change. *)
module Simple_forward_worklist(Fenv:FUNCTION_ENV):WORKLIST = struct

  (* The worklist, and the current index. *)
  type t = { bv: Bitvector.t;
	     mutable index: int };;

  let w =
    let bv = Bitvector.create Fenv.nb_stmts in
    let first =  Fenv.to_ordered (Kernel_function.find_first_stmt Fenv.kf) in
    {bv; index=first}
  ;;

  let insert ord = Bitvector.set w.bv ord;;

  let extract () =
    try let next = Bitvector.find_next_true w.bv w.index in
	Bitvector.clear w.bv next;
	w.index <- next;
	Some next
    with Not_found ->
      (* Try to start over. *)
      try let next = Bitvector.find_next_true w.bv 0 in
	  Bitvector.clear w.bv next;
	  w.index <- next;
	  Some next
      (* Nothing to do left. *)
      with Not_found -> None
end
;;

(****************************************************************)
(* Influence describes how a change in the value attached to a
   statement may influence the value of other statements. In forward
   dataflow, a statement can influence the following statements, while
   in backward dataflow, it influences the previous ones. *)
module type INFLUENCE = sig
  val influence: ordered_stmt -> ordered_stmt list
end

module Forward_influence(Fenv:FUNCTION_ENV):INFLUENCE = struct
  let influence ord =
    let stmt = Fenv.to_stmt ord in
    List.map Fenv.to_ordered stmt.succs
end

module Backward_influence(Fenv:FUNCTION_ENV):INFLUENCE = struct
  let influence ord =
    let stmt = Fenv.to_stmt ord in
    List.map Fenv.to_ordered stmt.preds
end

(****************************************************************)
(* The worklist algorithm, that iterates until there is no more work
   left to do. This is the one described in Principles of program
   analysis, by Nielson, Nielson and Hankin, page 369. *)
module type ITERATE = sig
  (** [iter f] executes f on all statements until there is no more
      work to do. The [f ord] function must return [true] if something
      has changed for statement [ord], and [false] otherwise. *)
  val iter: (ordered_stmt -> bool) -> unit
end

(* Note: This is not used by the forward dataflow, that records and
   updates worklists based on edges between statements. But this could
   still be useful for the backward dataflow, or for a simpler forward
   dataflow that would not handle guards specially (like the dominator
   computation). *)
module Iterate(Inf:INFLUENCE)(W:WORKLIST):ITERATE = struct
  (* Iterates over the worklist until there is nothing to do left.
     When the data for the current stmt has changed, update the
     worklist to recompute stmts influenced by the current stmt. *)
  let iter f =
    let rec loop () =
      match W.extract() with
      | None -> ()
      | Some(x) ->
	let changed = f x in
	if changed
	then List.iter W.insert (Inf.influence x)
	else ();
	loop()
    in loop()
end

(* Default simple instanciations for forward and backward
   dataflows. This is what everybody should use for direct
   iteration. *)
module Forward_Iterate(Fenv:FUNCTION_ENV):ITERATE =
  Iterate(Forward_influence(Fenv))(Simple_forward_worklist(Fenv))
;;

(* Note: usages of this functor: for the dominator/postdominator
   computation; for "bit-framework" based analysis; for the backward
   dataflow... *)

(****************************************************************)
(* Monotone Framework (see Nielson, Nielson, Hankin) *)

module type JOIN_SEMILATTICE = sig
  type t

  (* Must be idempotent (join a a = a), commutative, and associative. *)
  val join: t -> t -> t

  (* Must verify that forall a, join a bottom = a. *)
  val bottom: t

  (* Must verify: a is_included b <=> a join b = b. The dataflow does
     not require this function. *)
  (* val is_included: t -> t -> bool *)

  (* This function is used by the dataflow algorithm to determine if
     something has to be recomputed. Joining and inclusion testing are
     similar operations, so it is often more efficient to do both at
     the same time (e.g. when joining with bottom). Note that the
     names [smaller] and [larger] are actually correct only if there
     is an inclusion.

     Instead of defining it directly, it can be defined from join and
     equal, or from is_included, for instance by
     [if is_included new old then (true,old) else (false, join old new)] or
     [let j = join old new in (equal j new, j)]. *)
  val join_and_is_included: t -> t -> (t * bool)

  (* Display the contents of an element of the lattice. *)
  val pretty: Format.formatter -> t -> unit

end

module CurrentLoc = Cil_const.CurrentLoc;;


(****************************************************************)

(* Edge-based forward dataflow. It is edge-based because the transfer
   function can differentiate the state after a statement between
   different successors. In particular, the state can be reduced
   according to the conditions in if statements. *)
module type FORWARD_MONOTONE_PARAMETER_GENERIC_STORAGE = sig
  include JOIN_SEMILATTICE

  (* [transfer_stmt s state] must returns a list of pairs in which the
     first element is a statement [s'] in [s.succs], and the second
     element a value that will be joined with the current result for
     before [s'].

     Note that it is allowed that not all succs are present in the
     list returned by [transfer_stmt], or that succs are present several
     times (this is useful to handle switchs). *)
  val transfer_stmt: stmt -> t -> (stmt * t) list

  (* These functions explain how we store the state associated to each
     statement (the state before the statement). *)
  val get_before: ordered_stmt -> t
  val set_before: ordered_stmt -> t -> unit

  (* The initial value for each statement. Statements in this list are
     given the associated value, and are added to the worklist. Other
     statements are initialized to bottom. *)
  val init: (stmt * t) list

end

module Forward_monotone_generic_storage
  (Fenv:FUNCTION_ENV)(P:FORWARD_MONOTONE_PARAMETER_GENERIC_STORAGE)(W:WORKLIST) =
struct
  List.iter (fun (stmt,state) ->
    let ord = Fenv.to_ordered stmt in
    P.set_before ord state;
    W.insert ord) P.init;;

  let update_before (stmt, new_state) =
    let ord = Fenv.to_ordered stmt in
    (* TODO: if we know that we already have to recompute
       before.(ord), we can omit the inclusion testing, and only
       perform the join. We only need to query the worklist. *)
    CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
    let (join, is_included) =
      P.join_and_is_included new_state (P.get_before ord)
    in
    if not is_included then W.insert ord;
    P.set_before ord join
  ;;

  let do_stmt ord =
    let cur_state = P.get_before ord  in
    let stmt = Fenv.to_stmt ord in
    Kernel.debug ~dkey "doing stmt %d" stmt.sid;
    CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
    let l = P.transfer_stmt stmt cur_state in
    List.iter update_before l
  ;;

  (* Performs the fixpoint computation; the result is in [before]. *)
  let rec compute() =
    match W.extract() with
    | None -> ()
    | Some(ord) -> do_stmt ord; compute()
  in compute()
  ;;

  (* Easy access to the result of computation. *)
  let fold_on_result f init =
    let rec loop acc = function
      | i when i = Fenv.nb_stmts -> acc
      | i -> let acc = f acc (Fenv.to_stmt i) (P.get_before i)
	     in loop acc (i+1)
    in loop init 0;;

  let iter_on_result f =
    for i = 0 to (Fenv.nb_stmts - 1) do
      f (Fenv.to_stmt i) (P.get_before i)
    done;;

end

module Simple_forward_generic_storage(Fenv:FUNCTION_ENV)(P:FORWARD_MONOTONE_PARAMETER_GENERIC_STORAGE) =
  Forward_monotone_generic_storage(Fenv)(P)(Simple_forward_worklist(Fenv));;

(****************************************************************)
(* Edge-based forward dataflow with array-based storage. Should be
   used for most applications of the forward dataflow. *)
module type FORWARD_MONOTONE_PARAMETER = sig
  include JOIN_SEMILATTICE

  (* [transfer_stmt s state] must returns a list of pairs in which the
     first element is a statement [s'] in [s.succs], and the second
     element a value that will be joined with the current result for
     before [s'].

     Note that it is allowed that not all succs are present in the
     list returned by [transfer_stmt], or that succs are present several
     times (this is useful to handle switchs). *)
  val transfer_stmt: stmt -> t -> (stmt * t) list

  (* The initial value for each statement. Statements in this list are
     given the associated value, and are added to the worklist. Other
     statements are initialized to bottom. *)
  val init: (stmt * t) list
end

module Simple_forward(Fenv:FUNCTION_ENV)(P:FORWARD_MONOTONE_PARAMETER) = struct
  module W = Simple_forward_worklist(Fenv);;

  module P_array = struct
    include P
    let before = Array.make Fenv.nb_stmts P.bottom;;
    List.iter (fun (stmt,state) ->
      let ord = Fenv.to_ordered stmt in
      before.(ord) <- state;
      W.insert ord) P.init;;

    let get_before ord = before.(ord);;
    let set_before ord value = before.(ord) <- value;;
  end

  include Forward_monotone_generic_storage(Fenv)(P_array)(W);;
  let before = P_array.before;;
end

(****************************************************************)
(* Helper functions for implementing [transfer_stmt]. *)

(* The following functions allow implementing [transfer_stmt] for the
   [If] and [Switch] instruction, from a [transfer_guard] function. *)
let transfer_if_from_guard transfer_guard stmt state =
  let exp = match stmt.skind with
    | If(exp,_,_,_) -> exp
    | _ ->
      Kernel.fatal ~current:true "transfer_if_from_guard on a non-If statement."
  in
  let (then_state, else_state) = transfer_guard stmt exp state in
  let (then_stmt, else_stmt) = Cil.separate_if_succs stmt in
  [(then_stmt,then_state); (else_stmt, else_state)]
;;

let transfer_switch_from_guard transfer_guard stmt state =
  let cond = match stmt.skind with
    | Switch( cond, _, _, _) -> cond
    | _ ->
      Kernel.fatal ~current:true
	"transfer_switch_from_guard on a non-Switch statement."
  in

  let cases, default = Cil.separate_switch_succs stmt in
  let result = ref [] in
  (* We fold on the cases; the accumulator contains the state when
     the label is not taken. *)
  (* Note: we could early-exit the handling of the switch if we can
     detect that the false_state is bottom.  *)
  let do_one_case input_state succ =
    let do_one_label input_state label =
      match label with
      (* We do nothing for Default, because we handle it last. *)
      | Label _ | Default _ -> input_state
      | Case (exp_case, _) ->
	let if_equivalent_cond =
	  match exp_case.enode with
	  (* This helps when switch is used on boolean expressions. *)
	  | Const (CInt64 (z,_,_))
              when Integer.equal z Integer.zero ->
	    Cil.new_exp ~loc:cond.eloc (UnOp(LNot,cond,Cil.intType))
	  | _ -> Cil.new_exp exp_case.eloc
            (BinOp (Eq, cond, exp_case, Cil.intType))
	in
	let (true_state, false_state) =
	  transfer_guard stmt if_equivalent_cond input_state
	in
	result := (succ, true_state)::!result;
	false_state
    in
    List.fold_left do_one_label input_state succ.labels
  in
  (* We handle the default case last, so that we may benefit from the
     reduction of the successive guards. *)
  let final_state = List.fold_left do_one_case state cases in
  (default,final_state)::!result
;;

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
