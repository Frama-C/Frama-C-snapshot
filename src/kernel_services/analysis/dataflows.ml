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

open Ordered_stmt;;
open Cil_types;;

(****************************************************************)

(* Environment relative to the function being processed. *)
module type FUNCTION_ENV = sig
  val to_ordered: stmt -> ordered_stmt
  val to_stmt: ordered_stmt -> stmt
  val connected_component: ordered_stmt -> int
  val nb_stmts: int
  val kf: Kernel_function.t
end

let function_env kf =
  (module struct
    let (order,unorder,connex) = Ordered_stmt.get_conversion_tables kf;;
    let nb_stmts = Array.length unorder;;
    let to_stmt ordered = Ordered_stmt.to_stmt unorder ordered;;
    let to_ordered stmt = Ordered_stmt.to_ordered order stmt;;
    let connected_component ord = connex.(ord)
    let kf = kf
  end : FUNCTION_ENV)

(****************************************************************)
(* Worklists. *)

module type WORKLIST = sig

  (** Add a statement to the worklist. The statement can already be in
      the worklist; in this case it will not appear twice in the
      worklist (i.e. the worklist is a set, not a list). *)
  val insert: ordered_stmt -> unit

  (** Retrieve and remove the next element of the worklist. Returns
      [None] if the worklist is empty. *)
  val extract: unit -> ordered_stmt option
end

module type CONSULTABLE_WORKLIST = sig
  include WORKLIST

  (** [in_worklist x] returns true if it is guaranteed that a further
      call to [extract()] will return [x] (Thus it is safe for a
      worklist implementation to always return false here). *)
  val in_worklist: ordered_stmt -> bool

end

(* Worklist for a "rapid" framework. Just iterate over all statements
   until none has changed. *)
module Rapid_forward_worklist(Fenv:FUNCTION_ENV):CONSULTABLE_WORKLIST = struct

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

  let in_worklist ord = w.changed || ord > w.current_index

end

(* Iterates on all statements in order, but do something only on those
   for which there is a pending change. *)
module Simple_forward_worklist(Fenv:FUNCTION_ENV):CONSULTABLE_WORKLIST = struct

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

  let in_worklist ord = Bitvector.mem w.bv ord
end
;;

type direction = Forward | Backward;;

(* Iterate over statements by strongly connected components: i.e. do
   not leave a scc if there is still work to do in this scc. All
   statements inside a scc are handled before starting over on that
   scc. Iteration is done using the topological order of sccs. *)
module Connected_component_worklist
  (Dir:sig val direction:direction end)
  (Fenv:FUNCTION_ENV)
  :CONSULTABLE_WORKLIST =
struct

  (** Workqueue, implemented as a bit vector. Because the
      [find_next_true] operation only operates in ascending indices,
      we need to put statements in the reverse order for the backward
      dataflow. *)
  module Workqueue:sig
    val clear: ordered_stmt -> unit
    val set: ordered_stmt -> unit
    val mem: ordered_stmt -> bool
    val find_next_true: ordered_stmt -> ordered_stmt
  end = struct
    let rev = match Dir.direction with
      | Forward -> fun x -> x
      | Backward -> fun x -> (Fenv.nb_stmts - 1) - x
    ;;
    let bv = Bitvector.create Fenv.nb_stmts
    let clear i = Bitvector.clear bv (rev i)
    let set i = Bitvector.set bv (rev i)
    let mem i = Bitvector.mem bv (rev i)
    let find_next_true current = rev (Bitvector.find_next_true bv (rev current))
  end

  (* Forward iteration follows topological order, while backward
     iteration follows reverse topological order. Further, nearer etc.
     reflects topological distance to the first node. *)
  let first = match Dir.direction with
    | Forward -> 0
    | Backward -> Fenv.nb_stmts - 1

  let get_next = match Dir.direction with
    | Forward -> fun x -> x + 1
    | Backward -> fun x -> x - 1

  let is_further = match Dir.direction with
    | Forward -> (>=)
    | Backward -> (<=)

  let is_strictly_nearer = match Dir.direction with
    | Forward -> (<)
    | Backward -> (>)

  let nearest a b = match Dir.direction with
    | Forward -> min a b
    | Backward -> max a b


  (* Next statement to be retrieved. *)
  let next = ref first

    (* The current strongly connected component. Set it initially to
       the one of [next] so that extraction directly returns the
       initial [next]. *)
  let current_scc = ref (Fenv.connected_component !next);;

  Kernel.debug ~dkey "First statement %d, first scc %d" !next !current_scc;;

    (* We normally iterate using the ordered_stmt order. The only
       exception is when we have to restart iteration on the current
       strongly connected component. If this is the case,
       must_restart_cc is set to [Some(x)], where [x] is the first
       statement to be processed when we restart iterating on the
       current scc. *)
  let must_restart_scc = ref None

  let insert ord =
    (* We always iterate in topological order or stay in same connected component order. *)
    assert ((is_further ord !next)
	    || (Fenv.connected_component ord) = !current_scc);
    Workqueue.set ord;
    if is_strictly_nearer ord !next
    then must_restart_scc := match !must_restart_scc with
    | None -> Some ord
    | Some(x) -> Some(nearest ord x)

  let extract () =

    (* Note: these functions are called in tail position, and should
       be optimized as jumps. *)

    (* Remove i from the worklist, set up next for the next call, and return i.  *)
    let select i =
      Kernel.debug ~dkey "Selecting %d" i;
      Workqueue.clear i;
      next := get_next i;
      Some i
    in

    (* We reached the end of the current scc, and we need to further iterate on it. *)
    let select_restart_scc i =
      Kernel.debug ~dkey "Restarting to %d in same scc %d (current_scc = %d)"
	i (Fenv.connected_component i) !current_scc;
      assert((Fenv.connected_component i) == !current_scc);
      must_restart_scc := None;
      select i
    in

    (* We reached the end of the current scc, and we can switch to the next. *)
    let select_new_scc i =
      Kernel.debug ~dkey "Changing to %d in scc %d  (current_scc = %d)"
	i (Fenv.connected_component i) !current_scc;
      assert((Fenv.connected_component i) != !current_scc);
      current_scc := Fenv.connected_component i;
      must_restart_scc := None;
      select i
    in

    (* We did not reach the end of the current scc. *)
    let select_same_scc i =
      Kernel.debug ~dkey "Continuing to %d in scc %d  (current_scc = %d)"
	i (Fenv.connected_component i) !current_scc;
      assert((Fenv.connected_component i) == !current_scc);
      select i
    in

    try
      let next_true = Workqueue.find_next_true !next in
      let next_true_scc = Fenv.connected_component next_true in
      if next_true_scc = !current_scc
      then
	select_same_scc next_true
      else
	(* We reached the end of the current connected
           component. The trick is that OCamlgraph's topological
           ordering guarantees that elements of the same connected
           component have contiguous indexes, so we know that we
           have reached the end of the current scc. Check if we
           should start over in the same scc, or continue to the
           next scc. *)
	match !must_restart_scc with
	| None -> select_new_scc next_true
	| Some(i) -> select_restart_scc i
    with Not_found ->
      (* We found no further statement with work to do, but the
	 current scc may still contain some work. *)
      match !must_restart_scc with
      | None -> None
      | Some(i) -> select_restart_scc i
  ;;

  let in_worklist ord = Workqueue.mem ord

end

module Forward_connected_component_worklist =
  Connected_component_worklist(struct let direction = Forward end)
;;

module Backward_connected_component_worklist =
  Connected_component_worklist(struct let direction = Backward end)
;;

(****************************************************************)
(* Monotone Framework (see Nielson, Nielson, Hankin) *)

module type JOIN_SEMILATTICE = sig
  type t

  (* Must be idempotent ([join a a = a]), commutative, and associative. *)
  val join: t -> t -> t

  (* Must verify that [join a bottom = a]. *)
  val bottom: t

  (* Must verify: [is_included a b <=> join a b = b]. The dataflow does
     not require this function. *)
  val is_included: t -> t -> bool

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

(* Statement-based backward dataflow. Contrary to the forward dataflow,
   the transfer function cannot differentiate the state before a
   statement between different predecessors. *)
module type BACKWARD_MONOTONE_PARAMETER = sig
  include JOIN_SEMILATTICE

  (* [transfer_stmt s state] must implement the transfer function for [s]. *)
  val transfer_stmt: stmt -> t -> t

  (* The initial value for each statement. Statements in this list are
     given the associated value, and are added to the worklist. Other
     statements are initialized to bottom. *)
  val init: (stmt * t) list
end

module Simple_backward(Fenv:FUNCTION_ENV)(P:BACKWARD_MONOTONE_PARAMETER) = struct
  module W = Backward_connected_component_worklist(Fenv)

  let after = Array.make Fenv.nb_stmts P.bottom;;
  List.iter (fun (stmt,state) ->
    let ord = Fenv.to_ordered stmt in
    after.(ord) <- state;
    W.insert ord) P.init;;

  let rec loop () =
    match W.extract() with
    | None -> ()
    | Some(ord) ->
      let stmt = Fenv.to_stmt ord in
      let before_state = P.transfer_stmt stmt after.(ord) in
      Kernel.debug ~dkey "before_state = %a" P.pretty before_state;
      let to_update = List.map Fenv.to_ordered stmt.preds in
    let update_f upd =
      let join =
	(* If we know that we already have to recompute before.(ord), we
	   can omit the inclusion testing, and only perform the join. The
	   rationale is that querying the worklist is cheap, while
	   inclusion testing can be costly. *)
	if W.in_worklist upd
	then P.join after.(upd) before_state
	else
	  let (join,is_included) =
	    P.join_and_is_included after.(upd) before_state in
	  if is_included then W.insert upd;
	  join
      in
      after.(upd) <- join
    in
    List.iter update_f to_update;
    loop()
  ;;

  loop();;


  (* Easy access to the result of computation. *)
  let fold_on_result f init =
    let rec loop acc = function
      | i when i = Fenv.nb_stmts -> acc
      | i -> let acc = f acc (Fenv.to_stmt i) after.(i)
	     in loop acc (i+1)
    in loop init 0;;

  let iter_on_result f =
    for i = 0 to (Fenv.nb_stmts - 1) do
      f (Fenv.to_stmt i) after.(i)
    done;;

  let post_state stmt = after.(Fenv.to_ordered stmt)
  let pre_state stmt = P.transfer_stmt stmt (post_state stmt)

    
end
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
  (Fenv:FUNCTION_ENV)
  (P:FORWARD_MONOTONE_PARAMETER_GENERIC_STORAGE)
  (W:CONSULTABLE_WORKLIST) =
struct
  List.iter (fun (stmt,state) ->
    let ord = Fenv.to_ordered stmt in
    P.set_before ord state;
    W.insert ord) P.init;;

  let update_before (stmt, new_state) =
    let ord = Fenv.to_ordered stmt in
    CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
    let join =
      (* If we know that we already have to recompute before.(ord), we
	 can omit the inclusion testing, and only perform the join. The
	 rationale is that querying the worklist is cheap, while
	 inclusion testing can be costly. *)
      if W.in_worklist ord
      then P.join new_state (P.get_before ord)
      else
	let (join, is_included) =
	  P.join_and_is_included new_state (P.get_before ord)
	in
	if not is_included then W.insert ord;
	join
    in
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

  let pre_state stmt = P.get_before (Fenv.to_ordered stmt)
  let post_state stmt =
    let post_states = List.map snd (P.transfer_stmt stmt (pre_state stmt)) in
    List.fold_left P.join P.bottom post_states
end

module Simple_forward_generic_storage(Fenv:FUNCTION_ENV)(P:FORWARD_MONOTONE_PARAMETER_GENERIC_STORAGE) =
  Forward_monotone_generic_storage(Fenv)(P)(Forward_connected_component_worklist(Fenv));;

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
  module W = Forward_connected_component_worklist(Fenv);;

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
