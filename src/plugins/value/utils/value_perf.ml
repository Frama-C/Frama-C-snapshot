(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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



(****************************************************************)
(* Configuration *)

(* Period between two consecutive displays, in seconds. *)
let display_interval = 60.0;;		

(* Do not show functions that execute for less than that percent of
   the total running time.  The value is 1/60, i.e. does not display
   functions that execute for longer than 3s after it has run for
   3 minutes.
*)
let does_not_account_smaller_than = 1.667 

(* OCaml time is not always increasing, so we use max to fix this. *)
let duration a b = max (b -. a) 0.0

(****************************************************************)
(* The metrics being computed and displayed. *)

(* Performance information regarding a called function. *)
module Call_info = struct
  type t = {
    (* How many times the function was called. *)
    mutable nb_calls: int;

    (* How many times the call had to be computed (i.e. with calls
       cached with memexec removed) *)
    mutable nb_effective_calls: int;

    (* The accumulated execution time for past calls. *)
    mutable total_duration: float;

    (* If we are executing the function, since when. It is a list
       because of the recursive calls. *)
    mutable since: float list;
  }
  ;;

  let create() = { nb_calls = 0; nb_effective_calls = 0; total_duration = 0.0;
		   since = [] };;

  (* Represents the calls to the main function.  *)
  let main = create();;

  (* Also accounts for currently executing time. *)
  let total_duration current_time call_info =
    let additional_time = match call_info.since with 
      | [] -> 0.0
      | since::_ -> duration since current_time
    in
    assert (additional_time >= 0.0);
    additional_time +. call_info.total_duration
  ;;
    

  let print fmt kf call_info current_time = 
    let bullet = match call_info.since with 
      | [] -> "+"
      | _::_ -> "*"
    in
    Format.fprintf fmt "%s %a: executed: %dx total: %.3fs\n" bullet
      Kernel_function.pretty kf call_info.nb_calls (total_duration current_time call_info)
  ;;

  (* Sorts call_infos by decreasing execution time.  *)
  let cmp current_time ci1 ci2 = 
    - (Pervasives.compare (total_duration current_time ci1) (total_duration current_time ci2))
  ;;

  (* From an iteration, filter and sort by call_info, and returns the
     sorted list. *)
  let filter_and_sort iter get_ci _parent_duration current_time = 
    let analysis_total_time = total_duration current_time main in
    let threshold = analysis_total_time *. (does_not_account_smaller_than /. 100.0) in
    let list = ref [] in
    iter (fun elt -> 
      let ci = get_ci elt in
      if total_duration current_time ci	> threshold
      then list := elt::!list);
    let sorted_list = List.fast_sort 
      (fun elt1 elt2 -> (cmp current_time) (get_ci elt1) (get_ci elt2)) !list
    in
    sorted_list
  ;;

  (* before/after pair. *)
  let before_call t since = 
    t.since <- since::t.since
  ;;

  let after_call t to_ = 
    let since = List.hd t.since in
    let duration = duration since to_ in
    assert (duration >= 0.0);
    t.total_duration <- t.total_duration +. duration;
    t.nb_calls <- t.nb_calls + 1;
    t.since <- List.tl t.since
  ;;


end
  
(****************************************************************)
(* Flat and DAG views of performance. *)

(* Note: since need to be stored only in the flat view. *)

type flat_perf_info = {
  (* The grand total performance information for the function. *)
  call_info: Call_info.t;

  (* For DAG-view: the per-caller performance information. *)
  called_functions: Call_info.t Kernel_function.Hashtbl.t;
}
;;

let flat_perf_create() = { 
  call_info = Call_info.create();
  called_functions = Kernel_function.Hashtbl.create 17;
};;

let flat = Kernel_function.Hashtbl.create 17;;

let flat_print current_time fmt = 
  Format.fprintf fmt "Long running functions (does not include current running time):\n";
  Format.fprintf fmt "===============================================================\n";
  let each_flat_entry (kf, pi) = 
    Call_info.print fmt kf pi.call_info current_time;
    Format.fprintf fmt "    ";
    let caller_duration = Call_info.total_duration current_time pi.call_info in
    let total_sub = ref 0.0 in
    let total_others = ref 0.0 in
    let nb_others = ref 0 in
    let each_called_entry kf ci = 
      let callee_duration = Call_info.total_duration current_time ci in
      total_sub := !total_sub +. callee_duration;
      let percentage = (100.0 *. (callee_duration /. caller_duration)) in
      if percentage > 5.0
      then 
	Format.fprintf fmt "| %a %dx %.3fs (%.1f%%) " 
	  Kernel_function.pretty kf ci.Call_info.nb_calls
	  callee_duration percentage
      else
	(total_others := !total_others +. callee_duration;
	 incr nb_others)
    in
    Kernel_function.Hashtbl.iter_sorted_by_value 
      ~cmp:(Call_info.cmp current_time) each_called_entry pi.called_functions;
    (if !nb_others > 0
     then Format.fprintf fmt "| %d others: %.3fs (%.1f%%) "
	!nb_others !total_others (100.0 *. !total_others /. caller_duration));
    let self_duration = duration !total_sub caller_duration in
    Format.fprintf fmt "| self: %.3fs (%.1f%%)|\n" 
      self_duration
      (100.0 *. (self_duration /. caller_duration))
  in
  let flat_entries = Call_info.filter_and_sort 
    (fun f -> Kernel_function.Hashtbl.iter (fun k v -> f(k,v)) flat)
    (fun (_,v) -> v.call_info)
    (Call_info.total_duration current_time Call_info.main)
    current_time in
  List.iter each_flat_entry flat_entries
;;  
    


(****************************************************************)
(* Per-callstack performance. *)

module Call_site = Datatype.Pair(Kernel_function)(Cil_datatype.Kinstr)

module Imperative_callstack_trie(M:sig type t val default:unit -> t end) = struct

  module Hashtbl = Hashtbl.Make(Call_site)

  type elt = { 
    mutable self: M.t ; 	
    subtree: t 
  } 

  and t = elt Hashtbl.t
  ;;

  let empty() = Hashtbl.create 7;;
  let reset t = Hashtbl.clear t;;
  let create_node init = 
    { self = init; subtree = empty() }

  let rec find_subtree t callstack res = match callstack with
    | [] -> 
      (match res with
      | None -> failwith "Called findsubtree with an empty callstack"
      | Some x -> x)
    | a::b -> 
      let subnode = 
	try Hashtbl.find t a
	with Not_found -> let n = create_node (M.default()) in
			  Hashtbl.add t a n;
			  n
      in find_subtree subnode.subtree b (Some subnode)

  let find_subtree t callstack = find_subtree t (List.rev callstack) None

  let find t callstack = (find_subtree t callstack).self

  let _add t callstack smth = 
    let node = find_subtree t callstack in
    node.self <- smth
  ;;

  let _update t callstack f = 
    let node = find_subtree t callstack in
    node.self <- f node.self
  ;;
end

type perf_info = { 
  call_info_per_stack: Call_info.t;
}

module Perf_by_callstack = Imperative_callstack_trie(struct 
  type t = perf_info
  let default() = 
    { call_info_per_stack = Call_info.create() }
end)

(* Head of the tree. Only the subtree field il really used.  *)
let perf = Perf_by_callstack.empty();;
let last_time_displayed = ref 0.0;;


let print_indentation fmt n = 
  for _i = 0 to n-1 do Format.fprintf fmt "| " done;
;;

let rec display_node fmt kf indentation node curtime = 
  print_indentation fmt indentation;
  Call_info.print fmt kf node.Perf_by_callstack.self.call_info_per_stack curtime;
  display_subtree fmt (indentation+1) node.Perf_by_callstack.subtree 
    (Call_info.total_duration 
       curtime node.Perf_by_callstack.self.call_info_per_stack)
    curtime

and display_subtree fmt indentation subtree parent_duration curtime = 
  let entries = Call_info.filter_and_sort
    (fun f -> Perf_by_callstack.Hashtbl.iter (fun k v -> f(k,v)) subtree)
    (fun (_,node) -> node.Perf_by_callstack.self.call_info_per_stack)
    parent_duration
    curtime 
  in
  List.iter (fun ((kf,_),node) -> display_node fmt kf indentation node curtime) entries;
;;

let display fmt = 
  if Value_parameters.ValShowPerf.get()
  then begin 
    Format.fprintf fmt "####### Value execution feedback #########\n";
    let current_time = (Sys.time()) in
    flat_print current_time fmt;
    Format.fprintf fmt "\n";
    Format.fprintf fmt "Execution time per callstack (includes current running time):\n";
    Format.fprintf fmt "=============================================================\n";
    display_subtree fmt 0 perf 
      (Call_info.total_duration current_time Call_info.main) current_time;
    Format.fprintf fmt "################\n"
  end
;;

let caller_callee_callinfo = function
  | (callee_kf,_)::(caller_kf,_)::_ -> 
    (let caller_flat = Kernel_function.Hashtbl.find flat caller_kf in
    try 
      Kernel_function.Hashtbl.find caller_flat.called_functions callee_kf 
    with Not_found -> 
      let call_info = Call_info.create() in
      Kernel_function.Hashtbl.add caller_flat.called_functions callee_kf call_info;
      call_info)
    | [_] -> Call_info.main
    | [] -> assert false
;;

let start_doing_perf callstack =
  if Value_parameters.ValShowPerf.get()
  then begin
    let time = Sys.time() in
    assert (callstack != []);
    let kf = fst (List.hd callstack) in
    let flat_info = 
      try Kernel_function.Hashtbl.find flat kf
      with Not_found -> 
	let flatp = flat_perf_create() in 
	Kernel_function.Hashtbl.add flat kf flatp; flatp
    in
    
    Call_info.before_call flat_info.call_info time;
    Call_info.before_call (caller_callee_callinfo callstack) time;
    let node = Perf_by_callstack.find perf callstack in
    Call_info.before_call node.call_info_per_stack time;

    if (duration !last_time_displayed time) > display_interval 
    then (last_time_displayed := time; Kernel.feedback "%t" display)
  end
;;
  
let stop_doing_perf callstack =
  if Value_parameters.ValShowPerf.get() 
  then begin
    let time = Sys.time() in
    let kf = fst (List.hd callstack) in
    let flat_info = Kernel_function.Hashtbl.find flat kf in
    Call_info.after_call flat_info.call_info time;
    let node = Perf_by_callstack.find perf callstack in
    Call_info.after_call node.call_info_per_stack time;
    Call_info.after_call (caller_callee_callinfo callstack) time;
  end
;;

let reset_perf () =
  let reset_callinfo ci = 
    ci.Call_info.nb_calls <- 0;
    ci.Call_info.nb_effective_calls <- 0;
    ci.Call_info.total_duration <- 0.0;
    ci.Call_info.since <- []
  in
  reset_callinfo Call_info.main;
  Kernel_function.Hashtbl.clear flat;
  last_time_displayed := 0.0;
  Perf_by_callstack.reset perf
;;

(* -------------------------------------------------------------------------- *)
(* --- Flamegraphs                                                        --- *)
(* -------------------------------------------------------------------------- *)

(* Set to [Some _] if option [-val-dump-flamegraph] is set and [main] is
   currently being analyzed and the file is ok. Otherwise, set to [None]. *)
let oc_flamegraph = ref None

let stack_flamegraph = ref []
(* Callstack for flamegraphs. The most recent function is at the top of the
   list. The elements of the list are [(starting_time, self_total_time)].
   [starting_time] is the time when we started analyzing the function.
   [total_time] is the time spent so far in the function itself, _without the
   callees_. [total_time] is updated from [starting_time] when we start a
   callee, or when the analysis of the function ends. This stack is never
   empty when an analysis is in progress. *)

(* pretty-prints the functions in a Value callstack, starting by main (i.e.
   in reverse order). *)
let pretty_callstack oc l =
  let rec aux oc = function
    | [] -> () (* does not happen in theory *)
    | [main, _] -> Printf.fprintf oc "%s" (Kernel_function.get_name main)
    | (f, _) :: q ->
      Printf.fprintf oc "%a;%s" aux q (Kernel_function.get_name f)
  in
  aux oc l

(* update the [self_total_time] information for the function being analyzed,
   assuming that the current time is [time] *)
let update_self_total_time time =
  match !stack_flamegraph with
  | [] -> assert false
  | (start_caller, total) :: q ->
    let d = duration start_caller time in
    stack_flamegraph := (start_caller, d +. total) :: q

(* called when a new function is being analyzed *)
let start_doing_flamegraph callstack =
  match callstack with
  | [] -> assert false
  | [_] ->
    (* Analysis of main *)
    let file = Value_parameters.ValPerfFlamegraphs.get () in
    if file <> "" then begin
      try
        (* Flamegraphs must be computed. Set up the stack and the output file *)
        let oc = open_out file in
        oc_flamegraph := Some oc;
        stack_flamegraph := [ (Sys.time (), 0.) ]
      with e ->
        Value_parameters.error "cannot open flamegraph file: %s"
          (Printexc.to_string e);
        oc_flamegraph := None (* to be on the safe side  *)
    end
  | _ :: _ :: _ ->
    if !oc_flamegraph <> None then
      (* Flamegraphs are being computed. Update time spent in current function
         so far, then push a slot for the analysis of the new function *)
      let time = Sys.time () in
      update_self_total_time time;
      stack_flamegraph := (time, 0.) :: !stack_flamegraph;
;;

(* called when the analysis of a function ends. This function is at the top
   of [callstack] *)
let stop_doing_flamegraph callstack =
  match !oc_flamegraph with
  | None -> ()
  | Some oc -> (* Flamegraphs are being recorded *)
    let time = Sys.time() in
    update_self_total_time time; (* update current function *)
    match !stack_flamegraph with
    | [] -> assert false
    | (_, total) :: q ->
      (* dump the total time (that we just updated) for the current function *)
      Printf.fprintf oc "%a %.3f\n%!"
        pretty_callstack callstack (total *. 1000.);
      match q with
      | [] -> stack_flamegraph := [] (* we are back to the main function *)
      | (_, total_caller) :: q' ->
        (* drop the current function from the flamegraph stack AND update
           the 'current time' information, so that the time spent in the
           callee is not counted. *)
        stack_flamegraph := (time, total_caller) :: q'
;;

let reset_flamegraph () =
  match !oc_flamegraph with
  | None -> ()
  | Some fd -> close_out fd; stack_flamegraph := []


(* -------------------------------------------------------------------------- *)
(* --- Exported interface                                                 --- *)
(* -------------------------------------------------------------------------- *)

let start_doing callgraph =
  start_doing_perf callgraph;
  start_doing_flamegraph callgraph;
;;

let stop_doing callgraph =
  stop_doing_perf callgraph;
  stop_doing_flamegraph callgraph;
;;


let reset () =
  reset_perf ();
  reset_flamegraph ();
;;


(* TODO: Output files with more graphical outputs, such as

   Gprof2dot-like output: (directory output the dot)
   http://code.google.com/p/jrfonseca/wiki/Gprof2Dot

   The latter would be useful to see when imbricated loops multiply
   the number of calls to leaf functions. 

   TODO: Also account for the memexec hit rate; and for the individual
   execution time of derived plugins.
*)
