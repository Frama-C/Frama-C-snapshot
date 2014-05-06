(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

(** Overapproximation of the states that can be attained at each statement,
    together with actions that have been performed. *)

open Dataflow2
open Data_for_aorai
open Promelaast
open Cil_types

let forward_dkey = Aorai_option.register_category "dataflow:forward"
let backward_dkey = Aorai_option.register_category "dataflow:backward"

let set_of_map map =
  Data_for_aorai.Aorai_state.Map.fold
    (fun state _ acc -> Data_for_aorai.Aorai_state.Set.add state acc)
    map
    Data_for_aorai.Aorai_state.Set.empty

let filter_state set map =
  Data_for_aorai.Aorai_state.Map.filter
    (fun state _ -> Data_for_aorai.Aorai_state.Set.mem state set)
    map

let compose_range loc b r1 r2 =
  match r1, r2 with
    | Fixed c1, Fixed c2 -> Fixed (c1 + c2)
    | Fixed c, Interval(min,max) | Interval(min,max), Fixed c ->
       Interval (c+min,c+max)
    | Fixed c, Bounded(min,max) | Bounded(min,max), Fixed c ->
      let max = 
        Logic_const.term
          (TBinOp(PlusA,max, Logic_const.tinteger c))
          Linteger
      in
      Bounded(c+min,max)
    | Fixed c1, Unbounded min | Unbounded min, Fixed c1 -> Unbounded (min+c1)
    | Interval(min1,max1), Interval(min2,max2) ->
      Interval(min1+min2,max1+max2)
    (* NB: in the bounded case, we could check if upper bound of interval
       is less then lower bound of bounded to keep bounded.
     *)
    | Interval(min1,_), Bounded(min2,_) | Bounded(min2,_), Interval(min1,_)
    | Interval(min1,_), Unbounded min2 | Unbounded min2, Interval (min1,_)
    | Bounded(min1, _), Bounded (min2, _) | Unbounded min1, Unbounded min2
    | Bounded(min1,_), Unbounded min2 | Unbounded min1, Bounded(min2,_)
      ->
      if Cil.isLogicZero b then Data_for_aorai.absolute_range loc (min1 + min2)
      else Unbounded (min1 + min2)

let fail_on_both k elt1 elt2 =
  match elt1, elt2 with
    | None, None -> None
    | Some v, None 
    | None, Some v -> Some v
    | Some _, Some _ ->
      Aorai_option.fatal
        "found a binding in both action and parameters table for %a"
        Printer.pp_term k

let compose_bindings map1 loc vals map =
  let vals = Cil_datatype.Term.Map.fold 
    (fun base intv vals ->
      let vals' =
        if Cil.isLogicZero base then
          Cil_datatype.Term.Map.add base intv Cil_datatype.Term.Map.empty
        else
          try
            let orig_base = Cil_datatype.Term.Map.find base map1 in
            Cil_datatype.Term.Map.fold
              (fun base intv' map ->
                let intv' = compose_range loc base intv' intv in
                Cil_datatype.Term.Map.add base intv' map
              )
              orig_base Cil_datatype.Term.Map.empty
          with Not_found ->
            Cil_datatype.Term.Map.add base intv Cil_datatype.Term.Map.empty
      in
      Cil_datatype.Term.Map.merge
        (Extlib.merge_opt (Data_for_aorai.merge_range loc)) vals' vals
    )
    vals Cil_datatype.Term.Map.empty
  in
  try
    let vals' = Cil_datatype.Term.Map.find loc map in
    let vals' =
      Cil_datatype.Term.Map.merge 
        (Extlib.merge_opt (Data_for_aorai.merge_range loc)) vals' vals
    in
    Cil_datatype.Term.Map.add loc vals' map
  with Not_found ->
    Cil_datatype.Term.Map.add loc vals map

let compose_actions 
    ?(args=Cil_datatype.Term.Map.empty) (fst,_,map1) (_,last,map2) =
  let map_args = Cil_datatype.Term.Map.merge fail_on_both map1 args in
  let map = 
    Cil_datatype.Term.Map.fold (compose_bindings map_args)
      map2 Cil_datatype.Term.Map.empty
  in
  (fst,last,
   Cil_datatype.Term.Map.fold
     (fun elt bind map -> 
       if Cil_datatype.Term.Map.mem elt map2 then map
       else Cil_datatype.Term.Map.add elt bind map) map1 map)

let compose_states
    ?(args=Cil_datatype.Term.Map.empty) start_state end_state =
  let treat_one_curr_state stop bindings acc =
    try
      let new_states = Data_for_aorai.Aorai_state.Map.find stop end_state in
      let composed_actions =
        Data_for_aorai.Aorai_state.Map.map 
          (fun elt -> compose_actions ~args bindings elt) new_states
      in
      let merge_stop_state _ (fst1, last1, map1) (fst2, last2, map2) =
        (Data_for_aorai.Aorai_state.Set.union fst1 fst2,
         Data_for_aorai.Aorai_state.Set.union last1 last2,
         Data_for_aorai.merge_bindings map1 map2)
      in
      Data_for_aorai.Aorai_state.Map.merge
        (Extlib.merge_opt merge_stop_state) composed_actions acc
    with Not_found -> acc
  in
  let treat_one_start_state start curr_states acc =
    let trans_state =
      Data_for_aorai.Aorai_state.Map.fold 
        treat_one_curr_state curr_states Data_for_aorai.Aorai_state.Map.empty
    in
    if Data_for_aorai.Aorai_state.Map.is_empty trans_state then acc
    else Data_for_aorai.Aorai_state.Map.add start trans_state acc
  in
  Data_for_aorai.Aorai_state.Map.fold treat_one_start_state
    start_state Data_for_aorai.Aorai_state.Map.empty

module Call_state =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Case_state)
    (struct
        let name = "Data_for_aorai.Call_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_call_state stmt state =
  let real_state =
    try
      let loop =
        Kernel_function.find_enclosing_loop
          (Kernel_function.find_englobing_kf stmt) stmt
      in
      try
        let init_state = Data_for_aorai.get_loop_init_state loop in
        compose_states init_state state
      with Not_found ->
        Aorai_option.fatal
          "Cannot find state at loop entry when analyzing statement \
           inside loop body"
    with Not_found -> state
  in
  let change old_state =
    Data_for_aorai.merge_state old_state real_state
  in
  let set _ = real_state in
  ignore (Call_state.memo ~change set stmt)

module Return_state =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Case_state)
    (struct
        let name = "Data_for_aorai.Return_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_return_state stmt state =
  let change old_state = Data_for_aorai.merge_state old_state state in
  let set _ = state in
  ignore (Return_state.memo ~change set stmt)

module type Init = sig
    val kf: Kernel_function.t
    val stack: (Kernel_function.t * bool ref) list 
    (* call stack. flag is set to true for the topmost function of each
       recursion. *)
    val initial_state: Data_for_aorai.state * Cil_datatype.Stmt.Set.t
end

let compute_func = 
  ref 
    (fun _ _ _ _ -> 
      Aorai_option.fatal "Aorai_dataflow.compute_func not properly initialized")

let extract_current_states s =
  Data_for_aorai.Aorai_state.Map.fold
    (fun _ tbl acc ->
      Data_for_aorai.Aorai_state.Map.fold
        (fun s _ acc -> Data_for_aorai.Aorai_state.Set.add s acc)
        tbl acc)
    s Data_for_aorai.Aorai_state.Set.empty

let add_or_merge state (fst, last, bindings as elt) tbl =
  try
    let (old_fst, old_last, old_bindings) = 
      Data_for_aorai.Aorai_state.Map.find state tbl
    in
    let merged_fst = Data_for_aorai.Aorai_state.Set.union old_fst fst in
    let merged_last = Data_for_aorai.Aorai_state.Set.union old_last last in
    let merged_bindings = Data_for_aorai.merge_bindings old_bindings bindings in
    Data_for_aorai.Aorai_state.Map.add 
      state (merged_fst, merged_last, merged_bindings) tbl
  with Not_found ->
    Data_for_aorai.Aorai_state.Map.add state elt tbl

let actions_to_range l =
  let add_single_action t b off acc =
    let binding =
      Cil_datatype.Term.Map.add b off Cil_datatype.Term.Map.empty
    in
    Cil_datatype.Term.Map.add t binding acc
  in
  let treat_one_action acc =
    function
      | Counter_init lv ->
        let t = Data_for_aorai.tlval lv in
        add_single_action t (Cil.lzero()) (Fixed 1) acc
      | Counter_incr lv ->
        let t = Data_for_aorai.tlval lv in
        add_single_action t t (Fixed 1) acc
      | Pebble_init(_,_,c) -> (* TODO: put post-conds on pebble sets *)
        let t = Logic_const.tvar c in add_single_action t t (Fixed 1) acc
      | Pebble_move _ -> acc (* TODO: put post-conds on pebble sets *)
      | Copy_value (lv,t) ->
        let loc = Data_for_aorai.tlval lv in
        add_single_action loc t (Fixed 0) acc
  in List.fold_left treat_one_action Cil_datatype.Term.Map.empty l

let make_start_transition ?(is_main=false) kf init_states =
  let auto = Data_for_aorai.getAutomata () in
  let is_crossable = 
    if is_main then 
      Aorai_utils.isCrossableAtInit 
    else 
      (fun trans kf -> Aorai_utils.isCrossable trans kf Promelaast.Call)
  in
  let treat_one_state state acc =
    let my_trans = Path_analysis.get_transitions_of_state state auto in
    let treat_one_trans acc trans =
        if is_crossable trans kf then begin
          let (_,action) = trans.cross in
          let bindings = actions_to_range action in
          let fst_set =
            Data_for_aorai.Aorai_state.Set.singleton trans.stop
          in
          let last_set =
            Data_for_aorai.Aorai_state.Set.singleton state
          in
          add_or_merge trans.stop (fst_set, last_set, bindings) acc
        end
        else acc
    in
    let possible_states =
      List.fold_left 
        treat_one_trans Data_for_aorai.Aorai_state.Map.empty my_trans
    in
    if Data_for_aorai.Aorai_state.Map.is_empty possible_states then acc
    else Data_for_aorai.Aorai_state.Map.add state possible_states acc
  in
  let res =
    Data_for_aorai.Aorai_state.Set.fold 
      treat_one_state init_states Data_for_aorai.Aorai_state.Map.empty
  in res

let make_return_transition kf state =
  let s = Kernel_function.find_return kf in
  set_return_state s state;
  let auto = Data_for_aorai.getAutomata () in
  let treat_one_state state bindings acc =
    let my_trans = Path_analysis.get_transitions_of_state state auto in
    let last = Data_for_aorai.Aorai_state.Set.singleton state in
    let treat_one_trans acc trans =
      if Aorai_utils.isCrossable trans kf Promelaast.Return then begin
        let (_,action) = trans.cross in
        let my_bindings = actions_to_range action in
        let new_bindings = compose_actions bindings (last, last, my_bindings) in
        add_or_merge trans.stop new_bindings acc
      end else acc
    in
    List.fold_left treat_one_trans acc my_trans
  in
  let treat_one_path start_state curr_state acc =
    let res =
      Data_for_aorai.Aorai_state.Map.fold 
        treat_one_state curr_state Data_for_aorai.Aorai_state.Map.empty
    in
    if Data_for_aorai.Aorai_state.Map.is_empty res then acc
    else Data_for_aorai.Aorai_state.Map.add start_state res acc
  in
  Data_for_aorai.Aorai_state.Map.fold 
    treat_one_path state Data_for_aorai.Aorai_state.Map.empty

let create_loop_init state =
  let res =
    Aorai_state.Map.fold
      (fun _ s acc ->
        Aorai_state.Map.fold
          (fun final (_,pre_final,_) acc ->
            let map =
              try Aorai_state.Map.find final acc
              with Not_found -> Aorai_state.Map.empty
            in
            let (init,last,actions) =
              try Aorai_state.Map.find final map
              with Not_found ->
                (Aorai_state.Set.empty,Aorai_state.Set.empty,
                 Cil_datatype.Term.Map.empty)
            in
            let map = Aorai_state.Map.add
              final
              (Aorai_state.Set.union pre_final init,
               Aorai_state.Set.union pre_final last,
               actions)
              map
            in
            Aorai_state.Map.add final map acc)
          s acc)
      state Aorai_state.Map.empty
  in
  Aorai_option.debug ~dkey:forward_dkey "@[State at loop entry@\n%a@]"
    Data_for_aorai.pretty_state res;
  res

module Computer(I: Init) = struct
  let name = "Aorai forward analysis"
  let debug = false
  (* We keep track of the loops that we have entered, since we distinguish
     states at loop initialization from states during loop itself: when
     combining predecessors, we must know where we come from.
   *)
  type data = (Data_for_aorai.state * Cil_datatype.Stmt.Set.t)
  type t = data
  let copy = Extlib.id
  let pretty fmt (s,_) = Data_for_aorai.pretty_state fmt s

 (* we do not propagate inside the loop the actions made before,
    to obtain more precise loop assigns. This is merged back in doEdge
    when we exit the loop. 
  *)
  let computeFirstPredecessor stmt (s,loops as res) =
    match stmt.skind with
      | Loop _ -> 
        Data_for_aorai.set_loop_init_state stmt s;
        create_loop_init s, Cil_datatype.Stmt.Set.add stmt loops
      | _ -> res

  let combinePredecessors stmt ~old (cur,loops) =
    let (old,_) = old in 
    (* we don't care about loops in old state: it has already been handled *)
    let is_loop = match stmt.skind with
        | Loop _ -> true
        | _ -> false
    in
    Aorai_option.debug 
      ~dkey:forward_dkey
      "Combining state (loop is %B)@\n  @[%a@]@\nwith state@\n  @[%a@]"
      is_loop
      Data_for_aorai.pretty_state old Data_for_aorai.pretty_state cur;
    if Data_for_aorai.included_state cur old then begin
      Aorai_option.debug ~dkey:forward_dkey "Included";
      if is_loop && Cil_datatype.Stmt.Set.mem stmt loops &&
        Data_for_aorai.Aorai_state.Map.is_empty
        (Data_for_aorai.get_loop_invariant_state stmt)
      then 
        Data_for_aorai.set_loop_invariant_state stmt cur;
      None
    end else begin 
      let res = 
        if is_loop then begin
          (* set_loop implicitly merges states when needed.
             However, we still have to distinguish whether we are already
             in the loop or at the initial stage.
           *)
          if Cil_datatype.Stmt.Set.mem stmt loops then begin
            Data_for_aorai.set_loop_invariant_state stmt cur;
            Data_for_aorai.get_loop_invariant_state stmt
          end else begin
            Data_for_aorai.set_loop_init_state stmt cur;
            create_loop_init (Data_for_aorai.get_loop_init_state stmt)
          end
        end else begin
          Data_for_aorai.merge_state old cur 
        end
      in
      Aorai_option.debug ~dkey:forward_dkey "Merged state is@\n  @[%a@]"
        Data_for_aorai.pretty_state res;
      let loops = 
        if is_loop then
          Cil_datatype.Stmt.Set.add stmt loops
        else loops
      in
      Some (res,loops)
    end

  let doInstr s i (state,loops as d) =
    match i with
      | Call (_,{ enode = Lval(Var v,NoOffset) },args,_) ->
        let kf = Globals.Functions.get v in
        if Data_for_aorai.isIgnoredFunction (Kernel_function.get_name kf) 
        then d (* we simply skip ignored functions. *)
        else begin
          set_call_state s state;
          Aorai_option.debug
            ~dkey:forward_dkey "Call to %a from state:@\n  @[%a@]"
            Kernel_function.pretty kf Data_for_aorai.pretty_state state;
          let prms = Kernel_function.get_formals (Globals.Functions.get v) in
          let rec bind acc prms args =
            match prms, args with
              (* in case of variadics, we can have more args than prms *)
              | [],_ -> acc
              | _,[] -> 
                Aorai_option.fatal 
                  "too few arguments in call to %a" Printer.pp_varinfo v
              | p::prms, a::args ->
                let lv = Logic_const.tvar (Cil.cvar_to_lvar p) in
                let la = Logic_utils.expr_to_term ~cast:false a in
                let value =
                  Cil_datatype.Term.Map.add
                    la (Fixed 0) Cil_datatype.Term.Map.empty
                in
                let acc = Cil_datatype.Term.Map.add lv value acc in
                bind acc prms args
          in
          let args = bind Cil_datatype.Term.Map.empty prms args in
          let init_states = extract_current_states state in
          let kf = Globals.Functions.get v in
          let init_trans = make_start_transition kf init_states in
          let end_state = !compute_func I.stack (Kstmt s) kf init_trans in
          let new_state = compose_states ~args state end_state in
          Aorai_option.debug ~dkey:forward_dkey "At end of call:@\n  @[%a@]"
            Data_for_aorai.pretty_state new_state;
          (new_state,loops)
        end
      | Call (_,e,_,_) ->
	Aorai_option.not_yet_implemented
          "Indirect call to %a is not handled yet" Printer.pp_exp e
      | Set _ | Asm _ | Skip _ | Code_annot _ -> d
        

  let doGuard _ _ _ = (GDefault, GDefault)

  let doStmt _ (state,_) =
    if Data_for_aorai.Aorai_state.Map.is_empty state then
        (* Statement is not conforming to the automaton. It must be
           on a dead path for the whole program to match the spec.
         *)
      SDone
    else
      SDefault
 
  let edge_exits_loop kf s1 s2 =
    try
      let loop = Kernel_function.find_enclosing_loop kf s1 in
      not (Cil_datatype.Stmt.equal loop s2) &&
      (match loop.skind with
        | Loop(_,b,_,_,_) ->
          
          List.exists
            (fun b' -> Cil_datatype.Block.equal b b')
            (Kernel_function.blocks_closed_by_edge s1 s2)
        | _ -> false)
    with Not_found -> false

  let doEdge s1 s2 (state,loops as t) =
    let kf = Kernel_function.find_englobing_kf s1 in
    if edge_exits_loop kf s1 s2 then begin
      let loop = Kernel_function.find_enclosing_loop kf s1 in
      let pre_state = Data_for_aorai.get_loop_init_state loop in
      let propagate = compose_states pre_state state in
      Aorai_option.debug ~dkey:forward_dkey 
        "@[Exiting from loop:@\nInit state is@\n%a@\nCurrent state is@\n%a@\n\
           Propagated state is@\n%a@\n@]"
        Data_for_aorai.pretty_state pre_state
        Data_for_aorai.pretty_state state
        Data_for_aorai.pretty_state propagate;
      propagate,loops
    end else t
    

  module StmtStartData = 
    Dataflow2.StartData(struct type t = data let size = 17 end)

  let () =
    let start = Kernel_function.find_first_stmt I.kf in
    StmtStartData.add start I.initial_state
end

let compute_func_aux stack call_site kf init_state =
  if Data_for_aorai.isIgnoredFunction (Kernel_function.get_name kf) then
    Aorai_option.fatal "compute_func on function %a which is ignored by Aorai"
      Kernel_function.pretty kf
  else if List.mem_assq kf stack then begin
    (* Recursive call: we assume all possible paths can be taken *)
    let flag = List.assq kf stack in flag := true;
    Data_for_aorai.set_kf_init_state kf init_state;
    let end_state =
      try
        Data_for_aorai.get_kf_return_state kf
      with Not_found -> Data_for_aorai.Aorai_state.Map.empty
    in
    end_state
  end else begin
    let module Init =
        struct 
          let kf = kf 
          let stack = (kf, ref false) :: stack
          let initial_state =
            match Kernel_function.find_first_stmt kf with
              | { skind = Loop _ } as stmt -> 
                Data_for_aorai.set_loop_init_state stmt init_state;
                (* we are directly entering the loop *)
                create_loop_init init_state,
                Cil_datatype.Stmt.Set.singleton stmt
              | _ -> init_state, Cil_datatype.Stmt.Set.empty
        end
    in
    let module Compute = Computer (Init) in
    let module Dataflow = Forwards(Compute) in
    Aorai_option.debug
      ~dkey:forward_dkey "Call to %a, Initial state is:@\n  @[%a@]"
      Kernel_function.pretty kf Data_for_aorai.pretty_state init_state;
    Data_for_aorai.set_kf_init_state kf init_state;
    if Kernel_function.is_definition kf then begin
      let start = Kernel_function.find_first_stmt kf in
      (match start.skind with
        (* If the first statement itself is a loop,
           sets the appropriate table, as this won't be done in Computer
           (technically, there is not firstPredecessor in this particular case)
         *)
        | Loop _ -> Data_for_aorai.set_loop_init_state start init_state
        | _ -> ());
      Dataflow.compute [Kernel_function.find_first_stmt kf]
    end;
    let end_state = 
      if Kernel_function.is_definition kf then begin
        try
          Compute.StmtStartData.find (Kernel_function.find_return kf)
        with Not_found ->
          let source =
            match call_site with
              | Kglobal -> None
              | Kstmt _ -> Some (fst (Cil_datatype.Kinstr.loc call_site))
          in
          Aorai_option.warning ?source
            "Call to %a does not follow automaton's specification. \
           This path is assumed to be dead" Kernel_function.pretty kf;
          (Data_for_aorai.Aorai_state.Map.empty, Cil_datatype.Stmt.Set.empty)
      end
      else (* we assume a declared function does not make any call. *)
        (init_state, Cil_datatype.Stmt.Set.empty)
    in
    let trans_state = make_return_transition kf (fst end_state) in 
    let (my_kf, flag) = List.hd Init.stack in
    assert (kf == my_kf);
    if !flag then begin
      let curr_end =
        try Data_for_aorai.get_kf_return_state kf 
        with Not_found -> Data_for_aorai.Aorai_state.Map.empty
      in
      Data_for_aorai.set_kf_return_state kf trans_state;
      if Data_for_aorai.included_state trans_state curr_end then
        curr_end
      else
        (* See if we've reached a fixpoint *)
        let init_state = Data_for_aorai.get_kf_init_state kf in
        !compute_func stack call_site kf init_state
    end else begin
      Data_for_aorai.set_kf_return_state kf trans_state;
      trans_state
    end
  end

let () = compute_func := compute_func_aux

let compute_forward () =
  let kf = Globals.Functions.find_by_name (Kernel.MainFunction.get()) in
  if Data_for_aorai.isIgnoredFunction (Kernel_function.get_name kf) then
    Aorai_option.abort "Main function %a is ignored by Aorai"
      Kernel_function.pretty kf;
  let (states,_) = Data_for_aorai.getAutomata () in
  let start = 
    List.fold_left
      (fun acc s ->
        match s.Promelaast.init with 
          | Bool3.True -> Data_for_aorai.Aorai_state.Set.add s acc
          | _ -> acc)
      Data_for_aorai.Aorai_state.Set.empty
      states
  in
  let start_state = make_start_transition ~is_main:true kf start in
  ignore (compute_func_aux [] Kglobal kf start_state)

module type Reachable_end_states = 
sig
  val kf: Kernel_function.t
  val stack: Kernel_function.t list
  val end_state: Data_for_aorai.state
end

module Pre_state = 
  Kernel_function.Make_Table
    (Data_for_aorai.Case_state)
    (struct
        let name = "Aorai_dataflow.Pre_state"
        let dependencies = 
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_kf_init_state kf state =
  let change old_state = Data_for_aorai.merge_state old_state state in
  let set _ = state in
  let state = (Pre_state.memo ~change set kf) in
    Aorai_option.debug ~dkey:backward_dkey
      "Call to %a, pre-state after backward analysis:@\n  @[%a@]"
      Kernel_function.pretty kf Data_for_aorai.pretty_state state;


module Post_state = 
  Kernel_function.Make_Table
    (Data_for_aorai.Case_state)
    (struct
        let name = "Aorai_dataflow.Post_state"
        let dependencies = 
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_kf_return_state kf state =
  let change old_state = Data_for_aorai.merge_state old_state state in
  let set _ = state in
  ignore (Post_state.memo ~change set kf)

module Init_loop_state =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Data_for_aorai.Case_state)
    (struct
        let name = "Aorai_dataflow.Init_loop_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_init_loop_state stmt state =
  let change old_state = Data_for_aorai.merge_state old_state state in
  let set _ = state in
  ignore (Init_loop_state.memo ~change set stmt)

module Invariant_loop_state =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Data_for_aorai.Case_state)
    (struct
        let name = "Aorai_dataflow.Invariant_loop_state"
        let dependencies =
          [ Ast.self; Aorai_option.Ya.self; Aorai_option.Ltl_File.self;
            Aorai_option.To_Buchi.self; Aorai_option.Deterministic.self ]
        let size = 17
     end)

let set_invariant_loop_state stmt state =
  let change old_state = Data_for_aorai.merge_state old_state state in
  let set _ = state in
  ignore (Invariant_loop_state.memo ~change set stmt)

let backward_analysis =
  ref 
    (fun _ _ _ -> 
      Aorai_option.fatal
        "Aorai_dataflow.backward_analysis not properly initialized")

module Backwards_computer (Reach: Reachable_end_states) =
struct
  let name = "Aorai backward computation"
  let debug = false

  type t = Data_for_aorai.state

  let pretty = Data_for_aorai.pretty_state

  let funcExitData = Data_for_aorai.Aorai_state.Map.empty

  let combineStmtStartData s ~old st =
    Aorai_option.debug ~dkey:backward_dkey
      "Statement %d:@\n%a@\nOld state is@\n%a@\nNew state is@\n%a"
      s.sid Cil_datatype.Stmt.pretty s
      Data_for_aorai.pretty_state old
      Data_for_aorai.pretty_state st;
    if Data_for_aorai.included_state st old then 
      begin
        Aorai_option.debug ~dkey:backward_dkey "Included";
        None
      end
    else
      begin
        Aorai_option.debug ~dkey:backward_dkey
          "Continuing with:@\n%a" Data_for_aorai.pretty_state st;
        Some st
      end

  let combineSuccessors = Data_for_aorai.merge_state

  let doStmt s =
    match s.skind with
      | Return _ -> Dataflow2.Done Reach.end_state
      | _ -> Dataflow2.Default

  let doInstr s instr state =
    match instr with
      | Call (_,{ enode = Lval(Var v,NoOffset) },_,_) ->
        let kf = Globals.Functions.get v in
        if Data_for_aorai.isIgnoredFunction (Kernel_function.get_name kf) 
        then Dataflow2.Default (* we simply skip ignored functions. *)
        else begin
          try
            let call_state = Call_state.find s in
            let kf = Globals.Functions.get v in
            let treat_one_state state map acc =
              let current_states = set_of_map map in
              let before_state =
                !backward_analysis Reach.stack kf current_states
              in
              let possible_states = set_of_map before_state in
              let call_map = 
                Data_for_aorai.Aorai_state.Map.find state call_state
              in
              Aorai_option.debug ~dkey:backward_dkey
                "Stmt %d - %a@\nPossible states@\n%a"
                s.sid Cil_datatype.Stmt.pretty s
                (Data_for_aorai.pretty_end_state state) call_map;
              let call_map = filter_state possible_states call_map in
              Aorai_option.debug ~dkey:backward_dkey
                "Filtered states@\n%a" 
                (Data_for_aorai.pretty_end_state state) call_map;
              if Data_for_aorai.Aorai_state.Map.is_empty call_map then acc
              else Data_for_aorai.Aorai_state.Map.add state call_map acc
            in
            let before_state =
              Data_for_aorai.Aorai_state.Map.fold
                treat_one_state state Data_for_aorai.Aorai_state.Map.empty
            in
            Done before_state
          with Not_found ->
            (* Not attained by forward analysis: this code is dead anyway. *)
            Done Data_for_aorai.Aorai_state.Map.empty
        end
      | Call (_,e,_,_) ->
        Aorai_option.not_yet_implemented
          "Indirect call to %a is not handled yet" Printer.pp_exp e
      | Set _ | Asm _ | Skip _ | Code_annot _ -> Dataflow2.Default

  let filterStmt _ _ = true

  module StmtStartData =
    Dataflow2.StartData
      (struct type t = Data_for_aorai.state let size = 17 end)

  let () =
    if Kernel_function.is_definition Reach.kf then begin
      let (all_stmts,_) =
        Dataflow2.find_stmts (Kernel_function.get_definition Reach.kf)
      in
      List.iter
        (fun s -> StmtStartData.add s Data_for_aorai.Aorai_state.Map.empty)
        all_stmts;
    end

end

let filter_possible_states kf states =
  let post_state = Data_for_aorai.get_kf_return_state kf in
  let treat_one_state state post_state acc =
    let post_state = filter_state states post_state in
    if Data_for_aorai.Aorai_state.Map.is_empty post_state then acc
    else Data_for_aorai.Aorai_state.Map.add state post_state acc
  in
  Data_for_aorai.Aorai_state.Map.fold
    treat_one_state post_state Data_for_aorai.Aorai_state.Map.empty

let filter_return_states kf states =
  let end_state = Return_state.find (Kernel_function.find_return kf) in
  let auto = Data_for_aorai.getAutomata () in
  let is_possible_state start_state state _ =
    try
      let trans = Path_analysis.get_transitions_of_state state auto in
      let return_states = 
        Data_for_aorai.Aorai_state.Map.find start_state states
      in
      let crossable tr = 
        Aorai_utils.isCrossable tr kf Promelaast.Return &&
          Data_for_aorai.Aorai_state.Map.mem tr.stop return_states
      in
      List.exists crossable trans
    with Not_found -> false
  in
  let filter_possible_states state map =
    Data_for_aorai.Aorai_state.Map.filter (is_possible_state state) map
  in
  let treat_one_state state map acc =
    let res = filter_possible_states state map in
    if Data_for_aorai.Aorai_state.Map.is_empty res then acc
    else Data_for_aorai.Aorai_state.Map.add state res acc
  in
  let res =
    Data_for_aorai.Aorai_state.Map.fold 
      treat_one_state end_state Data_for_aorai.Aorai_state.Map.empty
  in
  if Data_for_aorai.Aorai_state.Map.is_empty res &&
    not (Data_for_aorai.Aorai_state.Map.is_empty end_state) then
    (* Do not emit warning if forward computation already decided that the
       call was not conforming to the spec. *)
    Aorai_option.warning ~current:true
      "Call to %a not conforming to automaton (post-cond). \
         Assuming it is on a dead path"
      Kernel_function.pretty kf;
  res

let filter_loop_init_states old_map restrict_map =
  let treat_one_state state old_states acc =
    try
      let restrict_states = 
        Data_for_aorai.Aorai_state.Map.find state restrict_map
      in
      let old_states = filter_state (set_of_map restrict_states) old_states in
      if Data_for_aorai.Aorai_state.Map.is_empty old_states then acc
      else Data_for_aorai.Aorai_state.Map.add state old_states acc
    with Not_found -> acc (* not accessible in any case *)
  in 
  Data_for_aorai.Aorai_state.Map.fold 
    treat_one_state old_map Data_for_aorai.Aorai_state.Map.empty

let filter_loop_invariant_states old_map restrict_map =
  let acceptable_states =
    Data_for_aorai.Aorai_state.Map.fold
      (fun _ s acc -> Data_for_aorai.Aorai_state.Set.union (set_of_map s) acc)
      restrict_map Data_for_aorai.Aorai_state.Set.empty
  in
  let treat_one_state state old_states acc =
    if Data_for_aorai.Aorai_state.Set.mem state acceptable_states then begin
      let old_states = filter_state acceptable_states old_states in
      if Data_for_aorai.Aorai_state.Map.is_empty old_states then acc
      else Data_for_aorai.Aorai_state.Map.add state old_states acc
    end else acc
  in
  Data_for_aorai.Aorai_state.Map.fold
    treat_one_state old_map Data_for_aorai.Aorai_state.Map.empty

let filter_init_state restrict initial map acc =
  try
    let restrict_map = Data_for_aorai.Aorai_state.Map.find initial restrict in
    let map =
      Data_for_aorai.Aorai_state.Map.filter
        (fun state _ -> Data_for_aorai.Aorai_state.Map.mem state restrict_map)
        map
    in
    if Data_for_aorai.Aorai_state.Map.is_empty map then acc
    else Data_for_aorai.Aorai_state.Map.add initial map acc
  with Not_found -> acc

let backward_analysis_aux stack kf ret_state =
  if (Data_for_aorai.isIgnoredFunction (Kernel_function.get_name kf)) then
    Aorai_option.fatal 
      "Call backward analysis on ignored function %a" Kernel_function.pretty kf
  else if List.memq kf stack then begin
  (* recursive function: just attempt to filter wrt attainable current states *)
    let kf_post_state = filter_possible_states kf ret_state in
    set_kf_return_state kf kf_post_state;
    let before_state = Data_for_aorai.get_kf_init_state kf in
    let before_state = 
      Data_for_aorai.Aorai_state.Map.filter
        (fun s _ -> Data_for_aorai.Aorai_state.Map.mem s kf_post_state)
        before_state
    in
    set_kf_init_state kf before_state;
    before_state
  end else begin
    let kf_post_state = filter_possible_states kf ret_state in
    set_kf_return_state kf kf_post_state;
    let end_state = filter_return_states kf kf_post_state in
    let module Computer =
          Backwards_computer
            (struct 
              let stack = kf :: stack
              let kf = kf
              let end_state = end_state
             end)
    in
    let module Compute = Dataflow2.Backwards(Computer) in
    let (all_stmts,sink_stmts) =
      Dataflow2.find_stmts (Kernel_function.get_definition kf)
    in
    Compute.compute sink_stmts;
    let restrict_state =
      try
        Computer.StmtStartData.find (Kernel_function.find_first_stmt kf)
      with Not_found -> Data_for_aorai.Aorai_state.Map.empty
    in
    let before_state = Data_for_aorai.get_kf_init_state kf in
    let new_state =
      Data_for_aorai.Aorai_state.Map.fold 
        (filter_init_state restrict_state)
        before_state
        Data_for_aorai.Aorai_state.Map.empty
    in
    if 
      Data_for_aorai.Aorai_state.Map.is_empty new_state &&
        not (Data_for_aorai.Aorai_state.Map.is_empty before_state) 
    then begin
      Aorai_option.warning ~current:true
        "Call to %a not conforming to automaton (pre-cond). \
         Assuming it is on a dead path"
        Kernel_function.pretty kf;
    end;
    set_kf_init_state kf new_state;
    let treat_one_loop s =
      try
        let states = Computer.StmtStartData.find s in
        (try
           let init = Data_for_aorai.get_loop_init_state s in
           let init = filter_loop_init_states init states in
           set_init_loop_state s init;
         with Not_found -> ());
        (try
           let inv = Data_for_aorai.get_loop_invariant_state s in
           let inv = filter_loop_invariant_states inv states in
           set_invariant_loop_state s inv
         with Not_found -> ())
      with Not_found ->
        Aorai_option.warning ~source:(fst (Cil_datatype.Stmt.loc s))
          "Statement %a@ not conforming to automaton. \
           Assuming it is on a dead path"
          Printer.pp_stmt s
    in
    let visit = object
      inherit Visitor.frama_c_inplace
      method! vstmt_aux s = 
        match s.skind with 
          | Loop _ -> treat_one_loop s; Cil.DoChildren
          | _ -> Cil.DoChildren
    end
    in 
    let visit_stmt s = ignore (Visitor.visitFramacStmt visit s) in
    List.iter visit_stmt all_stmts;
    before_state
  end

let () = backward_analysis := backward_analysis_aux

let compute_backward () =
  let kf = Globals.Functions.find_by_name (Kernel.MainFunction.get()) in
  if Data_for_aorai.isIgnoredFunction (Kernel_function.get_name kf) then
    Aorai_option.abort "Main function %a is ignored by Aorai"
      Kernel_function.pretty kf;
  let final_state = Data_for_aorai.get_kf_return_state kf in
  let accepted_states =
    Data_for_aorai.Aorai_state.Map.fold
      (fun _ map acc ->
        Data_for_aorai.Aorai_state.Set.union (set_of_map map) acc)
      final_state Data_for_aorai.Aorai_state.Set.empty
  in
  ignore (backward_analysis_aux [] kf accepted_states);
  Pre_state.iter Data_for_aorai.replace_kf_init_state;
  Post_state.iter Data_for_aorai.replace_kf_return_state;
  Init_loop_state.iter Data_for_aorai.replace_loop_init_state;
  Invariant_loop_state.iter Data_for_aorai.replace_loop_invariant_state

let compute () = 
  compute_forward (); 
  Aorai_option.debug ~dkey:forward_dkey "After forward analysis";
  Data_for_aorai.debug_computed_state ();
  compute_backward ();
  Aorai_option.debug ~dkey:backward_dkey "After backward analysis";
  Data_for_aorai.debug_computed_state ~dkey:backward_dkey();

(* 
Local Variables:
compile-command: "make -C ../.."
End:
*)
