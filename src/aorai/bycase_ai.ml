(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open Data_for_aorai
open Promelaast
open Cil_types
open Cil
open Cilutil
open Ast_info
open Spec_tools

let active_before_call kf =
  let (_,pre_trans) = Data_for_aorai.get_func_pre (Kernel_function.get_name kf)
  in
  let (_,trans) = Data_for_aorai.getAutomata () in
  List.fold_left
    (fun acc tr ->
      if pre_trans.(tr.numt) then 
        Data_for_aorai.Aorai_state.Set.add tr.start acc
      else acc)
    Data_for_aorai.Aorai_state.Set.empty trans

let merge_opt f k o1 o2 =
  match o1,o2 with
    | None, None -> None
    | Some x, None | None, Some x -> Some x
    | Some x1, Some x2 -> Some (f k x1 x2)

let compose_range loc b r1 r2 =
  match r1, r2 with
    | Fixed c1, Fixed c2 -> Fixed (c1 + c2)
    | Fixed c, Interval(min,max) | Interval(min,max), Fixed c ->
       Interval (c+min,c+max)
    | Fixed c, Bounded(min,max) | Bounded(min,max), Fixed c ->
      let max = 
        Logic_const.term
          (TBinOp(PlusA,max, Logic_const.tinteger ~ikind:IInt c))
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
        (merge_opt (Data_for_aorai.merge_range loc)) vals' vals
    )
    vals Cil_datatype.Term.Map.empty
  in
  try
    let vals' = Cil_datatype.Term.Map.find loc map in
    let vals' =
      Cil_datatype.Term.Map.merge 
        (merge_opt (Data_for_aorai.merge_range loc)) vals' vals
    in
    Cil_datatype.Term.Map.add loc vals' map
  with Not_found ->
    Cil_datatype.Term.Map.add loc vals map

let compose_actions map1 map2 =
  let map = 
    Cil_datatype.Term.Map.fold (compose_bindings map1)
      map2 Cil_datatype.Term.Map.empty
  in
  Cil_datatype.Term.Map.fold
    (fun elt bind map -> 
      if Cil_datatype.Term.Map.mem elt map2 then map
      else Cil_datatype.Term.Map.add elt bind map) map1 map

let print_action_binding fmt action =
      Cil_datatype.Term.Map.iter (fun t m ->
        Cil_datatype.Term.Map.iter (fun t' itv ->
          Format.fprintf fmt "%a <- %a + %a@." !Ast_printer.d_term t
            !Ast_printer.d_term t' Data_for_aorai.Range.pretty itv) m)
        action

let update_action_call kf pre_ki ki called_func pre_call_st post_call_st =
  let ret_called = Kstmt (Kernel_function.find_return called_func) in
  let (_,pre_trans) =
    Data_for_aorai.get_func_pre (Kernel_function.get_name kf)
  in
  let (_,call_trans) =
    Data_for_aorai.get_func_pre (Kernel_function.get_name called_func)
  in
  let treat_one_state_post pre pre_call post_call =
    let current_actions = 
      Data_for_aorai.get_action_bindings kf pre_ki pre pre_call in
    let post_actions = 
      Data_for_aorai.get_action_bindings 
        called_func ret_called pre_call post_call
    in
    let res = compose_actions current_actions post_actions in
(*    Format.printf "Merging actions in %a: %s -> %s@.%aand@.%agive@.%a"
      Cil_datatype.Kinstr.pretty pre_ki
      pre.Promelaast.name post_call.Promelaast.name
      print_action_binding current_actions
      print_action_binding post_actions
      print_action_binding res; *)
    Data_for_aorai.merge_action_bindings kf ki pre post_call res
  in
  let treat_one_trans_call pre pre_call trans =
    if call_trans.(trans.numt) then begin
      Array.iteri
        (fun index b ->
          if b then begin
            let state = Data_for_aorai.getState index in
            treat_one_state_post pre pre_call state 
          end)
        post_call_st.(trans.stop.nums)
    end
  in
  let auto = Data_for_aorai.getAutomata () in
  let treat_one_call_state pre pre_call =
    let trans = Path_analysis.get_transitions_of_state pre_call auto in
    List.iter (treat_one_trans_call pre pre_call) trans
  in
  let treat_one_trans_pre pre trans =
    if pre_trans.(trans.numt) then begin
      Array.iteri
        (fun index b -> 
          if b then begin
            let state = Data_for_aorai.getState index in
            treat_one_call_state pre state
          end)
        pre_call_st.(trans.stop.nums)
    end
  in
  let update_one_action pre =
    let trans = Path_analysis.get_transitions_of_state pre auto in
    List.iter (treat_one_trans_pre pre) trans
  in
  let my_pre = active_before_call kf in
  Data_for_aorai.Aorai_state.Set.iter update_one_action my_pre

let compose_assocs_post assocs_st (post_st,post_tr) =
  let st,tr = mk_empty_pre_or_post () in
  let st,tr = ref st, ref tr in
  Array.iteri
    (fun index b ->
      if b then begin
        st:=bool_array_or post_st.(index) !st;
        tr:=bool_array_or post_tr.(index) !tr
      end)
    assocs_st;
  (!st,!tr)

let compose_assocs_pre assocs_st (_,pre_tr) (post_st,_) =
  let st,tr = mk_empty_pre_or_post () in
  let st,tr = ref st, ref tr in
  let (_,trans_l) = Data_for_aorai.getAutomata() in
  Array.iteri
    (fun index b ->
       if b then begin
         Array.iteri
           (fun value val_assocs -> if val_assocs.(index) then !st.(value)<-true)
           post_st;
       end
    )
    assocs_st;
  List.iter
    (fun t -> if pre_tr.(t.numt) && (!st).(t.stop.nums) then !tr.(t.numt)<-true)
    trans_l;
  (!st,!tr)


(** bool array array -> (bool array array*bool array array) -> (bool array array*bool array array)
    Given a set of states and the bycase post-condition of an operation
    this function returns the new pre-condition after the call of the operation in the context of current_st.
*)
let mk_forward_composition kf pre_ki ki called_func pre_st in_func_st post =
  let new_st,new_tr = mk_empty_pre_or_post_bycase () in
  Array.iteri
    (fun index assocs ->
      let s,t = compose_assocs_post assocs post in
      new_st.(index)<-s;
      new_tr.(index)<-t
    )
    in_func_st;
  update_action_call kf pre_ki ki called_func pre_st (fst post);
  (new_st,new_tr)

(** bool array array -> (bool  array*bool  array) -> (bool array array*bool array array) -> (bool array array*bool array array)
    Given a set of states and the bycase post-condition of an operation
    this function returns the new pre-condition after the call of the operation in the context of current_st.
*)
let mk_backward_composition current_st pre post =
  let new_st,new_tr = mk_empty_pre_or_post_bycase () in
  Array.iteri
    (fun index assocs ->
       let s,t = compose_assocs_pre assocs pre post in
       new_st.(index)<-s;
       new_tr.(index)<-t
    )
    current_st;

  (new_st,new_tr)

let merge_actions kf ki ki1 ki2 post_st1 post_st2 =
  let (state,_ as auto) = Data_for_aorai.getAutomata () in
(*  Format.printf "Merging from %a and %a in %a@."
    Cil_datatype.Kinstr.pretty ki1
    Cil_datatype.Kinstr.pretty ki2
    Cil_datatype.Kinstr.pretty ki; *)
  let merge_one_path pre_state post_state =
    let trans = Path_analysis.get_transitions_of_state pre_state auto in
    if List.exists 
      (fun x -> post_st1.(x.stop.nums).(post_state.nums)) trans
    then begin
      let action =
        Data_for_aorai.get_action_bindings kf ki1 pre_state post_state
      in
      Data_for_aorai.merge_action_bindings kf ki pre_state post_state action
    end;
    if List.exists 
      (fun x -> post_st2.(x.stop.nums).(post_state.nums)) trans 
    then begin
      let action =
        Data_for_aorai.get_action_bindings kf ki2 pre_state post_state
      in
      Data_for_aorai.merge_action_bindings kf ki pre_state post_state action
    end;
(*    if List.exists 
      (fun x -> post_st2.(x.stop.nums).(post_state.nums)
        || post_st1.(x.stop.nums).(post_state.nums))
      trans 
    then begin
    let res = Data_for_aorai.get_action_bindings kf ki pre_state post_state in
       Format.printf "%s -> %s Result is@.%a" 
          pre_state.Promelaast.name post_state.Promelaast.name
       print_action_binding res
    end *)
  in
  ignore (Extlib.product merge_one_path state state)

let compute_actions_invariant action_pre action_post =
  let changed = ref false in
  let merge_binding_opt loc base range1 range2 =
    match range1, range2 with
      | _, None -> range1
      | None, Some _ -> changed:=true; range2
      | Some r1, Some r2 -> 
        let res = Data_for_aorai.merge_range loc base r1 r2 in
        if not (Data_for_aorai.Range.equal r1 res) then changed:=true;
        Some res
  in 
  let merge_bases loc bindings1 bindings2 =
    match bindings1, bindings2 with
      | _, None -> bindings1
      | None, Some b2 -> changed:=true; 
        let b1 =
          Cil_datatype.Term.Map.add 
            (Cil.lzero ()) (Fixed 0) Cil_datatype.Term.Map.empty
        in
        Some (Cil_datatype.Term.Map.merge (merge_binding_opt loc) b1 b2)
      | Some b1, Some b2 ->
        Some (Cil_datatype.Term.Map.merge (merge_binding_opt loc) b1 b2)
  in
  let map = Cil_datatype.Term.Map.merge merge_bases action_pre action_post in
  !changed, map

let update_loop_actions kf init inner last (post_st,_) =
  let (state,_ as auto) = Data_for_aorai.getAutomata () in
  let changed = ref false in
  let merge_one_path pre_state post_state =
    let trans = Path_analysis.get_transitions_of_state pre_state auto in
    if 
      List.exists (fun x -> post_st.(x.stop.nums).(post_state.nums)) trans 
    then begin
      let action_pre =
        Data_for_aorai.get_action_bindings kf init pre_state post_state
      in
      let action_step =
        Data_for_aorai.get_action_bindings kf inner pre_state post_state
      in
      let has_changed, action = 
        compute_actions_invariant action_pre action_step
      in
      changed := !changed || has_changed;
(*      if has_changed then begin
        Format.printf 
         "%s -> %s In loop (init is %a, inner is %a, after is %a):@.\
          Before:@.%aAfter:@.%a"
          pre_state.Promelaast.name post_state.Promelaast.name
          Cil_datatype.Kinstr.pretty init Cil_datatype.Kinstr.pretty inner
          Cil_datatype.Kinstr.pretty last
          print_action_binding action_pre print_action_binding action;
      end;
*)
      Data_for_aorai.set_action_bindings kf last pre_state post_state action
    end
  in
  ignore (Extlib.product merge_one_path state state);
  !changed

let init_specification () =
  List.iter
    (fun name ->
       let pre,_ = Data_for_aorai.get_func_pre name in
       let post_st,post_tr = mk_empty_pre_or_post_bycase () in
       Array.iteri 
	 (fun index _ -> 
	    if pre.(index) then begin
	      post_st.(index)<- fst (Data_for_aorai.get_func_post name) ;
	      post_tr.(index)<- snd (Data_for_aorai.get_func_post name) 
	    end
	 ) 
	 post_st;
       
       (* Use Dijsktra algorithm to remove unreachable states *)
       Array.iteri 
	 (fun st1 post -> 
	    Array.iteri  
	      (fun st2 b -> 
		 if b then 
		   if 
                     not 
                       (Path_analysis.existing_path
                          (Data_for_aorai.getAutomata())  st1 st2) 
                   then
		     begin
		       post_st.(st1).(st2) <- false;
		       Aorai_option.feedback ~level:2 
                         "Function %s : state %s unreachable in post \
                          from %s (Dijkstra simplification).\n" name 
                         (Data_for_aorai.getStateName st2) 
                         (Data_for_aorai.getStateName st1)
		     end
	      )
	      post ;
	 )
	 post_st; 

       (* Removing transitions corresponding to unreachable states *)
       Array.iteri 
	 (fun st1 _ -> 
	    List.iter
	      (fun tr -> 
		 let st2 = tr.Promelaast.stop.Promelaast.nums in
		 if (not (post_st.(st1).(st2))) && 
		   post_tr.(st1).(tr.Promelaast.numt) then 
		     begin
		       post_tr.(st1).(tr.Promelaast.numt) <- false;
		       if Aorai_option.verbose_atleast 2 then 
			 Aorai_option.feedback 
                           "Function %s: transition %d reaches an unreachable \
                            state in post from %s (Dijkstra simplification)."
                           name tr.Promelaast.numt 
                           (Data_for_aorai.getStateName st1)
		     end
	      )
	      (snd (Data_for_aorai.getAutomata()))
	 )
	 post_tr;
	    
       Data_for_aorai.set_func_post_bycase name (post_st,post_tr)
    )
    (Data_for_aorai.getFunctions_from_c ())

let tlval lv = Logic_const.term (TLval lv) (Cil.typeOfTermLval lv)

let actions_to_range l =
  let treat_one_action acc =
    function
      | Counter_init lv ->
        let t = tlval lv in
        (t,(Cil.lzero(), Fixed 1)) :: acc
      | Counter_incr lv ->
        let t = tlval lv in
        (t, (t,Fixed 1)) :: acc
      | Pebble_init(_,_,c) -> (* TODO: put post-conds on pebble sets *)
        let t = Logic_const.tvar c in
        (t,(t,Fixed 1)) :: acc
      | Pebble_move _ -> acc (* TODO: put post-conds on pebble sets *)
      | Copy_value (lv,t) ->
        let loc = tlval lv  in
        (loc,(t,Fixed 0)) :: acc
  in List.fold_left treat_one_action [] l

let update_actions_call_func kf (_,tr) =
  (* We update actions for the active transitions at the entrance
     of the function: the active states are exactly the ones upon which we
     split (and the ending states of the active transitions)
   *)
  let treat_one_trans idx status =
    if status then begin
      let trans = Data_for_aorai.getTransition idx in
      Aorai_option.debug ~dkey:"action" 
        "Call to %a: treating actions of trans %s -> %s"
        Kernel_function.pretty kf 
        trans.start.Promelaast.name trans.stop.Promelaast.name;
      let actions = actions_to_range (snd trans.cross) in
      List.iter
        (fun (l,v) ->
          Aorai_option.debug ~dkey:"action"
            "Add binding for %a: %a + %a"
            Cil_datatype.Term.pretty l Cil_datatype.Term.pretty (fst v)
            Data_for_aorai.Range.pretty (snd v);
          Data_for_aorai.add_action_path kf Kglobal trans.start trans.stop l v)
        actions
    end
  in
  Array.iter (Array.iteri treat_one_trans) tr

let add_one ~is_absolute = function
  | Fixed c -> Fixed (c+1)
  | Interval(min,max) -> Interval(min+1,max+1)
  | Bounded _ as r when is_absolute -> r
  | Bounded (min,_) -> Unbounded (min+1)
  | Unbounded min -> Unbounded (min+1)

let actions_to_range_step kf ki st1 trans =
  let map = Data_for_aorai.get_action_bindings kf ki st1 trans.start in
  let treat_one_action acc =
    function
      | Counter_init lv ->
        let t = tlval lv in
        (t,(Cil.lzero(), Fixed 1)) :: acc
      | Counter_incr lv ->
        let t = tlval lv in
        (try
           (let bindings = Cil_datatype.Term.Map.find t map in
            let abs =
              try 
                let r = Cil_datatype.Term.Map.find (Cil.lzero()) bindings in
                let r = add_one ~is_absolute:true r in
                (t, (Cil.lzero(), r)) :: acc
              with Not_found -> acc
            in
            try
              let r = Cil_datatype.Term.Map.find t bindings in
              let r = add_one ~is_absolute:false r in
              (t, (t,r)) :: acc
            with Not_found -> abs)
         with Not_found ->
           (* adds an absolute binding *)
           (t, (Cil.lzero(), Data_for_aorai.absolute_range t 1))::acc)
      | Pebble_init(_,_,c) -> (* TODO: put post-conds on pebble sets *)
        let t = Logic_const.tvar c in
        (t,(t,Fixed 1)) :: acc
      | Pebble_move _ -> acc (* TODO: put post-conds on pebble sets *)
      | Copy_value (lv,t) ->
        let loc = tlval lv  in
        (loc,(t,Fixed 0)) :: acc
  in List.fold_left treat_one_action [] (snd trans.cross)

let update_actions_return_func kf ki tr =
  let ret_ki = Kstmt (Kernel_function.find_return kf) in
  let auto = Data_for_aorai.getAutomata () in
  let treat_one_trans pre_state idx status =
    let trans = Data_for_aorai.getTransition idx in
    let map = Data_for_aorai.get_action_bindings kf ki pre_state trans.start in
    if status then begin
      Aorai_option.debug ~dkey:"action"
        "Return statement of %a: treating transition %s -> %s \
         from initial state %s"
      Kernel_function.pretty kf
      trans.start.Promelaast.name trans.stop.Promelaast.name
      pre_state.Promelaast.name;
      Cil_datatype.Term.Map.iter
        (fun l _ -> Aorai_option.debug ~dkey:"action" 
          "Got binding for %a" Cil_datatype.Term.pretty l)
        map;
      let actions = actions_to_range_step kf ki pre_state trans in
      let map = 
        List.fold_left
          (fun map (l, (b,r)) ->
            Aorai_option.debug ~dkey:"action" "%a <- %a + %a"
              Cil_datatype.Term.pretty l Cil_datatype.Term.pretty b
              Data_for_aorai.Range.pretty r;
            let bindings =
              try Cil_datatype.Term.Map.find l map
              with Not_found -> Cil_datatype.Term.Map.empty
            in
            Cil_datatype.Term.Map.add 
              l (Cil_datatype.Term.Map.add b r bindings) map)
          map actions
      in
      Data_for_aorai.merge_action_bindings kf ret_ki pre_state trans.stop map
    end
  in
  let treat_one_pre_trans trans =
    Array.iteri (treat_one_trans trans.start) tr.(trans.stop.nums)
  in
  let treat_one_state pre_state =
    let my_trans = Path_analysis.get_transitions_of_state pre_state auto in
    List.iter treat_one_pre_trans my_trans
  in
  let pre_states = active_before_call kf in
  Data_for_aorai.Aorai_state.Set.iter treat_one_state pre_states

(** Global information on functions that are collected during each pass. These
    information are furthermore used to restrict pre or post-condition of
    fonctions according to their scope of use.
*)
let functions_pre_usecase : (string , (bool array array * bool array array)) Hashtbl.t = Hashtbl.create 97
let functions_post_usecase : (string , (bool array array * bool array array)) Hashtbl.t = Hashtbl.create 97
(* let functions_pre_usecase_Recursive : (string , (bool array array * bool array array)) Hashtbl.t  = Hashtbl.create 97 *)
(* let functions_post_usecase_Recursive : (string , (bool array array * bool array array)) Hashtbl.t   = Hashtbl.create 97 *)

(* Mark to ensures the completion of functions specification computation *)
let spec_modified = ref false

(**
  This visitor requires that each function has a specification. It then
  computes a finer specification by forward and backard abstract interpretation
  on states.

  This vistor use mainly 2 sub-functions (propagates_pre and propagates_post)
  that implement respectively forward and backward treatment.
*)
class visit_propagating_pre_post_constraints_bycase 
  (auto:Promelaast.typed_automaton)  =
  (***************************************************************************)
  (* For the two pass                                                        *)
  (*                                                                         *)
  (** Associates each statement of the fonction to a pre/post specification  *)
(*  let labelled_stmts_spec : (int, (bool array array * bool array array * bool array array * bool array array)) Hashtbl.t  = Hashtbl.create 97  in*)
  (*                                                                         *)
  (** Associates each labeled statement to a pre-condition                   *)
  let old_observed_labelled_stmts_pre : (int, (bool array array * bool array array )) Hashtbl.t  = Hashtbl.create 97  in
  let labelled_stmts_pre : (int, (bool array array * bool array array )) Hashtbl.t  = Hashtbl.create 97  in
  (*                                                                         *)
  (** Associates each labeled statement to a post-condition                  *)
  (*let labelled_stmts_post : (int, (bool array array * bool array array )) Hashtbl.t  = Hashtbl.create 97  in*)
  (*                                                                         *)
  (***************************************************************************)
  (* For the pre-condition propagation pass                                  *)
  (*                                                                         *)
  (** During the pre-condition propagation, it represents the set of
      statements that need second computation of specification. For instance,
      it can occurs when the statement is pointed by a goto instruction. *)
  let stmts_to_compute_one_more_time : (int , bool) Hashtbl.t  = 
    Hashtbl.create 97
  in
  (*                                                                         *)
  (***************************************************************************)
  (* For the post-condition propagation pass                                 *)
  (*                                                                         *)
  (** Set of observed labeled statement. This information is used for goto
      treatment.                                                             *)
  let status_of_labelled_stmts : (int , bool) Hashtbl.t  = Hashtbl.create 97 in
  (*                                                                         *)
  (** True if and only if the computation has to be done again in oreder to
      compute a fix-point result. *)
  let second_computation_needed = ref false in
  (*                                                                         *)
  (***************************************************************************)
  (* Used for spec memorization before each call                             *)
  (** Name of the current function.                                          *)
  let currentFuncName = ref "" in
  (*                                                                         *)
  (***************************************************************************)
  (* Used for loop specification                                             *)
  (** Pre or post condition of each loop key stmt, and for FWD and BWD AI    *)
  let loop_fwd_ext_pre   = Hashtbl.create 97 in
  let loop_fwd_int_pre   = Hashtbl.create 97 in
  let loop_fwd_real_post = Hashtbl.create 97 in
  let loop_fwd_int_post  = Hashtbl.create 97 in
  let loop_bwd_ext_pre   = Hashtbl.create 97 in
  let loop_bwd_int_pre   = Hashtbl.create 97 in
  let loop_bwd_real_post = Hashtbl.create 97 in
  let loop_bwd_int_post  = Hashtbl.create 97 in
  (*                                                                         *)
  (* Accessors for loop specification                                        *)
  let get_loop_local_info hashtbl stmt =
    try Hashtbl.find hashtbl stmt
    with _ -> mk_full_pre_or_post_bycase()
  in
	(* status des functions a insérer *)
  (*                                                                         *)
  let update_loop_local_info hashtbl stmt value =
    let info =
      if Hashtbl.mem hashtbl stmt
      then double_bool_array_or_bycase value (Hashtbl.find hashtbl stmt)
      else value
    in
    Hashtbl.replace hashtbl stmt info
  in
  (*                                                                         *)
  (***************************************************************************)


  let update_hashtbl_or hasbtbl key value =
    let new_value =
      if Hashtbl.mem hasbtbl key
      then double_bool_array_or_bycase (Hashtbl.find hasbtbl key) value
      else value
    in
    Hashtbl.replace hasbtbl key new_value
  in
  (** Propagates pre-condition to each statement, by following control flow.
      It returns a couple of bool array, defining the strongest 
      post-condition of the statement list. *)
  let rec propagates_pre kf ki stmt_l (pre_st,pre_tr) =

    (** This function returns the current pre of a statement or an empty
	pre if no specification exists *)
    let get_labelled_stmt_pre stmt_sid =
      try
	let pre_st,pre_tr = Hashtbl.find labelled_stmts_pre stmt_sid in
	pre_st,pre_tr
      with
	| Not_found -> mk_empty_pre_or_post_bycase()
    in



    (** This function makes an OR filter between the given 
        pre and the old pre of the given stmt
        The result is stored as the new pre of the given stmt. *)
    let update_labelled_stmt_pre stmt_sid pre =
      try
	let old_pre = Hashtbl.find labelled_stmts_pre stmt_sid in
	let n_pre = double_bool_array_or_bycase old_pre pre in
	Hashtbl.replace labelled_stmts_pre stmt_sid n_pre
      with
	| _ ->
	    Hashtbl.replace labelled_stmts_pre stmt_sid pre
    in

    (** This function returns the current pre of the given statement.
	WARNING !
	Side effects of this function :
	  * If the statement is in stmts_to_compute_one_more_time 
            then it is removed
	  * The pre of the current stmt is updated according 
            to the current pre_st and pre_tr
    *)
    let update_and_get_stmts_pre stmt_sid with_labels =
      (* If this statement is annotated to be computed 
         again then we remove its annotation. *)
      Hashtbl.remove stmts_to_compute_one_more_time stmt_sid;

      (* Registering the new specification only if 
         it is a stmt with multiple entry points (labelled stmt) *)
      if with_labels then
	begin
	  update_labelled_stmt_pre stmt_sid (pre_st,pre_tr);
	  get_labelled_stmt_pre stmt_sid
	end
      else
	(pre_st,pre_tr)
    in
    (* Updating pre-condition with previous information *)
    let pre_st,pre_tr =
      if stmt_l <>[] then
	let s = (List.hd stmt_l) in
	update_and_get_stmts_pre s.sid (s.labels<>[])
      else
	pre_st,pre_tr
    in
    match stmt_l with
      | [] ->
	  (ki, pre_st,pre_tr)
      | ({skind=Instr(Call(_,{enode =
                               (Lval(Var(vi),_)
                               | CastE(_,{enode = Lval(Var vi,_)})
                               )},_,_))} as stmt)::l ->
	  if (Data_for_aorai.isIgnoredFunction vi.vname) then
	    propagates_pre kf ki l (pre_st,pre_tr)
	  else
	    begin
	      (* If the statement is unreachable then we skip the call *)
	      if (double_bool_array_eq_bycase 
                    (pre_st,pre_tr) (mk_empty_pre_or_post_bycase())) 
              then
  		propagates_pre kf ki l (pre_st,pre_tr)
	      else
		begin
                  let called_kf = Globals.Functions.get vi in
		  (* Simulating crossing transition *)
		  let pre_call=
                    Aorai_utils.get_next_bycase 
                      called_kf Promelaast.Call pre_st 
                  in
		  (* When stmt=call => 
                     the spec has to be memorized as pre of the call *)
		  Data_for_aorai.set_func_pre_call_bycase 
                    !currentFuncName stmt.sid pre_call;
	          (* Registering call context for future reinforcement 
                     of pre-condition. Treatment depends on call recursivity*)
		  update_hashtbl_or functions_pre_usecase vi.vname pre_call;
	          (* From now, pre-condition is the set of configurations 
                     from which the operation is callable according 
                     to its post-condition. 
                   *)
                  let my_ki = Kstmt stmt in
		  let post_state = 
                    mk_forward_composition 
                      kf ki my_ki called_kf
                      pre_st
                      (fst pre_call) 
                      (Data_for_aorai.get_func_post_bycase vi.vname) 
                  in
		  propagates_pre kf my_ki l post_state
		end
	    end



      | {skind=Instr(Call(_,e,_,_))}::_ ->
	  Aorai_option.fatal 
            "Aorai plugin internal error. Status : Operation calls \
             has to be done by explicit operation name (got %a)"
            !Ast_printer.d_exp e
        ;
      | ({skind=Instr (_)})::l ->
	  (* Computes next statements specification *)
	  propagates_pre kf ki l (pre_st,pre_tr)
      | ({skind=Block(b)})::l ->
	  (* Propagation into block *)
	  let (ki,post_st,post_tr) = 
            propagates_pre kf ki b.bstmts (pre_st,pre_tr)
          in
	  (* Computes next statements specification *)
  	  propagates_pre kf ki l (post_st,post_tr)
      | ({skind=If(_,b1,b2,_)} as stmt)::l ->
	  (* Constraints propagation into branches. *)
          let my_ki = Kstmt stmt in
	  let (ki1,post_st1,post_tr1) = 
            propagates_pre kf ki b1.bstmts (pre_st,pre_tr)
          in
	  let (ki2, post_st2, post_tr2) = 
            propagates_pre kf ki b2.bstmts (pre_st,pre_tr)
          in
	  (* The new post-condition is the disjunction of branches 
             post-conditions 
           *)
	  let post =
            double_bool_array_or_bycase 
              (post_st1, post_tr1) (post_st2, post_tr2)
          in
          merge_actions kf my_ki ki1 ki2 post_st1 post_st2;
	  (* Computes next statements specification *)
	  propagates_pre kf my_ki l post
      (* Computation of return transitions is done afterwards, directly in
         the visitor.
         [VP 2011-09-05] Needs more investigation. Not sure the fixpoint
         is reached that easily in interprocedural analysis...
       *)
      | [{skind=Return (_,_)}] -> (ki,pre_st,pre_tr); 
      | {skind=Return _} :: _ ->
        Aorai_option.fatal 
          "Expecting return statement at the end of main block"
      | ({skind=Goto(stmt_ref,_)})::stmt_l ->
	  (* Modifing specification of pointed statement and 
             registering it to be computed *)
	  (* If the statement has not yet been specified *)
	  let ref_pre = get_labelled_stmt_pre !stmt_ref.sid in

	  (* If Current statement is not 
             included into the pointed one, then we update it. *)
	  let disjunction = 
            (double_bool_array_or_bycase ref_pre (pre_st,pre_tr)) 
          in
	  if not (double_bool_array_eq_bycase ref_pre disjunction) then
	    begin
	      (* Updating pre-condition of pointed statement *)
	      update_labelled_stmt_pre !stmt_ref.sid (pre_st,pre_tr);
	      Hashtbl.replace stmts_to_compute_one_more_time !stmt_ref.sid true;
	    end;

	  (* In order to treat statements that are not directly reachable,
	     consumes following statements until a labeled one with a 
             defined pre-condition. *)
(* 	  let _ = propagates_pre stmt_l (mk_empty_pre_or_post_bycase ()) in *)
(* 	  (mk_empty_pre_or_post_bycase ()) *)
	  propagates_pre kf ki stmt_l (mk_empty_pre_or_post_bycase ())

      | ({skind=Loop (_,block,_,_,_)} as stmt)::stmt_l ->
	  (* In a loop we distinguishe 4 cases of pre or post conditions:
	     {pre1}
	     While (1) {
	     {Pre2}
	     ...
	     if (c) {goto Label_end_loop;}
	     ...
	     {Post2}
	     }
	     {Post1}
	     Label_end_loop:
	     {Real_loop_post}


	     Pre1  : pre-condition before entering the loop
	     Pre2  : pre-condition of each iteration
	     Post1 : False (infinite loop)
	     Post2 : Post condition of an iteration


	     State_builder.of conditions :

	     Initially :
 	       Pre1 is given
	       Pre2 = Pre1

	     do
	       Post2 = (Pre2[block])
  	       Pre2 = Pre2 \/ Post2
	     while fix-point not reached.

	     Finally :
	       Real_loop_post  = Pre2 /\ BWDed_real_loop_post
	       and the invariant is:
	         (Init => Pre1)
	       & (not Init => Post2)
	     (where init is a fresh variable to indicate if 
              the iteration is the first one).
	  *)

	  (* Updating pre-conditions with previous information *)
	  let loop_pre = 
            double_bool_array_and_bycase (pre_st,pre_tr) 
              (get_loop_local_info loop_bwd_ext_pre stmt) 
          in
	  let block_pre = loop_pre in
          let loop_ki = Kstmt stmt in
	  (* First forward propagation into block *)
          let (inner_ki, post_st, post_tr) = 
            propagates_pre kf ki block.bstmts block_pre
          in
	  let old_post   = ref block_pre in
	  let block_post = ref(post_st, post_tr) in
	  let block_pre  = 
            ref(double_bool_array_or_bycase block_pre !block_post)
          in
          let action_changed = 
            ref (update_loop_actions kf ki inner_ki loop_ki !block_pre)
          in
	  (* Fix-point computation *)
	  while not (double_bool_array_eq_bycase !old_post !block_post)
            || !action_changed
          do
	    old_post := !block_post;
            let (inner_ki,post_st,post_tr) = 
              propagates_pre kf loop_ki block.bstmts !block_pre
            in
	    block_post:= post_st, post_tr;
	    block_pre :=double_bool_array_or_bycase !block_pre !block_post;
            action_changed:=
              update_loop_actions kf loop_ki inner_ki loop_ki !block_pre
	  done;

	  (* Finally : Real_loop_post  = Pre2 /\ BWDed_real_loop_post *)
	  let real_loop_post =
            double_bool_array_and_bycase 
              !block_pre (get_loop_local_info loop_bwd_real_post stmt)
          in

	  (* Updating loop information *)
          update_loop_local_info loop_fwd_ext_pre stmt loop_pre;
          update_loop_local_info loop_fwd_int_pre stmt !block_pre;
          update_loop_local_info loop_fwd_real_post stmt real_loop_post;
          update_loop_local_info loop_fwd_int_post stmt !block_post;

	  (* Computes next statements specification *)
	  (* The end of the loop is done through a goto instruction that
	     does not appear in the CIL structure. This is why, the
	     post-condition is the exit case of the loop invariant. *)
          propagates_pre kf loop_ki stmt_l (mk_empty_pre_or_post_bycase ())
          (*loop_post_st,loop_post_tr*) 
          (* [VP 2011-09-06] why don't we continue with the state at
             end of loop? *)

      | {skind=UnspecifiedSequence(b)}::l ->
	  let (ki, post_st, post_tr) =
            propagates_pre kf ki
              (Cil.block_from_unspecified_sequence(b)).bstmts (pre_st,pre_tr) 
          in
	  propagates_pre kf ki l (post_st, post_tr)
      | {skind=Switch (_,bl,stmtl,_)}::l  ->
	  (* Step 1 : For each case, 
             the pre-condition is set to pre_st,pre_tr. 
           *)
	  List.iter
	    (fun stmt -> update_labelled_stmt_pre stmt.sid (pre_st,pre_tr))
	    stmtl;

	  (* Step 2 : The block is put into the todo list *)
(* 	  propagates_pre *)
(* 	    ((mkStmt(Block(bl)))::l) *)
(* 	    (pre_st,pre_tr) *)
         (* [VP 2011-09-06] We should not propagate in the block like that.
            It'd be much better to use Cil's Dataflow functor.
          *)
	  let (ki, post_st, post_tr) = 
            propagates_pre kf ki bl.bstmts (pre_st,pre_tr)
          in
	  propagates_pre kf ki l (post_st, post_tr)

      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ ->
	  Aorai_option.fatal 
            "Aorai plugin internal error. \
             Continue and Break statements have to be rewritten \
             into goto by the CFG pass.";

      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ ->
	Extlib.not_yet_implemented 
          "Aorai does not support try constructions yet"

(** Propagates post-condition to each statement, by following control flow.
    It returns a couple of bool array, definig the weakest pre-condition of 
    the statement list. Since then analysis is a backward one, 
    the list is first reversed. *)
(* [VP 2011-09-06] we don't have to intervene here for actions. Just pay
   attention to retrieve actions associated to states that are flagged active
   at the end of this analysis.
*)
  in let rec propagates_post stmt_l (post_st,post_tr) =

    (** This function returns the current spec of a statement or an empty
	spec if no specification exists *)
    let get_labelled_stmt_pre stmt_sid =
      try Hashtbl.find labelled_stmts_pre stmt_sid
      with Not_found -> mk_empty_pre_or_post_bycase()
    in


    (** This function makes an OR filter between the given
        pre and the old pre of the given stmt. The result is 
        stored as the new pre of the given stmt. *)
    let update_labelled_stmt_pre stmt_sid pre =
      let old_pre = get_labelled_stmt_pre stmt_sid in
      let new_pre = double_bool_array_or_bycase old_pre pre in

      Hashtbl.replace labelled_stmts_pre stmt_sid new_pre
    in


    (** This function returns the current spec of the given statement.
	WARNING !
	Side effects of this function :
	  * The pre of the current stmt is updated according to the given pre
    *)
    let update_labelled_stmt_pre stmt pre =
      (* Registering the new pre-condition if the stmt is labelled *)
      if stmt.labels<>[] then
	begin
	  update_labelled_stmt_pre stmt.sid pre;
    	  Hashtbl.replace status_of_labelled_stmts stmt.sid true
	end
    in


    (** Body of propagates_post (after list.rev) *)
    let rec prop  stmt_l (post_st,post_tr) =


      match stmt_l with
	| [] -> (post_st,post_tr)



	| ({skind=Instr(Call(_,{enode =
                                 (Lval(Var(vi),_)
                                 | CastE(_,{enode = Lval(Var vi,_)})
                                 )},_,_))} as stmt)::l ->
	    if (Data_for_aorai.isIgnoredFunction vi.vname) then begin
	      (* Updating the specification of the current stmt in the hashtbl. *)
              update_labelled_stmt_pre stmt (post_st,post_tr);

	      (* Computes next statements specification *)
	      prop l (post_st,post_tr)
	    end
	    else
	      begin

	    (* If statement is unreachable then we skip the call *)
		if (double_bool_array_eq_bycase (post_st,post_tr) (mk_empty_pre_or_post_bycase())) then
 		  prop l (post_st,post_tr)
		else
		  begin
                    let kf = Globals.Functions.get vi in
		    update_hashtbl_or functions_post_usecase 
                      vi.vname (post_st,post_tr);
		    (* From now, post-condition is the set of configurations 
                       from which the operation is callable according to
                       its pre-condition and of the current statement
                       pre-condition. *)
		    let pre_call =
		      mk_backward_composition
			post_st
			(Data_for_aorai.get_func_pre vi.vname)
			(Data_for_aorai.get_func_post_bycase vi.vname)
		    in
		    let cur_pre =
                      Aorai_utils.get_prev_bycase kf Promelaast.Call pre_call
                    in
		    (* When stmt=call => 
                       the spec has to be memorized as pre of the call *)
		    Data_for_aorai.set_func_pre_call_bycase 
                      !currentFuncName stmt.sid cur_pre;
		    (* Updating the specification of the current stmt 
                       in the hashtbl. *)
                    update_labelled_stmt_pre stmt cur_pre;

		    (* Computes next statements specification *)
		    prop l cur_pre
		  end
	      end

      | {skind=Instr(Call(_,_,_,_))}::_ ->
	  Aorai_option.fatal "Indirect calls are not supported yet"

      | ({skind=Instr (_)} as stmt)::l ->
	  (* Updating the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre stmt (post_st,post_tr);
	  (* Computes next statements specification *)
	  prop l (post_st,post_tr)

      | ({skind=Block(b)} as stmt)::l ->
	  (* Computes recursivly the block specification *)
	  let cur_pre = (propagates_post b.bstmts (post_st,post_tr)) in
	  (* Updating the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre stmt cur_pre ;
	  (* Computes next statements specification *)
  	  prop l cur_pre

      | ({skind=If(_,b1,b2,_)} as stmt)::l ->
	  (* Constraints propagation into branches. *)
	  let pre_block1 = propagates_post b1.bstmts (post_st,post_tr) in
	  let pre_block2 = propagates_post b2.bstmts (post_st,post_tr) in
	  (* The new pre-condition is the disjunction of 
             branches pre-conditions *)
	  let pre = double_bool_array_or_bycase pre_block1 pre_block2 in
	  (* Updating the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre stmt pre;
	  (* Computes next statements specification *)
	  prop l pre

      | ({skind=Return (_,_)} as stmt)::l ->
	  (* The 'prev' according to the return will be done be 
             the caller of the propagates function. *)
	  (* Updating the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre stmt (post_st,post_tr);
	  (* Return the post-condition of the current function *)
	  prop l (post_st,post_tr)

      | ({skind=Goto(stmt_ref,_)} as stmt)::stmt_l ->
	  (* Retrieving old specification information about 
             this statement and the pointed one. *)
	  let ref_pre = get_labelled_stmt_pre !stmt_ref.sid in
	  let old_ref_pre =
	    try Hashtbl.find old_observed_labelled_stmts_pre stmt.sid
	    with Not_found -> mk_empty_pre_or_post_bycase()
	  in
	  (* Second computation needed if the pointed stmt has 
             not yet been treated or if its pre differs from the current post *)
	  if not (double_bool_array_eq_bycase ref_pre old_ref_pre) then
	    begin
	      second_computation_needed:= true;
	      Hashtbl.replace old_observed_labelled_stmts_pre stmt.sid ref_pre
	    end;
	  (* Add the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre stmt ref_pre;
	  prop stmt_l ref_pre

      | ({skind=Loop (_,block,_,_,_)} as stmt)::stmt_l ->
	  (* In a loop we distinguishe 4 cases of pre or post conditions:

	     {pre1}
	     While (1) {
	     {Pre2}
	     ...
	     if (c) {goto Label_end_loop;}
	     ...
	     {Post2}
	     }
	     {Post1}
	     Label_end_loop:
	     {RealLoopPost}


	     Pre1  : pre-condition before entering the loop
	     Pre2  : pre-condition of each iteration
	     Post1 : False (Infinite loop)
	     Post2 : Post condition of an iteration
	     RealLoopPost : Real post-condition of the loop


	     We can consider 2 kinds of loop : 
	       * finite
	       * infinite

	     In a finite loop, only external gotos in the block will make the initial specification during the backward process 
	     In an infinite loop, there is no external specification. The only known thing is then the saved internal post-condition.

	     Initially :
	       if finite loop 
                 RealLoopPost = [block]EmptyPost   
  	         Pre2  = RealLoopPost
	         Post2 = Pre2
	       else 
                 RealLoopPost = pre2 from FWD computation   
	         Pre2  = RealLoopPost
	         Post2 = Pre2


	     do
  	       Pre2 = ([block]Post2) \/ Pre2 // Adding reachable pre states
	       Post2 = Pre2 \/ Post2         // Is there any new states ?
	     until fix-point reached.

	     Finally :
	       Pre1 = Pre2

	     The loop invariant is then :
	        (c  => Pre2)
	      & (!c => RealLoopPost)
	      & (Init => Pre1)
	      & (not Init => Post2)
	     (where init is a fresh variable to indicate if the iteration is the first one).

	  *)

          (* First backward propagation into block
	       RealLoopPost = ([block]EmptyPost) /\ FWD_real_post 
	       Pre2  = RealLoopPost
	       Post2 = Pre2
	  *)

	  let loopIsInfinite = ((Stmts_graph.get_all_stmt_last_stmts stmt)=[]) in
	  let real_loop_post  =
	    if loopIsInfinite then begin
	      Aorai_option.debug "Backward parsing infinite loop";
	      (get_loop_local_info loop_fwd_int_pre stmt)
	    end else begin
	      Aorai_option.debug "Backward parsing finite loop";
	      double_bool_array_and_bycase
		(propagates_post block.bstmts (mk_empty_pre_or_post_bycase()))
		(get_loop_local_info loop_fwd_real_post stmt)
	    end;
	  in
	  let block_pre  = real_loop_post in
          let block_post = block_pre in



          (* Looped backward propagation into block
	     do
  	       Pre2 = ([block]Post2) \/ Pre2 // Adding reachable pre states
	       Post2 = Pre2 \/ Post2         // Is there any new states ?
	     until fix-point reached.

	  *)
	  (* Loop initialisation for fix-point computation *)
          let old_pre = ref( mk_empty_pre_or_post_bycase()) in
	  let block_pre = ref block_pre in
	  let block_post = ref block_post in

	  while not (double_bool_array_eq_bycase !old_pre !block_pre) do

	    old_pre    := !block_pre ;
	    block_pre  := propagates_post block.bstmts !block_post ;
            block_pre  := double_bool_array_or_bycase !block_pre !old_pre ;
	    block_post := double_bool_array_or_bycase !block_pre !block_post

	  done;

	  (* The result is dereferenced *)
	  let block_pre = !block_pre in
	  let block_post = !block_post in


          (* Finally : Pre1  = Pre2 /\ FWD_pre1  *)
	  let loop_pre =
            double_bool_array_and_bycase 
              block_pre (get_loop_local_info loop_fwd_ext_pre stmt)
          in
	  (* Updating loop information *)
          update_loop_local_info loop_bwd_ext_pre stmt loop_pre;
          update_loop_local_info loop_bwd_int_pre stmt block_pre;
          update_loop_local_info loop_bwd_real_post stmt real_loop_post;
          update_loop_local_info loop_bwd_int_post stmt block_post;
	  (* Add the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre stmt loop_pre ;
	  (* Computes next statements specification *)
  	  prop stmt_l loop_pre
      | ({skind=UnspecifiedSequence(b)} as stmt)::l ->
	  (* Sequence is packed in a block which is added to the todo list. *)
	  let pre = 
            propagates_post 
              (Cil.block_from_unspecified_sequence(b)).bstmts (post_st,post_tr)
          in
	  (* Add the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre stmt pre ;
	  (* Computes next statements specification *)
	  prop l (post_st,post_tr)
      | ({skind=Switch (_,bl,stmtl,_)} as stmt)::l  ->
	  (* Step 1 : The block is treated by the classical block analysis *)
(* 	  let pre = ref (prop [mkStmt(Block(bl))] (post_st,post_tr)) in *)
(* 	  let _ = propagates_post bl.bstmts (post_st,post_tr) in *)
	  let pre = ref (propagates_post bl.bstmts (post_st,post_tr)) in


	  (* Step 2 : For each case, the current pre-condition is set to pre_case OR cur_pre . *)
(* 	  let pre = ref (mk_empty_pre_or_post_bycase())  *)
	  List.iter
	    (fun stmt ->
	       let stmt_pre = (get_labelled_stmt_pre stmt.sid) in
	       pre:=Spec_tools.double_bool_array_or_bycase stmt_pre !pre)
	    stmtl;

	  (* Add the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre stmt !pre ;
	  (* Computes next statements specification *)
	  prop l !pre
      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ ->
        Extlib.not_yet_implemented 
          "Break and Continue instructions are not yet supported";
      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ ->
        Extlib.not_yet_implemented
	  "try constructions are not yet supported.";
    in
    (* This computation is done from end to beginning *)
    prop (List.rev stmt_l) (post_st,post_tr)

  in

object (self)
  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    currentFuncName:=f.svar.vname;
    let kf = Extlib.the self#current_kf in
    assert (!currentFuncName = Kernel_function.get_name kf);
    let starting_pre  = (Data_for_aorai.get_func_pre f.svar.vname) in
    let starting_post = (Data_for_aorai.get_func_post_bycase f.svar.vname) in
    Aorai_option.debug "Before step 1 for function %s:" f.svar.vname;
    Aorai_utils.debug_display_func_status_bycase f.svar.vname;
    Hashtbl.clear labelled_stmts_pre;
    Hashtbl.clear stmts_to_compute_one_more_time;
    (* Pre-condition forward propagation *)
    let cur_pre = 
      Aorai_utils.mk_pre_or_post_bycase_from_pre_or_post
        (Data_for_aorai.get_func_pre f.svar.vname)
    in
    update_actions_call_func kf cur_pre;
    let res = ref (propagates_pre kf Kglobal f.sbody.bstmts cur_pre) in
    while (Hashtbl.length stmts_to_compute_one_more_time) > 0 do
      res:= propagates_pre kf Kglobal f.sbody.bstmts cur_pre
    done;
    (* Registration of the new post-condition *)
    let (ki, cur_post_st, _) = !res in
    let post     = 
      Aorai_utils.get_next_bycase kf Promelaast.Return cur_post_st 
    in
    let old_post = (Data_for_aorai.get_func_post_bycase f.svar.vname)  in
    let post     = double_bool_array_and_bycase post old_post in
    update_actions_return_func kf ki (snd post);
    Data_for_aorai.set_func_post_bycase f.svar.vname post;
    Aorai_option.debug "Between steps 1 and 2 for function %s:" f.svar.vname;
    Aorai_utils.debug_display_func_status_bycase f.svar.vname;
    (* Post-condition backward propagation *)
    (* cur_post : bool array array * bool array array 
       The first index is in term of the function input states *)
    let cur_post = (Data_for_aorai.get_func_post_bycase f.svar.vname) in
    let cur_post = 
      Aorai_utils.get_prev_bycase kf Promelaast.Return cur_post
    in
    Hashtbl.clear labelled_stmts_pre;
    Hashtbl.clear status_of_labelled_stmts;
    Hashtbl.clear old_observed_labelled_stmts_pre;

    second_computation_needed:=false;
    let cur_pre  = ref (propagates_post f.sbody.bstmts cur_post) in

    while !second_computation_needed do
      Hashtbl.clear status_of_labelled_stmts;
      second_computation_needed:=false;
      cur_pre := propagates_post f.sbody.bstmts cur_post
    done;


    (* Registration of the new pre-condition *)
    let cur_pre = pre_flattening !cur_pre in
    let old_pre = Data_for_aorai.get_func_pre f.svar.vname in
    let pre     = double_bool_array_and cur_pre old_pre in

    Data_for_aorai.set_func_pre f.svar.vname pre;

    Aorai_option.debug "After step 2 for function %s" f.svar.vname;
    Aorai_utils.debug_display_func_status_bycase f.svar.vname;


    let merge tbl1 tbl2 get set =
      Hashtbl.iter
	(fun key value ->
	   let v1 = double_bool_array_and_bycase value (Hashtbl.find tbl2 key) in
	   let v2 = get key in
	   if not( double_bool_array_eq_bycase v2 v1 ) then begin
	     set key ( double_bool_array_and_bycase v2 v1 );
	     spec_modified:=true
	   end
	)
	tbl1
    in
    merge loop_fwd_ext_pre loop_bwd_ext_pre Data_for_aorai.get_loop_ext_pre_bycase Data_for_aorai.set_loop_ext_pre_bycase;
    merge loop_fwd_int_pre loop_bwd_int_pre Data_for_aorai.get_loop_int_pre_bycase Data_for_aorai.set_loop_int_pre_bycase;
    merge loop_fwd_int_post loop_bwd_int_post Data_for_aorai.get_loop_int_post_bycase Data_for_aorai.set_loop_int_post_bycase;
(*     merge loop_fwd_real_post loop_bwd_real_post Data_for_aorai.get_loop_real_post_bycase Data_for_aorai.set_loop_real_post_bycase; *)




(*  Format.printf "Apres passe 2       : "; *)
(*  Aorai_utils.debug_display_func_status_bycase f.svar.vname; *)


    let ending_pre  = (Data_for_aorai.get_func_pre f.svar.vname) in
    let ending_post = (Data_for_aorai.get_func_post_bycase f.svar.vname) in

    if   (not (double_bool_array_eq starting_pre  ending_pre ) )
    then begin spec_modified:=true;  end;
    if   (not (double_bool_array_eq_bycase starting_post ending_post) )
    then begin spec_modified:=true;  end;

    DoChildren
end





let propagates_pre_post_constraints_bycase file root =
  Hashtbl.clear functions_pre_usecase ;
  Hashtbl.clear functions_post_usecase;
  spec_modified:=false;

  let visitor = new visit_propagating_pre_post_constraints_bycase (Data_for_aorai.getAutomata()) in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;

  (* Refining specification according to use-cases. *)
  List.iter
    (fun name ->
       if name <> root then
	 begin
	   let old_pre  = (Data_for_aorai.get_func_pre name) in
	   let old_post = (Data_for_aorai.get_func_post_bycase name) in
	   let used_pre =
	     try Hashtbl.find functions_pre_usecase  name with Not_found -> (mk_empty_pre_or_post_bycase()) in
	   let used_post =
	     try Hashtbl.find functions_post_usecase name with Not_found -> (mk_empty_pre_or_post_bycase()) in


	   (* Reformating usecases of pre and post *)
	   let used_pre_st,used_pre_tr = pre_flattening used_pre in
	   let used_pre_st,used_pre_tr = (ref used_pre_st),(ref used_pre_tr) in

	   let used_post_st,used_post_tr = post_pseudo_flattening used_post in
	   let used_post_st,used_post_tr = 
             (ref used_post_st),(ref used_post_tr)
           in
	   (* Computing new pre/post *)
	   let cur_pre =
             double_bool_array_and (!used_pre_st,!used_pre_tr) old_pre
           in
	   let cur_post =
             double_bool_array_and_bycase 
               (!used_post_st,!used_post_tr) old_post 
           in
	   if   (not (double_bool_array_eq old_pre  cur_pre ) )
	   then begin spec_modified:=true;  end;
	   if   (not (double_bool_array_eq_bycase old_post cur_post) )
	   then begin spec_modified:=true;  end;


	   Data_for_aorai.set_func_pre  name cur_pre;
	   Data_for_aorai.set_func_post_bycase name cur_post
	 end
    )
    (Data_for_aorai.getFunctions_from_c ());

  !spec_modified



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
