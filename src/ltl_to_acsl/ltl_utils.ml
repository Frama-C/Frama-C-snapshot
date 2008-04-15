(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(**************************************************************************)

(* $Id: ltl_utils.ml,v 1.7 2008/11/18 16:37:29 uid562 Exp $ *)

open Cil_types
open Promelaast
open Bool3




(** Given a transition a function name and a function status (call or return) it returns if the cross condition can be statisfied with only function status. *)
let isCrossable tr func st =
  let rec isCross p =
    match p with
      | POr  (c1, c2) -> bool3or (isCross c1) (isCross c2)
      | PAnd (c1, c2) -> bool3and (isCross c1) (isCross c2)
      | PNot (c1) -> bool3not (isCross c1)

      | PCall (s) -> if func=s && st=Call then True else False
      | PReturn (s) -> if func=s && st=Return then True else False
      | PCallOrReturn (s) -> if func=s then True else False

      | PTrue -> True
      | PFalse -> False
(*      | PGt (_,_)
      | PGe (_,_)
      | PLt (_,_)
      | PLe (_,_)
      | PEq (_,_)
      | PNeq (_,_)
      | PBoolVar (_)     -> Undefined*)

      | PIndexedExp (_) -> Undefined
  in
  (isCross tr.cross)<>False


(* ************************************************************************* *)


let debug_display_stmt_pre (pre,_)=
  let r=ref "{" in
  Array.iteri
    (fun i s ->
       if s then
	 begin
	   Format.printf "%s%s" !r (string_of_int i);
	   r:=","
	 end
    )
    pre;
  if !r="{" then
    Format.printf "{}"
  else
    Format.printf "}"


let debug_display_spec (pre_st,pre_tr,post_st,post_tr) name=
  debug_display_stmt_pre(pre_st,pre_tr);
  Format.printf " %s " name;
  debug_display_stmt_pre(post_st,post_tr);
  Format.printf "\n"




let debug_display_stmt_all_pre (st,tr)=
  Format.printf "st=";
  debug_display_stmt_pre(st,tr);
  Format.printf " tr=";
  debug_display_stmt_pre(tr,st)





let debug_display_func_status name =
  let pre = Data_for_ltl.get_func_pre name in
  let post = Data_for_ltl.get_func_post name in
  debug_display_stmt_all_pre pre;
  Format.printf " %s " name;
  debug_display_stmt_all_pre post;
  Format.printf "\n"




(* ************************************************************************* *)

let mk_empty_pre_or_post () =
  (Array.make (Data_for_ltl.getNumberOfStates()) false,
   Array.make (Data_for_ltl.getNumberOfTransitions()) false)

let mk_empty_spec () =
  (Array.make (Data_for_ltl.getNumberOfStates()) false,
   Array.make (Data_for_ltl.getNumberOfTransitions()) false,
   Array.make (Data_for_ltl.getNumberOfStates()) false,
   Array.make (Data_for_ltl.getNumberOfTransitions()) false
  )




(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of reachable states and the second one is the set of crossable transitions. *)
let get_next func status states =
  let st,tr = mk_empty_pre_or_post () in
  let (_,trans_l) = Data_for_ltl.getAutomata() in
    List.iter
      (fun t ->
	 if (states.(t.start.nums)) && (isCrossable t func status) then
	   begin
	     st.(t.stop.nums)<- true ;
	     tr.(t.numt)<- true
	   end
      )
      trans_l;
    (st,tr)



(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of possible initial states and the second one is the set of crossable transitions. *)
let get_prev func status (states,trans) =
  let st,tr = mk_empty_pre_or_post () in
  let (_,trans_l) = Data_for_ltl.getAutomata() in
    List.iter
      (fun t ->
	 if (states.(t.stop.nums)) && (isCrossable t func status) && trans.(t.numt) then
	   st.(t.start.nums)<- true
      )
      trans_l;
    List.iter
      (fun t ->
	 if (st.(t.stop.nums)) (*&& (isCrossable t func status)  <-  We do'nt have the action of prev transitions *) then
	   tr.(t.numt)<- true
      )
      trans_l;
    (st,tr)




(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical OR between cells with same index from the two arrays.  *)
let bool_array_and arr1 arr2 =
  if Array.length arr1 <> Array.length arr2 then
    assert false;
  let res=Array.make (Array.length arr1) false in
  Array.iteri
    (fun i b1 -> if b1 && arr2.(i) then res.(i)<-true)
    arr1;
  res


(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical AND between cells with same index from the two arrays.  *)
let bool_array_or arr1 arr2 =
  if Array.length arr1 <> Array.length arr2 then
    assert false;
  let res=Array.make (Array.length arr1) false in
  Array.iteri
    (fun i b1 -> if b1 || arr2.(i) then res.(i)<-true)
    arr1;
  res


(** Given two bool arrays with the same length, it returns true if and only if their cells are equal for each index. *)
let bool_array_eq arr1 arr2 =
  if Array.length arr1 <> Array.length arr2 then
    assert false;
  let res=ref true in
  Array.iteri
    (fun i b1 -> if b1 <> arr2.(i) then res:=false)
    arr1;
  !res




let double_bool_array_and (a1,a2) (b1,b2) =
  (bool_array_and a1 b1,
   bool_array_and a2 b2)

let quad_bool_array_and (a1,a2,a3,a4) (b1,b2,b3,b4) =
  (bool_array_and a1 b1,
   bool_array_and a2 b2,
   bool_array_and a3 b3,
   bool_array_and a4 b4)

let double_bool_array_or (a1,a2) (b1,b2) =
  (bool_array_or a1 b1,
   bool_array_or a2 b2)
let quad_bool_array_or (a1,a2,a3,a4) (b1,b2,b3,b4) =
  (bool_array_or a1 b1,
   bool_array_or a2 b2,
   bool_array_or a3 b3,
   bool_array_or a4 b4)


let double_bool_array_eq (a1,a2) (b1,b2) =
  (bool_array_eq a1 b1) &&
  (bool_array_eq a2 b2)

let quad_bool_array_eq (a1,a2,a3,a4) (b1,b2,b3,b4) =
  (bool_array_eq a1 b1) &&
  (bool_array_eq a2 b2) &&
  (bool_array_eq a3 b3) &&
  (bool_array_eq a4 b4)








(* ************************************************************************* *)


type pre_post_bycase_t = bool array array
type double_pre_post_bycase_t = (pre_post_bycase_t*pre_post_bycase_t)
type quad_pre_post_bycase_t = (pre_post_bycase_t*pre_post_bycase_t*pre_post_bycase_t*pre_post_bycase_t)


(* ************************************************************************* *)


let mk_empty_pre_or_post_bycase () =
  (Array.make_matrix (Data_for_ltl.getNumberOfStates()) (Data_for_ltl.getNumberOfStates()) false,
   Array.make_matrix (Data_for_ltl.getNumberOfStates()) (Data_for_ltl.getNumberOfTransitions()) false)

let mk_empty_spec_bycase () =
  (Array.make_matrix (Data_for_ltl.getNumberOfStates()) (Data_for_ltl.getNumberOfStates()) false,
   Array.make_matrix (Data_for_ltl.getNumberOfStates()) (Data_for_ltl.getNumberOfTransitions()) false,
   Array.make_matrix (Data_for_ltl.getNumberOfStates()) (Data_for_ltl.getNumberOfStates()) false,
   Array.make_matrix (Data_for_ltl.getNumberOfStates()) (Data_for_ltl.getNumberOfTransitions()) false
  )


let mk_pre_or_post_bycase_from_pre_or_post (st,tr) =
  let st_bc,tr_bc = mk_empty_pre_or_post_bycase () in
  let (_,trans_l) = Data_for_ltl.getAutomata() in
  Array.iteri
    (fun index t -> if st.(index) then t.(index)<-true)
    st_bc;

  List.iter
    (fun t -> if tr.(t.numt) then tr_bc.(t.stop.nums).(t.numt)<-true)
    trans_l;

  (st_bc,tr_bc)



let pre_flattening (pre_st,pre_tr) =
  let new_st,new_tr = mk_empty_pre_or_post () in
  let new_st,new_tr = ref new_st, ref new_tr in
  Array.iteri
    (fun index assocs ->
       new_st:=bool_array_or assocs !new_st ;
       new_tr:=bool_array_or pre_tr.(index) !new_tr
    )
    pre_st;
  (!new_st,!new_tr)


let post_pseudo_flattening post =
  let new_st,new_tr = mk_empty_pre_or_post_bycase () in
  Array.iteri
    (fun index _ ->
       let flat_st,flat_tr=pre_flattening post in
       new_st.(index) <- flat_st;
       new_tr.(index) <- flat_tr
    )
    new_st;
  (new_st,new_tr)

(* ************************************************************************* *)

let is_empty_behavior assocs =
  Array.fold_left (fun b c -> if c then false else b) true assocs

let assocs_to_string assocs =
  let r=ref "(" in
  let s=ref "" in
  Array.iteri
    (fun i b ->
       if b then
	 begin
	   s:=(!s)^(!r)^(string_of_int i);
	   r:=","
	 end
    )
    assocs;
  !s^")"



let debug_display_stmt_pre_bycase (pre,_)=
  let r=ref "{" in
  Array.iteri
    (fun i assocs ->
       if not (is_empty_behavior assocs) then
	 begin
	   Format.printf "%s%s->%s" !r (string_of_int i) (assocs_to_string assocs) ;
	   r:=","
	 end
    )
    pre;
  if !r="{" then
    Format.printf "{}"
  else
    Format.printf "}"



let debug_display_spec_bycase (pre_st,pre_tr,post_st,post_tr) name=
  debug_display_stmt_pre_bycase(pre_st,pre_tr);
  Format.printf " %s " name;
  debug_display_stmt_pre_bycase(post_st,post_tr);
  Format.printf "\n"



let debug_display_stmt_all_pre_bycase (st,tr)=
  Format.printf "st=";
  debug_display_stmt_pre_bycase(st,tr);
  Format.printf " tr=";
  debug_display_stmt_pre_bycase(tr,st)





let debug_display_func_status_bycase name =
  let pre = Data_for_ltl.get_func_pre name in
  let pre = mk_pre_or_post_bycase_from_pre_or_post pre in
  let post = Data_for_ltl.get_func_post_bycase name in

  debug_display_stmt_all_pre_bycase pre;
  Format.printf " %s " name;
  debug_display_stmt_all_pre_bycase post;
  Format.printf "\n"




(* ************************************************************************* *)


(** bool array -> (bool array array*bool array array) -> (bool array*bool array) *)
let compose_assocs_post assocs_st (post_st,post_tr) =
  let st,tr = mk_empty_pre_or_post () in
  let st,tr = ref st, ref tr in
  Array.iteri
    (fun index b ->
       if b then begin
	 st:=bool_array_or post_st.(index) !st;
	 tr:=bool_array_or post_tr.(index) !tr
       end
    )
    assocs_st;

  (!st,!tr)


(** bool array array -> (bool array array*bool array array) -> (bool array array*bool array array)
    Given a set of states and the bycase post-condition of an operation
    this function returns the new pre-condition after the call of the operation in the context of current_st.
*)
let mk_forward_composition current_st post =
  let new_st,new_tr = mk_empty_pre_or_post_bycase () in
  Array.iteri
    (fun index assocs ->
       let s,t = compose_assocs_post assocs post in
       new_st.(index)<-s;
       new_tr.(index)<-t
    )
    current_st;

  (new_st,new_tr)



(** bool array -> (bool array * bool array) (bool array array*bool array array) -> (bool array*bool array) *)
let compose_assocs_pre assocs_st (_,pre_tr) (post_st,_) =
  let st,tr = mk_empty_pre_or_post () in
  let st,tr = ref st, ref tr in
  let (_,trans_l) = Data_for_ltl.getAutomata() in
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





(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of reachable states and the second one is the set of crossable transitions. *)
let get_next_bycase func status states_bycase =
  (* In a first pass we compute all cases of specification (For each starting state, we compute ending states set) *)
  let st_bc,tr_bc = mk_empty_pre_or_post_bycase () in
  let (_,trans_l) = Data_for_ltl.getAutomata() in
    List.iter
      (fun t ->
	 if (isCrossable t func status) then
	   begin
	     st_bc.(t.start.nums).(t.stop.nums)<- true ;
	     tr_bc.(t.start.nums).(t.numt)<- true
	   end
      )
      trans_l;

  (* In second pass we replace each ending state from states_bycase by the new computed one *)
  let res_st_bc,res_tr_bc = mk_empty_pre_or_post_bycase () in
  Array.iteri
    (fun init_st init_st_assocs ->
       Array.iteri
	 (fun end_st b ->
	    if b then
	      begin
		res_st_bc.(init_st) <- (bool_array_or (res_st_bc.(init_st)) (st_bc.(end_st)) );
		res_tr_bc.(init_st) <- (bool_array_or (res_tr_bc.(init_st)) (tr_bc.(end_st)) );
	      end
	 )
	 init_st_assocs;
    )
    states_bycase;

  (res_st_bc,res_tr_bc)





(** Given a function name, is status (call or return) and an array of boolean describing states status, it returns a couple of boolean array. The first one describes the set of possible initial states and the second one is the set of crossable transitions. *)
let get_prev_bycase func status (states_bycase ,transitions_bycase) =
  let res_st_bc,res_tr_bc = mk_empty_pre_or_post_bycase () in
  (* For each starting case, we call the get_prev function *)
  Array.iteri
    (fun case_st case_st_assocs ->
       let prev_st,prev_tr= get_prev func status (case_st_assocs,transitions_bycase.(case_st)) in
       res_st_bc.(case_st) <- prev_st;
       res_tr_bc.(case_st) <- prev_tr
    )
    states_bycase;

  (res_st_bc,res_tr_bc)



(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical OR between cells with same index from the two arrays.  *)
let bool_array_and_bycase bc_arr1 bc_arr2 =
  if Array.length bc_arr1 <> Array.length bc_arr2 then
    assert false;

  let res=Array.make (Array.length bc_arr1) (Array.make (Array.length bc_arr1.(0)) false) in
  Array.iteri
    (fun case b1 -> res.(case)<-bool_array_and b1 (bc_arr2.(case)))
    bc_arr1;
  res



(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical AND between cells with same index from the two arrays.  *)
let bool_array_or_bycase bc_arr1 bc_arr2 =
  if Array.length bc_arr1 <> Array.length bc_arr2 then
    assert false;

  let res=Array.make (Array.length bc_arr1) (Array.make (Array.length bc_arr1.(0)) false) in
  Array.iteri
    (fun case b1 -> res.(case)<-bool_array_or b1 (bc_arr2.(case)))
    bc_arr1;
  res


(** Given two bool arrays with the same length, it returns true if and only if their cells are equal for each index. *)
let bool_array_eq_bycase bc_arr1 bc_arr2 =
  if Array.length bc_arr1 <> Array.length bc_arr2 then
    assert false;

  let res=ref true in
  Array.iteri
    (fun case b1 -> if not (bool_array_eq b1 (bc_arr2.(case))) then res :=false)
    bc_arr1;
  !res







let double_bool_array_and_bycase (a1,a2) (b1,b2) =
  (bool_array_and_bycase a1 b1,
   bool_array_and_bycase a2 b2)

let quad_bool_array_and_bycase (a1,a2,a3,a4) (b1,b2,b3,b4) =
  (bool_array_and_bycase a1 b1,
   bool_array_and_bycase a2 b2,
   bool_array_and_bycase a3 b3,
   bool_array_and_bycase a4 b4)

let double_bool_array_or_bycase (a1,a2) (b1,b2) =
  (bool_array_or_bycase a1 b1,
   bool_array_or_bycase a2 b2)
let quad_bool_array_or_bycase (a1,a2,a3,a4) (b1,b2,b3,b4) =
  (bool_array_or_bycase a1 b1,
   bool_array_or_bycase a2 b2,
   bool_array_or_bycase a3 b3,
   bool_array_or_bycase a4 b4)


let double_bool_array_eq_bycase (a1,a2) (b1,b2) =
  (bool_array_eq_bycase a1 b1) &&
  (bool_array_eq_bycase a2 b2)

let quad_bool_array_eq_bycase (a1,a2,a3,a4) (b1,b2,b3,b4) =
  (bool_array_eq_bycase a1 b1) &&
  (bool_array_eq_bycase a2 b2) &&
  (bool_array_eq_bycase a3 b3) &&
  (bool_array_eq_bycase a4 b4)














(* ************************************************************************* *)



(*open Cil_types*)

(** Given a transition a function name and a function status (call or return) it returns if the cross condition can be statisfied with only function status. *)
let isCrossableAtInit tr func =
  let rec isCross = function
    | POr  (c1, c2) ->
	if not (isCross c1) then (isCross c2) else false
    | PAnd (c1, c2) ->
	if (isCross c1) then (isCross c2) else false
    | PNot (c1) ->
	not (isCross c1)

    | PCall (s) ->
	(func=s)
    | PReturn (_) ->
	false
    | PCallOrReturn (s) ->
	func=s

    | PTrue ->
	true
    | PFalse ->
	false
(*      | PGt (_,_)
      | PGe (_,_)
      | PLt (_,_)
      | PLe (_,_)
      | PEq (_,_)
      | PNeq (_,_)
      | PBoolVar (_)     -> Undefined*)

    | PIndexedExp (e) ->
	(evalExpAtInit (Data_for_ltl.get_exp_from_tmpident e))<>0



  and error_msg msg =
    Format.printf "Ltl_to_acsl plugin internal error. Status : %s. \n" msg;
    assert false

  and evalExpAtInit:Cil_types.exp -> int = function
    | Cil_types.Const (c) ->
	begin
	  match c with
	    | Cil_types.CInt64(int64,_,_) -> Int64.to_int int64
	    | Cil_types.CStr (_)
	    | Cil_types.CWStr(_) -> error_msg "String values not supported into LTL expressions"
	    | Cil_types.CChr(c) -> Char.code c
	    | Cil_types.CReal (_,_,_) -> error_msg "Real values not supported into LTL expressions"
	    | Cil_types.CEnum {eival = exp} -> evalExpAtInit exp
	end


    | Cil_types.Lval (Cil_types.Var(vi),Cil_types.NoOffset) -> get_val_from_vi vi
    | Cil_types.Lval (_) ->
	error_msg "Only simple LVAL supported at this time into LTL expressions"

    | Cil_types.UnOp (unop,exp,typ) ->
	if not (Cil.isIntegralType typ) then
	  error_msg "Such operator not yet supported in LTL expressions"
	else
	  begin
	    match unop with
	      | Cil_types.Neg -> (-(evalExpAtInit exp))
	      | Cil_types.BNot -> error_msg "Bitwise complement not supported in LTL expressions"
	      | Cil_types.LNot -> if (evalExpAtInit exp)=0 then 1 else 0
	  end

    | Cil_types.BinOp (binop,exp1,exp2,typ) ->
	if not (Cil.isIntegralType typ) then
	  error_msg "Such operator not yet supported in LTL expressions"
	else
	  begin
	    match binop with
	      | Cil_types.PlusA -> (evalExpAtInit exp1) + (evalExpAtInit exp2)
	      | Cil_types.MinusA -> (evalExpAtInit exp1) - (evalExpAtInit exp2)
	      | Cil_types.Mult -> (evalExpAtInit exp1) * (evalExpAtInit exp2)
	      | Cil_types.Div  -> (evalExpAtInit exp1) / (evalExpAtInit exp2)
	      | Cil_types.Mod -> error_msg "Modulo not yet supported in LTL expressions"
	      | Cil_types.PlusPI
	      | Cil_types.IndexPI
	      | Cil_types.MinusPI
	      | Cil_types.MinusPP -> error_msg "Pointer and array not yet supported in LTL expressions"
	      | Cil_types.Shiftlt
	      | Cil_types.Shiftrt -> error_msg "Shifts not yet supported in LTL expressions"
	      | Cil_types.Lt -> if (evalExpAtInit exp1) <  (evalExpAtInit exp2) then 1 else 0
	      | Cil_types.Gt -> if (evalExpAtInit exp1) >  (evalExpAtInit exp2) then 1 else 0
	      | Cil_types.Le -> if (evalExpAtInit exp1) <= (evalExpAtInit exp2) then 1 else 0
	      | Cil_types.Ge -> if (evalExpAtInit exp1) >= (evalExpAtInit exp2) then 1 else 0
	      | Cil_types.Eq -> if (evalExpAtInit exp1) =  (evalExpAtInit exp2) then 1 else 0
	      | Cil_types.Ne -> if (evalExpAtInit exp1) <> (evalExpAtInit exp2) then 1 else 0
	      | Cil_types.BAnd
	      | Cil_types.BXor
	      | Cil_types.BOr  -> error_msg "Bitwise operations not supported in LTL expressions"
	      | Cil_types.LAnd -> if (evalExpAtInit exp1)<>0 && (evalExpAtInit exp2)<>0 then 1 else 0
	      | Cil_types.LOr  -> if (evalExpAtInit exp1)<>0 or (evalExpAtInit exp2)<>0 then 1 else 0
	  end



    | Cil_types.Info (exp,_) ->
	evalExpAtInit exp

    | Cil_types.CastE (_,exp) ->
	Format.printf "Warning (Ltl_to_acsl plugin) CastE is not yet fully supported as a valid LTL expression. \n" ;
	evalExpAtInit exp



    | Cil_types.SizeOf (_)
    | Cil_types.SizeOfE (_)
    | Cil_types.SizeOfStr (_) ->
	error_msg "Sizeof is not supported as a valid LTL expression"

    | Cil_types.AlignOf (_)
    | Cil_types.AlignOfE (_) ->
	error_msg "AlignOf is not supported as a valid LTL expression"

    | Cil_types.AddrOf (_) ->
	error_msg "AddrOf is not yet supported as a valid LTL expression"
    | Cil_types.StartOf(_) ->
	error_msg "StartOf is not yet supported as a valid LTL expression"


  and  get_val_from_vi vi =
    try
      let ini=Globals.Vars.find vi in
      match ini.Cil_types.init with
	| None ->  error_msg ("'"^(vi.Cil_types.vname)^"'Seems to not be initialized" )
	| Some (Cil_types.SingleInit(exp)) -> evalExpAtInit exp
	| Some (Cil_types.CompoundInit(_,_)) -> error_msg "Compound values not yet supported into LTL expressions"
    with
      | _ ->
	  error_msg ("initialisation of '"^(vi.Cil_types.vname)^"' not found")



  in
    isCross tr.cross





(* ************************************************************************* *)
(** {b Expressions management} *)
open Logic_const
open Cil_types
open Data_for_ltl



(** Returns an int constant expression which represents the given int value. *)
let mk_int_exp value =
  Const(CInt64(Int64.of_int value,IInt,Some(string_of_int value)))

(** Returns an lval expression which represents the access of the host_name variable (a string) with the offset off_exp (an expression). *)
let mk_offseted_array_lval host_name off_exp =
  let host_lval = (Cil.var (get_varinfo host_name)) in
    Cil.addOffsetLval
      (Index(off_exp,NoOffset))
      host_lval

(** Returns an lval expression which represents the access of the host_name variable (a string) with the offset off_value (an int). *)
let mk_int_offseted_array_lval host_name off_value =
  mk_offseted_array_lval host_name (mk_int_exp off_value)




(** This function rewrite a cross condition into a Cil expression.
    Moreover, by giving current operation name and its status (call or return) the generation simplifies the generated expression. *)
let crosscond_to_exp cross func status  =
  (* TODO : Consider particular cases of result during return and parameters during call *)
  let false_exp = Const(CInt64(Int64.of_int 0,IInt,Some("0"))) in
  let true_exp = Const(CInt64(Int64.of_int 1,IInt,Some("1"))) in
  let rec convert : Promelaast.condition -> Bool3.bool3 * Cil_types.exp = function
    (* Lazy evaluation of logic operators if the result can be statically computed *)
    | POr  (c1, c2) -> (*BinOp(LOr,convert c1,convert c2,Cil.intType)*)
	begin
	  let (c1_val,c1_exp) = convert c1 in
	  match c1_val with
            | Bool3.True      -> (c1_val,c1_exp)
	    | Bool3.False     -> convert c2
	    | Undefined ->
		let (c2_val,c2_exp) = convert c2 in
		match c2_val with
                  | Bool3.True      -> (c2_val,c2_exp)
		  | Bool3.False     -> (c1_val,c1_exp)
		  | Undefined -> (Undefined,BinOp(LOr,c1_exp,c2_exp,Cil.intType))
	end

    | PAnd (c1, c2) -> (*BinOp(LAnd,convert c1,convert c2,Cil.intType)*)
	begin
	  let (c1_val,c1_exp) = convert c1 in
	  match c1_val with
            | Bool3.True      -> convert c2
	    | Bool3.False     -> (c1_val,c1_exp)
	    | Undefined ->
		let (c2_val,c2_exp) = convert c2 in
		match c2_val with
                  | Bool3.True      -> (c1_val,c1_exp)
		  | Bool3.False     -> (c2_val,c2_exp)
		  | Undefined -> (Undefined,BinOp(LAnd,c1_exp,c2_exp,Cil.intType))
	end

    | PNot (c1)     -> (*UnOp(LNot,convert c1,Cil.intType)*)
	begin
	  let (c1_val,c1_exp) = convert c1 in
	  match c1_val with
            | Bool3.True      -> (Bool3.False,false_exp)
	    | Bool3.False     -> (Bool3.True,true_exp)
	    | Undefined -> (c1_val,UnOp(LNot,c1_exp,Cil.intType))
	end

    (* Call and return are statically defined *)
    | PCall (s) ->
	if(s=func) && (status=Promelaast.Call) then
	  (Bool3.True, true_exp)
	else
	  (Bool3.False,false_exp)


    | PReturn (s) ->
	if(s=func) && (status=Promelaast.Return) then
	  (Bool3.True,true_exp)
	    (*	   snd (convert(PAnd(
		   PEq(PVar(s),PVar(curOp)),
		   PEq(PVar(callStatus),PVar(curOpStatus))
		   ))))*)
	else
	  (Bool3.False,false_exp)



    | PCallOrReturn (s) ->
	if(s=func) then
	  (Bool3.True,true_exp)
	    (*	   snd (convert(PEq(PVar(s),PVar(curOp)))))*)
	else
	  (Bool3.False,false_exp)




    (* Other expressions are left unchanged *)
    | PTrue -> (Bool3.True, true_exp)
    | PFalse -> (Bool3.False, false_exp)

    | PIndexedExp(s) -> (Undefined,get_exp_from_tmpident s)

(*    | PGt (c1,c2)  -> (Undefined,BinOp(Gt,convert_arith c1,convert_arith c2,Cil.intType))
    | PGe (c1,c2)  -> (Undefined,BinOp(Ge,convert_arith c1,convert_arith c2,Cil.intType))
    | PLt (c1,c2)  -> (Undefined,BinOp(Lt,convert_arith c1,convert_arith c2,Cil.intType))
    | PLe (c1,c2)  -> (Undefined,BinOp(Le,convert_arith c1,convert_arith c2,Cil.intType))
    | PEq (c1,c2)  -> (Undefined,BinOp(Eq,convert_arith c1,convert_arith c2,Cil.intType))
    | PNeq (c1,c2) -> (Undefined,BinOp(Ne,convert_arith c1,convert_arith c2,Cil.intType))

    | PBoolVar (s) -> (Undefined,Lval(Cil.var(get_varinfo s))) *)
  in
  try
    (*let (_,res) = *)
      convert cross (*in
    res*)
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Not_found exception during exp conversion.\n";
	assert false



























(** This function rewrite a cross condition into a Cil expression.
    Moreover, by giving current operation name and its status (call or return) the generation simplifies the generated expression. *)
let crosscond_to_pred cross op_logic_var status_logic_var  =
  (* TODO : Consider particular cases of result dureing return and parameters during call *)
  let rec convert : Promelaast.condition -> Bool3.bool3 * Cil_types.predicate = function
    (* Lazy evaluation of logic operators if the result can be statically computed *)
    | POr  (c1, c2) -> (*BinOp(LOr,convert c1,convert c2,Cil.intType)*)
	begin
	  let (c1_val,c1_pred) = convert c1 in
	  match c1_val with
            | Bool3.True      -> (c1_val,c1_pred)
	    | Bool3.False     -> convert c2
	    | Undefined ->
		let (c2_val,c2_pred) = convert c2 in
		match c2_val with
                  | Bool3.True      -> (c2_val,c2_pred)
		  | Bool3.False     -> (c1_val,c1_pred)
		  | Undefined -> (Undefined,Por(unamed c1_pred, unamed c2_pred))
	end

    | PAnd (c1, c2) -> (*BinOp(LAnd,convert c1,convert c2,Cil.intType)*)
	begin
	  let (c1_val,c1_pred) = convert c1 in
	  match c1_val with
            | Bool3.True      -> convert c2
	    | Bool3.False     -> (c1_val,c1_pred)
	    | Undefined ->
		let (c2_val,c2_pred) = convert c2 in
		match c2_val with
                  | Bool3.True      -> (c1_val,c1_pred)
		  | Bool3.False     -> (c2_val,c2_pred)
		  | Undefined -> (Undefined,Pand(unamed c1_pred, unamed c2_pred))
	end

    | PNot (c1)     -> (*UnOp(LNot,convert c1,Cil.intType)*)
	begin
	  let (c1_val,c1_pred) = convert c1 in
	  match c1_val with
            | Bool3.True      -> (Bool3.False,Pfalse)
	    | Bool3.False     -> (Bool3.True,Ptrue)
	    | Undefined -> (c1_val,Pnot(unamed c1_pred))
	end

    (* Call and return are statically defined *)
    | PCall (s) ->
	(Undefined,
	 Pand(
	   unamed(
	     Prel(Req,
		  mk_dummy_term (TLval(TVar(op_logic_var),TNoOffset)) Cil.intType,
		  mk_dummy_term (TConst(func_to_cenum  s)) Cil.intType
		 )
	   ),
	   unamed (
	     Prel(Req,
		  mk_dummy_term (TLval(TVar(status_logic_var),TNoOffset)) Cil.intType,
		  mk_dummy_term (TConst(op_status_to_cenum Promelaast.Call)) Cil.intType
		 )
	   )
	 )
	)



    | PReturn (s) ->
	(Undefined,
	 Pand(
	   unamed(
	     Prel(Req,
		  mk_dummy_term (TLval(TVar(op_logic_var),TNoOffset)) Cil.intType,
		  mk_dummy_term (TConst(func_to_cenum s)) Cil.intType
		 )
	   ),
	   unamed (
	     Prel(Req,
		  mk_dummy_term (TLval(TVar(status_logic_var),TNoOffset)) Cil.intType,
		  mk_dummy_term (TConst(op_status_to_cenum Promelaast.Return)) Cil.intType
		 )
	   )
	 )
	)



    | PCallOrReturn (s) ->
	(Undefined,
	 Prel(Req,
	      mk_dummy_term (TLval(TVar(op_logic_var),TNoOffset)) Cil.intType,
	      mk_dummy_term (TConst(func_to_cenum s)) Cil.intType
	     )
	)




    (* Other expressions are left unchanged *)
    | PTrue -> (Bool3.True, Ptrue)
    | PFalse -> (Bool3.False, Pfalse)

    | PIndexedExp(s) -> (Undefined,get_pred_from_tmpident s)
  in
  try
    let (_,res) = convert cross in
    res
  with
    | _ ->
	Format.printf "Ltl_to_acsl plugin internal error. Status : Not_found exception during term conversion.\n";
	assert false





(* ************************************************************************* *)
(** {b Buchi automata and C code synchronisation } *)

let rec mk_expr_disjunction expr_l =
  match expr_l with
    | [] -> assert false
    | expr::[] -> expr
    | expr::l -> BinOp(LOr, expr,mk_expr_disjunction l,Cil.intType)


let conj_crosscond_old (value,cross) expr =
  if value=Bool3.True
  then expr
  else BinOp (LAnd,cross,expr,Cil.intType)




(** Computed formula : OR(tr)  (crosscond(tr) && i==curStateTMP[transStart(tr)])*)
(** It remains only to affect this result to curState[state]*)
let upd_one_state trans_l statenum func status loc =
  let expr_l=ref [] in
  List.iter
    (fun tr ->
       if (statenum=tr.stop.nums) && (isCrossable tr func status) then
	 expr_l:=
	   (conj_crosscond_old
	      (crosscond_to_exp tr.cross func status)
	      (Lval(mk_int_offseted_array_lval curStateOld tr.start.nums)))::!expr_l
    )
    trans_l;

  let expr =
    if !expr_l=[] then mk_int_exp 0
    else mk_expr_disjunction !expr_l
  in
  Cil_types.Set(
    (mk_int_offseted_array_lval curState statenum),
    expr,
    loc
  )



(** Computed formula : crosscond(trans) && curStateTMP[transStart(trans)] && curState[transStop(trans)]*)
(** It remains only to affect this result to curTrans[trans]*)
let upd_one_trans trans func status loc =
  let expr=
    if isCrossable trans func status then
(*      BinOp(LAnd,
	    crosscond_to_exp trans.cross func status,
	    BinOp(LAnd,
		  Lval(mk_int_offseted_array_lval curStateOld trans.start.nums),
		  Lval(mk_int_offseted_array_lval curState trans.stop.nums),
		  Cil.intType),
	    Cil.intType)
*)


      conj_crosscond_old
	(crosscond_to_exp trans.cross func status)
	(Lval(mk_int_offseted_array_lval curStateOld trans.start.nums))


    else
      (mk_int_exp 0)
  in
    Cil_types.Set(
      (mk_int_offseted_array_lval curTrans trans.numt),
      expr,
      loc
    )


(** This function returns the list of instructions that have to be introduced just before each call of function and each return of function. These instructions correspond to the synchronisation between C code and Buchi automata. The parameters are :
  + The buchi automata
  + the name of the function that is called or that returns
  + the status of this action (call or return)
  + the localisation associated to this generated code*)
let synch_upd_linear (state_l,trans_l) func status loc =
  (* WARNING ! Notice that the order of these operations is very important.

     Step 1 has to be done after the Step 0 and before the steps 2 and 3.
     Step 4 has to be done after the Step 3.
  *)


  (* Step 0 : define new value of current operation and its status *)
  let inst_curop_upd =
    Set(
      Cil.var (get_varinfo curOp),
      Const(func_to_cenum (func)),
      loc
    )
  in
  let inst_curopstatus_upd =
    Set(
      Cil.var (get_varinfo curOpStatus),
      Const(op_status_to_cenum status),
      loc
    )
  in

  (* Step 1 : update of Old states *)
  let step_one_inst =
    List.fold_left
      (fun inst_l st -> (
	 Cil_types.Set(
	   (mk_int_offseted_array_lval curStateOld st.nums),
	   Lval(mk_int_offseted_array_lval curState st.nums),
	   loc
	 ))::inst_l)
      (inst_curopstatus_upd::[inst_curop_upd])
      state_l
  in

  (* Step 2 : Computation of reachable states *)
  let step_two_inst =
    List.fold_left
      (fun inst_l st -> (upd_one_state trans_l st.nums func status loc)::inst_l)
      step_one_inst
      state_l
  in

  (* Step 3 : Computation of crossable transitions *)
  let step_three_inst =
    List.fold_left
      (fun inst_l tr -> (upd_one_trans tr func status loc)::inst_l)
      step_two_inst
      trans_l
  in

(*  (* Step 4 : update of Tmp transitions *)
  let step_four_inst =
    List.fold_left
      (fun inst_l tr -> (
	 Cil_types.Set(
	   (mk_int_offseted_array_lval curTransTmp tr.numt),
	   Lval(mk_int_offseted_array_lval curTrans tr.numt),
	   loc
	 ))::inst_l)
      step_three_inst
      trans_l
  in

  step_four_inst*)

  step_three_inst




(*

(** This function returns the list of instructions that have to be introduced just before each call of function and each return of function. These instructions correspond to the synchronisation between C code and Buchi automata. The parameters are :
  + The buchi automata
  + the name of the function that is called or that returns
  + the status of this action (call or return)
  + the localisation associated to this generated code*)
let synch_upd_loops (state_l,trans_l) func status loc =
  (* WARNING ! Notice that the order of these operations is very important.

     Step 1 has to be done after the Step 0 and before the steps 2 and 3.
     Step 4 has to be done after the Step 3.
  *)

  let vi_iter = make_local_tmp func in
  let vi_tmp = make_local_iter func in

  (* Step 0 : define new value of current operation and its status *)
  let stmt_curop_upd =
    Cil.mkStmtOneInstr (Set(
      Cil.var (get_varinfo curOp),
      Const(func_to_cenum (func)),
      loc
    ))
  in
  let stmt_curopstatus_upd =
    Cil.mkStmtOneInstr (Set(
      Cil.var (get_varinfo curOpStatus),
      Const(op_status_to_cenum status),
      loc
    ))
  in
  (* Step 1 : update of Old states and reset of states *)
  let loop_body =
    [Cil.mkStmtOneInstr (Cil_types.Set(
			   (mk_offseted_array_lval curStateOld Lval(Cil.var vi_iter)),
			   Lval(mk_offseted_array_lval curState Lval(Cil.var vi_iter)),
			   loc
			 ));
     Cil.mkStmtOneInstr (Cil_types.Set(
			   (mk_offseted_array_lval curState Lval(Cil.var vi_iter)),
			   Lval(mk_int_exp 0),
			   loc
			 ))
    ]
  in
  let step_one_stmt =
    (Cil.mkForIncr
       vi_iter
       (Lval(mk_int_exp 0))
       (Lval(mk_int_exp (getNumberOfStates ()) ))
       (Lval(mk_int_exp 1))
       loop_body)
    @
      [stmt_curopstatus_upd;
       stmt_curop_upd]
  in


  (* Step 2 : Computation of crossable transitions and reachable states *)
  let loop_body =
    [Cil.mkStmtOneInstr (Cil_types.Set(
			   Cil.var vi_tmp,
			   BinOp(BAnd,
...........
			 ));
     Cil.mkStmtOneInstr (Cil_types.Set(
			   (mk_offseted_array_lval curStateOld Lval(Cil.var vi_iter)),
			   Lval(mk_offseted_array_lval curState Lval(Cil.var vi_iter)),
			   loc
			 ));
     Cil.mkStmtOneInstr (Cil_types.Set(
			   (mk_offseted_array_lval curState Lval(Cil.var vi_iter)),
			   Lval(mk_int_exp 0),
			   loc
			 ))
    ]
  in


  step_three_inst

*)





(** This function returns the list of instructions that have to be introduced just before each call of function and each return of function. These instructions correspond to the synchronisation between C code and Buchi automata. The parameters are :
  + The buchi automata
  + the name of the function that is called or that returns
  + the status of this action (call or return)
  + the localisation associated to this generated code*)
let synch_upd (state_l,trans_l) func status loc =
  (*if true
  then synch_upd_loops (state_l,trans_l) func status loc
  else *)
  synch_upd_linear (state_l,trans_l) func status loc



(* ************************************************************************* *)
(** {b Globals management} *)

(** Local copy of the file pointer *)
let file = ref Cil.dummyFile

(** Copy the file pointer locally in the class in order to easiest globals management and initializes some tables. *)
let initFile f =
  file:=f ;
  Data_for_ltl.setCData ();
  (* Adding C variables into our hashtable *)
  Globals.Vars.iter (fun vi _ -> set_varinfo vi.vname vi
		    (*   ;
		       Format.printf" -> global variables added into hashtable: '%s' \n" vi.vname*)
		    )


(** List of globals awaiting for adding into C file globals *)
let globals_queue = ref []

(** Flush all queued globals declarations into C file globals. *)
let flush_globals () =
  let (before,after)=List.fold_left
    (fun (b,a) elem ->
	match elem with
	  | GFun(_,_) as func -> (b,func::a)
	  | _ as other -> (other::b,a)
    )
    ([],[])
    !file.globals in

  !file.globals <- (List.rev before)@(List.rev !globals_queue)@(List.rev after);
(*  !file.globals <- (List.rev !globals_queue)@(!file.globals);*)
  globals_queue:=[]





(* Utilities for global variables *)
let mk_global_c_initialized_vars name ty ini=
  let vi = (Cil.makeGlobalVar ~logic:true name ty) in
    vi.vghost<-true;
    globals_queue:=GVar(vi,ini,vi.vdecl)::(!globals_queue);
    Globals.Vars.add vi ini;
    set_varinfo name vi


let mk_global_c_vars name ty =
  let vi = (Cil.makeGlobalVar ~logic:true name ty) in
    vi.vghost<-true;
    let ini = {init=Some(Cil.makeZeroInit ty)} in
      globals_queue:=GVar(vi,ini,vi.vdecl)::(!globals_queue);
      Globals.Vars.add vi ini;
      set_varinfo name vi


let mk_int_const value =
  Const(
    CInt64(
      Int64.of_int (value),
      IInt,
      Some(string_of_int(value))
    )
  )

let mk_global_c_initialized_array name size init =
  let ty =
    (TArray(
       TInt(IInt,[]),
       Some(mk_int_const size),
       []
     ))
  in
    mk_global_c_initialized_vars name ty init

let mk_global_c_array name size =
  let ty =
    (TArray(
       TInt(IInt,[]),
       Some(mk_int_const size),
       []
     ))
  in
    mk_global_c_vars name ty


let mk_global_c_int name  =
  let ty = (TInt(IInt,[])) in
    mk_global_c_vars name ty






(* Utilities for global enumerations *)


let mk_global_c_enum_type name elements_l =
  let i = ref 0 in
   let einfo = {
    ename=name;
    eitems=[];
    eattr=[];
    ereferenced=true
   }
   in
   let l = List.map
     (fun e ->
        i:=!i+1;
        { einame = e;
          eival = mk_int_const(!i-1);
          eiloc = Cilutil.locUnknown;
          eihost = einfo})
     elements_l
  in
   einfo.eitems <- l;
   set_usedinfo name einfo;
   globals_queue:=GEnumTag(einfo,Cilutil.locUnknown)::(!globals_queue)


let mk_global_c_enum name name_enuminfo =
  mk_global_c_vars name (TEnum(get_usedinfo name_enuminfo,[]))


let mk_global_c_initialized_enum name name_enuminfo ini =
  mk_global_c_initialized_vars name (TEnum(get_usedinfo name_enuminfo,[])) ini



(* ************************************************************************* *)
(** {b Terms management / computation} *)

(** Return an integer constant term from the given value. *)
let mk_int_term value =
  mk_dummy_term
    (TConst( CInt64(Int64.of_int value,IInt,Some(string_of_int value))))
    Cil.intType

(** Return an integer constant term with the 0 value. *)
let zero_term() =
  mk_int_term 0

(** Returns a term representing the given logic variable (usually a fresh quantified variable). *)
let mk_term_from_logic_var lvar =
  mk_dummy_term (TLval(TVar(lvar),TNoOffset)) Cil.intType


(** Returns a term representing the variable associated to the given varinfo *)
let mk_term_from_vi vi =
  mk_dummy_term (TLval((Logic_const.lval_to_term_lval (Cil.var vi)))) Cil.intType


(** Given an lval term 'host' and an integer value 'off', it returns a lval term host[off]. *)
let mk_offseted_array host off =
  mk_dummy_term
    (TLval(Cil.addTermOffsetLval (TIndex(mk_int_term (off),TNoOffset)) host))
    Cil.intType


(** Given an lval term 'host' and a term 'term_off', it returns a lval term host[off]. *)
let mk_offseted_array_lval_from_term host term_off =
  mk_dummy_term
    (TLval(Cil.addTermOffsetLval (TIndex(term_off,TNoOffset)) host))
    Cil.intType


(** Given an lval term 'host' and a logic variable 'lvar_off', it returns a lval term host[off].
    Usually, logic variables stand for fresh quantified variables. *)
let mk_offseted_array_lval_from_lval host lvar_off =
  mk_offseted_array_lval_from_term host (mk_term_from_logic_var lvar_off)
(*  mk_dummy_term
    (TLval(Cil.addTermOffsetLval (TIndex(mk_term_from_logic_var lvar_off,TNoOffset)) host))
    Cil.intType
*)


(** Given the name of a logic and a list of logic variables it returns a call of the logic with variables as parameters. *)
let mk_logic_call name logicvar_param_l =
  mk_dummy_term
    (Tapp(get_logic name,[],List.map (fun p -> mk_term_from_logic_var p) logicvar_param_l))
    Cil.intType


(** Returns a lval term associated to the curState generated variable. *)
let host_state_term () =
  lval_to_term_lval (Cil.var (get_varinfo curState))

(** Returns a lval term associated to the curStateOld generated variable. *)
let host_stateOld_term () =
  lval_to_term_lval (Cil.var (get_varinfo curStateOld))

(** Returns a lval term associated to the curTrans generated variable. *)
let host_trans_term () =
  lval_to_term_lval (Cil.var (get_varinfo curTrans))



(** Given a logic variable and two bounces, it returns the predicate: min<=v<max *)
let mk_logicvar_intervalle logvar min max =
  Pand(
    unamed (Prel(Rle,mk_int_term min,mk_term_from_logic_var logvar)),
    unamed (Prel(Rlt,mk_term_from_logic_var logvar,mk_int_term max))
  )

(** Given two names of generated arrays and their size, it returns the predicate: (forall i. 0<=i<size => host1[i]==host2[i]) *)
let mk_eq_tables host_name1 host_name2 size =
  let lval1 = lval_to_term_lval ( Cil.var (get_varinfo host_name1)) in
  let lval2 = lval_to_term_lval ( Cil.var (get_varinfo host_name2)) in
  let tmp_i = Cil.make_logic_var "_buch_i" Cil_types.Linteger in
    Pforall([tmp_i],
	    unamed (
	      Pimplies (
		unamed ( mk_logicvar_intervalle tmp_i 0 size),
		unamed (
		  Prel(Req,
		       mk_offseted_array_lval_from_lval lval1 tmp_i ,
		       mk_offseted_array_lval_from_lval lval2 tmp_i
		      )
		)
	      ))
	   )


(** Given a name of generated array and its size, it returns the expression: (Valide_range(name,0,size-) *)
let mk_valid_range name size =
  let lval = lval_to_tsets_lval ( Cil.var (get_varinfo name)) in
  let min = mk_int_term 0 in
  let max = mk_int_term (size-1) in
    Pvalid(TSSingleton(TSAdd_range(TSLval(lval),Some(min),Some(max))))

(** Given a NON EMPTY list of predicates, it returns a conjunction of these predicates. *)
let rec mk_conjunction pred_l =
  match pred_l with
    | [] -> assert false
    | pred::[] -> pred
    | pred::l -> Pand (unamed pred,unamed (mk_conjunction l))

let rec mk_conjunction_named pred_l =
  match pred_l with
    | [] -> assert false
    | pred::[] -> pred
    | pred::l ->unamed ( Pand (pred,(mk_conjunction_named l)) )


(** Given a NON EMPTY list of predicates, it returns a disjunction of these predicates. *)
let rec mk_disjunction pred_l =
  match pred_l with
    | [] -> assert false
    | pred::[] -> pred
    | pred::l -> Por (unamed pred,unamed (mk_disjunction l))

let rec mk_disjunction_named pred_l =
  match pred_l with
    | [] -> assert false
    | pred::[] -> pred
    | pred::l -> unamed (Por ( pred, (mk_disjunction_named l)))

(* Utilities for other globals *)


let mk_global_invariant pred name =
  globals_queue:=GAnnot (Cil_types.Dinvariant(
			   {
   			     l_name=name;
   			     l_profile = [];
   			     l_labels = [];
(*   			     l_body = PDefinition(unamed pred);*)
   			     l_body = LBpred(unamed pred);
			     l_type = None;
			     l_tparams = []
			   }
			 ), Cilutil.locUnknown)::(!globals_queue)


let mk_global_comment txt =
  globals_queue:=GText (txt)::(!globals_queue)


(** Given
      + the name of the logic (string),
      + the list of its genericity parameter names (string),
      + the list of their type (logic_var),
      + the type of the function return
      + and a list of reads tsets,
    it returns a logic function declaration.
    A side effect of this function is the registration of this logic into the logics hashtbl from Data_for_ltl. *)
let mk_global_logic name (*generics_l*) types_l type_ret (*reads*) =
  let log_info = {
    l_name = name; 	(*	name of the function.	*)
    l_type = type_ret;	(*	return type.	*)
    l_profile = types_l;(*	type of the arguments.	*)
    l_labels = []; 	(*	label arguments of the function. *)
    l_body = LBreads([]); (*	body of the function.	*)
    l_tparams = []
  }  in
  Data_for_ltl.add_logic name log_info;
  Dfun_or_pred(log_info)


let mk_global_axiom name pred =
  Dlemma (name, true, [], [], unamed pred)




let mk_global_predicate name moment params_l pred =
  (*let log_var_params = List.map (fun p -> Cil.make_logic_var p Linteger) params_l in *)
  let pred_info={
    l_name =name;                                 (*	name of the predicate.	*)
    l_profile = params_l; (*log_var_params;*)     (*	arguments of the predicate.	*)
    l_labels = List.map (fun x -> LogicLabel(x)) moment;              (*	label arguments.	*)
    l_body = LBpred(unamed pred); (*	definition.	*)
    l_type = None;	(*	return type.	*)
    l_tparams = []
  } in
    Data_for_ltl.add_predicate name pred_info;
    globals_queue:=GAnnot(
      Dfun_or_pred(pred_info),
      (*Dpredicate_def (pred_info,
		      [] (*moment*),
		      pred_info.p_profile,
		      unamed pred
		     ),*)
      Cilutil.locUnknown
    )::(!globals_queue)


(** Generates an axiomatisation of transitions from automata into globals.
    These annotations are used to express some pre and post condition properties *)
let mk_decl_axiomatized_auotmata () =
  let (_,trans_l) = getAutomata() in
  let param=(Cil.make_logic_var "tr" Linteger) in
  let logic=mk_global_logic transStart (*[]*) [param] (Some Linteger) (*[]*) (*[TSSingleton(TSLval(TSVar(param),TSNo_offset))]*) in
  let tr_start_log_info = Data_for_ltl.get_logic transStart in
  let annotlist=List.fold_left
    (fun res tr ->
       (mk_global_axiom
	  (transStart^(string_of_int tr.numt))
	  (Prel(Req,
		mk_dummy_term
		  (Tapp(tr_start_log_info,[],[mk_int_term tr.numt]))
		  Cil.intType,
		mk_int_term tr.start.nums))
       )::res
    )
    [logic]
    trans_l in
  globals_queue:=(GAnnot(Daxiomatic(transStart,List.rev annotlist),
		  Cilutil.locUnknown
		 ))::(!globals_queue);


  let logic=mk_global_logic transStop  (*[]*) [param] (Some Linteger) (*[]*) (*[TSSingleton(TSLval(TSVar(param),TSNo_offset))]*) in
  let tr_stop_log_info = Data_for_ltl.get_logic transStop in
  let annotlist=List.fold_left
    (fun res tr ->
       (mk_global_axiom
	 (transStop^(string_of_int tr.numt))
	 (Prel(Req,
	       mk_dummy_term
		 (Tapp(tr_stop_log_info,[],[mk_int_term tr.numt]))
		 Cil.intType,
	       mk_int_term tr.stop.nums))
       )::res
    )
    [logic]
    trans_l in

  globals_queue:=(GAnnot(Daxiomatic(transStop,List.rev annotlist),
			 Cilutil.locUnknown
			))::(!globals_queue);






  let num= Cil.make_logic_var "_buch_numTrans" Linteger in
  let op = Cil.make_logic_var "_buch_op" Linteger in
  let st = Cil.make_logic_var "_buch_status" Linteger in
  let pred =
    mk_conjunction
      (List.map
	 (fun tr ->
	    Pimplies(
	      unamed (Prel(Req, mk_term_from_logic_var num, mk_int_term tr.numt)),
	      unamed (crosscond_to_pred tr.cross op st)
	    )
	 )
	 trans_l
      )
  in
  mk_global_predicate transCondP ["L"] [num;op;st] pred;

  let pred2 =
    Papp(
      Data_for_ltl.get_predicate transCondP,
      [(LogicLabel("L"),LogicLabel("L"))],
      [mk_term_from_logic_var num;
       mk_term_from_logic_var (Cil.cvar_to_lvar (Data_for_ltl.get_varinfo curOp));
       mk_term_from_logic_var (Cil.cvar_to_lvar (Data_for_ltl.get_varinfo curOpStatus))
      ]
    )
  in
  mk_global_predicate transCond ["L"] [num] pred2





(* ************************************************************************* *)
(** {b Initialization management / computation} *)


let get_states_trans_init root =
  let (states,trans) = Data_for_ltl.getAutomata () in
  let st_old_exps = (Array.make (List.length states) (mk_int_exp 0)) in
  let st_exps = (Array.make (List.length states) (mk_int_exp 0)) in
  let tr_exps = (Array.make (List.length trans ) (mk_int_exp 0)) in
  let acc_exps = (Array.make (List.length states) (mk_int_exp 0)) in

  List.iter (
    fun tr ->
      if (tr.start.Promelaast.init==Bool3.True) && (isCrossableAtInit tr root) then
	begin
	  Array.set tr_exps tr.numt (mk_int_exp 1);
	  Array.set st_exps tr.stop.nums (mk_int_exp 1);
	  Array.set st_old_exps tr.start.nums (mk_int_exp 1)
	end
  ) trans;
  List.iter (
    fun st ->
      if (st.acceptation==Bool3.True) then
	begin
	  Array.set acc_exps st.nums (mk_int_exp 1);
	end
  ) states;

  let st_old_init =
    Array.mapi (
      fun i exp ->
	(*Chaque cas doit contenir : (offset * init)*)
	(Index(mk_int_exp i,NoOffset),SingleInit(exp))
    ) st_old_exps
  in
  let st_init =
    Array.mapi (
      fun i exp ->
	(*Chaque cas doit contenir : (offset * init)*)
	(Index(mk_int_exp i,NoOffset),SingleInit(exp))
    ) st_exps
  in
  let tr_init =
    Array.mapi (
      fun i exp ->
	(Index(mk_int_exp i,NoOffset),SingleInit(exp))
    ) tr_exps
  in
  let acc_init =
    Array.mapi (
      fun i exp ->
	(Index(mk_int_exp i,NoOffset),SingleInit(exp))
    ) acc_exps
  in
   (
    {init=Some(CompoundInit(Cil.intType, Array.to_list st_old_init))},
    {init=Some(CompoundInit(Cil.intType, Array.to_list st_init))},
    {init=Some(CompoundInit(Cil.intType, Array.to_list tr_init))},
    {init=Some(CompoundInit(Cil.intType, Array.to_list acc_init))}
   )


let func_to_init name =
  {init=Some(SingleInit(Const(func_to_cenum (name))))}

let funcStatus_to_init st =
  {init=Some(SingleInit(Const(op_status_to_cenum (st))))}








class visit_decl_loops_init () =
object (*(self) *)
  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vstmt_aux stmt =
    begin
      match stmt.skind with
	| Loop _ -> mk_global_c_vars (Data_for_ltl.loopInit^"_"^(string_of_int stmt.sid)) (TInt(IInt,[]))
	| _ -> ()
    end;
    Cil.DoChildren
end

let mk_decl_loops_init () =
  let visitor = new visit_decl_loops_init ()  in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) !file




let mk_invariant_1 () =
  mk_global_comment "//* Inv 1 : Each active state is reachable";
  let tmp_st = Cil.make_logic_var "_buch_st" Cil_types.Linteger in
  let tmp_tr = Cil.make_logic_var "_buch_tr" Cil_types.Linteger in
  mk_global_invariant (
    Pforall(
      [tmp_st],
      unamed (Pimplies (
        unamed (Pand (
	  unamed (mk_logicvar_intervalle tmp_st 0 (getNumberOfStates ())),
	  unamed (Pforall([tmp_tr],
  	    unamed (Pimplies (
	      unamed ( mk_logicvar_intervalle tmp_tr 0 (getNumberOfTransitions ())),
	      unamed (
		mk_disjunction
		  [ (* curTrans[tr]==0 *)
		    Prel(Req,mk_offseted_array_lval_from_lval (host_trans_term ()) tmp_tr , mk_int_term 0) ;
		    (* transStop(tr)!=st *)
		    Prel(Rneq,(mk_logic_call transStop [tmp_tr]), mk_term_from_logic_var tmp_st) ;
		    (* !transCond(tr) *)
		    Pnot(unamed (Papp(get_predicate transCond,[],[mk_term_from_logic_var tmp_tr]))) ;
		    (* curStatesOld[transStart(tr)]==0 *)
		    Prel(Req,mk_offseted_array_lval_from_term (host_stateOld_term()) (mk_logic_call transStart [tmp_tr]), mk_int_term 0)
		  ]
	      )
	    ))
	  ))
	)),
	unamed (
	  (* curStates[st]==0 *)
	  Prel(Req,mk_offseted_array_lval_from_lval (host_state_term()) (tmp_st), mk_int_term 0)
	)
      ))
    )
  ) "_Buch_st_reach_1"







let mk_invariant_2 () =
  mk_global_comment "//* Inv 2 : Each non-active state is not reachable";
  let tmp_st = Cil.make_logic_var "_buch_st" Cil_types.Linteger in
  let tmp_tr = Cil.make_logic_var "_buch_tr" Cil_types.Linteger in
  mk_global_invariant (
    Pforall(
      [tmp_st],
      unamed (Pimplies (
        unamed (Pand (
	  (* 0 <= st <nbStates *)
	  unamed (mk_logicvar_intervalle tmp_st 0 (getNumberOfStates ())),
	  (* curStates[st]==0 *)
	  unamed (Prel(Req,mk_offseted_array_lval_from_lval (host_state_term()) (tmp_st), mk_int_term 0))
	)),
	unamed (Pforall([tmp_tr],
	  unamed (Pimplies (
	    (* 0 <= tr <nbTrans *)
	    unamed(mk_logicvar_intervalle tmp_tr 0 (getNumberOfTransitions ())),
	    unamed(
		mk_disjunction
		  [ (* curTrans[tr]==0 *)
		    Prel(Req,mk_offseted_array_lval_from_lval (host_trans_term ()) tmp_tr , mk_int_term 0) ;
		    (* transStop(tr)!=st *)
		    Prel(Rneq,(mk_logic_call transStop [tmp_tr]), mk_term_from_logic_var tmp_st) ;
		    (* !transCond(tr) *)
		    Pnot(unamed (Papp(get_predicate transCond,[],[mk_term_from_logic_var tmp_tr]))) ;
		    (* curStatesOld[transStart(tr)]==0 *)
		    Prel(Req,mk_offseted_array_lval_from_term (host_stateOld_term()) (mk_logic_call transStart [tmp_tr]), mk_int_term 0)
		  ]
	      )
	    ))
	  ))
	))
      )
  ) "_Buch_st_reach_2"


let mk_invariant_3 () =
  mk_global_comment "//* Inv 3 : Each active state is reachable";
  let tmp_st = Cil.make_logic_var "_buch_st" Cil_types.Linteger in
  let tmp_tr = Cil.make_logic_var "_buch_tr" Cil_types.Linteger in
  mk_global_invariant (
    Pforall(
      [tmp_st],
      unamed (Pimplies (
        unamed (Pand (
	  (* 0 <= st <nbStates *)
	  unamed (mk_logicvar_intervalle tmp_st 0 (getNumberOfStates ())),
	  (* curStates[st]!=0 *)
	  unamed (Prel(Rneq,mk_offseted_array_lval_from_lval (host_state_term()) (tmp_st), mk_int_term 0))
	)),
	unamed (Pexists([tmp_tr],
	  unamed (mk_conjunction
	    [ (* 0 <= tr <nbTrans *)
	      mk_logicvar_intervalle tmp_tr 0 (getNumberOfTransitions ()) ;
	      (* curTrans[tr]!=0 *)
	      Prel(Rneq,mk_offseted_array_lval_from_lval (host_trans_term ()) tmp_tr , mk_int_term 0) ;
	      (* transCond(tr) *)
	      Papp(get_predicate transCond,[],[mk_term_from_logic_var tmp_tr]) ;
	      (* transStop(tr)==st *)
	      Prel(Req,(mk_logic_call transStop [tmp_tr]), mk_term_from_logic_var tmp_st) ;
	      (* curStatesOld[transStart(tr)]!=0 *)
	      Prel(Rneq,mk_offseted_array_lval_from_term (host_stateOld_term()) (mk_logic_call transStart [tmp_tr]), mk_int_term 0)
	    ]
	  )
	))
      ))
    )
  ) "_Buch_st_reach_3"






let mk_invariant_4 () =
  mk_global_comment "//* Inv 4 : Each transition annotated as crossable is crossable";
  let tmp_tr = Cil.make_logic_var "_buch_tr" Cil_types.Linteger in
  mk_global_invariant (
    Pforall(
      [tmp_tr],
      unamed (Pimplies (
        unamed (Pand (
	  (* 0 <= tr <nbTrans *)
	  unamed (mk_logicvar_intervalle tmp_tr 0 (getNumberOfTransitions ())),
	  (* curTrans[tr]!=0 *)
	  unamed (Prel(Rneq,mk_offseted_array_lval_from_lval (host_trans_term ()) tmp_tr , mk_int_term 0))
	)),
	unamed (mk_conjunction
	  [
            (* transCond(tr) *)
	    Papp(get_predicate transCond,[],[mk_term_from_logic_var tmp_tr]) ;
	    (* curStatesOld[transStart(tr)]!=0 *)
	    Prel(Rneq,mk_offseted_array_lval_from_term (host_stateOld_term()) (mk_logic_call transStart [tmp_tr]), mk_int_term 0);
	    (* curStates[transStop(tr)]!=0 *)
	    Prel(Rneq,mk_offseted_array_lval_from_term (host_state_term()) (mk_logic_call transStop [tmp_tr]), mk_int_term 0)
	  ]
        )
      ))
    )
  ) "_Buch_tr_cross_1"



let mk_invariant_5 () =
  mk_global_comment "//* Inv 5 : Each crossable transition is crossed";
  let tmp_tr = Cil.make_logic_var "_buch_tr" Cil_types.Linteger in
  mk_global_invariant (
    Pforall(
      [tmp_tr],
      unamed (Pimplies (
        unamed (mk_conjunction
	  [
	    (* 0 <= tr <nbTrans *)
	    mk_logicvar_intervalle tmp_tr 0 (getNumberOfTransitions ());
	    (* curStatesOld[transStart(tr)]!=0 *)
	    Prel(Rneq,mk_offseted_array_lval_from_term (host_stateOld_term()) (mk_logic_call transStart [tmp_tr]), mk_int_term 0);
            (* transCond(tr) *)
	    Papp(get_predicate transCond,[],[mk_term_from_logic_var tmp_tr])
	  ]
	),
	unamed (mk_conjunction
	  [
	    (* curTrans[tr]!=0 *)
	    Prel(Rneq,mk_offseted_array_lval_from_lval (host_trans_term ()) tmp_tr , mk_int_term 0) ;
	    (* curStates[transStop(tr)]!=0 *)
	    Prel(Rneq,mk_offseted_array_lval_from_term (host_state_term()) (mk_logic_call transStop [tmp_tr]), mk_int_term 0)
	  ]
        )
      ))
    )
  ) "_Buch_tr_cross_2"





let mk_invariant_6 () =
  mk_global_comment "//* Inv 6 : Non-crossable transitions are not crossed over";
  let tmp_tr = Cil.make_logic_var "_buch_tr" Cil_types.Linteger in
  mk_global_invariant (
    Pforall(
      [tmp_tr],
      unamed (Pimplies (
	unamed (Pand (
	  (* 0 <= tr <nbTrans *)
	  unamed (mk_logicvar_intervalle tmp_tr 0 (getNumberOfTransitions ())),
	  unamed (mk_disjunction
            [
	      (* curStatesOld[transStart(tr)]==0 *)
	      Prel(Rneq,mk_offseted_array_lval_from_term (host_stateOld_term()) (mk_logic_call transStart [tmp_tr]), mk_int_term 0);
              (* transCond(tr) *)
	      Pnot(unamed(Papp(get_predicate transCond,[],[mk_term_from_logic_var tmp_tr])))
	    ]
	  )
	)),
	(* curTrans[tr]!=0 *)
	unamed(Prel(Rneq,mk_offseted_array_lval_from_lval (host_trans_term ()) tmp_tr , mk_int_term 0))
      ))
    )
  ) "_Buch_tr_cross_3"





(** This function computes all newly introduced globals (variables, enumeration structure, invariants, etc. *)
let initGlobals root =
  mk_global_comment "//****************";
  mk_global_comment "//* BEGIN Primitives generated for LTL verification";

  mk_global_comment "//* ";
  mk_global_comment "//* States and Trans Variables";
  let (st_old_init, st_init, tr_init, (*acc_init*) _) = get_states_trans_init root in
  mk_global_c_initialized_array curState (getNumberOfStates()) st_init;
  mk_global_c_initialized_array curTrans (getNumberOfTransitions()) tr_init;
  mk_global_c_initialized_array curStateOld (getNumberOfStates()) st_old_init;
(*  mk_global_c_initialized_array curTransTmp (getNumberOfTransitions()) tr_init;*)

  mk_global_comment "//* ";
  mk_global_comment "//* Their invariants";
  mk_global_invariant
    (mk_conjunction
       [(mk_valid_range curTrans (getNumberOfTransitions ())) ;
(*	(mk_valid_range curTransTmp (getNumberOfTransitions ())) ;*)
	(mk_valid_range curState (getNumberOfStates ())) ;
	(mk_valid_range curStateOld (getNumberOfStates ()))
       ])
    "Buch_Ranges_Validity";

(*  mk_global_invariant
    (*(mk_conjunction
       [*) (mk_eq_tables curTrans curTransTmp (getNumberOfTransitions ())) (*;
	(mk_eq_tables curState curStateTmp (getNumberOfStates      ()))
       ])*)
    "Buch_Arrays_Coherence";
*)

  mk_global_comment "//* ";
  (*mk_global_comment "//* Acceptation States -- UNUSED AT THIS TIME !!!";
  mk_global_c_initialized_array acceptSt (getNumberOfStates()) acc_init;
  mk_global_invariant (mk_valid_range acceptSt (getNumberOfStates ())) "Buch_acc_Ranges_Validity";
  *)
  mk_global_comment "//* ";
  mk_global_comment "//* Some constants";
  mk_global_c_enum_type  listOp (List.map (fun e -> func_to_op_func e) (getFunctions_from_c()));
  mk_global_c_initialized_enum curOp listOp (func_to_init root);
  mk_global_c_enum_type  listStatus (callStatus::[termStatus]);
  mk_global_c_initialized_enum curOpStatus listStatus (funcStatus_to_init Promelaast.Call);


  mk_global_comment "//* ";
  mk_global_comment "//* Loops management";
  mk_decl_loops_init ();


  mk_global_comment "//* ";
  mk_global_comment "//**************** ";
  mk_global_comment "//* Axiomatized transitions automata";

  mk_decl_axiomatized_auotmata ();

  mk_global_comment "//* ";
  mk_global_comment "//**************** ";
  mk_global_comment "//* Safety invariants";
  mk_global_comment "//* ";

  mk_invariant_1 ();
  mk_invariant_2 ();
  mk_invariant_3 ();
  mk_invariant_4 ();
  mk_invariant_5 ();
  mk_invariant_6 ();


  mk_global_comment "//* ";
  mk_global_comment "//* END Primitives generated for LTL verification";
  mk_global_comment "//****************";

  flush_globals()











(* ************************************************************************* *)
(** {b Pre/post management} *)


(** Function called by mk_asbstract_pre and mk_asbstract_post. *)
let mk_abstract_pre_post (states_l,trans_l) func status =
  (* Intially, no state is a source for crossable transition and no transition is crossable*)
  let st_status = Array.make (List.length states_l) false in
  let tr_status = Array.make (List.length trans_l) false in

  (* Conjunction of forbidden transitions and disjunction of crossable transitions are compute together.
     Moreover, authorized states are annotate in the same pass.
  *)

  List.iter
    (fun tr ->
       if isCrossable tr func status then
	 begin
	   Array.set st_status tr.stop.nums true;
	   Array.set tr_status tr.numt true
	 end
    )
    trans_l;

  (st_status,tr_status)

(**{b Pre and post condition of C functions} In our point of view, the pre or
   the post condition of a C function are defined by the set of states
   authorized just before/after the call, as such as the set of crossable
   transitions. The following functions generates abstract pre and post-conditions
   by using only informations deduced from the buchi automata.
*)
(** Given the buchi automata and the name of a function, it returns two arrays
    corresponding to the abstract pre-condition. *)
let mk_asbstract_pre auto func =
  mk_abstract_pre_post auto func Promelaast.Call


(** Given the buchi automata and the name of a function, it returns two arrays
    corresponding to the abstract post-condition. *)
let mk_asbstract_post auto func =
  mk_abstract_pre_post auto func Promelaast.Return


(** Generates a term representing the given pre or post condition.
    Transitions and states are rewrited into predicates in the same maner. The computation is then generalized
    Conjunction of forbidden and disjunction of authorized are compute together. *)
let pre_post_to_term  (st_status, tr_status) =
  let pp_to_term an_array array_term =
    let (authorized,forbidden,_) =
      Array.fold_left
	(fun (au_pred,fo_pred,i) b ->
	   if b then
	     begin
	       (por(au_pred,prel(Rneq, zero_term(), (mk_offseted_array array_term i))),
		fo_pred,
		i+1
	       )
	     end
	   else
	     (au_pred,
	      pand(fo_pred,prel(Req, zero_term(), (mk_offseted_array array_term i))),
	      i+1
	     )
	)
	(pfalse,ptrue,0)
	an_array
    in
    authorized::[forbidden]

  in
  let tr = pp_to_term tr_status (host_trans_term ()) in
  let st = pp_to_term st_status (host_state_term ()) in
  st@tr











let force_condition_to_predicate global_inv restricted_inv =
  let pred_l = ref [] in
  let treat global restric array_term=
    Array.iteri
      (fun index value ->
	 if (not value) && global.(index) then
	   begin
	     let n_pred = Prel(Req,(mk_offseted_array array_term index),zero_term())in
	     pred_l:= n_pred::!pred_l
	   end
      )
      restric
  in
  treat (fst global_inv) (fst restricted_inv) (host_state_term ());
  treat (snd global_inv) (snd restricted_inv) (host_trans_term ());
  if !pred_l<>[] then
    mk_conjunction (List.rev !pred_l)
  else
    Ptrue












(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
