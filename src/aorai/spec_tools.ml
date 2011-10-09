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

(* $Id: $ *)

let numberOfStates = ref 0
let numberOfTransitions = ref 0
let setNumberOfStates nbSt = numberOfStates:= nbSt
let setNumberOfTransitions nbTr = numberOfTransitions := nbTr

let mk_empty_pre_st () =
  Array.make (!numberOfStates) false

let mk_empty_pre_or_post () =
  (Array.make (!numberOfStates) false,
   Array.make (!numberOfTransitions) false)

let mk_full_pre_or_post () =
  (Array.make (!numberOfStates) true,
   Array.make (!numberOfTransitions) true)

let mk_empty_spec () =
  (Array.make (!numberOfStates) false,
   Array.make (!numberOfTransitions) false,
   Array.make (!numberOfStates) false,
   Array.make (!numberOfTransitions) false
  )

(** Given two bool arrays with the same length, it returns a fresh
    bool array corresponding to a logical AND between cells with same index
    from the two arrays.  *)
let bool_array_and arr1 arr2 =
  if Array.length arr1 <> Array.length arr2 then
    assert false;
  let res=Array.make (Array.length arr1) false in
  Array.iteri
    (fun i b1 -> if b1 && arr2.(i) then res.(i)<-true)
    arr1;
  res


(** Given two bool arrays with the same length, it returns a fresh bool array corresponding to a logical OR between cells with same index from the two arrays.  *)
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

type pre_post_bycase_t = bool array array
type double_pre_post_bycase_t = (pre_post_bycase_t*pre_post_bycase_t)
type quad_pre_post_bycase_t =
    (pre_post_bycase_t*pre_post_bycase_t*pre_post_bycase_t*pre_post_bycase_t)

(* ************************************************************************* *)

let mk_empty_pre_st_bycase () =
  Array.make_matrix (!numberOfStates) (!numberOfStates) false

let mk_empty_pre_or_post_bycase () =
  (Array.make_matrix (!numberOfStates) (!numberOfStates) false,
   Array.make_matrix (!numberOfStates) (!numberOfTransitions) false)

let mk_full_pre_or_post_bycase () =
  (Array.make_matrix (!numberOfStates) (!numberOfStates) true,
   Array.make_matrix (!numberOfStates) (!numberOfTransitions) true)

let mk_empty_spec_bycase () =
  (Array.make_matrix (!numberOfStates) (!numberOfStates) false,
   Array.make_matrix (!numberOfStates) (!numberOfTransitions) false,
   Array.make_matrix (!numberOfStates) (!numberOfStates) false,
   Array.make_matrix (!numberOfStates) (!numberOfTransitions) false
  )

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




(** Given two bool arrays with the same length, it returns a fresh bool array 
    corresponding to a logical AND between cells with same index from the 
    two arrays.  *)
let bool_array_and_bycase bc_arr1 bc_arr2 =
  if Array.length bc_arr1 <> Array.length bc_arr2 then
    assert false;
  let res=Array.make 
    (Array.length bc_arr1) (Array.make (Array.length bc_arr1.(0)) false) 
  in
  Array.iteri
    (fun case b1 -> res.(case)<-bool_array_and b1 (bc_arr2.(case)))
    bc_arr1;
  res


(** Given two bool arrays with the same length, it returns a fresh bool array 
    corresponding to a logical OR between cells with same index from 
    the two arrays.  *)
let bool_array_or_bycase bc_arr1 bc_arr2 =
  if Array.length bc_arr1 <> Array.length bc_arr2 then
    assert false;
  let res=Array.make 
    (Array.length bc_arr1) (Array.make (Array.length bc_arr1.(0)) false) 
  in
  Array.iteri
    (fun case b1 -> res.(case)<-bool_array_or b1 (bc_arr2.(case)))
    bc_arr1;
  res

(** Given two bool arrays with the same length, it returns true if and only 
    if their cells are equal for each index. *)
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

let is_empty_pre_post__ pp  =
  Array.fold_left
    (fun isempty value -> isempty && not value)
    true
    pp

(** Return false if and only if all states are associated to false *)
let is_empty_pre_post (pre_st,_) =
  is_empty_pre_post__ pre_st


(** Return false if and only if all states are associated to false *)
let is_empty_post_bc (post_st,_) =
  Array.fold_left
    (fun isempty val_bc -> isempty && is_empty_pre_post__ val_bc)
    true
    post_st

let separator = ""

let concat = String.concat ""

let debug_display_stmt_pre pre prefixe =
  let r=ref "{" in
    let result = ref "" in
  Array.iteri
    (fun i s ->
       if s then
     begin
(*       Format.printf "%s%s%s" !r prefixe (string_of_int i);*)
             result := concat [!result;!r;prefixe;(string_of_int i)];
       r:=","
     end
    )
    pre;
  if !r="{" then
    "{}"
  else
        concat[!result;"}"]
(*    Format.printf "}"*)


let debug_display_spec (pre_st,_,post_st,_) name=
  let pre_str = debug_display_stmt_pre pre_st "st" in
  let post_str = debug_display_stmt_pre post_st "st" in
  concat [pre_str;" ";name;" ";post_str];;
(*                                      *)
(*  debug_display_stmt_pre pre_st "st"; *)
(*  Format.printf " %s " name;          *)
(*  debug_display_stmt_pre post_st "st";*)
(*  Format.printf "\n"                  *)

let debug_display_stmt_all_pre (st,tr)=
  let tr_str = debug_display_stmt_pre tr "tr" in
  let st_str = debug_display_stmt_pre st "st" in
  concat ["tr=";tr_str;" ";"st=";st_str];;
(*  Format.printf "tr=";           *)
(*  debug_display_stmt_pre tr "tr";*)
(*  Format.printf " st=";          *)
(*  debug_display_stmt_pre st "st" *)

let is_empty_behavior assocs =
  Array.fold_left (fun b c -> if c then false else b) true assocs

let assocs_to_string assocs prefixe =
  let r=ref "(" in
  let s=ref "" in
  Array.iteri
    (fun i b ->
       if b then
     begin
       s:=(!s)^(!r)^prefixe^(string_of_int i);
       r:=","
     end
    )
    assocs;
  !s^")"

let debug_display_stmt_pre_bycase pre prefixe =
  let r=ref "{" in
    let result = ref "" in
  Array.iteri
    (fun i assocs ->
       if not (is_empty_behavior assocs) then
     begin
       result:= concat [!result;!r ;"st";(string_of_int i);"->";(assocs_to_string assocs prefixe)] ;
       r:=","
     end
    )
    pre;
  if !r="{" then
    "{}"
  else
    concat [!result ;"}"]

let debug_display_spec_bycase (pre_st,_,post_st,_) name=
  let pre_str = debug_display_stmt_pre_bycase pre_st "st" in
    let post_str = debug_display_stmt_pre_bycase post_st "st" in
    concat [pre_str;" ";name;" ";post_str];;
(*  Format.printf "%s %s %s" pre_str name post_str;;*)
(*  debug_display_stmt_pre_bycase post_st "st";*)
(*  Format.printf "\n"                         *)

let debug_display_stmt_all_pre_bycase (st,tr)=
  let tr_str = debug_display_stmt_pre_bycase tr "tr" in
  let st_str = debug_display_stmt_pre_bycase st "st" in
    concat ["tr=";tr_str;" ";"st=";st_str];;
(*  Format.printf "tr=";                  *)
(*  debug_display_stmt_pre_bycase tr "tr";*)
(*  Format.printf " st=";                 *)
(*  debug_display_stmt_pre_bycase st "st" *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
