(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Lang
open Lang.F
open Conditions
open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Compound Equality                                                  --- *)
(* -------------------------------------------------------------------------- *)

type update = term * term * term

type equality =
  | Record of term * term * Lang.field list (* a.f = b.f forall f *)
  | Array1 of update * term * tau
  | Array2 of update * update * tau

(* -------------------------------------------------------------------------- *)
(* --- Record Patterns                                                    --- *)
(* -------------------------------------------------------------------------- *)

let get_record_assoc = function
  | (f,_)::_ -> Some (Lang.fields_of_field f)
  | _ -> None

let get_record_type = function
  | Qed.Logic.Record fts -> get_record_assoc fts
  | _ -> None

let get_record_term a =
  match F.repr a with
  | Qed.Logic.Rdef fvs -> get_record_assoc fvs
  | Qed.Logic.Fvar x -> get_record_type (F.tau_of_var x)
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Array Patterns                                                     --- *)
(* -------------------------------------------------------------------------- *)

let rec typeof_index a k =
  try F.typeof k with Not_found ->
  match F.repr a with
  | Qed.Logic.Aset(a,k,_) -> typeof_index a k
  | _ -> typeof_domain a
and typeof_domain a = 
  match F.typeof a with
  | Qed.Logic.Array(t,_) -> t
  | _ -> raise Not_found
and typeof_update (a,k,_) = typeof_index a k

let get_array_update a =
  match F.repr a with
  | Qed.Logic.Aset(a,k,v) -> Some (a,k,v)
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Equality Patterns                                                  --- *)
(* -------------------------------------------------------------------------- *)

let array1 upd a =
  let t =
    try typeof_domain a 
    with Not_found -> typeof_update upd
  in Array1(upd,a,t)
    
let array2 p q =
  let t =
    try typeof_update p 
    with Not_found -> typeof_update q
  in Array2(p,q,t)

let get_compound_cmp a b =
  match get_record_term a with
  | Some fs -> Record(a,b,fs)
  | None -> match get_record_term b with
    | Some fs -> Record(a,b,fs)
    | None ->
        match get_array_update a , get_array_update b with
        | None , None -> raise Not_found
        | Some upd , None -> array1 upd b
        | None , Some upd -> array1 upd a
        | Some p , Some q -> array2 p q

let get_compound_equality e =
  match F.repr e with
  | Qed.Logic.Eq(a,b) -> true , get_compound_cmp a b
  | Qed.Logic.Neq(a,b) -> false , get_compound_cmp a b
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Clauses                                                            --- *)
(* -------------------------------------------------------------------------- *)

let field a b f =
  Pretty_utils.sfprintf "Field %a" Lang.Field.pretty f ,
  F.p_equal (F.e_getfield a f) (F.e_getfield b f)

let index vars tau =
  let pool = Lang.new_pool ~vars () in
  let x = F.fresh pool tau in
  [x] , F.e_var x

let eq i j p = F.p_imply (F.p_equal i j) p
let neq i j p = F.p_imply (F.p_neq i j) p
let get1 a k v = F.p_equal (F.e_get a k) v
let get2 a b k = F.p_equal (F.e_get a k) (F.e_get b k)

let clause ~(vars : F.Vars.t) = function
  | Record(a,b,fs) -> List.map (field a b) fs
  | Array1((a,i,u),b,t) ->
      let ks,k = index vars t in
      [ "Updated" , get1 b i u ;
        "Others" , F.p_forall ks (neq i k (get2 a b k)) ]
  | Array2((a,i,u),(b,j,v),t) ->
      let ks,k = index vars t in
      [ "Updated (both)" , eq i j (F.p_equal u v) ;
        "Updated (left)" , neq i j (get1 a j v) ;
        "Updated (right)" , neq i j (get1 b i u) ;
        "Others" , F.p_forall ks (neq i k (neq j k (get2 a b k))) ]

(* -------------------------------------------------------------------------- *)
(* --- Compound Tactic                                                    --- *)
(* -------------------------------------------------------------------------- *)
      
let conj cs = F.p_all snd cs
let disj cs = F.p_any (fun (_,p) -> F.p_not p) cs

let negative (f,p) = f , When (F.p_not p)

let name eq = if eq then "eq" else "neq"
let kind = function Record _ -> "compound" | Array1 _ | Array2 _ -> "array"
let equality eq = if eq then "equality" else "dis-equality"

let process_expand (feedback : Tactical.feedback) ?at e =
  let vars = F.vars e in
  let eq,cmp = get_compound_equality e in
  feedback#set_title "Compound (%s)" (name eq) ;
  feedback#set_descr "Expand %s %s" (kind cmp) (equality eq) ;
  let e' = (if eq then conj else disj) (clause ~vars cmp) in
  let cases = [feedback#get_title,F.p_true,e,F.e_prop e'] in
  Tactical.rewrite ?at cases

let process_have (feedback : Tactical.feedback) s =
  let e = F.e_prop (Conditions.have s) in
  let vars = F.vars e in
  let eq,cmp = get_compound_equality e in
  if eq then
    begin
      feedback#set_title "Compound (eq)" ;
      feedback#set_descr "Expand %s equality" (kind cmp) ;
      let cases = ["Compound (eq)",When (conj (clause ~vars cmp))] in
      Tactical.replace ~at:s.id cases
    end
  else
    begin
      feedback#set_title "Compound (split)" ;
      feedback#set_descr "Split %s dis-equality" (kind cmp) ;
      let cases = List.map negative (clause ~vars cmp) in
      Tactical.replace ~at:s.id cases
    end

let process_goal (feedback : Tactical.feedback) p =
  let eq,cmp = get_compound_equality (F.e_prop p) in
  let vars = F.varsp p in
  if eq then
    begin
      feedback#set_title "Compound (split)" ;
      feedback#set_descr "Split %s equality" (kind cmp) ;
      Tactical.split (clause ~vars cmp) ;
    end
  else
    begin
      feedback#set_title "Compound (neq)" ;
      feedback#set_descr "Expand compound dis-equality" ;
      let cases = ["Compound (neq)",disj (clause ~vars cmp)] in
      Tactical.split cases
    end

class compound =
  object
    inherit Tactical.make ~id:"Wp.compound"
        ~title:"Compound"
        ~descr:"Decompose compound equalities"
        ~params:[]

    method select feedback (s : Tactical.selection) =
      let process =
        match s with
        | Clause (Step s) -> process_have feedback s
        | Clause (Goal p) -> process_goal feedback p
        | Inside(_,e) -> process_expand feedback ?at:(Tactical.at s) e
        | Empty | Compose _ -> raise Not_found
      in Applicable process

  end

let tactical = Tactical.export (new compound)
let strategy = Strategy.make tactical
