(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Bunch of possibly Simplified hypotheses.                           --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Logic
open Lang
open Lang.F

module SIMPL = Qed.Simplifier.Make(Lang.F)

type qed = SIMPL.ground

type node =
  | MARK of Cil_types.stmt option * string
  | HAVE of pred
  | GUARD of pred
  | IF of pred * bundle * bundle
  | CASES of bundle list
  | CONTRADICTION of pred list
      
and bundle = hyp list
    
and hyp = {
  hid : int ;
  pred : pred ;
  node : node ;
}
    
(* -------------------------------------------------------------------------- *)
(* ---  Property Conversion                                               --- *)
(* -------------------------------------------------------------------------- *)
    
let cond_hyp h = h.pred

let rec cond_node = function
  | MARK _ -> p_true
  | GUARD p -> p
  | HAVE p -> p
  | IF(p,a,b) -> p_if p (cond_bundle a) (cond_bundle b)
  | CASES bs -> p_any cond_bundle bs
  | CONTRADICTION _ -> p_false
and cond_bundle hs = p_all cond_hyp hs

(* -------------------------------------------------------------------------- *)
(* --- Unique Sorted Bundles with Consistency Check                       --- *)
(* -------------------------------------------------------------------------- *)
  
module H = Qed.Listset.Make
  (struct 
     type t = hyp
     let compare a b = Pervasives.compare b.hid a.hid 
       (* intentionnaly in reverse order *)
   end)
  
type t = {
  hs : H.t ;
  qed : qed option ;
}
    
let empty = { hs = [] ; qed = None }
let kid = ref 0
let _reset () = kid := 0
let fresh () = incr kid ; !kid

let qed_make () = SIMPL.cc_make ~timeout:200 []
let qed_assume qed hs = SIMPL.cc_assume qed (F.e_props hs)
let qed_simplify_atom qed = F.lift (SIMPL.cc_simplify qed)

let qed_simplify_conj qed ps =
  let ps = Array.of_list ps in
  Array.iteri
    (fun i p ->
       ps.(i) <- p_true ;
       let q = 
	 match qed_assume qed (Array.to_list ps) with
	   | Some qed -> qed_simplify_atom qed p
	   | None -> p_false
       in 
       ps.(i) <- q
    ) ps ;
  F.p_conj (Array.to_list ps)
    
let qed_simplify_pred qed p =
  match F.pred p with
    | Logic.And ps -> qed_simplify_conj qed ps
    | _ -> qed_simplify_atom qed p

let hyp node = {
  hid = fresh () ;
  pred = cond_node node ;
  node = node ;
}
  
let context hs = 
  if Wp_parameters.Simpl.get () then
    let qed = qed_make () in
    (*TODO: report for incomplete simplification in case of Contradiction *)
    let context = qed_assume qed (List.map cond_hyp hs) in
    { hs = hs ; qed = context }
  else 
    { hs = hs ; qed = None }
      
let conditions w = List.map cond_hyp w.hs
let occurs x w = List.exists (fun hyp -> F.occursp x hyp.pred) w.hs
let intersect p w = List.exists (fun hyp -> F.intersectp p hyp.pred) w.hs

(* -------------------------------------------------------------------------- *)
(* --- Assumptions                                                        --- *)
(* -------------------------------------------------------------------------- *)
  
let rec check_trivial hs p = 
  match F.pred p with
    | True -> true
    | False -> false
    | And ps -> List.for_all (check_trivial hs) ps
    | _ -> 
	let q = p_not p in
	List.for_all (fun hyp -> hyp.pred != q) hs
	  
let get_qed w =
  match w.qed with Some _ as e -> e | None ->
    if Wp_parameters.Simpl.get () 
    then Some (qed_make ()) else None
      
let check_context w h =
  match get_qed w with
    | None -> 
	if (check_trivial w.hs h) then Some None else None
    | Some e -> 
	match qed_assume e [h] with
	  | (Some _) as r -> Some r
	  | None -> None

let mark ?descr ?stmt w =
  match descr , stmt with
    | None , None -> w
    | _ ->
	let text = match descr with Some d -> d | None -> "" in
	let hyp = hyp (MARK(stmt,text)) in
	{ hs = H.add hyp w.hs ; qed = w.qed }
	      
let assume w hs =
  let h = p_conj hs in
  match check_context w h with
    | None ->
	let hyp = hyp (CONTRADICTION hs) in
	{ hs = H.add hyp w.hs ; qed = None }
    | Some qed ->
	let hyp = hyp (HAVE h) in
	{ hs = H.add hyp w.hs ; qed = qed }
	  
let _simplify_goal w p =
  match w.qed with
    | Some qed -> qed_simplify_pred qed p
    | None -> 
	if List.exists (fun hyp -> hyp.pred == p_false || hyp.pred == p) w.hs 
	then p_true else p

(* -------------------------------------------------------------------------- *)
(* --- Split Case                                                         --- *)
(* -------------------------------------------------------------------------- *)
    
let factorize w1 w2 =
  let p,c,q = H.factorize w1.hs w2.hs in
  List.map (fun h -> h.pred) p , context c , List.map (fun h -> h.pred) q

let m_or w1 w2 =
  let p,c,q = H.factorize w1.hs w2.hs in
  match p,q with
    | [],_ -> w2 (* richer context in w2 *)
    | _,[] -> w1 (* richer context in w1 *)
    | _ -> 
	let split = hyp (CASES [p;q]) in
	context (split :: c) (* sorted, since split is fresh *)
	  
let m_either = function
  | [] -> None
  | [w1;w2] -> Some (m_or w1 w2)
  | w::ws ->
      let cs = List.fold_left (fun hs w -> H.inter hs w.hs) w.hs ws in
      let hs = List.map (fun w -> H.diff w.hs cs) ws in
      let split = hyp (CASES hs) in
      Some (context (split :: cs)) (* sorted, since split is fresh *)
	
(* -------------------------------------------------------------------------- *)
(* --- Conditions                                                         --- *)
(* -------------------------------------------------------------------------- *)
	
let m_decide_if p w1 w2 =
  let q = p_not p in
  match w1.qed , w2.qed with
    | Some qed1 , Some qed2 ->
	begin
	  match 
	    qed_assume qed1 [p] , 
	    qed_assume qed2 [q] 
	  with
	    | (Some _ as e1) , None -> 
		let hyp = hyp (HAVE p) in
		Some { hs = hyp :: w1.hs ; qed = e1 }
	    | None , (Some _ as e2) ->
		let hyp = hyp (HAVE q) in
		Some { hs = hyp :: w1.hs ; qed = e2 }
	    | _ -> None
	end
    | _ when not (check_trivial w1.hs p) ->
	let hyp = hyp (HAVE q) in
	Some (context (hyp :: w2.hs))
    | _ when not (check_trivial w2.hs q) ->
	let hyp = hyp (HAVE p) in
	Some (context (hyp :: w1.hs))
    | _ -> None

let is_true hs = List.for_all (fun h -> h.pred == F.p_true) hs 
let is_false hs = List.exists (fun h -> h.pred == F.p_false) hs
	
let m_if p w1 w2 =
  match m_decide_if p w1 w2 with
    | Some w -> w
    | None ->
	let hs1 , cs , hs2 = H.factorize w1.hs w2.hs in
	if is_true hs1 && is_true hs2 
	then 
	  context cs 
	else
	  let cond = hyp (IF(p,hs1,hs2)) in
	  context (cond :: cs)
	  
(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

let pp_loc fmt s =
  let loc = fst (Cil_datatype.Stmt.loc s) in
  let file = loc.Lexing.pos_fname in
  let line = loc.Lexing.pos_lnum in
  Format.fprintf fmt "%s:%d" file line

let rec pp_node fmt h =
  match h.node with
    | MARK(None,text) ->
	Format.fprintf fmt "(* %s *)" text
    | MARK(Some stmt,"") ->
	Format.fprintf fmt "(* %a *)" pp_loc stmt
    | MARK(Some stmt,text) ->
	Format.fprintf fmt "(* %s (%a) *)" text pp_loc stmt
    | CONTRADICTION hs ->
	Format.fprintf fmt "Contradiction:" ;
	List.iter
	  (fun h -> Format.fprintf fmt "@ @[<hov 2>%a ;@]" F.pp_pred h) hs ;
    | GUARD hs ->
	Format.fprintf fmt "@[<hov 2>Constraints:@ %a@]" F.pp_pred hs
    | HAVE p -> 
	Format.fprintf fmt "@[<hov 2>%a ;@]" F.pp_pred p ;
    | CASES us ->
	Format.fprintf fmt "@[<v 0>Either {" ;
	List.iter (fun u -> Format.fprintf fmt "@\n@[<hov 3> * %a@]" pp_case u) us ;
	Format.fprintf fmt "@\n}@]"
    | IF(p,u,v) ->
	Format.fprintf fmt "If @[<hov 2>%a@]@ %a@ %a" 
	  F.pp_pred p (pp_bundle "Then") u (pp_bundle "Else") v

and pp_case fmt = function
  | [] -> Format.fprintf fmt "True"
  | h::hs ->
      pp_node fmt h ;
      List.iter (fun h -> Format.fprintf fmt "@ %a" pp_node h) hs

and pp_bundle head fmt = function
  | [] -> Format.fprintf fmt "%s {}" head
  | [h] -> Format.fprintf fmt "%s %a" head pp_node h
  | hs ->
      Format.fprintf fmt "@[<hv 0>@[<hv 2>%s {" head ;
      List.iter (fun h -> Format.fprintf fmt "@ %a" pp_node h) hs ;
      Format.fprintf fmt "@]@ }@]"
  
let pretty fmt w = pp_bundle "Assume" fmt w.hs

(* -------------------------------------------------------------------------- *)
(* --- Hypotheses Simplifier                                              --- *)
(* -------------------------------------------------------------------------- *)

(* preliminary remark : all bundles are in positive position *)

let update hyp node = { hid = hyp.hid ; node = node ; pred = cond_node node }

let flat (bs : bundle array) : bundle =
  let pool = ref Bag.empty in
  Array.iter (fun hs -> pool := Bag.concat !pool (Bag.list hs)) bs ;
  Bag.elements !pool

let assumptions (bs : bundle array) : pred list =
  let pool = ref [] in
  Array.iter (fun hs -> List.iter (fun h -> pool := h.pred :: !pool) hs) bs ;
  !pool

let is_simple = function 
  | { node=(MARK _ | CONTRADICTION _) } -> true 
  | _ -> false

let rec simplify_hyp qed hyp =
  match hyp.node with
    | MARK _ -> [hyp]
    | CONTRADICTION _ -> [hyp]
    | HAVE p -> 
	begin
	  let q = qed_simplify_pred qed p in
	  match F.pred q with
	    | Logic.True -> []
	    | Logic.False -> [update hyp (CONTRADICTION [p])]
	    | _ -> [update hyp (HAVE q)]
	end
    | GUARD hs ->
	let p = qed_simplify_pred qed hs in
	begin
	  match F.pred p with 
	    | Logic.True -> []
	    | Logic.False -> [update hyp (CONTRADICTION [hs])]
	    | _ -> [update hyp (GUARD p)]
	end
    | IF(p,a,b) ->
	let p = qed_simplify_pred qed p in
	let q = p_not p in
	begin
	  match qed_assume qed [p] , qed_assume qed [q] with
	    | None , None ->
		[update hyp (CONTRADICTION [p])]
	    | Some qed_then , None ->
		let hyp = update hyp (HAVE p) in
		hyp :: simplify_bundle qed_then a
	    | None , Some qed_else ->
		let hyp = update hyp (HAVE q) in
		hyp :: simplify_bundle qed_else b
	    | Some qed_then , Some qed_else ->
		let a = simplify_bundle qed_then a in
		let b = simplify_bundle qed_else b in
		[update hyp (IF(p,a,b))]
	end
    | CASES us ->
	begin
	  match simplify_cases qed [] us with
	    | [] -> [update hyp (HAVE p_false)]
	    | [case] -> case
	    | cases -> [update hyp (CASES (List.rev cases))]
	end

and simplify_cases qed cases = function
  | [] -> cases
  | bundle :: others ->
      let bundle = simplify_bundle qed bundle in
      if is_false bundle then [bundle] else
	if is_true bundle 
	then simplify_cases qed cases others 
	else simplify_cases qed (bundle::cases) others

and simplify_bundle qed = function
  | [] -> []
  | [hyp] -> simplify_hyp qed hyp
  | hyps ->
      let bundles = Array.of_list hyps in
      let simplified = Array.map (fun u -> [u]) bundles in
      for i = Array.length bundles - 1 downto 0 do
	let bi = bundles.(i) in
	if not (is_simple bi) then
	  begin
	    simplified.(i) <- [] ;
	    let hs = assumptions simplified in
	    match qed_assume qed hs with
	      | Some qed ->
		  simplified.(i) <- simplify_hyp qed bi
	      | None ->
		  simplified.(i) <- [bi]
	  end
      done ;
      flat simplified

(* -------------------------------------------------------------------------- *)
(* --- Useless Variable Elimination                                       --- *)
(* -------------------------------------------------------------------------- *)

module Vmap = Map.Make(Var)

type 'a occur = 
  | TOP
  | TRUE
  | FALSE
  | EQ of 'a

let cup eq a y = match a with
  | EQ x when eq x y -> a
  | _ -> TOP

let cup_true = function
  | TRUE -> TRUE
  | _ -> TOP

let cup_false = function
  | FALSE -> FALSE
  | _ -> TOP

let set_top m p = Vars.fold (fun x m -> Vmap.add x TOP m) (F.varsp p) m
let add eq x d m = Vmap.add x (try cup eq (Vmap.find x m) d with Not_found -> EQ d) m
let add_true m x = Vmap.add x (try cup_true (Vmap.find x m) with Not_found -> TRUE) m
let add_false m x = Vmap.add x (try cup_false (Vmap.find x m) with Not_found -> FALSE) m
let add_var = add Var.equal
let add_fun = add Fun.equal

let rec add_pred m p =
  match F.pred p with
    | And ps -> List.fold_left add_pred m ps
    | If(e,a,b) -> add_pred (add_pred (set_top m e) a) b
    | Eq(a,b) -> 
	begin
	  match F.pred a , F.pred b with
	    | Var x , Var y -> add_var x y (add_var y x m)
	    | _ -> set_top m p
	end
    | Var x -> add_true m x
    | Not p ->
	begin
	  match F.pred p with
	    | Var x -> add_false m x
	    | _ -> set_top m p
	end
    | _ -> set_top m p

let rec add_guard m p =
  match F.pred p with
    | And ps -> List.fold_left add_guard m ps
    | Fun(f,[e]) ->
	begin
	  match F.pred e with
	    | Var x -> add_fun x f m
	    | _ -> set_top m p
	end
    | _ -> set_top m p

type usage = {
  mutable eq_var : var occur Vmap.t ;
  mutable eq_fun : lfun occur Vmap.t ;
}

let collect_atom m p = m.eq_var <- set_top m.eq_var p
let collect_pred m p = m.eq_var <- add_pred m.eq_var p
let collect_guard m p = m.eq_fun <- add_guard m.eq_fun p

let rec collect_hyp m hyp = 
  match hyp.node with
    | MARK _ -> ()
    | CONTRADICTION _ -> ()
    | HAVE p -> collect_pred m p
    | GUARD p -> collect_guard m p
    | IF(p,a,b) -> 
	begin
	  collect_atom m p ;
	  collect_bundle m a ;
	  collect_bundle m b ;
	end
    | CASES us -> List.iter (collect_bundle m) us

and collect_bundle m us = List.iter (collect_hyp m) us

let get x m = try Some (Vmap.find x m) with Not_found -> None

let is_true x m = 
  try match Vmap.find x m with TRUE -> true | _ -> false 
  with Not_found -> false

let is_false x m = 
  try match Vmap.find x m with FALSE -> true | _ -> false 
  with Not_found -> false

let is_var x m = 
  try match Vmap.find x m.eq_var with 
    | EQ y -> 
	begin
	  match get x m.eq_fun , get y m.eq_fun with
	    | None , _ -> true  (* we eliminate x, which has no guard... *)
	    | Some (EQ f) , Some (EQ g) -> Fun.equal f g
	    | _ -> false
	end
    | _ -> false
  with Not_found -> false

let rec filter_pred m p =
  match F.pred p with
    | And ps -> F.p_all (filter_pred m) ps
    | If(e,a,b) -> p_if e (filter_pred m a) (filter_pred m b)
    | Eq(a,b) -> 
	begin
	  match F.pred a , F.pred b with
	    | Var x , Var y when is_var x m || is_var y m -> p_true
	    | _ -> p
	end
    | Var x when is_true x m.eq_var -> p_true
    | Not q ->
	begin
	  match F.pred q with
	    | Var x when is_false x m.eq_var -> p_true
	    | _ -> p
	end
    | _ -> p

let rec filter_guard m p =
  match F.pred p with
    | And ps -> F.p_all (filter_guard m) ps
    | Fun(_,[e]) ->
	begin
	  match F.pred e with
	    | Var x when is_var x m -> p_true
	    | _ -> p
	end
    | _ -> p

let is_empty hyp = 
  match hyp.node with
    | MARK _ | CONTRADICTION _ -> false
    | HAVE p -> p == p_true
    | GUARD p -> p == p_true
    | IF(_,[],[]) -> true
    | IF _ -> false
    | CASES [] -> true
    | CASES _ -> false

let rec filter_hyp m hyp =
  match hyp.node with
    | MARK _ -> hyp 
    | CONTRADICTION _ -> hyp
    | HAVE p -> update hyp (HAVE(filter_pred m p))
    | GUARD p -> update hyp (GUARD(filter_guard m p))
    | IF(p,a,b) -> update hyp (IF(p,filter_bundle m a,filter_bundle m b))
    | CASES (us : bundle list) -> 
	begin
	  match filter_cases m us with
	    | [[u]] -> u
	    | us -> update hyp (CASES us)
	end
	  
and filter_bundle m = function
  | [] -> []
  | hyp::hyps ->
      let hyp = filter_hyp m hyp in
      match hyp.node with
	| CASES [b] -> b @ filter_bundle m hyps
	| _ -> 
	    if is_empty hyp 
	    then filter_bundle m hyps
	    else hyp :: filter_bundle m hyps
	      
and filter_cases m = function
  | [] -> []
  | b :: bs -> 
      match filter_bundle m b with
	| [] -> filter_cases m bs
	| b -> b :: filter_cases m bs

(* -------------------------------------------------------------------------- *)
(* --- Goal Simplifier                                                    --- *)
(* -------------------------------------------------------------------------- *)

let _simplify_goal w gs goal =
  let xs = List.fold_left
    (fun xs h -> Vars.union xs (F.varsp h))
    (F.varsp goal) (conditions w) in
  let gs = List.filter (fun g -> Vars.subset (F.varsp g) xs) gs in
  let bundle = if gs = [] then w.hs else hyp (GUARD (F.p_conj gs)) :: w.hs in
  if Wp_parameters.Qed.get () then
    begin
      let qed = qed_make () in
      let bundle = simplify_bundle qed bundle in
      match qed_assume qed (List.map cond_hyp bundle) with
	| None -> 
	    { hs = bundle ; qed = None } , p_true
	| Some qed ->
	    let goal = qed_simplify_pred qed goal in
	    let domain = { eq_var = Vmap.empty ; eq_fun = Vmap.empty } in
	    collect_atom domain goal ;
	    collect_bundle domain bundle ;
	    let bundle = filter_bundle domain bundle in
	    { hs = bundle ; qed = None } , goal
    end
  else
    { hs = bundle ; qed = None } , goal


let add_guards hyps goal guards =
  let xs = List.fold_left
    (fun xs h -> Vars.union xs (F.varsp h))
    (F.varsp goal) (conditions hyps) in
  let gs = List.filter (fun g -> Vars.subset (F.varsp g) xs) guards in
  let bundle = if gs = [] then hyps.hs else hyp (GUARD (F.p_conj gs)) :: hyps.hs in
  { hs = bundle ; qed = None }

let simplify_hyps hyps =
  let qed = qed_make () in
  context (simplify_bundle qed hyps.hs)

let simplify_goal hyps goal =
  let qed = qed_make () in
  let bundle = simplify_bundle qed hyps.hs in
  match qed_assume qed (List.map cond_hyp bundle) with
    | None -> 
	{ hs = bundle ; qed = None } , p_true
    | Some qed ->
	let goal = qed_simplify_pred qed goal in
	let domain = { eq_var = Vmap.empty ; eq_fun = Vmap.empty } in
	collect_atom domain goal ;
	collect_bundle domain bundle ;
	let bundle = filter_bundle domain bundle in
	{ hs = bundle ; qed = None } , goal

let close w p = F.p_close (F.p_hyps (conditions w) p)
