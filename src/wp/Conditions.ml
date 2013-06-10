(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Weakest Pre Accumulator                                            --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Cil_types
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Datatypes                                                          --- *)
(* -------------------------------------------------------------------------- *)

type step = {
  stmt : stmt option ;
  descr : string option ;
  deps : Property.t list ;
  warn : Warning.Set.t ;
  condition : condition ;
} and condition =
  | Type of pred
  | Have of pred
  | When of pred
  | Branch of pred * step list * step list
  | Either of step list list

let is_cond_true = function
  | Have p | Type p | When p -> F.is_ptrue p
  | Either [] -> No
  | Either [[]] -> Yes
  | Branch(_,[],[]) -> Yes
  | Branch _ | Either _ -> Maybe

let is_seq_true = function
  | [] -> Yes
  | s::_ -> is_cond_true s.condition

module Bundle :
sig
  type t
  val empty : t
  val is_empty : t -> bool
 (*  val non_empty : t -> bool *) (* unused for now *)
  val is_true : t -> Qed.Logic.maybe
  val add : step -> t -> t
  val factorize : t -> t -> t * t * t
  val big_inter : t list -> t
  val diff : t -> t -> t
  (* val iter : (step -> unit) -> t -> unit *) (* unused for now *)
  val freeze : t -> step list
  val exists : (condition -> bool) -> t -> bool
  val map : (condition -> 'a) -> t -> 'a list
  (* val debug : Format.formatter -> t -> unit *) (* unused for now *)
end =
struct
  module SEQ = Qed.Listset.Make
    (struct
       type t = int * step
       let equal (k1,_) (k2,_) = k1 = k2
       let compare (k1,s1) (k2,s2) = 
	 let rank = function
	   | Type _ -> 0
	   | When _ -> 1
	   | _ -> 2
	 in
	 let r = rank s1.condition - rank s2.condition in
	 if r = 0 then Pervasives.compare k2 k1 else r
     end)
  let cid = ref 0
  let fresh () = incr cid ; assert (!cid > 0) ; !cid
  type t = SEQ.t
  let add s t = SEQ.add (fresh (),s) t
  let empty = []
  let is_empty = function [] -> true | _ -> false
  (* unused for now *)
  (* let non_empty = function [] -> false | _ -> true *)
  let factorize = SEQ.factorize
  let big_inter = SEQ.big_inter
  let diff = SEQ.diff
  (* unused for now *)
  (* let iter f b = SEQ.iter (fun (_,s) -> f s) b *)
  let freeze b = List.map snd b
  let map f b = List.map (fun (_,s) -> f s.condition) b
  let exists f b = List.exists (fun (_,s) -> f s.condition) b
  (* unused for now *)
  (* let debug fmt hs = 
    begin
      Format.fprintf fmt "{" ;
      List.iter 
	(fun (id,step) -> 
	   Format.fprintf fmt " %d:" id ;
	   match step.condition with
	     | Have _ -> Format.fprintf fmt "H"
	     | Type _ -> Format.fprintf fmt "T"
	     | When _ -> Format.fprintf fmt "W"
	     | Branch _ -> Format.fprintf fmt "IF"
	     | Either cs -> Format.fprintf fmt "CS(%d)" (List.length cs)
	) hs ;
      Format.fprintf fmt " }" ;
    end *)
  let is_true = function
    | [] -> Yes
    | (_,s) :: _ -> is_cond_true s.condition
end
  
type bundle = Bundle.t
type t = step list

(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

type link = Lstmt of stmt | Lprop of Property.t
type linker = (string,link) Hashtbl.t	
let glinker = ref None
let pid = ref 0

let linker () = Hashtbl.create 131
let get_link = Hashtbl.find

let pp_link link pp fmt a =
  match !glinker with 
    | None -> pp fmt a 
    | Some href ->
	begin
	  let aref = match link with
	    | Lstmt s -> Printf.sprintf "s%d" s.sid
	    | Lprop _ -> incr pid ; Printf.sprintf "p%d" !pid
	  in
	  Hashtbl.add href aref link ;
	  Format.pp_open_tag fmt ("link:" ^ aref) ;
	  pp fmt a ;
	  Format.pp_close_tag fmt () ;
	end

let pp_loc fmt loc = 
  let file = loc.Lexing.pos_fname in
  let line = loc.Lexing.pos_lnum in
  Format.fprintf fmt "%s:%d: " file line

let pp_stmt fmt = function
  | None -> ()
  | Some stmt ->
      let loc = fst (Cil_datatype.Stmt.loc stmt) in
      pp_link (Lstmt stmt) pp_loc fmt loc

let pp_descr fmt s = 
  match s.descr with
    | None -> ()
    | Some msg -> 
	Format.fprintf fmt "@ @{<green>(* %a%s *)@}" pp_stmt s.stmt msg

let pp_depend fmt s p =
  let stmt = 
    match Property.get_kinstr p with Kstmt stmt -> Some stmt | _ -> s.stmt in
  Format.fprintf fmt "@ @{<blue>(* %a@[<hov 0>%a@]: *)@}" 
    pp_stmt stmt (pp_link (Lprop p) Description.pp_local) p
    
let pp_warning fmt w =
  Format.fprintf fmt 
    "@ @[<hov 0>@{<red>@{<bf>Warning@}@}[%s]: %s@ (%s).@]"
    w.Warning.source w.Warning.reason w.Warning.effect
    
let pp_clause fmt env title p =
  Format.fprintf fmt "@ @[<hov 2>@{<bf>%s@}: %a.@]" title (F.pp_epred env) p
    
let mark_seq m seq = List.iter 
  (fun s -> match s.condition with
     | When p | Type p | Have p | Branch(p,_,_) -> F.mark_p m p
     | Either _ -> ()
  ) seq

let rec pp_step fmt env s =
  begin
    pp_descr fmt s ;
    List.iter (pp_depend fmt s) s.deps ;
    Warning.Set.iter (pp_warning fmt) s.warn ;
    pp_condition fmt env s.condition ;
  end

and pp_condition fmt env = function
  | Type p -> pp_clause fmt env "Type" p
  | Have p -> pp_clause fmt env "Have" p
  | When p -> pp_clause fmt env "When" p
  | Branch(p,a,b) ->
      begin
	Format.fprintf fmt "@ @[<hov 2>@{<bf>If@}: %a@]" (F.pp_epred env) p ;
	if a<>[] then pp_sequence fmt "Then" env a ;
	if b<>[] then pp_sequence fmt "Else" env b ;
      end
  | Either cases ->
      begin
	Format.fprintf fmt "@[<hv 0>@[<hv 2>@{<bf>Either@} {" ;
	List.iter 
	  (fun seq -> 
	     Format.fprintf fmt "@ @[<hv 2>@{<bf>Case@}:" ;
	     pp_block fmt env seq ;
	     Format.fprintf fmt "@]" ;
	  ) cases ;
	Format.fprintf fmt "@]@ }@]" ;
      end

and pp_sequence fmt title env = function
  | [] -> Format.fprintf fmt "@ @{<bf>%s@} {}" title
  | seq ->
      begin
	Format.fprintf fmt "@ @[<hv 0>@[<hv 2>@{<bf>%s@} {" title ;
	pp_block fmt env seq ;
	Format.fprintf fmt "@]@ }@]" ;
      end

and pp_block fmt env seq =
  let m = F.marker env in
  mark_seq m seq ;
  let env = F.define
    (fun env x t ->
       Format.fprintf fmt "@ @[<hov 4>@{<bf>Let@} %s = %a.@]" 
	 x (F.pp_eterm env) t)
    env m in
  List.iter (pp_step fmt env) seq

let dump fmt (b:bundle) = pp_sequence fmt "Assume" F.empty (Bundle.freeze b)

let pp_seq title fmt s = pp_sequence fmt title F.empty s

(* -------------------------------------------------------------------------- *)
(* --- Extraction                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec occurs_cond x = function
  | Type p | When p | Have p -> Vars.mem x (F.varsp p)
  | Branch(p,a,b) -> Vars.mem x (F.varsp p) || occurs_seq x a || occurs_seq x b
  | Either cases -> List.exists (occurs_seq x) cases
and occurs_seq x seq = List.exists (fun s -> occurs_cond x s.condition) seq
let occurs x bundle = Bundle.exists (occurs_cond x) bundle

let rec intersect_cond p = function
  | Type q | When q | Have q -> F.intersectp p q
  | Branch(q,a,b) -> F.intersectp p q || intersect_seq p a || intersect_seq p b
  | Either cases -> List.exists (intersect_seq p) cases
and intersect_seq p s = List.exists (fun s -> intersect_cond p s.condition) s
let intersect p bundle = Bundle.exists (intersect_cond p) bundle

let rec vars_cond xs = function
  | Type q | When q | Have q -> vars_pred xs q
  | Branch(p,a,b) -> vars_seq (vars_seq (vars_pred xs p) a) b
  | Either cases -> List.fold_left vars_seq xs cases
and vars_seq xs seq = List.fold_left vars_step xs seq
and vars_step xs s = vars_cond xs s.condition
and vars_pred xs p = Vars.union xs (F.varsp p)

let rec pred_cond = function
  | When p | Type p | Have p -> p
  | Branch(p,a,b) -> F.p_if p (pred_seq a) (pred_seq b)
  | Either cases -> F.p_any pred_seq cases
and pred_seq seq = F.p_all (fun s -> pred_cond s.condition) seq
let extract bundle = Bundle.map pred_cond bundle
let freeze = Bundle.freeze

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printer                                                     --- *)
(* -------------------------------------------------------------------------- *)

let pretty ?linker fmt hyps goal = 
  try
    glinker := linker ;
    let env = F.closed (vars_seq (F.varsp goal) hyps) in 
    let m = F.marker env in
    mark_seq m hyps ;
    F.mark_p m goal ;
    let env = F.define
      (fun env x t ->
	 Format.fprintf fmt "@[<hov 4>@{<bf>Let@} %s = %a.@]@\n"
	   x (F.pp_eterm env) t
      ) env m in
    Format.fprintf fmt "@[<hv 0>@[<hv 2>@{<bf>Assume@} {" ;
    List.iter (pp_step fmt env) hyps ;
    Format.fprintf fmt "@]@ }@]@\n" ;
    Format.fprintf fmt "@[<hov 4>@{<bf>Prove:@} %a.@]@." 
      (F.pp_epred env) goal ;
    glinker := None ;
  with err ->
    glinker := None ; raise err

(* -------------------------------------------------------------------------- *)
(* --- Constructors                                                       --- *)
(* -------------------------------------------------------------------------- *)

let empty = Bundle.empty
let step ?descr ?stmt ?(deps=[]) ?(warn=Warning.Set.empty) cond = 
  {
    stmt = stmt ;
    descr = descr ;
    warn = warn ;
    deps = deps ;
    condition = cond ;
  }

type 'a disjunction = TRUE | FALSE | EITHER of 'a list

let disjunction phi cases = 
  let positive = ref false in
  (* invariant : OR { bundles } <-> ( positive \/ OR { filter } *)
  let remains = 
    List.filter
      (fun case -> match phi case with
	 | Yes -> positive := true ; false (* ? \/ True \/ Ci <-> True \/ filer Ci *)
	 | No -> false (* positive \/ False \/ Ci <-> positive \/ filter Ci *)
	 | Maybe -> true) (* positive \/ C \/ Ci <-> positive \/ C :: filter Ci *)
      cases in
  if remains = [] then
    if !positive then TRUE else FALSE
  else EITHER remains

(* -------------------------------------------------------------------------- *)
(* --- Constructors                                                       --- *)
(* -------------------------------------------------------------------------- *)

type 'a attributed = 
    ( ?descr:string ->
      ?stmt:stmt -> 
      ?deps:Property.t list ->
      ?warn:Warning.Set.t ->
      'a )

let domain ps hs =
  if ps = [] then hs else Bundle.add (step ~descr:"Domain" (Type (p_conj ps))) hs
let intros ps hs = 
  if ps = [] then hs else Bundle.add (step ~descr:"Goal" (When (p_conj ps))) hs

let assume ?descr ?stmt ?deps ?warn p hs =
  match F.is_ptrue p with
    | Yes -> hs
    | No ->
	let s = step ?descr ?stmt ?deps ?warn (Have p) in
	Bundle.add s Bundle.empty
    | Maybe ->
	begin
	  match Bundle.is_true hs with
	    | Yes | Maybe -> 
		let s = step ?descr ?stmt ?deps ?warn (Have p) in
		Bundle.add s hs
	    | No -> hs
	end

let branch ?descr ?stmt ?deps ?warn p ha hb =
  match F.is_ptrue p with
    | Yes -> ha
    | No -> hb
    | Maybe ->
	match Bundle.is_true ha , Bundle.is_true hb with
	  | Yes , Yes -> Bundle.empty
	  | _ , No -> assume ?descr ?stmt ?deps ?warn p ha
	  | No , _ -> assume ?descr ?stmt ?deps ?warn (p_not p) hb
	  | _ ->
	      let ha,hs,hb = Bundle.factorize ha hb in
	      if Bundle.is_empty ha && Bundle.is_empty hb then hs else
		let a = Bundle.freeze ha in
		let b = Bundle.freeze hb in
		let s = step ?descr ?stmt ?deps ?warn (Branch(p,a,b)) in
		Bundle.add s hs

let either ?descr ?stmt ?deps ?warn cases =
  match disjunction Bundle.is_true cases with
    | TRUE -> Bundle.empty
    | FALSE ->       
	let s = step ?descr ?stmt ?deps ?warn (Have p_false) in
	Bundle.add s Bundle.empty
    | EITHER cases ->
	let trunk = Bundle.big_inter cases in
	let cases = List.map (fun case -> Bundle.diff case trunk) cases in
	match disjunction Bundle.is_true cases with
	  | TRUE -> trunk
	  | FALSE -> 
	      let s = step ?descr ?stmt ?deps ?warn (Have p_false) in
	      Bundle.add s Bundle.empty
	  | EITHER cases ->
	      let cases = List.map Bundle.freeze cases in
	      let s = step ?descr ?stmt ?deps ?warn (Either cases) in
	      Bundle.add s trunk
      
let merge cases = either ~descr:"Merge" cases

(* -------------------------------------------------------------------------- *)
(* --- Flattening                                                         --- *)
(* -------------------------------------------------------------------------- *)

let flat_cons step tail =
  match is_seq_true tail with
    | Yes | Maybe -> step :: tail
    | No -> tail
  
let flat_concat head tail =
  match is_seq_true head with
    | Yes -> tail
    | No -> head
    | Maybe ->
	match is_seq_true tail with
	  | Yes -> head
	  | No -> tail
	  | Maybe -> head @ tail

let rec flatten_sequence m = function
  | [] -> []
  | step :: seq ->
	match step.condition with
	  | Have p | Type p | When p ->
	      begin
		match F.is_ptrue p with
		  | Yes -> m := true ; flatten_sequence m seq
		  | No -> if seq <> [] then m := true ; [step]
		  | Maybe -> flat_cons step (flatten_sequence m seq)
	      end
	  | Branch(p,sa,sb) ->
	      begin
		match F.is_ptrue p with
		  | Yes -> m := true ; flat_concat sa (flatten_sequence m seq)
		  | No -> m := true ; flat_concat sb (flatten_sequence m seq)
		  | Maybe -> 
		      match is_seq_true sa , is_seq_true sb with
			| Yes , Yes -> m := true ; flatten_sequence m seq
			| _ , No ->
			    m := true ;
			    let step = { step with condition = Have p } in
			    step :: sa @ flatten_sequence m seq
			| No , _ ->
			    m := true ;
			    let step = { step with condition = Have (p_not p) } in
			    step :: sb @ flatten_sequence m seq
			| _ ->
			    step :: flatten_sequence m seq
	      end
	  | Either [] -> [step]
	  | Either cases ->
	      match disjunction is_seq_true cases with
		| TRUE -> m := true ; flatten_sequence m seq
		| FALSE -> m := true ; [ { step with condition = Have p_false } ]
		| EITHER [hc] ->
		    m := true ; flat_concat hc (flatten_sequence m seq) 
		| EITHER cs ->
		    let step = { step with condition = Either cs } in
		    flat_cons step (flatten_sequence m seq)

(* -------------------------------------------------------------------------- *)
(* --- Letify                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Sigma = Letify.Sigma
module Defs = Letify.Defs

let used_of_dseq = Array.fold_left (fun ys (xs,_,_) -> Vars.union ys xs) Vars.empty
let bind_dseq target (_,di,_) sigma =
  Letify.bind (Letify.bind sigma di target) di (Defs.domain di)

let locals sigma ~target ~required ?(step=Vars.empty) k dseq = (* returns ( target , export ) *)
  let t = ref target in
  let e = ref (Vars.union required step) in
  Array.iteri
    (fun i (xs,_,_) ->
       if i > k then t := Vars.union !t xs ;
       if i <> k then e := Vars.union !e xs ;
    ) dseq ;
  Vars.diff !t (Sigma.domain sigma) , !e

let dseq_of_step sigma step =
  let xs = 
    match step.condition with
      | Type _ -> Vars.empty
      | cond -> vars_cond Vars.empty cond in
  let defs = 
    match step.condition with
      | Have p | When p -> Defs.extract (Sigma.p_apply sigma p)
      | Type _ | Branch _ | Either _ -> Defs.empty
  in (xs , defs , step)

let letify_assume sref (_,_,step) =
  let current = !sref in
  begin
    match step.condition with
      | Type _ | Branch _ | Either _ -> ()
      | Have p | When p ->
	  if Wp_parameters.Simpl.get () then
	    sref := Sigma.assume current p ;
  end ; current

let rec letify_type sigma used p = match F.pred p with
  | And ps -> p_all (letify_type sigma used) ps
  | _ -> 
      let p = Sigma.p_apply sigma p in
      if Vars.intersect used (F.varsp p) then p else F.p_true
	    
let rec letify_seq sigma0 ~target ~export (seq : step list) =
  let dseq = Array.map (dseq_of_step sigma0) (Array.of_list seq) in
  let sigma1 = Array.fold_right (bind_dseq target) dseq sigma0 in
  let sref = ref sigma1 in (* with definitions *)
  let dsigma = Array.map (letify_assume sref) dseq in
  let sigma2 = !sref in (* with assumptions *)
  let outside = Vars.union export target in
  let inside = used_of_dseq dseq in
  let used = Vars.diff (Vars.union outside inside) (Sigma.domain sigma2) in
  let required = Vars.union outside (Sigma.codomain sigma2) in
  let sequence =
    Array.mapi (letify_step dseq dsigma ~used ~required ~target) dseq in
  let modified = ref (not (Sigma.equal sigma0 sigma1)) in
  let sequence = flatten_sequence modified (Array.to_list sequence) in
  !modified , sigma1 , sigma2 , sequence

and letify_step dseq dsigma ~required ~target ~used i (_,d,s) =
  let sigma = dsigma.(i) in
  let cond =
    match s.condition with
      | Have p ->
	  let p = Sigma.p_apply sigma p in
	  let ps = Letify.add_definitions sigma d required [p] in
	  Have (p_conj ps)
      | When p -> When (Sigma.p_apply sigma p)
      | Type p -> Type (letify_type sigma used p)
      | Branch(p,a,b) ->
	  let p = Sigma.p_apply sigma p in
	  let step = F.varsp p in
	  let (target,export) = locals sigma ~target ~required ~step i dseq in
	  let sa = Sigma.assume sigma p in
	  let sb = Sigma.assume sigma (p_not p) in
	  let a = letify_case sa ~target ~export a in
	  let b = letify_case sb ~target ~export b in
	  Branch(p,a,b)
      | Either cases ->
	  let (target,export) = locals sigma ~target ~required i dseq in
	  Either (List.map (letify_case sigma ~target ~export) cases)
  in { s with condition = cond }

and letify_case sigma ~target ~export seq = 
  let (_,_,_,seq) = letify_seq sigma ~target ~export seq in seq

let rec fixpoint n sigma hs p =
  let target = F.varsp p in
  let export = Vars.empty in
  let modified , sigma1 , sigma2 , hs = letify_seq sigma ~target ~export hs in
  let p = Sigma.p_apply sigma2 p in
  if not modified then hs , p
  else fixpoint (succ n) sigma1 hs p

let letify hs p = fixpoint 0 Sigma.empty hs p

(* -------------------------------------------------------------------------- *)
(* --- Filtering                                                          --- *)
(* -------------------------------------------------------------------------- *)

let residual p = {
  stmt = None ;
  descr = Some "Residual" ;
  deps = [] ;
  warn = Warning.Set.empty ;
  condition = When p ;
}

let rec add_case p = function
  | ( { condition = (Type _) } as step ):: tail -> 
      step :: add_case p tail
  | hs -> residual p :: hs

let is_absurd h = match h.condition with
  | (Type p | When p | Have p) -> p == F.p_false
  | _ -> false

let is_trivial (hs,g) = g == F.p_true || List.exists is_absurd hs

let test_case p (hs,g) =
  let w = letify (add_case p hs) g in
  if is_trivial w then None else Some w

let tc = ref 0

let rec test_cases sequent = function
  | [] -> sequent
  | (p,_) :: tail ->
      match test_case p sequent , test_case (p_not p) sequent with
	| None , None -> incr tc ; [],F.p_true
	| Some w , None -> incr tc ; test_cases w tail
	| None , Some w -> incr tc ; test_cases w tail
	| Some _ , Some _ -> test_cases sequent tail

let rec collect_cond m = function
  | When _ | Have _ | Type _ -> ()
  | Branch(p,a,b) -> Letify.Split.add m p ; collect_seq m a ; collect_seq m b
  | Either cs -> List.iter (collect_seq m) cs
and collect_seq m seq = List.iter (fun s -> collect_cond m s.condition) seq

let pruning hs g =
  let sequent = (hs,g) in
  if is_trivial sequent then sequent
  else
    begin
      let m = Letify.Split.create () in
      collect_seq m hs ;
      tc := 0 ;
      let sequent = test_cases (hs,g) (Letify.Split.select m) in
      if !tc > 0 && Wp_parameters.has_dkey "pruning" then
	if is_trivial sequent then
	  Wp_parameters.feedback "[Pruning] Trivial"
	else
	  Wp_parameters.feedback "[Pruning] %d branche(s) removed" !tc ;
      sequent
    end

(* -------------------------------------------------------------------------- *)
(* --- Cleaning                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec collect_cond u = function
  | When p -> Cleaning.as_have u p
  | Have p -> Cleaning.as_have u p
  | Type p -> Cleaning.as_type u p
  | Branch(p,a,b) -> Cleaning.as_atom u p ; collect_seq u a ; collect_seq u b
  | Either cs -> List.iter (collect_seq u) cs
and collect_seq u seq = List.iter (fun s -> collect_cond u s.condition) seq

let rec clean_cond u = function
  | When p -> When (Cleaning.filter_pred u p)
  | Have p -> Have (Cleaning.filter_pred u p)
  | Type p -> Type (Cleaning.filter_pred u p)
  | Branch(p,a,b) -> Branch(p,clean_seq u a,clean_seq u b)
  | Either cases -> Either(List.map (clean_seq u) cases)

and clean_seq u = function
  | [] -> []
  | s :: seq ->
      let c = clean_cond u s.condition in
      let seq = clean_seq u seq in
      match is_cond_true c with
	| Yes -> seq
	| No -> [{ s with condition = c }]
	| Maybe -> { s with condition = c } :: seq

let clean hs p =
  let u = Cleaning.create () in
  Cleaning.as_atom u p ; collect_seq u hs ; clean_seq u hs , p

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let hypotheses hs = List.map (fun s -> pred_cond s.condition) hs
let close hs goal = F.p_close (F.p_hyps (hypotheses hs) goal)

(* -------------------------------------------------------------------------- *)
