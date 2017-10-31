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

(* -------------------------------------------------------------------------- *)
(* --- Weakest Pre Accumulator                                            --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Cil_types
open Lang
open Lang.F

let dkey_pruning = Wp_parameters.register_category "pruning"

(* -------------------------------------------------------------------------- *)
(* --- Category                                                           --- *)
(* -------------------------------------------------------------------------- *)

type category =
  | EMPTY  (** Empty Sequence, equivalent to True, but with State. *)
  | TRUE   (** Logically equivalent to True *)
  | FALSE  (** Logically equivalent to False *)
  | MAYBE  (** Any Hypothesis *)

let c_and c1 c2 =
  match c1 , c2 with
  | FALSE , _ | _ , FALSE -> FALSE
  | MAYBE , _ | _ , MAYBE -> MAYBE
  | TRUE , _  | _ , TRUE -> TRUE
  | EMPTY , EMPTY -> EMPTY

let c_or c1 c2 =
  match c1 , c2 with
  | FALSE , FALSE -> FALSE
  | EMPTY , EMPTY -> EMPTY
  | TRUE , TRUE -> TRUE
  | _ -> MAYBE

let rec cfold_and a f = function
  | [] -> a | e::es -> cfold_and (c_and a (f e)) f es

let rec cfold_or a f = function
  | [] -> a | e::es -> cfold_or (c_or a (f e)) f es

let c_conj f es = cfold_and EMPTY f es
let c_disj f = function [] -> FALSE | e::es -> cfold_or (f e) f es

(* -------------------------------------------------------------------------- *)
(* --- Datatypes                                                          --- *)
(* -------------------------------------------------------------------------- *)

type step = {
  mutable id : int ; (* step identifier *)
  size : int ; (* number of conditions *)
  vars : Vars.t ;
  stmt : stmt option ;
  descr : string option ;
  deps : Property.t list ;
  warn : Warning.Set.t ;
  condition : condition ;
}
and sequence = {
  seq_size : int ;
  seq_vars : Vars.t ;
  seq_core : Pset.t ;
  seq_catg : category ;
  seq_list : step list ;
}
and condition =
  | Type of pred
  | Have of pred
  | When of pred
  | Core of pred
  | Init of pred
  | Branch of pred * sequence * sequence
  | Either of sequence list
  | State of Mstate.state

(* -------------------------------------------------------------------------- *)
(* --- Variable Utilities                                                 --- *)
(* -------------------------------------------------------------------------- *)

let vars_seqs w = List.fold_left (fun xs s -> Vars.union xs s.seq_vars) Vars.empty w
let vars_list s = List.fold_left (fun xs s -> Vars.union xs s.vars) Vars.empty s
let size_list s = List.fold_left (fun n s -> n + s.size) 0 s
let vars_cond = function
  | Type q | When q | Have q | Core q | Init q -> F.varsp q
  | Branch(p,sa,sb) -> Vars.union (F.varsp p) (Vars.union sa.seq_vars sb.seq_vars)
  | Either cases -> vars_seqs cases
  | State _ -> Vars.empty
let size_cond = function
  | Type _ | When _ | Have _ | Core _ | Init _ | State _ -> 1
  | Branch(_,sa,sb) -> 1 + sa.seq_size + sb.seq_size
  | Either cases -> List.fold_left (fun n s -> n + s.seq_size) 1 cases
let vars_hyp hs = hs.seq_vars
let vars_seq (hs,g) = Vars.union (F.varsp g) hs.seq_vars

(* -------------------------------------------------------------------------- *)
(* --- Core Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let is_core p = match F.e_expr p with
  (*  | Qed.Logic.Eq (a,b) -> is_def a && is_def b *)
  | Qed.Logic.Eq _ -> true
  | _ -> false

let rec add_core s p = match F.p_expr p with
  | Qed.Logic.And ps -> List.fold_left add_core Pset.empty ps
  | _ -> if is_core p then Pset.add p s else s

let core_cond = function
  | Type _ | State _ -> Pset.empty
  | Have p | When p | Core p | Init p -> add_core Pset.empty p
  | Branch(_,sa,sb) -> Pset.inter sa.seq_core sb.seq_core
  | Either [] -> Pset.empty
  | Either (c::cs) ->
      List.fold_left (fun w s -> Pset.inter w s.seq_core) c.seq_core cs

let add_core_step ps s = Pset.union ps (core_cond s.condition)

let core_list s = List.fold_left add_core_step Pset.empty s

(* -------------------------------------------------------------------------- *)
(* --- Category                                                           --- *)
(* -------------------------------------------------------------------------- *)

let catg_seq s = s.seq_catg
let catg_cond = function
  | State _ -> TRUE
  | Have p | Type p | When p | Core p | Init p ->
      begin
        match F.is_ptrue p with
        | No -> FALSE
        | Maybe -> MAYBE
        | Yes -> EMPTY
      end
  | Either cs -> c_disj catg_seq cs
  | Branch(_,a,b) -> c_or a.seq_catg b.seq_catg
let catg_step s = catg_cond s.condition
let catg_list l = c_conj catg_step l

(* -------------------------------------------------------------------------- *)
(* --- Sequence Constructor                                               --- *)
(* -------------------------------------------------------------------------- *)

let sequence l = {
  seq_size = size_list l ;
  seq_vars = vars_list l ;
  seq_core = core_list l ;
  seq_catg = catg_list l ;
  seq_list = l ;
}

(* -------------------------------------------------------------------------- *)
(* --- Sequence Comparator                                                --- *)
(* -------------------------------------------------------------------------- *)

let rec equal_cond ca cb =
  match ca,cb with
  | State _ , State _ -> true
  | Type p , Type q
  | Have p , Have q
  | When p , When q
  | Core p , Core q
  | Init p , Init q
    -> p == q
  | Branch(p,a,b) , Branch(q,a',b') ->
      p == q && equal_seq a a' && equal_seq b b'
  | Either u, Either v ->
      Qed.Hcons.equal_list equal_seq u v
  | State _ , _ | _ , State _
  | Type _ , _ | _ , Type _
  | Have _ , _ | _ , Have _
  | When _ , _ | _ , When _ 
  | Core _ , _ | _ , Core _ 
  | Init _ , _ | _ , Init _ 
  | Branch _ , _ | _ , Branch _
    -> false

and equal_step a b =
  equal_cond a.condition b.condition

and equal_list sa sb =
  Qed.Hcons.equal_list equal_step sa sb

and equal_seq sa sb =
  equal_list sa.seq_list sb.seq_list

(* -------------------------------------------------------------------------- *)
(* --- Core Inference                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Core =
struct

  let rec fpred core p = match F.p_expr p with
    | Qed.Logic.And ps -> F.p_conj (List.map (fpred core) ps)
    | _ -> if Pset.mem p core then p_true else p

  let fcond core = function
    | Core p -> Core (fpred core p)
    | Have p -> Have (fpred core p)
    | When p -> When (fpred core p)
    | Init p -> Init (fpred core p)
    | (Type _ | Branch _ | Either _ | State _) as cond -> cond

  let fstep core step =
    let condition = fcond core step.condition in
    let vars = vars_cond condition in
    { step with condition ; vars }

  let factorize a b =
    if Wp_parameters.Core.get () then
      let core = Pset.inter a.seq_core b.seq_core in
      if Pset.is_empty core then None else
        let ca = List.map (fstep core) a.seq_list in
        let cb = List.map (fstep core) b.seq_list in
        Some (F.p_conj (Pset.elements core) , sequence ca , sequence cb)
    else None

end

(* -------------------------------------------------------------------------- *)
(* --- Bundle (non-simplified conditions)                                 --- *)
(* -------------------------------------------------------------------------- *)

module Bundle :
sig
  type t
  val empty : t
  val vars : t -> Vars.t
  val is_empty : t -> bool
  val category : t -> category
  val add : step -> t -> t
  val factorize : t -> t -> t * t * t
  val big_inter : t list -> t
  val diff : t -> t -> t
  val head : t -> Mstate.state option
  val freeze: ?join:step -> t -> sequence
  val map : (condition -> 'a) -> t -> 'a list
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

  type t = Vars.t * SEQ.t

  let vars = fst
  let cid = ref 0
  let fresh () = incr cid ; assert (!cid > 0) ; !cid

  let add s (xs,t) = Vars.union xs s.vars , SEQ.add (fresh (),s) t

  let empty = Vars.empty , []
  let is_empty = function (_,[]) -> true | _ -> false

  let head = function _,(_,{ condition = State s }) :: _ -> Some s | _ -> None

  let build seq =
    let xs = List.fold_left
        (fun xs (_,s) -> Vars.union xs s.vars) Vars.empty seq in
    xs , seq

  let factorize (_,a) (_,b) =
    let l,m,r = SEQ.factorize a b in
    build l , build m , build r

  let big_inter cs = build (SEQ.big_inter (List.map snd cs))
  let diff (_,a) (_,b) = build (SEQ.diff a b)
  let freeze ?join (seq_vars,bundle) =
    let seq = List.map snd bundle in
    let seq_list = match join with None -> seq | Some s -> seq @ [s] in
    let seq_size = size_list seq in
    let seq_catg = catg_list seq in
    { seq_size ; seq_vars ; seq_core = Pset.empty ; seq_catg ; seq_list }
  let map f b = List.map (fun (_,s) -> f s.condition) (snd b)
  let category (_,bundle) = c_conj (fun (_,s) -> catg_step s) bundle
end

(* -------------------------------------------------------------------------- *)
(* --- Hypotheses                                                         --- *)
(* -------------------------------------------------------------------------- *)

type bundle = Bundle.t
type sequent = sequence * F.pred

let pretty = ref (fun _ _ -> ())
let is_true = function { seq_catg = TRUE | EMPTY } -> true | _ -> false
let is_empty = function { seq_catg = EMPTY } -> true | _ -> false

let is_absurd_h h = match h.condition with
  | (Core p | When p | Have p) -> p == F.p_false
  | _ -> false

let is_trivial_h h = match h.condition with
  | State _ -> false
  | (Type p | Core p | When p | Have p | Init p) -> p == F.p_true
  | Branch(_,a,b) -> is_true a && is_true b
  | Either w -> List.for_all is_true w

let is_trivial_hs_p hs p = p == F.p_true || List.exists is_absurd_h hs
let is_trivial_hsp (hs,p) = is_trivial_hs_p hs p
let is_trivial (s:sequent) = is_trivial_hs_p (fst s).seq_list (snd s)

(* -------------------------------------------------------------------------- *)
(* --- Extraction                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec pred_cond = function
  | State _ -> F.p_true
  | When p | Type p | Have p | Core p | Init p -> p
  | Branch(p,a,b) -> F.p_if p (pred_seq a) (pred_seq b)
  | Either cases -> F.p_any pred_seq cases
and pred_seq seq = F.p_all (fun s -> pred_cond s.condition) seq.seq_list
let extract bundle = Bundle.map pred_cond bundle
let bundle = Bundle.freeze ?join:None

let intersect p bundle = Vars.intersect (F.varsp p) (Bundle.vars bundle)
let occurs x bundle = Vars.mem x (Bundle.vars bundle)

(* -------------------------------------------------------------------------- *)
(* --- Constructors                                                       --- *)
(* -------------------------------------------------------------------------- *)

let nil = Bundle.empty
let noid = (-1)

let step ?descr ?stmt ?(deps=[]) ?(warn=Warning.Set.empty) cond =
  {
    id = noid ;
    size = size_cond cond ;
    vars = vars_cond cond ;
    stmt = stmt ;
    descr = descr ;
    warn = warn ;
    deps = deps ;
    condition = cond ;
  }

let update_cond ?descr ?(deps=[]) ?(warn=Warning.Set.empty) h c =
  let descr = match h.descr, descr  with
    | None, _ -> descr ;
    | Some _, None -> h.descr ;
    | Some decr1, Some descr2 -> Some (decr1 ^ "-" ^ descr2)
  in {
    id = noid ;
    condition = c ;
    stmt = h.stmt ;
    size = size_cond c ;
    vars = vars_cond c ;
    descr = descr ;
    deps = deps@h.deps ;
    warn = Warning.Set.union h.warn warn ;
  }

type 'a disjunction = D_TRUE | D_FALSE | D_EITHER of 'a list

let disjunction phi es =
  let positives = ref false in (* TRUE or EMPTY items *)
  let remains = List.filter
      (fun e ->
         match phi e with
         | TRUE | EMPTY -> positives := true ; false
         | MAYBE -> true
         | FALSE -> false
      ) es in
  match remains with
  | [] -> if !positives then D_TRUE else D_FALSE
  | cs -> D_EITHER cs

(* -------------------------------------------------------------------------- *)
(* --- Prenex-Form Introduction                                           --- *)
(* -------------------------------------------------------------------------- *)

let prenex_intro p =
  try
    let open Qed.Logic in
    (* invariant: xs <> []; result <-> forall xs, hs -> p *)
    let rec walk hs xs p =
      match F.p_expr p with
      | Imply(h,p) -> walk (h::hs) xs p
      | Bind(Forall,tau,p) -> bind hs xs tau p
      | _ ->
          if hs = [] then raise Exit ;
          F.p_forall (List.rev xs) (F.p_hyps (List.concat hs) p)
    (* invariant: result <-> forall hs xs (\tau.bind) *)
    and bind hs xs tau bind =
      let x = Lang.freshvar tau in
      let p = F.p_bool (F.QED.lc_open x bind) in
      walk hs (x::xs) p
    (* invariant: result <-> p *)
    and crawl p =
      match F.p_expr p with
      | Imply(h,p) -> F.p_hyps h (crawl p)
      | Bind(Forall,tau,p) -> bind [] [] tau p
      | _ -> raise Exit
    in crawl p
  with Exit -> p

(* -------------------------------------------------------------------------- *)
(* --- Existential Introduction                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec exist_intro p =
  let open Qed.Logic in
  match F.p_expr p with
  | And ps -> F.p_all exist_intro ps
  | Bind(Exists,tau,p) ->
      let x = Lang.freshvar tau in
      exist_intro (F.p_bool (F.QED.lc_open x p))
  | _ ->
      if Wp_parameters.Prenex.get ()
      then prenex_intro p
      else p

let rec exist_intros = function
  | [] -> []
  | p::hs -> begin
      let open Qed.Logic in
      match F.p_expr p with
      | And ps -> exist_intros (ps@hs)
      | Bind(Exists,tau,p) ->
          let x = Lang.freshvar tau in
          exist_intros ((F.p_bool (F.QED.lc_open x p))::hs)
      | _ -> p::(exist_intros hs)
    end

(* -------------------------------------------------------------------------- *)
(* --- Universal Introduction                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec forall_intro p =
  let open Qed.Logic in
  match F.p_expr p with
  | Bind(Forall,tau,p) ->
      let x = Lang.freshvar tau in
      forall_intro (F.p_bool (F.QED.lc_open x p))
  | Imply(hs,p) ->
      let hs = exist_intros hs in
      let hs = List.map (fun p -> step (Have p)) hs in
      let hp,p = forall_intro p in
      hs @ hp , p
  | _ -> [] , p

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
  if ps = [] then hs else
    Bundle.add (step (Type (p_conj ps))) hs

let intros ps hs =
  if ps = [] then hs else
    let p = F.p_all exist_intro ps in
    Bundle.add (step ~descr:"Goal" (When p)) hs

let state ?descr ?stmt state hs =
  let cond = State state in
  let s = step ?descr ?stmt cond in
  Bundle.add s hs

let assume ?descr ?stmt ?deps ?warn ?(init=false) p hs =
  match F.is_ptrue p with
  | Yes -> hs
  | No ->
      let cond = if init then Init p else Have p in
      let s = step ?descr ?stmt ?deps ?warn cond in
      Bundle.add s Bundle.empty
  | Maybe ->
      begin
        match Bundle.category hs with
        | MAYBE | TRUE | EMPTY ->
            let p = exist_intro p in
            let cond = if init then Init p else Have p in
            let s = step ?descr ?stmt ?deps ?warn cond in
            Bundle.add s hs
        | FALSE -> hs
      end

let join = function None -> None | Some s -> Some (step (State s))

let branch ?descr ?stmt ?deps ?warn p ha hb =
  match F.is_ptrue p with
  | Yes -> ha
  | No -> hb
  | Maybe ->
      match Bundle.category ha , Bundle.category hb with
      | TRUE , TRUE -> Bundle.empty
      | _ , FALSE -> assume ?descr ?stmt ?deps ?warn p ha
      | FALSE , _ -> assume ?descr ?stmt ?deps ?warn (p_not p) hb
      | _ ->
          let ha,hs,hb = Bundle.factorize ha hb in
          if Bundle.is_empty ha && Bundle.is_empty hb then hs else
            let join = join (Bundle.head hs) in
            let a = Bundle.freeze ?join ha in
            let b = Bundle.freeze ?join hb in
            let s = step ?descr ?stmt ?deps ?warn (Branch(p,a,b)) in
            Bundle.add s hs

let either ?descr ?stmt ?deps ?warn cases =
  match disjunction Bundle.category cases with
  | D_TRUE -> Bundle.empty
  | D_FALSE ->
      let s = step ?descr ?stmt ?deps ?warn (Have p_false) in
      Bundle.add s Bundle.empty
  | D_EITHER cases ->
      let trunk = Bundle.big_inter cases in
      let cases = List.map (fun case -> Bundle.diff case trunk) cases in
      match disjunction Bundle.category cases with
      | D_TRUE -> trunk
      | D_FALSE ->
          let s = step ?descr ?stmt ?deps ?warn (Have p_false) in
          Bundle.add s Bundle.empty
      | D_EITHER cases ->
          let cases = List.map Bundle.freeze cases in
          let s = step ?descr ?stmt ?deps ?warn (Either cases) in
          Bundle.add s trunk

let merge cases = either ~descr:"Merge" cases

(* -------------------------------------------------------------------------- *)
(* --- Flattening                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec flat_catg = function
  | [] -> EMPTY
  | s::cs ->
      match catg_step s with
      | EMPTY -> flat_catg cs
      | r -> r

let flat_cons step tail =
  match flat_catg tail with
  | FALSE -> tail
  | _ -> step :: tail

let flat_concat head tail =
  match flat_catg head with
  | EMPTY -> tail
  | FALSE -> head
  | MAYBE|TRUE ->
      match flat_catg tail with
      | EMPTY -> head
      | FALSE -> tail
      | MAYBE|TRUE -> head @ tail

let core_residual step core = {
  id = noid ;
  size = 1 ;
  vars = F.varsp core ;
  condition = Core core ;
  descr = None ;
  warn = Warning.Set.empty ;
  deps = [] ;
  stmt = step.stmt ;
}

let core_branch step p a b =
  let condition =
    match a.seq_catg , b.seq_catg with
    | (TRUE | EMPTY) , (TRUE|EMPTY) -> Have p_true
    | _ -> Branch(p,a,b)
  in update_cond step condition

let rec flatten_sequence m = function
  | [] -> []
  | step :: seq ->
      match step.condition with
      | State _ -> flat_cons step (flatten_sequence m seq)
      | Have p | Type p | When p | Core p | Init p ->
          begin
            match F.is_ptrue p with
            | Yes -> m := true ; flatten_sequence m seq
            | No -> (* FALSE context *) if seq <> [] then m := true ; [step]
            | Maybe -> flat_cons step (flatten_sequence m seq)
          end
      | Branch(p,a,b) ->
          begin
            match F.is_ptrue p with
            | Yes -> m := true ; flat_concat a.seq_list (flatten_sequence m seq)
            | No -> m := true ; flat_concat b.seq_list (flatten_sequence m seq)
            | Maybe ->
                let sa = a.seq_list in
                let sb = b.seq_list in
                match a.seq_catg , b.seq_catg with
                | (TRUE|EMPTY) , (TRUE|EMPTY) ->
                    m := true ; flatten_sequence m seq
                | _ , FALSE ->
                    m := true ;
                    let step = update_cond step (Have p) in
                    step :: sa @ flatten_sequence m seq
                | FALSE , _ ->
                    m := true ;
                    let step = update_cond step (Have (p_not p)) in
                    step :: sb @ flatten_sequence m seq
                | _ ->
                    begin
                      match Core.factorize a b with
                      | None -> step :: flatten_sequence m seq
                      | Some( core , a , b ) ->
                          m := true ;
                          let score = core_residual step core in
                          let scond = core_branch step p a b in
                          score :: scond :: flatten_sequence m seq
                    end
          end
      | Either [] -> (* FALSE context *) if seq <> [] then m := true ; [step]
      | Either cases ->
          match disjunction catg_seq cases with
          | D_TRUE -> m := true ; flatten_sequence m seq
          | D_FALSE -> m := true ; [ update_cond step (Have p_false) ]
          | D_EITHER [hc] ->
              m := true ; flat_concat hc.seq_list (flatten_sequence m seq)
          | D_EITHER cs ->
              let step = update_cond step (Either cs) in
              flat_cons step (flatten_sequence m seq)

(* -------------------------------------------------------------------------- *)
(* --- Mapping                                                            --- *)
(* -------------------------------------------------------------------------- *)

let lift f e = F.e_prop (f (F.p_bool e))

let rec map_condition f = function
  | State s -> State (Mstate.apply (lift f) s)
  | Have p -> Have (f p)
  | Type p -> Type (f p)
  | When p -> When (f p)
  | Core p -> Core (f p)
  | Init p -> Init (f p)
  | Branch(p,a,b) -> Branch(f p,map_sequence f a,map_sequence f b)
  | Either cs -> Either (List.map (map_sequence f) cs)

and map_step f h = update_cond h (map_condition f h.condition)

and map_steplist f = function
  | [] -> []
  | h::hs ->
      let h = map_step f h in
      let hs = map_steplist f hs in
      if is_trivial_h h then hs else h :: hs

and map_sequence f s =
  sequence (map_steplist f s.seq_list)

and map_sequent f (hs,g) = map_sequence f hs , f g

(* -------------------------------------------------------------------------- *)
(* --- Ground Simplifier                                                  --- *)
(* -------------------------------------------------------------------------- *)

module Ground = Letify.Ground

let rec ground_flow ~fwd env h =
  match h.condition with
  | State s ->
      let s = Mstate.apply (Ground.e_apply env) s in
      update_cond h (State s)
  | Type _ | Have _ | When _ | Core _ | Init _ ->
      let phi = if fwd then Ground.forward else Ground.backward in
      let cond = map_condition (phi env) h.condition in
      update_cond h cond
  | Branch(p,a,b) ->
      let p,wa,wb = Ground.branch env p in
      let a = ground_flowseq ~fwd wa a in
      let b = ground_flowseq ~fwd wb b in
      update_cond h (Branch(p,a,b))
  | Either ws ->
      let ws = List.map
          (fun w -> ground_flowseq ~fwd (Ground.copy env) w) ws in
      update_cond h (Either ws)

and ground_flowseq ~fwd env hs =
  sequence (ground_flowlist ~fwd env hs.seq_list)

and ground_flowlist ~fwd env hs =
  if fwd
  then ground_flowdir ~fwd env hs
  else List.rev (ground_flowdir ~fwd env (List.rev hs))

and ground_flowdir ~fwd env = function
  | [] -> []
  | h::hs ->
      let h = ground_flow ~fwd env h in
      let hs = ground_flowdir ~fwd env hs in
      if is_trivial_h h then hs else h :: hs

let ground (hs,g) =
  let hs = ground_flowlist ~fwd:true (Ground.top ()) hs in
  let hs = ground_flowlist ~fwd:false (Ground.top ()) hs in
  let env = Ground.top () in
  let hs = ground_flowlist ~fwd:true env hs in
  hs , Ground.p_apply env g

(* -------------------------------------------------------------------------- *)
(* --- Letify                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Sigma = Letify.Sigma
module Defs = Letify.Defs

let used_of_dseq ds =
  Array.fold_left (fun ys (_,step) -> Vars.union ys step.vars) Vars.empty ds

let bind_dseq target (di,_) sigma =
  Letify.bind (Letify.bind sigma di target) di (Defs.domain di)

let locals sigma ~target ~required ?(step=Vars.empty) k dseq =
  (* returns ( target , export ) *)
  let t = ref target in
  let e = ref (Vars.union required step) in
  Array.iteri
    (fun i (_,step) ->
       if i > k then t := Vars.union !t step.vars ;
       if i <> k then e := Vars.union !e step.vars ;
    ) dseq ;
  Vars.diff !t (Sigma.domain sigma) , !e

let dseq_of_step sigma step =
  let defs =
    match step.condition with
    | Init p | Have p | When p | Core p -> Defs.extract (Sigma.p_apply sigma p)
    | Type _ | Branch _ | Either _ | State _ -> Defs.empty
  in defs , step

let letify_assume sref (_,step) =
  let current = !sref in
  begin
    match step.condition with
    | Type _ | Branch _ | Either _ | State _ -> ()
    | Init p | Have p | When p | Core p ->
        if Wp_parameters.Simpl.get () then
          sref := Sigma.assume current p
  end ; current

[@@@ warning "-32"]
let rec letify_type sigma used p = match F.p_expr p with
  | And ps -> p_all (letify_type sigma used) ps
  | _ ->
      let p = Sigma.p_apply sigma p in
      if Vars.intersect used (F.varsp p) then p else F.p_true
[@@@ warning "+32"]

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
(*
  let sequence =
    if Wp_parameters.Ground.get () then fst (ground_hrp sequence)
    else sequence in
*)
  let sequence = flatten_sequence modified (Array.to_list sequence) in
  !modified , sigma1 , sigma2 , sequence

and letify_step dseq dsigma ~required ~target ~used i (d,s) =
  let sigma = dsigma.(i) in
  let cond = match s.condition with
    | State s -> State (Mstate.apply (Sigma.e_apply sigma) s)
    | Init p ->
        let p = Sigma.p_apply sigma p in
        let ps = Letify.add_definitions sigma d required [p] in
        Init (p_conj ps)
    | Have p ->
        let p = Sigma.p_apply sigma p in
        let ps = Letify.add_definitions sigma d required [p] in
        Have (p_conj ps)
    | Core p ->
        let p = Sigma.p_apply sigma p in
        let ps = Letify.add_definitions sigma d required [p] in
        Core (p_conj ps)
    | When p ->
        let p = Sigma.p_apply sigma p in
        let ps = Letify.add_definitions sigma d required [p] in
        When (p_conj ps)
    | Type p ->
        Type (letify_type sigma used p)
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
  in update_cond s cond

and letify_case sigma ~target ~export seq =
  let (_,_,_,s) = letify_seq sigma ~target ~export seq.seq_list
  in sequence s

(* -------------------------------------------------------------------------- *)
(* --- External Simplifier                                                --- *)
(* -------------------------------------------------------------------------- *)

exception Contradiction

class type simplifier =
  object
    method name : string
    method copy : simplifier
    method assume : F.pred -> unit
    method target : F.pred -> unit
    method fixpoint : unit
    method infer : F.pred list

    method simplify_exp : F.term -> F.term
    method simplify_hyp : F.pred -> F.pred
    method simplify_branch : F.pred -> F.pred
    method simplify_goal : F.pred -> F.pred
  end

let simplify_exp solvers e =
  List.fold_left (fun e s -> s#simplify_exp e) e solvers
let simplify_goal solvers p =
  List.fold_left (fun p s -> s#simplify_goal p) p solvers
let simplify_hyp solvers p =
  List.fold_left (fun p s -> s#simplify_hyp p) p solvers
let simplify_branch solvers p =
  List.fold_left (fun p s -> s#simplify_branch p) p solvers

let apply_hyp modified solvers h =
  let simple p =
    let p' = simplify_hyp solvers p in
    if not (Lang.F.eqp p p') then modified := true;
    List.iter (fun s -> s#assume p') solvers; p'
  in
  match h.condition with
  | State s -> update_cond h (State (Mstate.apply (simplify_exp solvers) s))
  | Init p -> update_cond h (Init (simple p))
  | Type p -> update_cond h (Type (simple p))
  | Have p -> update_cond h (Have (simple p))
  | When p -> update_cond h (When (simple p))
  | Core p -> update_cond h (Core (simple p))
  | Branch(p,_,_) -> List.iter (fun s -> s#target p) solvers; h
  | Either _ -> h

let decide_branch modified solvers h =
  match h.condition with
  | Branch(p,a,b) ->
      let q = simplify_branch solvers p in
      if q != p then
        ( modified := true ; update_cond h (Branch(q,a,b)) )
      else h
  | _ -> h

let add_infer modified s hs =
  let p = p_conj s#infer in
  if p != p_true then
    ( modified := true ; step ~descr:s#name (Have p) :: hs )
  else
    hs

type outcome =
  | NoSimplification
  | Simplified of hsp
  | Trivial

and hsp = step list * pred

let simplify (solvers : simplifier list) (hs,g) =
  if solvers = [] then NoSimplification
  else
    try
      let modified = ref false in
      let solvers = List.map (fun s -> s#copy) solvers in
      let hs = List.map (apply_hyp modified solvers) hs in
      List.iter (fun s -> s#target g) solvers ;
      List.iter (fun s -> s#fixpoint) solvers ;
      let hs = List.map (decide_branch modified solvers) hs in
      let hs = List.fold_right (add_infer modified) solvers hs in
      let p = simplify_goal solvers g in
      if p != g || !modified then
        Simplified (hs,p)
      else
        NoSimplification
    with Contradiction ->
      Trivial

(* -------------------------------------------------------------------------- *)
(* --- Sequence Builder                                                   --- *)
(* -------------------------------------------------------------------------- *)

let empty = {
  seq_size = 0 ;
  seq_vars = Vars.empty ;
  seq_core = Pset.empty ;
  seq_catg = EMPTY ;
  seq_list = [] ;
}

let append sa sb =
  if sa.seq_size = 0 then sb else
  if sb.seq_size = 0 then sa else
    let seq_size = sa.seq_size + sb.seq_size in
    let seq_vars = Vars.union sa.seq_vars sb.seq_vars in
    let seq_core = Pset.union sa.seq_core sb.seq_core in
    let seq_list = sa.seq_list @ sb.seq_list in
    let seq_catg = c_and sa.seq_catg sb.seq_catg in
    { seq_size ; seq_vars ; seq_core ; seq_catg ; seq_list }

let concat slist =
  if slist = [] then empty else
    let seq_size = List.fold_left (fun n s -> n + s.seq_size) 0 slist in
    let seq_list = List.concat (List.map (fun s -> s.seq_list) slist) in 
    let seq_vars = List.fold_left (fun w s -> Vars.union w s.seq_vars)
        Vars.empty slist in
    let seq_core = List.fold_left (fun w s -> Pset.union w s.seq_core)
        Pset.empty slist in
    let seq_catg = c_conj catg_seq slist in
    { seq_size ; seq_vars ; seq_core ; seq_catg ; seq_list }

let seq_branch ?stmt p sa sb =
  sequence [step ?stmt (Branch(p,sa,sb))]

(* -------------------------------------------------------------------------- *)
(* --- Introduction Utilities                                             --- *)
(* -------------------------------------------------------------------------- *)

let lemma g =
  let cc g = let hs,p = forall_intro g in sequence hs , p
  in Lang.local ~vars:(F.varsp g) cc g

let introduction ((hs,g) as s) =
  let cc s =
    let flag = ref true in
    let intro p = let q = exist_intro p in if q != p then flag := true ; q in
    let hj = List.map (map_step intro) hs.seq_list in
    let hi,p = forall_intro g in
    if not !flag && hi == [] then
      if p == g then s else hs , p
    else
      sequence (hi @ hj) , p
  in Lang.local ~vars:(vars_seq s) cc s

(* -------------------------------------------------------------------------- *)
(* --- Constant Folder                                                    --- *)
(* -------------------------------------------------------------------------- *)

module ConstantFolder =
struct

  open Qed

  type sigma = {
    mutable cst : bool Tmap.t ;
    mutable dom : Vars.t ; (* support of defs *)
    mutable def : term Tmap.t ; (* defs *)
    mutable mem : term Tmap.t ; (* defs+memo *)
  }

  let rec is_cst s e = match F.repr e with
    | True | False | Kint _ | Kreal _ -> true
    | Fun(_,es) ->
        begin
          try Tmap.find e s.cst
          with Not_found ->
            let cst = List.for_all (is_cst s) es in
            s.cst <- Tmap.add e cst s.cst ; cst
        end
    | _ -> false

  let set_def s p a e =
    try
      let e0 = Tmap.find a s.def in
      match F.is_true (F.e_eq e e0) with
      | Logic.Yes -> ()
      | Logic.No -> raise Contradiction
      | Logic.Maybe ->
          if F.compare e e0 < 0 then s.def <- Tmap.add a e s.def
    with Not_found ->
      begin
        s.dom <- Vars.union (F.vars a) s.dom ;
        s.def <- Tmap.add a e s.def ;
        s.def <- Tmap.add p p s.def ;
      end

  let rec assume s p = match F.repr p with
    | Logic.And ps -> List.iter (assume s) ps
    | Logic.Eq(a,b) ->
        if is_cst s a then set_def s p b a ;
        if is_cst s b then set_def s p a b ;
    | _ -> ()

  let collect s = function
    | Have p | When p | Core p | Init p -> assume s (F.e_prop p)
    | Type _ | Branch _ | Either _ | State _ -> ()

  let rec e_apply s e =
    try Tmap.find e s.mem
    with Not_found ->
      let e' = F.QED.lc_map (e_apply s) e in
      s.mem <- Tmap.add e e' s.mem ; e'

  let p_apply s p = F.p_bool (e_apply s (F.e_prop p))

  let rec c_apply s = function
    | State m -> State (Mstate.apply (e_apply s) m)
    | Type p -> Type (p_apply s p)
    | Init p -> Init (p_apply s p)
    | Have p -> Have (p_apply s p)
    | When p -> When (p_apply s p)
    | Core p -> Core (p_apply s p)
    | Branch(p,sa,sb) -> Branch( p_apply s p , seq_apply s sa , seq_apply s sb )
    | Either cs -> Either (List.map (seq_apply s) cs)

  and s_apply s (step : step) : step =
    update_cond step (c_apply s step.condition)

  and seq_apply s seq =
    sequence (List.map (s_apply s) seq.seq_list)

  let simplify (hs,p) =
    let s = {
      cst = Tmap.empty ;
      mem = Tmap.empty ;
      def = Tmap.empty ;
      dom = Vars.empty ;
    } in
    try
      List.iter (fun h -> collect s h.condition) hs ;
      s.mem <- s.def ;
      let hs = List.map (s_apply s) hs in
      let p = p_apply s p in
      hs , p
    with Contradiction ->
      [] , F.p_true

end

(* -------------------------------------------------------------------------- *)
(* --- Letify-Fixpoint                                                    --- *)
(* -------------------------------------------------------------------------- *)

let rec fixpoint limit solvers sigma s0 =
  !Db.progress ();
  let s1 =
    if Wp_parameters.Ground.get () then ground s0
    else s0 in
  let hs,p = ConstantFolder.simplify s1 in
  let target = F.varsp p in
  let export = Vars.empty in
  let modified , sigma1 , sigma2 , hs =
    letify_seq sigma ~target ~export hs in
  let p = Sigma.p_apply sigma2 p in
  let s2 = ground (hs , p) in
  if is_trivial_hsp s2 then [],p_true
  else
  if modified || (limit > 0 && not (equal_list (fst s0) (fst s2)))
  then fixpoint (pred limit) solvers sigma1 s2
  else
    match simplify solvers s2 with
    | Simplified s3 -> fixpoint (pred limit) solvers sigma1 s3
    | Trivial -> [],p_true
    | NoSimplification -> s2

let letify_hsp ?(solvers=[]) hsp = fixpoint 10 solvers Sigma.empty hsp

let rec letify ?(solvers=[]) ?(intros=10) (seq,p) =
  let hs,p = fixpoint 10 solvers Sigma.empty (seq.seq_list,p) in
  let sequent = sequence hs , p in
  let introduced = introduction sequent in
  if sequent != introduced && intros > 0 then
    letify ~solvers ~intros:(pred intros) introduced
  else
    introduced

(* -------------------------------------------------------------------------- *)
(* --- Pruning                                                            --- *)
(* -------------------------------------------------------------------------- *)

let residual p = {
  id = noid ;
  size = 1 ;
  vars = F.varsp p ;
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

let test_case p (s:hsp) =
  let w = letify_hsp (add_case p (fst s) , snd s) in
  if is_trivial_hsp w then None else Some w

let tc = ref 0

let rec test_cases (s : hsp) = function
  | [] -> s
  | (p,_) :: tail ->
      !Db.progress () ;
      match test_case p s , test_case (p_not p) s with
      | None , None -> incr tc ; [],F.p_true
      | Some w , None -> incr tc ; test_cases w tail
      | None , Some w -> incr tc ; test_cases w tail
      | Some _ , Some _ -> test_cases s tail

let rec collect_cond m = function
  | When _ | Have _ | Type _ | Init _ | Core _ | State _ -> ()
  | Branch(p,a,b) -> Letify.Split.add m p ; collect_seq m a ; collect_seq m b
  | Either cs -> List.iter (collect_seq m) cs

and collect_seq m seq = collect_steps m seq.seq_list
and collect_steps m steps = List.iter (fun s -> collect_cond m s.condition) steps

let pruning ?(solvers=[]) seq =
  if is_trivial seq then seq
  else
    begin
      let hs = (fst seq).seq_list in
      let p = snd seq in
      ignore solvers ;
      let m = Letify.Split.create () in
      collect_steps m hs ;
      tc := 0 ;
      let hsp = test_cases (hs,p) (Letify.Split.select m) in
      if !tc > 0 && Wp_parameters.has_dkey dkey_pruning then
        if is_trivial_hsp hsp then
          Wp_parameters.feedback "[Pruning] Trivial"
        else
          Wp_parameters.feedback "[Pruning] %d branche(s) removed" !tc ;
      let hs,p = hsp in
      sequence hs , p
    end

(* -------------------------------------------------------------------------- *)
(* --- Cleaning                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec collect_cond u = function
  | State _ -> ()
  | When p -> Cleaning.as_have u p
  | Have p -> Cleaning.as_have u p
  | Core p -> Cleaning.as_have u p
  | Type p -> Cleaning.as_type u p
  | Init p -> Cleaning.as_init u p
  | Branch(p,a,b) -> Cleaning.as_atom u p ; collect_seq u a ; collect_seq u b
  | Either cs -> List.iter (collect_seq u) cs
and collect_seq u seq = collect_steps u seq.seq_list
and collect_steps u steps =
  List.iter (fun s -> collect_cond u s.condition) steps

let rec clean_cond u = function
  | State _ as cond -> cond
  | When p -> When (Cleaning.filter_pred u p)
  | Have p -> Have (Cleaning.filter_pred u p)
  | Core p -> Core (Cleaning.filter_pred u p)
  | Type p -> Type (Cleaning.filter_pred u p)
  | Init p -> Init (Cleaning.filter_pred u p)
  | Branch(p,a,b) -> Branch(p,clean_seq u a,clean_seq u b)
  | Either cases -> Either(List.map (clean_seq u) cases)

and clean_seq u s =
  let s = clean_steps u s.seq_list in
  { seq_size = size_list s ;
    seq_vars = vars_list s ;
    seq_core = Pset.empty ;
    seq_catg = catg_list s ;
    seq_list = s }

and clean_steps u = function
  | [] -> []
  | s :: seq ->
      let c = clean_cond u s.condition in
      let seq = clean_steps u seq in
      match catg_cond c with
      | EMPTY -> seq
      | FALSE -> [update_cond s c]
      | TRUE | MAYBE -> update_cond s c :: seq

let clean (s,p) =
  let u = Cleaning.create () in
  Cleaning.as_atom u p ; collect_steps u s.seq_list ;
  sequence (clean_steps u s.seq_list) , p

(* -------------------------------------------------------------------------- *)
(* --- Filter Used Variables                                              --- *)
(* -------------------------------------------------------------------------- *)

module Filter =
struct

  module Gmap = Qed.Mergemap.Make(Fun)
  module Gset = Qed.Mergeset.Make(Fun)
  module Fset = Qed.Mergeset.Make(Field)

  module FP =
  struct
    type t = Gset.t * Fset.t
    let empty = Gset.empty , Fset.empty
    let union (a,u) (b,v) = Gset.union a b , Fset.union u v
    let subset (a,u) (b,v) = Gset.subset a b && Fset.subset u v
    let intersect (a,u) (b,v) = Gset.intersect a b || Fset.intersect u v
  end

  type used = {
    mutable fixpoint : bool ;
    mutable footprint : FP.t Tmap.t ; (* memoized by terms *)
    mutable footcalls : Fset.t Gmap.t ; (* memorized by function *)
    mutable gs : FP.t ; (* used in sequent *)
    mutable xs : Vars.t ;  (* used in sequent *)
  }

  [@@@ warning "-32"]
  let pp_gset fmt (u,v) =
    begin
      Format.fprintf fmt "@[<hov 2>{" ;
      Gset.iter (fun f -> Format.fprintf fmt "@ %a" Lang.Fun.pretty f) u ;
      Format.fprintf fmt "," ;
      Fset.iter (fun f -> Format.fprintf fmt "@ %a" Lang.Field.pretty f) v ;
      Format.fprintf fmt " }@]" ;
    end
  let pp_used fmt used =
    begin
      Format.fprintf fmt "@[<hov 2>{" ;
      Vars.iter (fun x -> Format.fprintf fmt "@ %a" Lang.F.Var.pretty x) used.xs ;
      Format.fprintf fmt "," ;
      Gset.iter (fun f -> Format.fprintf fmt "@ %a" Lang.Fun.pretty f) (fst used.gs) ;
      Format.fprintf fmt "," ;
      Fset.iter (fun f -> Format.fprintf fmt "@ %a" Lang.Field.pretty f) (snd used.gs) ;
      Format.fprintf fmt " }@]" ;
    end
  [@@@ warning "+32"]

  let fsetmap phi es =
    List.fold_left
      (fun fs e -> Fset.union fs (phi e))
      Fset.empty es

  let rec gvars_of_term m t =
    try Tmap.find t m.footprint
    with Not_found ->
    match F.repr t with
    | Fun(f,[]) -> Gset.singleton f , Fset.empty
    | Fun(f,_) -> Gset.empty , fset_of_lfun m f
    | Rget(_,fd) -> Gset.empty , Fset.singleton fd
    | Rdef fts -> Gset.empty ,
                  List.fold_left (fun fs (f,_) -> Fset.add f fs)
                    Fset.empty fts
    | _ ->
        let gs = ref FP.empty in
        let collect m gs e = gs := FP.union !gs (gvars_of_term m e) in
        F.lc_iter (collect m gs) t ;
        let s = !gs in
        m.footprint <- Tmap.add t s m.footprint ; s

  and gvars_of_pred m p = gvars_of_term m (F.e_prop p)

  and fset_of_tau (t : Lang.tau) =
    match t with
    | Qed.Logic.Array(ta,tb) ->
        Fset.union (fset_of_tau ta) (fset_of_tau tb)
    | Qed.Logic.Record fts ->
        fsetmap (fun (f,t) -> Fset.add f (fset_of_tau t)) fts
    | Qed.Logic.Data(adt,ts) ->
        Fset.union (fsetmap fset_of_tau ts) (fset_of_adt adt)
    | _ -> Fset.empty

  and fset_of_adt adt =
    fsetmap fset_of_field (Lang.fields_of_adt adt)

  and fset_of_field fd =
    let tf = Lang.tau_of_field fd in
    Fset.add fd (fset_of_tau tf)

  and fset_of_lemma m d =
    snd (gvars_of_pred m d.Definitions.l_lemma)

  and fset_of_var x = fset_of_tau (F.tau_of_var x)

  and fset_of_lfun m f =
    try Gmap.find f m.footcalls
    with Not_found ->
      (* bootstrap recursive calls *)
      m.footcalls <- Gmap.add f Fset.empty m.footcalls ;
      let fs =
        try
          let open Definitions in
          let d = Definitions.find_symbol f in
          let ds = fsetmap fset_of_var d.d_params in
          let df =
            match d.d_definition with
            | Logic _ -> Fset.empty
            | Function(_,_,t) -> snd (gvars_of_term m t)
            | Predicate(_,p) -> snd (gvars_of_pred m p)
            | Inductive ds -> fsetmap (fset_of_lemma m) ds
          in Fset.union ds df
        with Not_found ->
          Fset.empty
      in m.footcalls <- Gmap.add f fs m.footcalls ; fs

  let collect_have m p =
    begin
      m.gs <- FP.union m.gs (gvars_of_pred m p) ;
      m.xs <- Vars.union m.xs (F.varsp p) ;
    end

  let rec collect_condition m = function
    | Have p | When p | Core p -> collect_have m p
    | Type _ | Init _ | State _ -> ()
    | Branch(p,sa,sb) -> collect_have m p ; collect_seq m sa ; collect_seq m sb
    | Either cs -> List.iter (collect_seq m) cs

  and collect_step m s = collect_condition m s.condition
  and collect_seq m s = List.iter (collect_step m) s.seq_list

  let rec filter_pred m p =
    match F.p_expr p with
    | And ps -> F.p_all (filter_pred m) ps
    | _ ->
        if Vars.subset (F.varsp p) m.xs then
          begin
            let gs = gvars_of_pred m p in
            if FP.subset gs m.gs then p else
            if FP.intersect gs m.gs then
              (m.fixpoint <- false ; m.gs <- FP.union gs m.gs ; p)
            else p_true
          end
        else p_true

  let rec filter_steplist m = function
    | [] -> []
    | s :: w ->
        match s.condition with
        | State _ | Have _ | When _ | Core _ | Branch _ | Either _ ->
            s :: filter_steplist m w
        | Type p ->
            let p = filter_pred m p in
            let w = filter_steplist m w in
            if p != F.p_true then
              let s = update_cond s (Type p) in
              s :: w
            else w
        | Init p ->
            let p = filter_pred m p in
            let w = filter_steplist m w in
            if p != F.p_true then
              let s = update_cond s (Init p) in
              s :: w
            else w

  let make (seq,g) =
    let m = {
      gs = FP.empty ;
      xs = Vars.empty ;
      fixpoint = false ;
      footprint = Tmap.empty ;
      footcalls = Gmap.empty ;
    } in
    List.iter (collect_step m) seq.seq_list ; collect_have m g ;
    let rec loop () =
      m.fixpoint <- true ;
      let hs' = filter_steplist m seq.seq_list in
      if m.fixpoint then ( sequence hs' , g ) else loop ()
    in loop ()

end

let filter = Filter.make

(* -------------------------------------------------------------------------- *)
(* --- Filter Parasite Definitions                                        --- *)
(* -------------------------------------------------------------------------- *)

module Parasite =
struct

  open Qed.Logic

  type usage = Used | Def of F.term
  type domain = usage Vmap.t

  [@@@ warning "-32"]
  let pretty fmt w =
    Format.fprintf fmt "@[<hov 2>{" ;
    Vmap.iter
      (fun x u -> match u with
         | Used -> Format.fprintf fmt "@ %a" F.pp_var x
         | Def e -> Format.fprintf fmt "@ @[<hov 2>%a:=%a;@]" F.pp_var x F.pp_term e
      ) w ;
    Format.fprintf fmt " }@]"
  [@@@ warning "+32"]

  let cyclic w x e =
    let m = ref Vars.empty in
    let once x = if Vars.mem x !m then false else (m := Vars.add x !m ; true) in
    let rec walk_y w x y =
      if F.Var.equal x y then raise Exit ;
      if once x then
        let r = try Vmap.find x w with Not_found -> Used in
        match r with Used -> () | Def e -> walk_e w x e
    and walk_e w x e = Vars.iter (walk_y w x) (F.vars e) in
    try walk_e w x e ; false with Exit -> true
      
(*
  let pivots w a b =
    let rec collect xs e = 
      match F.repr e with
      | Fvar x -> x :: xs
      | Add es -> List.fold_left collect xs es
      | _ -> xs in
    let define w a b =
      let xs = collect [] a in
      let def r x = x , F.e_sub r (F.e_var x) in
      let filter w (x,e) = acyclic w x e in
      if xs = [] then [] else
        List.filter (filter w)
          (List.map (def (F.e_sub b a)) xs) in
    define w a b @ define w b a
*)

  let rec add_used (w : domain) xs = Vars.fold add_usedvar xs w
  and add_usedvar x w =
    try match Vmap.find x w with
      | Used -> w
      | Def e -> add_used (Vmap.add x Used w) (F.vars e)
    with Not_found -> Vmap.add x Used w

  let add_def (w : domain) x e =
    try
      let xs = F.vars e in
      if cyclic w x e then add_used (add_usedvar x w) xs
      else
        match Vmap.find x w with
        | Used -> add_used w xs
        | Def e0 -> if F.equal e0 e then w else add_used (Vmap.add x Used w) xs
    with Not_found -> Vmap.add x (Def e) w

  let kind x w =
    try Some (Vmap.find x w)
    with Not_found -> None
  
  let add_eq (w : domain) x y =
    match kind x w , kind y w with
    | None , None ->
        let cmp = F.Var.compare x y in
        if cmp > 0 then add_def w x (F.e_var y) else
        if cmp < 0 then add_def w y (F.e_var x) else
          w
    | None , Some Used -> add_def w x (F.e_var y)
    | Some Used , None -> add_def w y (F.e_var x)
    | Some(Def e),(None | Some Used)
    | (None|Some Used),Some (Def e)
      -> add_usedvar x (add_usedvar y (add_used w (F.vars e)))
    | Some Used,Some Used -> w
    | Some(Def a),Some(Def b) ->
        let xs = Vars.union (F.vars a) (F.vars b) in
         add_usedvar x (add_usedvar y (add_used w xs))
  
  let branch p wa wb =
    let pool = ref (F.varsp p) in
    let w0 = Vmap.union
        (fun _x u v ->
           match u,v with
           | Used,Used -> Used
           | Def a,Def b -> Def( F.e_if (F.e_prop p) a b )
           | Def e,Used | Used,Def e ->
               pool := Vars.union !pool (F.vars e) ; Used
        ) wa wb in
    add_used w0 !pool
  
  let rec usage w p =
    match F.repr p with
    | And ps -> List.fold_left usage w ps
    | Eq(a,b) ->
        begin match F.repr a , F.repr b with
          | Fvar x , Fvar y -> add_eq w x y
          | Fvar x , _ -> add_def w x b
          | _ , Fvar y -> add_def w y a
          | _ -> add_used w (F.vars p)
        end
    | _ -> add_used w (F.vars p)

  let rec collect_step w s =
    match s.condition with
    | Type _ | State _ -> w
    | Have p | Core p | Init p | When p ->
        usage w (F.e_prop p)
    | Branch(p,a,b) ->
        let wa = collect_seq w a in
        let wb = collect_seq w b in
        branch p wa wb
    | Either ws ->
        List.fold_left collect_seq w ws

  and collect_seq w s = List.fold_left collect_step w s.seq_list

  let parasites w =
    Vmap.fold
      (fun x u xs -> match u with Used -> xs | Def _ -> Vars.add x xs)
      w Vars.empty
  
  let rec filter xs p =
    match F.p_expr p with
    | And ps -> p_all (filter xs) ps
    | _ -> if Vars.intersect (F.varsp p) xs then F.p_true else p

  let filter (hs,g) =
    let w = collect_seq (add_used Vmap.empty (F.varsp g)) hs in
    let xs = parasites w in
    if Vars.is_empty xs then (hs,g)
    else map_sequence (filter xs) hs , g

end

let parasite = Parasite.filter

(* -------------------------------------------------------------------------- *)
(* --- Finalization                                                       --- *)
(* -------------------------------------------------------------------------- *)

let close_cond = function
  | Type _ when Wp_parameters.SimplifyType.get () -> p_true
  | c -> pred_cond c

let closure = ref []
let at_closure f = closure := f::!closure
let alter_closure sequent = List.fold_left (fun seq f -> f seq) sequent !closure

let hyps s = List.map (fun s -> close_cond s.condition) s.seq_list
let head s =
  match s.condition with
  | Have p | When p | Core p | Init p | Type p
  | Branch(p,_,_) -> p
  | Either _ | State _ -> p_true
let have s =
  match s.condition with
  | Have p | When p | Core p | Init p | Type p -> p
  | Branch _ | Either _ | State _ -> p_true

let condition s = F.p_conj (hyps s)
let close sequent =
  let s,goal = alter_closure sequent in
  F.p_close (F.p_hyps (hyps s) goal)

(* -------------------------------------------------------------------------- *)
(* --- Visitor                                                            --- *)
(* -------------------------------------------------------------------------- *)

let list seq = seq.seq_list
let iter f seq = List.iter f seq.seq_list

(* -------------------------------------------------------------------------- *)
(* --- Index                                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec index_list k = function
  | [] -> k
  | s::w -> index_list (index_step k s) w

and index_step k s =
  s.id <- k ; let k = succ k in
  match s.condition with
  | Have _ | When _ | Type _ | Core _ | Init _ | State _ -> k
  | Branch(_,a,b) -> index_list (index_list k a.seq_list) b.seq_list
  | Either cs -> index_case k cs

and index_case k = function
  | [] -> k
  | c::cs -> index_case (index_list k c.seq_list) cs

let steps seq = index_list 0 seq.seq_list

let index (seq,_) = ignore (steps seq)

(* -------------------------------------------------------------------------- *)
(* --- Access                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec at_list k = function
  | [] -> assert false
  | s::w ->
      if k = 0 then s else
        let n = s.size in
        if k < n then at_step (k-1) s.condition else at_list (k - n) w

and at_step k = function
  | Have _ | When _ | Type _ | Core _ | Init _ | State _ -> assert false
  | Branch(_,a,b) ->
      let n = a.seq_size in
      if k < n then
        at_list k a.seq_list
      else
        at_list (k-n) b.seq_list
  | Either cs -> at_case k cs

and at_case k = function
  | [] -> assert false
  | c::cs ->
      let n = c.seq_size in
      if k < n then at_list k c.seq_list else at_case (k - n) cs

let step_at seq k =
  if 0 <= k && k < seq.seq_size
  then at_list k seq.seq_list
  else raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Insertion                                                          --- *)
(* -------------------------------------------------------------------------- *)

let in_sequence ~replace =
  let rec in_list k h w =
    if k = 0 then 
      h :: (if replace 
            then match w with
              | [] -> assert false
              | _::w -> w
            else w) 
    else
      match w with
      | [] -> assert false
      | s::w ->
          let n = s.size in
          if k < n then
            let cond = in_step (k-1) h s.condition in
            update_cond s cond :: w
          else s :: in_list (k-n) h w

  and in_step k h = function
    | Have _ | When _ | Type _ | Core _ | Init _ | State _ -> assert false
    | Branch(p,a,b) ->
        let n = a.seq_size in
        if k < n then
          Branch(p,in_sequence k h a,b)
        else
          Branch(p,a,in_sequence (k-n) h b)
    | Either cs -> Either (in_case k h cs)

  and in_case k h = function
    | [] -> assert false
    | c::cs ->
        let n = c.seq_size in
        if k < n
        then in_sequence k h c :: cs
        else c :: in_case (k-n) h cs

  and in_sequence k h s = sequence (in_list k h s.seq_list)
  in in_sequence

let size seq = seq.seq_size

let insert ?at step sequent =
  let seq,goal = sequent in
  let at = match at with None -> seq.seq_size | Some k -> k in
  if 0 <= at && at <= seq.seq_size
  then in_sequence ~replace:false at step seq , goal
  else raise (Invalid_argument "Conditions.insert")

let replace ~at step sequent =
  let seq,goal = sequent in
  if 0 <= at && at <= seq.seq_size
  then in_sequence ~replace:true at step seq , goal
  else raise (Invalid_argument "Conditions.insert")

(* -------------------------------------------------------------------------- *)
(* --- Replace                                                            --- *)
(* -------------------------------------------------------------------------- *)

let subst f s =
  let sigma = F.sigma () in
  map_sequent (F.p_subst ~sigma f) s

(* -------------------------------------------------------------------------- *)
