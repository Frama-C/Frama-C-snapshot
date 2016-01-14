(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
  vars : Vars.t ;
  stmt : stmt option ;
  descr : string option ;
  deps : Property.t list ;
  warn : Warning.Set.t ;
  condition : condition ;
}
and sequence = {
  seq_vars : Vars.t ;
  seq_core : Pset.t ;
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

(* -------------------------------------------------------------------------- *)
(* --- Variable Utilities                                                 --- *)
(* -------------------------------------------------------------------------- *)

let vars_seqs w = List.fold_left (fun xs s -> Vars.union xs s.seq_vars) Vars.empty w
let vars_list s = List.fold_left (fun xs s -> Vars.union xs s.vars) Vars.empty s
let vars_cond = function
  | Type q | When q | Have q | Core q | Init q -> F.varsp q
  | Branch(p,sa,sb) -> Vars.union (F.varsp p) (Vars.union sa.seq_vars sb.seq_vars)
  | Either cases -> vars_seqs cases

let vars_sequent (hs,g) = Vars.union (F.varsp g) (vars_list hs)

(* -------------------------------------------------------------------------- *)
(* --- Core Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let rec is_def a = F.is_simple a || match F.repr a with
  | Qed.Logic.Add xs -> List.for_all F.is_simple xs
  | _ -> false

let is_core p = match F.epred p with
  (*  | Qed.Logic.Eq (a,b) -> is_def a && is_def b *)
  | Qed.Logic.Eq _ -> true
  | _ -> false

let rec add_core s p = match F.pred p with
  | Qed.Logic.And ps -> List.fold_left add_core Pset.empty ps
  | _ -> if is_core p then Pset.add p s else s

let core_cond = function
  | Type _ -> Pset.empty
  | Have p | When p | Core p | Init p -> add_core Pset.empty p
  | Branch(_,sa,sb) -> Pset.inter sa.seq_core sb.seq_core
  | Either [] -> Pset.empty
  | Either (c::cs) ->
      List.fold_left (fun w s -> Pset.inter w s.seq_core) c.seq_core cs

let add_core_step ps s = Pset.union ps (core_cond s.condition)

let core_list s = List.fold_left add_core_step Pset.empty s

module Core =
struct

  let rec fpred core p = match F.pred p with
    | Qed.Logic.And ps -> F.p_conj (List.map (fpred core) ps)
    | _ -> if Pset.mem p core then p_true else p

  let fcond core = function
    | Core p -> Core (fpred core p)
    | Have p -> Have (fpred core p)
    | When p -> When (fpred core p)
    | Init p -> Init (fpred core p)
    | (Type _ | Branch _ | Either _) as cond -> cond

  let fstep core step =
    let condition = fcond core step.condition in
    let vars = vars_cond condition in
    { step with condition ; vars }

  let seq l = {
    seq_vars = vars_list l ;
    seq_core = core_list l ;
    seq_list = l ;
  }

  let factorize a b =
    if Wp_parameters.Core.get () then
      let core = Pset.inter a.seq_core b.seq_core in
      if Pset.is_empty core then None else
        let ca = List.map (fstep core) a.seq_list in
        let cb = List.map (fstep core) b.seq_list in
        Some (F.p_conj (Pset.elements core) , seq ca , seq cb)
    else None

end

(* -------------------------------------------------------------------------- *)
(* --- Trivial Conditions                                                 --- *)
(* -------------------------------------------------------------------------- *)

let is_cond_true = function
  | Have p | Type p | When p | Core p | Init p -> F.is_ptrue p
  | Either [] -> No
  | Either [{ seq_list=[] }] -> Yes
  | Branch(_,{ seq_list=[] },{ seq_list=[] }) -> Yes
  | Branch _ | Either _ -> Maybe

let rec is_seq_true = function
  | [] -> Yes
  | s::seq ->
      begin
        match is_cond_true s.condition with
        | No -> No
        | Maybe -> Maybe
        | Yes -> is_seq_true seq
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
  val is_true : t -> Qed.Logic.maybe
  val add : step -> t -> t
  val factorize : t -> t -> t * t * t
  val big_inter : t list -> t
  val diff : t -> t -> t
  val freeze: t -> sequence
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

  let build seq =
    let xs = List.fold_left
        (fun xs (_,s) -> Vars.union xs s.vars) Vars.empty seq in
    xs , seq

  let factorize (_,a) (_,b) =
    let l,m,r = SEQ.factorize a b in
    build l , build m , build r

  let big_inter cs = build (SEQ.big_inter (List.map snd cs))
  let diff (_,a) (_,b) = build (SEQ.diff a b)
  let freeze (xs,bundle) =
    { seq_vars = xs ;
      seq_core = Pset.empty ;
      seq_list = List.map snd bundle }
  let map f b = List.map (fun (_,s) -> f s.condition) (snd b)
  let exists f b = List.exists (fun (_,s) -> f s.condition) (snd b)
  let is_true = function
    | (_,[]) -> Yes
    | (_,(_,s) :: _) -> is_cond_true s.condition
end

(* -------------------------------------------------------------------------- *)
(* --- Hypotheses                                                         --- *)
(* -------------------------------------------------------------------------- *)

type bundle = Bundle.t
type hypotheses = step list
type sequent = hypotheses * F.pred

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
  Format.fprintf fmt "%s:%d: " (Filepath.pretty file) line

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
       | When p | Type p | Have p | Init p | Core p | Branch(p,_,_) -> F.mark_p m p
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
  | Init p -> pp_clause fmt env "Init" p
  | Type p -> pp_clause fmt env "Type" p
  | Have p -> pp_clause fmt env "Have" p
  | When p -> pp_clause fmt env "When" p
  | Core p -> pp_clause fmt env "Core" p
  | Branch(p,{seq_list=a},{seq_list=b}) ->
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
             pp_block fmt env seq.seq_list ;
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

and pp_block fmt env (seq : step list) =
  let m = F.marker env in
  mark_seq m seq ;
  let env = F.define
      (fun env x t ->
         Format.fprintf fmt "@ @[<hov 4>@{<bf>Let@} %s = %a.@]"
           x (F.pp_eterm env) t)
      env m in
  List.iter (pp_step fmt env) seq

let dump fmt (b:bundle) =
  let s = Bundle.freeze b in
  pp_sequence fmt "Assume" (F.env s.seq_vars) s.seq_list

let pp_seq title fmt s = pp_sequence fmt title (F.env (vars_list s)) s

(* -------------------------------------------------------------------------- *)
(* --- Extraction                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec pred_cond = function
  | When p | Type p | Have p | Core p | Init p -> p
  | Branch(p,a,b) -> F.p_if p (pred_seq a) (pred_seq b)
  | Either cases -> F.p_any pred_seq cases
and pred_seq seq = F.p_all (fun s -> pred_cond s.condition) seq.seq_list
let extract bundle = Bundle.map pred_cond bundle
let hypotheses bundle = (Bundle.freeze bundle).seq_list

let intersect p bundle = Vars.intersect (F.varsp p) (Bundle.vars bundle)
let occurs x bundle = Vars.mem x (Bundle.vars bundle)

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printer                                                     --- *)
(* -------------------------------------------------------------------------- *)

let pretty ?linker fmt s =
  try
    glinker := linker ;
    let env = F.env (vars_sequent s) in
    let m = F.marker env in
    let (hyps,goal) = s in
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
    vars = vars_cond cond ;
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
(* --- Existential Introduction                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec introduce p =
  let open Qed.Logic in
  match F.pred p with
  | And ps -> F.p_all introduce ps
  | Bind(Exists,tau,p) ->
      let x = Lang.freshvar tau in
      introduce (F.p_bool (F.lc_open x p))
  | _ -> p

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
    Bundle.add (step ~descr:"Domain" (Type (p_conj ps))) hs


let intros ps hs =
  if ps = [] then hs else
    let p = F.p_all introduce ps in
    Bundle.add (step ~descr:"Goal" (When p)) hs

let assume ?descr ?stmt ?deps ?warn ?(init=false) p hs =
  match F.is_ptrue p with
  | Yes -> hs
  | No ->
      let cond = if init then Init p else Have p in
      let s = step ?descr ?stmt ?deps ?warn cond in
      Bundle.add s Bundle.empty
  | Maybe ->
      begin
        match Bundle.is_true hs with
        | Yes | Maybe ->
            let p = introduce p in
            let cond = if init then Init p else Have p in
            let s = step ?descr ?stmt ?deps ?warn cond in
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

let core_residual step core = {
  vars = F.varsp core ;
  condition = Core core ;
  descr = None ;
  warn = Warning.Set.empty ;
  deps = [] ;
  stmt = step.stmt ;
}

let core_branch step p a b =
  let condition =
    if is_seq_true a.seq_list = Yes && is_seq_true b.seq_list = Yes
    then Have p_true else Branch(p,a,b) in
  let vars = vars_cond condition in
  { step with condition ; vars }

let rec flatten_sequence m = function
  | [] -> []
  | step :: seq ->
      match step.condition with
      | Have p | Type p | When p | Core p | Init p ->
          begin
            match F.is_ptrue p with
            | Yes -> m := true ; flatten_sequence m seq
            | No -> if seq <> [] then m := true ; [step]
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
                match is_seq_true sa , is_seq_true sb with
                | Yes , Yes -> m := true ; flatten_sequence m seq
                | _ , No ->
                    m := true ;
                    let vars = F.varsp p in
                    let step = { step with vars ; condition = Have p } in
                    step :: sa @ flatten_sequence m seq
                | No , _ ->
                    m := true ;
                    let vars = F.varsp p in
                    let step = { step with vars ; condition = Have (p_not p) } in
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
      | Either [] -> [step]
      | Either cases ->
          let cases = List.map (fun s -> s.seq_list) cases in
          match disjunction is_seq_true cases with
          | TRUE -> m := true ; flatten_sequence m seq
          | FALSE -> m := true ;
              [ { step with vars = Vars.empty ; condition = Have p_false } ]
          | EITHER [hc] ->
              m := true ; flat_concat hc (flatten_sequence m seq)
          | EITHER cs ->
              let cases = List.map
                  (fun s -> { seq_vars = vars_list s ;
                              seq_core = core_list s ;
                              seq_list = s }) cs in
              let vars = vars_seqs cases in
              let step = { step with vars ; condition = Either cases } in
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
    | _ -> step.vars in
  let defs =
    match step.condition with
    | Init p | Have p | When p | Core p -> Defs.extract (Sigma.p_apply sigma p)
    | Type _ | Branch _ | Either _ -> Defs.empty
  in (xs , defs , step)

let letify_assume sref (_,_,step) =
  let current = !sref in
  begin
    match step.condition with
    | Type _ | Branch _ | Either _ -> ()
    | Init p | Have p | When p | Core p ->
        if Wp_parameters.Simpl.get () then
          sref := Sigma.assume current p
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
  let cond = match s.condition with
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
  in { s with vars = vars_cond cond ; condition = cond }

and letify_case sigma ~target ~export seq =
  let (_,_,_,seq) = letify_seq sigma ~target ~export seq.seq_list
  in {
    seq_vars = vars_list seq ;
    seq_core = core_list seq ;
    seq_list = seq ;
  }

(* -------------------------------------------------------------------------- *)
(* --- External Simplifier                                                --- *)
(* -------------------------------------------------------------------------- *)

exception Contradiction

class type simplifier =
  object
    method name : string
    method copy : simplifier
    method simplify_hyp : F.pred -> F.pred
    method assume : F.pred -> unit
    method target : F.pred -> unit
    method fixpoint : unit
    method simplify_branch : F.pred -> F.pred
    method infer : F.pred list
    method simplify_goal : F.pred -> F.pred
  end

let simplify_goal solvers p =
  List.fold_left (fun p s -> s#simplify_goal p) p solvers
let simplify_hyp solvers p =
  List.fold_left (fun p s -> s#simplify_hyp p) p solvers
let simplify_branch solvers p =
  List.fold_left (fun p s -> s#simplify_branch p) p solvers

let update_cond h c = { h with condition = c ; vars = vars_cond c }

let apply_hyp modified solvers h =
  let simple p =
    let p' = simplify_hyp solvers p in
    if not (Lang.F.eqp p p') then modified := true;
    List.iter (fun s -> s#assume p') solvers; p'
  in
  match h.condition with
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
        ( modified := true ;
          let condition = Branch(q,a,b) in
          let vars = vars_cond condition in
          { h with vars ; condition } )
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
  | Simplified of sequent
  | Trivial

let simplify (solvers : simplifier list) sequent =
  if solvers = [] then NoSimplification
  else
    try
      let modified = ref false in
      let solvers = List.map (fun s -> s#copy) solvers in
      let hs,g = sequent in
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
(* --- Constant Folder                                                    --- *)
(* -------------------------------------------------------------------------- *)

module ConstantFolder =
struct

  open Qed

  type state = term Tmap.t

  let modified s1 s2 = not (Tmap.equal F.equal s1 s2)

  type sigma = {
    mutable dom : Vars.t ; (* support of defs *)
    mutable def : term Tmap.t ; (* defs *)
    mutable mem : term Tmap.t ; (* defs+memo *)
  }

  let is_cst e = match F.repr e with
    | True | False | Kint _ | Kreal _ -> true
    | _ -> false

  let set_def s p a e =
    try
      let e0 = Tmap.find a s.def in
      match F.is_true (F.e_eq e e0) with
      | Logic.Yes -> ()
      | Logic.No -> raise Contradiction
      | Logic.Maybe ->
          if F.compare e e0 < 0 then s.def <- Tmap.add a e0 s.def
    with Not_found ->
      begin
        s.dom <- Vars.union (F.vars a) s.dom ;
        s.def <- Tmap.add a e s.def ;
        s.def <- Tmap.add p p s.def ;
      end

  let rec assume s p = match F.repr p with
    | Logic.And ps -> List.iter (assume s) ps
    | Logic.Eq(a,b) ->
        if is_cst a then set_def s p b a ;
        if is_cst b then set_def s p a b ;
    | _ -> ()

  let collect s = function
    | Have p | When p | Core p | Init p -> assume s (F.e_prop p)
    | Type _ | Branch _ | Either _ -> ()

  let rec e_apply s e =
    try Tmap.find e s.mem
    with Not_found ->
      let e' = F.lc_map (e_apply s) e in
      s.mem <- Tmap.add e e' s.mem ; e'

  let p_apply s p = F.p_bool (e_apply s (F.e_prop p))

  let rec c_apply s = function
    | Type p -> Type (p_apply s p)
    | Init p -> Init (p_apply s p)
    | Have p -> Have (p_apply s p)
    | When p -> When (p_apply s p)
    | Core p -> Core (p_apply s p)
    | Branch(p,sa,sb) -> Branch( p_apply s p , seq_apply s sa , seq_apply s sb )
    | Either cs -> Either (List.map (seq_apply s) cs)

  and s_apply s (step : step) : step =
    let condition = c_apply s step.condition in
    let vars = vars_cond condition in
    { step with vars ; condition }

  and seq_apply s seq =
    let seq_list = List.map (s_apply s) seq.seq_list in
    let seq_core = core_list seq_list in
    let seq_vars = vars_list seq_list in
    { seq_list ; seq_core ; seq_vars }

  let simplify (hs,p) =
    let s = {
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

let rec fixpoint n solvers sigma sequent =
  !Db.progress ();
  let hs,p = ConstantFolder.simplify sequent in
  let target = F.varsp p in
  let export = Vars.empty in
  let modified , sigma1 , sigma2 , hs =
    letify_seq sigma ~target ~export hs in
  let p = Sigma.p_apply sigma2 p in
  let s = hs , p in
  if modified
  then fixpoint n solvers sigma1 s
  else
    match simplify solvers s with
    | Simplified s -> fixpoint (succ n) solvers sigma1 s
    | Trivial -> [],p_true
    | NoSimplification -> s

let letify ?(solvers=[]) s = fixpoint 1 solvers Sigma.empty s

(* -------------------------------------------------------------------------- *)
(* --- Pruning                                                            --- *)
(* -------------------------------------------------------------------------- *)

let residual p = {
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

let is_absurd h = match h.condition with
  | (Type p | When p | Have p) -> p == F.p_false
  | _ -> false

let is_trivial (s:sequent) =
  (snd s) == F.p_true || List.exists is_absurd (fst s)

let test_case p (s:sequent) =
  let w = letify (add_case p (fst s) , snd s) in
  if is_trivial w then None else Some w

let tc = ref 0

let rec test_cases (s : sequent) = function
  | [] -> s
  | (p,_) :: tail ->
      !Db.progress () ;
      match test_case p s , test_case (p_not p) s with
      | None , None -> incr tc ; [],F.p_true
      | Some w , None -> incr tc ; test_cases w tail
      | None , Some w -> incr tc ; test_cases w tail
      | Some _ , Some _ -> test_cases s tail

let rec collect_cond m = function
  | When _ | Have _ | Type _ | Init _ | Core _ -> ()
  | Branch(p,a,b) -> Letify.Split.add m p ; collect_seq m a ; collect_seq m b
  | Either cs -> List.iter (collect_seq m) cs

and collect_seq m seq = collect_steps m seq.seq_list
and collect_steps m steps = List.iter (fun s -> collect_cond m s.condition) steps

let pruning ?(solvers=[]) sequent =
  if is_trivial sequent then sequent
  else
    begin
      ignore solvers ;
      let m = Letify.Split.create () in
      collect_steps m (fst sequent) ;
      tc := 0 ;
      let sequent = test_cases sequent (Letify.Split.select m) in
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
  | Core p -> Cleaning.as_have u p
  | Type p -> Cleaning.as_type u p
  | Init p -> Cleaning.as_init u p
  | Branch(p,a,b) -> Cleaning.as_atom u p ; collect_seq u a ; collect_seq u b
  | Either cs -> List.iter (collect_seq u) cs
and collect_seq u seq = collect_steps u seq.seq_list
and collect_steps u steps =
  List.iter (fun s -> collect_cond u s.condition) steps

let rec clean_cond u = function
  | When p -> When (Cleaning.filter_pred u p)
  | Have p -> Have (Cleaning.filter_pred u p)
  | Core p -> Core (Cleaning.filter_pred u p)
  | Type p -> Type (Cleaning.filter_pred u p)
  | Init p -> Init (Cleaning.filter_pred u p)
  | Branch(p,a,b) -> Branch(p,clean_seq u a,clean_seq u b)
  | Either cases -> Either(List.map (clean_seq u) cases)

and clean_seq u s =
  let s = clean_steps u s.seq_list in
  { seq_vars = vars_list s ;
    seq_core = Pset.empty ;
    seq_list = s }

and clean_steps u = function
  | [] -> []
  | s :: seq ->
      let c = clean_cond u s.condition in
      let seq = clean_steps u seq in
      match is_cond_true c with
      | Yes -> seq
      | No -> [{ s with vars = vars_cond c ; condition = c }]
      | Maybe -> { s with vars = vars_cond c ; condition = c } :: seq

let clean (hs,p) =
  let u = Cleaning.create () in
  Cleaning.as_atom u p ; collect_steps u hs ;
  clean_steps u hs , p

(* -------------------------------------------------------------------------- *)
(* --- Filter Used Variables                                              --- *)
(* -------------------------------------------------------------------------- *)

module Filter =
struct

  module Gset = Qed.Mergeset.Make(Fun)

  type used = {
    mutable fixpoint : bool ;
    mutable footprint : Gset.t Tmap.t ; (* memoized by terms *)
    mutable gs : Gset.t ; (* used in sequent *)
    mutable xs : Vars.t ;  (* used in sequent *)
  }

  let rec gvars_of_term m t =
    try Tmap.find t m.footprint
    with Not_found ->
      match F.repr t with
      | Fun(f,[]) -> Gset.singleton f
      | _ ->
          let gs = ref Gset.empty in
          let collect m gs e = gs := Gset.union !gs (gvars_of_term m e) in
          F.lc_iter (collect m gs) t ;
          let s = !gs in
          m.footprint <- Tmap.add t s m.footprint ; s

  let gvars_of_pred m p = gvars_of_term m (F.e_prop p)

  let collect_have m p =
    begin
      m.gs <- Gset.union m.gs (gvars_of_pred m p) ;
      m.xs <- Vars.union m.xs (F.varsp p) ;
    end

  let rec collect_condition m = function
    | Have p | When p | Core p -> collect_have m p
    | Type _ | Init _ -> ()
    | Branch(p,sa,sb) -> collect_have m p ; collect_seq m sa ; collect_seq m sb
    | Either cs -> List.iter (collect_seq m) cs

  and collect_step m s = collect_condition m s.condition
  and collect_seq m s = List.iter (collect_step m) s.seq_list

  let rec filter_pred m p =
    match F.pred p with
    | And ps -> F.p_all (filter_pred m) ps
    | _ ->
        if Vars.subset (F.varsp p) m.xs then
          begin
            let gs = gvars_of_pred m p in
            if Gset.subset gs m.gs then p else
            if Gset.intersect gs m.gs then
              (m.fixpoint <- false ; m.gs <- Gset.union gs m.gs ; p)
            else p_true
          end
        else p_true

  let rec filter_steplist m = function
    | [] -> []
    | s :: w ->
        match s.condition with
        | Have _ | When _ | Core _ | Branch _ | Either _ ->
            s :: filter_steplist m w
        | Type p ->
            let p = filter_pred m p in
            let w = filter_steplist m w in
            if p != F.p_true then
              let s = { s with condition = Type p ; vars = F.varsp p } in
              s :: w
            else w
        | Init p ->
            let p = filter_pred m p in
            let w = filter_steplist m w in
            if p != F.p_true then
              let s = { s with condition = Init p ; vars = F.varsp p } in
              s :: w
            else w

  let make (hs,g) =
    let m = { gs = Gset.empty ; xs = Vars.empty ; fixpoint = false ; footprint = Tmap.empty } in
    List.iter (collect_step m) hs ; collect_have m g ;
    let rec loop m hs g =
      m.fixpoint <- true ;
      let hs' = filter_steplist m hs in
      if m.fixpoint then hs' , g else loop m hs g
    in loop m hs g

end

let filter = Filter.make

(* -------------------------------------------------------------------------- *)
(* --- Finalization                                                       --- *)
(* -------------------------------------------------------------------------- *)

let close_cond = function
  | Type _ when Wp_parameters.SimplifyType.get () -> p_true
  | c -> pred_cond c

let close (hs,goal) =
  let hs = List.map (fun s -> close_cond s.condition) hs in
  F.p_close (F.p_hyps hs goal)

(* -------------------------------------------------------------------------- *)
