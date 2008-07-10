(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: hypotheses_filtering.ml,v 1.57 2008/07/09 15:06:51 giorgetti Exp $ i*)

(**
   This module provides a quick way to filter hypotheses of 
   a goal. It is activated by the option 
   --prune-hyp vb pb 
   where vb an pb are integers that are positive or null. 
   vb is the bound of the max depth of the variable graph search 
   pb is the bound of the max depth of the predicate graph search 
   The number of selected hypotheses increases w.r.t. vb and pb. 
   
   The internal flow is:
   1) Hypotheses are expanded thanks to the intros tactic. 
   A short goal is then produced.
   2) Variables of hypothesis are stored into a graph where 
   - a variable is represented by a vertex
   - a predicate linking some variables is represented by 
   the complete graph with all the vertices 
   corresponding to the variables 
   3) A breadth-first search algorithm, bounded by the constant pb
   computes the set of relevant predicates P
   4) A breadth-first search algorithm, bounded by the constant k
   computes the set of relevant variables V
   5) An hypothesis is selected  by comparing its set of variables 
   with R and its set of predicates with P 
**)


open Ident
open Options
open Misc
open Error
open Logic
open Logic_decl
open Env
open Cc
open Format
open Pp
open Hashtbl
open Set
open Util
open Graph.Graphviz
open Graph.Path

(* Strategies with constants *)
type var_strat =
    AllVars
  | One
  | AllInABranch
  | SplitHyps
  | CNFHyps

let prune_context = Options.prune_context
let use_comparison_as_criteria_for_graph_construction =
  Options.pruning_hyp_CompInGraph
let use_comparison_as_criteria_for_hypothesis_filtering = 
	Options.pruning_hyp_CompInFiltering
let keep_quantification_link_beween_vars = 
	Options.pruning_hyp_LinkVarsQuantif
(* Setup of comparison management *)
let keep_single_comparison_representation = 
	Options.pruning_hyp_keep_single_comparison_representation
let comparison_eqOnly = Options.pruning_hyp_comparison_eqOnly
let suffixed_comparison = Options.pruning_hyp_suffixed_comparison
let equalities_linked = Options.pruning_hyp_equalities_linked
let arith_tactic = Options.pruning_hyp_arithmetic_tactic
let var_filter_tactic = match Options.pruning_hyp_var_tactic with
  | 0 -> AllVars
  | 1 -> One
  | 2 -> AllInABranch
  | 3 -> SplitHyps
  | 4 -> CNFHyps
  | _ -> failwith "Heuristic failed."
let polarized_preds = Options.pruning_hyp_polarized_preds

let pb = ref Options.pruning_hyp_p
let vb = ref Options.pruning_hyp_v
let v_count = ref 0
let hyp_count = ref 0

let set_pb n =
  pb := n

let set_vb n =
  vb := n

let context = ref []

(**
***********************
VarStringSet module and miscenalous tools
***********************
**)

type var_string =
  | PureVar of string (*already present*)
  | FreshVar of string (*introduced by a flatening step*)

module VarStringSet = Set.Make(struct type t = var_string let compare = compare end)

let member_of str st =
  VarStringSet.mem (PureVar str) st || VarStringSet.mem (FreshVar str) st

let distinct_vars v1 v2 =
  match (v1, v2) with
    (PureVar (id1), PureVar (id2)) ->
      id1 <> id2
  | _ -> assert false

let string_of_var v =
  match v with
    PureVar(id) -> id
  | FreshVar(id) -> id

let display_var_set set =
  VarStringSet.iter
    (fun s ->
          Format.printf "%s " (string_of_var s)) set;
  Format.printf "@\n@."

(** returns the free variables of a term that are not outer quantified (in qvars) **)
let free_vars_of qvars t =
  let vars = ref VarStringSet.empty in
  let rec collect formula =
    match formula with
    | Tapp (id, tl, _) ->
        List.iter collect tl
    | Tvar (id) ->
        if not (VarStringSet.mem (PureVar (Ident.string id)) qvars) then
          vars := VarStringSet.add (PureVar (Ident.string id)) !vars
    | _ -> () in
  collect t;
  !vars

(** returns the free variables of a predicate that are not inner quantified **)
let free_vars_of p =
  let vars = ref VarStringSet.empty in
  let rec collect qvars formula =
    match formula with
    | Papp (_, tl, _) ->
        List.iter
          (fun t ->
                let v' = free_vars_of qvars t in
                vars := VarStringSet.union v' !vars) tl
    | Pand (_, _, a, b) | Forallb (_, a, b) | Por (a, b) | Piff (a, b) |
    Pimplies (_, a, b) ->
        collect qvars a;
        collect qvars b
    | Pif (a, b, c) ->
        let v' = free_vars_of qvars a in
        vars := VarStringSet.union v' !vars;
        collect qvars b;
        collect qvars c
    | Pnot a -> collect qvars a;
    | Forall (_, id, _, _, _, p) | Exists (id, _, _, p) ->
        collect (VarStringSet.add (PureVar (Ident.string id)) qvars) p
    | Pfpi _ ->
        failwith "fpi not yet suported "
    | Pnamed (_, p) -> collect qvars p
    | Pvar _ | Pfalse | Ptrue -> ()
  in
  collect VarStringSet.empty p;
  !vars

(** returns the nnf of a formula **)
let rec nnf fm =
  let boolTrue = Tconst (ConstBool true) in
  match fm with
  | Pand (p1, p2, p, q) -> Pand(p1, p2, nnf p, nnf q)
  | Forallb (p1, p, q) -> Pand(p1, false, nnf p, nnf q)
  | Por (p, q) -> Por(nnf p, nnf q)
  | Pimplies (_, p, q)
  -> Por (nnf (Pnot p), nnf q)
  | Piff (p, q) ->
      Pand(false, false, Por(nnf (Pnot p), nnf q), Por(nnf p, nnf(Pnot q)))
  | Pif (a, b, c) ->
      Pand (false, false,
        Por (Papp (t_neq_bool, [boolTrue; a], []),
          nnf b),
        Por (Papp (t_eq_bool, [boolTrue; a], []), nnf c))
  | Forall (t1, t2, p3, p4, p5, p) ->
      Forall(t1, t2, p3, p4, p5, nnf p)
  | Exists (t1, t2, pt, p) ->
      Exists (t1, t2, pt, nnf p)
  | Pnot (Pnot p) -> nnf p
  | Pnot (Pand (_, _, p, q)) -> Por(nnf (Pnot p), nnf(Pnot q))
  | Pnot (Por (p, q)) -> Pand(false, false, nnf (Pnot p), nnf (Pnot q))
  | Pnot (Pimplies (pwp, p, q)) -> Pand(pwp, false, nnf p, nnf (Pnot q))
  | Pnot (Piff (p, q)) -> Por( Pand(false, false, nnf p, nnf(Pnot q)),
        Pand(false, false, nnf (Pnot p), nnf q))
  | Pnot(Forall(_, t1, t2, ty, _, p)) ->
      Exists(t1, t2, ty, (nnf (Pnot p)))
  | Pnot(Forallb (_, p, q)) -> Por(nnf (Pnot p), nnf(Pnot q))
  | Pnot(Exists (t1, t2, pt, p)) ->
      Forall(false, t1, t2, pt,[], (nnf (Pnot p)))
  | Pnot(Pif (a, b, c)) ->
      Por (
        Pand (false, false, Papp (t_eq_bool, [boolTrue; a], []),
          nnf (Pnot b)),
        Pand (false, false, Papp (t_neq_bool, [boolTrue; a], []),
          nnf (Pnot c)))
  | _ -> fm

(** reduces as possible the scope of quantifier
@param pr is the concerned predicate **)
let miniscoping pr =
  let rec mq q fm =
    match q with
    | Forall (t1, t2, p3, p4, p5, _) ->
        begin
          if not (member_of
                (Ident.string t2)
                (free_vars_of fm)) then
            fm
          else
            begin
              match fm with
              | Pand (p1, p2, p, q) ->
                  if not (member_of
                        (Ident.string t2)
                        (free_vars_of p)) then
                    let q' = mq (Forall(t1, t2, p3, p4, p5, q)) q in
                    Pand (p1, p2, p, q')
                  else
                  if not (member_of
                        (Ident.string t2)
                        (free_vars_of q)) then
                    let p' = mq (Forall(t1, t2, p3, p4, p5, p)) p in
                    Pand (p1, p2, p', q)
                  else
                    Pand (p1, p2,
                      mq (Forall (t1, t2, p3, p4, p5, p)) p,
                      mq (Forall (t1, t2, p3, p4, p5, q)) q)
              | Por (p, q) ->
                  if not (member_of
                        (Ident.string t2)
                        (free_vars_of p)) then
                    let q' = mq (Forall(t1, t2, p3, p4, p5, q)) q in
                    Por (p, q')
                  else
                  if not (member_of
                        (Ident.string t2)
                        (free_vars_of q)) then
                    let p' = mq (Forall (t1, t2, p3, p4, p5, p)) p in
                    Por (p', q)
                  else
                    Forall (t1, t2, p3, p4, p5, fm)
              | _ -> Forall (t1, t2, p3, p4, p5, fm)
            end
        end
    | Exists (t1, t2, pt, p) ->
        begin
          if not (member_of
                (Ident.string t1)
                (free_vars_of fm)) then
            fm
          else
            match fm with
            | Pand (p1, p2, p, q) ->
                if not (member_of
                      (Ident.string t1)
                      (free_vars_of p)) then
                  let q' = mq (Exists(t1, t2, pt, q)) q in
                  Pand (p1, p2, p, q')
                else
                if not (member_of
                      (Ident.string t1)
                      (free_vars_of q)) then
                  let p' = mq (Exists(t1, t2, pt, p)) p in
                  Pand (p1, p2, p', q)
                else
                  Exists(t1, t2, pt, fm)
            | Por (p, q) ->
                if not (member_of
                      (Ident.string t1)
                      (free_vars_of p)) then
                  let q' = mq (Exists(t1, t2, pt, q)) q in
                  Por (p, q')
                else
                if not (member_of
                      (Ident.string t1)
                      (free_vars_of q)) then
                  let p' = mq (Exists (t1, t2, pt, p)) p in
                  Por (p', q)
                else
                  Por(
                    mq (Exists (t1, t2, pt, p)) p,
                    mq (Exists (t1, t2, pt, q)) q)
            | _ -> Exists(t1, t2, pt, fm)
        end
    | _ -> assert false
  in
  (** rewrites Forallb into Forall and call mq to reduce scoping of quantifiers **)
  let rec minib fm =
    match fm with
    | Pand (p1, p2, p, q) -> Pand(p1, p2, minib p, minib q)
    | Forallb (p1, p, q) -> Pand(p1, false, minib p, minib q)
    | Por (p, q) -> Por(minib p, minib q)
    | Forall (t1, t2, p3, p4, p5, p) as pi -> mq pi (minib p)
    | Exists (t1, t2, pt, p) as pi -> mq pi (minib p)
    | _ -> fm
  in
  minib (nnf pr)

(** compute the cnf of a predicate **)
let cnf fm =
  let rec cnfp p = match p with
    | Pand (p1, p2, p, q) -> Pand(p1, p2, cnfp p, cnfp q)
    | Por (p1, p2) -> distr (cnfp p1, cnfp p2)
    | Forall (t1, t2, p3, p4, p5, p) -> Forall (t1, t2, p3, p4, p5, cnfp p)
    | Exists (t1, t2, pt, p) -> Exists (t1, t2, pt, cnfp p)
    | _ -> p
  and distr = function
    | (Exists (t1, t2, pt, p), y) -> Exists (t1, t2, pt, distr (p, y))
    | (y, Exists (t1, t2, pt, p)) -> Exists (t1, t2, pt, distr (y, p))
    | (Forall (t1, t2, p3, p4, p5, p), y) -> Forall (t1, t2, p3, p4, p5, distr(p, y))
    | (y, Forall (t1, t2, p3, p4, p5, p)) -> Forall (t1, t2, p3, p4, p5, distr(y, p))
    | (Pand (p1, p2, x2, x3), y) -> Pand (p1, p2, distr (x2, y), distr (x3, y))
    | (x, Pand (p1, p2, y2, y3)) -> Pand (p1, p2, distr (x, y2), distr (x, y3))
    | (x, y) -> Por(x, y)
  
  (* all hypothesis functions are defined without Pnamed case. It is then    *)
  (* removed before selection. It does not have any implication on produced  *)
  (* code.                                                                   *)
  and rm_pnamed par = match par with
    | Pnamed(_, p) -> rm_pnamed p
    | Pfpi (a, b, c) -> Pfpi (a, b, c)
    | Exists (a, b, c, d) -> Exists (a, b, c, (rm_pnamed d))
    | Forallb (a, b, c) -> Forallb (a, (rm_pnamed b), (rm_pnamed c))
    | Forall (a, b, c, d, e, f) -> Forall (a, b, c, d, e, (rm_pnamed f))
    | Pnot a -> rm_pnamed a
    | Piff (a, b) -> Piff (rm_pnamed a, rm_pnamed b)
    | Por (c, d) -> Por (rm_pnamed c, rm_pnamed d)
    | Pand (a, b, c, d) -> Pand (a, b, rm_pnamed c, rm_pnamed d)
    | Pif (a, b, c) -> Pif (a, rm_pnamed b, rm_pnamed c)
    | Pimplies (a, b, c) -> Pimplies (a, rm_pnamed b, rm_pnamed c)
    | Papp (a, b, c) -> Papp (a, b, c)
    | Pvar a -> Pvar a
    | Pfalse -> Pfalse
    | Ptrue -> Ptrue
  in
  cnfp (miniscoping (rm_pnamed fm))

(** avoided vars **)
let avoided_vars = VarStringSet.singleton (PureVar "alloc")

(** a variable name will be associated to each hypothesis **)
let hash_hyp_vars : (predicate,'a) Hashtbl.t = Hashtbl.create 20

let display_str str set =
  Format.printf "%s : " str;
  display_var_set set;
  Format.printf "@\n@."

module SS_set = Set.Make(struct type t = VarStringSet.t let compare = compare end)

(* Provide a fresh variable identifier from an identifier *)
let bound_variable id =
  v_count := !v_count + 1;
  Ident.create ((Ident.string id)^"_"^ (string_of_int !v_count))

let my_fresh_hyp_str () =
  hyp_count := !hyp_count + 1;
  (string_of_int !hyp_count)

let my_fresh_hyp () =
  hyp_count := !hyp_count + 1;
  Ident.create (string_of_int !hyp_count)

let get_flaged_var v ac_fv_set =
  if not (member_of (Ident.string v) ac_fv_set)
  then
    (PureVar (Ident.string v))
  else
    (FreshVar (Ident.string v))

(**
@return vars which is a set of sets of variables
(either pure or fresh) contained in tl
@param qvars is a set of quantified variables
@tl is the list of terms
@param ac : accumulateur des variables fraiches de l'appelant
**)
let vars_of_list qvars tl ac =
  let vars = ref SS_set.empty in
  let rec collect l ac_fv_set =
    let lp = ref l in
    let inner_vars = ref VarStringSet.empty in
    let f t =
      match t with
      | Tapp (id, tl, _) when is_arith_binop id ->
          lp := !lp@tl
      | Tapp (id, tl, _) ->
          let id' = (bound_variable id) in
          let bv = (FreshVar (Ident.string id')) in
          inner_vars := VarStringSet.add bv !inner_vars;
          let l' = Tvar(id'):: tl in
          collect l' (VarStringSet.add bv ac_fv_set)
      | Tvar (id) ->
          if not (member_of (Ident.string id) qvars) then
            inner_vars := VarStringSet.add (get_flaged_var id (VarStringSet.union ac_fv_set ac)) !inner_vars
      (* if not (member_of (Ident.string id) ac_fv_set) then inner_vars :=       *)
      (* VarStringSet.add (PureVar (Ident.string id)) !inner_vars else           *)
      (* inner_vars := VarStringSet.add (FreshVar (Ident.string id)) !inner_vars *)
      | _ -> ()
    in
    List.iter f !lp;
    vars := SS_set.add (VarStringSet.diff !inner_vars avoided_vars) !vars
  in
  collect tl VarStringSet.empty;
  !vars

(**
@return vars which is a set of sets of variables
@param f the formula to be analyzed
**)
let sets_of_vars f =
  
  let rec local_subst_in_term id1 id2 = function
    | Tvar x as t ->
    (* if debug then begin Format.printf "\nTvar found : %s Tvar seeked : %s   *)
    (* to substitute with %s status = " (Ident.string x) (Ident.string id1)    *)
    (* (Ident.string id2); if id1==x then Format.printf "OK\n" else            *)
    (* Format.printf "KO\n" end;                                               *)
        if (Ident.string id1) == (Ident.string x) then (Tvar id2) else t
    | Tderef x as t ->
        if (Ident.string id1) == (Ident.string x) then (Tderef id2) else t
    | Tapp (x, l, i) ->
        Tapp (x, List.map (local_subst_in_term id1 id2) l, i)
    | Tconst _ as t ->
        t
    | Tnamed(lab, t) -> Tnamed(lab, local_subst_in_term id1 id2 t)
  
  in let rec local_subst_in_predicate id1 id2 = function
    | Papp (id, l, i) -> Papp (id, List.map (local_subst_in_term id1 id2) l, i)
    | Pif (a, b, c) -> Pif (local_subst_in_term id1 id2 a,
          local_subst_in_predicate id1 id2 b,
          local_subst_in_predicate id1 id2 c)
    | Pfpi (t, f1, f2) -> Pfpi (local_subst_in_term id1 id2 t, f1, f2)
    | Forall (w, id, b, v, tl, p) ->
        Forall (w, id, b, v, List.map (List.map (
                  (fun id1 id2 -> function
                        | TPat t -> TPat (local_subst_in_term id1 id2 t)
                        | PPat p -> PPat (local_subst_in_predicate id1 id2 p) ) id1 id2)) tl,
          local_subst_in_predicate id1 id2 p)
    | p -> map_predicate (local_subst_in_predicate id1 id2) p
  in
  
  let vars = ref SS_set.empty in
  let ac_fv_set = ref VarStringSet.empty in
  let rec collect qvars formula =
    match formula with
    | Papp (id, [el1; el2], _) when is_eq id ->
        begin
          match (el1, el2) with
          |	(Tvar (v1), Tvar(v2)) ->
              vars := SS_set.add
                ((VarStringSet.add (get_flaged_var v1 !ac_fv_set)) (* (PureVar (Ident.string v1))  *)
                    (VarStringSet.singleton (get_flaged_var v1 !ac_fv_set)))    (* (PureVar (Ident.string v2))) )*)
                !vars
          | (Tvar (v1), Tapp (_, tl, _)) ->
              let l' = Tvar(v1):: tl in
              let v = vars_of_list qvars l' !ac_fv_set in
              (** TODO modifier mettre pure var ? **)
              vars := SS_set.union v !vars
          | (Tapp (_, tl, _), Tvar(v1)) ->
              let l' = Tvar(v1):: tl in
              let v = vars_of_list qvars l' !ac_fv_set in
              vars := SS_set.union v !vars
          | (Tapp (id, tl, _), Tapp (_, tl', _)) ->
              let id' = bound_variable id in
              let tl = Tvar(id'):: tl in
              let tl' = Tvar(id'):: tl' in
              let v = vars_of_list qvars tl !ac_fv_set in
              let v' = vars_of_list qvars tl' !ac_fv_set in
              vars := SS_set.union v !vars;
              vars := SS_set.union v' !vars;
          | (Tapp (_, tl, _), _) ->
              let v = vars_of_list qvars tl !ac_fv_set in
              vars := SS_set.union v !vars;
          | (Tvar(v1), _) ->
              vars := SS_set.add
                (VarStringSet.singleton (get_flaged_var v1 !ac_fv_set) (*(PureVar (Ident.string v1))*) )
                !vars;
          | (_, Tapp (_, tl, _)) ->
              let v = vars_of_list qvars tl !ac_fv_set in
              vars := SS_set.union v !vars
          | (_, Tvar(v1)) ->
              vars := SS_set.add
                (VarStringSet.singleton (get_flaged_var v1 !ac_fv_set) (*(PureVar (Ident.string v1))*) )
                !vars
          | _ -> ()
        end
    | Papp (_, tl, _) ->
        let v = vars_of_list qvars tl !ac_fv_set in
        vars := SS_set.union v !vars
    | Pand (_, _, a, b) | Forallb (_, a, b) | Por (a, b) | Piff (a, b) |
    Pimplies (_, a, b) ->
        collect qvars a;
        collect qvars b
    | Pif (a, b, c) ->
        let l = a::[] in
        let v' = vars_of_list qvars l !ac_fv_set in
        vars := SS_set.union v' !vars;
        collect qvars b;
        collect qvars c
    | Pnot a ->
        collect qvars a;
    | Forall (_, id, _, _, _, p) | Exists (id, _, _, p) ->
        if (keep_quantification_link_beween_vars) then
          begin
            (* The quantified variable id is modified as a fresh one and is    *)
            (* substituted in P, before collecting                             *)
            let id' = (bound_variable id) in
            let bv = (FreshVar (Ident.string id')) in
            (* inner_vars := VarStringSet.add bv !inner_vars; let l' = Tvar(id')::tl   *)
            (* in                                                                      *)
            ac_fv_set:= (VarStringSet.add bv !ac_fv_set);  (* Fresh var. list update *)
            let p'= (local_subst_in_predicate id id' p) in   (*  substitutes id with id' in P *)
            (* let p'= (subst_in_predicate (subst_onev id id') p) in if debug    *)
            (* then begin Format.printf "\nVar : %s subst by %s\n" (Ident.string *)
            (* id) (Ident.string id'); Format.printf "Predicate : %a \n          *)
            (* Substituted in : %a \n\n" Util.print_predicate p                  *)
            (* Util.print_predicate p' end;                                      *)
            collect qvars p'
          end
        else
          collect (VarStringSet.add (PureVar (Ident.string id)) qvars) p
    | Pfpi _ ->
        failwith "fpi not yet suported "
    | Pnamed (_, p) ->
        collect qvars p
    | Pvar _ | Pfalse | Ptrue -> ()
  in
  collect VarStringSet.empty f;
  !vars

(** end of  VarStringSet module  **)

(**
***********************
basic strategy type
***********************
**)

(** Abstract clauses. 

An abstract clause only stores atome numbers,
the set of positive predicates symbols
and the set of negative symbols.
Each predicate is represented as a string.
The set of positive predicates is stored as a StringSet.t
**)
module StringSet = Set.Make(struct type t = string let compare = compare end)
(* A type for Ident pairs *)
type identPair = Pair of Ident.t * Ident.t
(* A type for sets of ident pairs *)
module IdentPairSet = Set.Make(struct type t = identPair let compare = compare end)
(* Type of an abstract clause *)	
type abstractClause = { 
	num : int; 
	pos : StringSet.t; 
	neg : StringSet.t;
	cmp : IdentPairSet.t }
module AbstractClauseSet = Set.Make(
	struct type t = abstractClause let compare = compare end)

let display_cl cl =
  let display_set_pr =
    StringSet.fold
      (fun x1 a -> x1^" "^a);
  in
  Format.printf "[num: %d, pos: {%s}, neg :{%s}]"
    cl.num
    (display_set_pr cl.pos "")
    (display_set_pr cl.neg "")

let display_cl_set set =
  Format.printf "{ ";
  AbstractClauseSet.iter
    (fun c ->
          display_cl c) set;
  Format.printf "}\n"

(**
********************************
Graph of variables
********************************
**)
module Var_node =
struct
  type t = var_string
  let hash = Hashtbl.hash
  let compare n1 n2 = Pervasives.compare n1 n2
  let equal = (=)
end

module Var_graph = Graph.Imperative.Graph.Concrete(Var_node)
let vg = ref (Var_graph.create())

module DisplayVarGraph = struct
  let vertex_name v = string_of_var v
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let edge_attributes _ = []
  include Var_graph
end

module DotVG = Graph.Graphviz.Dot(DisplayVarGraph)

(**
updates the graph_of_variables
**)
let update_v_g vars =
  (** computes the vertex **)
  VarStringSet.iter (fun n -> Var_graph.add_vertex !vg n) vars;
  let rec updateb n v =
    (** adds the edges n<->n2 for all n2 in v **)
    VarStringSet.iter (
        fun n2 -> Var_graph.add_edge !vg n n2) v;
    (** choose another variable and computes the other edges **)
    if not (VarStringSet.is_empty v) then
      let n' = VarStringSet.choose v in
      updateb n' (VarStringSet.remove n' v)
  in
  if not (VarStringSet.is_empty vars) then
    let n' = (VarStringSet.choose vars) in
    updateb
      n'
      (VarStringSet.remove n' vars)

(**
@param l list of variable declaration or predicates (hypotheses)
@param c is the conclusion
Update the hashtables
- symbs which associates to each hypothesis its symbols (as a triple)
and class_of_hyp
- class_of_hyp which associates to each hypothesis its representative
variable
**)
let build_var_graph (l, c) =
  
  (** retrieves the variables of the conclusion **)
  let v = (sets_of_vars c) in
  let _ = SS_set.fold (fun s t ->
        (** update the graph of variables **)
            update_v_g s;
            VarStringSet.union s t) v VarStringSet.empty in
  let rec mem = function
    | [] -> ()
    | Svar (id, v) :: q -> mem q
    | Spred (_, p) :: q ->
        let v = sets_of_vars p in
        (** for each set of variables, build the SCC
        of the set and computes the union of all the variables **)
        let v' =
          SS_set.fold (fun s t ->
                  update_v_g s;
                  VarStringSet.union s t) v VarStringSet.empty in
        (** v' is the union of all the variables **)
        let v' = VarStringSet.diff v' avoided_vars in
        (** associates v' to the hypothesis **)
        Hashtbl.add hash_hyp_vars p v';
        if debug then
          begin
          (* Format.printf " In hyps %a" Util.print_predicate p ; display_str "vars  *)
          (* " v';                                                                   *)
          end;
        mem q
  in
  mem l

(** **)
let removes_fresh_vars vs =
  VarStringSet.fold
    (fun el s ->
          match el with
            PureVar _ as x -> VarStringSet.add x s
          | FreshVar _ -> s)
    vs
    VarStringSet.empty

(** returns the set of  all the successors of a node **)
let succs_of vs =
  VarStringSet.fold
    (fun el l ->
          let succ_set =
            try
              let succ_list = Var_graph.succ !vg el in
              List.fold_right
                (fun el s ->
                      VarStringSet.add el s) succ_list VarStringSet.empty
            with _ -> VarStringSet.empty in
          VarStringSet.union l succ_set
    )
    vs
    VarStringSet.empty

(**
@param v the initial set of variables
@param n the depth of the tree of variables
@param acc acumumulates the variables that have already been visited
@return the ist of variables reachable in n steps

**)
let get_reachable_vars v n =
  let rec get_vars_in_tree_b v n acc =
    let vret =
      if n > 0 then
        let succ_set = succs_of v in
        let v' = VarStringSet.diff succ_set acc in
        VarStringSet.union v
          (get_vars_in_tree_b
              v'
              (n - 1)
              (VarStringSet.union v succ_set)
          )
      else
        v in
    VarStringSet.diff vret avoided_vars in
  VarStringSet.diff
    (get_vars_in_tree_b v n VarStringSet.empty) v
(** End of graph of variables **)

(**
********************************
Graph of predicates
********************************
**)
type positivity = Pos | Neg

let oposite = function
    Pos -> Neg
  | Neg -> Pos

let string_value_of t =
  match t with
    Pos -> ""
  | Neg -> "_C"

type vertexLabel = { l: string; pol: positivity }

module Vrtx =
struct
  type t = vertexLabel
  let hash = Hashtbl.hash
  let compare n1 n2 = Pervasives.compare n1 n2
  let equal = (=)
end




module Edg =
struct
  type t = int
  let compare = Pervasives.compare
  let default = 1
end

module PdlGraph =
  Graph.Imperative.Digraph.ConcreteLabeled(Vrtx)(Edg)
let pdlg = ref (PdlGraph.create())

(**
**************
PdlSet
**************
**)
module PdlSet = Set.Make(struct type t = vertexLabel
    let compare = compare end)
let abstr_mem_of el pdl_set =
  PdlSet.mem { l = el.l; pol = Pos } pdl_set or
  PdlSet.mem { l = el.l; pol = Neg } pdl_set

let is_positive el =
  el.pol == Pos
let is_negative el =
  el.pol == Neg

let mem_of el pdl_set =
  (PdlSet.mem { l = el.l; pol = el.pol } pdl_set )

let mem_of_or_pos el pdl_set =
  (PdlSet.mem { l = el.l; pol = el.pol } pdl_set ) or el.pol == Pos

let mem_of_or_neg el pdl_set =
  (PdlSet.mem { l = el.l; pol = el.pol } pdl_set ) or el.pol == Neg

let abstr_subset_of_pdl set1 set2 =
  if polarized_preds then
    let test_pos = PdlSet.exists(fun el -> is_positive el) set2 in
    let test_neg = PdlSet.exists(fun el -> is_negative el) set2 in
    begin
      if test_pos && test_neg then
        PdlSet.for_all (fun el -> mem_of el set2) set1
      else
        begin
          if test_pos then
            (PdlSet.for_all (fun el -> mem_of_or_neg el set2) set1)
          else
            PdlSet.for_all (fun el -> mem_of_or_pos el set2) set1
        end
    end
  else
    PdlSet.for_all
      (fun el -> abstr_mem_of el set2) set1



let display_symb_of_pdl_set set =
  PdlSet.iter
    (fun el ->
          Format.printf "(%s,%s)" el.l (string_value_of el.pol)) set;
  Format.printf "@\n@."

let vSet = ref PdlSet.empty 


(** end of PdlSet **)


(** Computes arcs from a pair of literals.
    Let l1 and l2 be two literals.
		l1 or l2 is memorized by the edges ~ l1 -> l2 and ~ l2 -> l1 
		with ~ ~ p = p for each predicate p.
		  
		Warning: different from Table 1 from [CGS08] where p is v with
		polarity lp and q is v' with polarity rp.
		 
    @param lp : positivity, left-hand-side polarity.
		@param rp : positivity, right-hand-side polarity.
		@param we : int, Weight to add.
		@param v : string, left-hand-side name.
		@param v' : string, right-hand-side name.
		assigns !pdlg. 
**)
let add_edge lp rp we v v'=
  (* reverse the left polarity since starts from a disjunction *)
  let lp = oposite lp in
	(* Construction of vertices: *)
	(*   type vertexLabel = { l: string; pol: positivity } *)
  let le = { l = v; pol = lp } in
  let re = { l = v'; pol = rp } in
  let lep = { l = v'; pol = oposite rp } in
  let rep = { l = v; pol = oposite lp } in

  try    
    let (_, w, _) = PdlGraph.find_edge !pdlg le re in
		(* edge already exists ... *)
    if w > we then
      begin
				(* ... and its weight is bigger. Its weight is reduced. *)
        PdlGraph.remove_edge !pdlg le re;
        PdlGraph.add_edge_e !pdlg (le, we, re);
        PdlGraph.remove_edge !pdlg lep rep;
        PdlGraph.add_edge_e !pdlg (lep, we, rep);
      end
  with Not_found -> 
		(* New edge. Two arcs. *)
    PdlGraph.add_edge_e !pdlg (le, we, re);
    PdlGraph.add_edge_e !pdlg (lep, we, rep);
    vSet := PdlSet.add 
	       rep 
	       (PdlSet.add 
		  lep
		  (PdlSet.add 
		     re
		     (PdlSet.add le !vSet)  
		  ) 
	       ) 
	       
      
    

module DisplayPdlGraph = struct
  let vertex_name v =
    let v' = PdlGraph.V.label v in
    v'.l^(string_value_of v'.pol)
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let edge_attributes e =
    let l = string_of_int (PdlGraph.E.label e) in
    [`Label l]
  include PdlGraph
end

module DotPdlGraph = Graph.Graphviz.Dot(DisplayPdlGraph)

(**
updates the graph_of_predicates
**)
let update_pdlg acs =
	(** Updates the graph from one clause.
	@param a_clause: Input abstract clause.
	assigns !pdlg. **)
	let update_pdlg_c a_clause =
		if (StringSet.cardinal (StringSet.union a_clause.pos a_clause.neg))
		<= 1 then ()
		else
			begin
				(* Implements Table 1 from [CGS08]. n is p and n' is q. *)
				let nlab = a_clause.num - 1 in
				let neg = ref a_clause.neg in
				StringSet.iter
					(fun p ->
						(* From (neg p or neg q) adds p -> neg q (or equivalently q -> *)
						(* neg p).                                                     *)
								neg := StringSet.remove p !neg;
								StringSet.iter (fun q ->
												if not(p = q) then
													add_edge Neg Neg nlab p q
									) !neg;
								(* From (neg p or q) adds p -> q (or equivalently neg q -> *)
								(* neg p)                                                  *)
								StringSet.iter (fun q ->
												if not(p = q) then
													add_edge Neg Pos nlab p q
									) a_clause.pos
					) a_clause.neg;
				let pos = ref a_clause.pos in
				StringSet.iter
					(fun p ->  (* p' is q in Table 1. *)
						(* From (p or p') add neg p-> p' (or equivalently neg p'-> p) *)
								pos := StringSet.remove p !pos;
								StringSet.iter (fun p' ->
												if not(p = p') then
													add_edge Pos Pos nlab p p'
									) !pos
					) a_clause.pos;
				(* Addition of arcs memorizing comparison predicates: *)
				let cmps = ref a_clause.cmp in
				IdentPairSet.iter
					(fun ip ->  (* ip is a pair of functional symbols. *)
								cmps := IdentPairSet.remove ip !cmps;
								match ip with
								| Pair(f1, f2) ->
										begin
											StringSet.iter
												(fun p ->
															add_edge Neg Pos nlab p (Ident.string f1);
															add_edge Neg Pos nlab p (Ident.string f2);
												) a_clause.neg;
											(* Idem for positive literals *)
											StringSet.iter
												(fun p ->
															add_edge Pos Pos nlab p (Ident.string f1);
															add_edge Pos Pos nlab p (Ident.string f2);
												) a_clause.pos;
											(* Links between f1 and f2 *)
											add_edge Neg Pos nlab (Ident.string f1) (Ident.string f2);
											add_edge Neg Pos nlab (Ident.string f2) (Ident.string f1);
										end
					) a_clause.cmp
			end
	in
	AbstractClauseSet.iter update_pdlg_c acs;
	if debug then
		begin
			let oc = open_out "/tmp/gwhy_pdlg_graph.dot" in
			DotPdlGraph.output_graph oc !pdlg
		end

let rec remove_percents s =
  try
    let i = String.index s '%' in
    (String.set s i '_');
    (remove_percents s)
  with Not_found -> s

let remove_percent_from_stringset sts =
  let r = ref StringSet.empty in
  (StringSet.iter
      (fun st -> r:= StringSet.add (remove_percents st) !r)
      sts);
  !r

let remove_percent_from_abstractclauseset cls =
  let r = ref AbstractClauseSet.empty in
  (AbstractClauseSet.iter
      (fun cl -> r:= AbstractClauseSet.add { 
				num = cl.num; 
				neg = remove_percent_from_stringset cl.neg; 
				pos = remove_percent_from_stringset cl.pos;
				cmp = IdentPairSet.empty } !r)
      cls);
  !r

(** To take comparison operators into account, we consider each operator 
    suffixed by each of its closed identifier **)
(** @params i1 : ident of the comparison **)
(** @params i2 : ident of the closed operator **)
(** @result : string i1 suffixed by i2 **)
(** assigns nothing; **)
let get_suffixed_ident i1 i2 =
  (* let s= *)
  if suffixed_comparison then
    ((Ident.string i1)^"_"^(Ident.string i2))
  else
    (Ident.string i1)
(* in (remove_percents s) *)

let comparison_to_consider id =
  use_comparison_as_criteria_for_graph_construction && (
    ((not comparison_eqOnly) && (is_comparison id or is_int_comparison id or is_real_comparison id))
    || (id == t_eq || id == t_neq || id == t_eq_int || id == t_neq_int || id == t_eq_real || id == t_neq_real )
  )

let is_negative_comparison id =
  (id == t_neq || id == t_neq_int || id == t_neq_real )
  || (id == t_gt || id == t_ge)
  || (id == t_gt_int || id == t_ge_int)
  || (id == t_gt_real || id == t_ge_real)

let inv_comparison id =
  if id == t_eq then t_neq
  else if id == t_neq then t_eq
  else if id == t_eq_int then t_neq_int
  else if id == t_neq_int then t_eq_int
  else if id == t_eq_real then t_neq_real
  else if id == t_neq_real then t_eq_real
  
  else if id == t_lt then t_ge
  else if id == t_le then t_gt
  else if id == t_gt then t_le
  else if id == t_ge then t_lt
  
  else if id == t_lt_int then t_ge_int
  else if id == t_le_int then t_gt_int
  else if id == t_gt_int then t_le_int
  else if id == t_ge_int then t_lt_int
  
  else if id == t_lt_real then t_ge_real
  else if id == t_le_real then t_gt_real
  else if id == t_gt_real then t_le_real
  else if id == t_ge_real then t_lt_real
  
  else assert false (* cases have to match with cases from comparison_to_consider *)

(** @param id a comparison identifier **)
(** @param ti a variable or function identifier **)
(** @return a string composed of id and ti **)
let get_positive_suffixed_ident id ti =
  if (is_negative_comparison id) && (keep_single_comparison_representation) then
    (get_suffixed_ident (inv_comparison id) ti)
  else
    (get_suffixed_ident id ti)

(** @param id a comparison identifier **)
(** @return a string composed with the positive version of id **)
let get_positive_ident id =
  if (is_negative_comparison id) && (keep_single_comparison_representation) then
    (Ident.string (inv_comparison id))
  else
    (Ident.string id)

let build_pred_graph decl =
	let eq_link = ref AbstractClauseSet.empty in
	let treated_suffixes = ref StringSet.empty in
	(* assigns eq_link. *)
	let add_suffixed_depends compId appId =
		if suffixed_comparison && not (StringSet.mem (Ident.string appId) !treated_suffixes) then
			begin (* CGS08. 1. *)
				if equalities_linked then
					begin
						(* Creation des liens =_f -> = et = -> =_f *)
						eq_link:= AbstractClauseSet.add
							({ num = 1;
									neg = StringSet.singleton (get_positive_suffixed_ident compId appId);
									pos = StringSet.singleton (get_positive_ident compId);
									cmp = IdentPairSet.empty })
							(AbstractClauseSet.add
									({ num = 1;
											neg = StringSet.singleton (get_positive_ident compId);
											pos = StringSet.singleton
													(get_positive_suffixed_ident compId appId);
											cmp = IdentPairSet.empty })
									!eq_link)
					end;
				
				if arith_tactic then
					begin
						(* Creation du trio lt_f / le_f / =_f *)
						let eq =
							if is_int_comparison compId then (StringSet.singleton (get_suffixed_ident t_eq_int appId))
							else if is_real_comparison compId then (StringSet.singleton (get_suffixed_ident t_eq_real appId))
							else (StringSet.singleton (get_suffixed_ident t_eq appId)) in
						let lt =
							if is_int_comparison compId then (StringSet.singleton (get_suffixed_ident t_lt_int appId))
							else if is_real_comparison compId then (StringSet.singleton (get_suffixed_ident t_lt_real appId))
							else (StringSet.singleton (get_suffixed_ident t_lt appId)) in
						let le =
							if is_int_comparison compId then (StringSet.singleton (get_suffixed_ident t_le_int appId))
							else if is_real_comparison compId then (StringSet.singleton (get_suffixed_ident t_le_real appId))
							else (StringSet.singleton (get_suffixed_ident t_le appId)) in
						
						let trio = ref AbstractClauseSet.empty in
						trio := AbstractClauseSet.add { num = 1; neg = eq; pos = le;
								cmp = IdentPairSet.empty } !trio;
						trio := AbstractClauseSet.add { num = 2; neg = le; pos = eq;
								cmp = IdentPairSet.empty } !trio;
						trio := AbstractClauseSet.add { num = 2; neg = le; pos = lt;
								cmp = IdentPairSet.empty } !trio;
						trio := AbstractClauseSet.add { num = 1; neg = lt; pos = le;
								cmp = IdentPairSet.empty } !trio;
						
						eq_link:= AbstractClauseSet.union !trio !eq_link
					end;
				
				treated_suffixes:= StringSet.add (Ident.string appId) !treated_suffixes
			end
	in
	
	(* Construction of the Preds dependence graph. Assumes that the clause   *)
	(* cl is in nnf.                                                         *)
	(** @result : a set of clauses. **)
	let add_atom atome cl =
		match atome with
		| Pnot (Papp (id, l, i)) when not (is_comparison id or
				is_int_comparison id or is_real_comparison id) ->
				begin
					if debug then
						begin
						(* Format.printf "\nOperation non suffixee : %a\n"             *)
						(* Util.print_predicate (Papp (id, l, i));                     *)
						end;
					AbstractClauseSet.singleton
						{ num = cl.num + 1;
							pos = cl.pos;
							neg = StringSet.add (Ident.string id) cl.neg;
							cmp = IdentPairSet.empty }
				end
		
		| Papp (id, l, i) when not (is_comparison id or
				is_int_comparison id or is_real_comparison id) ->
				AbstractClauseSet.singleton
					{ num = cl.num + 1;
						neg = cl.neg;
						pos = StringSet.add (Ident.string id) cl.pos;
						cmp = IdentPairSet.empty }
		| Pnot (Papp (id, [el1; el2], i)) when (comparison_to_consider id) ->
				begin
					match (el1, el2) with
					| (Tapp(ti1, _, _), Tapp(ti2, _, _ )) ->
							(add_suffixed_depends id ti1);
							(add_suffixed_depends id ti2);
							(* Added for [CGS08] Sec. 4.3. *)
							if suffixed_comparison then
								AbstractClauseSet.singleton
									{ num = cl.num + 1;
										neg = cl.neg;
										pos = cl.pos;
										cmp = IdentPairSet.add (Pair(ti1, ti2)) cl.cmp }
							else
								(* End of "Added for [CGS08] Sec. 4.3" *)
								if (is_negative_comparison id) && (keep_single_comparison_representation) then
									AbstractClauseSet.add
										{ num = cl.num + 1;
											neg = StringSet.add (get_positive_suffixed_ident id ti1) (StringSet.add (get_positive_suffixed_ident id ti2) cl.neg);
											pos = cl.pos;
											cmp = IdentPairSet.empty }
										(AbstractClauseSet.singleton
												{ num = cl.num + 1;
													neg = cl.neg;
													pos = StringSet.add
															(get_positive_suffixed_ident id ti1)
															(StringSet.add (get_positive_suffixed_ident id ti2) cl.pos);
													cmp = IdentPairSet.empty } )
								else
									AbstractClauseSet.add
										{ num = cl.num + 1;
											neg = cl.neg;
											pos = StringSet.add
													(get_suffixed_ident id ti1)
													(StringSet.add (get_suffixed_ident id ti2) cl.pos);
											cmp = IdentPairSet.empty }
										(AbstractClauseSet.singleton
												{ num = cl.num + 1;
													neg = StringSet.add (get_suffixed_ident id ti1) (StringSet.add (get_suffixed_ident id ti2) cl.neg);
													pos = cl.pos;
													cmp = IdentPairSet.empty } )
					
					| (Tapp(ti, _, _), _ )
					| ( _, Tapp(ti, _, _)) ->
							(add_suffixed_depends id ti);
							if (is_negative_comparison id) && (keep_single_comparison_representation) then
								AbstractClauseSet.add
									{ num = cl.num + 1;
										neg = StringSet.add (get_positive_suffixed_ident id ti) cl.neg;
										pos = cl.pos;
										cmp = IdentPairSet.empty }
									(AbstractClauseSet.singleton
											{ num = cl.num + 1;
												neg = cl.neg;
												pos = StringSet.add (get_positive_suffixed_ident id ti) cl.pos;
												cmp = IdentPairSet.empty } )
							else
								AbstractClauseSet.add
									{ num = cl.num + 1;
										neg = StringSet.add (get_positive_suffixed_ident id ti) cl.neg;
										pos = cl.pos;
										cmp = IdentPairSet.empty }
									(AbstractClauseSet.singleton
											{ num = cl.num + 1;
												neg = StringSet.add (get_suffixed_ident id ti) cl.neg;
												pos = cl.pos;
												cmp = IdentPairSet.empty })
					
					| ( _, _ ) ->
							AbstractClauseSet.singleton
								{ num = cl.num + 1;
									neg = cl.neg;
									pos = cl.pos;
									cmp = IdentPairSet.empty }
				end
		
		| Papp (id, [el1; el2], i) when (comparison_to_consider id) ->
				begin
					match (el1, el2) with
					| (Tapp(ti1, _, _), Tapp(ti2, _, _ )) ->
							(add_suffixed_depends id ti1);
							(add_suffixed_depends id ti2);
							(* Added for [CGS08] Sec. 4.3 *)
							if suffixed_comparison then
								AbstractClauseSet.singleton
									{ num = cl.num + 1;
										neg = cl.neg;
										pos = cl.pos;
										cmp = IdentPairSet.add (Pair(ti1, ti2)) cl.cmp }
							else
								(* End of "Added for [CGS08] Sec. 4.3" *)
								if (is_negative_comparison id) && (keep_single_comparison_representation) then
									AbstractClauseSet.add
										{ num = cl.num + 2;
											neg = cl.neg;
											pos = StringSet.add
													(get_positive_suffixed_ident id ti1)
													(StringSet.add (get_positive_suffixed_ident id ti2) cl.pos);
											cmp = IdentPairSet.empty }
										(AbstractClauseSet.singleton
												{ num = cl.num + 2;
													neg = StringSet.add (get_positive_suffixed_ident id ti1) (StringSet.add (get_positive_suffixed_ident id ti2) cl.neg);
													pos = cl.pos;
													cmp = IdentPairSet.empty } )
								else
									AbstractClauseSet.add
										{ num = cl.num + 2;
											neg = StringSet.add (get_suffixed_ident id ti1) (StringSet.add (get_suffixed_ident id ti2) cl.neg);
											pos = cl.pos;
											cmp = IdentPairSet.empty }
										(AbstractClauseSet.singleton
												{ num = cl.num + 2;
													neg = cl.neg;
													pos = StringSet.add
															(get_suffixed_ident id ti1)
															(StringSet.add (get_suffixed_ident id ti2) cl.pos);
													cmp = IdentPairSet.empty } )
					
					| (Tapp(ti, _, _), _ )
					| ( _, Tapp(ti, _, _)) ->
							(add_suffixed_depends id ti);
							if (is_negative_comparison id) && (keep_single_comparison_representation) then
								AbstractClauseSet.add
									{ num = cl.num + 2;
										neg = cl.neg;
										pos = StringSet.add (get_positive_suffixed_ident id ti) cl.pos;
										cmp = IdentPairSet.empty }
									(AbstractClauseSet.singleton
											{ num = cl.num + 2;
												neg = StringSet.add (get_positive_suffixed_ident id ti) cl.neg;
												pos = cl.pos;
												cmp = IdentPairSet.empty } )
							else
								AbstractClauseSet.add
									{ num = cl.num + 2;
										neg = StringSet.add (get_suffixed_ident id ti) cl.neg;
										pos = cl.pos;
										cmp = IdentPairSet.empty }
									(AbstractClauseSet.singleton
											{ num = cl.num + 2;
												neg = cl.neg;
												pos = StringSet.add (get_suffixed_ident id ti) cl.pos;
												cmp = IdentPairSet.empty })
					
					| ( _, _ ) ->
							AbstractClauseSet.singleton
								{ num = cl.num + 1;
									neg = cl.neg;
									pos = cl.pos;
									cmp = IdentPairSet.empty }
				end
		| _ ->
				AbstractClauseSet.singleton
					{ num = cl.num + 1;
						neg = cl.neg;
						pos = cl.pos;
						cmp = IdentPairSet.empty }
	
	in
	let rec get_abstract_clauses p =
		let rec compute_clause p acs =
			match p with
			| Forall (_, _, _, _, _, p) -> compute_clause p acs
			| Exists (_, _, _, p) -> compute_clause p acs
			| Por (p1, p2) -> compute_clause p1 (compute_clause p2 acs)
			| _ as p ->
					let r = ref AbstractClauseSet.empty in
					(AbstractClauseSet.iter
							(fun ac -> r:= AbstractClauseSet.union (add_atom p ac) !r)
							acs);
					!r
		(* AbstractClauseSet.fold_right (AbstractClauseSet.union (add_atom p)) *)
		(* acs AbstractClauseSet.empty in                                      *)
		in
		match p with
		| Forall (_, _, _, _, _, p) -> get_abstract_clauses p
		| Exists (_, _, _, p) -> get_abstract_clauses p
		| Pand (_, _, p1, p2) ->
				AbstractClauseSet.union
					(get_abstract_clauses p1)
					(get_abstract_clauses p2)
		| Papp(_) as p ->
		(* AbstractClauseSet.singleton *)
				(add_atom p { num = 0; pos = StringSet.empty; neg = StringSet.empty;
							cmp = IdentPairSet.empty })
		| Pnot(_) as p ->
		(* AbstractClauseSet.singleton *)
				(add_atom p { num = 0; pos = StringSet.empty; neg = StringSet.empty;
							cmp = IdentPairSet.empty })
		| Por (_) as p ->
		(* AbstractClauseSet.singleton *)
				(compute_clause
						p (AbstractClauseSet.singleton {
									num = 0;
									pos = StringSet.empty;
									neg = StringSet.empty;
									cmp = IdentPairSet.empty }))
		| Pfalse ->
		(* AbstractClauseSet.singleton *)
				(add_atom Pfalse { num = 0; pos = StringSet.empty; neg = StringSet.empty;
							cmp = IdentPairSet.empty })
		| Ptrue ->
		(* AbstractClauseSet.singleton *)
				(add_atom Ptrue { num = 0; pos = StringSet.empty; neg = StringSet.empty;
							cmp = IdentPairSet.empty })
		| Pnamed(_, _)
		| Pfpi (_, _, _)
		| Forallb (_, _, _)
		| Piff (_, _)
		| Pif (_, _, _)
		| Pimplies (_, _, _)
		| Pvar _ -> assert false
	in
	let compute_pred_graph = function
		| Dpredicate_def (loc, ident, def) ->
				let bl, p = def.scheme_type in
				let rootexp = (Papp (
							Ident.create ident,
							List.map (fun (i, _) -> Tvar i) bl, [])) in
				let piff = Piff (rootexp, p) in
				let pforall = List.fold_right
						(fun (var, tvar) pred -> Forall(false, var, var, tvar,[], pred))
						bl piff in
				let p' = cnf pforall in
				begin
					context := (p', Dpredicate_def (loc, ident, def))::!context;
					let cls = get_abstract_clauses p' in
					let cls = AbstractClauseSet.union cls !eq_link in
					if debug then
						begin
						(* Format.printf "%a" Util.print_predicate p; display_cl_set   *)
						(* cls                                                         *)
						end;
					update_pdlg (remove_percent_from_abstractclauseset cls )
				end
		| Daxiom (loc, ident, ps) ->
		(* loc: , ident: , ps: *)
				let p = ps.scheme_type in
				let p' = cnf p in
				begin
					context := (p', Daxiom (loc, ident, ps))::!context;
					let cls = get_abstract_clauses p' in
					let cls = AbstractClauseSet.union cls !eq_link in
					if debug then
						begin
						(* Format.printf "%a" Util.print_predicate p; display_cl_set   *)
						(* cls                                                         *)
						end;
					update_pdlg (remove_percent_from_abstractclauseset cls)
				end
		| a ->
				context := (Ptrue, a)::!context;
	in
	Queue.iter compute_pred_graph decl

(** End of graph of predicates**)


let set_of_pred_p vs =
  PdlSet.fold
    (fun v s ->
          let pred_set =
            try
              let pred_list = PdlGraph.pred !pdlg v in
              List.fold_right
                (fun el s ->
                      PdlSet.add el s) pred_list PdlSet.empty
            with
              _ -> PdlSet.empty in
          let s' = PdlSet.union s pred_set in
          let succ_set =
            try
              let succ_list = PdlGraph.succ !pdlg { l = v.l; pol = oposite v.pol } in
              List.fold_right
                (fun v s ->
                      PdlSet.add { l = v.l; pol = oposite v.pol } s)
                succ_list PdlSet.empty
            with
              _ -> PdlSet.empty in
          PdlSet.union s' succ_set
    )
    vs
    PdlSet.empty


(***************************************
*********
*******************)

module W = struct 
  type label = PdlGraph.E.label
  type t = int
  let weight x = x
  let zero = 0
  let add = (+)
  let compare = compare
end

  

module Dij = Dijkstra(PdlGraph)(W)

let compute_sequence_of_predicates concl_preds =
	(* compute the weight of the shortest path from the node elem and nodes of *)
	(* the conclusion                                                          *)
	let max = ref 0 in
	let dij elem =
		let dij_and_comp v2 acc =
			try
			(* Dijkstra from elem to node v2 of the conclusion *)
				let (_, d) = Dij.shortest_path !pdlg elem v2 in
				(* update max *)
				max := if d > !max then d else !max;
				(* memorizing the shortest path *)
				if (acc == (- 1)) || (d < acc) then
					d
				else
					acc
			with Not_found ->
					(- 1)
		in
		PdlSet.fold dij_and_comp concl_preds (- 1)
	in
	
	let hash_lits : (int,'a) Hashtbl.t = Hashtbl.create 10 in
	(* adds into L[dij] the node elem where dij is the dijkstra result. If dij *)
	(* is -1, then we set L[max+1]                                             *)
	PdlSet.iter
		(fun elem ->
					let w = dij elem in
					try
						let l_i = Hashtbl.find hash_lits w in
						if w != (- 1) then
							Hashtbl.replace hash_lits w (PdlSet.add elem l_i)
						else
							Hashtbl.replace hash_lits (!max + 1) (PdlSet.add elem l_i)
					with Not_found ->
							Hashtbl.add hash_lits w (PdlSet.singleton elem )
		) !vSet;
	
	if debug then
		begin
			Format.printf "List of predicates \n";
			for i = 0 to 10 do
				Format.printf "L[%d] is " i;
				try
					let l_i = Hashtbl.find hash_lits i in
					display_symb_of_pdl_set l_i
				with _ ->
						Format.printf " empty \n";
			done;
		end;
	hash_lits





    
 
	   
	 
    
  





(* Use of the preds dependence graph to filter hypothesis *)
let get_preds_of p filter_comparison =
  let s = ref PdlSet.empty in
  
  (* @params i : ident of the comparison @params ref s : list of           *)
  (* predicates where i has to be added @params polarity : polarity to set *)
  (* up @aprams t : term to add as suffixe @result : s including i         *)
  (* suffixed with t                                                       *)
  let add_suffixed_comparison i polarity = function
    | Tapp (ti, _, _) ->
        if (is_negative_comparison i) && (keep_single_comparison_representation) then (* negative comparaisons are traducted as negative in the graph *)
        s:= (PdlSet.add { l = remove_percents (get_suffixed_ident (inv_comparison i) ti);
              pol = if polarity == 1 then Neg else Pos } !s)
        else
          s:= (PdlSet.add { l = remove_percents (get_suffixed_ident i ti);
                pol = if polarity == 1 then Pos else Neg } !s)
    
    | Tvar (_)
    | Tderef (_)
    | Tconst (_) ->
        () (* We do not consider cases with literal constants in the suffix *)
    | Tnamed (_, _) ->
        assert false (* Currently, no label is present close to a comparison predicate *)
  in
  let rec get polarity = function
    (* Treatment of each predicates cases, expect comparison predicates *)
    | Papp (id, l, i) when not
      (is_comparison id or
        is_int_comparison id or
        is_real_comparison id) ->
        s := PdlSet.add { l = remove_percents (Ident.string id);
            pol = if polarity == 1 then Pos else Neg } !s
    
    (* | Papp (id, l, i) when use_arith_comp_as_direct_criteria -> s :=        *)
    (* PdlSet.add {l=Ident.string id ; pol= if polarity == 1 then Pos else     *)
    (* Neg} !s                                                                 *)
    
    (* Particular treatment for comparison predicates (only equality at    *)
    (* this time). Each of them is added twice (with a suffix corresonding *)
    (* to each of parameters)                                              *)
    | Papp (id, l, i) when (comparison_to_consider id) && filter_comparison ->
        (List.iter (add_suffixed_comparison id polarity) l)
    
    | Forall (w, id, b, v, tl, p) ->
        get polarity p
    | Exists (id, b, v, p) ->
        get polarity p
    | Pimplies (_, p1, p2) ->
        get (- 1 * polarity) p1;
        get polarity p2
    | Pand (_, _, p1, p2) | Por (p1, p2) ->
        get polarity p1;
        get polarity p2
    | Piff (p1, p2) ->
    (* get polarity Pimplies (_,p1,p2) ; *)
        get (- 1 * polarity) p1;
        get polarity p2;
        (* get polarity Pimplies (_,p2,p1) *)
        get (- 1 * polarity) p2;
        get polarity p1
    | Pnot p1 ->
        get (- 1 * polarity) p1;
    | Pvar _ | Ptrue | Pfalse -> ()
    | _ -> () in
  get 1 p;
  !s

let get_predecessor_pred p n =
  (** av stands for tha already visited preds
  nyt stands for not yet treated preds **)
  let rec get_preds_in_graph nyv av n =
    if n > 0 then
      let new_preds = set_of_pred_p nyv in
      let nyv = PdlSet.diff new_preds av in
      PdlSet.union new_preds
        (get_preds_in_graph
            nyv
            (PdlSet.union new_preds av)
            (n - 1)
        )
    else
      av in
  get_preds_in_graph p p n

let get_relevant_preds hl n =
  let avs = ref PdlSet.empty in
  for i = 0 to n do
    try 
      let l_i = Hashtbl.find hl i in
      avs := PdlSet.union !avs l_i
    with Not_found -> 
      () (* TODO remove this *)
  done; 
  !avs
 


 


(**
functions for the main function: reduce
@param l' : liste d'hypothï¿½ses
@param g' : prï¿½dicat du but
**)
let reduce_subst (l', g') =
  let many_substs_in_predicate sl p =
    List.fold_left
      (fun p s ->
            tsubst_in_predicate s p
      ) p sl
  in
  let many_substs_in_term sl t =
    List.fold_left
      (fun t s ->
            tsubst_in_term s t
      ) t sl
  in
  let many_substs_in_context sl = function
    | Papp (id, l, i) ->
        Papp (id, List.map (many_substs_in_term sl ) l, i)
    | Pif (a, b, c) ->
        Pif (many_substs_in_term sl a,
          many_substs_in_predicate sl b,
          many_substs_in_predicate sl c)
    | Pfpi (t, f1, f2) ->
        Pfpi (many_substs_in_term sl t, f1, f2)
    | Forall (w, id, b, v, tl, p) ->
        Forall (w, id, b, v, tl,
          many_substs_in_predicate sl p)
    | Exists (id, b, v, p) ->
        Exists (id, b, v, many_substs_in_predicate sl p)
    | p ->
        map_predicate (many_substs_in_predicate sl) p
  in
  
  let compute_subst (sl, fl) f =
    match f with
    | Svar (id, v) as var_def -> (sl, var_def:: fl)
    | Spred (id, p) ->
        begin
          match p with
            Papp (id, [el1; el2], _) when is_eq id ->
              begin
                match (el1, el2) with
                  (Tvar (v1), t2) | (t2, Tvar (v1)) ->
                    let subst = subst_one v1 t2 in
                    begin
                      if debug then
                        Format.printf "Removed  pred %a" Util.print_predicate p;
                    end;
                    (subst:: sl, fl)
                
                | _ ->
                    (sl, Spred(id, (many_substs_in_context sl p)):: fl)
              end
          | _ as p -> (sl, Spred(id, (many_substs_in_context sl p)):: fl)
          
        end in
  let (sl, fl) = List.fold_left compute_subst ([],[]) l' in
  let l' = List.rev fl in
  let g' = many_substs_in_predicate sl g' in
  (l', g')

(**
@param concl_rep the representative variable of the conclusion
@param l the list of hypothesis
@return a list where the hypotheses have been filtered.
An hypothese is selected if
- it is a variable declaration
- it is an hypothese which has the same representant than
the conclusion
**)
let filter_acc_variables l concl_rep selection_strategy pred_symb =
  (** UPDATE THIS ACCORDING TO THE ARRTICLE **)
  
  let rec all_vars_from_term vars qvars = function
    | Tvar v | Tderef v ->
        let r = (member_of (Ident.string v) vars) || (List.mem (Ident.string v) qvars) in
        if debug then
          if r then
            Format.printf "   ->  Tvar %s OK\n" (Ident.string v)
          else
            begin
              Format.printf "   ->  Tvar %s NON PRESENTE\n" (Ident.string v);
              Format.printf "       qvars :";
              List.iter(fun v -> Format.printf ", %s" v) qvars;
              Format.printf "\n";
              
            end;
        r
    | Tapp (_, lt, _) ->
        begin
          if debug then Format.printf "      -> Tapp -->\n"
        end;
        List.for_all (all_vars_from_term vars qvars) lt
    | Tconst _ ->
        true
    | Tnamed(lab, t) ->
        all_vars_from_term vars qvars t
  in
  
  let rec all_vars_in_one_branch lp vars =
    let rec all_v pred qvars =
      match pred with
      | Forall (_, id, _, _, _, p)
      | Exists (id, _, _, p) ->
          begin
            if debug then Format.printf "   -> Quantif %s \n" (Ident.string id)
          end;
          all_v p ((Ident.string id):: qvars)
      
      | Piff (p1, p2)
      | Pand (_, _, p1, p2)
      | Por (p1, p2)
      | Pimplies (_, p1, p2) ->
          begin
            if debug then Format.printf "   -> And / Or / Implies %a %a \n" Util.print_predicate p1 Util.print_predicate p2
          end;
          if all_v p1 qvars then (all_v p2 qvars) else false
      
      | Pnot (p) ->
          begin
            if debug then Format.printf "   ->  Pnot %a \n" Util.print_predicate p
          end;
          all_v p qvars
      
      | Papp (_, lt, _) ->
          begin
            if debug then Format.printf "   ->  Papp --> \n"
          end;
          List.for_all (all_vars_from_term vars qvars) lt
      
      | Pvar (v) ->
          begin
            let r = (member_of (Ident.string v) vars) || (List.mem (Ident.string v) qvars) in
            if debug then
              if r then
                Format.printf "   ->  Pvar %s OK\n" (Ident.string v)
              else
                Format.printf "   ->  Pvar %s NON PRESENTE\n" (Ident.string v);
            r
          end;
      
      | Pfalse
      | Ptrue -> true
      | Pfpi _ ->
          failwith "fpi not yet suported "
      | Pnamed (_, _) ->
          failwith "Pnamed has to be not found there (nnf has to remove it)"
      | Forallb (_, _, _) ->
          failwith "Forallb has to be not found there (nnf has to remove it)"
      | Pif (a, b, c) ->
          failwith "Pif has to be not found there (nnf has to remove it)"
    
    in
    match lp with
    | [] -> false
    | p:: l ->
        if all_v p [] then
          begin
            if debug then Format.printf " Branche retenue : %a \n" Util.print_predicate p;
            true
          end
        else
          begin
            if debug then Format.printf " Branche rejetee : %a \n" Util.print_predicate p;
            (all_vars_in_one_branch l vars)
          end
  
  in
  
  let rec filter = function
    | [] -> []
    | Svar (id, v) :: q ->
        Svar (id, v) :: filter q
    | Spred (t, p) :: q ->
        let vars =
          try Hashtbl.find hash_hyp_vars p
          with Not_found -> assert false
        in
        let v' = (removes_fresh_vars vars) in
        let condition = match selection_strategy with
          | AllVars
          | SplitHyps
          | CNFHyps ->
              VarStringSet.subset
                v'
                concl_rep
          (*** TODO UPDATE THIS ***)
          
          | One -> not (VarStringSet.is_empty
                    (VarStringSet.inter
                        v'
                        concl_rep))
          
          | AllInABranch ->
              all_vars_in_one_branch (Util.split_one [] 1 p) concl_rep
        
        in
        if debug then begin
          Format.printf "\nHyp :\n %a \n\n" Util.print_predicate p;
          display_str "vars" vars;
          display_str "concl_rep" concl_rep;
        end;
        if condition then
          begin
            if debug then
              Format.printf "Keeped #1 (Vars)\n\n";
            
            (* the predicate symbols has to be in the list of symbols *)
            let preds_of_p = get_preds_of p use_comparison_as_criteria_for_hypothesis_filtering in
            begin
              if debug then
                begin
                  Format.printf "Hyp Preds : \n";
                  display_symb_of_pdl_set preds_of_p;
                  Format.printf "Relevent Preds : \n";
                  display_symb_of_pdl_set pred_symb;
                end;
              if abstr_subset_of_pdl preds_of_p pred_symb then
                begin
                  if debug then
                    Format.printf "Keeped #2 (Preds)\n\n";
                  Spred (t, p):: filter q
                end
              else
                begin
                  if debug then
                    Format.printf "Dropped (Preds)\n\n";
                  filter q
                end
            end
          end
        else
          begin
            if debug then
              Format.printf "Dropped (Vars)\n\n";
            filter q
          end;
  in
  filter l

(** Filter the context global variable according to preserved preds **)
let managesContext relevantPreds decl =
  (* Seulement si pruning axiomes *)
  if prune_context then
    begin
      Queue.clear decl;
      (* Pour chaque "sous-axiome" introduit : regarder si on le garde ou  *)
      (* pas                                                               *)
      let filter_one_axiom (p, ax) =
        let preds_of_p = get_preds_of p use_comparison_as_criteria_for_hypothesis_filtering in
        if abstr_subset_of_pdl preds_of_p relevantPreds then
          Queue.push ax decl
        else
          begin
            if debug then
              Format.printf "Ctx Dropped\n\n";
            (* Queue.push ax decl *)
          end
      in
      let rec filter ctx =
        match ctx with
        | [] -> ()
        
        | (p_cnf, Dpredicate_def (loc, ident, def)) :: l ->
            let preds_of_p_cnf = get_preds_of p_cnf use_comparison_as_criteria_for_hypothesis_filtering in
            if (abstr_subset_of_pdl preds_of_p_cnf relevantPreds) then
              begin
                (* On garde tout le prï¿½dicat *)
                if debug then
                  Format.printf "Ctx Keeped\n\n";
                Queue.push (Dpredicate_def (loc, ident, def)) decl;
                filter l
              end
            else
              begin
                (* Sinon on insï¿½re la sa signature du prï¿½dicat avec un logic *)
                (let bl, _ = def.scheme_type in
                  Queue.push (Dlogic(loc, ident, generalize_logic_type (Predicate(List.map snd bl)))) decl);
                
                (* Ensuite, on test chacune des clauses pour ï¿½ventuellement les  *)
                (* prï¿½server                                                     *)
                let p_list = (split_one [] 1 p_cnf) in
                let ax_list = List.map (fun p ->
                          let i = my_fresh_hyp_str () in
                          (p, Daxiom (loc, "axiom_from_"^ident^"_part_"^i, generalize_predicate p))
                    ) p_list in
                
                List.iter filter_one_axiom ax_list;
                filter l
              end
        
        | (p_cnf, Daxiom (loc, ident, ps)) :: l ->
            let preds_of_p_cnf = get_preds_of p_cnf use_comparison_as_criteria_for_hypothesis_filtering in
            if (abstr_subset_of_pdl preds_of_p_cnf relevantPreds) then
              begin
                (* On garde tout l'axiome en forme originale *)
                if debug then
                  Format.printf "Ctx Keeped\n\n";
                Queue.push (Daxiom (loc, ident, ps)) decl;
                filter l
              end
            else
              begin
                (* Sinon, on splite le prï¿½dicat de dï¿½finition en Clauses       *)
                (* devenant autant d'axiomes                                       *)
                let p_list = (split_one [] 1 p_cnf) in
                let ax_list = List.map (fun p ->
                          let i = my_fresh_hyp_str () in
                          (p, Daxiom (loc, ident^"_part_"^i, generalize_predicate p))
                    ) p_list in
                
                List.iter filter_one_axiom ax_list;
                filter l
              end
        
        | (p_cnf, c) :: l ->
            if debug then
              begin
                Format.printf "Ctx (Autre): \n";
                Format.printf "%a \n" print_decl c
              end;
            Queue.push c decl;
            filter l
      in
      filter (List.rev !context)
    end
(* Fin pruning des axiomes du context *)

let managesGoal ax (hyps, concl) decl =
  let (loc, expl, id, _) = ax in
  (** retrieves the list of predicates in the conclusion **)
  let concl_preds = get_preds_of concl true in
  (* if weights are not consider (as [FTP07] implementation)*)
  let relevant_preds = 
      if prune_coarse_pred_comp then 
	get_predecessor_pred concl_preds !pb 
      else
	begin
	  let hl = compute_sequence_of_predicates concl_preds in 
	  get_relevant_preds hl !pb 
	end in

  (** retrieves the list of variables in the conclusion **)
  let vars_of_concl = VarStringSet.diff (free_vars_of concl) avoided_vars in
  let reachable_vars = VarStringSet.diff
      (get_reachable_vars vars_of_concl !vb)
      avoided_vars in
  let relevant_vars = VarStringSet.union reachable_vars vars_of_concl in 
  (* Traitement fait 2 fois pour l'optimisation *)
  
  let l' = filter_acc_variables hyps relevant_vars var_filter_tactic 
    (*All  AllInABranch*) relevant_preds in
  
  if debug then
    begin
      display_str "Relevant vars " relevant_vars;
      display_str "Reachable vars " reachable_vars;
      
      if VarStringSet.subset relevant_vars reachable_vars then Format.printf "Relevant <: Reachable\n" else Format.printf "Relevant /<: Reachable\n";
      if VarStringSet.subset reachable_vars relevant_vars then Format.printf "Reachable <: Relevant\n" else Format.printf "Reachable /<: Relevant\n";
      
      Format.printf "Concl preds ";
      display_symb_of_pdl_set concl_preds;
      Format.printf "Relevant preds ";
      display_symb_of_pdl_set relevant_preds;
      let oc = open_out "/tmp/gwhy_var_graph.dot" in
      DotVG.output_graph oc !vg
    end;
  
  managesContext relevant_preds decl;
  
  (loc, expl, id, (l', concl))

let hyps_to_cnf lh =
  let lh_cnf =
    List.fold_left (fun l h ->
            match h with
            | Svar (id, v) as var_def -> var_def:: l
            | Spred (id, p) ->
                let p'= cnf p in 
		(* p' est une CNF de P mais on veut 1 hypothese par clause 
		   alors on coupe selon les and. 
		   On fera appelle au  split a la fin du traitement 
		   de toutes les hypotheses*) 
                Spred(id, p'):: l
        
      ) [] lh
  in
  Util.split [] my_fresh_hyp lh_cnf

let reset () =
  vg := Var_graph.create();
  pdlg := PdlGraph.create();
  Hashtbl.clear hash_hyp_vars;
  v_count := 0;
  hyp_count := 0

(* Module entry point. *)
(* @param q *)
(* @param decl *)
(* assigns ??? *)
let reduce q decl =
  reset();
  
  (** memorize the theory as a graph of predicate symbols **)
  build_pred_graph decl;
  
  (** manage goal **)
  let q' = match q with
      (loc, expl, id, s) as ax ->
        let (l, g) = s in (* l : liste d'hypothï¿½ses (contexte local) et g : le but*)
        let (l', g') = Util.intros [] g my_fresh_hyp (var_filter_tactic == SplitHyps) in
        let l' = if (var_filter_tactic == CNFHyps) then hyps_to_cnf l' else l' in
        let l' = List.append l l' in
        let (l', g') = reduce_subst (l', g') in
        
        (** memorize hypotheses as a graph of variables **)
        build_var_graph (l', g');
        
        (** focus on goal **)
        managesGoal ax (l', g') decl
  in
  q'