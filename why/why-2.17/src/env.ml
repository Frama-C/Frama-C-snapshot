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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: env.ml,v 1.80 2008/11/05 14:03:17 filliatr Exp $ i*)

open Ident
open Misc
open Types
open Ast
open Logic
open Error
open Report
open Cc

(* generalization *)

module TypeVar = struct
  type t = type_var
  let compare v1 v2 = compare v1.tag v2.tag 
end
module Vset = Set.Make(TypeVar)
module Vmap = Map.Make(TypeVar)
type var_subst = type_var Vmap.t

type 'a scheme = { scheme_vars : Vset.t; scheme_type : 'a }

let empty_scheme t = { scheme_vars = Vset.empty ; scheme_type = t }

let rec find_pure_type_vars env t =
  match t with
    | PTvar ({ type_val = None } as v) -> 
	Vset.add v env
    | PTvar { type_val = Some t } ->
	find_pure_type_vars env t
    | PTexternal(l,id) ->
	List.fold_left find_pure_type_vars env l
    | PTint | PTreal | PTbool | PTunit -> 
	env

let find_logic_type_vars t =
  match t with
    | Function(tl,tr) ->
	let env = find_pure_type_vars Vset.empty tr in
	List.fold_left find_pure_type_vars env tl
    | Predicate(tl) ->
	List.fold_left find_pure_type_vars Vset.empty tl

let rec find_type_v_vars acc t =
  match t with
    | Ref t | PureType t -> find_pure_type_vars acc t
    | Arrow(bl,c) ->
	let acc = find_type_c_vars acc c in
	List.fold_left find_binder_vars acc bl
and find_type_c_vars acc c = find_type_v_vars acc c.c_result_type
and find_binder_vars acc (_,t) = find_type_v_vars acc t


let generalize_logic_type t =
  let l = find_logic_type_vars t in
  { scheme_vars = l ; scheme_type = t }

let rec find_term_vars acc = function
  | Tconst _
  | Tvar _
  | Tderef _ ->
      acc
  | Tapp (_, tl, i) ->
      List.fold_left find_term_vars 
	(List.fold_left find_pure_type_vars acc i) tl
  | Tnamed(_,t) -> find_term_vars acc t

let rec find_pattern_vars acc = function
  | TPat t -> find_term_vars acc t
  | PPat p -> find_predicate_vars acc p

and find_predicate_vars acc p =
  match p with
    | Pvar _
    | Pfpi _
    | Ptrue
    | Pfalse -> 
	acc
    | Papp (_, tl, i) ->
	List.fold_left find_term_vars 
	  (List.fold_left find_pure_type_vars acc i) tl
    | Pimplies (_,p1,p2) 
    | Pif (_,p1,p2)
    | Pand (_,_,p1,p2)
    | Piff (p1,p2)
    | Por (p1,p2) ->
	find_predicate_vars (find_predicate_vars acc p1) p2
    | Pnot p -> find_predicate_vars acc p
    | Forall (_,_,_,t,tl,p) ->
	List.fold_left (List.fold_left find_pattern_vars)
	  (find_predicate_vars (find_pure_type_vars acc t) p) tl
    | Exists (_,_,t,p) ->
	find_predicate_vars (find_pure_type_vars acc t) p
    | Forallb (_,p1,p2) ->
	find_predicate_vars (find_predicate_vars acc p1) p2
    | Pnamed (_,p) ->
	find_predicate_vars acc p

let generalize_predicate p =
  let l = find_predicate_vars Vset.empty p in
  { scheme_vars = l ; scheme_type = p }

let generalize_predicate_def (bl,p) = 
  let l = 
    List.fold_left (fun acc (_,pt) -> find_pure_type_vars acc pt) Vset.empty bl
  in
  let l = find_predicate_vars l p in
  { scheme_vars = l; scheme_type = (bl,p) }

let generalize_inductive_def (bl,l) =
  let vars = List.fold_left find_pure_type_vars Vset.empty bl in
  let vars =
    List.fold_left (fun acc (_,p) -> find_predicate_vars acc p) vars l
  in
  { scheme_vars = vars ; scheme_type = (bl,l) }

let generalize_function_def (bl,t,e) = 
  let l = 
    List.fold_left (fun acc (_,pt) -> find_pure_type_vars acc pt) Vset.empty bl 
  in
  let l = find_pure_type_vars l t in
  { scheme_vars = l; scheme_type = (bl,t,e) }

(* specialization *)

let dump_type_var = ref (fun (v:type_var) -> ())

let new_type_var =
  let c = ref 0 in
  fun ?(user=false) () -> 
    incr c; 
    let v = { tag = !c; user=user; type_val = None } in 
    v

let rec subst_pure_type s t =
  match t with
    | PTvar ({ type_val = None } as v) ->
	(try PTvar (Vmap.find v s) with Not_found -> t)
    | PTvar { type_val = Some t } ->
	subst_pure_type s t
    | PTexternal(l,id) ->
	PTexternal(List.map (subst_pure_type s) l,id)
    | PTint | PTreal | PTbool | PTunit -> t


let subst_logic_type s = function
  | Function (tl,tr) -> 
      Function (List.map (subst_pure_type s) tl, subst_pure_type s tr)
  | Predicate (tl) -> 
      Predicate (List.map (subst_pure_type s) tl)

let rec subst_term s = function
  | Tapp (id, tl, i) -> 
      Tapp (id, List.map (subst_term s) tl, List.map (subst_pure_type s) i)
  | Tconst _ | Tvar _ | Tderef _ as t -> 
      t
  | Tnamed (lab,t) -> Tnamed(lab,subst_term s t)

let rec subst_pattern s = function
  | TPat t -> TPat (subst_term s t)
  | PPat p -> PPat (subst_predicate s p)

and subst_trigger s = List.map (subst_pattern s)

and subst_triggers s = List.map (subst_trigger s)

and subst_predicate s p =
  let f = subst_predicate s in
  match p with
  | Pimplies (w, a, b) -> Pimplies (w, f a, f b)
  | Pif (a, b, c) -> Pif (subst_term s a, f b, f c)
  | Pand (w, s, a, b) -> Pand (w, s, f a, f b)
  | Por (a, b) -> Por (f a, f b)
  | Piff (a, b) -> Piff (f a, f b)
  | Pnot a -> Pnot (f a)
  | Forall (w, id, b, v, tl, p) -> 
      Forall (w, id, b, subst_pure_type s v, subst_triggers s tl, f p)
  | Exists (id, b, v, p) -> Exists (id, b, subst_pure_type s v, f p)
  | Forallb (w, a, b) -> Forallb (w, f a, f b)
  | Papp (id, tl, i) -> 
      Papp (id, List.map (subst_term s) tl, List.map (subst_pure_type s) i)
  | Pfpi (t, a, b) -> Pfpi (subst_term s t, a, b)
  | Pnamed (n, a) -> Pnamed (n, f a)
  | Ptrue | Pfalse | Pvar _ as p -> p

let rec subst_type_v s = function
  | Ref t -> Ref (subst_pure_type s t)
  | PureType t -> PureType (subst_pure_type s t)
  | Arrow (bl,c) -> Arrow (List.map (subst_binder s) bl,subst_type_c s c)
and subst_binder s (id,t) = 
  (id, subst_type_v s t)
and subst_type_c s c =
  { c with 
    c_result_type = subst_type_v s c.c_result_type;
    c_pre = List.map (subst_predicate s) c.c_pre;
    c_post = option_app (post_app (subst_predicate s)) c.c_post }

let specialize_scheme subst s =
  let env =
    Vset.fold
      (fun x s -> Vmap.add x (new_type_var()) s)
      s.scheme_vars Vmap.empty
  in 
  (env, subst env s.scheme_type)

let specialize_logic_type = specialize_scheme subst_logic_type

let specialize_type_v = specialize_scheme subst_type_v

let specialize_predicate = specialize_scheme subst_predicate

let subst_predicate_def s (bl,p) =
  let bl = List.map (fun (x,pt) -> (x, subst_pure_type s pt)) bl in
  bl, subst_predicate s p

let specialize_predicate_def = specialize_scheme subst_predicate_def

let subst_inductive_def s (bl,p) =
  let bl = List.map (subst_pure_type s) bl in
  bl, List.map (fun (id,p) -> (id,subst_predicate s p)) p

let specialize_inductive_def = specialize_scheme subst_inductive_def

let subst_function_def s (bl,t,e) =
  let bl = List.map (fun (x,pt) -> (x, subst_pure_type s pt)) bl in
  bl, subst_pure_type s t, e

let specialize_function_def = specialize_scheme subst_function_def

let rec find_cc_type_vars acc = function
  | TTpure pt -> find_pure_type_vars acc pt
  | TTarray cc -> find_cc_type_vars acc cc
  | TTlambda (b, cc) -> find_cc_binder_vars (find_cc_type_vars acc cc) b
  | TTarrow (b, cc) -> find_cc_binder_vars (find_cc_type_vars acc cc) b
  | TTtuple (bl, cco) -> 
      List.fold_left find_cc_binder_vars 
	(match cco with None -> acc | Some cc -> find_cc_type_vars acc cc) bl
  | TTpred p -> find_predicate_vars acc p
  | TTapp (cc, l) -> 
      find_cc_type_vars (List.fold_left find_cc_type_vars acc l) cc
  | TTterm _ 
  | TTSet -> acc

and find_cc_bind_type_vars acc = function
  | CC_var_binder cc -> find_cc_type_vars acc cc
  | CC_pred_binder p -> find_predicate_vars acc p
  | CC_untyped_binder -> acc

and find_cc_binder_vars acc (_, bt) = find_cc_bind_type_vars acc bt

let rec subst_cc_type s = function
  | TTpure pt -> TTpure (subst_pure_type s pt)
  | TTarray cc -> TTarray (subst_cc_type s cc)
  | TTlambda (b, cc) -> TTlambda (subst_cc_binder s b, subst_cc_type s cc)
  | TTarrow (b, cc) -> TTarrow (subst_cc_binder s b, subst_cc_type s cc)
  | TTtuple (bl, cco) -> 
      TTtuple (List.map (subst_cc_binder s) bl, 
	       option_app (subst_cc_type s) cco)
  | TTpred p -> TTpred (subst_predicate s p)
  | TTapp (cc, l) -> TTapp (subst_cc_type s cc, List.map (subst_cc_type s) l)
  | TTterm t -> TTterm (subst_term s t)
  | TTSet -> TTSet

and subst_cc_bind_type s = function
  | CC_var_binder cc -> CC_var_binder (subst_cc_type s cc)
  | CC_pred_binder p -> CC_pred_binder (subst_predicate s p)
  | CC_untyped_binder -> CC_untyped_binder

and subst_cc_binder s (id, bt) = (id, subst_cc_bind_type s bt)

let subst_sequent s (h, p) = 
  let subst_hyp = function
    | Svar (id, t) -> Svar (id, subst_pure_type s t)
    | Spred (id, p) -> Spred (id, subst_predicate s p)
  in
  (List.map subst_hyp h, subst_predicate s p)

let find_sequent_vars (h, p) =
  let find_hyp_vars acc = function
    | Svar (_, t) -> find_pure_type_vars acc t
    | Spred (_, p) -> find_predicate_vars acc p
  in
  let l = List.fold_left find_hyp_vars Vset.empty h in
  find_predicate_vars l p

let generalize_sequent s =
  let l = find_sequent_vars s in
  { scheme_vars = l; scheme_type = s }

let specialize_sequent = specialize_scheme subst_sequent

let rec find_gen_cc_term_vars hole_fun acc t = 
  let find_cc_term_vars = find_gen_cc_term_vars hole_fun in
    match t with
      | CC_var _ ->
	  acc
      | CC_letin (_, bl, t1, t2) ->
	  List.fold_left find_cc_binder_vars
	    (find_cc_term_vars (find_cc_term_vars acc t1) t2) bl
      | CC_lam (b, t) ->
	  find_cc_binder_vars (find_cc_term_vars acc t) b
      | CC_app (t1, t2) ->
	  find_cc_term_vars (find_cc_term_vars acc t1) t2
      | CC_tuple (tl, top) ->
	  List.fold_left find_cc_term_vars
	    (match top with None -> acc | Some t -> find_cc_type_vars acc t) tl
      | CC_if (t1, t2, t3) ->
	  find_cc_term_vars (find_cc_term_vars (find_cc_term_vars acc t1) t2) t3
      | CC_case (t, pl) ->
	  let find_case_vars acc (p, t) =
	    find_cc_pattern_vars (find_cc_term_vars acc t) p
	  in
	    List.fold_left find_case_vars (find_cc_term_vars acc t) pl
      | CC_term t ->
	  find_term_vars acc t
      | CC_hole pr ->
	  hole_fun acc pr
      | CC_type cc
      | CC_any cc ->
	  find_cc_type_vars acc cc

and find_cc_pattern_vars acc = function
  | PPvariable b -> find_cc_binder_vars acc b
  | PPcons (_, pl) -> List.fold_left find_cc_pattern_vars acc pl

let rec find_proof_vars acc = function
  | Lemma _
  | True
  | Proj1 _
  | Proj2 _
  | Assumption _
  | Conjunction _
  | Loop_variant_1 _
  | Absurd _
  | ShouldBeAWp ->
      acc
  | Reflexivity t
  | WfZwf t ->
      find_term_vars acc t
  | ProofTerm cc ->
      find_cc_term_vars acc cc

and find_cc_term_vars x = find_gen_cc_term_vars find_proof_vars x

and find_cc_functional_program_vars = 
  find_gen_cc_term_vars (fun accu (loc, pred) -> find_predicate_vars accu pred)

let rec subst_gen_cc_term subst_hole s t = 
  let subst_cc_term = subst_gen_cc_term subst_hole in
    match t with
      | CC_var _ as cc -> cc
      | CC_letin (b, bl, t1, t2) -> 
	  CC_letin (b, List.map (subst_cc_binder s) bl, 
		    subst_cc_term s t1, subst_cc_term s t2)
      | CC_lam (b, t) -> CC_lam (subst_cc_binder s b, subst_cc_term s t)
      | CC_app (t1, t2) -> CC_app (subst_cc_term s t1, subst_cc_term s t2)
      | CC_tuple (tl, top) -> CC_tuple (List.map (subst_cc_term s) tl,
					option_app (subst_cc_type s) top)
      | CC_if (t1, t2, t3) ->
	  CC_if (subst_cc_term s t1, subst_cc_term s t2, subst_cc_term s t3)
      | CC_case (t, cl) ->
	  let subst_case (p, t) = subst_cc_pattern s p, subst_cc_term s t in
	    CC_case (subst_cc_term s t, List.map subst_case cl)
      | CC_term t -> CC_term (subst_term s t)
      | CC_hole pr -> CC_hole (subst_hole s pr)
      | CC_type t -> CC_type (subst_cc_type s t)
      | CC_any t -> CC_any (subst_cc_type s t)

and subst_cc_pattern s = function
  | PPvariable b -> PPvariable (subst_cc_binder s b)
  | PPcons (id, pl) -> PPcons (id, List.map (subst_cc_pattern s) pl)

let rec subst_proof s = function
  | Lemma _
  | True
  | Proj1 _
  | Proj2 _
  | Assumption _
  | Conjunction _
  | Loop_variant_1 _
  | Absurd _
  | ShouldBeAWp as pr ->
      pr
  | Reflexivity t -> Reflexivity (subst_term s t)
  | WfZwf t -> WfZwf (subst_term s t)
  | ProofTerm cc -> ProofTerm (subst_cc_term s cc)

and subst_cc_term x = subst_gen_cc_term subst_proof x

let subst_cc_functional_program = 
  subst_gen_cc_term (fun s (loc, pred) -> (loc, subst_predicate s pred))

let specialize_cc_type tt = 
  let l = find_cc_type_vars Vset.empty tt in
  specialize_scheme subst_cc_type {scheme_vars=l; scheme_type=tt}

let specialize_validation tt cc =
  let l = find_cc_term_vars (find_cc_type_vars Vset.empty tt) cc in
  if Vset.is_empty l then
    Vmap.empty, tt, cc
  else
    let env = 
      Vset.fold (fun x l -> Vmap.add x (new_type_var()) l) l Vmap.empty 
    in 
    (env, subst_cc_type env tt, subst_cc_term env cc)

let specialize_cc_functional_program tt cc =
  let l = find_cc_functional_program_vars (find_cc_type_vars Vset.empty tt) cc in
  if Vset.is_empty l then
    Vmap.empty, tt, cc
  else
    let env = 
      Vset.fold (fun x l -> Vmap.add x (new_type_var()) l) l Vmap.empty 
    in 
    (env, subst_cc_type env tt, subst_cc_functional_program env cc)

let type_var_names = Hashtbl.create 97
let type_var_name v = Hashtbl.find type_var_names v.tag


(* Environments for imperative programs.
 *
 * An environment of programs is an association tables
 * from identifiers (Ident.t) to types of values with effects
 * (ProgAst.ml_type_v), together with a list of these associations, since
 * the order is relevant (we have dependent types e.g. [x:nat; t:(array x T)])
 *)

module Penv = struct
  type 'a t = {
    map : 'a Idmap.t;
    elements: (Ident.t * 'a) list;
    rec_funs : Idset.t;
    type_vars : (string, type_var) Hashtbl.t
  }
  let empty () = 
    { map = Idmap.empty; elements = []; rec_funs = Idset.empty;
      type_vars = Hashtbl.create 17 }
  let add id v e = 
    { e with map = Idmap.add id v e.map; elements = (id,v)::e.elements }
  let find id e = Idmap.find id e.map
  let mem id e = Idmap.mem id e.map
  let fold f e x0 = List.fold_right f e.elements x0
  let iter f e = List.iter f e.elements
  let add_rec x e = { e with rec_funs = Idset.add x e.rec_funs }
  let is_rec x e = Idset.mem x e.rec_funs
  let find_type_var id e =
    let s = Ident.string id in
    try
      Hashtbl.find e.type_vars s
    with Not_found ->
      let v = new_type_var ~user:true () in
      Hashtbl.add type_var_names v.tag s;
      Hashtbl.add e.type_vars s v;
      v
end

(* The global environment.
 *
 * We have a global typing environment env
 * We also keep a table of programs for extraction purposes
 * and a table of initializations (still for extraction)
 *)

let (env : type_v scheme Penv.t ref) = ref (Penv.empty ())
let (global_refs : pure_type Idmap.t ref) = ref Idmap.empty

(* Local environments *)

type local_env = {
  progs: type_v scheme Penv.t;
  logic: pure_type Idmap.t 
}

(* logical variables *)

let is_logic x env = Idmap.mem x env.logic

let find_logic x env = Idmap.find x env.logic

let add_logic x pt env = 
  { env with logic = Idmap.add x pt env.logic }

(* empty local environment: contains the references as *)

let add_logic_ref id v env = match v with
  | Ref pt -> Idmap.add id pt env
  | PureType _ | Arrow _ -> env

let empty_progs () = 
  { progs = Penv.empty (); logic = !global_refs }

let empty_logic () =
  { progs = Penv.empty (); logic = Idmap.empty }

let add_logic_pure_or_ref id v env = match v with
  | Ref pt | PureType pt -> Idmap.add id pt env
  | Arrow _ -> env

let generalize_type_v t =
  let l = find_type_v_vars Vset.empty t in
  { scheme_vars = l ; scheme_type = t }

let add ?(generalize=false) id v env = 
  let v' = if generalize then generalize_type_v v else empty_scheme v in
  { progs = Penv.add id v' env.progs;
    logic = add_logic_pure_or_ref id v env.logic }

let find_type_var x env = Penv.find_type_var x env.progs

let specialize_type_scheme = specialize_type_v

let find id env =
  let s = Penv.find id env.progs in
  snd (specialize_type_scheme s)

let is_local env id = Penv.mem id env.progs


(* typed programs *)

type typing_info = { 
  t_loc : Loc.position;
  t_env : local_env;
  t_label : label;
  mutable t_userlabel : label;
  t_result_name : Ident.t;
  t_result_type : type_v;
  t_effect : Effect.t;
  t_post : postcondition option 
}
  
type typed_expr = typing_info Ast.t

let (pgm_table : (typed_expr option) Idmap.t ref) = ref Idmap.empty

(* Operations on the global environment. *)

let add_global_gen id v p =
  try
    let _ = Penv.find id !env in
    raise_unlocated (Clash id)
  with Not_found -> begin
    env := Penv.add id v !env; 
    global_refs := add_logic_ref id v.scheme_type !global_refs;
    pgm_table := Idmap.add id p !pgm_table
  end

let add_global id v p =
  let v = generalize_type_v v in
  add_global_gen id v p

let is_global id = Penv.mem id !env

let lookup_global id = 
  let s = Penv.find id !env in
  snd (specialize_type_scheme s)

let iter_global id =  Penv.iter id !env

let find_pgm id = Idmap.find id !pgm_table

(* exceptions *)

let exn_table = Hashtbl.create 97

let add_exception = Hashtbl.add exn_table
let is_exception = Hashtbl.mem exn_table
let find_exception = Hashtbl.find exn_table

(* predefined exception [Exit] *)
let _ = add_exception exit_exn None

(* Logic types with their arities *) 

let types = Hashtbl.create 97

let is_type = Hashtbl.mem types

let bad_arity =
  let rec check s = function
    | [] -> false
    | v :: l -> Idset.mem v s || check (Idset.add v s) l
  in
  check Idset.empty

let add_type loc v id = 
  if is_type id then raise_located loc (ClashType id);
  if bad_arity v then raise_located loc TypeBadArity;
  Hashtbl.add types id (List.length v)

let type_arity = Hashtbl.find types

(* access in env, local then global *)

let type_in_env env id =
  try find id env with Not_found -> lookup_global id

let is_in_env env id =
  (is_global id) || (is_local env id)

let is_ref env id =
  try is_mutable (type_in_env env id) with Not_found -> false

let fold_all f lenv x0 =
  let f (id,s) = f (id,s.scheme_type) in
  let x1 = Penv.fold f !env x0 in
  Penv.fold f lenv.progs x1

let add_rec x env = { env with progs = Penv.add_rec x env.progs }
let is_rec x env = Penv.is_rec x env.progs


let type_v_of_logic tl tr = match tl with
  | [] -> 
      PureType tr
  | _ ->
      let binder pt = Ident.anonymous, PureType pt in
      Arrow (List.map binder tl, type_c_of_v (PureType tr))

(* Logical environment *)

let logic_table = ref (Idmap.empty : logic_type scheme Idmap.t)

let add_global_logic x t = 
  logic_table := Idmap.add x t !logic_table;
  match t.scheme_type with
    | Function (tl, tr) ->
	let v = type_v_of_logic tl tr in
	let v = { scheme_vars = t.scheme_vars ; scheme_type = v } in
	add_global_gen x v None
    | Predicate _ ->
	()

let is_global_logic x = Idmap.mem x !logic_table

let find_global_logic x =
  let t = Idmap.find x !logic_table in
  specialize_logic_type t

let is_global_logic x = Idmap.mem x !logic_table

let is_logic_function x =
  try 
    (match (Idmap.find x !logic_table).scheme_type with 
       | Function _ -> true 
       | _ -> false)
  with Not_found -> 
    false

let iter_global_logic f = Idmap.iter f !logic_table

let add_global_logic_gen x t =
  add_global_logic x (generalize_logic_type t)

(*s Labels *)

module Label = struct

  module LabelSet = Set.Make(struct type t = string let compare = compare end)

  type t = LabelSet.t

  let empty = LabelSet.empty

  let add = LabelSet.add

  let mem = LabelSet.mem

end
