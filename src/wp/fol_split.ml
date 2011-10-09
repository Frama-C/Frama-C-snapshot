(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

let dkey = "split" (* Debugging key *)

(* ------------------------------------------------------------------------ *)
(* ---  Splitter for FOL                                                --- *)
(* ------------------------------------------------------------------------ *)

open Fol


module Env = Map.Make(Fol.Var)

type term = Fol.term
type pred = Fol.pred

let is_zone = function
  | Tapp(s,_) -> List.mem s
      [ "zunion" ; "zempty" ; "zrange" ;
        "zrange_of_addr" ; "zrange_of_addr_range" ;
        "zs_empty" ; "zs_singleton" ; "zs_union" ;
      ]
  | _ -> false

let rec e_unfold env = function
  | Tvar x as t -> (try Env.find x env with Not_found -> t)
  | Tlet(x,a,b) ->
      let a0 = e_unfold env a in
      if is_zone a0
      then e_unfold (Env.add x a0 env) b
      else e_let x a0 (e_unfold (Env.remove x env) b)
  | Tapp(f,xs) -> e_app f (List.map (e_unfold env) xs)
  | Taccess(t,i) -> e_access (e_unfold env t) (e_unfold env i)
  | Tupdate(t,i,v) -> e_update (e_unfold env t) ( e_unfold env i) (e_unfold env v)
  | Tgetfield(f,r) -> e_getfield f (e_unfold env r)
  | Tsetfield(f,r,v) -> e_setfield f (e_unfold env r) (e_unfold env v)
  | Tif(c,a,b) -> e_if (e_unfold env c) (e_unfold env a) (e_unfold env b)
  | Tconst _ as t -> t

let rec p_unfold env = function
  | Plet(x,t,p) ->
      let t0 = e_unfold env t in
      if is_zone t0
      then p_unfold (Env.add x t0 env) p
      else p_let x t0 (p_unfold (Env.remove x env) p)
  | Pnamed(a,p) -> Pnamed(a,p_unfold env p)
  | (Ptrue | Pfalse) as p -> p
  | Papp(f,es) -> p_app f (List.map (e_unfold env) es)
  | Pimplies(p,q) -> p_implies (p_unfold env p) (p_unfold env q)
  | Pif(c,p,q) -> p_if (e_unfold env c) (p_unfold env p) (p_unfold env q)
  | Pand(p,q) -> p_and (p_unfold env p) (p_unfold env q)
  | Por(p,q) -> p_or (p_unfold env p) (p_unfold env q)
  | Piff(p,q) -> p_iff (p_unfold env p) (p_unfold env q)
  | Pnot p -> p_not (p_unfold env p)
  | Pforall(x,p) -> p_forall x (p_unfold env p)
  | Pexists(x,p) -> p_exists x (p_unfold env p)

(* ------------------------------------------------------------------------ *)
(* --- Case Split                                                       --- *)
(* ------------------------------------------------------------------------ *)

module SplitZoneInclusion (M :
  sig
    val union : string (* symbol for union *)
    val empty : string (* symbol for empty *)
    val included : string (* symbol for inclusion *)
  end)
  =
struct

  let rec flatten xs = function
    | Tapp( f , es ) when f = M.union -> List.fold_left flatten xs es
    | Tapp( f , [] ) when f = M.empty -> xs
    | t -> t::xs

  let rec zunion = function
    | [z] -> z
    | [] -> e_app M.empty []
    | z::zs -> e_app M.union [ z ; zunion zs ]

  let included a b =
    let zas = flatten [] a in
    let zbs = flatten [] b in
    let zb = zunion zbs in
    List.fold_left
      (fun acc z ->
         if Wp_parameters.Simpl.get () && List.exists (Fol.eq_terms z) zbs
         then acc
         else Bag.add (p_app M.included [z;zb]) acc)
      Bag.empty zas

end

module StoreInclusion = SplitZoneInclusion
  (struct
     let union = "zunion"
     let empty = "zempty"
     let included = "included"
   end)

module RuntimeInclusion = SplitZoneInclusion
  (struct
     let union = "zs_union"
     let empty = "zs_empty"
     let included = "zs_incl"
   end)

(* Do not bound the depth when [max_depth]=0,
   otherwise, the spliting is pruned and [max_split] bound may be not reached. *)
let dispatch max_depth max_split p =
  let rec nb_splits = ref 1

  and concat depth pol orig d1 b1 d2 b2 =
    let rec incr_depth kid =
      (* sometime, depth is not incremented *)
      match orig, kid with
        | _, Pnamed(_,p) -> incr_depth p
        | Por _, Por _ -> depth
        | Pand _, Pand _ -> depth
        | _, _ -> depth + 1
    in
      if (!nb_splits >= max_split) || ((depth >= max_depth) && (max_depth <> 0))
      then Bag.elt (if pol then orig else (p_not orig))
      else (incr nb_splits ; Bag.concat (d1 (incr_depth b1) b1) (d2 (incr_depth b2) b2))

  and choose p bag =
    let nb = !nb_splits + (Bag.length bag) - 1
    in if nb >= max_split
    then Bag.elt p
    else (nb_splits := nb ; bag)

  and concat_if depth pol orig d c p q =
    if (!nb_splits >= max_split) || ((depth >= max_depth) && (max_depth <> 0))
    then Bag.elt (if pol then orig else (p_not orig))
    else (incr nb_splits ;
          Bag.concat
            (Bag.map (fun p -> p_implies (p_eq c e_true)  p) (d (depth+1) p))
            (Bag.map (fun q -> p_implies (p_eq c e_false) q) (d (depth+1) q)))

  and dispatch_neg depth = function
    | Pfalse -> Bag.empty
    | Pnot p -> dispatch_pos depth p
    | (Ptrue|Pand _|Piff _|Papp _ |Pforall _) as p -> Bag.elt (p_not p)
    | Por(p,q) as full ->      concat depth false full dispatch_neg p dispatch_neg q
    | Pimplies(h,p)as full ->  concat depth false full dispatch_pos h dispatch_neg p
    | Pif(c,p,q) as full -> concat_if depth false full dispatch_neg c p q
    | Pnamed(a,p) ->  Bag.map (fun p -> Pnamed(a,p)) (dispatch_neg depth p)
    | Pexists(x,p) -> Bag.map (p_forall x) (dispatch_neg depth p)
    | Plet(x,t,p) ->  Bag.map (p_let x t)  (dispatch_neg depth p)

  and dispatch_pos depth = function
    | Ptrue -> Bag.empty
    | Pnot p -> dispatch_neg depth p
    | Papp( "included" , [a;b]) as p -> choose p (StoreInclusion.included a b)
    | Papp( "zs_incl" , [a;b]) as p -> choose p (RuntimeInclusion.included a b)

    | (Pfalse|Por _|Piff _|Papp _ |Pexists _) as p -> Bag.elt p
    | Pimplies(h,p) -> Bag.map (fun p -> p_implies h p) (dispatch_pos depth p)
    | Pand(p,q) as full ->     concat depth true full dispatch_pos p dispatch_pos q
    | Pif(c,p,q) as full -> concat_if depth true full dispatch_pos c p q
    | Pnamed(a,p) ->  Bag.map (fun p -> Pnamed(a,p)) (dispatch_pos depth p)
    | Pforall(x,p) -> Bag.map (p_forall x) (dispatch_pos depth p)
    | Plet(x,t,p) ->  Bag.map (p_let x t)  (dispatch_pos depth p)
  in

    dispatch_pos 1 p

(* ------------------------------------------------------------------------ *)
(* --- Splitter Interface                                               --- *)
(* ------------------------------------------------------------------------ *)

let simplify = Fol_let.compile

(* [split] may deliver stronger sub-predicates *)
let split meth p =
  let nb = Wp_parameters.SplitDim.get()
  in let max_depth = if nb < 0 then 0 (* <- do not prune the search *) else (2 + nb)
     and max_split = (* 2**|nb| *)
      try
        Big_int.int_of_big_int (Big_int.power_int_positive_int 2 (if nb < 0 then -nb else nb))
      with (Invalid_argument _ | Failure _) ->
        Wp_parameters.debug ~dkey "Invalid value for option %s@." Wp_parameters.SplitDim.name;
        0 (* <- do not split *)
  in
  let bag =
    match meth with
      | Mcfg.EffectAssigns -> dispatch max_depth max_split (p_unfold Env.empty p)
      | _ -> dispatch max_depth max_split p
  in let nb = Bag.length bag
      (* TODO:
         if the bag is not full and [max_depth]<>0
         then it could be possible to iter once more
         on the elements of the bag with [max_depth]=0 *)
  in if nb > 1 then
      Wp_parameters.debug ~dkey "Predicate splited into %d parts@." nb;
    bag
