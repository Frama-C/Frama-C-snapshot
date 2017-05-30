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
(* --- Sequent Cleaning                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Lang
open Lang.F

(* Inductive Properties:
    - filter ~polarity:true p ==> p
    - p ==> filter ~polarity:false p
*)
let rec filter ~polarity f p =
  match F.p_expr p with
  | And ps when polarity -> F.p_all (filter ~polarity f) ps
  | Or ps when not polarity -> F.p_any (filter ~polarity f) ps
  | Not p -> F.p_not (filter_inv ~polarity f p)
  | Imply(hs,p) ->
      F.p_hyps
        (List.map (filter_inv ~polarity f) hs)
        (filter ~polarity f p)
  | _ ->
      (* polarity=true: FALSE -> p
         polarity=false: p -> TRUE *)
      if f p then p else if polarity then F.p_false else F.p_true

and filter_inv ~polarity f p = filter ~polarity:(not polarity) f p

(* -------------------------------------------------------------------------- *)
(* --- Usage Domain                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Funs = Qed.Mergeset.Make(Fun)
module Fmap = Qed.Mergemap.Make(Field)
module Imap = Qed.Intmap

type usage =
  | Top | Bot
  | Array of usage
  | Index of usage Imap.t (* Constant map *)
  | Field of usage Fmap.t

module Usage =
struct

  let rec pretty fmt = function
    | Top -> Format.pp_print_string fmt "Top"
    | Bot -> Format.pp_print_string fmt "Bot"
    | Array u -> Format.fprintf fmt "[%a]" pretty u
    | Index m ->
        begin
          Format.fprintf fmt "@[<hov 2>[" ;
          Imap.iteri (fun k u -> Format.fprintf fmt "@ %d:%a" k pretty u) m ;
          Format.fprintf fmt " ]@]" ;
        end
    | Field m ->
        begin
          Format.fprintf fmt "@[<hov 2>{" ;
          Fmap.iter
            (fun fd u -> Format.fprintf fmt "@ %a:%a" Field.pretty fd pretty u)
            m ;
          Format.fprintf fmt " }@]" ;
        end

  let rec join u v =
    match u,v with
    | Top , _ | _ , Top -> Top
    | Bot , w | w , Bot -> w
    | Array u , Array v -> Array(join u v)
    | Index m , Array u | Array u , Index m ->
        Array(Imap.fold (fun u w -> join u w) m u)
    | Index a , Index b ->
        Index(Imap.union (fun _ u v -> join u v) a b)
    | Field a , Field b ->
        Field(Fmap.union (fun _ u v -> join u v) a b)
    | (Index _ | Array _) , Field _
    | Field _ , (Index _ | Array _)
      -> Top

  let meet_array = function Bot -> Bot | u -> Array u
  let meet_filter = function Bot -> None | u -> Some u
  let meet_index m = if Imap.is_empty m then Bot else Index m
  let meet_field m = if Fmap.is_empty m then Bot else Field m

  let rec meet u v =
    match u,v with
    | Top , w | w , Top -> w
    | Bot , _ | _ , Bot -> Bot
    | Array u , Array v -> meet_array (meet u v)
    | Index m , Array v | Array v , Index m ->
        meet_index (Imap.mapf (fun _ u -> meet_filter (meet u v)) m)
    | Index a , Index b ->
        meet_index (Imap.interf (fun _ u v -> meet_filter (meet u v)) a b)
    | Field a , Field b ->
        meet_field (Fmap.interf (fun _ u v -> meet_filter (meet u v)) a b)
  | (Index _ | Array _) , Field _
    | Field _ , (Index _ | Array _)
      -> Bot

  let meetf u v = meet_filter (meet u v)

  let rec leq u v =
    match u , v with
    | Bot , _ -> true
    | _ , Top -> true
    | _ , Bot -> false
    | Array u , Array v -> leq u v
    | Array _ , (Index _ | Field _) -> false
    | Index m , Array v -> Imap.for_all (fun _ u -> leq u v) m
    | Index a , Index b -> Imap.subset (fun _ u v -> leq u v) a b
    | Index _ , Field _ -> false
    | Field _ , (Array _ | Index _) -> false
    | Field a , Field b -> Fmap.subset (fun _ u v -> leq u v) a b
    | Top , _ -> false

end

type domain = { vars : usage Vmap.t ; funs : Funs.t }

module Domain =
struct

  let pretty fmt d =
    begin
      Format.fprintf fmt "@[<hv 0>@[<hv 2>{" ;
      Vmap.iter
        (fun x u -> Format.fprintf fmt "@ @[<hov 2>%a: %a@] ;"
            F.pp_var x Usage.pretty u)
        d.vars ;
      Funs.iter
        (fun f -> Format.fprintf fmt "@ %a ;" Fun.pretty f)
        d.funs ;
      Format.fprintf fmt "@]@ }@]" ;
    end

  let join d1 d2 =
    { vars = Vmap.union (fun _ u v -> Usage.join u v) d1.vars d2.vars ;
      funs = Funs.union d1.funs d2.funs }

  let meet d1 d2 =
    { vars = Vmap.interf (fun _ u v -> Usage.meetf u v) d1.vars d2.vars ;
      funs = Funs.inter d1.funs d2.funs }

  let empty = { vars = Vmap.empty ; funs = Funs.empty }
  let is_empty d = Vmap.is_empty d.vars && Funs.is_empty d.funs

  let separated u v = is_empty (meet u v)

  let leq u v =
    Funs.subset u.funs v.funs &&
    Vmap.subset (fun _ u v -> Usage.leq u v) u.vars v.vars

end

type delta = Darray | Dindex of int | Dfield of Field.t
type value =
  | E
  | X of F.var * delta list
  | D of domain

module Value =
struct

  let rec delta = function
    | [] -> Top
    | Darray :: w -> Array(delta w)
    | Dindex i :: w -> Index(Imap.add i (delta w) Imap.empty)
    | Dfield f :: w -> Field(Fmap.add f (delta w) Fmap.empty)

  let path x ds =
    { vars = Vmap.add x (delta ds) Vmap.empty ; funs = Funs.empty }

  [@@@ warning "-32"]
  let pretty fmt = function
    | E -> Format.pp_print_string fmt "empty"
    | X(x,ds) ->
        Format.fprintf fmt "%a(%a)@." F.pp_var x Usage.pretty (delta ds)
    | D dom -> Domain.pretty fmt dom
  [@@@ warning "+32"]

  let symbol f = { vars = Vmap.empty ; funs = Funs.singleton f }

  let getfield v fd = match v with
    | X(x,ds) -> X(x,ds @ [Dfield fd])
    | D _ | E -> v

  let getindex ~mu v k = match v with
    | X(x,ds) ->
        begin match F.repr k with
          | Kint z ->
              let d =
                try Dindex(Integer.to_int z) with Integer.Too_big -> Darray in
              X( x , ds @ [ d ] )
          | _ ->
              let ds = ds @ [ Darray ] in
              let dk = mu k in
              if Domain.is_empty dk then X(x,ds)
              else D (Domain.join (path x ds) dk)
        end
    | D d -> D (Domain.join d (mu k))
    | E -> D (mu k)

  type env = {
    mutable mvalue : value Tmap.t ;
    mutable mdomain : domain Tmap.t ;
  }

  let create () = { mvalue = Tmap.empty ; mdomain = Tmap.empty }

  let rec compute env e =
    try Tmap.find e env.mvalue with Not_found ->
    let module L = Qed.Logic in
    match F.repr e with
    | L.True | L.False | L.Kint _ | L.Kreal _ -> E
    | _ ->
        let result = match F.repr e with
          | L.Rget( r , f ) -> getfield (compute env r) f
          | L.Aget( r , k ) -> getindex ~mu:(domain env) (compute env r) k
          | L.Fvar x -> X(x,[])
          | L.Fun(f,_) -> D (subterms env (symbol f) e)
          | _ -> D (subterms env Domain.empty e)
        in env.mvalue <- Tmap.add e result env.mvalue ; result

  and subterms env d0 e =
    let pool = ref d0 in
    F.lc_iter
      (fun e -> pool := Domain.join (domain env e) !pool) e ;
    !pool

  and domain env e =
    try Tmap.find e env.mdomain with Not_found ->
    match compute env e with
    | E -> Domain.empty
    | D dom -> dom
    | X(x,ds) ->
        let dom = path x ds in
        env.mdomain <- Tmap.add e dom env.mdomain ; dom

 end

(* -------------------------------------------------------------------------- *)
(* --- Collect                                                            --- *)
(* -------------------------------------------------------------------------- *)

module Fixpoint =
struct

  type env = {
    usage : Value.env ;
    mutable target : domain ;
  }

  let rec collect_hyp env p =
    match F.p_expr p with
    | And ps | Or ps -> List.iter (collect_hyp env) ps
    | Imply(hs,p) ->
        List.iter (collect_hyp env) hs ;
        collect_hyp env p
    | _ ->
        let dp = Value.domain env.usage (F.e_prop p) in
        if not (Domain.separated dp env.target)
        then
          ( env.target <- Domain.join dp env.target )

  let rec collect_seq env s = Conditions.iter (collect_step env) s
  and collect_step env s =
    let open Conditions in
    match s.condition with
    | Type _ | State _ -> ()
    | Core p | Have p | When p | Init p -> collect_hyp env p
    | Either cs -> List.iter (collect_seq env) cs
    | Branch(p,a,b) ->
        begin
          collect_hyp env p ;
          collect_seq env a ;
          collect_seq env b ;
        end

  let rec fixpoint env sequence =
    let d0 = env.target in
    collect_seq env sequence ;
    let d1 = env.target in
    if Domain.leq d1 d0 then d1
    else
      ( fixpoint env sequence )

  let target (sequence,goal) =
    let usage = Value.create () in
    let target = Value.domain usage (F.e_prop goal) in
    if Domain.is_empty target then
      usage , target
    else
      usage , fixpoint { usage ; target } sequence

end

(* -------------------------------------------------------------------------- *)
(* --- Calculus                                                           --- *)
(* -------------------------------------------------------------------------- *)

let compute ?(anti=false) sequent =
  let usage,target = Fixpoint.target sequent in
  let d_accept =
    match Domain.is_empty target , anti with
    | true , false -> Domain.is_empty
    | true , true -> fun d -> not (Domain.is_empty d)
    | false , false -> fun d -> not (Domain.is_empty d) && Domain.leq d target
    | false , true -> fun d -> Domain.is_empty d || not (Domain.leq d target)
  in
  let accept p = d_accept (Value.domain usage (F.e_prop p)) in
  Conditions.map_sequence (filter ~polarity:false accept) (fst sequent) ,
  filter ~polarity:true accept (snd sequent)

(* -------------------------------------------------------------------------- *)
