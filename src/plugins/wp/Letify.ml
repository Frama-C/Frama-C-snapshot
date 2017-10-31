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
(* --- Letification of Goals                                              --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Lang
open Lang.F

let vmem x a = Vars.mem x (F.vars a)
let occurs xs a = Vars.intersect xs (F.vars a)

(* -------------------------------------------------------------------------- *)
(* --- Trivial Simplifications                                            --- *)
(* -------------------------------------------------------------------------- *)

module Ground =
struct

  type subst = pred -> pred
  type env = {
    mutable ground : bool Tmap.t ;
    mutable domain : term Tmap.t ;
  }

  let rec is_ground env e =
    F.is_primitive e ||
    begin
      try Tmap.find e env.ground with Not_found ->
      let r = match F.repr e with
        | Rdef fvs -> List.for_all (fun (_,e) -> is_ground env e) fvs
        | Fun(f,es) ->
            begin match Fun.category f with
              | Constructor -> List.for_all (is_ground env) es
              | _ -> false
            end
        | _ -> false in
      env.ground <- Tmap.add e r env.ground ; r
    end

  let merge a b =
    Tmap.union (fun _ u v -> if F.compare u v <= 0 then u else v) a b

  let clause env h =
    begin
      env.domain <- Tmap.add h F.e_true env.domain ;
      env.domain <- Tmap.add (e_not h) F.e_false env.domain ;
    end

  let frank = function
    | ACSL _ -> 0
    | CTOR _ -> 3
    | Model { m_category = Function } -> 0
    | Model { m_category = Injection } -> 1
    | Model { m_category = Operator _ } -> 2
    | Model { m_category = Constructor } -> 3
  
  let reduce env a b =
    match F.repr a , F.repr b with
    | Fun(f,_) , Fun(g,_) when Wp_parameters.Reduce.get () ->
        let cmp = frank f - frank g in
        if cmp < 0 then env.domain <- Tmap.add a b env.domain ;
        if cmp > 0 then env.domain <- Tmap.add b a env.domain ;
    | _ -> ()
  
  let rec walk env h =
    match F.repr h with
    | True | False -> ()
    | And ps -> List.iter (walk env) ps
    | Eq(a,b) ->
        clause env h ;
        if is_ground env b then
          env.domain <- Tmap.add a b env.domain
        else
        if is_ground env a then
          env.domain <- Tmap.add b a env.domain
        else
          reduce env a b
    | Fun(f,[x]) ->
        begin
          clause env h ;
          try
            let iota = Cint.is_cint f in
            let conv = Cint.convert iota x in
            env.domain <- Tmap.add conv x env.domain ;
          with Not_found -> ()
        end
    | _ ->
        clause env h

  let lookup mu e = Tmap.find e mu
  let subst mu =
    let sigma = F.sigma () in
    F.p_subst ~sigma (lookup mu)

  let e_apply env =
    let sigma = F.sigma () in
    F.e_subst ~sigma (lookup env.domain)
      
  let p_apply env =
    let sigma = F.sigma () in
    F.p_subst ~sigma (lookup env.domain)

  [@@@ warning "-32"]
  let pp_sigma fmt s =
    begin
      Format.fprintf fmt "@[<hov 2>[" ;
      Tmap.iter
        (fun a b -> Format.fprintf fmt "@ %a -> %a ;" F.pp_term a F.pp_term b)
        s ;
      Format.fprintf fmt "]@]" ;
    end
  [@@@ warning "+32"]

  let pretty fmt env = pp_sigma fmt env.domain
  
  let assume env p =
    let p = F.p_subst (lookup env.domain) p in
    walk env (F.e_prop p) ; p

  let top () = { ground = Tmap.empty ; domain = Tmap.empty } 
  let copy env = { domain = env.domain ; ground = env.ground }

  let compute seq =
    let n = Array.length seq in
    let lhs = Array.make n Tmap.empty in
    let rhs = Array.make n Tmap.empty in
    let env = top () in
    for i = 0 to n-2 do
      seq.(i) <- assume env seq.(i) ;
      lhs.(succ i) <- env.domain ;
    done ;
    if n > 1 then
      seq.(n-1) <- assume env seq.(n-1) ;
    let mu = env.domain in
    env.domain <- Tmap.empty ;
    for i = n-1 downto 1 do
      seq.(i) <- assume env seq.(i) ;
      rhs.(pred i) <- env.domain ;
    done ;
    let gs =
      Array.init n
        (fun i ->
           let mu = merge lhs.(i) rhs.(i) in
           subst mu) in
    let g = subst mu in
    gs , g

  let singleton p =
    let env = { domain = Tmap.empty ; ground = Tmap.empty } in
    ignore (assume env p) ;
    subst env.domain

  let branch env p =
    let p = p_apply env p in
    let wa = copy env in
    let wb = copy env in
    ignore (assume wa p) ;
    ignore (assume wb (F.p_not p)) ;
    p , wa , wb

  let forward env p =
    match F.p_expr p with
    | And ps -> F.p_all (assume env) ps
    | _ -> assume env p

  let backward env p =
    match F.p_expr p with
    | And ps -> F.p_all (assume env) (List.rev ps)
    | _ -> assume env p
  
end

(* -------------------------------------------------------------------------- *)
(* --- Generalized Substitution                                           --- *)
(* -------------------------------------------------------------------------- *)

module Sigma :
sig
  type t
  val equal : t -> t -> bool
  val pretty : string -> Format.formatter -> t -> unit

  val empty : t
  val add : var -> term -> t -> t
  val mem : var -> t -> bool
  val find : var -> t -> term
  val e_apply : t -> term -> term
  val p_apply : t -> pred -> pred

  val assume : t -> pred -> t

  val iter : (var -> term -> unit) -> t -> unit
  val class_of : t -> var -> var list
  val domain : t -> Vars.t
  val codomain : t -> Vars.t
end =
struct

  module Ceq = Qed.Partition.Make(Var)(Vars)(Vmap)

  type t = {
    dvar : Vars.t ; (* Domain of def *)
    dcod : Vars.t ; (* Codomain of def *)
    dall : Vars.t ; (* Domain of cst and def *)
    def : term Vmap.t ; (* Definitions *)
    ceq : Ceq.t ; (* Variable Classes *)
    cst : term Tmap.t ; (* Constants *)
    mutable mem : term Tmap.t array ; (* Memoization *)
  }

  let empty = {
    dcod = Vars.empty ;
    dvar = Vars.empty ;
    dall = Vars.empty ;
    ceq = Ceq.empty ;
    def = Vmap.empty ;
    cst = Tmap.empty ;
    mem = Array.make 5 Tmap.empty ;
  }

  let equal s1 s2 =
    Vmap.equal F.equal s1.def s2.def && Tmap.equal F.equal s1.cst s2.cst

  let mem x sigma = Vmap.mem x sigma.def
  let find x sigma = Vmap.find x sigma.def
  let iter f sigma = Vmap.iter f sigma.def

  let rec m_apply sigma n (e:term) =
    match F.repr e with
    | Fvar x ->
        begin
          try Vmap.find x sigma.def
          with Not_found -> e
        end
    | _ ->
        let ys = F.vars e in
        if not (Vars.is_empty ys || Vars.intersect ys sigma.dall)
        then e (* no subst *)
        else if n < 5 then
          begin
            (* memoization *)
            try Tmap.find e sigma.mem.(n)
            with Not_found ->
              let r =
                try
                  if n > 0 then raise Not_found ;
                  Tmap.find e sigma.cst
                with Not_found ->
                  F.QED.f_map (m_apply sigma) n e
              in
              sigma.mem.(n) <- Tmap.add e r sigma.mem.(n) ; r
          end
        else F.QED.f_map (m_apply sigma) n e

  let e_apply sigma e = m_apply sigma 0 e
  let p_apply sigma p = F.p_bool (e_apply sigma (F.e_prop p))

  (* Returns true if [x:=a] applied to [y:=b] raises a circularity *)
  let occur_check sigma x a =
    try
      if vmem x a then raise Exit ;
      Vmap.iter
        (fun y b -> if vmem x b && vmem y a then raise Exit)
        sigma.def ;
      false
    with Exit -> true

  let add_ceq x e ceq =
    match F.repr e with
    | Fvar y -> Ceq.merge ceq x y
    | _ -> ceq

  let single x e =
    let sx = Vars.singleton x in
    {
      dvar = sx ; dall = sx ; dcod = F.vars e ;
      def = Vmap.add x e Vmap.empty ;
      ceq = add_ceq x e Ceq.empty ;
      cst = Tmap.empty ;
      mem = [| Tmap.empty |] ;
    }

  let add x e sigma =
    let e = e_apply sigma e in
    if Vmap.mem x sigma.def then sigma
    else
    if occur_check sigma x e then sigma
    else
      let sx = single x e in
      let def = Vmap.add x e (Vmap.map (fun _ d -> e_apply sx d) sigma.def) in
      let cst0 = Tmap.filter (fun e _c -> not (vmem x e)) sigma.cst in
      let cst1 = Tmap.fold
          (fun e c cst ->
             if vmem x e then Tmap.add (e_apply sx e) c cst else cst)
          cst0 sigma.cst in
      let cache = Array.make (Array.length sigma.mem) Tmap.empty in
      cache.(0) <- cst1 ;
      {
        mem = cache ;
        cst = cst1 ;
        def = def ;
        ceq = add_ceq x e sigma.ceq ;
        dvar = Vars.add x sigma.dvar ;
        dall = Vars.add x sigma.dall ;
        dcod = Vars.union (F.vars e) sigma.dcod ;
      }

  let domain sigma = sigma.dvar
  let codomain sigma = sigma.dcod
  let class_of sigma x = Vars.elements (Ceq.members sigma.ceq x)

  (* --- Constants --- *)

  (* c must be closed *)
  let add_cst e c sigma =
    try
      let c0 = Tmap.find e sigma.cst in
      if compare c c0 < 0 then raise Not_found else sigma
    with Not_found ->
      let cst = Tmap.add e c sigma.cst in
      let all = Vars.union (F.vars e) sigma.dall in
      let cache = Array.make (Array.length sigma.mem) Tmap.empty in
      cache.(0) <- cst ;
      {
        mem = cache ;
        cst = cst ;
        dall = all ;
        dvar = sigma.dvar ;
        dcod = sigma.dcod ;
        def = sigma.def ;
        ceq = sigma.ceq ;
      }

  let mem_lit l sigma =
    try Tmap.find l sigma.mem.(0) == e_true
    with Not_found -> false

  let add_lit l sigma =
    add_cst l e_true (add_cst (e_not l) e_false sigma)


  (** look for the shape:
          \forall x:integer. (csta <= x /\ x <= cstb) => t1=t2
      and return [Some(csta,cstb)]

          < on integer are always normalized to <=
  *)
  let extract_forall_equality fb =
    begin match F.repr (F.QED.lc_repr fb) with
      | Imply ([la;lb],c) ->
          begin match F.repr c with
            | Eq _ ->
                let order = 0 in (** todo get the order from term *)
                begin match F.repr la, F.repr lb with
                  | Leq(a,b), Leq(c,d) ->
                      begin
                        match F.repr a, F.repr b, F.repr c, F.repr d with
                        | Bvar(o1,Int), Kint cstb, Kint csta, Bvar(o2,Int) when
                            o1 = order && o2 = order -> Some(csta,cstb)
                        | Kint csta, Bvar(o1,Int), Bvar(o2,Int), Kint cstb when
                            o1 = order && o2 = order -> Some(csta,cstb)
                        | _ -> None
                      end
                  | _ -> None
                end
            | _ -> None
          end
      | _ -> None
    end

  let is_kint e = match F.repr e with Qed.Logic.Kint _ -> true | _ -> false
  
  let rec add_pred sigma p = match F.repr p with
    | And ps -> List.fold_left add_pred sigma ps
    | Eq(a,b) ->
        begin
          match F.repr a , F.repr b with
          | Fvar x , _ when not (F.occurs x b) -> add x b sigma
          | _ , Fvar x when not (F.occurs x a) -> add x a sigma
          | _ ->
              match F.is_closed a , F.is_closed b with
              | true , false -> add_cst b a sigma
              | false , true -> add_cst a b sigma
              | true , true ->
                  if F.compare a b < 0
                  then add_cst b a sigma
                  else add_cst a b sigma
              | false , false -> add_lit p sigma
        end
    | Leq(a,b) ->
        if mem_lit (e_leq b a) sigma
        then add_pred sigma (e_eq a b)
        else add_lit p sigma
    | Lt(a,b) ->
        let sigma = if is_kint b then add_pred sigma (e_leq a (e_add b e_one)) else sigma in
        let sigma = if is_kint a then add_pred sigma (e_leq (e_sub a e_one) b) else sigma in
        add_lit p (add_lit (e_leq a b) (add_lit (e_neq a b) sigma))
    | Neq _ | Fun _ | Not _ -> add_lit p sigma
    | Bind (Forall,Int,fb) ->
        let bound = Integer.of_int (Wp_parameters.BoundForallUnfolding.get ()) in
        begin match extract_forall_equality fb with
          | Some (csta,cstb) when
              Integer.le csta cstb &&
              Integer.le (Integer.sub cstb csta) bound ->
              let rec aux sigma i =
                if Integer.lt cstb i then sigma
                else begin
                  let eq = F.QED.lc_open_term (e_zint i) fb in
                  (** qed should be able to simplify it directly *)
                  let sigma = add_pred sigma eq in
                  aux sigma (Integer.succ i)
                end
              in
              aux sigma csta
          | _ -> sigma
        end
    | _ -> sigma

  let assume sigma p = add_pred sigma (F.e_prop p)

  (* --- Pretty --- *)

  module Xmap = FCMap.Make(Var)

  let pretty title fmt sigma =
    let def = Vmap.fold Xmap.add sigma.def Xmap.empty in
    begin
      Format.fprintf fmt "@[<hv 0>@[<hv 2>%s {" title ;
      Format.fprintf fmt "@ @[vars: %a;@]" F.pp_vars sigma.dall ;
      Xmap.iter
        (fun x e ->
           Format.fprintf fmt "@ @[%a := %a ;@]"
             F.pp_term (F.e_var x) F.pp_term e
        ) def ;
      Array.iteri
        (fun i w ->
           Tmap.iter
             (fun e m ->
                Format.fprintf fmt "@ C%d: @[%a := %a ;@]" i
                  F.pp_term e F.pp_term m
             ) w
        ) sigma.mem ;
      Format.fprintf fmt "@ @]}@]" ;
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Definition Extractions                                             --- *)
(* -------------------------------------------------------------------------- *)

module Defs =
struct
  type t = Tset.t Vmap.t

  let empty = Vmap.empty
  let merge = Vmap.union (fun _ -> Tset.union)

  let add_def (w : t ref) x e =
    let es = try Vmap.find x !w with Not_found -> Tset.empty in
    w := Vmap.add x (Tset.add e es) !w

  let rec diff s y = function
    | [] -> s
    | e::es ->
        match F.repr e with
        | Fvar x when x==y -> diff s y es
        | _ -> diff (e_opp e :: s) y es

  let add_linear w x pos neg =
    add_def w x (e_sum (diff pos x neg))

  let terms e = match F.repr e with Add es -> es | _ -> [e]
  let rec atoms = function
    | [] -> []
    | e::es ->
        match F.repr e with
        | Fvar x -> x :: atoms es
        | _ -> atoms es

  let rec defs w p =
    match F.repr p with
    | And ps -> List.iter (defs w) ps
    | Eq(a,b) -> defs_eq w a b
    | Not p ->
        begin
          match F.repr p with
          | Fvar x -> add_def w x e_false
          | _ -> ()
        end
    | Fvar x -> add_def w x e_true
    | _ -> ()

  and defs_affine w a b =
    let ta = terms a in
    let tb = terms b in
    let xa = atoms ta in
    let yb = atoms tb in
    begin
      List.iter (fun x -> add_linear w x tb ta) xa ;
      List.iter (fun y -> add_linear w y ta tb) yb ;
    end

  and defs_eq w a b =
    match F.repr a , F.repr b with
    | Add _ , _ | _ , Add _ -> defs_affine w a b
    | Fvar x , Fvar y -> add_def w x b ; add_def w y a
    | Fvar x , _ -> add_def w x b
    | _ , Fvar y -> add_def w y a
    | _ -> ()

  let extract p =
    let w = ref empty in
    defs w (F.e_prop p) ; !w

  let add w p = defs w (F.e_prop p)

  let domain d =
    Vmap.fold (fun x _ xs -> Vars.add x xs) d Vars.empty

end

(* -------------------------------------------------------------------------- *)
(* --- Substitution Extraction                                            --- *)
(* -------------------------------------------------------------------------- *)

module XS = FCSet.Make(Var)

let elements xs = Vars.fold XS.add xs XS.empty
let iter f xs = XS.iter f (elements xs)

let rec extract defs sref cycle x =
  if not (Vars.mem x cycle) && not (Sigma.mem x !sref) then
    try
      let cycle = Vars.add x cycle in
      let ds = Vmap.find x defs in (* if no defs, exit early *)
      let ys = ref [] in (* variables equal to x *)
      let es = ref [] in (* possible definitions *)
      let rs = ref [] in (* sigma definitions *)
      Tset.iter
        (fun e ->
           if not (occurs cycle e) then
             match F.repr e with
             | Fvar y ->
                 begin
                   try let d = Sigma.find y !sref in rs := d :: !rs
                   with Not_found -> ys := y :: !ys
                 end
             | _ -> es := e :: !es
        ) ds ;
      (* Now choose the represent of x and the dependencies *)
      let select d = sref := Sigma.add x d !sref ; d , F.vars d in
      let ceq , depends =
        match List.sort F.compare !rs with
        | r :: _ -> select r
        | [] -> match List.sort F.compare !es with
          | e :: _ -> select e
          | [] -> e_var x , Vars.empty
      in
      List.iter (fun y -> sref := Sigma.add y ceq !sref) !ys ;
      iter (extract defs sref cycle) depends
    with Not_found -> ()

let bind sigma defs xs =
  let sref = ref sigma in
  iter (extract defs sref Vars.empty) xs ;
  !sref

let get_class sigma xs x =
  List.sort Var.compare
    (List.filter (fun y -> Vars.mem y xs) (Sigma.class_of sigma x))

let rec add_eq ps y = function
  | z::zs -> add_eq (p_equal (e_var y) (e_var z) :: ps) y zs
  | [] -> ps

let add_equals ys ps =
  match ys with [] -> ps | y::ys -> add_eq ps y ys

let add_definitions sigma defs xs ps =
  let xs = Vars.filter (fun x -> Vmap.mem x defs) xs in
  Vars.fold
    (fun x ps ->
       let ps = add_equals (get_class sigma xs x) ps in
       try F.p_equal (e_var x) (Sigma.find x sigma) :: ps
       with Not_found -> ps
    ) xs ps

(* -------------------------------------------------------------------------- *)
(* --- Split-Cases                                                        --- *)
(* -------------------------------------------------------------------------- *)

module Split =
struct

  type occur = int F.Tmap.t ref

  let create () = ref Tmap.empty

  let literal m p =
    try
      let n = Tmap.find p !m in
      m := Tmap.add p (succ n) !m
    with Not_found ->
      m := Tmap.add p 1 !m

  let rec occur m p =
    match F.repr p with
    | And ps | Or ps -> List.iter (occur m) ps
    | Imply(hs,p) -> List.iter (occur m) (p::hs)
    | Not p -> occur m p
    | If(p,a,b) -> occur m p ; occur m a ; occur m b
    | Eq(a,b) when F.is_closed a || F.is_closed b -> literal m p
    | Neq(a,b) when F.is_closed a || F.is_closed b -> literal m (e_not p)
    | Fun _ | Leq _ -> literal m p
    | Lt _ -> literal m (e_not p)
    | _ -> ()

  let add m p = occur m (F.e_prop p)

  let select m =
    let compare (c1,n1) (c2,n2) =
      (* most often first *)
      if n1 < n2 then 1 else
      if n1 > n2 then (-1) else
        F.comparep c1 c2
    in
    List.sort compare (Tmap.fold (fun c n s -> (F.p_bool c,n)::s) !m [])

end

(* -------------------------------------------------------------------------- *)
