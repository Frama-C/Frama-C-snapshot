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

open Qed
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Logical Sets                                                       --- *)
(* -------------------------------------------------------------------------- *)

type set = vset list
and vset =
  | Set of tau * term
  | Singleton of term
  | Range of term option * term option
  | Descr of var list * term * pred

let occurs_opt x = function
  | None -> false
  | Some t -> occurs x t

let occurs_vset x = function
  | Set(_,t)
  | Singleton t -> occurs x t
  | Range(a,b) -> occurs_opt x a || occurs_opt x b
  | Descr(xs,t,p) ->
      if List.exists (Var.equal x) xs then false
      else (occurs x t || occursp x p)

let occurs x = List.exists (occurs_vset x)

let vars_opt = function None -> Vars.empty | Some e -> F.vars e

let vars_vset = function
  | Set(_,t) -> F.vars t
  | Singleton t -> F.vars t
  | Range(a,b) -> Vars.union (vars_opt a) (vars_opt b)
  | Descr(xs,t,p) ->
      List.fold_left
        (fun xs x -> Vars.remove x xs)
        (Vars.union (F.vars t) (F.varsp p)) xs

let vars vset = List.fold_left
    (fun xs s -> Vars.union xs (vars_vset s))
    Vars.empty vset

(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

let pp_bound fmt = function
  | None -> ()
  | Some e -> F.pp_term fmt e

let pp_vset fmt = function
  | Set(_,t) -> F.pp_term fmt t
  | Singleton x -> Format.fprintf fmt "@[<hov 2>{ %a }@]" F.pp_term x
  | Range(None,None) -> Format.pp_print_string fmt "[..]"
  | Range(a,b) -> Format.fprintf fmt "@[<hov 2>[%a@,..%a]@]" pp_bound a pp_bound b
  | Descr _ -> Format.fprintf fmt "{ <comprehension> }"

let pretty fmt = function
  | [] -> Format.pp_print_string fmt "{}"
  | [v] -> pp_vset fmt v
  | v::vs ->
      Format.fprintf fmt "@[<hov 2>(%a" pp_vset v ;
      List.iter (fun v -> Format.fprintf fmt "@ + %a" pp_vset v) vs ;
      Format.fprintf fmt ")@]"

(* -------------------------------------------------------------------------- *)
(* --- Set Operations                                                     --- *)
(* -------------------------------------------------------------------------- *)

let library = "vset"

let adt_set = Lang.datatype ~library "set"
let tau_of_set te = Logic.Data( adt_set , [te] )
let p_member = Lang.extern_p ~library ~bool:"member_bool" ~prop:"member" ()
let f_empty = Lang.extern_f ~library "empty"
let f_union = Lang.extern_f ~library "union"
let f_inter = Lang.extern_f ~library "inter"
let f_range = Lang.extern_f ~library "range"
let f_range_sup = Lang.extern_f ~library "range_sup"
let f_range_inf = Lang.extern_f ~library "range_inf"
let f_range_all = Lang.extern_f ~library "range_all"
let f_singleton = Lang.extern_f ~library "singleton"

let single a b = match a,b with
  | Some x , Some y when F.QED.equal x y -> a
  | _ -> None

let test_range x y a b =
  let p_inf = match a with Some a -> p_leq a x | None -> p_true in
  let p_sup = match b with Some b -> p_leq y b | None -> p_true in
  p_and p_inf p_sup

let sub_range x y a b =
  match single a b with
  | Some z -> p_and (p_equal x z) (p_equal y z)
  | None -> test_range x y a b

let in_size x n = p_and (p_leq e_zero x) (p_lt x (e_int n))

let in_range x a b =
  match single a b with
  | Some y -> p_equal x y
  | None -> test_range x x a b

let ordered ~limit ~strict a b =
  match a , b with
  | Some x , Some y -> if strict then p_lt x y else p_leq x y
  | _ -> if limit then p_true else p_false

let member x xs = p_any
    (function
      | Set(_,s) -> p_call p_member [x;s]
      | Singleton e -> p_equal x e
      | Range(a,b) -> in_range x a b
      | Descr(xs,t,p) -> p_exists xs (p_and (p_equal x t) p)
    ) xs

let empty = []
let singleton x = [Singleton x]
let range a b = [Range(a,b)]

let union xs ys = (xs @ ys)

let descr = function
  | Set(t,s) ->
      let x = Lang.freshvar t in
      let e = e_var x in
      [x] , e , p_call p_member [e;s]
  | Singleton e -> ( [] , e , p_true )
  | Range(a,b) ->
      let x = Lang.freshvar ~basename:"k" Logic.Int in
      let e = e_var x in
      [x] , e , in_range e a b
  | Descr(xs,t,p) ->
      xs, t, p

(* -------------------------------------------------------------------------- *)
(* --- Concretize                                                         --- *)
(* -------------------------------------------------------------------------- *)

let concretize_vset = function
  | Set(_,s) -> s
  | Singleton e -> e_fun f_singleton [e]
  | Range(None,None) -> e_fun f_range_all []
  | Range(None,Some b) -> e_fun f_range_inf [b]
  | Range(Some a,None) -> e_fun f_range_sup [a]
  | Range(Some a,Some b) -> e_fun f_range [a;b]
  | Descr _ ->
      Warning.error "Concretization for comprehension sets not implemented yet"

let concretize = function
  | [] -> e_fun f_empty []
  | x::xs ->
      List.fold_left
        (fun w x -> e_fun f_union [w;concretize_vset x])
        (concretize_vset x) xs

let inter xs ys = e_fun f_inter [xs;ys]

(* -------------------------------------------------------------------------- *)
(* --- Inclusion                                                          --- *)
(* -------------------------------------------------------------------------- *)

let subrange a b = function
  | [Range(c,d)] ->
      p_and
        (match c,a with
         | None,_ -> p_true
         | Some _,None -> p_false
         | Some c,Some a -> p_leq c a)
        (match b,d with
         | _,None -> p_true
         | None,Some _ -> p_false
         | Some b,Some d -> p_leq b d)
  | ys ->
      let x = Lang.freshvar ~basename:"k" Logic.Int in
      let k = e_var x in
      p_forall [x] (p_imply (in_range k a b) (member k ys))

let subset xs ys =
  p_all (function
      | Set(t,s) ->
          let x = Lang.freshvar t in
          let e = e_var x in
          p_forall [x] (p_imply (p_call p_member [e;s]) (member e ys))
      | Singleton e -> member e ys
      | Descr(xs,t,p) ->
          p_forall xs (p_imply p (member t ys))
      | Range(a,b) ->
          subrange a b ys
    ) xs

(* -------------------------------------------------------------------------- *)
(* --- Equality                                                           --- *)
(* -------------------------------------------------------------------------- *)

let equal xs ys =
  p_and (subset xs ys) (subset ys xs)

(* -------------------------------------------------------------------------- *)
(* --- Separation                                                         --- *)
(* -------------------------------------------------------------------------- *)

let empty_range a b =
  match a,b with
  | None,_ | _,None -> p_false
  | Some x , Some y -> p_lt y x

let disjoint_bounds left right =
  match left , right with
  | None,_ | _,None -> p_false
  | Some x , Some y -> p_lt x y

let disjoint_vset x y =
  match x , y with

  | Singleton x , Singleton y ->
      p_neq x y

  | Singleton e , Range(a,b)
  | Range(a,b) , Singleton e ->
      p_not (in_range e a b)

  | Range(a,b) , Range(c,d) ->
      p_disj [
        empty_range a b ;
        empty_range c d ;
        disjoint_bounds b c ;
        disjoint_bounds d a ;
      ]

  | Singleton x , Descr(xs,t,p)
  | Descr(xs,t,p) , Singleton x ->
      p_forall xs (p_imply p (p_neq x t))

  | Range(a,b) , Descr(xs,t,p)
  | Descr(xs,t,p) , Range(a,b) ->
      p_forall xs (p_imply p (p_not (in_range t a b)))

  | Descr(xs,ta,pa) , Descr(ys,tb,pb) ->
      p_forall xs
        (p_forall ys
           (p_hyps [pa;pb] (p_neq ta tb)))

  | Singleton e , Set(_,s)
  | Set(_,s) , Singleton e ->
      p_not (p_call p_member [e;s])

  | Set _ , Set _ ->
      let xs,a,p = descr x in
      let ys,b,q = descr y in
      p_forall (xs @ ys) (p_hyps [p;q] (p_neq a b))

  | Set(_,s) , w | w , Set(_,s) ->
      let xs,t,p = descr w in
      let t_in_s = p_call p_member [t;s] in
      p_forall xs (p_not (p_and p t_in_s))

let disjoint xs ys =
  let ws =
    List.fold_left
      (fun w x ->
         List.fold_left
           (fun w y -> disjoint_vset x y :: w) w ys
      ) [] xs
  in p_conj ws

(* -------------------------------------------------------------------------- *)
(* --- Lifting & Mapping                                                  --- *)
(* -------------------------------------------------------------------------- *)

let cartesian f xs ys =
  let zs =
    List.fold_left
      (fun w x ->
         List.fold_left (fun w y -> f x y :: w) w ys
      ) [] xs
  in List.rev zs

let map_vset f x = let xs,t,p = descr x in Descr(xs,f t,p)

let map f xs = List.map
    (function Singleton x -> Singleton (f x) | u -> map_vset f u) xs

let map_opt f = function None -> None | Some x -> Some (f x)

let map_opp xs = List.map
    (function
      | Singleton x -> Singleton (e_opp x)
      | Range(a,b) -> Range(map_opt e_opp b,map_opt e_opp a)
      | Descr(xs,t,p) -> Descr(xs,e_opp t,p)
      | (Set _) as w -> let xs,t,p = descr w in Descr(xs,e_opp t,p)
    ) xs

let lift_vset f x y =
  let xs,ta,pa = descr x in
  let ys,tb,pb = descr y in
  Descr (xs @ ys , f ta tb , p_and pa pb)

let lift f xs ys =
  cartesian
    (fun x y ->
       match x , y with
       | Singleton a , Singleton b -> Singleton (f a b)
       | _ -> lift_vset f x y
    ) xs ys

let bound_shift a k =
  match a with
  | None -> None
  | Some x -> Some (e_add x k)

let bound_add a b =
  match a,b with
  | None,_ | _,None -> None
  | Some x , Some y -> Some (e_add x y)

let bound_sub a b =
  match a,b with
  | None,_ | _,None -> None
  | Some x , Some y -> Some (e_sub x y)

let lift_add xs ys =
  cartesian
    (fun x y ->
       match x , y with
       | Singleton a , Singleton b -> Singleton(e_add a b)
       | Singleton u , Range(a,b) | Range(a,b) , Singleton u ->
           Range(map_opt (e_add u) a, map_opt (e_add u) b)
       | Range(a,b) , Range(c,d) ->
           Range(bound_add a c,bound_add b d)
       | _ -> lift_vset e_add x y
    ) xs ys

let lift_sub xs ys =
  cartesian
    (fun x y ->
       match x , y with
       | Singleton a , Singleton b -> Singleton(e_sub a b)
       | Singleton u , Range(a,b) ->
           Range(bound_sub (Some u) b , bound_sub (Some u) a)
       | Range(a,b) , Singleton u ->
           Range(bound_sub a (Some u) , bound_sub b (Some u))
       | Range(a,b) , Range(c,d) ->
           Range(bound_sub a d , bound_sub b c)
       | _ -> lift_vset e_sub x y
    ) xs ys

(* -------------------------------------------------------------------------- *)
