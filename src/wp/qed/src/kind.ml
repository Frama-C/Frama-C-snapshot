(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* --- Tau & Sort Manipulations                                           --- *)
(* -------------------------------------------------------------------------- *)

open Logic

let rec of_poly alpha = function
  | Prop -> Sprop
  | Bool -> Sbool
  | Int -> Sint
  | Real -> Sreal
  | Tvar x -> alpha x
  | Data _ -> Sdata
  | Array(_,d) -> Sarray (of_poly alpha d)
  | Record _ -> Sdata

let of_tau t = of_poly (fun _ -> Sdata) t

let rec merge a b =
  match a,b with
  | Sprop , _ | _ , Sprop -> Sprop
  | Sbool , _ | _ , Sbool -> Sbool
  | Sarray x , Sarray y -> Sarray (merge x y)
  | Sarray _ , _ | _ , Sarray _ -> Sdata
  | Sint , Sreal | Sreal , Sint -> Sreal
  | Sint , Sint -> Sint
  | Sreal , Sreal -> Sreal
  | Sdata , _ | _ , Sdata -> Sdata

let image = function Sarray s -> s | _ -> Sdata

let rec merge_list f s = function
  | [] -> s
  | x::xs ->
      if s = Sprop then Sprop 
      else merge_list f (merge s (f x)) xs

let pretty fmt = function
  | Sprop -> Format.pp_print_string fmt "Prop"
  | Sbool -> Format.pp_print_string fmt "Bool"
  | Sdata -> Format.pp_print_string fmt "Term"
  | Sint -> Format.pp_print_string fmt "Int"
  | Sreal -> Format.pp_print_string fmt "Real"
  | Sarray _ -> Format.pp_print_string fmt "Array"

let basename = function
  | Sprop | Sbool -> "P"
  | Sdata -> "a"
  | Sint  -> "x"
  | Sreal -> "r"
  | Sarray _ -> "m"

let rec degree_of_tau = function
  | Tvar n -> n
  | Int | Real | Bool | Prop -> 0
  | Data(_,ts) -> degree_of_list ts
  | Array(a,b) -> max (degree_of_tau a) (degree_of_tau b)
  | Record fts -> 
      List.fold_left
        (fun r (_,t) -> max r (degree_of_tau t)) 0 fts

and degree_of_list = function
  | [] -> 0
  | t::ts -> max (degree_of_tau t) (degree_of_list ts)

and degree_of_sig f = max (degree_of_tau f.result) (degree_of_list f.params)

let rec tmap xs = function
  | Int -> Int
  | Real -> Real
  | Bool -> Bool
  | Prop -> Prop
  | Tvar k -> xs.(k-1)
  | Array(a,b) -> Array(tmap xs a,tmap xs b)
  | Data(a,ts) -> Data(a,List.map (tmap xs) ts)
  | Record fts -> Record(List.map (fun (f,t) -> f,tmap xs t) fts)

let type_params n =
  let rec vars k n = if k <= n then Tvar k :: vars (succ k) n else [] 
  in vars 1 n

let pp_data pdata ptau fmt a = function
  | [] -> pdata fmt a
  | [t] -> Format.fprintf fmt "%a %a" ptau t pdata a
  | t::ts -> 
      Format.fprintf fmt "@[(@[<hov 2>%a" ptau t ;
      List.iter
        (fun t -> Format.fprintf fmt ",@,%a" ptau t) ts ;
      Format.fprintf fmt ")@]@ %a@]" pdata a

let pp_record pfield ptau fmt ?(opened=false) fts =
  Format.fprintf fmt "@[<hv 0>{@[<hv 2>" ;
  List.iter
    (fun (f,t) -> Format.fprintf fmt "@ @[<hov 2>%a : %a ;@]" pfield f ptau t)
    fts ;
  if opened then Format.fprintf fmt "@ ..." ;
  Format.fprintf fmt "@]@ }@]"

let rec pp_tau pvar pfield pdata fmt = function
  | Int -> Format.pp_print_string fmt "int"
  | Real -> Format.pp_print_string fmt "real"
  | Bool -> Format.pp_print_string fmt "bool"
  | Prop -> Format.pp_print_string fmt "prop"
  | Tvar x -> pvar fmt x
  | Array(Int,te) -> 
      Format.fprintf fmt "%a[]" (pp_tau pvar pfield pdata) te
  | Array(tk,te) ->
      Format.fprintf fmt "%a[%a]" 
        (pp_tau pvar pfield pdata) te (pp_tau pvar pfield pdata) tk
  | Data(a,ts) -> pp_data pdata (pp_tau pvar pfield pdata) fmt a ts
  | Record fts -> pp_record pfield (pp_tau pvar pfield pdata) fmt fts

let rec eq_tau cfield cadt t1 t2 =
  match t1 , t2 with
  | (Bool|Int|Real|Prop|Tvar _) , (Bool|Int|Real|Prop|Tvar _) -> t1 = t2
  | Array(ta,tb) , Array(ta',tb') -> 
      eq_tau cfield cadt ta ta' && eq_tau cfield cadt tb tb'
  | Array _ , _  | _ , Array _ -> false
  | Data(a,ts) , Data(b,ts') ->
      cadt a b && Hcons.equal_list (eq_tau cfield cadt) ts ts'
  | Data _ , _ | _ , Data _ -> false
  | Record fts , Record gts ->
      Hcons.equal_list
        (fun (f,t) (g,t') -> cfield f g && eq_tau cfield cadt t t')
        fts gts
  | Record _ , _ | _ , Record _ -> false

let rec compare_tau cfield cadt t1 t2 =
  match t1 , t2 with
  | Bool , Bool -> 0
  | Bool , _ -> (-1)
  | _ , Bool -> 1
  | Int , Int -> 0
  | Int , _ -> (-1)
  | _ , Int -> 1
  | Real , Real -> 0
  | Real , _ -> (-1)
  | _ , Real -> 1
  | Prop , Prop -> 0
  | Prop , _ -> (-1)
  | _ , Prop -> 1
  | Tvar k , Tvar k' -> Pervasives.compare k k'
  | Tvar _ , _ -> (-1)
  | _ , Tvar _ -> 1
  | Array(ta,tb) , Array(ta',tb') -> 
      let c = compare_tau cfield cadt ta ta' in
      if c = 0 then compare_tau cfield cadt tb tb' else c
  | Array _ , _ -> (-1)
  | _ , Array _ -> 1
  | Data(a,ts) , Data(b,ts') ->
      let c = cadt a b in
      if c = 0 then Hcons.compare_list (compare_tau cfield cadt) ts ts' else c
  | Data _ , _ -> (-1)
  | _ , Data _ -> 1
  | Record fts , Record gts ->
      Hcons.compare_list
        (fun (f,t) (g,t') ->
           let c = cfield f g in
           if c = 0 then compare_tau cfield cadt t t' else c
        ) fts gts
