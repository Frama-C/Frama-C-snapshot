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

(* -------------------------------------------------------------------------- *)
(* --- Logical Language                                                   --- *)
(* -------------------------------------------------------------------------- *)

open LogicId

(* -------------------------------------------------------------------------- *)
(* --- Types                                                              --- *)
(* -------------------------------------------------------------------------- *)

type tau =
  | Integer
  | Real
  | Boolean
  | Pointer
  | Set of tau
  | Array of tau * tau
  | Record of id
  | ADT of id * tau list
  | ALPHA of int

type field = {
  f_record : id ;
  f_name : id ;
  f_type : tau ;
}

let rec compare_tau t1 t2 =
  match t1 , t2 with

    | Integer , Integer -> 0
    | Integer , _ -> (-1)
    | _ , Integer -> 1

    | Real , Real -> 0
    | Real , _ -> (-1)
    | _ , Real -> 1

    | Boolean , Boolean -> 0
    | Boolean , _ -> (-1)
    | _ , Boolean -> 1

    | Pointer , Pointer -> 0
    | Pointer , _ -> (-1)
    | _ , Pointer -> 1

    | ALPHA k , ALPHA k' -> Pervasives.compare k k'
    | ALPHA _ , _ -> (-1)
    | _ , ALPHA _ -> 1

    | Set ta , Set tb -> compare_tau ta tb
    | Set _ , _ -> (-1)
    | _ , Set _ -> 1

    | Array(ta,tb) , Array(ta',tb') -> compare_sig [ta;tb] [ta';tb']
    | Array _ , _ -> (-1)
    | _ , Array _ -> 1

    | Record ra , Record rb -> LogicId.compare ra rb
    | Record _ , _ -> (-1)
    | _ , Record _ -> 1

    | ADT(a,ps) , ADT(b,qs) ->
	let cid = LogicId.compare a b in
	if cid<>0 then cid else compare_sig ps qs

and compare_sig ps qs =
  match ps , qs with
    | [] , [] -> 0
    | [] , _ -> (-1)
    | _ , [] -> 1
    | t1::ps , t2::qs ->
	let ct = compare_tau t1 t2 in
	if ct<>0 then ct else compare_sig ps qs

let compare_field f1 f2 = LogicId.compare f1.f_name f2.f_name
    
let rec depend ids = function
  | Integer | Real | Boolean | Pointer | ALPHA _ -> ids
  | Set te -> depend ids te
  | Array(ta,tb) -> depend (depend ids ta) tb
  | Record r -> Iset.add r ids
  | ADT(a,ts) -> List.fold_left depend (Iset.add a ids) ts
