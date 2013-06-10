(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
(* --- Datatype Unifier                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(ADT:Data)(Field:Field) =
struct

  type tau = (Field.t,ADT.t) datatype
  type signature = (Field.t,ADT.t) funtype

  type domain =
    | Top
    | Eqx of int
    | Type of t

  and t =
    | Gvar of int
    | Gint
    | Greal
    | Gbool
    | Gprop
    | Garray of t * t
    | Gnamed of ADT.t * t list
    | Gabstract of ADT.t * t list
    | Grecord of (Field.t * t) list

  let int = Gint
  let real = Greal
  let bool = Gbool
  let prop = Gprop
  let array a b = Garray(a,b)
  let record fts = Grecord fts
  let data a ts = Gnamed(a,ts)

  let rec typedef gs = function
    | Int -> Gint
    | Real -> Greal
    | Bool -> Gbool
    | Prop -> Gprop
    | Tvar k -> gs.(k-1)
    | Array(a,b) -> Garray(typedef gs a,typedef gs b)
    | Data(a,ts) -> Gnamed(a,List.map (typedef gs) ts)
    | Record fts -> Grecord(List.map (fun (f,t) -> f,typedef gs t) fts)

  type mgu = {
    datatype : ADT.t -> tau option ;
    mutable mgu : domain array ;
    mutable quoted : (string * t) list ;
    mutable def : tau Intmap.t ; (* for generalized variables *)
    mutable gen : int ; (* number of generalized variables *)
  }

  let create env = { datatype=env ; mgu=[| |] ; quoted=[] ; gen=1 ; def=Intmap.empty }
      
  let fresh s = 
    let k = Array.length s.mgu in
    s.mgu <- Array.append s.mgu [| Top |] ; Gvar k

  let quoted s a =
    try List.assoc a s.quoted
    with Not_found ->
      let t = fresh s in
      s.quoted <- (a,t) :: s.quoted ; t
    
  let rec shift k = function
    | Int -> Gint
    | Real -> Greal
    | Bool -> Gbool
    | Prop -> Gprop
    | Tvar i -> Gvar(k+i)
    | Array(a,b) -> Garray(shift k a,shift k b)
    | Data(a,ts) -> Gnamed(a,List.map (shift k) ts)
    | Record fts -> Grecord(List.map (fun (f,t) -> f,shift k t) fts)
	
  let alloc s n =
    let k = Array.length s.mgu in
    let m = Array.create n Top in
    s.mgu <- Array.append s.mgu m ; k-1

  let of_tau s t = let k = alloc s (Kind.degree_of_tau t) in shift k t
    
  let of_sig s f =
    let k = alloc s (Kind.degree_of_sig f) in
    shift k f.result , List.map (shift k) f.params
      
  (* union-find algorithm *)
  let rec find s k =
    match s.mgu.(k) with
      | Eqx k0 ->
	  let k1 = find s k0 in
	  if k1 < k0 then s.mgu.(k) <- s.mgu.(k0) ; k1
      | _ -> k

  let rec definition s a gs =
    match try s.datatype a with Not_found -> None with
      | Some t ->
	  let g = typedef (Array.of_list gs) t in
	  (match g with Gnamed(a,gs) -> definition s a gs | _ -> g)
      | None -> Gabstract(a,gs)

  (* -------------------------------------------------------------------------- *)
  (* --- Unification                                                        --- *)
  (* -------------------------------------------------------------------------- *)
    
  let rec normvar s k =
    match s.mgu.(k) with
      | Eqx k0 ->
	  let k1 = find s k0 in
	  if k1 < k0 then s.mgu.(k) <- s.mgu.(k0) ;
	  normvar s k1
      | Top -> Gvar k
      | Type t -> t
	  
  let norm s = function
    | Gvar k -> normvar s k
    | t -> t

  let fields s t =
    let rec getfields s = function
      | Gnamed(a,gs) -> getfields s (definition s a gs)
      | Grecord fts -> fts
      | _ -> failwith "not a record type"
    in getfields s (norm s t)
	
  let rec occur s x = function
    | Gvar y -> if x = find s y then failwith "cyclic type"
    | Gnamed(_,ts) | Gabstract(_,ts) -> List.iter (occur s x) ts
    | Grecord fts -> List.iter (fun (_,t) -> occur s x t) fts
    | Garray(a,b) -> occur s x a ; occur s x b
    | Gint | Greal | Gbool | Gprop -> ()
	
  let rec mgu s = function
    | [] -> ()
    | (a,b)::eqs ->
	match norm s a , norm s b with
	  | Gvar x , Gvar y ->
	      if x < y then s.mgu.(y) <- Eqx x else
		if x > y then s.mgu.(x) <- Eqx y ;
	      mgu s eqs
	  | (Gvar x,t) | (t,Gvar x) ->
	      occur s x t ;
	      s.mgu.(x) <- Type t ;
	      mgu s eqs

	  | ( Gnamed(a,ps) | Gabstract(a,ps) ) , ( Gnamed(b,qs) | Gabstract(b,qs) )
	      when a=b && List.length ps = List.length qs ->
	      mgu s (List.combine ps qs @ eqs)

	  | Gnamed(a,ps) , Gnamed(b,qs) -> 
	      mgu s (( definition s a ps , definition s b qs )::eqs)

	  | Gnamed(a,ps) , _ ->
	      mgu s (( definition s a ps , b )::eqs)

	  | _ , Gnamed(b,qs) ->
	      mgu s (( a , definition s b qs )::eqs)

	  | Gint,Gint -> mgu s eqs
	  | Greal,Greal -> mgu s eqs
	  | (Gprop|Gbool),(Gbool|Gprop) -> mgu s eqs
	  | Garray(a,b) , Garray(a',b') -> mgu s ( (a,a')::(b,b')::eqs )
	  | Grecord fts , Grecord grs -> mgu_record s fts grs eqs
	  | _ -> failwith "incompatible types"
	      
  and mgu_record s fts grs eqs =
    match fts,grs with
      | (f,t)::fts , (g,r)::grs ->
	  if Field.equal f g then
	    mgu_record s fts grs ((t,r)::eqs)
	  else
	    Plib.failure "incompatible fields %a and %a" 
	      Field.pretty f Field.pretty g
      | [] , [] -> mgu s eqs
      | (f,_)::_ , [] | [] , (f,_)::_ -> 
	  Plib.failure "unexpected field %a" Field.pretty f

  let unify s a b = mgu s [a,b]

  (* -------------------------------------------------------------------------- *)
  (* --- Generalization                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  let rec sort s a =
    match norm s a with
      | Gint -> Sint
      | Greal -> Sreal
      | Gprop -> Sprop
      | Gbool -> Sbool
      | Gvar _ | Gabstract _ | Grecord _ -> Sdata
      | Garray(_,b) -> Sarray (sort s b)
      | Gnamed(a,ps) -> sort s (definition s a ps)
	  
  let rec alpha s k =
    match s.mgu.(k) with
      | Eqx k0 ->
	  let k1 = find s k0 in
	  if k1 < k0 then s.mgu.(k) <- s.mgu.(k0) ;
	  alpha s k1
      | Type t -> generalize s t
      | Top ->
	  try Intmap.find k s.def
	  with Not_found ->
	    let x = Tvar s.gen in
	    s.gen <- succ s.gen ;
	    s.def <- Intmap.add k x s.def ; x
	      
  and generalize s = function
    | Gvar k -> alpha s k
    | Gint -> Int
    | Greal -> Real
    | Gbool -> Bool
    | Gprop -> Prop
    | Garray(a,b) -> Array(generalize s a,generalize s b)
    | Gnamed(a,ts) | Gabstract(a,ts) -> Data(a,List.map (generalize s) ts)
    | Grecord fts -> Record(List.map (fun (f,t) -> f,generalize s t) fts)

  let final_degree s = s.gen
	  
  (* -------------------------------------------------------------------------- *)
  (* --- Description                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec pretty s fmt = function
    | Gint -> Format.pp_print_string fmt "int"
    | Greal -> Format.pp_print_string fmt "real"
    | Gbool -> Format.pp_print_string fmt "bool"
    | Gprop -> Format.pp_print_string fmt "prop"
    | Gvar k -> 
	begin
	  match normvar s k with
	    | Gvar k -> Format.fprintf fmt "?%d" k
	    | t -> pretty s fmt t
	end
    | Gnamed(a,ts) | Gabstract(a,ts) -> Kind.pp_data ADT.pretty (pretty s) fmt a ts
    | Grecord fts -> Kind.pp_record Field.pretty (pretty s) fmt fts
    | Garray(Gint,e) -> Format.fprintf fmt "%a[]" (pretty s) e
    | Garray(k,e) -> Format.fprintf fmt "%a[%a]" (pretty s) e (pretty s) k

end
