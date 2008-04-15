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

(* $Id: jc_region.ml,v 1.32 2008/11/19 17:42:00 moy Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset

open Format
open Pp

let string_explode s = 
  let rec next acc i = 
    if i >= 0 then next (s.[i] :: acc) (i-1) else acc
  in
  next [] (String.length s - 1)

let string_implode ls =
  let s = String.create (List.length ls) in
  ignore (List.fold_left (fun i c -> s.[i] <- c; i + 1) 0 ls);
  s

let filter_alphanumeric s =
  let alphanum c = 
    String.contains "abcdefghijklmnopqrstuvwxyz" c
    || String.contains "ABCDEFGHIJKLMNOPQRSTUVWXYZ" c
    || String.contains "0123456789" c
    || c = '_'
  in
  string_implode (List.filter alphanum (string_explode s))

let dummy_region = 
  {
    jc_reg_variable = false;
    jc_reg_bitwise = false;
    jc_reg_id = 0;
    jc_reg_name = "dummy_region";
    jc_reg_final_name = "dummy_region";
    jc_reg_type = JCTnull; (* Type does not matter. *)
  }

let is_dummy_region r = r.jc_reg_id = 0

module InternalRegion = struct

  type t = region

  let equal r1 r2 = r1.jc_reg_id = r2.jc_reg_id

  let compare r1 r2 = Pervasives.compare r1.jc_reg_id r2.jc_reg_id

  let hash r = r.jc_reg_id      

  let prefer r1 r2 = 
    if r1.jc_reg_variable && not r2.jc_reg_variable then 1 
    else if r2.jc_reg_variable && not r1.jc_reg_variable then -1
    else r1.jc_reg_id - r2.jc_reg_id

end

(* By default, elements should become representant only if they are 
 * "preferred" according to function [prefer]. Otherwise, decided by ranking.
 *)
module UnionFind
  (Elem : 
    sig type t 
	val equal : t -> t -> bool 
	val prefer : t -> t -> int 
    end)
  (ElemTable : Hashtbl.S with type key = Elem.t) =
struct
  
  let table = ElemTable.create 73
  let ranks = ElemTable.create 73

  let rec repr e = 
    try 
      let r = repr(ElemTable.find table e) in
      ElemTable.replace table e r;
      r
    with Not_found -> e

  let rank e =
    try ElemTable.find ranks e with Not_found -> 0

  let unify e1 e2 =
    let r1 = repr e1 and r2 = repr e2 in
    if Elem.equal r1 r2 then ()
    else 
      (* Start with preference as defined by function [prefer]. *)
      let pref = Elem.prefer r1 r2 in
      let k1 = rank r1 and k2 = rank r2 in
      if pref < 0 then
	begin
	  ElemTable.replace table r2 r1;
	  if k1 <= k2 then ElemTable.replace ranks r1 (k2 + 1)
	end
      else if pref > 0 then
	begin
	  ElemTable.replace table r1 r2;
	  if k2 <= k1 then ElemTable.replace ranks r2 (k1 + 1)
	end
      else
	(* If no definite preference, resolve to classical ranking. *)
	if k1 < k2 then
	  ElemTable.replace table r1 r2
	else if k2 < k1 then
	  ElemTable.replace table r2 r1
	else 
	  begin
	    ElemTable.replace table r1 r2;
	    ElemTable.replace ranks r2 (k2 + 1)
	  end

end

module RegionTable = Hashtbl.Make(InternalRegion)

module PairOrd(A : Set.OrderedType)(B : Set.OrderedType) =
struct
  type t = A.t * B.t
  let compare (a1,b1) (a2,b2) =
    let res1 = A.compare a1 a2 in
    if res1 <> 0 then res1 else B.compare b1 b2
  let equal (a1,b1) (a2,b2) =
    compare (a1,b1) (a2,b2) = 0
end

module RegionUF = UnionFind(InternalRegion)(RegionTable)

module RegionSet = struct 
  module S = Set.Make(InternalRegion)
  let empty = S.empty
  let mem r s = S.mem (RegionUF.repr r) s
  let add r s = S.add (RegionUF.repr r) s
  let of_list ls =
    List.fold_left (fun s e -> add e s) empty ls
  let to_list s =
    S.fold (fun e acc -> e :: acc) s []
  let singleton = S.singleton
end

(* Sets should be computed after unification takes place, so that operations
 * can maintain easily the invariant that only representative regions are used.
 *)
module PairRegionSet
  (T : sig type t end)(P : Set.OrderedType with type t = T.t * region) = 
struct
  module S = Set.Make(P)
  let empty = S.empty
  let mem (fi,r) s = S.mem (fi,RegionUF.repr r) s
  let add (fi,r) s = S.add (fi,RegionUF.repr r) s
  let singleton (fi,r) = S.singleton (fi,RegionUF.repr r)
  let remove (fi,r) s = S.remove (fi,RegionUF.repr r) s
  let split (fi,r) s = S.split (fi,RegionUF.repr r) s
  let elements = S.elements
  let exists = S.exists
  let union = S.fold add
    (* Added w.r.t. standard Set. *)
  let of_list ls =
    List.fold_left (fun s e -> add e s) empty ls
  let to_list s =
    S.fold (fun e acc -> e :: acc) s []
  let fold = S.fold
  let map_repr s = 
    S.fold (fun (fi,r) acc -> S.add (fi,RegionUF.repr r) acc) s S.empty
  let find_region r s =
    S.filter (fun (_fi,r') -> 
		InternalRegion.equal (RegionUF.repr r) (RegionUF.repr r')) s
  let mem_region r s =
    S.exists (fun (_fi,r') -> 
		InternalRegion.equal (RegionUF.repr r) (RegionUF.repr r')) s
end

(* Maps should be computed after unification takes place, so that operations
 * can maintain easily the invariant that only representative regions are used.
 *)
module PairRegionMap
  (T : sig type t end)(P : Set.OrderedType with type t = T.t * region) = 
struct
  module M = Map.Make(P)
  include M
  let add (fi,r) s = M.add (fi,RegionUF.repr r) s
  let find (fi,r) s = M.find (fi,RegionUF.repr r) s
  let remove (fi,r) s = M.remove (fi,RegionUF.repr r) s
  let mem (fi,r) s = M.mem (fi,RegionUF.repr r) s
  let add_merge f (k,r) v m = add_merge f (k,RegionUF.repr r) v m
end

let global_region_table : (InternalRegion.t FieldTable.t) RegionTable.t 
    = RegionTable.create 73

module Region =
struct

  include InternalRegion

  let name r = (RegionUF.repr r).jc_reg_final_name

  let representative = RegionUF.repr

  let polymorphic r = 
    let r = RegionUF.repr r in r.jc_reg_variable

  let count = ref 1 
  let next_count () = let tmp = !count in incr count; tmp

  let make_const ty name =
    let name = filter_alphanumeric name in
    if !Jc_common_options.separation_sem = SepNone then dummy_region
    else if not(is_pointer_type ty) then dummy_region else
      let id = next_count () in
      {
	jc_reg_variable = false;
	jc_reg_bitwise = false;
	jc_reg_id = id;
	jc_reg_name = name;
	jc_reg_final_name = name ^ "_" ^ (string_of_int id);
	jc_reg_type = ty;
      }

  let make_var ty name =
    let name = filter_alphanumeric name in
    if !Jc_common_options.separation_sem = SepNone then dummy_region
    else if not(is_pointer_type ty) then dummy_region else
      let id = next_count () in
      {
	jc_reg_variable = true;
	jc_reg_bitwise = false;
	jc_reg_id = id;
	jc_reg_name = name;
	jc_reg_final_name = name ^ "_" ^ (string_of_int id);
	jc_reg_type = ty;
      }

  let print fmt r =
    fprintf fmt "%s" r.jc_reg_final_name

  let print_assoc fmt assocl =
    fprintf fmt "%a" (print_list comma 
      (fun fmt (r1,r2) -> fprintf fmt "%a->%a" print r1 print r2)) assocl

  let bitwise r = (RegionUF.repr r).jc_reg_bitwise

  let equal r1 r2 =
    InternalRegion.equal (RegionUF.repr r1) (RegionUF.repr r2)

  let some_bitwise_region = ref false

  let exists_bitwise () = !some_bitwise_region

  let rec make_bitwise r =
    some_bitwise_region := true;
    if bitwise r then () else
      let rep = RegionUF.repr r in
      if !Jc_common_options.separation_sem = SepNone then 
	(* Potentially all pointers may be aliased *)
	(assert (is_dummy_region rep);
	 rep.jc_reg_bitwise <- true)
      else
	rep.jc_reg_bitwise <- true;
      try 
	let t = RegionTable.find global_region_table rep in
	begin match FieldTable.choose t with
	  | Some(_,r1) ->
	      make_bitwise r1;
	      FieldTable.iter (fun _fi ri -> unify r1 ri) t
	  | None -> ()
	end
      with Not_found -> ()
	
  and unify ?(poly=true) r1 r2 = 
    if is_dummy_region r1 && is_dummy_region r2 then ()
    else if is_dummy_region r1 || is_dummy_region r2 then
      let r,dr = if is_dummy_region r1 then r2, r1 else r1, r2 in
      Format.printf "unifying a dummy region %a with a non dummy region %a@."
	print dr print r;
      assert false
    else if equal r1 r2 then () else
      let rep1 = RegionUF.repr r1 and rep2 = RegionUF.repr r2 in
      RegionUF.unify r1 r2;
      let t1 = 
	try RegionTable.find global_region_table rep1 
	with Not_found -> FieldTable.create 0 
      in
      let t2 = 
	try RegionTable.find global_region_table rep2
	with Not_found -> FieldTable.create 0 
      in
      (* If polymorphic argument is false, then current unification is
	 a field unification coming from parents being unified, with one
	 parent being a constant region. Then its field region must be
	 constant too. 
      *)
      assert (poly || not (polymorphic r1 && polymorphic r2));
      let poly = 
	poly && polymorphic r1 && polymorphic r2 
      in
      (* Recursively make field regions constant if top region is constant *)
      let rec mkconst r = 
	let rep = RegionUF.repr r in
	if polymorphic rep then
	  begin
	    rep.jc_reg_variable <- false;
	    try
	      let t = RegionTable.find global_region_table rep in
	      FieldTable.iter (fun _fi ri -> mkconst ri) t
	    with Not_found -> ()
	  end
      in
      FieldTable.iter 
	(fun fi r1 ->
	  try 
	    begin 
	      let r2 = FieldTable.find t2 fi in
	      unify ~poly r1 r2
	    end
	  with Not_found -> 
	    if not poly then mkconst r1;
	    FieldTable.add t2 fi r1
	) t1;
      FieldTable.iter 
	(fun fi r2 ->
	  try ignore(FieldTable.find t1 fi)
	  with Not_found -> 
	    if not poly then mkconst r2
	) t2;
      (* Recursively make region bitwise if necessary *)
      let rep = RegionUF.repr r1 in
      if rep1.jc_reg_bitwise || rep2.jc_reg_bitwise then
	make_bitwise rep;
      (* Update with correct field table *)
      RegionTable.replace global_region_table rep t2

  let make_field r fi =
    let r = RegionUF.repr r in
(*     assert (not r.jc_reg_bitwise); *)
    if !Jc_common_options.separation_sem = SepNone then dummy_region
    else if not(is_pointer_type fi.jc_field_info_type) then dummy_region else
      try 
	let t = RegionTable.find global_region_table r in
	try FieldTable.find t fi
	with Not_found -> 
	  let fr =
	    if is_embedded_field fi then
	      r
	    else if r.jc_reg_variable then
	      make_var fi.jc_field_info_type fi.jc_field_info_name 
	    else
	      make_const fi.jc_field_info_type (* fi.jc_field_info_name *)
		r.jc_reg_name
	  in
	  FieldTable.replace t fi fr;
	  fr
      with Not_found ->
	let fr = 
	  if is_embedded_field fi then
	    r
	  else if r.jc_reg_variable then
	    make_var fi.jc_field_info_type fi.jc_field_info_name 
	  else
	    make_const fi.jc_field_info_type (* fi.jc_field_info_name *)
	      r.jc_reg_name
	in
	let t = FieldTable.create 5 in
	FieldTable.replace t fi fr;
	RegionTable.replace global_region_table r t;
	fr

end

module RegionList =
struct

  let rec assoc r assocl = 
    if is_dummy_region r then dummy_region else
      match assocl with
	| [] -> raise Not_found
	| (r1,r2)::ls -> 
	    if Region.equal r r1 then RegionUF.repr r2 else assoc r ls

  let rec assoc' r assocl = 
    match assocl with
      | [] -> raise Not_found
      | (r1,v)::ls -> 
	  if Region.equal r r1 then v else assoc' r ls
	    
  let rec mem r = function
    | [] -> false
    | r1::ls -> Region.equal r r1 || mem r ls

  (* Do not duplicate the bitwise field of regions *)
  let duplicate rls =
    let assocl = 
      List.fold_left (fun acc r ->
	if is_dummy_region r then acc 
	else (r,Region.make_var r.jc_reg_type r.jc_reg_name) :: acc
      ) [] rls
    in
    List.iter (fun (r1,r2) ->
      try
	let r1 = RegionUF.repr r1 in
	let t = FieldTable.copy(RegionTable.find global_region_table r1) in
	FieldTable.iter (fun fi r -> 
	  try
	    FieldTable.replace t fi (assoc r assocl)
	  with Not_found -> ()
	) t;
	RegionTable.replace global_region_table r2 t
      with Not_found -> ()
    ) assocl;
    assocl

  let reachable rls =
    let rec collect acc r =
      if is_dummy_region r then acc else
	let r = RegionUF.repr r in
	if mem r acc then acc else
	  let acc = r :: acc in
	  try
	    let t = RegionTable.find global_region_table r in
	    FieldTable.fold (fun fi fr acc -> collect acc fr) t acc
	  with Not_found -> acc
    in
    List.fold_left collect [] rls

end

module Alloc = PairOrd(AllocClass)(InternalRegion)

module AllocSet = PairRegionSet(AllocClass)(Alloc)

module AllocMap = PairRegionMap(AllocClass)(Alloc)

module AllocList =
struct

  let rec mem (a,r) = function
    | [] -> false
    | (a',r')::rest -> 
	a = a' && Region.equal r r' || mem (a,r) rest

end

module Tag = PairOrd(VariantOrd)(InternalRegion)

module TagSet = PairRegionSet(VariantOrd)(Tag)

module TagMap = PairRegionMap(VariantOrd)(Tag)

module TagList =
struct

  let rec mem (a,r) = function
    | [] -> false
    | (a',r')::rest -> 
	a = a' && Region.equal r r' || mem (a,r) rest

end

module Memory = PairOrd(MemClass)(InternalRegion)

module MemorySet = 
  PairRegionSet(MemClass)(Memory)

module MemoryMap = 
  PairRegionMap(MemClass)(Memory)

module MemoryList =
struct

  let rec mem (fi,r) = function
    | [] -> false
    | (fi',r')::rest -> 
	MemClass.equal fi fi' && Region.equal r r' || mem (fi,r) rest

end

module Pointer = PairOrd(PointerClass)(InternalRegion)

module PointerSet = PairRegionSet(PointerClass)(Pointer)



(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
