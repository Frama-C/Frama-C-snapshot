(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(**************************************************************************)

(* $Id: retype.ml,v 1.3 2008/07/01 17:02:50 uid570 Exp $ *)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Ast_info
open Extlib

(* Utility functions *)
open Common


(*****************************************************************************)
(* Retype int field used as pointer.                                         *)
(*****************************************************************************)

class collectIntField
  (cast_field_to_type : typ FieldinfoHashtbl.t) =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr = function
    | CastE(ty,e) ->
	if isPointerType ty then 
	  match stripCastsAndInfo e with
	    | Lval(_host,off) ->
		begin match lastOffset off with
		  | Field(fi,_) ->
		      if isIntegralType fi.ftype
			&& bits_sizeof ty = bits_sizeof fi.ftype then
			  FieldinfoHashtbl.replace
			    cast_field_to_type fi fi.ftype 
		      else ()
		  | _ -> ()
		end
	    | _ -> ()
	else ();
	DoChildren
    | _ -> DoChildren
    
end

class retypeIntField
  (cast_field_to_type : typ FieldinfoHashtbl.t) =

  let postaction_expr e = match e with
    | Lval(_host,off) ->
	begin match lastOffset off with
	  | Field(fi,_) ->
	      begin try 
		CastE(FieldinfoHashtbl.find cast_field_to_type fi,e)
	      with Not_found -> e end
	  | _ -> e
	end
    | _ -> e
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vglob_aux = function 
    | GCompTag (compinfo,_) ->
	let fields = compinfo.cfields in
	let field fi =
	  if FieldinfoHashtbl.mem cast_field_to_type fi then
	    fi.ftype <- TPtr(!Common.struct_type_for_void,[])
	in
	List.iter field fields;
	DoChildren
    | _ -> DoChildren

  method vterm =
    do_on_term (None,Some postaction_expr)

  method vtsets_elem =
    do_on_tsets_elem (None,Some postaction_expr)

end

let retype_int_field file =
  let cast_field_to_type = FieldinfoHashtbl.create 17 in
  let visitor = new collectIntField cast_field_to_type in
  visitCilFile (visitor :> cilVisitor) file;
  let visitor = new retypeIntField cast_field_to_type in
  visitCilFile (visitor :> cilVisitor) file


(*****************************************************************************)
(* Organize structure types in hierarchy.                                    *)
(*****************************************************************************)

(* By default, elements should become representant only if they are 
 * "preferred" according to function [prefer]. Otherwise, decided by ranking.
 *)
module UnionFind
  (Elem :
    sig type t
	val equal : t -> t -> bool
	val prefer : t -> t -> int 
    end)
  (ElemSet : Set.S with type elt = Elem.t)
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

  (* Does not return singleton classes *)
  let classes () =
    let repr2class = ElemTable.create 17 in
    ElemTable.iter (fun e _ ->
		      let r = repr e in
		      try
			let c = ElemTable.find repr2class r in
			let c = ElemSet.add e c in
			ElemTable.replace repr2class r c
		      with Not_found ->
			ElemTable.add repr2class r (ElemSet.singleton e)
		   ) table;
    ElemTable.fold (fun r c ls -> ElemSet.add r c :: ls) repr2class []

  let one_class e =
    let r = repr e in
    ElemTable.fold (fun e _ ls ->
		      if Elem.equal r (repr e) then e::ls else ls
		   ) table []

  let same_class e1 e2 =
    Elem.equal (repr e1) (repr e2)

end

module FieldElem = struct include FieldinfoComparable let prefer _ _ = 1 end
module FieldUnion = UnionFind(FieldElem)(FieldinfoSet)(FieldinfoHashtbl)

let add_field_representant fi1 fi2 =
  FieldUnion.unify fi1 fi2 

module TypeElem = struct include TypeComparable let prefer _ _ = 0 end
module TypeUnion = UnionFind(TypeElem)(TypeSet)(TypeHashtbl)

let type_to_parent_type = TypeHashtbl.create 17

let add_inheritance_relation ty parentty =
  if TypeComparable.equal ty parentty then () else
    begin
      TypeHashtbl.add type_to_parent_type ty parentty;
      TypeUnion.unify ty parentty
    end

let rec subtype ty parentty =
  TypeComparable.equal ty parentty ||
    try
      subtype (TypeHashtbl.find type_to_parent_type ty) parentty
    with Not_found ->
      false

(* module Node = struct *)

(*   type t = NodeVar of varinfo | NodeField of fieldinfo | NodeType of typ *)

(*   let equal n1 n2 = match n1,n2 with *)
(*     | NodeVar v1,NodeVar v2 -> VarinfoComparable.equal v1 v2 *)
(*     | NodeField f1,NodeField f2 -> FieldinfoComparable.equal f1 f2 *)
(*     | NodeType ty1,NodeType ty2 -> TypeComparable.equal ty1 ty2 *)
	  
(*   let compare n1 n2 = match n1,n2 with *)
(*     | NodeVar v1,NodeVar v2 -> VarinfoComparable.compare v1 v2 *)
(*     | NodeVar _,_ -> -1 *)
(*     | _,NodeVar _ -> 1 *)
(*     | NodeField f1,NodeField f2 -> FieldinfoComparable.compare f1 f2 *)
(*     | NodeField _,_ -> -1 *)
(*     | _,NodeField _ -> 1 *)
(*     | NodeType ty1,NodeType ty2 -> TypeComparable.compare ty1 ty2 *)

(*   let hash = function *)
(*     | NodeVar v -> 17 * VarinfoComparable.hash v *)
(*     | NodeField f -> 43 * FieldinfoComparable.hash f *)
(*     | NodeType ty -> 73 * TypeComparable.hash ty *)
(* end *)

(* module NodeHashtbl = Hashtbl.Make(Node) *)
(* module NodeSet = Set.Make(Node) *)
(* module NodeUnion = UnionFind(Node)(NodeHashtbl)(NodeSet) *)

let sub_list l n =
  let rec aux acc n l = 
    if n = 0 then acc else
      match l with [] -> assert false | x::r -> aux (x::acc) (n-1) r
  in
  List.rev (aux [] n l)

class createStructHierarchy =
  let unify_type_hierarchies ty1 ty2 =
    (* Extract info from types *)
    let compinfo1 = match unrollType (pointed_type ty1) with
      | TComp(compinfo,_attr) -> compinfo
      | _ -> assert false 
    in
    let sty1 = TComp(compinfo1,[]) in
    let fields1 = compinfo1.cfields in
    let compinfo2 = match unrollType (pointed_type ty2) with
      | TComp(compinfo,_attr) -> compinfo
      | _ -> assert false 
    in
    let fields2 = compinfo2.cfields in
    let sty2 = TComp(compinfo2,[]) in
    (* Compare types *)
    let minlen = min (List.length fields1) (List.length fields2) in
    let prefix,complete = List.fold_left2
      (fun (acc,compl) f1 f2 ->
	 if compl && TypeComparable.equal f1.ftype f2.ftype then
	   f1::acc,compl
	 else acc,false
      )
      ([],true)
      (sub_list fields1 minlen) (sub_list fields2 minlen)
    in
    let prefix = List.rev prefix in
    if complete then 
      if List.length prefix = List.length fields1 then
	(* [ty2] subtype of [ty1] *)
	add_inheritance_relation sty2 sty1
      else
	(* [ty1] subtype of [ty2] *)
	add_inheritance_relation sty1 sty2
    else
      begin
	(* Neither one is subtype of the other *)
	add_inheritance_relation sty1 !Common.struct_type_for_void;
	add_inheritance_relation sty2 !Common.struct_type_for_void
      end
  in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr = function
    | CastE(ty,e) when isPointerType ty ->
	let enocast = stripCastsAndInfo e in
	let ety = typeOf enocast in
	if isPointerType ety then
	  unify_type_hierarchies ty ety
	else ();
	DoChildren
    | _ -> DoChildren

end

class exploitStructHierarchy =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super
    
    

end

let create_struct_hierarchy file =
  let struct_fields ty =
    match unrollType ty with
      | TComp(compinfo,_attr) -> compinfo.cfields
      | _ -> assert false 
  in
  let num_fields ty = List.length (struct_fields ty) in
  let compare_num_fields ty1 ty2 = 
    Pervasives.compare (num_fields ty1) (num_fields ty2)
  in
  let subtype ty1 ty2 =
    let fields1 = struct_fields ty1 and fields2 = struct_fields ty2 in
    let len1 = List.length fields1 and len2 = List.length fields2 in
    if len1 > len2 then
      List.fold_left2 (fun eq fi1 fi2 ->
			 eq && TypeComparable.equal fi1.ftype fi2.ftype
		      ) true (sub_list fields1 len2) fields2
    else false
  in
  let compute_hierarchy () =
    let classes = TypeUnion.classes () in
    List.iter (fun cls ->
		 let types = TypeSet.elements cls in
		 let types = List.sort compare_num_fields types in
		 let root,types = 
		   match types with [] -> assert false | a::r -> a,r
		 in
		 (* First element is new root *)
		 TypeHashtbl.remove type_to_parent_type root;
		 List.iter 
		   (fun ty ->
		      add_inheritance_relation ty root;
		      List.iter 
			(fun party ->
			   if subtype ty party then
			     add_inheritance_relation ty party
			) types
		   ) types
	      ) classes;
    TypeHashtbl.iter (fun ty party ->
			let fields1 = struct_fields ty in
			let fields2 = struct_fields party in
			let num2 = List.length fields2 in
			let subfields1 = sub_list fields1 num2 in
			List.iter2 add_field_representant subfields1 fields2
		     ) type_to_parent_type
  in
  
  let visitor = new createStructHierarchy in
  visitCilFile (visitor :> cilVisitor) file;
  compute_hierarchy ();
  let visitor = new exploitStructHierarchy in
  visitCilFile (visitor :> cilVisitor) file


(*****************************************************************************)
(* Retype the C file for Jessie translation.                                 *)
(*****************************************************************************)

let retype file =
  if checking then check_types file;
  (* Retype int field casted to pointer. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Retype int field casted to pointer@.";
  retype_int_field file;
  if checking then check_types file;
  (* Organize structure types in hierarchy. *)
  if Cmdline.Debug.get () >= 1 then
    Format.printf "Organize structure types in hierarchy@.";
  create_struct_hierarchy file;
  if checking then check_types file;

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j byte"
End:
*)
