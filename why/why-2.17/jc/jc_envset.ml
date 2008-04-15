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

(* $Id: jc_envset.ml,v 1.35 2008/11/05 14:03:15 filliatr Exp $ *)

open Jc_stdlib
open Jc_env

module type OrderedType =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type OrderedHashedType =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module ChoiceOrd(A : OrderedHashedType)(B : OrderedHashedType) =
struct
  type t = A of A.t | B of B.t
  let equal = function
    | A a1,A a2 -> A.equal a1 a2
    | B b1,B b2 -> B.equal b1 b2
    | _ -> false
  let compare = function
    | A a1,A a2 -> A.compare a1 a2
    | B b1,B b2 -> B.compare b1 b2
    | A _,_ -> 1
    | B _,_ -> -1
  let hash = function
    | A a -> A.hash a
    | B b -> B.hash b
end

module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

(* used names (in order to rename identifiers when necessary) *)
let used_names = Hashtbl.create 97

let mark_as_used x = 
  Hashtbl.add used_names x ()

let () = 
  List.iter mark_as_used 
    [ (* Why keywords *)
      "absurd"; "and"; "array"; "as"; "assert"; "axiom"; "begin";
      "bool"; "do"; "done"; "else"; "end"; "exception"; "exists";
      "external"; "false"; "for"; "forall"; "fun"; "function"; "goal";
      "if"; "in"; "int"; "invariant"; "label"; "let"; "logic"; "not";
      "of"; "or"; "parameter"; "predicate"; "prop"; "raise"; "raises";
      "reads"; "real"; "rec"; "ref"; "result"; "returns"; "then"; "true"; "try";
      "type"; "unit"; "variant"; "void"; "while"; "with"; "writes"; "init";
      (* jessie generated names *)
      (* "global" ; "alloc"  *)
    ]

let is_used_name n = Hashtbl.mem used_names n

let use_name ?local_names n = 
  if is_used_name n then raise Exit; 
  begin match local_names with 
    | Some h -> if StringSet.mem n h then raise Exit 
    | None -> () 
  end;
  mark_as_used n;
  n

let rec next_name ?local_names n i = 
  let n_i = n ^ "_" ^ string_of_int i in
  try use_name ?local_names n_i 
  with Exit -> next_name ?local_names n (succ i)

let get_unique_name ?local_names n = 
  try use_name ?local_names n 
  with Exit -> next_name ?local_names n 0

let is_pointer_type t =
  match t with
    | JCTnull -> true
    | JCTpointer _ -> true
    | _ -> false

let is_nonnull_pointer_type t =
  match t with
    | JCTpointer _ -> true
    | _ -> false

let is_integral_type = function
  | JCTnative Tinteger -> true
  | JCTenum _ -> true 
  | JCTnative _ | JCTlogic _ | JCTpointer _ | JCTnull | JCTany
  | JCTtype_var _ -> false

let is_embedded_field fi =
  match fi.jc_field_info_type with
    | JCTpointer(_,Some _,Some _) -> true
    | _ -> false

module VarOrd = struct
  type t = var_info
  let compare v1 v2 = 
    Pervasives.compare v1.jc_var_info_tag v2.jc_var_info_tag
  let equal v1 v2 = v1.jc_var_info_tag = v2.jc_var_info_tag
  let hash v = Hashtbl.hash v.jc_var_info_tag
end

module VarSet = Set.Make(VarOrd)

module VarMap = Map.Make(VarOrd)

module StructOrd =
  struct type t = struct_info
	 let compare st1 st2 = 
	   Pervasives.compare 
	     st1.jc_struct_info_name st2.jc_struct_info_name
	 let equal st1 st2 =
	   st1.jc_struct_info_name = st2.jc_struct_info_name
	 let hash st =
	   Hashtbl.hash st.jc_struct_info_name 
  end

module StructSet = Set.Make(StructOrd)

module StructMap = Map.Make(StructOrd)

module VariantOrd = struct
  type t = root_info
  let compare v1 v2 = 
    Pervasives.compare v1.jc_root_info_name v2.jc_root_info_name
  let equal v1 v2 =
    v1.jc_root_info_name = v2.jc_root_info_name
  let hash v =
    Hashtbl.hash v.jc_root_info_name 
end

module VariantSet = Set.Make(VariantOrd)

module VariantMap = Map.Make(VariantOrd)

module FieldOrd =
  struct type t = field_info
	 let compare f1 f2 = 
	   Pervasives.compare 
	     f1.jc_field_info_tag f2.jc_field_info_tag
	 let equal f1 f2 =
	   f1.jc_field_info_tag = f2.jc_field_info_tag
	 let hash fi = fi.jc_field_info_tag
  end

module FieldSet = Set.Make(FieldOrd)

module FieldMap = Map.Make(FieldOrd)
module FieldTable = Hashtbl.Make(FieldOrd)

module MemClass = 
struct
  type t = mem_class
  let equal fv1 fv2 = match fv1,fv2 with
    | JCmem_field a1,JCmem_field a2 -> FieldOrd.equal a1 a2
    | JCmem_plain_union b1,JCmem_plain_union b2 -> VariantOrd.equal b1 b2
    | JCmem_bitvector,JCmem_bitvector -> true
    | _ -> false
  let compare fv1 fv2 = match fv1,fv2 with
    | JCmem_field a1,JCmem_field a2 -> FieldOrd.compare a1 a2
    | JCmem_plain_union b1,JCmem_plain_union b2 -> VariantOrd.compare b1 b2
    | JCmem_bitvector,JCmem_bitvector -> 0
    | JCmem_field _,_ -> 1
    | _,JCmem_field _ -> -1
    | JCmem_plain_union _,_ -> 1
    | _,JCmem_plain_union _ -> -1
  let hash = function
    | JCmem_field a -> FieldOrd.hash a
    | JCmem_plain_union b -> VariantOrd.hash b
    | JCmem_bitvector -> 0
end

module MemClassSet = Set.Make(MemClass)

module AllocClass = 
struct
  type t = alloc_class
  let equal fv1 fv2 = match fv1,fv2 with
    | JCalloc_root st1,JCalloc_root st2 -> VariantOrd.equal st1 st2
    | JCalloc_bitvector,JCalloc_bitvector -> true
    | _ -> false
  let compare fv1 fv2 = match fv1,fv2 with
    | JCalloc_root a1,JCalloc_root a2 -> VariantOrd.compare a1 a2
    | JCalloc_bitvector,JCalloc_bitvector -> 0
    | JCalloc_root _,_ -> 1
    | _,JCalloc_root _ -> -1
  let hash = function
    | JCalloc_root a -> VariantOrd.hash a
    | JCalloc_bitvector -> 0
end

(* TODO: take into account type parameters *)
module PointerClass = 
struct
  type t = pointer_class
  let equal fv1 fv2 = match fv1,fv2 with
    | JCtag(st1,_),JCtag(st2,_) -> StructOrd.equal st1 st2
    | JCroot vi1,JCroot vi2 -> VariantOrd.equal vi1 vi2
    | _ -> false
  let compare fv1 fv2 = match fv1,fv2 with
    | JCtag(a1,_),JCtag(a2,_) -> StructOrd.compare a1 a2
    | JCroot b1,JCroot b2 -> VariantOrd.compare b1 b2
    | JCtag _,_ -> 1
    | _,JCtag _ -> -1
  let hash = function
    | JCtag(a,_) -> StructOrd.hash a
    | JCroot b -> VariantOrd.hash b
end

module ExceptionOrd =   
  struct type t = exception_info
	 let compare f1 f2 = 
	   Pervasives.compare 
	     f1.jc_exception_info_tag f2.jc_exception_info_tag
  end

module ExceptionSet = Set.Make(ExceptionOrd)

module ExceptionMap = Map.Make(ExceptionOrd)

module LogicLabelOrd =
  struct type t = label
	 let compare = (Pervasives.compare : label -> label -> int)
  end

module LogicLabelSet = Set.Make(LogicLabelOrd)

(*
Local Variables: 
compile-command: "LC_ALL=C make -C .. bin/jessie.byte"
End: 
*)
