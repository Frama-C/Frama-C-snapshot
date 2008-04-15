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

(* $Id: jc_invariants.ml,v 1.84 2008/11/05 22:07:56 nrousset Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast
open Jc_fenv

open Jc_name
open Jc_constructors
open Jc_pervasives
open Jc_iterators
open Jc_interp_misc
open Jc_struct_tools

open Output

(* other modifications for this extension can be found in:
     ast, typing, norm, interp: about pack / unpack, and mutable
     jc_main.ml
       phase 3.5
       production phase 5
     jc_interp.ml
       function tr_fun: 2 calls to "assume_all_invariants"
       function statement
         JCSassign_heap: call to "assume_field_invariants"
     jc_typing.ml
       hashtbl mutable_fields_table
       hashtbl committed_fields_table
       function create_mutable_field
       function find_field_struct: "mutable" and "committed" cases (and the parameter allow_mutable)
       function decl: JCPDstructtype: call to create_mutable_field
       function statement: call to "assert_mutable"

TODOs:
     Maybe generate assocs (or global invariant) when doing unpack or pack, as it modifies
mutable and committed.
     Arrays and global invariant.
*)

let prop_type = simple_logic_type "prop"


(* returns (inv, reads) where i is the assertion of the invariants of the structure
and r is a StringSet of the "reads" needed by these invariants *)
let invariant_for_struct this st =
  let (_, invs) = Hashtbl.find Jc_typing.structs_table st.jc_struct_info_name in
  let inv =
    make_and_list
      (List.map 
	 (fun (li, _) -> 
	    make_logic_pred_call ~label_in_name:false ~region_assoc:[] ~label_assoc:[] li [this]) invs)
  in
  let reads =
    List.fold_left
      (fun acc (li, _) -> logic_info_reads acc li)
      StringSet.empty
      invs
  in
  (inv, reads)

let make_assume reads assume =
  BlackBox (Annot_type (LTrue, unit_type, reads, [], assume, []))

let fully_packed pc e =
  LPred(
    fully_packed_name,
    [ LVar (generic_tag_table_name (pointer_class_root pc));
      LVar (mutable_name pc);
      e ])
(*
let type_structure = function
  | JCTpointer(JCtag st, _, _) -> st
  | _ -> failwith "type_structure"
*)

let type_pc = function
  | JCTpointer(pc, _, _) -> pc
  | _ -> raise (Invalid_argument "type_pc")

let range_min = function
  | JCTpointer(_, x, _) -> x
  | _ -> raise (Invalid_argument "range_min")

let range_max = function
  | JCTpointer(_, _, x) -> x
  | _ -> raise (Invalid_argument "range_max")

(*let mutable_instance_of_name root_structure_name =
  (mutable_name root_structure_name)^"_instance_of"*)

let omin_omax alloc basep min max =
  let omin = match min with
    | None -> LApp("offset_min", [ alloc; basep ])
    | Some n -> LConst(Prim_int(Num.string_of_num n))
  in
  let omax = match max with
    | None -> LApp("offset_max", [ alloc; basep ])
    | Some n -> LConst(Prim_int(Num.string_of_num n))
  in
  omin, omax

let make_range index omin omax =
  if omin = omax then
    LPred("eq", [ index; omin ])
  else
    make_and
      (LPred("ge_int", [ index; omin ]))
      (LPred("le_int", [ index; omax ]))

let pset_singleton p =
  LApp("pset_singleton", [ p ])

let pset_union a b =
  LApp("pset_union", [ a; b ])

let pset_union_list = function
  | [] -> LVar "pset_empty"
  | hd::tl -> List.fold_left pset_union hd tl

let pset_range s a b =
  LApp("pset_range", [ s; a; b ])

let make_shift p i =
  LApp("shift", [ p; i ])

let make_not_assigns alloc old_mem new_mem pset =
  LPred("not_assigns", [ alloc; old_mem; new_mem; pset ])

let make_bool b = LConst(Prim_bool b)

(************************************)
(* Checking an invariant definition *)
(************************************)

(* Typing imposes non pointer fields to have the flag "rep" *)
let field this pos fi =
  if not fi.jc_field_info_rep then
    Jc_typing.typing_error pos "this term is not a rep field of %s"
      this.jc_var_info_name

(*let rec check_rep ?(must_deref=false) this pos t =
  match t.jc_term_node with
    | JCTvar vi when vi == this && not must_deref -> ()
    | JCTderef (t, lab, fi) ->
	field fi this pos;
	check_rep ~must_deref:false this pos t
    | JCTcast (t, _, _) -> assert false (* TODO *)
    | JCTshift (t, _) ->
	(* t must not be this, but might be a field of this if it is a table *)
	(* (? TODO) *)
	check_rep ~must_deref:true this pos t
    | _ ->
	Jc_typing.typing_error pos "this term is not a rep field of %s"
	  this.jc_var_info_name*)

(* A pattern may hide some dereferencing. *)
let pattern this p =
  iter_pattern
    (fun p -> match p#node with
       | JCPstruct(_, fipl) ->
	   List.iter (field this p#pos) (List.map fst fipl)
       | _ -> ())
    p

(* When typing the body of the invariant, the only pointer in the environment
is the argument of the pointer. Thus, it is sufficient to check that all
dereferencing is done on a rep field AND that all applications do not read
any memory (else one could hide dereferencing in logic functions). *)
let term this t =
  ITerm.iter
    (fun t -> match t#node with
       | JCTapp app ->
	   let id = app.jc_app_fun in
	   if not (MemoryMap.is_empty
		     id.jc_logic_info_effects.jc_effect_memories) then
	     Jc_typing.typing_error t#pos
	       "this call is not allowed in structure invariant"
       | JCTderef(_, _, fi) ->
	   field this t#pos fi
       | JCTmatch(_, ptl) ->
	   List.iter (pattern this) (List.map fst ptl)
       | _ -> ())
    t

(*let term this t =
  iter_term
    (fun t -> match t.jc_term_node with
       | JCTconst _
       | JCTvar _
       | JCTrange(None, None) -> ()
       | JCTbinary(t1, _, t2)
       | JCTrange(Some t1, Some t2) ->
	   term this t1;
	   term this t2
       | JCTunary(_, t)
       | JCTold t
       | JCTat(t, _)
       | JCToffset(_, t, _)
       | JCTinstanceof(t, _, _)
       | JCTcast(t, _, _)
       | JCTrange(Some t, None)
       | JCTrange(None, Some t) ->
	   term this t
       | JCTif(t1, t2, t3) ->
	   term this t1
	   term this t2
	   term this t3
       | JCTmatch of term * (pattern * term) list
       | JCTshift(t1, t2) ->
       | JCTsub_pointer(t1, t2) ->
       | JCTderef(t, _, fi) ->
       | JCTapp app ->
	   let id = app.jc_app_fun in
	   if not (FieldRegionMap.is_empty
		     id.jc_logic_info_effects.jc_effect_memories) then
	     Jc_typing.typing_error t.jc_term_loc
	       "this call is not allowed in structure invariant"
       | JCTderef _ ->
	   check_rep this t.jc_term_loc t
       | _ -> ()
    ) t*)

let tag this t =
  match t#node with
    | JCTtag _
    | JCTbottom -> ()
    | JCTtypeof(t, _) -> term this t

let rec assertion this p =
  match p#node with
    | JCAtrue | JCAfalse -> ()
    | JCAif (_, _, _) -> assert false (* TODO *)
    | JCAinstanceof(t,_,_)
    | JCAbool_term t -> term this t
    | JCAold p -> assertion this p
    | JCAat(p,_) -> assertion this p
    | JCAquantifier(_,id, p) -> assertion this p
    | JCAapp app ->
	let id = app.jc_app_fun in
	if MemoryMap.is_empty id.jc_logic_info_effects.jc_effect_memories
	then List.iter (term this) app.jc_app_args
	else
	  Jc_typing.typing_error p#pos
	    "this call is not allowed in structure invariant"
    | JCAnot p -> assertion this p
    | JCAiff (p1, p2)
    | JCAimplies (p1, p2) -> assertion this p1; assertion this p2
    | JCArelation (t1,_, t2) -> term this t1; term this t2
    | JCAand l | JCAor l -> List.iter (assertion this) l
    | JCAmutable _ ->
	Jc_typing.typing_error p#pos
	  "\\mutable is not allowed in structure invariant"
    | JCAeqtype(t1, t2, _) | JCAsubtype(t1, t2, _) ->
	tag this t1;
	tag this t2
    | JCAmatch(t, pal) ->
	term this t;
	List.iter (fun (_, a) -> assertion this a) pal

let check invs =
  List.iter
    (fun (li,p) -> 
       Jc_options.lprintf
	 "    Checking invariant: %s@." li.jc_logic_info_name;
       match li.jc_logic_info_parameters with
	 | [this] -> assertion this p
	 | _ -> assert false)
    invs

(***********************************)
(* Tools for structure definitions *)
(***********************************)

let rec term_memories aux t = 
  fold_term 
    (fun aux t -> match t#node with
    | JCTderef(t, lab, fi) ->
	let m = fi.jc_field_info_final_name in
	StringSet.add m aux
    | _ -> aux
    ) aux t

let tag_memories aux t = match t#node with
  | JCTtag _ | JCTbottom -> aux
  | JCTtypeof(t, _) -> term_memories aux t

let rec assertion_memories aux a = match a#node with
  | JCAtrue
  | JCAfalse -> aux
  | JCAand l
  | JCAor l -> List.fold_left assertion_memories aux l
  | JCAimplies(a1, a2)
  | JCAiff(a1, a2) -> assertion_memories (assertion_memories aux a1) a2
  | JCArelation(t1,_,t2) -> term_memories (term_memories aux t1) t2
  | JCAnot a
  | JCAold a
  | JCAat(a,_)
  | JCAquantifier(_,_, a) -> assertion_memories aux a
  | JCAapp app -> List.fold_left term_memories aux app.jc_app_args
  | JCAinstanceof(t, _, _ )
  | JCAbool_term t -> term_memories aux t
  | JCAif(t, a1, a2) -> assertion_memories (assertion_memories (term_memories aux t) a1) a2
  | JCAmutable(t, _, _) -> term_memories aux t
  | JCAeqtype(t1, t2, _) | JCAsubtype(t1, t2, _) -> 
      tag_memories (tag_memories aux t2) t1
  | JCAmatch(t, pal) ->
      term_memories (List.fold_left assertion_memories aux (List.map snd pal)) t

(* Returns (as a StringSet.t) every structure name that can be reached from st.
Assumes the structures whose name is in acc have already been visited
and won't be visited again. *)
let rec all_structures st acc =
  if StringSet.mem st.jc_struct_info_name acc then acc else
    List.fold_left
      (fun acc fi ->
	 match fi.jc_field_info_type with
	   | JCTpointer(JCtag(st, _), _, _) -> all_structures st acc
	   | _ -> acc)
      (StringSet.add st.jc_struct_info_name acc)
      st.jc_struct_info_fields   

(* Returns all memories used by the structure invariants. *)
let struct_inv_memories acc st =
  let _, invs = Hashtbl.find Jc_typing.structs_table st in
  List.fold_left
    (fun acc (_, a) -> assertion_memories acc a)
    acc
    invs

(* Returns the parameters needed by an invariant, "this" not included *)
(* TODO: factorize using Jc_interp_misc.logic_params *)
let invariant_params acc li =
  let acc =
    MemoryMap.fold
      (fun (mc,r) labels acc -> 
	 (memory_name(mc,r), memory_type mc)::acc)
      li.jc_logic_info_effects.jc_effect_memories
      acc
  in
  let acc =
    AllocMap.fold
      (fun (ac,r) labs acc -> 
	 (alloc_table_name (ac, r),
	  alloc_table_type (ac))::acc)
      li.jc_logic_info_effects.jc_effect_alloc_tables
      acc
  in
  let acc =
    TagMap.fold
      (fun (v,r) labels acc -> 
	 let t = { logic_type_args = [root_model_type v];
		   logic_type_name = "tag_table" }
	 in
	 (tag_table_name (v,r), t)::acc)
      li.jc_logic_info_effects.jc_effect_tag_tables
      acc
  in
    acc

(* Returns the parameters needed by the invariants of a structure, "this" not included *)
let invariants_params acc st =
  let (_, invs) = Hashtbl.find Jc_typing.structs_table st.jc_struct_info_name in
  List.fold_left (fun acc (li, _) -> invariant_params acc li) acc invs

(* Returns the structure and its parents, up to its root *)
let rec parents acc st =
  match st.jc_struct_info_parent with
    | None -> acc
    | Some(p, _) -> parents (st::acc) p
let parents = parents []

(* Returns every structure (name) that can be used by a function,
given its parameters *)
let function_structures params =
  let structures = List.fold_left
    (fun acc vi ->
       match vi.jc_var_info_type with
	 | JCTpointer(JCtag(st, _), _, _) ->
	     all_structures st acc
	 | JCTpointer(JCroot vi, _, _) ->
	     List.fold_right all_structures vi.jc_root_info_hroots acc
	 | _ -> acc)
    StringSet.empty
    params
  in
  StringSet.elements structures

let hierarchy_structures h =
  Hashtbl.fold
    (fun _ (st, _) acc ->
       (* don't use equality directly on st and h, as it might not terminate *)
       (* we could use == instead though *)
       if root_name st = root_name h then st::acc else acc)
    Jc_typing.structs_table
    []
  
let hierarchies () =
  let h = Hashtbl.fold
    (fun _ (st, _) acc ->
       StringMap.add (root_name st) st.jc_struct_info_hroot acc)
    Jc_typing.structs_table
    StringMap.empty
  in
  StringMap.fold
    (fun _ st acc -> st::acc)
    h
    []

let is_pointer = function
  | JCTpointer _ -> true
  | _ -> false

(* Returns every rep pointer fields of a structure *)
let rep_fields st =
  List.filter
    (fun fi -> fi.jc_field_info_rep && is_pointer fi.jc_field_info_type)
    st.jc_struct_info_fields

(* Returns every rep fields of a hierarchy *)
let hierarchy_rep_fields root =
  List.flatten (List.map rep_fields (hierarchy_structures root))

(*********)
(* assoc *)
(*********)

(*let program_point_type = simple_logic_type "int"

let fresh_program_point =
  let c = ref 0 in fun () ->
  c := !c + 1; string_of_int !c

let assoc_declaration =
  (* logic assoc: int, ('a, 'b) memory -> prop *)
  Logic(
    false,
    "assoc",
    [ "", program_point_type;
      "", memory_type "'a" (simple_logic_type "'b") ],
    prop_type)

let make_assoc pp m =
  LPred("assoc", [LConst(Prim_int pp); LVar m])

let make_assoc_list pp mems =
  make_and_list (List.map (make_assoc pp) mems)

let make_assume_assocs pp mems =
  let assocs = make_assoc_list pp mems in
  make_assume mems assocs

(* List of each memory m that appears in an invariant
which can be broken by the modification of the field fi *)
let field_assocs fi =
  let _, invs = Hashtbl.find Jc_typing.structs_table fi.jc_field_info_root in
  let mems = List.fold_left
    (fun aux (_, a) ->
       let amems = assertion_memories StringSet.empty a in
       if StringSet.mem fi.jc_field_info_final_name amems then
         StringSet.union amems aux
       else
	 aux
    ) (StringSet.singleton (mutable_name fi.jc_field_info_root)) invs in
  StringSet.elements mems

(* Assume all assocs needed after a field has been modified *)
let make_assume_field_assocs pp fi =
  make_assume_assocs pp (field_assocs fi)

(* Returns a list of all memories which need an "assoc"
(calculated from a function parameter list) *)
let all_assocs pp params =
  let structures = function_structures params in
  (* memories used by these structures' invariants *)
  let memories = List.fold_left
    struct_inv_memories
    StringSet.empty
    structures
  in
  (* mutable fields *)
  let mutable_fields = List.map (fun s -> mutable_name s) structures in
  StringSet.elements memories@mutable_fields

(* Assume all assocs needed at the start of a fonction *)
let make_assume_all_assocs pp params =
  make_assume_assocs pp (all_assocs pp params)*)

(***********)
(* mutable *)
(***********)

let mutable_memory_type pc =
  raw_memory_type (pointer_class_model_type pc)
    (tag_id_type (pointer_class_root pc))

let committed_memory_type pc =
  raw_memory_type (pointer_class_model_type pc) (simple_logic_type "bool")

let mutable_declaration st acc =
  if st.jc_struct_info_parent = None then
    let st = JCtag(st, []) in
    (* mutable_T: T tag_id *)
    Param(
      false,
      mutable_name st,
      Ref_type(Base_type (mutable_memory_type st)))
    (* committed_T: bool *)
    ::Param(
      false,
      committed_name st,
      Ref_type(Base_type (committed_memory_type st)))
    ::acc
  else
    acc

(* Assert the condition under which a field update statement can be executed.
The object must be "sufficiently unpacked", that is: its "mutable" field is
a strict superclass of the class in which the field is defined.
  And the object must not be committed. *)
(* assert ((st.jc_struct_info_parent <: e.mutable || e.mutable = bottom_tag) && e.committed = false) *)
(* Actually, the "not committed" part is meta-implied by the
condition on mutable. *)
let assert_mutable e fi =
  if fi.jc_field_info_rep then
    begin
      let st = fi.jc_field_info_struct in
      let mutable_name = mutable_name (JCtag(st, [])) in
      (*let committed_name = committed_name st.jc_struct_info_hroot in*)
      let e_mutable = LApp("select", [LVar mutable_name; e]) in
      (*let e_committed = LApp("select", [LVar committed_name; e]) in*)
      let parent_tag = match st.jc_struct_info_parent with
	| None -> LVar "bottom_tag"
	| Some(parent, _) -> LVar (tag_name parent)
      in
      let sub = make_subtag parent_tag e_mutable in
      (*let not_committed =
	LPred(
	  "eq",
	  [ e_committed;
	    LConst (Prim_bool false) ])
      in
      Assert(make_and sub not_committed, Void)*)
      Assert(sub, Void)
    end
  else
    Void

(********************)
(* Invariant axioms *)
(********************)

(*let invariant_axiom st acc (li, a) =
  let params = invariant_params [] li in
  
  (* this *)
  let this = "this" in
  let this_ty =
    { logic_type_name = "pointer";
      logic_type_args = [simple_logic_type st.jc_struct_info_hroot] } in

  (* program point *)
  let pp = "program_point" in
  let pp_ty = simple_logic_type "int" in

  (* assoc memories with program point => not this.mutable => this.invariant *)
  let mutable_ty = mutable_memory_type st.jc_struct_info_hroot in
  let mutable_is_false =
    LPred(
      "eq",
      [ LConst(Prim_bool false);
	LApp("select", [LVar "mutable"; LVar this]) ]) in
  let assoc_memories = StringSet.fold
    (fun mem acc ->
       LPred("assoc", [LVar pp; LVar mem])::acc)
    (assertion_memories
       (StringSet.singleton "mutable")
       a)
    [] in
  let invariant = make_logic_pred_call li [LVar this] in
  let axiom_impl = List.fold_left (fun acc assoc -> LImpl(assoc, acc))
    (LImpl(mutable_is_false, invariant))
    assoc_memories in

  (* quantifiers *)
  let quantified_vars = params in
  let quantified_vars = ("mutable", mutable_ty)::quantified_vars in
  let quantified_vars = (pp, pp_ty)::quantified_vars in
  let quantified_vars = (this, this_ty)::quantified_vars in
  let axiom =
    List.fold_left (fun acc (id, ty) -> LForall(id, ty, acc))
      axiom_impl quantified_vars in
  Axiom("axiom_"^li.jc_logic_info_name, axiom)::acc

let invariants_axioms st acc =
  let _, invs = Hashtbl.find Jc_typing.structs_table st.jc_struct_info_name in
  List.fold_left (invariant_axiom st) acc invs*)

(******************************************)
(* Invariant assumes (axioms pre-applied) *)
(******************************************)

(* hierarchy root (string) -> hierarchy invariant parameters ((string * logic_type) list) *)
let hierarchy_invariants = Hashtbl.create 97

(* List of each invariant that can be broken by modifying a given field *)
let field_invariants fi =
  if not fi.jc_field_info_rep then [] else (* small optimisation, it is not needed *)
    (* List of all structure * invariant *)
    let invs = Hashtbl.fold
      (fun _ (st, invs) acc -> (List.map (fun inv -> st, inv) invs)@acc)
      Jc_typing.structs_table
      []
    in
    (* Only keep the invariants which uses the field *)
    List.fold_left
      (fun aux ((_, (_, a)) as x) ->
	 let amems = assertion_memories StringSet.empty a in
	 if StringSet.mem fi.jc_field_info_final_name amems then
           x::aux
	 else
	 aux)
      []
      invs

(* this.mutable <: st => invariant *)
(* (the invariant li must be declared in st) *)
let not_mutable_implies_invariant this st (li, _) =
  let params = invariant_params [] li in
  
  (* this.mutable <: st *)
  let mutable_name = mutable_name (JCtag(st, [])) in
  let mutable_io = make_subtag
    (LApp("select", [ LVar mutable_name; LVar this ]))
    (LVar (tag_name st))
  in

  (* invariant *)
  let invariant = 
    make_logic_pred_call ~label_in_name:false ~region_assoc:[] ~label_assoc:[] li [LVar this]
  in

  (* implies *)
  let impl = LImpl(mutable_io, invariant) in

  (* params *)
  let params = (mutable_name, mutable_memory_type (JCtag(st, [])))::params in
  let params = (generic_tag_table_name (struct_root st),
                tag_table_type (struct_root st))::params in

  params, impl

(* this.mutable <: st => this.fields.committed *)
let not_mutable_implies_fields_committed this st =
  let fields = rep_fields st in

  (* this.mutable <: st *)
  let mutable_name = mutable_name (JCtag(st, [])) in
  let mutable_io = make_subtag
    (LApp("select", [ LVar mutable_name; LVar this ]))
    (LVar (tag_name st))
  in

  (* fields committed *)
  let fields_pc = List.fold_left
    (fun acc fi ->
       match fi.jc_field_info_type with
	 | JCTpointer(fi_pc, min, max) ->
	     let n = fi.jc_field_info_final_name in
	     let index = "jc_index" in
	     let committed_name = committed_name fi_pc in
	     let fi_ac = alloc_class_of_pointer_class fi_pc in
	     let alloc = generic_alloc_table_name fi_ac in
	     let params =
	       [ n, memory_type (JCmem_field fi);
		 committed_name, committed_memory_type fi_pc;
		 alloc, alloc_table_type fi_ac; ]
	     in
	     let this_fi = make_select (LVar n) (LVar this) in
	     let f = make_shift this_fi (LVar index) in
	     let eq = make_eq (make_select_committed fi_pc f)
	       (make_bool true)
	     in
	     let omin, omax = omin_omax (LVar alloc) this_fi min max in
	     let range = make_range (LVar index) omin omax in
	     let pred =
	       LForall(
		 index, simple_logic_type "int",
		 LImpl(range, eq))
	     in
	     (params, pred)::acc
	 | _ -> acc)
    [] fields
  in
  let params, coms = List.flatten (List.map fst fields_pc),
    make_and_list (List.map snd fields_pc) in

  (* additional params *)
  let params = (mutable_name, mutable_memory_type (JCtag(st, [])))::params in
  let params = (generic_tag_table_name (struct_root st),
                tag_table_type (struct_root st))::params in

  (* implies *)
  let impl = LImpl(mutable_io, coms) in

  params, impl

(* this.committed => fully_packed(this) *)
let committed_implies_fully_packed this root =
  let committed_name = committed_name root in
  let committed_type = committed_memory_type root in
  let mutable_name = mutable_name root in
  let mutable_type = mutable_memory_type root in
  let tag_table = generic_tag_table_name (pointer_class_root root) in
  let tag_table_type = tag_table_type (pointer_class_root root) in

  (* this.committed = true *)
  let com = LPred(
    "eq",
    [ LApp(
	"select",
	[ LVar committed_name;
	  LVar this ]);
      LConst(Prim_bool true) ])
  in

  (* fully_packed(this) *)
  let packed = LPred(
    fully_packed_name,
    [ LVar tag_table;
      LVar mutable_name;
      LVar this ])
  in

  (* implies *)
  let impl = LImpl(com, packed) in

  (* params *)
  let params = [
    tag_table, tag_table_type;
    committed_name, committed_type;
    mutable_name, mutable_type;
  ] in

  params, impl

let lex2 (a1, b1) (a2, b2) =
  let c = compare a1 a2 in
  if c = 0 then compare b1 b2 else c

(* forall x, ((meta) forall rep field f),
   this.f = x.f /\ this.f.committed -> this = y *)
let owner_unicity this root =
  let reps = hierarchy_rep_fields root in

  (* x name and type *)
  let x_name = this^"_2" in
  let pc = JCtag(root, []) in
  let ac = alloc_class_of_pointer_class pc in
  let x_type = pointer_type ac pc in

  (* shift indexes *)
  let index1 = "jc_index" in
  let index2 = "jc_index_2" in

  (* big "or" on all rep fields *)
  let eq_and_com_list = List.map
    (fun fi ->
       try
	 let name = fi.jc_field_info_final_name in
	 Printf.printf "***** FIELD %s *****\n%!" name;
	 let fi_pc = type_pc fi.jc_field_info_type in
	 let committed_name = committed_name fi_pc in
	 let this_dot_f = make_select (LVar name) (LVar this) in
	 let x_dot_f = make_select (LVar name) (LVar x_name) in

	 (* indexes, ranges *)
	 let fi_ac = alloc_class_of_pointer_class fi_pc in
	 let alloc = generic_alloc_table_name fi_ac in
	 let omin1, omax1 = omin_omax (LVar alloc) this_dot_f
	   (range_min fi.jc_field_info_type)
	   (range_max fi.jc_field_info_type)
	 in
	 let omin2, omax2 = omin_omax (LVar alloc) x_dot_f
	   (range_min fi.jc_field_info_type)
	   (range_max fi.jc_field_info_type)
	 in
	 let range1 = make_range (LVar index1) omin1 omax1 in
	 let range2 = make_range (LVar index2) omin2 omax2 in
	 let shift1 = make_shift this_dot_f (LVar index1) in
	 let shift2 = make_shift this_dot_f (LVar index2) in

	 let eq = make_eq shift1 shift2 in

	 let com = make_eq
	   (make_select (LVar committed_name) shift1)
	   (make_bool true)
	 in

	 make_and_list [ range1; range2; eq; com ]
       with Failure "type_structure" ->
	 LFalse)
    reps
  in
  let big_or = make_or_list eq_and_com_list in

  let impl = LImpl(big_or, make_eq (LVar this) (LVar x_name)) in

  let forall =
    LForall(
      index1, simple_logic_type "int",
      LForall(
	index2, simple_logic_type "int",
	LForall(x_name, x_type, impl)))
  in

  (* params *)
  let params = List.map
    (fun fi ->
       let ac = JCalloc_root (struct_root fi.jc_field_info_struct) in
       [ fi.jc_field_info_final_name, memory_type (JCmem_field fi);
	 committed_name (JCtag(fi.jc_field_info_hroot, [])),
	 committed_memory_type (JCtag(fi.jc_field_info_hroot, []));
	 generic_alloc_table_name ac,
	 alloc_table_type ac ])
    reps
  in
  let params = List.flatten params in

  params, forall

let make_hierarchy_global_invariant acc root =
  (* this *)
  let this = "this" in
  let pc = JCtag(root, []) in
  let ac = alloc_class_of_pointer_class pc in
  let this_ty = pointer_type ac pc in

  (* not mutable => invariant, and their parameters *)
  let structs = hierarchy_structures root in
  let mut_inv = List.map
    (fun st ->
       let _, invs =
	 Hashtbl.find Jc_typing.structs_table st.jc_struct_info_name in
       List.map (fun inv -> not_mutable_implies_invariant this st inv) invs)
    structs
  in
  let mut_inv = List.flatten mut_inv in
  let params, mut_inv = List.map fst mut_inv, List.map snd mut_inv in
  let mut_inv = make_and_list mut_inv in

  (* not mutable for T => fields defined in T committed *)
  let params, mut_com = List.fold_left
    (fun (params, coms) st ->
       let p, c = not_mutable_implies_fields_committed this st in
       p@params, make_and c coms)
    (List.flatten params, LTrue)
    structs
  in

  (* committed => fully packed *)
  let params_cfp, com_fp = committed_implies_fully_packed this
    (JCtag(root, [])) in
  let params = params_cfp@params in

  (* unicity of the owner: x.f == y.f && x.f.committed ==> x == y *)
  let params_ou, owner_unicity = owner_unicity this root in
  let params = params_ou@params in

  (* predicate body, quantified on "this" *)
  let body = LForall(this, this_ty, make_and_list [ mut_inv; mut_com; com_fp; owner_unicity ]) in

  (* sort the parameters and only keep one of each *)
  let params = List.fold_left
    (fun acc (n, t) -> StringMap.add n t acc)
    StringMap.empty
    params
  in
  let params = StringMap.fold (fun n t acc -> (n, t)::acc) params [] in
  let params = List.sort lex2 params in

  (* fill hash table *)
  Hashtbl.add hierarchy_invariants root.jc_struct_info_name params;

  (* return the predicate *)
  match params with
    | [] -> acc (* Not supposed to happen though *)
    | _ -> Predicate(false, hierarchy_invariant_name root, params, body)::acc

let make_global_invariants acc =
  let h = hierarchies () in
  List.fold_left make_hierarchy_global_invariant acc h

let assume_global_invariant st =
  let params =
    try
      Hashtbl.find hierarchy_invariants (root_name st)
    with Not_found ->
      failwith
	("Jc_invariants.assume_global_invariant: "^
	   (root_name st)^" not found")
  in
  match params with
    | [] -> Void
    | _ ->
	let reads = List.map fst params in
	let params = List.map (fun (n, _) -> LVar n) params in
	let inv = LPred(hierarchy_invariant_name st, params) in
	make_assume reads inv

let assume_global_invariants hl =
  List.fold_left append Void (List.map assume_global_invariant hl)

(* Given a field that has just been modified, assume all potentially
useful invariant for all objects that is not mutable *)
let assume_field_invariants fi =
  let invs = field_invariants fi in
  (* keep hierarchies only once *)
  let hl = List.fold_left
    (fun acc (st, _) ->
       StringMap.add (root_name st) st.jc_struct_info_hroot acc)
    StringMap.empty
    invs
  in
  assume_global_invariants (StringMap.values hl)

(* Given the parameters of a function, assume all potentially useful
forall this: st, not this.mutable => invariant *)
let assume_all_invariants params =
  let structures = function_structures params in
  (* keep hierarchies only once *)
  let hl = List.fold_left
    (fun acc st ->
       StringSet.add (root_name (find_struct st)) acc)
    StringSet.empty
    structures
  in
  let roots = List.map find_struct (StringSet.elements hl) in
  assume_global_invariants roots

(*(* Given a field that has just been modified, assume all potentially
useful invariant for all objects that is not mutable *)
let assume_field_invariants fi =
  (* structure in which the field is defined *)
  let st, _ = Hashtbl.find Jc_typing.structs_table fi.jc_field_info_struct in
  let assumes = List.map (fun inv -> forall_mutable_invariant st inv) (field_invariants fi) in (* FAUX (st n'est pas forcÃ©ment la bonne structure !) *)
  let assumes = List.map (fun (params, inv) -> make_assume (List.map fst params) inv) assumes in
  List.fold_left append Void assumes

let rec flatten_snd = function
  | [] -> []
  | (a, l)::tl -> (List.map (fun b -> a, b) l)@(flatten_snd tl)

(* Given the parameters of a function, assume all potentially useful
forall this: st, not this.mutable => invariant *)
let assume_all_invariants params =
  let structures = function_structures params in
  let st_invs =
    List.map
      (fun id -> Hashtbl.find Jc_typing.structs_table id)
      structures
  in
  let st_invs = flatten_snd st_invs in
  let assumes = List.map (fun (st, inv) -> forall_mutable_invariant st inv) st_invs in
  let assumes = List.map (fun (params, inv) -> make_assume (List.map fst params) inv) assumes in
  List.fold_left append Void assumes*)

(*****************)
(* pack / unpack *)
(*****************)

(* return fields that are both pointers and rep fields *)
let components st =
  List.fold_left
    (fun acc fi ->
       if fi.jc_field_info_rep then
	 match fi.jc_field_info_type with
	   | JCTpointer(pc, _, _) -> (fi, pc)::acc
	   | _ -> acc
       else acc)
    []
    st.jc_struct_info_fields

let components_by_type st =
  let compare_pcs s t =
    compare (pointer_class_type_name s) (pointer_class_type_name t) in
  let comps = components st in
  let comps =
    List.sort
      (fun (_, s) (_, t) -> compare_pcs s t)
      comps
  in
  let rec part prev acc = function
    | [] -> [prev, acc]
    | (fi, si)::tl ->
	if compare_pcs si prev = 0 then
	  part prev (fi::acc) tl
	else
	  (prev, acc)::(part si [fi] tl)
  in
  let part = function
    | [] -> []
    | (fi, si)::tl -> part si [fi] tl
  in
  part comps

(* return a post-condition stating that the committed
field of each component of "this" (in "fields") of the
hierarchy "root" has been set to "value", and only them *)
let hierarchy_committed_postcond this root fields value =
  let com = committed_name root in
  let ac = alloc_class_of_pointer_class root in
  let alloc = generic_alloc_table_name ac in
  (* fields information and range *)
  let fields = List.map
    (fun fi ->
       let this_fi = make_select_fi fi this in
       let omin, omax = omin_omax (LVar alloc) this_fi
	 (range_min fi.jc_field_info_type)
	 (range_max fi.jc_field_info_type)
       in
       fi, this_fi, omin, omax)
    fields
  in
  (* pset of pointers that have been modified *)
  let pset_list = List.map
    (fun (fi, this_fi, omin, omax) ->
       (pset_range (pset_singleton this_fi) omin omax))
    fields
  in
  let pset = pset_union_list pset_list in
  (* "not_assigns" saying that only the pointers of pset
     have been modified *)
  let not_assigns = make_not_assigns (LVar alloc)
    (LVarAtLabel(com, ""))
    (LVar com)
    pset
  in
  (* new values for the fields in their ranges *)
  let com_values = List.map
    (fun (fi, this_fi, omin, omax) ->
       let fi_pc = type_pc fi.jc_field_info_type in
       let index = "jc_index" in
       let range = make_range (LVar index) omin omax in
       let new_value = make_eq
	 (make_select_committed fi_pc
	    (make_shift this_fi (LVar index)))
	 (LConst(Prim_bool value))
       in
       LForall(index, simple_logic_type "int", LImpl(range, new_value)))
    fields
  in
  (* result *)
  make_and_list (not_assigns::com_values)

(* all components have "committed" = committed *)
let make_components_postcond this st reads writes committed =
  let comps = components_by_type st in
  let writes =
    List.fold_left
      (fun acc (pc, _) -> StringSet.add (committed_name pc) acc)
      writes
      comps
  in
  let reads =
    List.fold_left
      (fun acc (_, fields) ->
	 List.fold_left
	   (fun acc fi -> StringSet.add fi.jc_field_info_final_name acc)
	   acc
	   fields)
      reads
      comps
  in
  let reads = StringSet.union reads writes in
  let reads = List.fold_left
    (fun acc (pc, fields) -> StringSet.add
       (generic_alloc_table_name (alloc_class_of_pointer_class pc)) acc)
    reads comps
  in
  let postcond =
    make_and_list
      (* for each hierarchy... *)
      (List.map
	 (fun (pc, fields) -> hierarchy_committed_postcond
	    this pc fields committed)
	 comps)
  in
  postcond, reads, writes  

(* all components must have mutable = committed = false (for pack) *)
let make_components_precond this st reads =
  let comps = components st in
  let reads =
    List.fold_left
      (fun acc (fi, _) -> StringSet.add fi.jc_field_info_final_name acc)
      reads
      comps
  in
  let l, reads = List.fold_left
    (fun (l, reads) (fi, si) ->
       let index_name = "jc_index" in
       let mutable_name = mutable_name si in
       let committed_name = committed_name si in
       (* x.f *)
       let base_field =
	 LApp("select", [LVar fi.jc_field_info_final_name; this])
       in
       (* x.f+i *)
       let this_field =
	 LApp("shift", [ base_field; LVar index_name ])
       in
       let fi_pc = type_pc fi.jc_field_info_type in
       let fi_ac = alloc_class_of_pointer_class fi_pc in
       let alloc = generic_alloc_table_name fi_ac in
       let reads = StringSet.add (generic_tag_table_name (pointer_class_root fi_pc)) reads in
       let reads = StringSet.add alloc reads in
       let reads = StringSet.add mutable_name reads in
       (* pre-condition: forall i, valid(x.f+i) => fp(x.f+i) /\ not committed(x.f+i) *)
       let body = make_and
	 (fully_packed fi_pc this_field)
	 (LPred(
	    "eq",
	    [ LApp("select", [LVar committed_name; this_field]);
	      LConst(Prim_bool false) ]))
       in
       let omin, omax = omin_omax (LVar alloc) base_field
	 (range_min fi.jc_field_info_type)
	 (range_max fi.jc_field_info_type)
       in
       let range = make_range (LVar index_name) omin omax in
       let valid_impl = LImpl(range, body) in
       let forall = LForall(index_name, simple_logic_type "int", valid_impl) in
       forall::l, reads)
    ([], reads)
    (components st)
  in
  make_and_list l, reads

let pack_declaration st acc =
  let this = "this" in
  let pc = JCtag(st, []) in
  let ac = alloc_class_of_pointer_class pc in
  let this_type = pointer_type ac pc in
  let tag = "tag" in
  let tag_type = tag_id_type (struct_root st) in
  let mutable_name = mutable_name (JCtag(st, [])) in
  let inv, reads = invariant_for_struct (LVar this) st in
  let writes = StringSet.empty in
  let components_post, reads, writes = make_components_postcond (LVar this) st reads writes true in
  let components_pre, reads = make_components_precond (LVar this) st reads in
  let reads = StringSet.add mutable_name reads in
  let writes = StringSet.add mutable_name writes in
  let requires =
    make_and_list [
      (LPred(
	 "parenttag",
	 [ LVar tag;
	   LApp("select", [LVar mutable_name; LVar this]) ]));
      inv;
      components_pre
    ]
  in
  let ensures =
    make_and
      (LPred(
	 "eq",
	 [ LVar mutable_name;
	   LApp(
	     "store",
	     [ LVarAtLabel(mutable_name, "");
	       LVar this;
	       LVar tag ])]))
      components_post
  in
  let annot_type =
    Annot_type(
      requires,
      Base_type (simple_logic_type "unit"),
      StringSet.elements reads,
      StringSet.elements writes,
      ensures,
      []
    )
  in
  if st.jc_struct_info_parent = None then
    Param(
      false,
      pack_name st,
      Prod_type(
	this,
	Base_type this_type,
	Prod_type(
	  tag,
	  Base_type tag_type,
	  annot_type
	)
      )
    )::acc
  else
    acc

(* Unlike Boogie, Jessie has "unpack to S" instead of "unpack from T" *)
let unpack_declaration st acc =
  let this = "this" in
  let pc = JCtag(st, []) in
  let ac = alloc_class_of_pointer_class pc in
  let this_type = pointer_type ac pc in
  let tag = "tag" in
  let tag_type = tag_id_type (struct_root st) in
  let mutable_name = mutable_name (JCtag(st, [])) in
  let committed_name = committed_name (JCtag(st, [])) in
  let reads = StringSet.singleton mutable_name in
  let writes = StringSet.singleton mutable_name in
  let reads = StringSet.add committed_name reads in
  let components_post, reads, writes = make_components_postcond (LVar this) st reads writes false in
  let requires =
    (* unpack this as tag: requires parenttag(this.mutable, tag) and not this.committed *)
    make_and
      (LPred(
	 "parenttag",
	 [ LApp("select", [LVar mutable_name; LVar this]);
	   LVar tag ]))
      (LPred(
	 "eq",
	 [ LConst(Prim_bool false);
	   LApp("select", [LVar committed_name; LVar this]) ]))
  in
  let ensures =
    make_and
      (LPred(
	 "eq",
	 [ LVar mutable_name;
	   LApp(
	     "store",
	     [ LVarAtLabel(mutable_name, "");
	       LVar this;
	       LVar tag ])]))
      components_post
  in
  let annot_type =
    Annot_type(
      requires,
      Base_type (simple_logic_type "unit"),
      StringSet.elements reads,
      StringSet.elements writes,
      ensures,
      []
    )
  in
  if st.jc_struct_info_parent = None then
    Param(
      false,
      "unpack_"^(root_name st),
      Prod_type(
	this,
	Base_type this_type,
	Prod_type(
	  tag,
	  Base_type tag_type,
	  annot_type
	)
      )
    )::acc
  else
    acc

(*********************************************************************)
(*               Using a recursively-defined predicate               *)
(*********************************************************************)
(*let valid_inv_name st = st.jc_struct_info_name ^ "_inv"

let valid_inv_axiom_name st = st.jc_struct_info_name ^ "_inv_sem"

let rec struct_depends st acc mem =
  let name = st.jc_struct_info_name in
  if StringSet.mem name mem then acc, mem else
  let acc, mem = List.fold_left (fun (acc, mem) (_, fi) -> match fi.jc_field_info_type with
      | JCTpointer(st, _, _) -> struct_depends st acc mem
      | JCTnull -> assert false
      | JCTnative _ | JCTlogic _ | JCTrange _ -> acc, mem)
    (st::acc, StringSet.add name mem) st.jc_struct_info_fields
  in
  match st.jc_struct_info_parent with
    None -> acc, mem
  | Some pst -> struct_depends pst acc mem

let struct_depends =
  let table = Hashtbl.create 97 in fun st ->
  let name = st.jc_struct_info_name in
  try Hashtbl.find table name with Not_found ->
  let result = fst (struct_depends st [] StringSet.empty) in
  Hashtbl.add table name result;
  result

(* "this" is not returned in the list of parameters of valid_inv_params *)
let valid_inv_params st =
  let deps = struct_depends st in
  let memories = List.fold_left (fun acc st ->
    List.fold_left (fun acc (_, fi) ->
      (fi.jc_field_info_final_name, memory_field fi)::acc) acc st.jc_struct_info_fields)
    [] deps in
  let params = List.fold_left invariants_params memories deps in
  let params = List.sort (fun (name1, _) (name2, _) ->
    compare name2 name1) params in
  let rec only_one prev acc = function
    [] -> acc
  | ((name, _) as x)::tl ->
      if name = prev then only_one prev acc tl
      else only_one name (x::acc) tl in
  let params = only_one "" [] params in
    params

(* generate valid_inv predicate and its axiom *)
let tr_valid_inv st acc =
  let params = valid_inv_params st in

  (**** valid_inv predicate declaration ****)
  let valid_inv_type = simple_logic_type "prop" in
  let vi_this = "???",
    { logic_type_name = "pointer" ;
      logic_type_args = [simple_logic_type st.jc_struct_info_hroot] } in
  let logic = Logic(false, valid_inv_name st, vi_this::params,
    valid_inv_type) in
  let acc = logic::acc in

  (**** valid_inv_sem axiom ****)
  let this = "inv_this" in
  let this_var = LVar this in
  let this_ty =
    { logic_type_name = "pointer";
      logic_type_args = [simple_logic_type st.jc_struct_info_hroot] } in
  let fields_valid_inv = List.map (fun (_, fi) ->
    match fi.jc_field_info_type with
    | JCTpointer(st, _, _) ->
        let params = valid_inv_params st in
        let params_var = List.map (fun (name, _) -> LVar name) params in
        LPred(valid_inv_name st,
          LApp("select", [LVar fi.jc_field_info_final_name; this_var])::params_var)
    | JCTnull -> assert false
    | JCTnative _
    | JCTlogic _
    | JCTrange _ -> LTrue) st.jc_struct_info_fields in
  let params_var = List.map (fun (name, _) -> LVar name) params in
  let sem = LIff(LPred(valid_inv_name st, this_var::params_var),
    LImpl(LPred("neq", [LVar this; LVar "null"]),
      make_and (make_and_list fields_valid_inv) (invariant_for_struct this_var st))) in
  (* parent invariant *)
  let sem = match st.jc_struct_info_parent with
    None -> sem
  | Some pst ->
      let parent_params = valid_inv_params pst in
      let parent_params_var = List.map (fun (name, _) -> LVar name) parent_params in
      make_and sem (LPred(valid_inv_name pst, this_var::parent_params_var))
  in
  (* quantifiers *)
  let sem = List.fold_left (fun acc (id, ty) ->
    LForall(id, ty, acc)) sem ((this, this_ty)::params) in
  Axiom(valid_inv_axiom_name st, sem)::acc*)

let rec invariant_for_struct ?pos this si =
  let _, invs = 
    Hashtbl.find Jc_typing.structs_table si.jc_struct_info_name 
  in
  let invs = Assertion.mkand ?pos
    ~conjuncts:(List.map 
		  (fun (li, _) -> 
		     let a = { jc_app_fun = li;
			       jc_app_args = [this];
			       jc_app_label_assoc = [];
			       jc_app_region_assoc = [] }
		     in
		       new assertion ?pos (JCAapp a)) invs) 
    ()
  in
    match si.jc_struct_info_parent with
      | None -> invs
      | Some(si, _) -> (* add invariants from the type hierarchy *)
	  let this =
	    match this#typ with
	      | JCTpointer (_, a, b) ->
		  new term_with ~typ:(JCTpointer (JCtag(si, []), a, b)) this
	      | _ -> assert false (* never happen *)
	  in
	    Assertion.mkand ?pos
	      ~conjuncts:[invs; (invariant_for_struct ?pos this si)]
	      ()
	      
let code_function (fi, pos, fs, sl) vil =
  begin
    match !Jc_common_options.inv_sem with
      | InvArguments ->
	  (* apply arguments invariant policy *)
	  let invariants =
	    (* Calculate global invariants. *)
	    let _vitl = 
	      List.map 
		(fun vi -> Term.mkvar ~var:vi ()) vil 
	    in
	    let global_invariants =
	      Hashtbl.fold
		(fun li _ acc -> 
		   (* li.jc_logic_info_parameters <- vil; *)
		   let a = { jc_app_fun = li;
			     jc_app_args = (* vitl *)[];
			     jc_app_label_assoc = [];
			     jc_app_region_assoc = [] }
		   in
		     (new assertion ~mark:(Jc_pervasives.new_label_name ())
			~pos (JCAapp a)) :: acc)
		Jc_typing.global_invariants_table []
	    in
	    let global_invariants = 
	      Assertion.mkand ~pos ~conjuncts:global_invariants ()
	    in
	      (* Calculate invariants for each parameter. *)
	    let invariants =
	      List.fold_left
		(fun acc vi ->
		   match vi.jc_var_info_type with
		     | JCTpointer (JCtag (st, []), _, _) ->
			 Assertion.mkand ~pos
			   ~conjuncts:
			   [acc; (invariant_for_struct ~pos
				    (Term.mkvar ~var:vi ()) st)]
			   ()
		     | _ -> acc)
		(Assertion.mktrue ())
		fi.jc_fun_info_parameters
	    in
	    Assertion.mkand ~pos ~conjuncts:[global_invariants; invariants] ()
	  in
	    (* add invariants to the function precondition *)
	  fs.jc_fun_requires <- 
	    Assertion.mkand ~pos ~conjuncts:[fs.jc_fun_requires; invariants] ();
	    (* add invariants to the function postcondition *)
	  if is_purely_exceptional_fun fs then () else
	    let safety_exists = ref false in
	    let post = invariants in
	    List.iter
	      (fun (_, s, b) ->
		 if s = "safety" then safety_exists := true;
		 b.jc_behavior_ensures <- 
		   Assertion.mkand ~pos ~conjuncts:[b.jc_behavior_ensures; post] ())
	      fs.jc_fun_behavior;
	    (* add the 'safety' spec if it does not exist 
	       (it could exist e.g. from Krakatoa) *)
	    if not !safety_exists then
	      if Jc_options.verify_invariants_only then
		let invariants_b = { default_behavior with jc_behavior_ensures = post } in
		fs.jc_fun_behavior <- 
		  (Loc.dummy_position, "invariants", invariants_b) :: fs.jc_fun_behavior;
	      else
		let safety_b = { default_behavior with jc_behavior_ensures = post } in
		fs.jc_fun_behavior <- 
		  (Loc.dummy_position, "safety", safety_b) :: fs.jc_fun_behavior;
      | _ -> ()
  end;


(*
Local Variables: 
compile-command: "unset LANG; make -C .. bin/jessie.byte"
End: 
*)
