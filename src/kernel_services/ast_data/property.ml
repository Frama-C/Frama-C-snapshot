(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil_types
open Cil_datatype

type behavior_or_loop =
    Id_behavior of funbehavior
  | Id_code_annot of code_annotation

type identified_complete = kernel_function * kinstr * string list
type identified_disjoint =  identified_complete
type identified_code_annotation =
    kernel_function * stmt * code_annotation

type identified_allocation =
    kernel_function 
    * kinstr 
    * behavior_or_loop 
    * (identified_term list * identified_term list)

type identified_assigns =
    kernel_function
    * kinstr
    * behavior_or_loop
    * identified_term from list

type identified_from =
    kernel_function
    * kinstr
    * behavior_or_loop
    * (identified_term from (* * identified_term list *) )

type identified_decrease =
    kernel_function * kinstr * code_annotation option * term variant
type identified_behavior = kernel_function * kinstr * funbehavior

type predicate_kind =
  | PKRequires of funbehavior
  | PKAssumes of funbehavior
  | PKEnsures of funbehavior * termination_kind
  | PKTerminates

let pretty_predicate_kind fmt = function
  | PKRequires _ -> Format.pp_print_string fmt "requires"
  | PKAssumes _ -> Format.pp_print_string fmt "assumes"
  | PKEnsures(_, tk)  ->
    Format.pp_print_string fmt
      (match tk with
      | Normal -> "ensures"
      | Exits -> "exits"
      | Breaks -> "breaks"
      | Continues -> "continues"
      | Returns -> "returns")
  | PKTerminates -> Format.pp_print_string fmt "terminates"

type identified_predicate =
    predicate_kind * kernel_function * kinstr * Cil_types.identified_predicate

type program_point = Before | After

type identified_reachable = kernel_function option * kinstr * program_point

type identified_type_invariant = string * typ * predicate named * location

type identified_global_invariant = string * predicate named * location

and identified_axiomatic = string * identified_property list

and identified_lemma = 
    string * logic_label list * string list * predicate named * location

and identified_axiom = identified_lemma

and identified_instance =
    kernel_function option * kinstr * identified_property

and identified_property =
  | IPPredicate of identified_predicate
  | IPAxiom of identified_axiom
  | IPAxiomatic of identified_axiomatic
  | IPLemma of identified_lemma
  | IPBehavior of identified_behavior
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPAllocation of identified_allocation
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease
  | IPReachable of identified_reachable
  | IPPropertyInstance of identified_instance
  | IPTypeInvariant of identified_type_invariant
  | IPGlobalInvariant of identified_global_invariant
  | IPOther of string * kernel_function option * kinstr 

let get_kinstr = function
  | IPPredicate (_,_,ki,_) 
  | IPBehavior(_, ki, _) 
  | IPComplete (_,ki,_)
  | IPDisjoint(_,ki,_) 
  | IPAllocation (_,ki,_,_) 
  | IPAssigns (_,ki,_,_) 
  | IPFrom(_,ki,_,_)
  | IPReachable (_, ki, _)
  | IPDecrease (_,ki,_,_)
  | IPPropertyInstance (_, ki, _) -> ki
  | IPAxiom _ 
  | IPAxiomatic _
  | IPLemma _  -> Kglobal
  | IPOther(_,_,ki) -> ki
  | IPCodeAnnot (_,s,_) -> Kstmt s
  | IPTypeInvariant _ | IPGlobalInvariant _ -> Kglobal

let get_kf = function
  | IPPredicate (_,kf,_,_)
  | IPBehavior(kf, _, _)
  | IPCodeAnnot (kf,_,_)
  | IPComplete (kf,_,_) 
  | IPDisjoint(kf,_,_) 
  | IPAllocation(kf,_,_,_)
  | IPAssigns(kf,_,_,_)
  | IPFrom(kf,_,_,_) 
  | IPDecrease (kf,_,_,_) -> Some kf
  | IPAxiom _ 
  | IPAxiomatic _
  | IPLemma _ -> None
  | IPReachable (kfopt, _, _)
  | IPPropertyInstance (kfopt, _, _)
  | IPOther(_,kfopt,_) -> kfopt
  | IPTypeInvariant _ | IPGlobalInvariant _ -> None

let loc_of_kf_ki kf = function
  | Kstmt s -> Cil_datatype.Stmt.loc s
  | Kglobal -> Kernel_function.get_location kf

let rec location = function
  | IPPredicate (_,_,_,ip) -> ip.ip_loc
  | IPBehavior(kf,ki, _) 
  | IPComplete (kf,ki,_) 
  | IPReachable(Some kf, ki, _) -> loc_of_kf_ki kf ki
  | IPPropertyInstance (Some kf, ki, _)
  | IPDisjoint(kf,ki,_) -> loc_of_kf_ki kf ki
  | IPPropertyInstance (None, Kstmt s, _)
  | IPReachable(None, Kstmt s, _) -> Cil_datatype.Stmt.loc s
  | IPCodeAnnot (_,s,ca) -> (
    match Cil_datatype.Code_annotation.loc ca with
    | None -> Cil_datatype.Stmt.loc s
    | Some loc -> loc)
  | IPPropertyInstance (None, Kglobal, _)
  | IPReachable(None, Kglobal, _) -> Cil_datatype.Location.unknown
  | IPAssigns(kf,ki,_,a) ->
    (match a with
      | [] -> loc_of_kf_ki kf ki
      | (t,_) :: _ -> t.it_content.term_loc)
  | IPAllocation(kf,ki,_,fa) ->
    (match fa with
      | [],[] -> loc_of_kf_ki kf ki
      | (t :: _),_ 
      | _,(t :: _) -> t.it_content.term_loc)
  | IPFrom(_,_,_,(t,_)) -> t.it_content.term_loc
  | IPDecrease (_,_,_,(t,_)) -> t.term_loc
  | IPAxiom (_,_,_,_,loc) -> loc
  | IPAxiomatic (_,l) ->
    (match l with
      | [] -> Cil_datatype.Location.unknown
      | p :: _ -> location p)
  | IPLemma (_,_,_,_,loc) -> loc
  | IPOther(_,kf,ki) -> 
    (match kf with
      | None -> Cil_datatype.Location.unknown
      | Some kf -> loc_of_kf_ki kf ki)
  | IPTypeInvariant(_,_,_,loc) | IPGlobalInvariant(_,_,loc) -> loc

(* Pretty information about the localization of a IPPropertyInstance *)
let pretty_instance_location fmt (kfopt, ki) =
  match kfopt, ki with
  | None, Kglobal -> Format.pp_print_string fmt "at global scope"
  | Some kf, Kglobal ->
    Format.fprintf fmt "in function %a" Kernel_function.pretty kf
  | None, Kstmt stmt -> Format.fprintf fmt "at stmt %d" stmt.sid
  | Some kf, Kstmt stmt
    when Kernel_function.(equal kf (find_englobing_kf stmt)) ->
    Format.fprintf fmt "at stmt %d" stmt.sid
  | Some kf, Kstmt stmt ->
    Format.fprintf fmt "at stmt %d and function %a"
      stmt.sid Kernel_function.pretty kf


let get_pk_behavior = function
  | PKRequires b | PKAssumes b | PKEnsures (b,_) -> Some b
  | PKTerminates -> None

let get_behavior = function
  | IPPredicate (pk,_,_,_) -> get_pk_behavior pk
  | IPBehavior(_, _, b) -> Some b
  | IPAllocation(_,_,Id_behavior b,_)
  | IPAssigns(_,_,Id_behavior b,_)
  | IPFrom(_,_,Id_behavior b,_) -> Some b
  | IPAllocation(_,_,Id_code_annot _,_)
  | IPAssigns(_,_,Id_code_annot _,_)
  | IPFrom(_,_,Id_code_annot _,_)
  | IPAxiom _ 
  | IPAxiomatic _
  | IPLemma _
  | IPCodeAnnot (_,_,_)
  | IPComplete (_,_,_)
  | IPDisjoint(_,_,_)
  | IPDecrease _
  | IPReachable _
  | IPPropertyInstance _
  | IPTypeInvariant _
  | IPGlobalInvariant _
  | IPOther _ -> None

include Datatype.Make_with_collections
    (struct
      
      include Datatype.Serializable_undefined 

      type t = identified_property
      let name = "Property.t"
      let reprs = [ IPAxiom ("",[],[],Logic_const.ptrue,Location.unknown) ]
      let mem_project = Datatype.never_any_project

      let equal_opt eq a b =
	match a,b with
	  | None,None -> true
	  | Some _,None | None,Some _ -> false
	  | Some x , Some y -> eq x y

      let compare_opt cmp a b =
	match a,b with
	  | None,None -> 0
	  | None,Some _ -> (-1)
	  | Some _,None -> 1
	  | Some x,Some y -> cmp x y
      
      let rec pretty fmt = function
	| IPPredicate (kind,_,_,p) -> 
          Format.fprintf fmt "%a@ %a"
            pretty_predicate_kind kind Cil_printer.pp_identified_predicate p
	| IPAxiom (s,_,_,_,_) -> Format.fprintf fmt "axiom@ %s" s
	| IPAxiomatic(s, _) -> Format.fprintf fmt "axiomatic@ %s" s
	| IPLemma (s,_,_,_,_) -> Format.fprintf fmt "lemma@ %s" s
	| IPTypeInvariant(s,ty,_,_) ->
	  Format.fprintf fmt "invariant@ %s for type %a" s Cil_printer.pp_typ ty
	| IPGlobalInvariant(s,_,_) ->
	  Format.fprintf fmt "global invariant@ %s" s
	| IPBehavior(_kf, ki, b) -> 
            if Cil.is_default_behavior b then
              Format.pp_print_string fmt "default behavior"
            else
	      Format.fprintf fmt "behavior %s" b.b_name;
	    (match ki with 
	    | Kstmt s -> Format.fprintf fmt " for statement %d" s.sid
	    | Kglobal -> ())
	| IPCodeAnnot(_, _, a) -> Cil_printer.pp_code_annotation fmt a
	| IPComplete(_, _, l) ->
	  Format.fprintf fmt "complete@ %a"
	    (Pretty_utils.pp_list ~sep:","
	       (fun fmt s ->  Format.fprintf fmt "@ %s" s))
	    l
	| IPDisjoint(_, _, l) ->
	  Format.fprintf fmt "disjoint@ %a"
	    (Pretty_utils.pp_list ~sep:","
	       (fun fmt s ->  Format.fprintf fmt " %s" s))
	    l
	| IPAllocation(_, _, _, (f,a)) -> 
	    Cil_printer.pp_allocation fmt (FreeAlloc(f,a))
	| IPAssigns(_, _, _, l) -> Cil_printer.pp_assigns fmt (Writes l)
	| IPFrom (_,_,_, f) -> Cil_printer.pp_from fmt f
	| IPDecrease(_, _, None,v) -> Cil_printer.pp_decreases fmt v
	| IPDecrease(_, _, _,v) -> Cil_printer.pp_variant fmt v
	| IPReachable(None, Kstmt _, _) ->  assert false
	| IPReachable(None, Kglobal, _) -> 
	  Format.fprintf fmt "reachability of entry point"
	| IPReachable(Some kf, Kglobal, _) -> 
	  Format.fprintf fmt "reachability of function %a" Kf.pretty kf
	| IPReachable(Some kf, Kstmt stmt, ba) -> 
	  Format.fprintf fmt "reachability %s stmt %a in %a" 
	    (match ba with Before -> "of" | After -> "post")
	    Cil_datatype.Location.pretty_line (Cil_datatype.Stmt.loc stmt)
	    Kf.pretty kf
	| IPPropertyInstance (kfopt, ki, ip) ->
	  Format.fprintf fmt "status of '%a'%t %a"
	    pretty ip
	    (fun fmt -> match get_kf ip with
	    | Some kf -> Format.fprintf fmt " of %a" Kernel_function.pretty kf
	    | None -> ())
	    pretty_instance_location (kfopt, ki)
	| IPOther(s,_,_) -> Format.pp_print_string fmt s

      let rec hash =
	let hash_bhv_loop = function
          | Id_behavior b -> (0, Hashtbl.hash b.b_name)
          | Id_code_annot ca -> (1, ca.annot_id)
	in
	function
	| IPPredicate (_,_,_,x) -> Hashtbl.hash (1, x.ip_id)
	| IPAxiom (x,_,_,_,_) -> Hashtbl.hash (2, (x:string))
	| IPAxiomatic (x,_) -> Hashtbl.hash (3, (x:string))
	| IPLemma (x,_,_,_,_) -> Hashtbl.hash (4, (x:string))
	| IPCodeAnnot(_,_, ca) -> Hashtbl.hash (5, ca.annot_id)
	| IPComplete(f, ki, x) ->
	  Hashtbl.hash (6, Kf.hash f, Kinstr.hash ki, (x:string list))
	| IPDisjoint(f, ki, x) ->
	  Hashtbl.hash(7, Kf.hash f, Kinstr.hash ki, (x:string list))
	| IPAssigns(f, ki, b, _l) ->
	  Hashtbl.hash (8, Kf.hash f, Kinstr.hash ki, hash_bhv_loop b)
	| IPFrom(kf,ki,b,(t,_)) ->
          Hashtbl.hash
            (9, Kf.hash kf, Kinstr.hash ki,
             hash_bhv_loop b, Identified_term.hash t)
	| IPDecrease(kf, ki, _ca, _v) ->
        (* At most one loop variant per statement anyway, no
           need to discriminate against the code annotation itself *)
	  Hashtbl.hash (10, Kf.hash kf, Kinstr.hash ki)
	| IPBehavior(kf, s, b) -> 
	  Hashtbl.hash (11, Kf.hash kf, Kinstr.hash s, (b.b_name:string))
	| IPReachable(kf, ki, ba) -> 
	  Hashtbl.hash(12, Extlib.may_map Kf.hash ~dft:0 kf,
                       Kinstr.hash ki, Hashtbl.hash ba)
	| IPAllocation(f, ki, b, _fa) ->
	  Hashtbl.hash (13, Kf.hash f, Kinstr.hash ki, hash_bhv_loop b)
	| IPPropertyInstance (kf_caller, ki, ip) ->
	  Hashtbl.hash (14, Extlib.opt_hash Kf.hash kf_caller,
	                Kinstr.hash ki, hash ip)
	| IPOther(s,_,_) -> Hashtbl.hash (15, (s:string))
	| IPTypeInvariant(s,_,_,_) -> Hashtbl.hash (16, (s:string))
	| IPGlobalInvariant(s,_,_) -> Hashtbl.hash (17, (s:string))

      let rec equal p1 p2 =
	let eq_bhv (f1,ki1,b1) (f2,ki2,b2) =
	  Kf.equal f1 f2 && Kinstr.equal ki1 ki2
	  && 
            (match b1, b2 with
            | Id_code_annot ca1, Id_code_annot ca2 ->
              ca1.annot_id = ca2.annot_id
            | Id_behavior b1, Id_behavior b2 -> b1.b_name = b2.b_name
            | Id_code_annot _, Id_behavior _
            | Id_behavior _, Id_code_annot _ -> false)
	in
	match p1, p2 with
	| IPPredicate (_,_,_,s1), IPPredicate (_,_,_,s2) -> s1.ip_id = s2.ip_id
	| IPAxiom (s1,_,_,_,_), IPAxiom (s2,_,_,_,_)
	| IPAxiomatic(s1, _), IPAxiomatic(s2, _)
	| IPTypeInvariant(s1,_,_,_), IPTypeInvariant(s2,_,_,_)
	| IPGlobalInvariant(s1,_,_), IPGlobalInvariant(s2,_,_)
	| IPLemma (s1,_,_,_,_), IPLemma (s2,_,_,_,_) -> 
	  Datatype.String.equal s1 s2
	| IPCodeAnnot(_,_,ca1), IPCodeAnnot(_,_,ca2) ->
          ca1.annot_id = ca2.annot_id
	| IPComplete(f1, ki1, x1), IPComplete(f2, ki2, x2)
	| IPDisjoint(f1, ki1, x1), IPDisjoint(f2, ki2, x2) ->
	  Kf.equal f1 f2 && Kinstr.equal ki1 ki2 && x1 = x2
	| IPAllocation (f1, ki1, b1, _), IPAllocation (f2, ki2, b2, _) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2)
	| IPAssigns (f1, ki1, b1, _), IPAssigns (f2, ki2, b2, _) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2)
	| IPFrom (f1,ki1,b1,(t1,_)), IPFrom (f2, ki2,b2,(t2,_)) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2) && t1.it_id = t2.it_id
	| IPDecrease(f1, ki1, _, _), IPDecrease(f2, ki2, _, _) ->
	  Kf.equal f1 f2 && Kinstr.equal ki1 ki2
	| IPReachable(kf1, ki1, ba1), IPReachable(kf2, ki2, ba2) ->
	  Extlib.opt_equal Kf.equal kf1 kf2 && Kinstr.equal ki1 ki2 && ba1 = ba2
	| IPBehavior(f1, k1, b1), IPBehavior(f2, k2, b2) ->
	  Kf.equal f1 f2
	  && Kinstr.equal k1 k2 
	  && Datatype.String.equal b1.b_name b2.b_name
	| IPOther(s1,kf1,ki1), IPOther(s2,kf2,ki2) -> 
	    Datatype.String.equal s1 s2 
	    && Kinstr.equal ki1 ki2
	    && equal_opt Kf.equal kf1 kf2
	| IPPropertyInstance (kf1, ki1, ip1),
	  IPPropertyInstance (kf2, ki2, ip2) ->
	  Extlib.opt_equal Kernel_function.equal kf1 kf2 &&
	  Kinstr.equal ki1 ki2 && equal ip1 ip2
	| (IPPredicate _ | IPAxiom _ | IPAxiomatic _ | IPLemma _ 
	      | IPCodeAnnot _ | IPComplete _ | IPDisjoint _ | IPAssigns _ 
	      | IPFrom _ | IPDecrease _ | IPBehavior _ | IPReachable _ 
	      | IPAllocation _ | IPOther _ | IPPropertyInstance _
	      | IPTypeInvariant _ | IPGlobalInvariant _), _ -> false

      let rec compare x y =
	let cmp_bhv (f1,ki1,b1) (f2,ki2,b2) =
	  let n = Kf.compare f1 f2 in
	  if n = 0 then
	    let n = Kinstr.compare ki1 ki2 in
	    if n = 0 then
	      match b1, b2 with
	      | Id_behavior b1, Id_behavior b2 ->
		Datatype.String.compare b1.b_name b2.b_name
              | Id_code_annot ca1, Id_code_annot ca2 ->
		Datatype.Int.compare ca1.annot_id ca2.annot_id
              | Id_behavior _, Id_code_annot _ -> -1
              | Id_code_annot _, Id_behavior _ -> 1
            else n
          else n
	in
	match x, y with
	| IPPredicate (_,_,_,s1), IPPredicate (_,_,_,s2) ->
          Datatype.Int.compare s1.ip_id s2.ip_id
	| IPCodeAnnot(_,_,ca1), IPCodeAnnot(_,_,ca2) ->
          Datatype.Int.compare ca1.annot_id ca2.annot_id
	| IPBehavior(f1, k1, b1), IPBehavior(f2, k2, b2) -> 
	  cmp_bhv (f1, k1, Id_behavior b1) (f2, k2, Id_behavior b2)
	| IPComplete(f1, ki1, x1), IPComplete(f2, ki2, x2)
	| IPDisjoint(f1, ki1, x1), IPDisjoint(f2, ki2, x2) ->
          let n = Kf.compare f1 f2 in
          if n = 0 then
            let n = Kinstr.compare ki1 ki2 in
            if n = 0 then Extlib.compare_basic x1 x2 else n
          else n
	| IPAssigns (f1, ki1, b1, _), IPAssigns (f2, ki2, b2, _) ->
          cmp_bhv (f1,ki1,b1) (f2,ki2,b2)
	| IPFrom (f1,ki1,b1,(t1,_)), IPFrom(f2,ki2,b2,(t2,_)) ->
          let n = cmp_bhv (f1,ki1,b1) (f2,ki2,b2) in
          if n = 0 then Identified_term.compare t1 t2 else n
	| IPDecrease(f1, ki1,_,_), IPDecrease(f2, ki2,_,_) ->
	  let n = Kf.compare f1 f2 in
	  if n = 0 then Kinstr.compare ki1 ki2 else n
	| IPReachable(kf1, ki1, ba1), IPReachable(kf2, ki2, ba2) ->
	  let n = Extlib.opt_compare Kf.compare kf1 kf2 in
	  if n = 0 then 
	    let n = Kinstr.compare ki1 ki2 in
	    if n = 0 then Pervasives.compare ba1 ba2 else n
	  else 
	    n
	| IPAxiom (s1,_,_,_,_), IPAxiom (s2,_,_,_,_)
	| IPAxiomatic(s1, _), IPAxiomatic(s2, _)
	| IPTypeInvariant(s1,_,_,_), IPTypeInvariant(s2,_,_,_)
	| IPLemma (s1,_,_,_,_), IPLemma (s2,_,_,_,_) ->
	    Datatype.String.compare s1 s2
	| IPOther(s1,kf1,ki1), IPOther(s2,kf2,ki2) -> 
	    let s = Datatype.String.compare s1 s2 in
	    if s <> 0 then s else
	      let s = compare_opt Kf.compare kf1 kf2 in
	      if s <> 0 then s else
		Kinstr.compare ki1 ki2
	| IPAllocation (f1, ki1, b1, _), IPAllocation (f2, ki2, b2, _) ->
            cmp_bhv (f1,ki1,b1) (f2,ki2,b2)
	| IPPropertyInstance (kf1, ki1, ip1),
	  IPPropertyInstance (kf2, ki2, ip2) ->
	  let c = Extlib.opt_compare Kernel_function.compare kf1 kf2 in
	  if c <> 0 then c else
	    let c = Kinstr.compare ki1 ki2 in
	    if c <> 0 then c else compare ip1 ip2
	| (IPPredicate _ | IPCodeAnnot _ | IPBehavior _ | IPComplete _ |
            IPDisjoint _ | IPAssigns _ | IPFrom _ | IPDecrease _ |
            IPReachable _ | IPAxiom _ | IPAxiomatic _ | IPLemma _ |
            IPOther _ | IPAllocation _ | IPPropertyInstance _ |
	    IPTypeInvariant _ | IPGlobalInvariant _) as x, y ->
          let nb = function
            | IPPredicate _ -> 1
            | IPAssigns (_, _, _, _) -> 2
            | IPDecrease _ -> 3
            | IPAxiom _ -> 4
            | IPAxiomatic _ -> 5
	    | IPLemma _ -> 6
            | IPCodeAnnot _ -> 7
            | IPComplete (_, _, _) -> 8
            | IPDisjoint (_, _, _) -> 9
            | IPFrom _ -> 10
	    | IPBehavior _ -> 11
	    | IPReachable _ -> 12
	    | IPAllocation _ -> 13
	    | IPOther _ -> 14
	    | IPPropertyInstance _ -> 15
	    | IPTypeInvariant _ -> 16
	    | IPGlobalInvariant _ -> 17
	  in
	  Datatype.Int.compare (nb x) (nb y)

     end)

let rec short_pretty fmt p = match p with
  | IPPredicate (_,_,_,{ ip_name = name :: _ }) ->
    Format.pp_print_string fmt name
  | IPPredicate _ -> pretty fmt p
  | IPAxiom (name,_,_,_,_) | IPLemma(name,_,_,_,_)
  | IPTypeInvariant(name,_,_,_) -> Format.pp_print_string fmt name
  | IPGlobalInvariant(name,_,_) -> Format.pp_print_string fmt name
  | IPAxiomatic (name,_) -> Format.pp_print_string fmt name
  | IPBehavior(kf,_,{b_name = name }) ->
    Format.fprintf fmt "behavior %s in function %a"
      name Kernel_function.pretty kf
  | IPComplete (kf,_,_) ->
    Format.fprintf fmt "complete clause in function %a"
      Kernel_function.pretty kf
  | IPDisjoint (kf,_,_) ->
    Format.fprintf fmt "disjoint clause in function %a"
      Kernel_function.pretty kf
  | IPCodeAnnot (_,_,{ annot_content = AAssert (_, { name = name :: _ })}) ->
    Format.pp_print_string fmt name
  | IPCodeAnnot(_,_,{annot_content = AInvariant (_,_, { name = name :: _ })})->
    Format.pp_print_string fmt name
  | IPCodeAnnot _ -> pretty fmt p
  | IPAllocation (kf,_,_,_) ->
    Format.fprintf fmt "allocates/frees clause in function %a"
      Kernel_function.pretty kf
  | IPAssigns (kf,_,_,_) ->
    Format.fprintf fmt "assigns clause in function %a"
      Kernel_function.pretty kf
  | IPFrom (kf,_,_,(t,_)) ->
    Format.fprintf fmt "from clause of term %a in function %a"
      Cil_printer.pp_identified_term t Kernel_function.pretty kf
  | IPDecrease(kf,_,_,_) ->
    Format.fprintf fmt "decrease clause in function %a"
      Kernel_function.pretty kf
  | IPPropertyInstance (kfopt, ki, ip) ->
    Format.fprintf fmt "specialization of %a %a" short_pretty ip
      pretty_instance_location (kfopt, ki)
  | IPReachable _ | IPOther _ -> pretty fmt p

module Names = struct
  module NamesTbl = 
    State_builder.Hashtbl(Datatype.String.Hashtbl)(Datatype.Int)
      (struct
	 let name = "PropertyNames"
	 let dependencies = [ ] 
	 let size = 97
       end)
  module IndexTbl =
    State_builder.Hashtbl(Hashtbl)(Datatype.String)
      (struct
	 let name = "PropertyIndex"
	 let dependencies = [ Ast.self; NamesTbl.self; Globals.Functions.self ] 
	 let size = 97
       end)

  let self = IndexTbl.self

  let kf_prefix kf = (Ast_info.Function.get_vi kf.fundec).vname ^ "_"

  let ident_names names =
    List.filter (function "" -> true
		   | _ as n -> '\"' <> (String.get n 0) ) names

  let pp_names fmt l =
    let l = ident_names l in
    match l with [] -> ()
      | _ -> Format.fprintf fmt "_%a"
          (Pretty_utils.pp_list ~sep:"_" Format.pp_print_string) l
	  
  let pp_code_annot_names fmt ca = 
    match ca.annot_content with
      | AAssert(for_bhv,named_pred) | AInvariant(for_bhv,_,named_pred) -> 
	  let pp_for_bhv fmt l =  
	    match l with [] -> ()
	      | _ -> Format.fprintf fmt "_for_%a"
		  (Pretty_utils.pp_list ~sep:"_" Format.pp_print_string) l
	  in Format.fprintf fmt "%a%a" pp_names named_pred.name pp_for_bhv for_bhv
      | AVariant(term, _) -> pp_names fmt term.term_name
      | _ -> () (* TODO : add some more names ? *)
	  
  let behavior_prefix b = 
    if Cil.is_default_behavior b then ""
    else b.b_name ^ "_"

  let variant_suffix = function      
    | (_,Some s) -> s
    | _ -> ""
	
  let string_of_termination_kind = function
      Normal -> "post"
    | Exits -> "exit"
    | Breaks -> "break"
    | Continues -> "continue"
    | Returns -> "return"

  let ki_prefix = function
      | Kglobal -> ""
      | Kstmt _ -> "stmt_"
	
  let predicate_kind_txt pk ki = 
    let name = match pk with
      | PKRequires b -> (behavior_prefix b) ^ "pre"
      | PKAssumes b -> (behavior_prefix b) ^ "assume"
      | PKEnsures (b, tk) -> (behavior_prefix b) ^ string_of_termination_kind tk
      | PKTerminates -> "term" 
    in
      (ki_prefix ki) ^ name
	
  let rec id_prop_txt p = match p with
    | IPPredicate (pk,kf,ki,idp) ->
        Pretty_utils.sfprintf "%s%s%a"
          (kf_prefix kf) (predicate_kind_txt pk ki) pp_names idp.ip_name
    | IPCodeAnnot (kf,_, ca) ->
        let name = match ca.annot_content with
          | AAssert _ -> "assert" 
          | AInvariant (_,true,_) -> "loop_inv"
          | AInvariant _ -> "inv"
          | APragma _ -> "pragma"
          | _ -> assert false
        in Pretty_utils.sfprintf "%s%s%a" (kf_prefix kf) name pp_code_annot_names ca
    | IPComplete (kf, ki, lb) ->
        Pretty_utils.sfprintf  "%s%scomplete%a" (kf_prefix kf) (ki_prefix ki) pp_names lb
    | IPDisjoint (kf, ki, lb) ->
        Pretty_utils.sfprintf  "%s%sdisjoint%a" (kf_prefix kf) (ki_prefix ki) pp_names lb
    | IPDecrease (kf,_,None, variant) -> (kf_prefix kf) ^ "decr" ^ (variant_suffix variant)
    | IPDecrease (kf,_,_,variant) -> (kf_prefix kf) ^ "loop_term" ^ (variant_suffix variant)
    | IPAxiom (name,_,_,named_pred,_) ->
	Pretty_utils.sfprintf "axiom_%s%a" name pp_names named_pred.name
    | IPAxiomatic(name, _) -> "axiomatic_" ^ name
    | IPLemma (name,_,_,named_pred,_) ->
	Pretty_utils.sfprintf "lemma_%s%a" name pp_names named_pred.name
    | IPTypeInvariant (name,_,named_pred,_) ->
      Pretty_utils.sfprintf "type_invariant_%s%a" name pp_names named_pred.name
    | IPGlobalInvariant (name,named_pred,_) ->
      Pretty_utils.sfprintf "global_invariant_%s%a"name pp_names named_pred.name
    | IPAllocation (kf, ki, (Id_behavior b), _) ->  (kf_prefix kf) ^ (ki_prefix ki) ^ (behavior_prefix b) ^ "alloc" 
    | IPAllocation (kf, Kstmt _s, (Id_code_annot ca), _) -> Pretty_utils.sfprintf "%sloop_alloc%a" (kf_prefix kf) pp_code_annot_names ca
    | IPAllocation _ -> assert false
    | IPAssigns (kf, ki, (Id_behavior b), _) ->  (kf_prefix kf) ^ (ki_prefix ki) ^ (behavior_prefix b) ^ "assign" 
    | IPAssigns (kf, Kstmt _s, (Id_code_annot ca), _) -> Pretty_utils.sfprintf "%sloop_assign%a" (kf_prefix kf) pp_code_annot_names ca
    | IPAssigns _ -> assert false
    | IPFrom (_, _, _, (out,_)) -> 
        "from_id_"^(string_of_int (out.it_id))
    | IPReachable _ -> "reachable_stmt"
    | IPBehavior(_, _, b) -> b.b_name
    | IPPropertyInstance (kfopt, ki, ip) ->
      Pretty_utils.sfprintf "specialization_%s_at_%t" (id_prop_txt ip)
        (fun fmt -> match kfopt, ki with
        | None, Kglobal -> Format.pp_print_string fmt "global"
        | Some kf, Kglobal -> Kernel_function.pretty fmt kf
        | None, Kstmt s -> Format.fprintf fmt "stmt_%d" s.sid
        | Some kf, Kstmt s ->
          Format.fprintf fmt "%a_stmt_%d" Kernel_function.pretty kf s.sid)
    | IPOther(s,Some kf,ki) -> (kf_prefix kf) ^ (ki_prefix ki) ^ s
    | IPOther(s,None,ki) -> (ki_prefix ki) ^ s

  (** function used to normanize basename *)
  let normalize_basename s = 
    let is_valid_id = ref true 
    and is_valid_char_id = function
      | 'a'..'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    and is_numeric = function
      | '0'..'9' -> true
      | _ -> false
    in
    String.iter (fun c -> if not (is_valid_char_id c) then is_valid_id := false) s  ;
    let s = if !is_valid_id then s else
      begin
	let sn = String.copy s
	and i = ref 0 
	in String.iter (fun c -> if not (is_valid_char_id c) then String.set sn !i '_' ; i := succ !i) s ;
	   sn
      end
    in if s = "" then "property" else 
	if is_numeric (String.get s 0) then "property_" ^ s else s

  (** returns the name that should be returned by the function [get_prop_name_id] if the given property has [name] as basename. That name is reserved so that [get_prop_name_id prop] can never return an identical name. *)
  let reserve_name_id basename =
    let basename = normalize_basename basename in
      try 
	let speed_up_start = NamesTbl.find basename in
	  (* this basename is already reserved *)
	let n,unique_name = Extlib.make_unique_name NamesTbl.mem ~sep:"_" ~start:speed_up_start basename
	in NamesTbl.replace basename (succ n) ; (* to speed up Extlib.make_unique_name for next time *)
	  unique_name
      with Not_found -> (* first time that basename is reserved *)
	NamesTbl.add basename 2 ;
	basename
	  
  (** returns the basename of the property. *)
  let get_prop_basename ip = normalize_basename (id_prop_txt ip)

  (** returns a unique name identifying the property.
      This name is built from the basename of the property. *)
  let get_prop_name_id ip =
    try IndexTbl.find ip
    with Not_found -> (* first time we are asking for a name for that [ip] *)
      let basename = get_prop_basename ip in
      let unique_name = reserve_name_id basename in
	IndexTbl.add ip unique_name ;
	unique_name

(* 
  (** force computation of the unique name identifying the property *)
  let make_prop_name_id ip =
    ignore (get_prop_name_id ip)
      
  let remove_prop_name_id ip =
    try 
      ignore (IndexTbl.find ip);
      IndexTbl.remove ip
  with Not_found -> ()
*)
end

let ip_other s kf ki = IPOther(s,kf,ki)

let ip_reachable_stmt kf ki = IPReachable(Some kf, Kstmt ki, Before)

let ip_reachable_ppt p =
  let kf = get_kf p in
  let ki = get_kinstr p in
  let ba = match p with
    | IPPredicate((PKRequires _ | PKAssumes _ | PKTerminates), _, _, _)
    | IPAxiom _ | IPAxiomatic _ | IPLemma _ | IPComplete _
    | IPDisjoint _ | IPCodeAnnot _ | IPAllocation _
    | IPDecrease _ | IPPropertyInstance _ | IPOther _
    | IPTypeInvariant _ | IPGlobalInvariant _
      -> Before
    | IPPredicate(PKEnsures _, _, _, _) | IPAssigns _ | IPFrom _ 
    | IPBehavior _ 
      -> After
    | IPReachable _ -> Kernel.fatal "IPReachable(IPReachable _) is not possible"
  in
  IPReachable(kf, ki, ba)

let ip_of_ensures kf st b (k,p) = IPPredicate (PKEnsures(b,k),kf,st,p)

let ip_ensures_of_behavior kf st b =
  List.map (ip_of_ensures kf st b) b.b_post_cond

let ip_of_allocation kf st loc = function
  | FreeAllocAny -> None
  | FreeAlloc(f,a) -> Some (IPAllocation (kf,st,loc,(f,a)))

let ip_allocation_of_behavior kf st b =
  ip_of_allocation kf st (Id_behavior b) b.b_allocation

let ip_of_assigns kf st loc = function
  | WritesAny -> None
  | Writes [(a,_)] when Logic_utils.is_result a.it_content ->
    (* We're only assigning the result (with dependencies), but no
       global variables, this amounts to \nothing.
     *)
    Some (IPAssigns (kf, st, loc, []))
  | Writes a -> Some (IPAssigns (kf,st,loc,a))

let ip_assigns_of_behavior kf st b =
  ip_of_assigns kf st (Id_behavior b) b.b_assigns

let ip_of_from kf st loc from = IPFrom (kf,st, loc, from)

let ip_from_of_behavior kf st b = match b.b_assigns with
  | WritesAny -> []
  | Writes l ->
    let treat_from acc (out, froms) = match froms with 
      | FromAny -> acc
      | From _ ->
	let ip = ip_of_from kf st (Id_behavior b) (out, froms) in 
	ip :: acc
    in
    List.fold_left treat_from [] l

let ip_allocation_of_code_annot kf st ca = match ca.annot_content with
  | AAllocation (_,a) -> ip_of_allocation kf st (Id_code_annot ca) a
  | _ -> None

let ip_assigns_of_code_annot kf st ca = match ca.annot_content with
  | AAssigns (_,a) -> ip_of_assigns kf st (Id_code_annot ca) a
  | _ -> None

let ip_from_of_code_annot kf st ca = match ca.annot_content with
  | AAssigns(_,WritesAny) -> []
  | AAssigns (_,Writes l) ->
    let treat_from acc (out, froms) = match froms with FromAny -> acc
      | From _ ->
        let ip = ip_of_from kf st (Id_code_annot ca) (out, froms) in ip::acc
    in
    List.fold_left treat_from [] l
  | _ -> []

let ip_post_cond_of_behavior kf st b =
  ip_ensures_of_behavior kf st b
  @ (Extlib.list_of_opt (ip_assigns_of_behavior kf st b))
  @ ip_from_of_behavior kf st b
  @ (Extlib.list_of_opt (ip_allocation_of_behavior kf st b))

let ip_of_behavior kf s b = IPBehavior(kf, s, b)

let ip_of_requires kf st b p = IPPredicate (PKRequires b,kf,st,p)

let ip_requires_of_behavior kf st b =
  List.map (ip_of_requires kf st b) b.b_requires

let ip_of_assumes kf st b p = IPPredicate (PKAssumes b,kf,st,p)

let ip_assumes_of_behavior kf st b =
  List.map (ip_of_assumes kf st b) b.b_assumes

let ip_all_of_behavior kf st b =
  ip_of_behavior kf st b
  :: ip_requires_of_behavior kf st b
  @ ip_assumes_of_behavior kf st b
  @ ip_post_cond_of_behavior kf st b

let ip_of_complete kf st bhvs = IPComplete(kf,st,bhvs)

let ip_complete_of_spec kf st s =
  List.map (ip_of_complete kf st) s.spec_complete_behaviors

let ip_of_disjoint kf st bhvs = IPDisjoint(kf,st,bhvs)

let ip_disjoint_of_spec kf st s =
  List.map (ip_of_disjoint kf st) s.spec_disjoint_behaviors

let ip_of_terminates kf st p = IPPredicate(PKTerminates,kf,st,p)

let ip_terminates_of_spec kf st s = match s.spec_terminates with
  | None -> None
  | Some p -> Some (ip_of_terminates kf st p)

let ip_of_decreases kf st d = IPDecrease(kf,st,None,d)

let ip_decreases_of_spec kf st s =
  Extlib.opt_map (ip_of_decreases kf st) s.spec_variant

let ip_post_cond_of_spec kf st s =
  List.concat (List.map (ip_post_cond_of_behavior kf st) s.spec_behavior)

let ip_of_spec kf st s =
  List.concat (List.map (ip_all_of_behavior kf st) s.spec_behavior)
  @ ip_complete_of_spec kf st s
  @ ip_disjoint_of_spec kf st s
  @ (Extlib.list_of_opt (ip_terminates_of_spec kf st s))
  @ (Extlib.list_of_opt (ip_decreases_of_spec kf st s))

let ip_axiom s = IPAxiom s
let ip_lemma s = IPLemma s
let ip_type_invariant s = IPTypeInvariant s
let ip_global_invariant s = IPGlobalInvariant s

let ip_property_instance kfopt ki ip = IPPropertyInstance (kfopt, ki, ip)

let ip_of_code_annot kf ki ca =
  let st = Kstmt ki in
  match ca.annot_content with
  | AAssert _ | AInvariant _ -> [ IPCodeAnnot(kf, ki, ca) ]
  | AStmtSpec (_bhv,s) -> 
    (* [JS 2011/08/29] seem to be incorrect since it does not use [bhv] 
       while [ip_of_spec] keeps all behaviors *)
    ip_of_spec kf st s
  | AVariant t -> [ IPDecrease (kf,st,(Some ca),t) ]
  | AAllocation _ -> 
      Extlib.list_of_opt (ip_allocation_of_code_annot kf st ca)
    @ ip_from_of_code_annot kf st ca
  | AAssigns _ -> 
    Extlib.list_of_opt (ip_assigns_of_code_annot kf st ca)
    @ ip_from_of_code_annot kf st ca
  | APragma p when Logic_utils.is_property_pragma p ->
    [ IPCodeAnnot (kf,ki,ca) ]
  | APragma _ -> []

let ip_of_code_annot_single kf ki ca = match ip_of_code_annot kf ki ca with
  | [] -> 
    (* [JS 2011/06/07] using Kernel.error here seems very strange.
       Actually it is incorrect in case of pragma which is not a property (see
       function ip_of_code_annot above. *)
    Kernel.error
      "@[cannot find a property to extract from code annotation@\n%a@]"
      Cil_printer.pp_code_annotation ca;
    raise (Invalid_argument "ip_of_code_annot_single")
  | [ ip ] -> ip
  | ip :: _ ->
    Kernel.warning 
      "@[choosing one of multiple properties associated \
           to code annotation@\n%a@]"
      Cil_printer.pp_code_annotation ca;
    ip

(* Must ensure that the first property is the best one in order to represent
   the annotation (see ip_of_global_annotation_single) *)
let ip_of_global_annotation a =
  let once = true in
  let rec aux acc = function
    | Daxiomatic(name, l, _) -> 
      let ppts = List.fold_left aux [] l in
      IPAxiomatic(name, ppts) :: (ppts @ acc)
    | Dlemma(name, true, a, b, c, d) -> ip_axiom (name,a,b,c,d) :: acc
    | Dlemma(name, false, a, b, c, d) -> ip_lemma (name,a,b,c,d) :: acc
    | Dinvariant(l, loc) -> 
      let pred = match l.l_body with
	| LBpred p -> p
	| _ -> assert false
      in
      IPGlobalInvariant(l.l_var_info.lv_name,pred,loc) :: acc
    | Dtype_annot(l, loc) ->
      let parameter = match l.l_profile with
	| h :: [] -> h
	| _ -> assert false
      in
      let ty = match parameter.lv_type with
	| Ctype x -> x
	| _ -> assert false
      in
      let pred = match l.l_body with
	| LBpred p -> p
	| _ -> assert false
      in
      IPTypeInvariant(l.l_var_info.lv_name,ty,pred,loc) :: acc
    | Dcustom_annot(_c, _n, _) ->
      (* TODO *)
      Kernel.warning ~once "ignoring status of custom annotation";
      acc
    | Dmodel_annot _ | Dfun_or_pred _ | Dvolatile _ | Dtype _ -> 
      (* no associated status for these annotations *) 
      acc
  in
  aux [] a

let ip_of_global_annotation_single a = match ip_of_global_annotation a with
  | [] -> None
  | ip :: _ -> 
    (* the first one is the good one, see ip_of_global_annotation *)
    Some ip

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
