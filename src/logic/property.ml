(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

type identified_unreachable =
  | UStmt of kernel_function * stmt
  | UProperty of identified_property

and identified_axiomatic = string * identified_property list

and identified_property =
  | IPPredicate of identified_predicate
  | IPAxiom of string
  | IPAxiomatic of identified_axiomatic
  | IPLemma of string
  | IPBehavior of identified_behavior
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease
  | IPUnreachable of identified_unreachable
  | IPOther of string * kernel_function option * kinstr 

let rec get_kinstr = function
  | IPPredicate (_,_,ki,_) 
  | IPBehavior(_, ki, _) 
  | IPComplete (_,ki,_)
  | IPDisjoint(_,ki,_) 
  | IPAssigns (_,ki,_,_) 
  | IPFrom(_,ki,_,_)
  | IPDecrease (_,ki,_,_) -> ki
  | IPUnreachable (UStmt(_, s)) -> Kstmt s
  | IPUnreachable (UProperty ppt) -> get_kinstr ppt
  | IPAxiom _ 
  | IPAxiomatic _
  | IPLemma _  -> Kglobal
  | IPOther(_,_,ki) -> ki
  | IPCodeAnnot (_,s,_) -> Kstmt s

let rec get_kf = function
  | IPPredicate (_,kf,_,_)
  | IPBehavior(kf, _, _)
  | IPCodeAnnot (kf,_,_)
  | IPComplete (kf,_,_) 
  | IPDisjoint(kf,_,_) 
  | IPAssigns(kf,_,_,_)
  | IPFrom(kf,_,_,_) 
  | IPDecrease (kf,_,_,_)
  | IPUnreachable (UStmt (kf, _)) -> Some kf
  | IPUnreachable(UProperty ppt) -> get_kf ppt
  | IPAxiom _ 
  | IPAxiomatic _
  | IPLemma _ -> None
  | IPOther(_,kf,_) -> kf

let get_pk_behavior = function
  | PKRequires b | PKAssumes b | PKEnsures (b,_) -> Some b
  | PKTerminates -> None

let get_behavior = function
  | IPPredicate (pk,_,_,_) -> get_pk_behavior pk
  | IPBehavior(_, _, b) -> Some b
  | IPAssigns(_,_,Id_behavior b,_)
  | IPFrom(_,_,Id_behavior b,_) -> Some b
  | IPAssigns(_,_,Id_code_annot _,_)
  | IPFrom(_,_,Id_code_annot _,_)
  | IPAxiom _ 
  | IPAxiomatic _
  | IPLemma _
  | IPCodeAnnot (_,_,_)
  | IPComplete (_,_,_)
  | IPDisjoint(_,_,_)
  | IPDecrease _
  | IPUnreachable _
  | IPOther _ -> None

include Datatype.Make_with_collections
    (struct
      
      include Datatype.Serializable_undefined 

      type t = identified_property
      let name = "identified property"
      let reprs = [ IPAxiom "" ]
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
            pretty_predicate_kind kind Cil.d_identified_predicate p
	| IPAxiom s -> Format.fprintf fmt "axiom@ %s" s
	| IPAxiomatic(s, _) -> Format.fprintf fmt "axiomatic@ %s" s
	| IPLemma s -> Format.fprintf fmt "lemma@ %s" s
	| IPBehavior(_kf, ki, b) -> 
            if Cil.is_default_behavior b then
              Format.pp_print_string fmt "default behavior"
            else
	      Format.fprintf fmt "behavior %s" b.b_name;
	    (match ki with 
	    | Kstmt s -> Format.fprintf fmt " for statement %d" s.sid
	    | Kglobal -> ())
	| IPCodeAnnot(_, _, a) -> Cil.d_code_annotation fmt a
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
	| IPAssigns(_, _, _, l) -> Cil.d_assigns fmt (Writes l)
	| IPFrom (_,_,_, f) -> Cil.d_from fmt f
	| IPDecrease(_, _, None,v) -> Cil.d_decreases fmt v
	| IPDecrease(_, _, _,v) -> Cil.d_loop_variant fmt v
	| IPUnreachable(UStmt(_, stmt)) -> 
	  Format.fprintf fmt "unreachable stmt %a" Cil.d_stmt stmt
	| IPUnreachable(UProperty p) -> 
	  Format.fprintf fmt "unreachable property %a" pretty p
	| IPOther(s,_,_) -> Format.pp_print_string fmt s

      let rec hash =
	let hash_bhv_loop = function
          | Id_behavior b -> (0, Hashtbl.hash b.b_name)
          | Id_code_annot ca -> (1, ca.annot_id)
	in
	function
	| IPPredicate (_,_,_,x) -> Hashtbl.hash (1, x.ip_id)
	| IPAxiom x -> Hashtbl.hash (2, x)
	| IPAxiomatic x -> Hashtbl.hash (3, x)
	| IPLemma x -> Hashtbl.hash (4, x)
	| IPCodeAnnot(_,_, ca) -> Hashtbl.hash (5, ca.annot_id)
	| IPComplete(f, ki, x) ->
	  Hashtbl.hash (6, Kf.hash f, Kinstr.hash ki, x)
	| IPDisjoint(f, ki, x) ->
	  Hashtbl.hash(7, Kf.hash f, Kinstr.hash ki, x)
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
	  Hashtbl.hash (11, Kf.hash kf, Kinstr.hash s, b.b_name)
	| IPUnreachable(UStmt(_, s)) -> Hashtbl.hash(12, Stmt.hash s)
	| IPUnreachable(UProperty p) -> Hashtbl.hash(13, hash p)
	| IPOther(s,_,_) -> Hashtbl.hash (14, s)

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
	| IPAxiom s1, IPAxiom s2
	| IPAxiomatic(s1, _), IPAxiomatic(s2, _)
	| IPLemma s1, IPLemma s2 -> s1 = s2
	| IPCodeAnnot(_,_,ca1), IPCodeAnnot(_,_,ca2) ->
          ca1.annot_id = ca2.annot_id
	| IPComplete(f1, ki1, x1), IPComplete(f2, ki2, x2)
	| IPDisjoint(f1, ki1, x1), IPDisjoint(f2, ki2, x2) ->
	  Kf.equal f1 f2 && Kinstr.equal ki1 ki2 && x1 = x2
	| IPAssigns (f1, ki1, b1, _), IPAssigns (f2, ki2, b2, _) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2)
	| IPFrom (f1,ki1,b1,(t1,_)), IPFrom (f2, ki2,b2,(t2,_)) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2) && t1.it_id = t2.it_id
	| IPDecrease(f1, ki1, _, _), IPDecrease(f2, ki2, _, _) ->
	  Kf.equal f1 f2 && Kinstr.equal ki1 ki2
	| IPUnreachable(UStmt(_, s1)), IPUnreachable(UStmt(_, s2)) ->
	  Stmt.equal s1 s2
	| IPUnreachable(UProperty p1), IPUnreachable(UProperty p2) ->
	  equal p1 p2
	| IPBehavior(f1, k1, b1), IPBehavior(f2, k2, b2) ->
	  Kf.equal f1 f2
	  && Kinstr.equal k1 k2 
	  && Datatype.String.equal b1.b_name b2.b_name
	| IPOther(s1,kf1,ki1), IPOther(s2,kf2,ki2) -> 
	    Datatype.String.equal s1 s2 
	    && Kinstr.equal ki1 ki2
	    && equal_opt Kf.equal kf1 kf2
	| (IPPredicate _ | IPAxiom _ | IPAxiomatic _ | IPLemma _ 
	      | IPCodeAnnot _ | IPComplete _ | IPDisjoint _ | IPAssigns _ 
	      | IPFrom _ | IPDecrease _ | IPBehavior _ | IPUnreachable _ 
	      | IPOther _ ), _ -> false

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
	| IPUnreachable(UStmt(_, s1)), IPUnreachable(UStmt(_, s2)) ->
	  Stmt.compare s1 s2 
	| IPUnreachable(UProperty p1), IPUnreachable(UProperty p2) ->
	  compare p1 p2
	| IPAxiom s1, IPAxiom s2
	| IPAxiomatic(s1, _), IPAxiomatic(s2, _)
	| IPLemma s1, IPLemma s2 ->
	    Datatype.String.compare s1 s2
	| IPOther(s1,kf1,ki1), IPOther(s2,kf2,ki2) -> 
	    let s = Datatype.String.compare s1 s2 in
	    if s <> 0 then s else
	      let s = compare_opt Kf.compare kf1 kf2 in
	      if s <> 0 then s else
		Kinstr.compare ki1 ki2
	| x, y ->
	(* [JS 2011/07/20] very dangerous pattern matching *)
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
	    | IPUnreachable _ -> 12
	    | IPOther _ -> 13
	  in
	  Datatype.Int.compare (nb x) (nb y)

     end)
 
(* [JS 2011/08/04] seem to be dead code *)
(*let short_pretty fmt = function
  | IPPredicate (kind, kf,_,_) ->
    Format.fprintf fmt "%a of function %a" 
      pretty_predicate_kind kind Kf.pretty kf
  | IPAxiom s -> Format.fprintf fmt "axiom %s" s
  | IPLemma s -> Format.fprintf fmt "lemma %s" s
  | IPCodeAnnot(_, _, a) -> 
    Format.pp_print_string fmt
      (match a.annot_content with
      | AAssert _ -> "assertion"
      | AStmtSpec _ -> "stmt contract"
      | AInvariant _ -> "invariant"
      | AVariant _ -> "variant"
      | AAssigns _ -> "assigns"
      | APragma _ -> "pragma")
  | IPComplete(kf, Kglobal, _) -> 
    Format.fprintf fmt "complete behavior of function %a" Kf.pretty kf
  | IPComplete(_, Kstmt _, _) -> Format.fprintf fmt "complete behavior"
  | IPDisjoint(kf, Kglobal, _) -> 
    Format.fprintf fmt "disjoint behavior of function %a" Kf.pretty kf
  | IPDisjoint(_, Kstmt _, _) -> Format.fprintf fmt "disjoint behavior"
  | IPAssigns(kf, Kglobal, _, _) -> 
    Format.fprintf fmt "assigns of function %a" Kf.pretty kf
  | IPAssigns(_, Kstmt _, _, _) -> Format.pp_print_string fmt "assigns"
  | IPFrom _ -> Format.pp_print_string fmt "from"
  | IPDecrease(kf, Kglobal, None,_) -> 
    Format.fprintf fmt "decrease of function %a" Kf.pretty kf
  | IPDecrease(_, Kstmt _, None,_) -> Format.pp_print_string fmt "decrease"
  | IPDecrease(_, _, _,_) -> Format.pp_print_string fmt "loop variant"
  | IPNotacsl s -> Format.pp_print_string fmt s
 *)

let ip_other s kf ki = IPOther(s,kf,ki)
let ip_unreachable_stmt kf ki = IPUnreachable(UStmt(kf, ki))
let ip_unreachable_ppt p = IPUnreachable(UProperty p)

let ip_of_ensures kf st b (k,p) = IPPredicate (PKEnsures(b,k),kf,st,p)

let ip_ensures_of_behavior kf st b =
  List.map (ip_of_ensures kf st b) b.b_post_cond

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

let ip_of_code_annot kf ki ca =
  let st = Kstmt ki in
  match ca.annot_content with
  | AAssert _ | AInvariant _ -> [ IPCodeAnnot(kf, ki, ca) ]
  | AStmtSpec (_bhv,s) -> 
    (* [JS 2011/08/29] seem to be incorrect since it does not use the behavior 
       while [ip_of_spec] keeps all behaviors *)
    ip_of_spec kf st s
  | AVariant t -> [ IPDecrease (kf,st,(Some ca),t) ]
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
      Cil.d_code_annotation ca;
    raise (Invalid_argument "ip_of_code_annot_single")
  | [ ip ] -> ip
  | ip :: _ ->
    Kernel.warning 
      "@[choosing one of multiple properties associated \
           to code annotation@\n%a@]"
      Cil.d_code_annotation ca;
    ip

(* Must ensure that the first property is the best one in order to represent
   the annotation (see ip_of_global_annotation_single) *)
let ip_of_global_annotation a = 
  let rec aux acc = function
    | Daxiomatic(name, l, _) -> 
      let ppts = List.fold_left aux [] l in
      IPAxiomatic(name, ppts) :: (ppts @ acc)
    | Dlemma(name, true, _, _,_, _) -> ip_axiom name :: acc
    | Dlemma(name, false, _, _, _, _) -> ip_lemma name :: acc
    | Dinvariant(l, _) -> 
      (* TODO *)
      Kernel.warning "ignoring status of global invariant `%s'" 
	l.l_var_info.lv_name;
      acc
    | Dtype_annot(l, _) -> 
      (* TODO *)
      Kernel.warning "ignoring status of type invariant `%s'" 
	l.l_var_info.lv_name;
      acc
    | Dmodel_annot(l, _) ->
      (* TODO *)
      Kernel.warning "ignoring status of model `%s'" l.l_var_info.lv_name;
      acc
    | Dfun_or_pred(_) | Dvolatile _ | Dtype _ -> 
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
compile-command: "make -C ../.."
End:
*)
