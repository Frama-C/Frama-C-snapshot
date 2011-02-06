(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Status of properties.
    @since Boron-20100401 *)

open Cil_types
open Db_types
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
    * (identified_term * identified_term list)

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

type identified_spec = kernel_function * kinstr * funspec

type identified_property =
  | IPBlob of State.t (* an unidentified property *)
  | IPPredicate of identified_predicate
  | IPAxiom of string
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPBehavior of identified_behavior
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease

let get_kinstr = function
  | IPBlob _ -> Kglobal
  | IPPredicate (_,_,ki,_) -> ki
  | IPAxiom _ -> Kglobal
  | IPCodeAnnot (_,ki,_) -> Kstmt ki
  | IPComplete (_,ki,_) -> ki
  | IPDisjoint(_,ki,_) -> ki
  | IPAssigns (_,ki,_,_) -> ki
  | IPFrom(_,ki,_,_) -> ki
  | IPDecrease (_,ki,_,_) -> ki
  | IPBehavior(_,ki,_) -> ki

let get_kf = function
  | IPBlob _ -> None
  | IPPredicate (_,kf,_,_) -> Some kf
  | IPAxiom _ -> None
  | IPCodeAnnot (kf,_,_) -> Some kf
  | IPComplete (kf,_,_) -> Some kf
  | IPDisjoint(kf,_,_) -> Some kf
  | IPAssigns(kf,_,_,_) -> Some kf
  | IPFrom(kf,_,_,_) -> Some kf
  | IPDecrease (kf,_,_,_) -> Some kf
  | IPBehavior(kf,_,_) -> Some kf

let get_pk_behavior = function
  | PKRequires b -> Some b
  | PKAssumes b -> Some b
  | PKEnsures (b,_) -> Some b
  | PKTerminates -> None

let get_behavior = function
  | IPBlob _ -> None
  | IPPredicate (pk,_,_,_) -> get_pk_behavior pk
  | IPAxiom _ -> None
  | IPCodeAnnot (_,_,_) -> None
  | IPComplete (_,_,_) -> None
  | IPDisjoint(_,_,_) -> None
  | IPAssigns(_,_,Id_behavior b,_) -> Some b
  | IPAssigns(_,_,Id_code_annot _,_) -> None
  | IPFrom(_,_,Id_behavior b,_) -> Some b
  | IPFrom(_,_,Id_code_annot _,_) -> None
  | IPDecrease (_,_,_,_) -> None
  | IPBehavior(_,_,b) -> Some b

include Datatype.Make_with_collections
    (struct

      include Datatype.Undefined
      type t = identified_property
      let name = "identified property"
      let reprs = [ IPBlob State.dummy ]
      let mem_project = Datatype.never_any_project

      let hash = 
        let hash_bhv_loop = function
          | Id_behavior b -> (0,Hashtbl.hash b.b_name)
          | Id_code_annot ca -> (1,ca.annot_id)
        in
        function
	| IPBlob x -> Hashtbl.hash (0, State.hash x)
	| IPPredicate (_,_,_,x) -> Hashtbl.hash (1, x.ip_id)
	| IPAxiom x -> Hashtbl.hash (2, x)
	| IPCodeAnnot(_,_, ca) -> Hashtbl.hash (3, ca.annot_id)
	| IPComplete(f, ki, x) ->
	  Hashtbl.hash (4, Kernel_function.hash f, Kinstr.hash ki, x)
	| IPDisjoint(f, ki, x) ->
	  Hashtbl.hash (5, Kernel_function.hash f, Kinstr.hash ki, x)
	| IPAssigns(f, ki, b, _l) ->
	  Hashtbl.hash
	    (6, Kernel_function.hash f, Kinstr.hash ki, hash_bhv_loop b)
        | IPFrom(kf,ki,b,(t,_)) ->
          Hashtbl.hash
            (7, Kernel_function.hash kf, Kinstr.hash ki,
             hash_bhv_loop b, Cil_datatype.Identified_term.hash t)
	| IPDecrease(kf, ki, _ca, _v) ->
          (* At most one loop variant per statement anyway, no
             need to discriminate against the code annotation itself
           *)
	  Hashtbl.hash (8, Kernel_function.hash kf, Kinstr.hash ki)
	| IPBehavior(kf, ki, b) ->
	  Hashtbl.hash (9, Kernel_function.hash kf, Kinstr.hash ki, b.b_name)

      let equal p1 p2 = 
        let eq_bhv (f1,ki1,b1) (f2,ki2,b2) =
	  Kernel_function.equal f1 f2 && Kinstr.equal ki1 ki2
	  && 
            (match b1, b2 with
	      | Id_code_annot ca1, Id_code_annot ca2 -> 
                ca1.annot_id = ca2.annot_id
	      | Id_behavior b1, Id_behavior b2 -> b1.b_name = b2.b_name
	      | Id_code_annot _, Id_behavior _ 
              | Id_behavior _, Id_code_annot _ -> false)
        in
        match p1, p2 with
	| IPBlob s1, IPBlob s2 -> State.equal s1 s2
	| IPPredicate (_,_,_,s1), IPPredicate (_,_,_,s2) -> s1.ip_id = s2.ip_id
	| IPAxiom s1, IPAxiom s2 -> s1 = s2
	| IPCodeAnnot(_,_,ca1), IPCodeAnnot(_,_,ca2) ->
          ca1.annot_id = ca2.annot_id
	| IPComplete(f1, ki1, x1), IPComplete(f2, ki2, x2)
	| IPDisjoint(f1, ki1, x1), IPDisjoint(f2, ki2, x2) ->
	  Kernel_function.equal f1 f2 && Kinstr.equal ki1 ki2 && x1 = x2
	| IPAssigns (f1, ki1, b1, _), IPAssigns (f2, ki2, b2, _) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2)
        | IPFrom (f1,ki1,b1,(t1,_)), IPFrom (f2, ki2,b2,(t2,_)) ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2) && t1.it_id = t2.it_id
	| IPDecrease(f1, ki1, _, _), IPDecrease(f2, ki2, _, _) ->
	  Kernel_function.equal f1 f2 && Kinstr.equal ki1 ki2
	| IPBehavior(f1, ki1, b1), IPBehavior(f2, ki2, b2) ->
	  Kernel_function.equal f1 f2
	  && Kinstr.equal ki1 ki2
	  && b1.b_name = b2.b_name
	| (IPBlob _ | IPPredicate _ | IPAxiom _ | IPCodeAnnot _ 
          | IPComplete _ | IPDisjoint _ | IPAssigns _ | IPFrom _ 
          | IPDecrease _ | IPBehavior _ ), _ -> false

      let compare x y = 
        let cmp_bhv (f1,ki1,b1) (f2,ki2,b2) =
	  let n = Kernel_function.compare f1 f2 in
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
	| IPBlob s1, IPBlob s2 -> State.compare s1 s2
	| IPPredicate (_,_,_,s1), IPPredicate (_,_,_,s2) ->
          Datatype.Int.compare s1.ip_id s2.ip_id
	| IPAxiom s1, IPAxiom s2 -> String.compare s1 s2
	| IPCodeAnnot(_,_,ca1), IPCodeAnnot(_,_,ca2) ->
          Datatype.Int.compare ca1.annot_id ca2.annot_id
	| IPComplete(f1, ki1, x1), IPComplete(f2, ki2, x2)
	| IPDisjoint(f1, ki1, x1), IPDisjoint(f2, ki2, x2) ->
          let n = Kernel_function.compare f1 f2 in
          if n = 0 then
            let n = Kinstr.compare ki1 ki2 in
	    if n = 0 then Extlib.compare_basic x1 x2 else n
          else n
	| IPAssigns (f1, ki1, b1, _), IPAssigns (f2, ki2, b2, _) ->
          cmp_bhv (f1,ki1,b1) (f2,ki2,b2)
        | IPFrom (f1,ki1,b1,(t1,_)), IPFrom(f2,ki2,b2,(t2,_)) ->
          let n = cmp_bhv (f1,ki1,b1) (f2,ki2,b2) in
          if n = 0 then Cil_datatype.Identified_term.compare t1 t2 else n
	| IPDecrease(f1, ki1,_,_), IPDecrease(f2, ki2,_,_) ->
	  let n = Kernel_function.compare f1 f2 in
	  if n = 0 then Kinstr.compare ki1 ki2 else n
	| IPBehavior(f1, ki1, b1), IPBehavior(f2, ki2, b2) ->
          cmp_bhv (f1,ki1, Id_behavior b1) (f2,ki2,Id_behavior b2)
	| x, y ->
	  let nb = function
	    | IPPredicate _ -> 1
	    | IPAssigns (_, _, _, _) -> 2
	    | IPDecrease _ -> 3
	    | IPAxiom _ -> 4
	    | IPCodeAnnot _ -> 5
	    | IPBehavior _ -> 6
	    | IPComplete (_, _, _) -> 7
	    | IPDisjoint (_, _, _) -> 8
	    | IPBlob _ -> 9
            | IPFrom _ -> 10
	  in
	  Datatype.Int.compare (nb x) (nb y)

      let pretty fmt = function
	| IPBlob s -> Format.fprintf fmt "%s" (State.get_name s)
	| IPPredicate (kind,_,_,p) ->
          Format.fprintf fmt "%a %a"
            pretty_predicate_kind kind Cil.d_identified_predicate p
	| IPAxiom s -> Format.fprintf fmt "axiom %s" s
	| IPCodeAnnot(_, _, a) -> Cil.d_code_annotation fmt a
	| IPComplete(_, _, l) ->
	  Format.fprintf fmt "complete %a"
	    (Pretty_utils.pp_list (fun fmt s ->  Format.fprintf fmt " %s" s))
	    l
	| IPDisjoint(_, _, l) ->
	  Format.fprintf fmt "disjoint %a"
	    (Pretty_utils.pp_list (fun fmt s ->  Format.fprintf fmt " %s" s))
	    l
	| IPAssigns(_, _, _, l) -> Cil.d_assigns fmt (Writes l)
        | IPFrom (_,_,_,(b,f)) -> Cil.d_from fmt (b,From f)
	| IPDecrease(_, _, _,_) -> Format.fprintf fmt "decrease: <TODO>"
	| IPBehavior(_, _, b) -> Format.fprintf fmt "behavior %s" b.b_name

     end)

let ip_of_behavior kf st b = IPBehavior(kf,st,b)

let ip_of_ensures kf st b (k,p) = IPPredicate (PKEnsures(b,k),kf,st,p)

let ip_ensures_of_behavior kf st b =
  List.map (ip_of_ensures kf st b) b.b_post_cond

let ip_of_assigns kf st loc a =
  match a with
      WritesAny -> None
    | Writes a -> Some (IPAssigns (kf,st,loc,a))

let ip_assigns_of_behavior kf st b = 
  ip_of_assigns kf st (Id_behavior b) b.b_assigns

let ip_of_from kf st loc (b,f) =
  match f with
      FromAny -> None
    | From f -> Some (IPFrom (kf,st, loc, (b,f)))
  

let ip_from_of_behavior kf st b =
  match b.b_assigns with
    | WritesAny -> []
    | Writes l ->
      let treat_from acc from = 
        match ip_of_from kf st (Id_behavior b) from with
            None -> acc
          | Some ip -> ip::acc
      in
      List.fold_left treat_from [] l

let ip_assigns_of_code_annot kf st ca =
  match ca.annot_content with
    | AAssigns (_,a) -> 
      ip_of_assigns kf st (Id_code_annot ca) a
    | _ -> None

let ip_from_of_code_annot kf st ca =
  match ca.annot_content with
    | AAssigns(_,WritesAny) -> []
    | AAssigns (_,Writes l) ->
      let treat_from acc from = 
        match ip_of_from kf st (Id_code_annot ca) from with
            None -> acc
          | Some ip -> ip::acc
      in
      List.fold_left treat_from [] l
    | _ -> []

let ip_post_cond_of_behavior kf st b =
  ip_ensures_of_behavior kf st b 
  @ (Extlib.list_of_opt (ip_assigns_of_behavior kf st b))
  @ ip_from_of_behavior kf st b

let ip_of_requires kf st b p = IPPredicate (PKRequires b,kf,st,p)

let ip_requires_of_behavior kf st b = 
  List.map (ip_of_requires kf st b) b.b_requires

let ip_of_assumes kf st b p = IPPredicate (PKAssumes b,kf,st,p)

let ip_assumes_of_behavior kf st b = 
  List.map (ip_of_assumes kf st b) b.b_assumes

let ip_all_of_behavior kf st b =
  ip_requires_of_behavior kf st b
  @ ip_assumes_of_behavior kf st b
  @ ip_post_cond_of_behavior kf st b

let ip_of_complete kf st bhvs = IPComplete(kf,st,bhvs)

let ip_complete_of_spec kf st s =
  List.map (ip_of_complete kf st) s.spec_complete_behaviors

let ip_of_disjoint kf st bhvs = IPDisjoint(kf,st,bhvs)

let ip_disjoint_of_spec kf st s =
  List.map (ip_of_disjoint kf st) s.spec_disjoint_behaviors

let ip_of_terminates kf st p = IPPredicate(PKTerminates,kf,st,p)

let ip_terminates_of_spec kf st s =
  match s.spec_terminates with
      None -> None
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

let ip_blob s = IPBlob s

let ip_of_code_annot kf ki ca =
  let st = Kstmt ki in
  match ca.annot_content with
      | AAssert _ | AInvariant _ ->
        [IPCodeAnnot(kf, ki, ca)]
      | AStmtSpec s -> ip_of_spec kf st s
      | AVariant t -> [IPDecrease (kf,st,(Some ca),t)]
      | AAssigns _ -> 
        (Extlib.list_of_opt (ip_assigns_of_code_annot kf st ca))
        @ ip_from_of_code_annot kf st ca
      | APragma _ -> [IPCodeAnnot (kf,ki,ca)]

let ip_of_code_annot_single kf ki ca =
  match ip_of_code_annot kf ki ca with
      [] -> 
        Kernel.error 
          "@[Cannot find a property to extract from code annotation@\n%a@]"
          Cil.d_code_annotation ca;
        raise (Invalid_argument "ip_of_code_annot_single")
    | [ip] -> ip
    | ip::_ ->
      Kernel.warning 
        "@[Choosing one of multiple properties associated \
           to code annotation@\n%a@]"
        Cil.d_code_annotation ca;
      ip

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
