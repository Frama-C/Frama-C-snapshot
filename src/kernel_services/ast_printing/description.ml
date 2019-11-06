(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Property
open Cil_types

let pp_loc = Cil_datatype.Location.pretty_long

let pp_kloc kloc fmt loc =
  if kloc then Format.fprintf fmt " (%a)" pp_loc loc else ()

let pp_opt doit pp fmt x = if doit then pp fmt x

let goto_stmt stmt =
  let rec goto_label = function
    | [] -> Printf.sprintf "s%04d" stmt.sid
    | Label(a,_,true)::_ -> a
    | _::labels -> goto_label labels
  in goto_label stmt.labels

let rec stmt_labels = function
  | Label(a,_,true) :: ls -> a :: stmt_labels ls
  | Label _ :: ls -> stmt_labels ls
  | Case(e,_) :: ls ->
    let cvalue = (Cil.constFold true e) in
    Format.asprintf "case %a" Printer.pp_exp cvalue
    :: stmt_labels ls
  | Default _ :: ls ->
    "default" :: stmt_labels ls
  | [] -> []

let pp_labels fmt stmt =
  match stmt_labels stmt.labels with
  | [] -> ()
  | ls -> Format.fprintf fmt " '%s'" (String.concat "," ls)

let pp_idpred kloc fmt idpred =
  let np = idpred.ip_content in
  if np.pred_name <> []
  then Format.fprintf fmt " '%s'" (String.concat "," np.pred_name)
  else pp_kloc kloc fmt np.pred_loc

let pp_allocation kloc fmt (allocation:identified_term list) =
  if allocation = [] then Format.fprintf fmt "nothing"
  else
    let names =
      List.fold_left
        (fun names x -> names @ x.it_content.term_name)
        [] allocation in
    match names with
    | [] ->
      if kloc then
        let x = List.hd allocation in
        Format.fprintf fmt "(%a)" pp_loc x.it_content.term_loc
      else Format.fprintf fmt "..."
    | _ ->
      Format.fprintf fmt "'%s'" (String.concat "," names)

let pp_region kloc fmt (region:from list) =
  if region = [] then Format.fprintf fmt "nothing"
  else
    let names =
      List.fold_left
        (fun names (x,_) -> names @ x.it_content.term_name)
        [] region in
    match names with
    | [] ->
      if kloc then
        let x = fst (List.hd region) in
        Format.fprintf fmt "(%a)" pp_loc x.it_content.term_loc
      else Format.fprintf fmt "..."
    | _ ->
      Format.fprintf fmt "'%s'" (String.concat "," names)

let pp_bhv fmt bhv =
  if not (Cil.is_default_behavior bhv) then
    Format.fprintf fmt " for '%s'" bhv.b_name

let pp_bhvs fmt = function
  | [] -> ()
  | b::bs ->
    Format.fprintf fmt " @[<hov 0>'%s'" b ;
    List.iter (fun b -> Format.fprintf fmt ",@ '%s'" b) bs ;
    Format.fprintf fmt "@]"

let pp_for fmt = function
  | [] -> ()
  | bs -> Format.fprintf fmt " for '%s'" (String.concat "," bs)

let pp_named fmt nx =
  if nx.pred_name <> [] then
    Format.fprintf fmt " '%s'" (String.concat "," nx.pred_name)

let pp_code_annot fmt ca =
  match ca.annot_content with
  | AAssert(bs,Assert,np) ->
    Format.fprintf fmt "assertion%a%a" pp_for bs pp_named np
  | AAssert(bs,Check,np) ->
    Format.fprintf fmt "check%a%a" pp_for bs pp_named np
  | AInvariant(bs,_,np) ->
    Format.fprintf fmt "invariant%a%a" pp_for bs pp_named np
  | AAssigns(bs,_) -> Format.fprintf fmt "assigns%a" pp_for bs
  | AAllocation(bs,_) -> Format.fprintf fmt "allocates_frees%a" pp_for bs
  | APragma _ -> Format.pp_print_string fmt "pragma"
  | AVariant _ -> Format.pp_print_string fmt "variant"
  | AStmtSpec _ -> Format.pp_print_string fmt "block contract"
  | AExtended _ -> Format.pp_print_string fmt "extension"

let pp_stmt kloc fmt stmt =
  match stmt.skind with
  | Instr (Local_init (v,_,loc)) ->
    Format.fprintf fmt "initialization of '%s'%a" v.vname (pp_kloc kloc) loc
  | Instr (Call(_,{enode=Lval(Var v,_)},_,loc)) ->
    Format.fprintf fmt "call '%s'%a" v.vname (pp_kloc kloc) loc
  | Instr (Set(_,_,loc)|Call(_,_,_,loc)) ->
    Format.fprintf fmt "instruction%a" (pp_kloc kloc) loc
  | Instr (Asm(_,_,_,loc)) ->
    Format.fprintf fmt "assembly%a%a" pp_labels stmt (pp_kloc kloc) loc
  | Instr (Skip(_,loc)) ->
    Format.fprintf fmt "program point%a%a" pp_labels stmt (pp_kloc kloc) (loc,loc)
  | Instr (Code_annot(ca,loc)) ->
    Format.fprintf fmt "%a%a" pp_code_annot ca (pp_kloc kloc) loc
  | Return(_,loc) -> Format.fprintf fmt "return%a" (pp_kloc kloc) loc
  | Goto(s,loc) -> Format.fprintf fmt "goto %s%a" (goto_stmt !s) (pp_kloc kloc) loc
  | Break loc -> Format.fprintf fmt "break%a" (pp_kloc kloc) loc
  | Continue loc -> Format.fprintf fmt "continue%a" (pp_kloc kloc) loc
  | If(_,_,_,loc) -> Format.fprintf fmt "if-then-else%a" (pp_kloc kloc) loc
  | Switch(_,_,_,loc) -> Format.fprintf fmt "switch%a" (pp_kloc kloc) loc
  | Loop(_,_,loc,_,_) -> Format.fprintf fmt "loop%a" (pp_kloc kloc) loc
  | Block _ -> Format.fprintf fmt "block%a" pp_labels stmt
  | UnspecifiedSequence _ -> Format.fprintf fmt "instruction%a" pp_labels stmt
  | Throw(_,loc) -> Format.fprintf fmt "throw%a" (pp_kloc kloc) loc
  | TryFinally(_,_,loc) | TryExcept(_,_,_,loc) | TryCatch(_,_,loc)->
    Format.fprintf fmt "try-catch%a" (pp_kloc kloc) loc

let pp_stmt_loc kloc fmt s = Format.fprintf fmt " at %a" (pp_stmt kloc) s

let pp_kinstr kloc fmt = function
  | Kglobal -> () | Kstmt s -> pp_stmt_loc kloc fmt s

let pp_predicate fmt = function
  | PKRequires bhv ->
    Format.fprintf fmt "Pre-condition%a" pp_bhv bhv
  | PKAssumes bhv ->
    Format.fprintf fmt "Assumption%a" pp_bhv bhv
  | PKEnsures(bhv,Normal) ->
    Format.fprintf fmt "Post-condition%a" pp_bhv bhv
  | PKEnsures(bhv,Breaks) ->
    Format.fprintf fmt "Breaking-condition%a" pp_bhv bhv
  | PKEnsures(bhv,Continues) ->
    Format.fprintf fmt "Continue-condition%a" pp_bhv bhv
  | PKEnsures(bhv,Returns) ->
    Format.fprintf fmt "Return-condition%a" pp_bhv bhv
  | PKEnsures(bhv,Exits) ->
    Format.fprintf fmt "Exit-condition%a" pp_bhv bhv
  | PKTerminates ->
    Format.fprintf fmt "Termination-condition"

let pp_kf_context kfopt fmt kf =
  match kfopt with
  | `Always ->
    Format.fprintf fmt " in '%s'" (Kernel_function.get_name kf)
  | `Never -> ()
  | `Context kf0 ->
    if not (Kernel_function.equal kf0 kf) then
      Format.fprintf fmt " of '%s'" (Kernel_function.get_name kf)

let pp_context kfopt fmt = function
  | None -> ()
  | Some kf -> pp_kf_context kfopt fmt kf

let pp_extended_loc kfopt kiopt kloc fmt (loc,le) =
  let open Property in
  match le with
  | ELContract kf -> pp_kf_context kfopt fmt kf
  | ELStmt (kf, s) ->
    pp_kf_context kfopt fmt kf; pp_opt kiopt (pp_stmt_loc kloc) fmt s
  | ELGlob -> pp_kloc kloc fmt loc

let pp_other_loc kfopt kiopt kloc fmt le =
  let open Property in
  match le with
  | OLContract kf -> pp_kf_context kfopt fmt kf
  | OLStmt (kf, s) ->
    pp_kf_context kfopt fmt kf; pp_opt kiopt (pp_stmt_loc kloc) fmt s
  | OLGlob loc -> pp_kloc kloc fmt loc

let pp_active fmt active =
  Pretty_utils.pp_list
    ~pre:" under active behaviors" ~sep:"," Format.pp_print_string fmt
    (Datatype.String.Set.elements active)

let pp_capitalize fmt s =
  Format.pp_print_string fmt (String.capitalize_ascii s)

let pp_acsl_extension fmt {ext_name} = pp_capitalize fmt ext_name

let rec pp_prop kfopt kiopt kloc fmt = function
  | IPAxiom {il_name=s} -> Format.fprintf fmt "Axiom '%s'" s
  | IPLemma {il_name=s} -> Format.fprintf fmt "Lemma '%s'" s
  | IPTypeInvariant {iti_name=s} -> Format.fprintf fmt "Type invariant '%s'" s
  | IPGlobalInvariant {igi_name=s} -> Format.fprintf fmt "Global invariant '%s'" s
  | IPAxiomatic {iax_name=s} -> Format.fprintf fmt "Axiomatic '%s'" s
  | IPOther {io_name=s;io_loc=le} ->
    Format.fprintf fmt "%a%a" pp_capitalize s (pp_other_loc kfopt kiopt kloc) le
  | IPPredicate {ip_kind; ip_kf; ip_kinstr=Kglobal; ip_pred} ->
    Format.fprintf fmt "%a%a%a"
      pp_predicate ip_kind
      (pp_idpred kloc) ip_pred
      (pp_context kfopt) (Some ip_kf)
  | IPPredicate {ip_kind; ip_kinstr; ip_pred} ->
    Format.fprintf fmt "%a%a%a"
      pp_predicate ip_kind
      (pp_idpred kloc) ip_pred
      (pp_kinstr kloc) ip_kinstr
  | IPExtended {ie_loc=le; ie_ext=ext} ->
    Format.fprintf fmt "%a%a"
      pp_acsl_extension ext
      (pp_extended_loc kfopt kiopt kloc) (ext.ext_loc,le)
  | IPBehavior {ib_kinstr=ki; ib_active=active; ib_bhv=bhv} ->
    if Cil.is_default_behavior bhv then
      Format.fprintf fmt "Default behavior%a%a"
        (pp_opt kiopt (pp_kinstr kloc)) ki (pp_opt kiopt pp_active) active
    else
      Format.fprintf fmt "Behavior '%s'%a"
        bhv.b_name
        (pp_opt kiopt (pp_kinstr kloc)) ki
  | IPComplete {ic_kinstr; ic_active; ic_bhvs} ->
    Format.fprintf fmt "Complete behaviors%a%a%a"
      pp_bhvs ic_bhvs
      (pp_opt kiopt (pp_kinstr kloc)) ic_kinstr (pp_opt kiopt pp_active) ic_active
  | IPDisjoint {ic_kinstr; ic_active; ic_bhvs} ->
    Format.fprintf fmt "Disjoint behaviors%a%a%a"
      pp_bhvs ic_bhvs
      (pp_opt kiopt (pp_kinstr kloc)) ic_kinstr
      (pp_opt kiopt pp_active) ic_active
  | IPCodeAnnot {ica_ca={annot_content=AAssert(bs,Assert,np)}} ->
    Format.fprintf fmt "Assertion%a%a%a"
      pp_for bs
      pp_named np
      (pp_kloc kloc) np.pred_loc
  | IPCodeAnnot {ica_ca={annot_content=AAssert(bs,Check,np)}} ->
    Format.fprintf fmt "Check%a%a%a"
      pp_for bs
      pp_named np
      (pp_kloc kloc) np.pred_loc
  | IPCodeAnnot {ica_ca={annot_content=AInvariant(bs,_,np)}} ->
    Format.fprintf fmt "Invariant%a%a%a"
      pp_for bs
      pp_named np
      (pp_kloc kloc) np.pred_loc
  | IPCodeAnnot {ica_ca={annot_content=AExtended(bs,_,{ext_name})};ica_stmt} ->
    Format.fprintf fmt "%a%a %a"
      pp_capitalize ext_name pp_for bs (pp_stmt kloc) ica_stmt
  | IPCodeAnnot {ica_stmt} ->
    Format.fprintf fmt "Annotation %a" (pp_stmt kloc) ica_stmt
  | IPAllocation {ial_kf; ial_kinstr=Kglobal; ial_bhv=Id_contract (_,bhv);
                  ial_allocs = (frees, allocates)} ->
    Format.fprintf fmt "Frees/Allocates%a %a/%a %a"
      pp_bhv bhv
      (pp_allocation kloc) frees
      (pp_allocation kloc) allocates
      (pp_context kfopt) (Some ial_kf)
  | IPAssigns {ias_kf; ias_kinstr=Kglobal; ias_bhv=Id_contract (_,bhv);
               ias_froms = region} ->
    Format.fprintf fmt "Assigns%a %a%a"
      pp_bhv bhv
      (pp_region kloc) region
      (pp_context kfopt) (Some ias_kf)
  | IPFrom {if_kf; if_kinstr=Kglobal; if_bhv=Id_contract (_,bhv);
            if_from = depend} ->
    Format.fprintf fmt "Froms%a %a%a"
      pp_bhv bhv
      (pp_region kloc) [depend]
      (pp_context kfopt) (Some if_kf)
  | IPAllocation {ial_kinstr; ial_bhv=Id_contract (active,bhv);
                  ial_allocs = (frees, allocates)} ->
    Format.fprintf fmt "Frees/Allocates%a %a/%a %a%a"
      pp_bhv bhv
      (pp_allocation kloc) frees
      (pp_allocation kloc) allocates
      (pp_opt kiopt (pp_kinstr kloc)) ial_kinstr
      (pp_opt kiopt pp_active) active
  | IPAssigns {ias_kinstr; ias_bhv=Id_contract (active,bhv); ias_froms = region} ->
    Format.fprintf fmt "Assigns%a %a%a%a"
      pp_bhv bhv
      (pp_region kloc) region
      (pp_opt kiopt (pp_kinstr kloc)) ias_kinstr
      (pp_opt kiopt pp_active) active
  | IPFrom {if_kinstr; if_bhv=Id_contract (active,bhv); if_from = depend} ->
    Format.fprintf fmt "Froms%a %a%a%a"
      pp_bhv bhv
      (pp_region kloc) [depend]
      (pp_opt kiopt (pp_kinstr kloc)) if_kinstr
      (pp_opt kiopt pp_active) active
  | IPAllocation {ial_bhv=Id_loop _; ial_allocs = (frees, allocates)} ->
    Format.fprintf fmt "Loop frees%a Loop allocates%a"
      (pp_allocation kloc) frees
      (pp_allocation kloc) allocates
  | IPAssigns {ias_bhv = Id_loop _; ias_froms = region} ->
    Format.fprintf fmt "Loop assigns %a" (pp_region kloc) region
  | IPFrom {if_bhv = Id_loop _; if_from = depend} ->
    Format.fprintf fmt "Loop froms %a" (pp_region kloc) [depend]
  | IPDecrease {id_kinstr = Kglobal} ->
    Format.fprintf fmt "Recursion variant"
  | IPDecrease {id_kinstr = Kstmt stmt} ->
    Format.fprintf fmt "Loop variant at %a" (pp_stmt kloc) stmt
  | IPReachable {ir_kf=None; ir_kinstr=Kglobal; ir_program_point = Before} ->
    (* print "Unreachable": it seems that it is what the user want to see *)
    Format.fprintf fmt "Unreachable entry point"
  | IPReachable {ir_kf=None; ir_kinstr=Kglobal; ir_program_point = After}
  | IPReachable {ir_kf=None; ir_kinstr=Kstmt _} -> assert false
  | IPReachable {ir_kf=Some _; ir_kinstr=Kstmt stmt;  ir_program_point=ba} ->
    (* print "Unreachable": it seems that it is what the user want to see *)
    Format.fprintf fmt "Unreachable %a%s"
      (pp_stmt kloc) stmt
      (match ba with Before -> "" | After -> " (after it)")
  | IPReachable {ir_kf=Some kf; ir_kinstr=Kglobal} ->
    (* print "Unreachable": it seems that it is what the user want to see *)
    Format.fprintf fmt "Unreachable %a" Kernel_function.pretty kf
  | IPPropertyInstance {ii_kf; ii_stmt; ii_ip} ->
    Format.fprintf fmt "Instance of '%a'%a%a@."
      (pp_prop kfopt kiopt kloc) ii_ip
      (pp_context kfopt) (Some ii_kf)
      (pp_opt kiopt (pp_kinstr kloc)) (Kstmt ii_stmt)



type kf = [ `Always | `Never | `Context of kernel_function ]

let pp_property = pp_prop `Always true true
let pp_localized ~kf ~ki ~kloc = pp_prop kf ki kloc
let pp_local = pp_prop `Never false false

let to_string pp elt =
  let b = Buffer.create 20 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt 1000000;
  pp fmt elt;
  Format.pp_print_flush fmt ();
  Buffer.contents b

let code_annot_kind_and_node code_annot = match code_annot.annot_content with
  | AAssert (_, kind, {pred_content; pred_name}) ->
    let kind = match Alarms.find code_annot with
      | Some alarm -> Alarms.get_name alarm
      | None ->
        if List.exists ((=) "missing_return") pred_name
        then "missing_return"
        else match kind with
          | Assert -> "user assertion"
          | Check -> "user check"
    in
    Some (kind, to_string Printer.pp_predicate_node pred_content)
  | AInvariant (_, _, {pred_content}) ->
    Some ("loop invariant", to_string Printer.pp_predicate_node pred_content)
  | _ -> None

let property_kind_and_node property =
  let default kind = Some (kind, to_string Property.pretty property) in
  let open Property in
  match property with
  | IPCodeAnnot {ica_ca} -> code_annot_kind_and_node ica_ca
  | IPPredicate {ip_kind; ip_pred} ->
    let kind = match ip_kind with
      | PKRequires _ -> "precondition"
      | PKAssumes _  -> "behavior assumption"
      | PKEnsures _  -> "postcondition"
      | PKTerminates -> "termination clause"
    in
    Some (kind, to_string Printer.pp_identified_predicate ip_pred)
  | IPPropertyInstance {ii_ip=IPPredicate {ip_kind=PKRequires _; ip_kf;
                                           ip_kinstr=Kglobal; ip_pred}} ->
    let kind = "precondition of " ^ Kernel_function.get_name ip_kf in
    Some (kind, to_string Printer.pp_identified_predicate ip_pred)
  | IPAssigns _  -> default "assigns clause"
  | IPFrom _     -> default "from clause"
  | IPComplete _ -> default "complete behaviors"
  | IPDisjoint _ -> default "disjoint behaviors"
  | _ -> None

let status_feedback status =
  let open Property_status.Feedback in
  match status with
  | Never_tried       -> "Ignored"
  | Considered_valid  -> "Considered valid"
  | Valid             -> "Valid"
  | Valid_under_hyp   -> "Partially proven"
  | Unknown           -> "Unknown"
  | Invalid           -> "Invalid"
  | Invalid_under_hyp -> "Invalid or unreachable"
  | Inconsistent      -> "Inconsistent"
  | Invalid_but_dead | Valid_but_dead | Unknown_but_dead -> "Dead"

(* -------------------------------------------------------------------------- *)
(* --- Property Comparison                                                --- *)
(* -------------------------------------------------------------------------- *)

type order =
  | I of int
  | S of string
  | F of Kernel_function.t
  | K of kinstr
  | B of funbehavior
  | A of Datatype.String.Set.t

let cmp_order a b = match a , b with
  | I a , I b -> Transitioning.Stdlib.compare a b
  | I _ , _ -> (-1)
  | _ , I _ -> 1
  | S a , S b -> String.compare a b
  | S _ , _ -> (-1)
  | _ , S _ -> 1
  | F f , F g -> Kernel_function.compare f g
  | F _ , _ -> (-1)
  | _ , F _ -> 1
  | B a , B b ->
    begin
      match Cil.is_default_behavior a , Cil.is_default_behavior b with
      | true , true -> 0
      | true , false -> (-1)
      | false , true -> 1
      | false , false -> String.compare a.b_name b.b_name
    end
  | B _ , _ -> (-1)
  | _ , B _ -> 1
  | K a , K b -> Cil_datatype.Kinstr.compare a b
  | K _, _ -> (-1)
  | _, K _ -> 1
  | A a1, A a2 -> Datatype.String.Set.compare a1 a2

let rec cmp xs ys = match xs,ys with
  | [],[] -> 0
  | [],_ -> (-1)
  | _,[] -> 1
  | x::xs,y::ys ->
    let c = cmp_order x y in
    if c<>0 then c else cmp xs ys

let kind_order = function
  | PKRequires bhv -> [B bhv;I 1]
  | PKAssumes bhv -> [B bhv; I 2]
  | PKEnsures(bhv,Normal) -> [B bhv;I 3]
  | PKEnsures(bhv,Breaks) -> [B bhv;I 4]
  | PKEnsures(bhv,Continues) -> [B bhv;I 5]
  | PKEnsures(bhv,Returns) -> [B bhv;I 6]
  | PKEnsures(bhv,Exits) -> [B bhv;I 7]
  | PKTerminates -> [I 8]

let named_order xs = List.map (fun x -> S x) xs
let for_order k = function
  | [] -> [I k]
  | bs -> I (succ k) :: named_order bs
let annot_order = function
  | {annot_content=AAssert(bs,Check,np)} ->
    for_order 0 bs @ named_order np.pred_name
  | {annot_content=AAssert(bs,Assert,np)} ->
    for_order 2 bs @ named_order np.pred_name
  | {annot_content=AInvariant(bs,_,np)} ->
    for_order 4 bs @ named_order np.pred_name
  | _ -> [I 6]
let loop_order = function
  | Id_contract (active,b) -> [B b; A active]
  | Id_loop _ -> []

let rec ip_order = function
  | IPAxiomatic {iax_name=a} -> [I 0;S a]
  | IPAxiom {il_name=a} | IPLemma {il_name=a} -> [I 1;S a]
  | IPOther {io_name=s;io_loc=OLContract kf} -> [I 3;F kf;S s]
  | IPOther {io_name=s;io_loc=OLStmt (kf, stmt)} -> [I 4;F kf;K (Kstmt stmt);S s]
  | IPOther {io_name=s;io_loc=OLGlob _} -> [I 5; S s]
  | IPBehavior {ib_kf;ib_kinstr;ib_active;ib_bhv} ->
    [I 6;F ib_kf;K ib_kinstr;B ib_bhv; A ib_active]
  | IPComplete {ic_kf;ic_kinstr;ic_active;ic_bhvs} ->
    [I 7;F ic_kf;K ic_kinstr; A ic_active] @ for_order 0 ic_bhvs
  | IPDisjoint {ic_kf;ic_kinstr;ic_active;ic_bhvs} ->
    [I 8;F ic_kf;K ic_kinstr; A ic_active] @ for_order 0 ic_bhvs
  | IPPredicate {ip_kind;ip_kf;ip_kinstr} ->
    [I 9;F ip_kf;K ip_kinstr] @ kind_order ip_kind
  | IPCodeAnnot {ica_kf;ica_stmt;ica_ca} ->
    [I 10;F ica_kf;K(Kstmt ica_stmt)] @ annot_order ica_ca
  | IPAllocation {ial_kf;ial_kinstr;ial_bhv} ->
    [I 11;F ial_kf;K ial_kinstr] @ loop_order ial_bhv
  | IPAssigns {ias_kf; ias_kinstr; ias_bhv} ->
    [I 12;F ias_kf;K ias_kinstr] @ loop_order ias_bhv
  | IPFrom {if_kf;if_kinstr;if_bhv} ->
    [I 13;F if_kf;K if_kinstr] @ loop_order if_bhv
  | IPDecrease {id_kf;id_kinstr;id_ca=None} -> [I 14;F id_kf;K id_kinstr]
  | IPDecrease {id_kf;id_kinstr;id_ca=Some a} ->
    [I 15;F id_kf;K id_kinstr] @ annot_order a
  | IPReachable {ir_kf=None} -> [I 16]
  | IPReachable {ir_kf=Some kf; ir_kinstr} -> [I 17;F kf;K ir_kinstr]
  | IPPropertyInstance {ii_kf; ii_stmt; ii_ip} ->
    [I 18; F ii_kf; K (Kstmt ii_stmt)] @ ip_order ii_ip
  | IPTypeInvariant {iti_name=a} -> [I 19; S a]
  | IPGlobalInvariant {igi_name=a} -> [I 20; S a]
  | IPExtended {ie_loc=ELContract kf;ie_ext={ext_name}} -> [I 21;F kf; S ext_name]
  | IPExtended {ie_loc=ELStmt (kf, stmt);ie_ext={ext_name}} ->
    [ I 22; F kf; K (Kstmt stmt); S ext_name]
  | IPExtended {ie_loc=ELGlob; ie_ext={ext_name}} -> [ I 23; S ext_name]

let pp_compare p q = cmp (ip_order p) (ip_order q)

let full_compare p q =
  let cmp = pp_compare p q in
  if cmp<>0 then cmp else Property.compare p q







(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
