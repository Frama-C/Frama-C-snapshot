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

open Cil_types
open Cil_datatype

(*----------------------------------------------------------------------------*)
(* Property identification                                                    *)
(*----------------------------------------------------------------------------*)

(** Beside the property identification, it can be found in different contexts
 * depending on which part of the computation is involved.
 * For instance, properties on loops are split in 2 parts : establishment and
 * preservation.
 *)

type prop_kind =
  | PKProp        (** normal property *)
  | PKEstablished (** computation related to a loop property before the loop. *)
  | PKPreserved   (** computation related to a loop property inside the loop. *)
  | PKPropLoop    (** loop property used as hypothesis inside a loop. *)
  | PKVarDecr     (** computation related to the decreasing of a variant in a loop *)
  | PKVarPos      (** computation related to a loop variant being positive *)
  | PKAFctOut     (** computation related to the function assigns on normal termination *)
  | PKAFctExit    (** computation related to the function assigns on exit termination *)
  | PKPre of kernel_function * stmt * Property.t (** precondition for function
        at stmt, property of the require. Many information that should come
        from the p_prop part of the prop_id, but in the PKPre case,
        it seems that it is hiden in a IPBlob property ! *)

type prop_id = {
  p_kind : prop_kind ;
  p_prop : Property.t ;
  p_part : (int * int) option ;
}

let parts_of_id p = p.p_part
let mk_part pid (k, n) = { pid with p_part = Some (k,n) }
let property_of_id p = p.p_prop
           
exception Found of int
let num_of_bhv_from bhv (out, _) =
    match bhv.b_assigns with
        WritesAny -> Wp_parameters.fatal "no \\from in this behavior ???"
      | Writes l ->
          let add n (o, f) = match f with FromAny -> n
            | From _ ->
                if Logic_utils.is_same_identified_term out o then
                  raise (Found n)
                else n+1
          in
            try
              let _ = List.fold_left add 1 l in
                Wp_parameters.fatal "didn't found this \\from"
            with Found n -> n

(*----------------------------------------------------------------------------*)
(* Constructors *)
(*----------------------------------------------------------------------------*)

let mk_annot_id kf stmt ca = Property.ip_of_code_annot_single kf stmt ca

let mk_prop kind prop = { p_kind=kind ; p_prop=prop ; p_part=None }

let mk_code_annot_id kf s ca = mk_prop PKProp  (mk_annot_id kf s ca)
let mk_assert_id kf s ca = mk_prop PKProp  (mk_annot_id kf s ca)
let mk_establish_id  kf s ca = mk_prop PKEstablished (mk_annot_id kf s ca)
let mk_preserve_id   kf s ca = mk_prop PKPreserved (mk_annot_id kf s ca)
let mk_inv_hyp_id    kf s ca = mk_prop PKPropLoop  (mk_annot_id kf s ca)
let mk_var_decr_id   kf s ca = mk_prop PKVarDecr (mk_annot_id kf s ca)
let mk_var_pos_id    kf s ca = mk_prop PKVarPos  (mk_annot_id kf s ca)

let mk_loop_from_id kf s ca from =
  let id = Property.ip_of_from kf (Kstmt s) (Property.Id_code_annot ca) from in
    mk_prop PKPropLoop id

let mk_bhv_from_id kf ki bhv from =
  let id = Property.ip_of_from kf ki (Property.Id_behavior bhv) from in 
    mk_prop PKProp id

let get_kind_for_tk kf tkind = match tkind with
  | Normal -> 
      if Cil2cfg.has_exit (Cil2cfg.get kf) then PKAFctOut else PKProp
  | Exits -> PKAFctExit
  | _ -> assert false

let mk_fct_from_id kf bhv tkind from =
  let id = Property.ip_of_from kf Kglobal (Property.Id_behavior bhv) from in 
  let kind = get_kind_for_tk kf tkind in
    mk_prop kind id

let mk_disj_bhv_id (kf,ki,disj)  =
  mk_prop PKProp (Property.ip_of_disjoint kf ki disj)
let mk_compl_bhv_id (kf,ki,comp) =
  mk_prop PKProp (Property.ip_of_complete kf ki comp)
let mk_decrease_id (kf, s, x)  =
  mk_prop PKProp (Property.ip_of_decreases kf s x)

let mk_axiom_id name  = mk_prop PKProp (Property.ip_axiom name)

let mk_stmt_assigns_id kf s b a =
  let b = Property.Id_behavior b in
  let p = Property.ip_of_assigns kf (Kstmt s) b (Writes a) in
  Extlib.opt_map (mk_prop PKProp) p

let mk_loop_assigns_id kf s ca a =
  let ca = Property.Id_code_annot ca in
  let p = Property.ip_of_assigns kf (Kstmt s) ca (Writes a) in
  Extlib.opt_map (mk_prop PKPropLoop) p

let mk_fct_assigns_id kf b tkind a = 
  let b = Property.Id_behavior b in
  let kind = get_kind_for_tk kf tkind in
  let p = Property.ip_of_assigns kf Kglobal b (Writes a) in
  Extlib.opt_map (mk_prop kind) p

let mk_pre_id kf ki b p =
  mk_prop PKProp (Property.ip_of_requires kf ki b p)

let mk_stmt_post_id kf s b p =
  mk_prop PKProp (Property.ip_of_ensures kf (Kstmt s) b p)

let mk_fct_post_id kf b p =
  mk_prop PKProp (Property.ip_of_ensures kf Kglobal b p)

let mk_call_pre_id called_kf s_call called_pre called_pre_p =
  let kind = PKPre (called_kf, s_call, called_pre) in
    mk_prop kind called_pre_p

(*----------------------------------------------------------------------------*)

module Prop_id_datatype =
  Datatype.Make(
    struct
      type t = prop_id
      include Datatype.Undefined
      let name = "WpAnnot.prop_id"
      let reprs =
        List.map
          (fun x -> { p_kind = PKProp; p_prop = x; p_part = None })
          Property.reprs
    end)




(*----------------------------------------------------------------------------*)
(* Names and Printing *)
(* [JS 2011/08/04] This stuff seems to be related only to Property.t.
   Maybe better to put it in a kernel-friendly way somewhere in module
   [Property]? *)
(*----------------------------------------------------------------------------*)

let pp_names fmt l =  match l with [] -> ()
  | _ ->
      Format.fprintf fmt "_%a" (Wp_error.pp_string_list ~empty:"" ~sep:"_") l

let code_annot_names ca = match ca.annot_content with
  | AAssert (_, named_pred) | AInvariant (_,_,named_pred) -> named_pred.name
  | AVariant (term, _) -> term.term_name
  | _ -> [] (* TODO : add some more names ? *)

(** This is used to give the name of the property that the user can give
 * to select it from the command line (-wp-prop option) *)
let user_prop_names p = match p with
    | Property.IPPredicate (_,_,_,idp) -> idp.ip_name
    | Property.IPCodeAnnot (_,_, ca) -> code_annot_names ca
    | Property.IPComplete (_, _, lb) ->
        let name =
          Pretty_utils.sfprintf  "complete_behaviors%a" pp_names lb
        in [name]
    | Property.IPDisjoint (_, _, lb) ->
        let name = Pretty_utils.sfprintf  "disjoint_behaviors%a" pp_names lb
        in [name]
    | Property.IPAssigns (_, _, _, l) ->
      List.fold_left
        (fun acc (t,_) -> t.it_content.term_name @ acc) ["assigns"] l
    | Property.IPFrom _ -> ["from"] (* TODO: steal term names from assigns? *)
    | Property.IPDecrease (_,_, Some ca,_) -> code_annot_names ca
    | Property.IPDecrease _ -> ["decreases"](*TODO: add more names ? *)
    | Property.IPAxiom _
    | Property.IPAxiomatic _
    | Property.IPLemma _
    | Property.IPBehavior _
    | Property.IPUnreachable _
    | Property.IPOther _ -> []

let string_of_termination_kind = function
    Normal -> "post"
  | Exits -> "exits"
  | Breaks -> "breaks"
  | Continues -> "continues"
  | Returns -> "returns"

let predicate_kind_txt pk ki = match pk, ki with
  | Property.PKRequires _, Kglobal -> "pre"
  | Property.PKRequires _, Kstmt _ -> "stmt_pre"
  | Property.PKAssumes _, _ -> "assume"
  | Property.PKEnsures (_, tk), Kglobal ->
      string_of_termination_kind tk
  | Property.PKEnsures (_, tk), Kstmt _ ->
      "stmt_" ^ (string_of_termination_kind tk)
  | Property.PKTerminates, _ -> "terminates"

let id_prop_txt p = match p with
    | Property.IPPredicate (pk,_,ki,idp) ->
        Pretty_utils.sfprintf "%s_%d%a"
         (predicate_kind_txt pk ki)  idp.ip_id pp_names idp.ip_name
    | Property.IPCodeAnnot (_,_, ca) ->
        let name = match ca.annot_content with
          | AAssert _ -> "assert"
          | AInvariant _ -> "loop_inv"
          | APragma _ -> "code_annot"
          | _ -> assert false
        in Pretty_utils.sfprintf "%s_%d%a" name ca.annot_id
          pp_names (code_annot_names ca)
    | Property.IPComplete (_, _, lb) ->
        Pretty_utils.sfprintf  "complete_behaviors%a" pp_names lb
    | Property.IPDisjoint (_, _, lb) ->
        Pretty_utils.sfprintf  "disjoint_behaviors%a" pp_names lb
    | Property.IPDecrease (_,_,None,_) -> "decreases"
    | Property.IPDecrease _ -> "loop_variant"
    | Property.IPAxiom name -> "axiom_" ^ name
    | Property.IPAxiomatic(name, _) -> "axiomatic_" ^ name
    | Property.IPLemma name -> "lemma_" ^ name
    | Property.IPAssigns (_kf, ki, _bhv, _) ->
        let name = match ki with
          | Kglobal -> "function_assigns"
          | Kstmt s ->
              match s.skind with
                | Loop _ -> "loop_assigns_" ^ string_of_int s.sid
                | _ -> "stmt_assigns_" ^ string_of_int s.sid
        in name
    | Property.IPFrom (_, _, _, (out,_)) -> 
        "from_id_"^(string_of_int (out.it_id))
    | Property.IPUnreachable _ -> "unreachable stmt"
    | Property.IPBehavior(_, _, b) -> b.b_name
    | Property.IPOther(s,_,_) -> s

let name_of_prop_id p = match p.p_kind , p.p_prop with
  | PKProp , Property.IPAssigns (_kf, (Kstmt s), _, _) ->
      "stmt_assigns_" ^ string_of_int s.sid
  | PKProp , p -> id_prop_txt p
  | PKPropLoop , Property.IPAssigns (_kf, (Kstmt s), _, _) ->
      "loop_assigns_" ^ string_of_int s.sid
  | PKPropLoop , p -> id_prop_txt p
  | PKEstablished , p -> id_prop_txt p ^ "_established"
  | PKPreserved , p -> id_prop_txt p ^ "_preserved"
  | PKVarDecr , p -> id_prop_txt p ^ "_decrease"
  | PKVarPos , p -> id_prop_txt p ^ "_positive"
  | PKAFctOut , Property.IPFrom _ -> "normal_from"
  | PKAFctExit , Property.IPFrom _ -> "exit_from"
  | PKAFctOut , _ -> "normal_assigns"
  | PKAFctExit , _ -> "exit_assigns"
  | PKPre(kf,stmt,p) , _ ->
      let pid = match p with
        | Property.IPCodeAnnot(_,_,p) -> p.annot_id
        | Property.IPPredicate(_,_,_,p) -> p.ip_id
        | property -> Wp_parameters.fatal "No precondition id for @[%a@]" 
                        Property.pretty property
      in
      Printf.sprintf "pre%d_%s_s%d" pid (Kernel_function.get_name kf) stmt.sid

let prop_id_name p = match p.p_part with
  | None -> name_of_prop_id p
  | Some(k,_) -> Printf.sprintf "%s_part%d" (name_of_prop_id p) (succ k)

let label_of_kind = function
  | PKProp -> "Property"
  | PKPropLoop -> "Invariant" (* should be assert false ??? *)
  | PKEstablished -> "Establishment"
  | PKPreserved -> "Preservation"
  | PKVarDecr -> "Decreasing"
  | PKVarPos -> "Positive"
  | PKAFctOut -> "Function assigns"
  | PKAFctExit -> "Exit assigns"
  | PKPre(kf,_,_) ->
      Printf.sprintf "Precondition for '%s'" (Kernel_function.get_name kf)

let label_of_prop_id p =
  match p.p_part with
    | None -> label_of_kind p.p_kind
    | Some(k,n) ->
        Printf.sprintf "%s (%d/%d)" (label_of_kind p.p_kind) (succ k) n

let pp_id_name fmt pid = Format.fprintf fmt "%s" (prop_id_name pid)

(*----------------------------------------------------------------------------*)
(* Pretty-Print *)
(*----------------------------------------------------------------------------*)

let pp_goal_kind fmt = function
  | PKProp
  | PKPropLoop
  | PKAFctOut
  | PKAFctExit
  | PKPre _ -> ()
  | PKEstablished -> Format.pp_print_string fmt "Establishment of "
  | PKPreserved -> Format.pp_print_string fmt "Preservation of "
  | PKVarDecr -> Format.pp_print_string fmt "Decreasing of "
  | PKVarPos -> Format.pp_print_string fmt "Positivity of "

let pp_goal_part fmt = function
  | None -> ()
  | Some(k,n) -> Format.fprintf fmt " (%d/%d)" (succ k) n

let pretty fmt pid =
  begin
    pp_goal_kind fmt pid.p_kind ;
    Description.pp_property fmt pid.p_prop ;
    pp_goal_part fmt pid.p_part ;
  end

let pretty_context kf fmt pid =
  begin
    pp_goal_kind fmt pid.p_kind ;
    Description.pp_localized ~kf:(`Context kf) ~ki:true fmt pid.p_prop ;
    pp_goal_part fmt pid.p_part ;
  end

(*----------------------------------------------------------------------------*)
(* Comparison *)
(*----------------------------------------------------------------------------*)

let kind_order = function
  | PKProp -> 0
  | PKPre _ -> 0
  | PKEstablished -> 1
  | PKPreserved -> 2
  | PKVarPos -> 3
  | PKVarDecr -> 4
  | PKPropLoop -> 5
  | PKAFctOut -> 6
  | PKAFctExit -> 7

let compare_prop_id pid1 pid2 =
  (* This order of compatison groups together prop_pids with same properties *)
  let p1 = property_of_id pid1 in
  let p2 = property_of_id pid2 in
  let cmp = Property.compare p1 p2 in
  if cmp <> 0 then cmp
  else
    let cmp = kind_order pid2.p_kind - kind_order pid1.p_kind in
    if cmp <> 0 then cmp
    else
      Pervasives.compare pid1.p_part pid2.p_part

let is_assigns p =
  match property_of_id p with
    | Property.IPAssigns _ -> true
    | _ -> false

let is_requires = function
  | Property.IPPredicate (Property.PKRequires _,_,_,_) -> true
  | _ -> false

let select_by_name asked pid =
  let p_prop = match pid.p_kind with
    | PKPre (_,_,p_prop) -> p_prop
    | _ -> property_of_id pid
  in
  let names = user_prop_names p_prop in
  let take_it, msg = 
    if List.mem asked names
    then true, " (asked named prop)"
    else false, (match names with [] -> " (no names)"
                   | name::_ -> (" (asked name <> "^ name^")"))
  in take_it, msg

let select_call_pre s_call asked_pre pid =
  let take_it, msg = match pid.p_kind with
    | PKPre (_, p_stmt, p_prop) ->
        if  Stmt.equal s_call p_stmt then
          let x = match asked_pre with
            | None -> true, ""
            | Some asked_pre ->
                if Property.equal p_prop asked_pre then true, ""
                else false, " (stmt ok, but not not the asked pre)"
          in x
          else false, " (not the asked stmt)"
    | _ -> false, " (not a call pre)"
  in take_it, msg


(*----------------------------------------------------------------------------*)
(* About assigns identification *)
(*----------------------------------------------------------------------------*)

(** TODO: it seems that this type is not used anymore... *)
type a_fun = Assigns_FctOut | Assigns_FctExit | Assigns_Stmt | Assigns_Loop

type a_kind = LoopAssigns | StmtAssigns

type assigns_desc = {
  a_label : Cil_types.logic_label ;
  (* a_fun : a_fun ; *)
  a_kind : a_kind ;
  a_assigns : Cil_types.identified_term Cil_types.assigns ;
}

let mk_loop_assigns_desc s assigns = {
  a_label = Clabels.mk_logic_label s ;
  (* a_fun = Assigns_Loop ; *)
  a_kind = LoopAssigns ;
  a_assigns = Writes assigns
}

let mk_stmt_assigns_desc s assigns = {
  a_label =  Clabels.mk_logic_label s ;
  (* a_fun = Assigns_Stmt ; *)
  a_kind = StmtAssigns ;
  a_assigns = Writes assigns ;
}

(*
(** kf assigns for normal path when there is an exit path *)
let mk_fout_assigns_desc assigns = {
  a_label = Logic_const.pre_label ; 
  (* a_fun = Assigns_FctOut ;  *)
  a_kind = StmtAssigns ;
  a_assigns = Writes assigns ;
}

(** kf assigns for exit path *)
let mk_exit_assigns_desc assigns = {
  a_label = Logic_const.pre_label ; 
  (* a_fun = Assigns_FctExit ; *)
  a_kind = StmtAssigns ;
  a_assigns = Writes assigns ;
}
*)

let mk_kf_assigns_desc assigns = {
  a_label = Logic_const.pre_label ; 
  (* a_fun = Assigns_Stmt ; *)
  a_kind = StmtAssigns ;
  a_assigns = Writes assigns ;
}

let pp_assigns_desc fmt a = Wp_error.pp_assigns fmt a.a_assigns
(*----------------------------------------------------------------------------*)
(**
 * 2 kinds of annotations can be found : predicates and assigns.
 * because assigns properties can only be translated into predicates
 * by the memory model.
 * - Assigns properties are composed of the assigns list from Cil,
 * and a label to know where to stop.
 * - Predicates are just the predicate type from Cil.
 *)
(*----------------------------------------------------------------------------*)

type pred_info = prop_id * Cil_types.predicate named

let mk_pred_info id p = (id, p)
let pred_info_id (id, _) = id
let pp_pred_of_pred_info fmt (_id, p) = !Ast_printer.d_predicate_named fmt p
let pp_pred_info fmt (id, p) =
  Format.fprintf fmt "(@[%a:@ %a@])" pp_id_name id !Ast_printer.d_predicate_named p

type assigns_info = prop_id * assigns_desc

let assigns_info_id (id,_) = id

type assigns_full_info = 
    AssignsLocations of assigns_info
  | AssignsAny of assigns_desc
  | NoAssignsInfo

let empty_assigns_info = NoAssignsInfo
let mk_assigns_info id a = AssignsLocations (id, a)

let mk_stmt_any_assigns_info s = 
  let a = { a_label = Clabels.mk_logic_label s; (* a_fun = Assigns_Stmt; *)
            a_kind = StmtAssigns; a_assigns = WritesAny } in
  AssignsAny a

let mk_kf_any_assigns_info () = 
  let a = { a_label = Logic_const.pre_label; (* a_fun = Assigns_Stmt; *)
            a_kind = StmtAssigns; a_assigns = WritesAny } in
  AssignsAny a

let mk_loop_any_assigns_info s = 
  let a = { a_label = Clabels.mk_logic_label s; (* a_fun = Assigns_Loop; *)
            a_kind = LoopAssigns; a_assigns = WritesAny } in
  AssignsAny a

let pp_assigns_id (id, _a) = pp_id_name id

let pp_assign_info k fmt a = match a with
  | NoAssignsInfo -> ()
  | AssignsAny a ->
      let pkind =
        match a.a_kind with
          | StmtAssigns -> ""
          | LoopAssigns -> "loop"
      in
        Format.fprintf fmt "%s(@@%a): %s assigns everything@."
          k Wp_error.pp_logic_label a.a_label pkind
  | AssignsLocations (_,a) -> Format.fprintf fmt "%s(@@%a): %a@." k
                                Wp_error.pp_logic_label a.a_label
                                pp_assigns_desc a

let merge_assign_info a1 a2 = match a1,a2 with
  | NoAssignsInfo, a | a, NoAssignsInfo -> a
  | (AssignsLocations _ | AssignsAny _),
    (AssignsLocations _ | AssignsAny _) ->
        Wp_parameters.fatal "Several assigns ?" 


type t_axiom = string * Cil_types.logic_label list * Cil_types.predicate named
type axiom_info = prop_id * t_axiom

let mk_axiom_info name labels p =
  let id = mk_axiom_id name in (id, (name, labels, p))

let pp_axiom_info fmt (id, (_name, _labels, p)) = 
  Format.fprintf fmt "(@[%a:@ %a@])" pp_id_name id !Ast_printer.d_predicate_named p

(*----------------------------------------------------------------------------*)
(** About proofs *)
(*----------------------------------------------------------------------------*)
let subproofs id = match id.p_kind with
  | PKProp | PKPre _ | PKPropLoop -> 1
  | PKEstablished | PKPreserved
  | PKVarDecr | PKVarPos
  | PKAFctExit | PKAFctOut -> 2

let subproof_idx id = match id.p_kind with
  | PKProp | PKPre _ | PKPropLoop -> 0 (* 1/1 *)
  | PKPreserved  -> 0 (* 1/2 *)
  | PKEstablished-> 1 (* 2/2 *)
  | PKVarDecr    -> 0 (* 1/2 *)
  | PKVarPos     -> 1 (* 2/2 *)
  | PKAFctOut    -> 0 (* 1/2 *)
  | PKAFctExit   -> 1 (* 2/2 *)
(** find the outer loop in which the stmt is. *)
let get_loop_stmt kf stmt =
  (* because we don't have the cfg here, we can only use Cil information,
  * and then we can only recognize syntactic loops... TODO: use the cfg ? *)
  let rec is_in_blk b = List.exists is_in_stmt b.bstmts
  and is_in_stmt s = if s.sid = stmt.sid then true
    else match s.skind with
      | If (_, b1, b2,_) -> is_in_blk b1 || is_in_blk b2
      | Switch (_, b, _, _) | Block b -> is_in_blk b
      | UnspecifiedSequence seq ->
          let b = Cil.block_from_unspecified_sequence seq in
            is_in_blk b
      | Loop (_, b, _, _, _) -> is_in_blk b
      | _ -> false
  and find_loop_in_blk blk = find_loop_in_stmts blk.bstmts
  and find_loop_in_stmts l = match l with
    | [] -> None
    | s::tl ->
        (match find_loop_in_stmt s with Some l -> Some l
           | None -> find_loop_in_stmts tl)
  and find_loop_in_stmt s = match s.skind with
    | (Loop _) -> if is_in_stmt s then Some s else None
    | If (_, b1, b2,_) ->
        (match find_loop_in_blk b1 with Some l -> Some l
           | None -> find_loop_in_blk b2)
    | Switch (_, b, _, _) | Block b -> find_loop_in_blk b
    | UnspecifiedSequence seq ->
        let b = Cil.block_from_unspecified_sequence seq in
          find_loop_in_blk b
    | _ -> None
  in let f = Kernel_function.get_definition kf in
    find_loop_in_blk f.sbody

(** Quite don't understand what is going on here... what is it supposed to do ?
* [2011-07-07-Anne] *)
let get_induction p =
  let get_stmt = function
    | Property.IPDecrease(kf,Kstmt stmt,_,_) -> Some (kf, stmt)
    | Property.IPCodeAnnot(kf,stmt,_) -> Some (kf, stmt)
    | Property.IPAssigns(kf,Kstmt stmt,_,_) -> Some (kf, stmt)
    | _ -> None
  in match p.p_kind with
    | PKAFctOut|PKAFctExit|PKPre _ -> None
    | PKProp ->
        let loop_stmt_opt = match get_stmt (property_of_id p) with
          | None -> None
          | Some (kf, s) -> get_loop_stmt kf s
        in loop_stmt_opt
    | PKPropLoop ->
        let loop_stmt_opt = match property_of_id p with
          | Property.IPCodeAnnot(kf,stmt,
                                 {annot_content = AInvariant(_, loop, _)})
            ->
              if loop then (*loop invariant *) Some stmt
              else (* invariant inside loop *) get_loop_stmt kf stmt
          | Property.IPAssigns (_, Kstmt stmt, Property.Id_code_annot _, _) ->
              (* loop assigns *) Some stmt
          | _ -> None (* assert false ??? *)
        in loop_stmt_opt
    | PKEstablished|PKVarDecr|PKVarPos|PKPreserved ->
        (match get_stmt (property_of_id p) with 
           | None -> None | Some (_, s) -> Some s)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
