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

let dkey = "froms" (* debugging key *)

(** This file groups functions needed to check the fonctional dependencies *)

open Cil_types

exception NoFromForBhv
exception NoFromForLoop of stmt
exception NoFromForCall of stmt

(* -------------------------------------------------------------------------- *)
(** Build a full qualified name for logic_info about the nth from in the b
* behavior of the kf function. *)
let mk_name prefix kf ki b nth sufix =
  let ki_info = match ki with Kglobal -> ""
    |  Kstmt s -> ("_stmt"^(string_of_int s.sid))
  in
  let bhv_name =
    if b.b_name =  Cil.default_behavior_name then "" else ("_"^b.b_name)
  in
    Pretty_utils.sfprintf "%s%a%s%s_%d%s"
      prefix
      Kernel_function.pretty  kf
      bhv_name
      ki_info
      nth
      sufix

(** Build the logic type of the function that takes parameters of [in_types]
* and return an [out_type] result ([None] for a predicate) *)
let mk_linfo_type (out_type, in_types) =
  let lvar_out_type = match out_type with Some t -> t
    | None -> (* TODO: ugly ! but see in Logic_typing.logic_decl *)
        Ctype Cil.voidType
  in
  let ltype = match in_types with [] -> lvar_out_type
    | _ -> Larrow (in_types, lvar_out_type)
  in ltype

(** Build a [logic_info] with [fname] and the signature given by
* [(out_type, in_types)] (see {!mk_linfo_type})
* TODO: should be [Cil_const.make_logic_info] when it be finished. *)
let make_logic_info fname (out_type, in_types) =
  let ltype = mk_linfo_type (out_type, in_types) in
  let lvar = Cil_const.make_logic_var fname ltype in
  let mk_in_lvar t = Cil_const.make_logic_var "x" t in
  let in_vars = List.map mk_in_lvar in_types in
  let linfo = {
    l_var_info = lvar;
    l_labels = [];
    l_tparams = [];
    l_type = out_type;
    l_profile = in_vars;
    l_body = LBnone;
  } in linfo

(** Find the [logic_info] for the given name and signature.
* Build and register it if it doesn't exist yet.
* *)
let get_linfo name sgn =
  let ptype = mk_linfo_type sgn in
  let info_ok info = Logic_utils.is_same_type info.l_var_info.lv_type ptype in
    match Logic_env.find_all_logic_functions name with
      | [] -> 
          let linfo = make_logic_info name sgn in
            Logic_utils.add_logic_function linfo;
            linfo
      | info::[] when info_ok info -> info
      | _ -> Wp_parameters.fatal "several function named %s ???" name

let mk_bhv_implicit_fun_name kf ki b n =
  mk_name "FI_" kf ki b n ""

let mk_loop_implicit_fun_name s n =
  Pretty_utils.sfprintf "Floop%d_%d" s.sid n

let get_pred_linfo kf ki bhv nth t = 
  let name = mk_name "Pfrom_" kf ki bhv nth ""in
    get_linfo name (None, [t])

let get_init_linfo kf ki bhv n_assigns n_from t = 
  let name = mk_name "Init_" kf ki bhv n_assigns ("_"^(string_of_int n_from)) in
  get_linfo name (Some t, [(*Linteger*)])

(** Build the implicit function for the nth assign clause of behavior b
* in the ki element of function kf. *)
let get_implicit_fun name (out_type, inputs_type) =
  let linfos = Logic_env.find_all_logic_functions name in
  let f = match linfos with
    | f::[] -> f
    | _::_ -> Wp_parameters.fatal "several functions named %s" name
    | [] ->
        let linfo = make_logic_info name (Some out_type, inputs_type) in
          Logic_utils.add_logic_function linfo;
          linfo
  in f
(** Build the lvalue [ \at (mlab, * (\at (addrlab, & elem))) ].
* This is needed because the left part of assigns properties is an lvalue
* which address has to be interpreted in the pre-state ([addrlab]), 
* but its value is to be considered in the post-state ([mlab]). *)
let build_elem_opt ~addrlab ~mlab elem = 
  let mk_mem_at t = 
    if Logic_utils.is_same_logic_label mlab Logic_const.here_label then t
    else Logic_const.tat (t, mlab) 
  in
  if Logic_utils.is_same_logic_label addrlab mlab then
    Some (mk_mem_at elem)
  else match elem.term_node with
    | TLval (h, off) ->
      let mk_addr_at t = Logic_const.tat (t, addrlab) in
      let rec mk_at_off off = match off with TNoOffset -> off
        | TField (f, off) -> TField (f, mk_at_off off)
        | TIndex (i, off) -> TIndex (mk_addr_at i, mk_at_off off)
      in
      let off' = mk_at_off off in
      let h' = match h with
        | TVar _ | TResult _ -> h
        | TMem p -> TMem (mk_addr_at p)
      in
      let lv' = TLval (h', off') in
      let elem' = Logic_const.term lv' elem.term_type in
      let elem' = mk_mem_at elem' in
        Some (elem')
  | _ ->
      Wp_parameters.not_yet_implemented
        "assigns left part is not a lvalue: %a" Cil.d_term elem

(** see [build_elem_opt] above. *)
let build_elem ~addrlab ~mlab elem =
  match build_elem_opt ~addrlab ~mlab elem with None -> assert false
    | Some elem -> elem

(** Build the left part of a contract [assigns] property 
Process [\result] and [\exit_status] according to [termination_kind].
Returns [None] if [out] is not compatible with [termination_kind].
* *)
let build_post_output termination_kind output =
  let out = output.it_content in
  let out = match out.term_node with (* remove \at(\result,Post) *)
    | Tat ({term_node=(TLval(TResult _,_) as tr)}, LogicLabel (_, "Post")) ->
        Logic_const.term tr out.term_type
    | _ -> out
  in
  match termination_kind, out.term_node with
    | Exits, TLval (TResult _, _ )  -> None
    | Normal, TLval (TVar{lv_name = "\\exit_status"},_) -> None
    | _, _ ->
        build_elem_opt
          ~addrlab:Logic_const.old_label ~mlab:Logic_const.here_label out

(** Build [P(out)] where [out] is the left part of the assigns property.
Process [\result] and [\exit_status] according to [termination_kind].
Returns [None] if [out] is not compatible with [termination_kind].
**)
let mk_assign_post kf bhv nth termination_kind (output, _) =
  match build_post_output termination_kind output with
    | None -> None
    | Some out' ->
        let linfos = get_pred_linfo kf Kglobal bhv nth out'.term_type in
        let p = Logic_const.papp (linfos, [], [out']) in
          Some (Logic_const.new_predicate p)

module Vars = struct
  let new_vars = ref []

  let get_and_init () =
    let vars = !new_vars in
      new_vars := [];
      vars

  let mk_new name ty =
    (** Notice that [make_logic_var] create a frech variable.
    * This is intended since several calls shouldn't share the same variable ! 
    **)
    let v = Cil_const.make_logic_var name ty in
      new_vars := v::!new_vars;
      v
end

(** Build [out = f_n (inputs)]. 
* The correct label \at should already be in [output] and [inputs].
* @raise NoFromForBhv if [inputs = None] meaning [FromAny].
**)
let build_fimpl_eq fi_name output inputs = 
  let out_type = output.term_type in
  let fun_impl = match inputs with
  | None ->
      let var = Vars.mk_new fi_name out_type in
        Logic_const.tvar var
  | Some inputs ->
      let fimpl_sig = (out_type, List.map (fun i -> i.term_type) inputs) in
      let fun_impl = get_implicit_fun fi_name fimpl_sig in
        Logic_const.term (Tapp (fun_impl, [], inputs)) out_type 
  in let p_eq = Logic_const.prel (Req, output, fun_impl) in
    p_eq


(** @return the list of pair [from, out_i = implicit_fun_i (inputs)] 
* for each [out_i \from inputs] assigns property of the behavior.
* The [from] part is for identification purpose later on.
* [implicit_fun_i] is the implicit fonction for the output.
* [kf] and [ki] give information to know there the specification comes from
* in order to build the names for the implicit functions.
* [termination_kind] is used to filter [\result] and [\exit_status] when needed.
*)
let bhv_from_hyps kf ki bhv l_froms termination_kind =
  let add_assign (n, acc) ((output, inputs) as from) =
    let acc = 
      match build_post_output termination_kind output with
        | None -> acc
        | Some output ->
            let inputs = match inputs with
              | FromAny -> None 
              | From inputs ->
                let mk_input x = build_elem ~addrlab:Logic_const.old_label 
                                   ~mlab:Logic_const.old_label x.it_content
                in
                let inputs = List.map mk_input inputs in
                  Some inputs
            in
            let fi_name = mk_bhv_implicit_fun_name kf ki bhv n in
            let p_eq = build_fimpl_eq fi_name output inputs in
              (from, p_eq)::acc
    in n+1, acc
  in snd (List.fold_left add_assign (1, []) l_froms)

(** For each behavior of the specification, and for each \from in the behavior,
* return a predicate which is [assumes => out_i = implicit_fun_i (inputs)].
* If the assigns information is missing from a behavior, try to use
* the whole assigns information of the spec.
* @raise NoFromForBhv if we don't manage to compute the assigns information.
* See [bhv_from_hyps] above.
* *)
let post_of_spec_assigns kf ki spec termination_kind =
  let add_behav (compl, acc) bhv =
    match bhv.b_assigns with
      | WritesAny -> (* skip *) compl, acc
      | Writes l ->
          (* post for behavior is [\old(assumes) => out = f(in)]*)
          let assumes = Ast_info.behavior_assumes bhv in
          let compl = compl || Logic_utils.is_trivially_true assumes in
          let assumes = Logic_const.pold assumes in
          let l = bhv_from_hyps kf ki bhv l termination_kind in
          let add_assume acc (from, p) =
            let p = Logic_const.pimplies (assumes, p) in
              (bhv, from, p)::acc
          in let acc = List.fold_left add_assume acc l in
            (compl, acc)
  in 
  let compl = spec.spec_complete_behaviors <> [] in (* TODO: add dpds ? *)
  let compl, acc = List.fold_left add_behav (compl, []) spec.spec_behavior in
    if compl then acc
    else (* some assigns information is missing: try to complete *)
      match WpStrategy.assigns_upper_bound spec with
        | None -> raise NoFromForBhv
        | Some (b, l) -> 
            let l = bhv_from_hyps kf ki b l termination_kind in
              List.fold_left (fun acc (from, p) -> (b, from, p)::acc) acc l

(** Build the from hyp for the loop assigns *)
let inv_of_loop_from s n (output, inputs) =
  let output = build_elem ~addrlab:Logic_const.here_label 
                 ~mlab:Logic_const.here_label output.it_content
  in
  let pre_loop_lab = Clabels.mk_logic_label s in
  let inputs =match inputs with
    | FromAny -> None
    | From inputs ->
        let mk_input x = build_elem ~addrlab:Logic_const.here_label
                           ~mlab:pre_loop_lab x.it_content
        in 
        let inputs = List.map mk_input inputs in
          Some inputs
  in
  let fi_name = mk_loop_implicit_fun_name s n in
  let p_eq = build_fimpl_eq fi_name output inputs in
    p_eq

(** Build [ xi = Init (i) /\ ...] forall inputs part of the assigns property. *)
let mk_assign_pre kf ki bhv nth inputs =
  let get_init lv n =
    let linfo = get_init_linfo kf ki bhv nth n lv.term_type in
      Logic_const.term (Tapp (linfo, [], [(*Logic_const.tinteger n*)])) lv.term_type
  in
  let add_in (n, acc) input =
    let lv = input.it_content in
    let _name = lv.term_name in (* TODO process name *)
    let init = get_init lv n in
    let pre = Logic_const.prel (Req, lv, init) in
    n+1, pre::acc
  in
  let _, pres = List.fold_left add_in (1, []) inputs in
  Logic_const.new_predicate (Logic_const.pands pres)

(* -------------------------------------------------------------------------- *)
(** {2 Build Strategy} *)
(* -------------------------------------------------------------------------- *)

let annot_for_asked_bhv b_list asked_bhv =
  b_list = [] || List.exists (fun x -> x = asked_bhv) b_list

let get_loop_assigns_for_froms asked_bhv s =
  let do_annot a acc =
    let ca = match a with User ca | AI (_, ca) -> ca in
    match ca.annot_content with
      | AAssigns (b_list, Writes a) when annot_for_asked_bhv b_list asked_bhv ->
          Some (ca,a)
      | _ -> acc
  in Annotations.single_fold_stmt do_annot s None

let add_loop_assigns_hyp kf asked_bhv s acc =
  let asgn_opt = get_loop_assigns_for_froms asked_bhv s in
  let acc = WpStrategy.add_loop_assigns_hyp acc kf s asgn_opt in
  match asgn_opt with
    | None -> raise (NoFromForLoop s)
    | Some (ca, assigns) ->
        let add_assign (n, acc) from =
          let inv = 
            try inv_of_loop_from s n from 
            with NoFromForBhv -> raise (NoFromForLoop s)
          in
          let id = WpPropId.mk_loop_from_id kf s ca from in
          let labels = NormAtLabels.labels_loop_inv s in
          let acc = WpStrategy.add_prop acc WpStrategy.Ahyp labels id inv in
            n+1, acc
        in
        let _, acc = List.fold_left add_assign (1, acc) assigns in
            acc

let add_stmt_spec_assigns_hyp (p_acc, e_acc) kf s l_post spec =
  let p_acc = 
    WpStrategy.add_stmt_spec_assigns_hyp p_acc kf s l_post spec
  in (* TODO add_stmt_spec_assigns_hyp in e_acc but crach at the moment... *)
    (p_acc, e_acc)

let add_call_assigns_hyp (p_acc, e_acc) kf_caller s l_post spec =
  let p_acc = 
    WpStrategy.add_call_assigns_hyp p_acc kf_caller s l_post (Some spec)
  in (* TODO add_call_assigns_hyp in e_acc but crach at the moment... *)
    (p_acc, e_acc)

(** @raise NoFromForBhv is the assigns information is missing. *)
let add_spec_annots kf s l_post spec (b_acc, (p_acc, e_acc)) =
  let kind = WpStrategy.Aboth false in
  let b_acc = WpStrategy.add_prop_stmt_spec_pre b_acc kind kf s spec in

  let add_from acc (bhv, from, p) =
    let id = 
      WpPropId.mk_bhv_from_id kf (Kstmt s) bhv from 
    in (* TODO use tk in id*)
    let labels = NormAtLabels.labels_stmt_post s l_post in
      WpStrategy.add_prop acc WpStrategy.Ahyp  labels id p
  in
  let p_froms = post_of_spec_assigns kf (Kstmt s) spec Normal in
  let p_acc = List.fold_left add_from p_acc p_froms in
  let e_froms = post_of_spec_assigns kf (Kstmt s) spec Exits in
  let e_acc = List.fold_left add_from e_acc e_froms in

  let a_acc = add_stmt_spec_assigns_hyp (p_acc, e_acc) kf s l_post spec in
    (b_acc, a_acc)

let get_stmt_hyp kf asked_bhv s l_post =
  let do_annot a acc =
    let ca = Annotations.get_code_annotation a in
      match ca.annot_content with
        | AStmtSpec (b_list, spec) when annot_for_asked_bhv b_list asked_bhv ->
            (try add_spec_annots kf s l_post spec acc
             with NoFromForBhv -> (* TODO: not sure this is correct!*) acc)
        | _ -> (* ignore other annotations *) acc
  in
  let before_acc, after_acc, exits_acc =
    WpStrategy.empty_acc, WpStrategy.empty_acc, WpStrategy.empty_acc in
  let acc = before_acc, (after_acc, exits_acc) in
    Annotations.single_fold_stmt do_annot s acc

(** Collect the \from hypotheses of the function spectication.
* TODO: maybe we should also take the [ensures] properties ? 
* @raise NoFromForBhv is the assigns information is missing.
**)
let get_called_post kf termination_kind =
  let spec = Kernel_function.get_spec kf in
  Wp_parameters.debug ~dkey "[get_called_post] '%s' for %a@."
    (WpPropId.string_of_termination_kind termination_kind)
    Kernel_function.pretty  kf;
  let posts = post_of_spec_assigns kf Kglobal spec termination_kind in
  let mk_prop acc (bhv, from, post) =
    let id = WpPropId.mk_bhv_from_id kf Kglobal bhv from in
    let labels = NormAtLabels.labels_fct_post in
      WpStrategy.add_prop acc WpStrategy.AcallHyp labels id post
  in List.fold_left mk_prop WpStrategy.empty_acc posts

let get_call_hyp kf_caller s l_post fct =
  match WpStrategy.get_called_kf fct with
    | Some kf ->
        let spec = Kernel_function.get_spec kf in
        let before_annots = WpStrategy.empty_acc in
        let post_annots = 
          try get_called_post kf Normal 
          with NoFromForBhv -> raise (NoFromForCall s)
        in
        let exits_annots = 
          try get_called_post kf Exits 
          with NoFromForBhv -> raise (NoFromForCall s)
        in
        let after_annots = post_annots, exits_annots in
        let after_annots = 
          add_call_assigns_hyp after_annots kf_caller s l_post spec
        in
          before_annots, after_annots
    | None ->
        Wp_parameters.warning
          "call through function pointer not implemented yet: \
                                   ignore called function properties.";
        raise (NoFromForCall s)

(** Collect all the annotations to be used to prove one \from property of 
 * the function behavior **)
let get_fct_bhv_from_annots cfg bhv nth assign =
  let kf = Cil2cfg.cfg_kf cfg in
  let asked_bhv = bhv.b_name in
  let annots = WpStrategy.create_tbl () in
  let add_post v tk = match mk_assign_post kf bhv nth tk assign with
    | None -> ()
    | Some post ->
        let edges = Cil2cfg.succ_e cfg v in
        let acc = WpStrategy.empty_acc in
          (* TODO: goal_to_select for only one from *)
        let kind = WpStrategy.Agoal in
        let labels = NormAtLabels.labels_fct_assigns in
        let id = WpPropId.mk_fct_from_id kf bhv tk assign in 
        let post = Logic_const.pred_of_id_pred post in
        let acc = WpStrategy.add_prop acc kind labels id post in
          WpStrategy.add_on_edges annots acc edges
  in
  let add_stmt_annots v s =
    let l_post = Cil2cfg.get_post_logic_label cfg v in
    let stmt_annots = get_stmt_hyp kf asked_bhv s l_post in
      WpStrategy.add_node_annots annots cfg v stmt_annots
  in
  let get_node_annot v =
    match Cil2cfg.node_type v with
    | Cil2cfg.VfctIn -> ()
        (* Don't put the precondition here because we don't want to build
        * (pre => post) => (pre' => post') but rather
        * (pre /\ pre' /\ post => post') so we have to process the pre latter
        * (see SKfroms) *)

    | Cil2cfg.VfctOut  -> add_post v Normal
    | Cil2cfg.Vexit -> add_post v Exits

    | Cil2cfg.VblkIn (Cil2cfg.Bstmt s, _)
    | Cil2cfg.Vstmt s
    | Cil2cfg.Vswitch (s,_) | Cil2cfg.Vtest (true, s, _)
      -> add_stmt_annots v s

    | Cil2cfg.Vcall (s,_,fct,_) ->
        let l_post = Cil2cfg.get_post_logic_label cfg v in
        let call_annots = get_call_hyp kf s l_post fct in
          WpStrategy.add_node_annots annots cfg v call_annots

    | Cil2cfg.Vloop (_, s) ->
        add_stmt_annots v s;
        let loop_core =
          add_loop_assigns_hyp kf asked_bhv s WpStrategy.empty_acc in
        let edges_to_head = Cil2cfg.succ_e cfg v in
          WpStrategy.add_on_edges annots loop_core edges_to_head
    | _ -> ()
  in
  let _ = Cil2cfg.iter_nodes get_node_annot cfg in
    annots

let mk_strategy_for_fct_from cfg bhv pre ((out,from) as assign) =
  let n = out.it_id in (* TODO: chose a better num with a user meaning ? *)
  let kf = Cil2cfg.cfg_kf cfg in
  let get_pre () =
    let pre_init = match from with 
      | FromAny -> Wp_parameters.fatal "no from to prove"
      | From inputs -> mk_assign_pre kf Kglobal bhv n inputs 
    in
    let assumes = None in (* assumes are already hyp of the strategy. *)
      WpStrategy.add_prop_fct_pre pre WpStrategy.Ahyp kf bhv ~assumes pre_init
  in
  let annots = get_fct_bhv_from_annots cfg bhv n assign in
  let _ = WpStrategy.add_all_axioms annots in
  let desc =
    Pretty_utils.sfprintf "'%a', %d from property of '%s' behavior"
      Kernel_function.pretty kf n bhv.b_name
  in
  let kind = WpStrategy.SKfroms { 
    WpStrategy.get_pre = get_pre;
    WpStrategy.more_vars = Vars.get_and_init ();
  } in
  let new_loops = Wp_parameters.Invariants.get() in
  let bname = if Cil.is_default_behavior bhv then "default" else bhv.b_name in
  let bname = (bname^"_assign_"^(string_of_int n)) in
    WpStrategy.mk_strategy desc cfg (Some bname)
      new_loops kind annots

let pp_err fmt e = 
  let no_from = "no \\from information" in
  let pp_stmt_loc fmt s =
    Format.fprintf fmt "@[%a@]" Cil.d_loc (Cil_datatype.Stmt.loc s)
  in
  match e with
    | NoFromForCall s -> 
        Format.fprintf fmt "%s for call at @[%a@]" no_from pp_stmt_loc s
    | NoFromForLoop s -> 
        Format.fprintf fmt "%s for loop at @[%a@]" no_from pp_stmt_loc s
    | _ -> raise e

let get_bhv_pre kf bhv =
  let add_bhv_pre_hyp b acc = (* add both requires and assumes as precond *)
    let kind = WpStrategy.Ahyp in
      WpStrategy.add_prop_fct_bhv_pre acc kind kf b ~impl_assumes:false
  in
  let pre = add_bhv_pre_hyp bhv (WpStrategy.empty_acc) in
  let pre = (* also add the default behavior precond *)
    if (Cil.is_default_behavior bhv) then pre
    else match Cil.find_default_behavior (Kernel_function.get_spec kf) with 
      | None -> pre
      | Some bdef -> add_bhv_pre_hyp bdef pre
  in
    pre

let get_strategy_for_from id_from =
  let kf, ki, behavior_or_loop, from = id_from in
    match ki, behavior_or_loop with
      | Kglobal, Property.Id_behavior bhv ->
          let cfg = Cil2cfg.get kf in
          let pre = get_bhv_pre kf bhv in
            mk_strategy_for_fct_from cfg bhv pre from
      | _ -> Wp_parameters.not_yet_implemented "local \\from property check"

(** Build strategies to prove the [from] properties of the function.
* At the moment, only the function behaviors are handled,
* but the strategies make use of the [from] properties of stmt spec,
* loops and called functions. *)
let get_strategies_for_froms kf =
  if not (Kernel_function.is_definition kf) then
    begin
      Wp_parameters.warning 
      "Function %a has no body : cannot prove its \\from properties (skip)"
      Kernel_function.pretty kf;
      []
    end
  else
    let stmt_bhvs = Kernel_function.internal_function_behaviors kf in
     if stmt_bhvs <> [] then
      Wp_parameters.warning 
        "Not implemented: prove local \\from properties (skip)";
     (* TODO: \\from in loops. *)
    let spec =  Kernel_function.get_spec kf in
    let cfg = Cil2cfg.get kf in
    let add_bhv acc bhv =
      let pre = get_bhv_pre kf bhv in
      let add_assign_strategy acc (b,f) =
        match f with
          | FromAny -> acc
          | From _l ->
              let stg = mk_strategy_for_fct_from cfg bhv pre (b,f) in
                stg::acc
      in
        match bhv.b_assigns with
          | WritesAny -> acc
          | Writes l ->
              try List.fold_left add_assign_strategy acc l 
              with e ->
                Wp_parameters.warning 
                  "cannot check \\from properties of '%a':@,@[%a@]" 
                  Kernel_function.pretty kf pp_err e;
                acc
    in List.fold_left add_bhv [] spec.spec_behavior
