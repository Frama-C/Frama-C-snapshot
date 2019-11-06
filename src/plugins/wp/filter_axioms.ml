(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Why3
open Term
open Decl

let meta_remove_altergo =
  Theory.register_meta "remove_for_altergo"
    [Theory.MTprsymbol]
    ~desc:"Don't@ translate@ this@ lemma@ for@ altergo."

let meta_remove_why3 =
  Theory.register_meta "remove_for_why3"
    [Theory.MTprsymbol]
    ~desc:"Don't@ translate@ this@ lemma@ for@ why3."

let meta_remove_ =
  Theory.register_meta "remove_for_"
    [Theory.MTprsymbol]
    ~desc:"Don't@ translate@ this@ lemma@ for@ why3 and altergo."


let elim_abstract remove_pr d = match d.d_node with
  | Dprop (Paxiom,pr,_) when Spr.mem pr remove_pr ->
      (* Format.eprintf "Remove %a@." Pretty.print_pr pr; *)
      []
  | Dprop (Paxiom,_,_) ->
      (* Format.eprintf "Not Remove %a@." Pretty.print_pr pr; *)
      [d]
  | _ ->
      (* Format.eprintf "Not Seen %a@." Pretty.print_decl d; *)
      [d]

let remove_prop meta =
  Trans.on_tagged_pr meta
    (fun remove_pr ->
       Trans.on_tagged_pr meta_remove_
         (fun remove_pr2 ->
            Trans.decl (elim_abstract (Spr.union remove_pr remove_pr2)) None))

let remove_for_altergo = remove_prop meta_remove_altergo
let remove_for_why3 = remove_prop meta_remove_why3


let () =
  Trans.register_transform "remove_for_altergo"
    remove_for_altergo
    ~desc:"Remove@ tagged@ proposition@ with \"remove_for_altergo\"@ and \
           \"remove_for_\"@ metas."

let () =
  Trans.register_transform "remove_for_why3"
    remove_for_why3
    ~desc:"Remove@ tagged@ proposition@ with \"remove_for_why3\"@ and \
           \"remove_for_\" metas."


(** inlining *)

let meta_inline_in =
  Theory.register_meta "inline_in"
    [Theory.MTlsymbol;Theory.MTprsymbol;]
    ~desc:"Inline@ the@ symbol@ in@ the@ proposition."

let t_unfold defs fs tl ty =
  match Mls.find_opt fs defs with
  | None ->
      assert false (** absurd: it is in mpr so it is in sls so added in defs *)
  | Some (vl,e) ->
      let add (mt,mv) x y = Ty.ty_match mt x.vs_ty (t_type y), Mvs.add x y mv in
      let (mt,mv) = List.fold_left2 add (Ty.Mtv.empty, Mvs.empty) vl tl in
      let mt = Ty.oty_match mt e.t_ty ty in
      t_ty_subst mt mv e

(* inline every symbol *)

let rec t_replace_all defs s t =
  let t = t_map (t_replace_all defs s) t in
  match t.t_node with
  | Tapp (fs,tl) when Sls.mem fs s ->
      t_attr_copy t (t_unfold defs fs tl t.t_ty)
  | _ -> t

let fold mpr sls d (defs, task) =
  (** replace *)
  let d = match d.d_node with
    | Dprop (k,pr,f) ->
        let s = Mpr.find_def Sls.empty pr mpr in
        if Sls.is_empty s then d
        else create_prop_decl k pr (t_replace_all defs s f)
    | _ -> d
  in
  (** add to defs if needed *)
  match d.d_node with
  | Dlogic [ls,ld] when Sls.mem ls sls ->
      let vl,e = open_ls_defn ld in
      Mls.add ls (vl,e) defs, Task.add_decl task d
  | _ ->
      defs, Task.add_decl task d

let fold mpr sls task_hd (defs, task) =
  match task_hd.Task.task_decl.Theory.td_node with
  | Theory.Decl d ->
      fold mpr sls d (defs, task)
  | _ ->
      defs, Task.add_tdecl task task_hd.Task.task_decl

let trans =
  let add (mpr,sls) = function
    | [Theory.MAls ls; Theory.MApr pr] ->
        Mpr.change (function None -> Some (Sls.singleton ls)
                           | Some s -> Some (Sls.add ls s)) pr mpr,
        Sls.add ls sls
    | _ -> assert false
  in
  Trans.on_meta meta_inline_in (fun l ->
      let mpr, sls = List.fold_left add (Mpr.empty,Sls.empty) l in
      Trans.fold_map (fold mpr sls) Mls.empty None)


let () =
  Trans.register_transform "inline_in"
    trans
    ~desc:"Inline@ the@ symbol@ in@ the@ proposition(meta@ of@ the@ same@ name)"

(*** eliminate function *)
let meta_def_into_axiom =
  Theory.register_meta "def_into_axiom"
    [Theory.MTlsymbol]
    ~desc:"Turn the marked function into an axiom"

let add_ld which (ls,ld) (abst,defn,axl) =
  if which ls then
    let vl,e = open_ls_defn ld in
    let nm = ls.ls_name.Ident.id_string ^ "_def" in
    let pr = create_prsymbol (Ident.id_derive nm ls.ls_name) in
    let hd = t_app ls (List.map t_var vl) e.t_ty in
    let e = TermTF.t_selecti Term.t_equ_simp Term.t_iff_simp hd e in
    let ax = t_forall_close vl [[hd]] e in
    let ax = create_prop_decl Paxiom pr ax in
    let ld = create_param_decl ls in
    ld :: abst, defn, ax :: axl
  else
    abst, (ls,ld) :: defn, axl

let elim_decl which l =
  let abst,defn,axl = List.fold_right (add_ld which) l ([],[],[]) in
  let defn = if defn = [] then [] else [create_logic_decl defn] in
  abst @ defn @ axl

let elim which d = match d.d_node with
  | Dlogic l -> elim_decl which l
  | _ -> [d]

let def_into_axiom =
  Trans.on_tagged_ls meta_def_into_axiom (fun sls ->
      Trans.decl (elim (fun ls -> Term.Sls.mem ls sls)) None)

let () =
  Trans.register_transform "def_into_axiom"
    def_into_axiom
    ~desc:"Turn the marked function into an axiom"
