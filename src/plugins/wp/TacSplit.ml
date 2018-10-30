(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Lang

module PartitionsQQ : sig
  val destructs_qq :
    Lang.F.pred ->
    Qed.Logic.binder -> tau:Lang.F.QED.tau -> phi:Lang.F.QED.bind ->
    Lang.F.Vars.t * Lang.F.QED.term

  val get : vars:Lang.F.Vars.t -> Lang.F.term list ->
    int * Lang.F.Tset.t list
end
= struct
  let dkey = Wp_parameters.register_category "tac_split_quantifiers" (* debugging key *)
  let debug fmt = Wp_parameters.debug ~dkey fmt

  let destructs_qq p qq ~tau ~phi =
    let pool =
      let quant = F.e_prop p in
      let vars = Lang.F.vars quant in
      Lang.new_pool ~vars ()
    in
    let rec destructs_qq vars ~tau ~phi =
      let open Qed.Logic in
      let x = F.fresh pool tau in
      let vars = F.Vars.add x vars in
      let p = F.QED.lc_open x phi in
      match F.repr p with
      | Bind (binder, tau, phi) when binder = qq ->
          destructs_qq vars ~tau ~phi
      | _ -> vars, p
    in destructs_qq F.Vars.empty ~tau ~phi

  type kind_t = Root of F.Tset.t | Node of node_t
  and  node_t = { var: F.Var.t;
                  mutable rank: int;
                  mutable kind: kind_t
                }
  type var2node_t = node_t F.Vmap.t

  let is_root root = match root.kind with
    | Node _ -> false
    | Root _ -> true

  let find (map:var2node_t) var =
    let find_root var root = function
      | None -> (* adds an empty root partition for that [var] *)
          root := Some { var; rank=0; kind=(Root F.Tset.empty) };
          debug ". . find(%a)= %a (inserted)@." Lang.F.pp_var var Lang.F.pp_var var ;
          !root
      | (Some { kind=(Root _); var=debug_var }) as old ->
          debug ". . find(%a)= %a@." Lang.F.pp_var var Lang.F.pp_var debug_var ;
          root := old ;
          old
      | Some ({ kind=(Node _) } as node) ->
          debug ". . find(%a)=" Lang.F.pp_var var ;
          let rec find_aux node =
            debug " %a" Lang.F.pp_var node.var ;
            match node.kind with
            | Node y ->
                let r = find_aux y in
                node.kind <- Node r ; r
            | Root _ -> node
          in
          root := Some (find_aux node);
          debug "@." ;
          !root
    in
    let root = ref None in
    let map = F.Vmap.change find_root var root map in
    let root = match !root with
      | None -> assert false
      | Some root -> root
    in
    assert (is_root root);
    map,root

  let union ((map:var2node_t),rootX) varY =
    debug "< union(%a,%a)@." Lang.F.pp_var rootX.var Lang.F.pp_var varY ;
    assert (is_root rootX);
    let map,rootY = find map varY in
    assert ((rootX == rootY) = (0 = F.Var.compare rootX.var rootY.var)) ;
    if rootX == rootY then begin
      debug "> union(%a,%a)=%a@." Lang.F.pp_var rootX.var Lang.F.pp_var varY Lang.F.pp_var rootX.var ;
      map,rootX
    end
    else
      let terms root = match root.kind with
        | Node _ -> assert false
        | Root terms -> terms
      in
      let root, node =
        if rootX.rank < rootY.rank then rootY, rootX
        else (if rootX.rank = rootY.rank then rootX.rank <- 1+rootX.rank;
              rootX,rootY)
      in
      root.kind <- Root (F.Tset.union (terms rootX) (terms rootY));
      node.kind <- Node root ;
      assert (is_root root);
      debug "> union(%a,%a)=%a@." Lang.F.pp_var rootX.var Lang.F.pp_var varY Lang.F.pp_var root.var ;
      map,root

  let partitions ~vars es =
    let debug_term_nth = ref 0 in
    let partitions (set,map) term =
      incr debug_term_nth ;
      let vars = F.Vars.inter vars (Lang.F.vars term) in
      match F.Vars.elements vars with
      | [] ->
          debug "- term #%d: no vars -> %a@." !debug_term_nth Lang.F.pp_term term;
          (F.Tset.add term set), map (* closed term partition *)
      | var::others -> (* term partition bound to variables *)
          debug "- term #%d: nb vars=%d -> %a@." !debug_term_nth (1+List.length others) Lang.F.pp_term term;
          let map,root = List.fold_left union (find map var) others in
          (* adds the current term to the partition *)
          (match root.kind with
           | Node _ -> assert false
           | Root terms ->
               root.kind <- Root (F.Tset.add term terms));
          set,map
    in
    debug "------------@.Partitions(vars #%d,terms #%d)@." (F.Vars.cardinal vars) (List.length es);
    List.fold_left partitions (F.Tset.empty,F.Vmap.empty) es

  let extract (set,map) =
    debug "------------@.Extract@.no vars: nb terms = %d@." (F.Tset.cardinal set);
    let acc = if F.Tset.is_empty set then 0, [] else 1, [set] in
    let extract var node ((nb_parts,parts) as acc) =
      if 0 != F.Var.compare node.var var then acc
      else match node.kind with
        | Root part -> assert (not (F.Tset.is_empty part));
            debug "var %a, nb terms = %d@." Lang.F.pp_var var (F.Tset.cardinal part);
            (nb_parts+1),part::parts
        | Node _ -> acc
    in
    let ((nb_part,_) as r) = F.Vmap.fold extract map acc in
    assert (nb_part > 0);
    r

  let get ~vars es =
    extract (partitions ~vars es)
end

open Lang.F
open Conditions
open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Split Tactical                                                     --- *)
(* -------------------------------------------------------------------------- *)
let bind qq ~vars p = F.Vars.fold (F.e_bind qq) vars p

class split =
  object
    inherit Tactical.make
        ~id:"Wp.split"
        ~title:"Split"
        ~descr:"Decompose Logical Connectives and Conditionals"
        ~params:[]

    method select feedback (s : Tactical.selection) =
      match s with
      | Empty | Compose _ -> Not_applicable
      | Inside(_,e) ->
          begin
            let split_cmp title x y =
              feedback#set_title title ;
              feedback#set_descr
                "Decompose into three comparisons (lt, eq, gt)" ;
              let cases = [
                "Lt",F.p_bool (e_lt x y);
                "Eq",F.p_bool (e_eq x y);
                "Gt",F.p_bool (e_lt y x);
              ] in
              let at = Tactical.at s in
              Applicable (Tactical.insert ?at cases)
            in
            let open Qed.Logic in
            match Lang.F.repr e with
            | Leq(x,y) -> split_cmp "Split (comp.)" x y
            | Lt(x,y)  -> split_cmp "Split (comp.)" x y
            | Eq(x,y)  when not (is_prop x || is_prop y) ->
                split_cmp "Split (eq.)" x y
            | Neq(x,y) when not (is_prop x || is_prop y) ->
                split_cmp "Split (neq.)" x y
            | _ when F.is_prop e->
                feedback#set_title "Split (true,false)" ;
                feedback#set_descr "Decompose between True and False values" ;
                let cases = ["True",F.p_bool e;"False",F.p_not (F.p_bool e)] in
                let at = Tactical.at s in
                Applicable (Tactical.insert ?at cases)
            | _ -> Not_applicable
          end
      | Clause(Goal p) ->
          begin
            let open Qed.Logic in
            match Lang.F.e_expr p with
            | Bind (Exists,tau,phi) -> begin
                let vars,q = PartitionsQQ.destructs_qq p Exists ~tau ~phi in
                match Lang.F.repr q with
                | If (c,p,q) ->
                    if F.Vars.is_empty (F.Vars.inter (F.vars c) vars) then
                      begin
                        (* unbound condition: proceed by case *)
                        feedback#set_title "Split (exists if)" ;
                        feedback#set_descr
                          "Split unbound Condition into Branches" ;
                        let p = F.e_imply [c] (bind Exists ~vars p) in
                        let q = F.e_imply [e_not c] (bind Exists ~vars q) in
                        Applicable (Tactical.split [
                            "Then" , F.p_bool p ;
                            "Else" , F.p_bool q ;
                          ])
                      end
                    else
                      begin
                        feedback#set_title "Split (rewrite exists if)" ;
                        feedback#set_descr
                          "Rewrite the Conditional in a Disjunction \
                           and Distribute the Quantifier under" ;
                        let p = bind Exists ~vars (F.e_and [c;p]) in
                        let q = bind Exists ~vars (F.e_and [(e_not c); q]) in
                        let cases = [ "Split" , F.p_bool (F.e_or [p;q]) ] in
                        Applicable (Tactical.split cases)
                      end
                | Or es ->
                    feedback#set_title "Split (exists or)" ;
                    feedback#set_descr
                      "Distributes the Quantifier under the Disjunction" ;
                    let p = F.e_or (List.map (bind Exists ~vars) es) in
                    let cases = [ "Split" , F.p_bool p ] in
                    Applicable (Tactical.split cases)
                | Imply (es, p) ->
                    feedback#set_title "Split (exists imply)" ;
                    feedback#set_descr
                      "Distributes the Quantifier under the Imply" ;
                    let p = F.e_imply (List.map (bind Forall ~vars) es)
                        (bind Exists ~vars p) in
                    let cases = [ "Split" , F.p_bool p ] in
                    Applicable (Tactical.split cases)
                | And es ->
                    let nb_parts,parts = PartitionsQQ.get vars es in
                    if nb_parts=1 then Not_applicable
                    else begin
                      feedback#set_title "Split (exists and)" ;
                      feedback#set_descr
                        "Decompose the Quantifier into %d Blocks" nb_parts ;
                      let bind es =
                        bind Exists ~vars (F.e_and (F.Tset.elements es)) in
                      let goal i n es =
                        Printf.sprintf "Goal %d/%d" i n , F.p_bool (bind es) in
                      Applicable (Tactical.split (Tactical.mapi goal parts))
                    end
                | _ -> Not_applicable
              end
            | And es ->
                let n = List.length es in
                feedback#set_title "Split (and)" ;
                feedback#set_descr
                  "Decompose between the %d parts of the Conjunction" n ;
                let goal i n e = Printf.sprintf "Goal %d/%d" i n , F.p_bool e in
                Applicable (Tactical.split (Tactical.mapi goal es))
            | Eq(x,y) when (F.is_prop x) && (F.is_prop y) ->
                feedback#set_title "Split (iff)" ;
                feedback#set_descr "Turn Equivalence into Implications" ;
                let p = F.p_bool (F.e_imply [x] y) in
                let q = F.p_bool (F.e_imply [y] x) in
                let cases = [ "Necessity" , p ; "Sufficiency" , q ] in
                Applicable (Tactical.split cases)
            | Neq(x,y) when (F.is_prop x) && (F.is_prop y) ->
                feedback#set_title "Split (xor)" ;
                feedback#set_descr "Turn Dis-Equivalence into Implications" ;
                let p = F.p_bool (F.e_imply [x] (e_not y)) in
                let q = F.p_bool (F.e_imply [y] (e_not x)) in
                let cases = [ "Necessity" , p ; "Sufficiency" , q ] in
                Applicable (Tactical.split cases)
            | If(c,p,q) -> (* Split + intro *)
                feedback#set_title "Split (if)" ;
                feedback#set_descr "Decompose Conditional into Branches" ;
                let p = F.p_bool (F.e_imply [c] p) in
                let q = F.p_bool (F.e_imply [e_not c] q) in
                let cases = [ "Then" , p ; "Else" , q ] in
                Applicable (Tactical.split cases)
            | _ ->
                Not_applicable
          end
      | Clause(Step step) ->
          begin
            match step.condition with
            | State _ -> Not_applicable
            | Branch(p,_,_) ->
                feedback#set_title "Split (branch)" ;
                feedback#set_descr "Decompose Conditional into Branches" ;
                let cases = [ "Then" , p ; "Else" , p_not p ] in
                Applicable (Tactical.insert ~at:step.id cases)
            | Either seqs ->
                let n = List.length seqs in
                feedback#set_title "Split (switch)" ;
                feedback#set_descr "Decompose each %d Cases" n ;
                let either i n s = Printf.sprintf "Case %d/%d" i n , Either [s] in
                let cases = Tactical.mapi either seqs in
                Applicable (Tactical.replace ~at:step.id cases)
            | (Type p | Have p | When p | Core p | Init p) ->
                begin
                  let open Qed.Logic in
                  match F.e_expr p with
                  | Bind (Forall,tau,phi) -> begin
                      let vars,q = PartitionsQQ.destructs_qq p Forall ~tau ~phi in
                      match Lang.F.repr q with
                      | If (c,p,q) ->
                          if F.Vars.is_empty (F.Vars.inter (F.vars c) vars) then
                            begin (* unbound condition: so, the If is considered as a disjunction *)
                              feedback#set_title "Split (forall if)" ;
                              feedback#set_descr "Decompose unbound conditional into Branches" ;
                              let p = F.p_bool (F.e_and [c; (bind Exists ~vars p)]) in
                              let q = F.p_bool (F.e_and [(e_not c); (bind Exists ~vars q)]) in
                              let cases = [ "Then" , When p ; "Else" , When q ] in
                              Applicable (Tactical.replace ~at:step.id cases)
                            end
                          else
                            begin
                              feedback#set_title "Split (rewrite forall if)" ;
                              feedback#set_descr "Rewrite the Conditional in a Conjunction and Distributes the Quantifier under the Conjunction" ;
                              let p = bind Exists ~vars (F.e_imply [c] p) in
                              let q = bind Exists ~vars (F.e_imply [e_not c] q) in
                              let cases = [ "Split (rewrite exists if)" , When (F.p_bool (F.e_and [p;q])) ] in
                              Applicable (Tactical.replace ~at:step.id cases)
                            end
                      | And es ->
                          feedback#set_title "Split (forall and)" ;
                          feedback#set_descr "Distributes the Quantifier under the Conjunction" ;
                          let p = F.p_bool (F.e_and (List.map (bind Forall ~vars) es)) in
                          let cases = [ "Split (distrib forall and)" , When p ] in
                          Applicable (Tactical.replace ~at:step.id cases)
                      | Or es ->
                          let nb_parts,parts = PartitionsQQ.get vars es in
                          if nb_parts=1 then Not_applicable
                          else begin
                            feedback#set_title "Split (forall or)" ;
                            feedback#set_descr "Decompose the Quantifier between %d parts of the Disjunction" nb_parts ;
                            let bind es = bind Forall ~vars (F.e_or (F.Tset.elements es)) in
                            let goal i n es = Printf.sprintf "Goal %d/%d" i n , When (F.p_bool (bind es)) in
                            let cases = Tactical.mapi goal parts in
                            Applicable (Tactical.replace ~at:step.id cases)
                          end
                      | _ -> Not_applicable
                    end
                  | Or xs ->
                      let n = List.length xs in
                      feedback#set_title "Split (or)" ;
                      feedback#set_descr "Distinguish the %d parts of the Disjunction" n ;
                      let hyp i n e = Printf.sprintf "Case %d/%d" i n , When (F.p_bool e) in
                      let cases = Tactical.mapi hyp xs in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | Eq(x,y) when (F.is_prop x)&&(F.is_prop y) ->
                      feedback#set_title "Split (iff)";
                      feedback#set_descr "Decompose Equivalence into both True/False" ;
                      let p = F.p_bool x in
                      let q = F.p_bool y in
                      let cases = [
                        "Both True" , When F.(p_and p q) ;
                        "Both False" , When F.(p_and (p_not p) (p_not q)) ;
                      ] in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | Neq(x,y) when (F.is_prop x)&&(F.is_prop y) ->
                      feedback#set_title "Split (xor)";
                      feedback#set_descr "Decompose Dis-Equivalence into alternated True/False" ;
                      let p = F.p_bool x in
                      let q = F.p_bool y in
                      let cases = [
                        "True/False" , When F.(p_and p (p_not q)) ;
                        "False/True" , When F.(p_and (p_not p) q) ;
                      ] in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | Neq(x,y) when not (is_prop x || is_prop y) ->
                      feedback#set_title "Split (<>)";
                      feedback#set_descr "Decompose into two comparisons (<, >)" ;
                      let cases = ["Lt", When F.(p_bool (e_lt x y));
                                   "Gt", When F.(p_bool (e_lt y x))] in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | Leq(x,y) when not (is_prop x || is_prop y) ->
                      feedback#set_title "Split (<=)";
                      feedback#set_descr "Decompose into two comparisons (<, =)" ;
                      let cases = ["Lt", When F.(p_bool (e_lt x y));
                                   "Eq", When F.(p_bool (e_eq y x))] in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | If(c,p,q) ->
                      feedback#set_title "Split (if)" ;
                      feedback#set_descr "Split Conditional into Branches" ;
                      let p = F.p_bool (F.e_and [c;p]) in
                      let q = F.p_bool (F.e_and [e_not c;q]) in
                      let cases = [ "Then" , When p ; "Else" , When q ] in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | _ ->
                      Not_applicable
                end
          end

  end

let tactical = Tactical.export (new split)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
