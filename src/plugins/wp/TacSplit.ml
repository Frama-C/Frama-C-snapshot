(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
open Lang.F
open Conditions
open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Split Tactical                                                     --- *)
(* -------------------------------------------------------------------------- *)

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
              feedback#set_descr "Decompose into three comparisons (lt, eq, gt)" ;
              let cases = ["Lt",F.p_bool (e_lt x y);"Eq",F.p_bool (e_eq x y);"Gt",F.p_bool (e_lt y x)] in
              let at = Tactical.at s in
              Applicable (Tactical.insert ?at cases)
            in
            let open Qed.Logic in
            match Lang.F.repr e with
            | Leq(x,y) -> split_cmp "Split (comp.)" x y
            | Lt(x,y)  -> split_cmp "Split (comp.)" x y
            | Eq(x,y)  when not (is_prop x || is_prop y) -> split_cmp "Split (eq.)" x y
            | Neq(x,y) when not (is_prop x || is_prop y) -> split_cmp "Split (neq.)" x y
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
            | And es ->
                let n = List.length es in
                feedback#set_title "Split (and)" ;
                feedback#set_descr "Decompose between the %d parts of the Conjunction" n ;
                let goal i n e = Printf.sprintf "Goal %d/%d" i n , F.p_bool e in
                let cases = Tactical.mapi goal es in
                Applicable (Tactical.split cases)
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
                      let cases = ["Lt", When F.(p_bool (e_lt x y));"Gt", When F.(p_bool (e_lt y x))] in
                      Applicable (Tactical.replace ~at:step.id cases)
                  | Leq(x,y) when not (is_prop x || is_prop y) ->
                      feedback#set_title "Split (<=)";
                      feedback#set_descr "Decompose into two comparisons (<, =)" ;
                      let cases = ["Lt", When F.(p_bool (e_lt x y));"Eq", When F.(p_bool (e_eq y x))] in
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
let strategy = Strategy.make tactical

(* -------------------------------------------------------------------------- *)
