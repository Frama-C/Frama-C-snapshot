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
open Conditions
open Tactical

(** Useful **)

let str_case = "Case"
let sub_cases phi = function
  | []   -> List.map (fun t -> phi str_case (F.p_bool t))
  | [descr]     -> List.map (fun t -> phi descr (F.p_bool t))
  | infos -> List.map2 (fun info t -> phi info (F.p_bool t)) infos

(* split into n sequents: [hyps] |- [subcases]_i *)
let f_replace_goal infos subcases (hyps,_) =
  let sub_case descr p  = descr, (hyps,p) in
  sub_cases sub_case infos subcases

(* split into n sequents: [subcases]_i, hyps/[hyp] |- goal
   in fact, [hyp] is replaced by [subcases]_i *)
let f_replace_hyp hyp infos subcases sequent =
  let sub_case descr p =
    descr, Conditions.(replace ~at:hyp.id (update_cond hyp ~descr (When p))
                         sequent)
  in sub_cases sub_case infos subcases

(* -------------------------------------------------------------------------- *)
(* --- FNC (goal) and FND (hyp) tacticals                                 --- *)
(* -------------------------------------------------------------------------- *)

let nf_conj_args e = match F.repr e with
  | Qed.Logic.And xs -> xs
  | _ -> [e]
let nf_disj_args e = match F.repr e with
  | Qed.Logic.Or xs -> xs
  | _ -> [e]

let f_nf_goal e   ~depth = f_replace_goal  ["CNF"] (nf_conj_args (WpTac.e_cnf ~depth e))
let f_nf_hyp  s e ~depth = f_replace_hyp s ["DNF"] (nf_disj_args (WpTac.e_dnf ~depth e))

let match_selection = function
  | Clause(Goal p) ->
      let e = Lang.F.e_prop p in
      if WpTac.is_cnf e then None
      else Some (true, e, f_nf_goal e)
  | Clause(Step s) ->
      begin
        match s.condition with
        | (Type p | Have p | When p | Core p | Init p) ->
            let e = Lang.F.e_prop p in
            if WpTac.is_dnf e then None
            else Some (false, e, f_nf_hyp s e)
        | _ -> None
      end
  | Inside(_,_) | Compose _ | Empty -> None

class normal_form =
  object
    inherit Tactical.make ~id:"Wp.normal_form"
        ~title:"Intuition"
        ~descr:"Decompose with Conjunctive/Disjunctive Normal Form"
        ~params:[]

    method select feedback (s : Tactical.selection) =
      match match_selection s with
      | Some (pol,_,continuation) ->
          feedback#set_title
            (if pol then "Intuition (CNF)" else "Intuition (DNF)") ;
          let depth = (-1) in
          Applicable (continuation ~depth)
      | _ -> Not_applicable

  end

let tactical = Tactical.export (new normal_form)
let strategy = Strategy.make tactical

(* -------------------------------------------------------------------------- *)
