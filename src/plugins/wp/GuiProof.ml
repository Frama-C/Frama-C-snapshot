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

let rec rootchain node ns =
  match ProofEngine.parent node with
  | None -> node,ns
  | Some p -> rootchain p (p::ns)

let pp_status fmt node =
  match ProofEngine.state node with
  | `Opened -> Format.fprintf fmt "@{<red>opened@}"
  | `Proved | `Pending 0 -> Format.fprintf fmt "@{<green>proved@}"
  | `Pending 1 -> Format.fprintf fmt "@{<orange>pending@}"
  | `Pending n -> Format.fprintf fmt "@{<orange>pending %d@}" n
  | `Script n -> Format.fprintf fmt "script with %d leaves" n

class printer (text : Wtext.text) =
  let nodes : ProofEngine.position Wtext.marker = text#marker in
  let backs : ProofEngine.node Wtext.marker = text#marker in
  object(self)

    initializer
      begin
        nodes#set_hover [`BACKGROUND "orange"] ;
        backs#set_hover [`FOREGROUND "white" ; `BACKGROUND "red"] ;
      end

    method on_click f = nodes#on_click (fun (_,_,pos) -> f pos)
    method on_backtrack f = backs#on_click (fun (_,_,node) -> f node)

    method pp_node fmt node =
      nodes#mark (`Node node) Wpo.pp_title fmt (ProofEngine.goal node)

    method pp_main fmt tree =
      nodes#mark `Main Wpo.pp_title fmt (ProofEngine.main tree)

    method private results wpo =
      List.iter
        (fun (prv,res) ->
           if prv <> VCS.Tactical then
             if VCS.is_verdict res then
               if VCS.is_valid res then
                 text#printf "@{<bf>Prover@} %a: @{<green>%a@}.@\n"
                   VCS.pp_prover prv VCS.pp_result res
               else
                 text#printf "@{<bf>Prover@} %a: @{<green>%a@}.@\n"
                   VCS.pp_prover prv VCS.pp_result res
        ) (Wpo.get_results wpo)

    method private pp_state fmt node =
      match ProofEngine.state node with
      | `Proved -> Format.pp_print_string fmt "proved"
      | `Opened -> Format.pp_print_string fmt "opened"
      | `Pending 0 -> Format.pp_print_string fmt "terminated"
      | `Pending 1 -> Format.pp_print_string fmt "pending"
      | `Pending n -> Format.fprintf fmt "pending(%d)" n
      | `Script 0 -> Format.pp_print_string fmt "script"
      | `Script n -> Format.fprintf fmt "script(%d)" n

    method private tactic header node =
      match ProofEngine.children node with
      | [] ->
          text#printf "@{<bf>Tactical@}@} %s: @{<green>proved@} (Qed).@\n" header
      | [child] ->
          text#printf "@{<bf>Tactical@} %a: %a.@\n" self#pp_node child self#pp_state child
      | children ->
          begin match ProofEngine.pending node with
            | 0 -> text#printf "@{<green>@{<bf>Tactical@}@} %s: @{<green>proved@}.@\n" header
            | 1 -> text#printf "@{<bf>Tactical@} %s: @{<orange>pending@}.@\n" header ;
            | n -> text#printf "@{<bf>Tactical@} %s: @{<orange>pending(%d)@}.@\n" header n ;
          end ;
          List.iter
            (fun child -> text#printf "@{<bf>SubGoal@} %a : %a.@\n"
                self#pp_node child self#pp_state child)
            children

    method private alternative g a =
      let open ProofScript in match a with
      | Tactic(0,{ header },_) -> text#printf "@{<bf>Script@} %s: terminating.@\n" header
      | Tactic(n,{ header },_) -> text#printf "@{<bf>Script@} %s: pending %d.@\n" header n
      | Error(msg,_) -> text#printf "@{<bf>Script@} Error (%S).@\n" msg
      | Prover(p,r) ->
          if not (Wpo.has_verdict g p) then
            text#printf "@{<bf>Script@} %a: %a.@\n" VCS.pp_prover p VCS.pp_result r

    method private strategy index i h =
      text#printf "@{<bf>Strategy@} %s" 
        h.Strategy.tactical#title ;
      if index = i
      then text#printf "(%4.2f)*@\n" h.Strategy.priority
      else text#printf "@{<fg:grey>(%4.2f)@}@\n" h.Strategy.priority

    method pending node =
      begin
        let g = ProofEngine.goal node in
        self#results g ;
        match ProofEngine.tactical node with
        | None -> List.iter (self#alternative g) (ProofEngine.bound node)
        | Some { ProofScript.header } ->
            self#tactic header node ;
            let index,hs = ProofEngine.get_strategies node in
            if Array.length hs > 0 then
              ( text#hrule ; Array.iteri (self#strategy index) hs )
      end

    method status tree =
      match ProofEngine.current tree with
      | `Main -> self#results (ProofEngine.main tree)
      | `Internal node | `Leaf(_,node) -> self#pending node

    (* -------------------------------------------------------------------------- *)
    (* ---  Script Printing                                                   --- *)
    (* -------------------------------------------------------------------------- *)

    method private pp_step ~prefix ~here fmt node =
      begin
        let goal = ProofEngine.goal node in
        let pp_goal fmt goal =
          Format.fprintf fmt "Goal %a" Wpo.pp_title goal in
        if node == here then
          Format.fprintf fmt "@\n%s@{<ul>%a@}"
            prefix pp_goal goal
        else
          let pp_node = nodes#mark (`Node node) pp_goal in
          text#printf "@\n%s%a" prefix pp_node goal ;
      end

    method private backtrack fmt node =
      let k,hs = ProofEngine.get_strategies node in
      let n = Array.length hs in
      if n > 1 then
        let k = if succ k < n then succ k else 0 in
        let tac = hs.(k).Strategy.tactical in
        let pp_label fmt tac =
          Format.fprintf fmt "backtrack(%s,%d/%d)" tac#title (succ k) n
        in
        Format.fprintf fmt " [ %a ]" (backs#mark node pp_label) tac

    method private proofstep ~prefix ~direct ~path ~here fmt node =
      begin
        self#pp_step ~prefix ~here fmt node ;
        match ProofEngine.tactical node with
        | None ->
            Format.fprintf fmt " (%a)" pp_status node
        | Some tactic ->
            Format.fprintf fmt " (%s" tactic.ProofScript.header ;
            match ProofEngine.children node with

            | [] ->
                Format.fprintf fmt ": @{<green>qed@})"

            | _::_ when not (List.mem node path) ->
                Format.fprintf fmt ": %a)%a" pp_status node self#backtrack node

            | [child] ->
                Format.fprintf fmt ")%a" self#backtrack node ;
                self#proofstep ~prefix:direct ~direct ~path ~here fmt child

            | children ->
                Format.fprintf fmt ": %a)%a" pp_status node self#backtrack node ;
                let prefix = direct ^ " + " in
                let direct = direct ^ "   " in
                List.iter (self#proofstep ~prefix ~direct ~path ~here fmt) children
      end

    method tree tree =
      match ProofEngine.current tree with
      | `Main ->
          begin
            match ProofEngine.(get (main tree)) with
            | `Proof ->
                text#printf "@{<it>Existing Script (navigate to explore)@}@."
            | `Script ->
                text#printf "@{<it>Existing Script (replay to explore)@}@."
            | `Saved ->
                text#printf "@{<it>Saved Script (replay to load)@}@."
            | `None ->
                text#printf "@{<it>No Script@}@."
          end
      | `Internal here | `Leaf(_,here) ->
          begin
            let root,path = rootchain here [here] in
            let qed = if Wpo.is_proved (ProofEngine.main tree) then "Qed" else "End"
            in text#printf "@[<hv 0>@{<bf>Proof@}:%a@\n@{<bf>%s@}.@]@."
              (self#proofstep ~prefix:"  " ~direct:"  " ~path ~here) root qed ;
          end

  end
