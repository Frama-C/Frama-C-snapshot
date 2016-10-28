(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(* -------------------------------------------------------------------------- *)

open Qed.Plib
module F = Lang.F
module Env = Plang.Env
open F

type env = Plang.Env.t

let rec xmark_hyp pool f seq =
  let open Conditions in
  if not (Vars.subset (Conditions.vars_hyp seq) (Plang.xmark pool)) then
    Conditions.iter
      (fun step ->
         if not (Vars.subset step.vars (Plang.xmark pool)) then
           match step.condition with
           | Have p | When p | Type p | Init p | Core p ->
               Plang.xmark_p pool f p
           | Branch(p,sa,sb) ->
               Plang.xmark_p pool f p ;
               xmark_hyp pool f sa ;
               xmark_hyp pool f sb ;
           | Either cases -> List.iter (xmark_hyp pool f) cases
      ) seq

let xmark_seq pool f (hs,g) =
  begin
    Plang.xmark_p pool f g ; (* Mark goal first *)
    xmark_hyp pool f hs ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Sequent Printer                                                    --- *)
(* -------------------------------------------------------------------------- *)

open Conditions

let mark m step =
  match step.condition with
  | When p | Type p | Have p | Init p | Core p | Branch(p,_,_) -> F.mark_p m p
  | Either _ -> ()

let spaced pp fmt a = Format.pp_print_space fmt () ; pp fmt a
let append pp fmt a = pp fmt a ; Format.pp_print_space fmt ()
let pp_open_block pp fmt a left =
  Format.fprintf fmt "@[<hv 0>@[<hv 2>%a %s" pp a left
let pp_close_block fmt right =
  Format.fprintf fmt "@]@ %s@]" right

class engine (lang : Plang.engine) =
  object(self)

    (* -------------------------------------------------------------------------- *)
    (* --- Horizontal Printers                                                --- *)
    (* -------------------------------------------------------------------------- *)
    
    method pp_clause fmt s = Format.fprintf fmt "@{<wp:clause>%s@}" s
    method pp_name = Format.pp_print_string
    method pp_core = lang#pp_sort
    method pp_comment fmt s =
      Format.fprintf fmt "@[<hov 0>@{<wp:comment>(* %s *)@}@]" s
    method pp_property fmt p =
      Format.fprintf fmt "@[<hov 0>@{<wp:property>(* %a *)@}@]"
        Description.pp_local p
    method pp_warning fmt w =
      let open Warning in
      Format.fprintf fmt "@[<hov 0>@{<wp:warning>Warning@}[%s]: %s@ (%s).@]"
        w.source w.reason w.effect

    method pp_definition fmt x e =
      Format.fprintf fmt "@[<hov 4>%a %a = %a.@]"
        self#pp_clause "Let" self#pp_name x self#pp_core e
        
    method pp_intro ~step ~clause ?(dot=".") fmt p =
      ignore step ;
      Format.fprintf fmt "@[<hov 4>%a %a%s@]"
        self#pp_clause clause lang#pp_pred p dot

    (* -------------------------------------------------------------------------- *)
    (* --- Block Printers                                                     --- *)
    (* -------------------------------------------------------------------------- *)
    
    method pp_condition ~step fmt = function
      | Core p -> self#pp_intro ~step ~clause:"Core:" fmt p
      | Type p -> self#pp_intro ~step ~clause:"Type:" fmt p
      | Init p -> self#pp_intro ~step ~clause:"Init:" fmt p
      | Have p -> self#pp_intro ~step ~clause:"Have:" fmt p
      | When p -> self#pp_intro ~step ~clause:"When:" fmt p
      | Branch(p,sa,sb) ->
          begin
            self#pp_intro ~step ~clause:"If" ~dot:"" fmt p ;
            if not (Conditions.is_empty sa)
            then self#pp_sequence ~clause:"Then" fmt sa ;
            if not (Conditions.is_empty sb)
            then self#pp_sequence ~clause:"Else" fmt sb ;
          end
      | Either cases ->
          begin
            pp_open_block self#pp_clause fmt "Either" "{" ;
            List.iter
              (fun seq ->
                 Format.fprintf fmt "@ @[<hv 2>%a" self#pp_clause "Case:" ;
                 self#block fmt seq ;
                 Format.fprintf fmt "@]" ;
              ) cases ;
            pp_close_block fmt "}" ;
          end
    
    method pp_step fmt step =
      begin
        ( match step.descr with None -> () | Some s ->
          spaced self#pp_comment fmt s ) ;
        Warning.Set.iter (spaced self#pp_warning fmt) step.warn ;
        List.iter (spaced self#pp_property fmt) step.deps ;
        spaced (self#pp_condition ~step) fmt step.condition ;
      end

    method pp_sequence ~clause fmt seq =
      if Conditions.is_empty seq then
        Format.fprintf fmt "@ %a {}" self#pp_clause clause
      else
        begin
          Format.pp_print_space fmt () ;
          pp_open_block self#pp_clause fmt clause "{";
          self#block fmt seq ;
          pp_close_block fmt "}" ;
        end

    method private dump fmt seq () =
      let pool = Plang.pool () in
      xmark_seq pool (fun x -> ignore (lang#bind x)) seq ;
      let env,marks = lang#marks in
      let hs,goal = seq in
      F.mark_p marks goal ;
      Conditions.iter (self#mark marks) hs ;
      Format.fprintf fmt "@[<hv 0>" ;
      List.iter (append (self#define env) fmt) (F.defs marks) ;
      lang#scope env
        begin fun () ->
          if not (Conditions.is_empty hs) then
            begin
              pp_open_block self#pp_clause fmt "Assume" "{" ;
              Conditions.iter (self#pp_step fmt) hs ;
              pp_close_block fmt "}" ;
              Format.pp_print_newline fmt () ;
            end ;
          Format.fprintf fmt "@[<hov 4>%a %a.@]"
            self#pp_clause "Prove:" lang#pp_pred goal ;
        end ;
      Format.fprintf fmt "@]@."

    method pp_sequent fmt seq =
      lang#global (self#dump fmt seq)

    method pp_esequent env fmt seq =
      lang#scope env (self#dump fmt seq)

    (* --- Scope Management --- *)

    method mark m s = mark m s
    method name env e = Env.fresh env (F.basename e)
    
    method private define env fmt e =
      let name = self#name env e in
      lang#scope env (fun () -> self#pp_definition fmt name e) ;
      Env.define env name e

    method private block fmt seq =
      begin
        let env,marks = lang#marks in
        Conditions.iter (self#mark marks) seq ;
        List.iter (spaced (self#define env) fmt) (F.defs marks) ;
        lang#scope env (fun () -> Conditions.iter (self#pp_step fmt) seq) ;
      end
    
  end
  
(* -------------------------------------------------------------------------- *)
(* --- All-In-One Printers                                                --- *)
(* -------------------------------------------------------------------------- *)

let pretty fmt seq =
  let plang = new Plang.engine in
  let pcond = new engine plang in
  pcond#pp_sequent fmt seq

let sequence ?(clause="Assume") fmt seq =
  let plang = new Plang.engine in
  let pcond = new engine plang in
  plang#global
    (fun () ->
       Vars.iter (fun x -> ignore (plang#bind x)) (Conditions.vars_hyp seq) ;
       pcond#pp_sequence ~clause fmt seq)

let bundle ?clause fmt bundle =
  sequence ?clause fmt (Conditions.sequence bundle)

let dump = bundle ~clause:"Assume"
