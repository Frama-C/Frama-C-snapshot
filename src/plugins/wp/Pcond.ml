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

(* -------------------------------------------------------------------------- *)

module F = Lang.F
module Env = Plang.Env
open F

let dkey_state = Wp_parameters.register_category "state"

type env = Plang.Env.t

let rec alloc_hyp pool f seq =
  let open Conditions in
  if not (Vars.subset (Conditions.vars_hyp seq) (Plang.alloc_domain pool))
  then
    Conditions.iter
      (fun step ->
         if not (Vars.subset step.vars (Plang.alloc_domain pool)) then
           match step.condition with
           | State _ ->
               Plang.alloc_xs pool f step.vars
           | Have p | When p | Type p | Init p | Core p ->
               Plang.alloc_p pool f p
           | Branch(p,sa,sb) ->
               Plang.alloc_p pool f p ;
               alloc_hyp pool f sa ;
               alloc_hyp pool f sb ;
           | Either cases -> List.iter (alloc_hyp pool f) cases
      ) seq

let alloc_seq pool f (hs,g) =
  begin
    Plang.alloc_p pool f g ; (* Mark goal first *)
    alloc_hyp pool f hs ;
  end

(* -------------------------------------------------------------------------- *)
(* --- State Printer                                                      --- *)
(* -------------------------------------------------------------------------- *)

type context = NoWhere | InAddr | AtLabel of Pcfg.label

class state =
  object(self)
    inherit Plang.engine as super
    inherit Pcfg.engine
    val mutable env = Pcfg.create ()
    val mutable context = NoWhere
    val mutable domain = Vars.empty

    method clear =
      begin
        env <- Pcfg.create () ;
        context <- NoWhere ;
        domain <- Vars.empty ;
      end

    method set_sequence seq =
      begin
        env <- Pcfg.register seq ;
        context <- NoWhere ;
        domain <- Conditions.vars_hyp seq ;
      end

    method domain = domain
    method set_domain vars = domain <- vars

    method label_at ~id = Pcfg.at env ~id

    method private at : 'a. ?lbl:Pcfg.label ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
      begin fun ?lbl pp fmt w ->
        match context , lbl with
        | NoWhere , None ->
            context <- InAddr ;
            Format.fprintf fmt "« %a »" pp w ;
            context <- NoWhere ;
        | NoWhere , Some l ->
            context <- AtLabel l ;
            Format.fprintf fmt "« %a »%a" pp w self#pp_at l ;
            context <- NoWhere ;
        | InAddr , None -> pp fmt w
        | AtLabel _ , None -> pp fmt w
        | AtLabel l0 , Some l when l == l0 -> pp fmt w
        | (InAddr | AtLabel _) as here , Some l ->
            context <- AtLabel l ;
            Format.fprintf fmt "( %a )%a" pp w self#pp_at l ;
            context <- here ;
      end

    method private atflow : 'a. ?lbl:Pcfg.label ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
      begin fun ?lbl pp fmt w ->
        match context , lbl with
        | NoWhere , None ->
            context <- InAddr ;
            pp fmt w ;
            context <- NoWhere ;
        | InAddr , None -> pp fmt w
        | AtLabel _ , None -> pp fmt w
        | AtLabel l0 , Some l when l == l0 -> pp fmt w
        | (InAddr | AtLabel _ | NoWhere) as here , Some l ->
            context <- AtLabel l ;
            Format.fprintf fmt "%a%a" pp w self#pp_at l ;
            context <- here ;
      end

    method pp_at fmt lbl = Format.fprintf fmt "@@%a" self#pp_label lbl

    val mutable force = false

    method! pp_var fmt x = Format.pp_print_char fmt '`' ; Format.pp_print_string fmt x

    method! pp_repr fmt e =
      if force then (force <- false ; super#pp_repr fmt e) else
        begin
          match Pcfg.find env e with
          | Pcfg.Term -> super#pp_repr fmt e
          | Pcfg.Addr lv ->
              if self#is_atomic_lv lv
              then self#atflow self#pp_addr fmt lv
              else self#at self#pp_addr fmt lv
          | Pcfg.Lval(lv,lbl) ->
              if self#is_atomic_lv lv
              then self#atflow ~lbl self#pp_lval fmt lv
              else self#at ~lbl self#pp_lval fmt lv
          | Pcfg.Chunk(m,lbl) ->
              self#atflow ~lbl self#pp_chunk fmt m
        end

    method pp_value fmt e = force <- true ; super#pp_sort fmt e
    method! pp_sort fmt e = context <- NoWhere ; super#pp_sort fmt e
    method! pp_term fmt e = context <- NoWhere ; super#pp_term fmt e
    method! pp_pred fmt p = context <- NoWhere ; super#pp_pred fmt p

    method! subterms f e =
      if not (Pcfg.subterms env f e) then super#subterms f e

    method updates seq = Pcfg.updates env seq domain

    method pp_update lbl fmt = function Memory.Mstore(lv,v) ->
      let stack = context in
      context <- AtLabel lbl ;
      Format.fprintf fmt "@[<hov 2>%a =@ %a;@]"
        self#pp_lval lv self#pp_value v ;
      context <- stack ;

  end

(* -------------------------------------------------------------------------- *)
(* --- Sequent Printer                                                    --- *)
(* -------------------------------------------------------------------------- *)

open Conditions

let mark_step m step =
  (* sub-sequences are marked recursively marked later *)
  match step.condition with
  | When p | Type p | Have p | Init p | Core p
  | Branch(p,_,_) -> F.mark_p m p
  | Either _ | State _ -> ()

let spaced pp fmt a = Format.pp_print_space fmt () ; pp fmt a
let append pp fmt a = pp fmt a ; Format.pp_print_space fmt ()
let pp_open_block pp fmt a left =
  Format.fprintf fmt "@[<hv 0>@[<hv 2>%a %s" pp a left
let pp_close_block fmt right =
  Format.fprintf fmt "@]@ %s@]" right

class engine (lang : #Plang.engine) =
  object(self)

    (* -------------------------------------------------------------------------- *)
    (* --- Horizontal Printers                                                --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_clause fmt s = Format.fprintf fmt "@{<wp:clause>%s@}" s
    method pp_stmt fmt s = Format.fprintf fmt "@{<wp:stmt>%s@}" s
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
      | State _ -> ()
      | Core p -> self#pp_intro ~step ~clause:"Core:" fmt p
      | Type p -> self#pp_intro ~step ~clause:"Type:" fmt p
      | Init p -> self#pp_intro ~step ~clause:"Init:" fmt p
      | Have p -> self#pp_intro ~step ~clause:"Have:" fmt p
      | When p -> self#pp_intro ~step ~clause:"When:" fmt p
      | Branch(p,sa,sb) ->
          begin
            self#pp_intro ~step ~clause:"If" ~dot:"" fmt p ;
            if not (Conditions.is_true sa)
            then self#sequence ~clause:"Then" fmt sa ;
            if not (Conditions.is_true sb)
            then self#sequence ~clause:"Else" fmt sb ;
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
      match step.condition with
      | State _ ->
          self#pp_condition ~step fmt step.condition
      | _ ->
          begin
            ( match step.descr with None -> () | Some s ->
                  spaced self#pp_comment fmt s ) ;
            Warning.Set.iter (spaced self#pp_warning fmt) step.warn ;
            List.iter (spaced self#pp_property fmt) step.deps ;
            spaced (self#pp_condition ~step) fmt step.condition ;
          end

    method private sequence ~clause fmt seq =
      Format.pp_print_space fmt () ; self#pp_block ~clause fmt seq

    method pp_block ~clause fmt seq =
      if Conditions.is_true seq then
        Format.fprintf fmt "%a {}" self#pp_clause clause
      else
        begin
          pp_open_block self#pp_clause fmt clause "{";
          self#block fmt seq ;
          pp_close_block fmt "}" ;
        end

    method private dump fmt seq () =
      alloc_seq (Plang.pool ()) (fun x -> ignore (lang#bind x)) seq ;
      let env,marks = lang#marks in
      let hs,goal = seq in
      F.mark_p marks goal ;
      Conditions.iter (self#mark marks) hs ;
      Format.fprintf fmt "@[<hv 0>" ;
      List.iter (append (self#define env) fmt) (F.defs marks) ;
      lang#set_env env ;
      if not (Conditions.is_true hs) then
        begin
          self#pp_block ~clause:"Assume" fmt hs ;
          Format.pp_print_newline fmt () ;
        end ;
      self#pp_goal fmt goal ;
      Format.fprintf fmt "@]@."

    method pp_goal fmt goal =
      Format.fprintf fmt "@[<hov 4>%a %a.@]"
        self#pp_clause "Prove:" lang#pp_pred goal

    method pp_sequence ~clause fmt seq =
      lang#global (fun () -> self#pp_block ~clause fmt seq)

    method pp_sequent fmt seq =
      lang#global (self#dump fmt seq)

    method pp_esequent env fmt seq =
      lang#set_env env ;
      self#dump fmt seq ()

    (* --- Scope Management --- *)

    method mark m s = mark_step m s
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

let is_nop = function None -> true | Some(_,upd) -> Bag.is_empty upd

class sequence (lang : #state) =
  object(self)
    inherit engine lang as super

    method private label step = function
      | State _ ->
          (try Some (lang#label_at ~id:step.id)
           with Not_found -> None)
      | _ -> None

    method private updates fmt = function
      | None -> ()
      | Some( lbl , upd ) ->
          if not (Bag.is_empty upd) then
            Bag.iter ((spaced (lang#pp_update lbl)) fmt) upd

    method! pp_condition ~step fmt cond =
      match self#label step cond with
      | None -> super#pp_condition ~step fmt cond
      | Some lbl ->
          let before = match Pcfg.prev lbl with
            | [ pre ] when (Pcfg.branching pre) ->
                let seq = Memory.{ pre ; post = lbl } in
                let upd = lang#updates seq in
                Some(pre,upd)
            | _ -> None in
          let after = match Pcfg.next lbl with
            | [ post ] ->
                let seq = Memory.{ pre = lbl ; post } in
                let upd = lang#updates seq in
                Some(lbl,upd)
            | _ -> None in
          if Pcfg.visible lbl || not (is_nop before) || not (is_nop after)
             || Wp_parameters.debug_atleast 1
          then
            lang#with_mode Qed.Engine.Mterm
              (fun _mode ->
                 begin
                   Format.fprintf fmt "@ @[<hv 0>@[<hv 2>%a {" self#pp_stmt "Stmt" ;
                   self#updates fmt before ;
                   if Pcfg.visible lbl then
                     Format.fprintf fmt "@ %a:" lang#pp_label lbl ;
                   if Wp_parameters.debug_atleast 1 then
                     begin
                       if not (Pcfg.visible lbl) then
                         Format.fprintf fmt "@ label %a:" lang#pp_label lbl ;
                       List.iter
                         (fun lbl -> Format.fprintf fmt "@ from %a;" lang#pp_label lbl)
                         (Pcfg.prev lbl) ;
                       List.iter
                         (fun lbl -> Format.fprintf fmt "@ goto %a;" lang#pp_label lbl)
                         (Pcfg.next lbl) ;
                       (*
                       Pcfg.iter
                         (fun _m v ->
                            if Vars.intersect lang#domain (F.vars v) then
                              Format.fprintf fmt "@ (%a := %a)"
                                lang#pp_term v lang#pp_value v
                         ) lbl ;
                       *)
                     end ;
                   self#updates fmt after ;
                   Format.fprintf fmt " @]@ }@]" ;
                 end)

    val mutable active = true
    method set_state s = active <- s
    method get_state = active

    method set_sequence seq =
      if active then
        lang#set_sequence seq
      else
        lang#clear

    method set_goal p =
      lang#set_domain (Vars.union lang#domain (F.varsp p))

    method set_sequent (hs,p) =
      self#set_sequence hs ; self#set_goal p

    method! pp_sequence ~clause fmt seq =
      begin
        self#set_sequence seq ;
        super#pp_sequence ~clause fmt seq ;
      end

    method! pp_sequent fmt seq =
      begin
        self#set_sequent seq ;
        super#pp_sequent fmt seq ;
      end

    method! pp_esequent env fmt seq =
      begin
        self#set_sequent seq ;
        super#pp_esequent env fmt seq ;
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- All-In-One Printers                                                --- *)
(* -------------------------------------------------------------------------- *)

let engine () =
  if Wp_parameters.has_dkey dkey_state then
    ( new sequence (new state) :> engine )
  else
    new engine (new Plang.engine)

let pretty fmt seq =
  (engine())#pp_sequent fmt seq

let () = Conditions.pretty := pretty

let sequence ?(clause="Sequence") fmt seq =
  let plang = new Plang.engine in
  let pcond = new engine plang in
  plang#global
    (fun () ->
       Vars.iter (fun x -> ignore (plang#bind x)) (Conditions.vars_hyp seq) ;
       pcond#pp_sequence ~clause fmt seq)

let bundle ?clause fmt bundle =
  sequence ?clause fmt (Conditions.bundle bundle)

let dump = bundle ~clause:"Bundle"
