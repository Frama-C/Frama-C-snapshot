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
(* --- Tactic to Apply Substitution by Hand                                   *)
(* -------------------------------------------------------------------------- *)
open Tactical
open Repr

let rewrite ~select ~replaced ~value =
  Applicable
    (Tactical.rewrite
       ?at:(Tactical.at select)
       [ "Rewrite" , Lang.F.p_true , replaced , value ])

let hypothesis s =
  let open Conditions in
  match s.condition with
  | When p | Have p | Core p | Init p -> p
  | _ -> raise Not_found

let clause = function
  | Clause(Step s) -> hypothesis s
  | Inside(Step s,e) ->
      begin
        match Repr.pred (hypothesis s) with
        | And es when List.memq e es -> Lang.F.p_bool e
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

class rewrite dir =
  let id = if dir then "Wp.TacRewrite.Left" else "Wp.TacRewrite.Right" in
  let title = if dir then "Rewrite (<-)" else "Rewrite (->)" in
  object
    inherit Tactical.make
        ~id ~title ~descr:"Rewrite from equality" ~params:[]

    method select _feedback select =
      try
        let p = clause select in
        match Repr.pred p with
        | Eq(a,b) ->
            let replaced,value = if dir then a,b else b,a in
            rewrite ~select ~replaced ~value
        | _ -> Not_applicable
      with Not_found -> Not_applicable
  end

let tacl = Tactical.export (new rewrite true :>  Tactical.tactical)
let tacr = Tactical.export (new rewrite false :>  Tactical.tactical)

let mem a b =
  let rec walk m e =
    if a==e then raise Exit ;
    if not (Lang.F.Tset.mem e !m) then
      begin
        m := Lang.F.Tset.add e !m ;
        Lang.F.lc_iter (walk m) e
      end
  in try walk (ref Lang.F.Tset.empty) b ; false with Exit -> true

let direct a goal =
  match Repr.term goal with
  | Eq(u,v) when a == u || a == v -> true
  | _ -> false

let submit push select e goal rw =
  if direct e goal then
    push (Strategy.make ~priority:1.5 rw select)
  else
  if mem e goal then
    push (Strategy.make ~priority:0.5 rw select)

let rec lookup step push goal e =
  match Repr.term e with
  | And ps -> List.iter (lookup step push goal) ps
  | Eq(a,b) ->
      begin
        let select = Inside(Step step,e) in
        submit push select a goal tacl ;
        submit push select b goal tacr ;
      end
  | _ -> ()
      
class auto_rewrite =
  object
    method id = "WP.autorewrite"
    method title = "Auto Rewrite"
    method descr = "Lookup for Equalities to Rewrite with"
    method search (push : Strategy.strategy -> unit) (hyps,goal) =
      Conditions.iter
        (fun s ->
           let open Conditions in
           match s.condition with
           | Have p | When p | Core p | Init p ->
               lookup s push (Lang.F.e_prop goal) (Lang.F.e_prop p)
           | _ -> ())
        hyps 
  end

let () = Strategy.register (new auto_rewrite)

type dir = [ `Left | `Right ]
let tactical = function
  | `Left -> tacl
  | `Right -> tacr

let strategy ?priority dir selection =
  Strategy.make ?priority (tactical dir) selection
