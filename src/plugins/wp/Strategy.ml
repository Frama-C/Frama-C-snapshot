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
(* --- Lookup Selection                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F
open Tactical
open Conditions

let env seq = Repr.env (Conditions.vars_seq seq)

type lookup = {
  target : Lang.F.term ;
  mutable marked : Tset.t ;
}

let lookup_term env e =
  let rec walk env e =
    if e == env.target then raise Exit ;
    if not (Tset.mem e env.marked) then
      begin
        env.marked <- Tset.add e env.marked ;
        Lang.F.lc_iter (walk env) e ;
      end
  in
  try walk env e ; false
  with Exit -> true

let occurs_x x a = F.Vars.mem x (F.vars a)
let occurs_y x p = F.Vars.mem x (F.varsp p)
let occurs_e a b = lookup_term { target = a ; marked = Tset.empty } b
let occurs_p a p = occurs_e a (F.e_prop p)
let occurs_q p q = occurs_e (F.e_prop p) (F.e_prop q)

let lookup_step env queue s =
  match s.condition with
  | State _ -> Empty
  | When p | Have p | Init p | Core p | Type p ->
      let p = Lang.F.e_prop p in
      if p == env.target then Clause(Step s) else
      if lookup_term env p then Inside(Step s,env.target) else Empty
  | Branch(c,sa,sb) ->
      let p = Lang.F.e_prop c in
      if lookup_term env p then Inside(Step s,env.target) else
        ( Queue.add sa queue ; Queue.add sb queue ; Empty )
  | Either cs ->
      List.iter (fun s -> Queue.add s queue) cs ; Empty

exception Found of selection

let lookup_sequence env queue seq =
  Conditions.iter
    (fun s ->
       match lookup_step env queue s with
       | Empty -> ()
       | sel -> raise (Found sel)
    ) seq

let select_e (sequence,goal) e =
  let g = Lang.F.e_prop goal in
  if g == e then Clause(Goal goal) else
    let env = { target = e ; marked = Tset.empty } in
    if lookup_term env g then Inside(Goal goal,e) else
      try
        let queue = Queue.create () in
        lookup_sequence env queue sequence ;
        while not (Queue.is_empty queue) do
          lookup_sequence env queue (Queue.pop queue)
        done ; Empty
      with Found sel -> sel

let select_p seq p = select_e seq (F.e_prop p)

(* -------------------------------------------------------------------------- *)
(* --- Elementary Tactics                                                 --- *)
(* -------------------------------------------------------------------------- *)

type argument = ARG: 'a field * 'a -> argument

type strategy = {
  priority : float ;
  tactical : tactical ;
  selection : selection ;
  arguments : argument list ;
} and t = strategy

let highest a b = Pervasives.compare b.priority a.priority

class pool =
  object
    val pool : strategy Vector.t = Vector.create ()
    method add = Vector.add pool
    method sort =
      let hs = Vector.to_array pool in
      Array.stable_sort highest hs ; hs
  end

class type heuristic =
  object
    method id : string
    method title : string
    method descr : string
    method search : (strategy -> unit) -> sequent -> unit
  end

module Tmap = Map.Make(String)
let registry = ref Tmap.empty

let register s =
  let id = s#id in
  if Tmap.mem id !registry then
    Wp_parameters.error "Strategy #%s already registered (skipped)" id
  else
    registry := Tmap.add id (s :> heuristic) !registry

let export s = register s ; (s :> heuristic)

let lookup ~id = Tmap.find id !registry

let iter f = Tmap.iter (fun _ s -> f s) !registry

let arg fd v = ARG(fd,v)
let set_arg (tactical : #tactical) =
  function ARG(fd,v) -> tactical#set_field fd v
let set_args tactical arguments =
  List.iter (set_arg tactical) arguments

let make tactical ?(priority=1.0) selection =
  { priority ; tactical ; selection ; arguments = [] }

(* -------------------------------------------------------------------------- *)
