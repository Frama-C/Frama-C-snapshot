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
(* --- Built-in Tactics                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

let (^) a b =
  if a="" then b else
  if b="" then a else
    Printf.sprintf "%s ; %s" a b

let t_id s = ["",s]
let t_absurd s = [ "Absurd" , (fst s,p_false) ]
let t_descr d0 p s = List.map (fun (d,s) -> (d0 ^ d) , s) (p s)

let t_finally d0 s = [ d0 , s ]

let t_chain (p : Tactical.process) (q : Tactical.process) s =
  let pool = ref [] in
  List.iter
    (fun (d,s) ->
       List.iter
         (fun (d',s') ->
            pool := (d ^ d' , s') :: !pool
         ) (q s)
    ) (p s) ;
  List.rev !pool

let t_split ?(pos="") ?(neg="") p (hs,g) =
  [ pos , (hs,p_imply p g) ; neg , (hs,p_imply (p_not p) g) ]

let t_cut ?(by="") (p : F.pred) (pi : Tactical.process) (hs,g) =
  ( by , (hs,p) ) :: (pi (hs,p_imply p g))

let t_case (p : F.pred) (a : Tactical.process) (b : Tactical.process) =
  fun (hs,g) ->
    List.append
      (a (hs,F.p_imply p g))
      (b (hs,F.p_imply (F.p_not p) g))

let t_cases ?(complete = "complete") (dps : (pred * Tactical.process) list) =
  fun (hs,g) ->
    let pool = ref [] in
    List.iter
      (fun (p,pi) ->
         List.iter
           (fun u -> pool := u :: !pool)
           (pi (hs , p_imply p g))
      ) dps ;
    ( complete , (hs , p_any fst dps) ) :: List.rev !pool

let t_range e a b ~upper ~lower ~range s =
  if (not (a <= b)) then raise (Invalid_argument "Wp.Auto.t_range") ;
  let cases = ref [] in
  for i = a to b do
    cases := (Printf.sprintf "Value %d" i , p_equal e (e_int i)) :: !cases ;
  done ;
  List.concat [
    upper (fst s , p_lt e (e_int a)) ;
    lower (fst s , p_lt (e_int b) e) ;
    t_chain (Tactical.insert !cases) range s ;
  ]

let t_replace ?(equal="equal") ~src ~tgt (pi : Tactical.process) s =
  let s' = Conditions.subst (fun e -> if e == src then tgt else e) s in
  (equal , (fst s, p_equal src tgt)) :: (pi s')

(* -------------------------------------------------------------------------- *)
(* --- Built-in Strategies                                                --- *)
(* -------------------------------------------------------------------------- *)

let array = TacArray.strategy
let choice = TacChoice.Choice.strategy
let absurd = TacChoice.Absurd.strategy
let contrapose = TacChoice.Contrapose.strategy
let compound = TacCompound.strategy
let cut = TacCut.strategy
let filter = TacFilter.strategy
let havoc = TacHavoc.Havoc.strategy
let separated = TacHavoc.Separated.strategy
let intuition = TacNormalForm.strategy
let range = TacRange.strategy
let split = TacSplit.strategy
let definition = TacUnfold.strategy
let instance = TacInstance.strategy
let lemma = TacLemma.strategy

(* -------------------------------------------------------------------------- *)
(* --- Auto-Range                                                         --- *)
(* -------------------------------------------------------------------------- *)

module Range =
struct

  open Repr
  
  let update merge x v ofs map =
    match Repr.term v with
    | Int v -> 
        let v0 = Integer.add v ofs in
        let v1 = try merge v0 (Tmap.find x map) with Not_found -> v0 in
        Tmap.add x v1 map
    | _ -> map
      
  type rg = {
    mutable vmin : Integer.t F.Tmap.t ;
    mutable vmax : Integer.t F.Tmap.t ;
  }
  
  let set_vmin rg x v ofs =
    rg.vmin <- update Integer.max x v ofs rg.vmin
        
  let set_vmax rg x v ofs =
    rg.vmax <- update Integer.min x v ofs rg.vmax
        
  let rec add_bound rg p =
    match Repr.term p with
    | And ps -> List.iter (add_bound rg) ps
    | Lt(a,b) when Lang.F.is_int a && Lang.F.is_int b ->
        set_vmax rg a b Integer.minus_one ;
        set_vmin rg b a Integer.one ;
    | Leq(a,b) when Lang.F.is_int a && Lang.F.is_int b ->
        set_vmax rg a b Integer.zero ;
        set_vmin rg b a Integer.zero ;
    | _ -> ()

  let compute hs =
    let rg = { vmin = F.Tmap.empty ; vmax = F.Tmap.empty } in
    Conditions.iter
      (fun s ->
         let open Conditions in
         match s.condition with
         | Have p | When p | Core p -> add_bound rg (F.e_prop p)
         | _ -> ())
      hs ;
    rg

  let ranges rg =
    Tmap.interf
      (fun _ a b ->
         try Some(Integer.to_int a,Integer.to_int b)
         with Integer.Too_big -> None
      ) rg.vmin rg.vmax

  let small = function
    | None -> None
    | Some z -> try Some(Integer.to_int z) with Integer.Too_big -> None
  
  let bounds rg =
    Tmap.merge
      (fun _ a b -> Some(small a,small b))
      rg.vmin rg.vmax
      
end

(* -------------------------------------------------------------------------- *)
(* --- Heuristics: Auto-Range                                             --- *)
(* -------------------------------------------------------------------------- *)

class autorange =
  object
    method id = "Wp.autorange"
    method title = "Auto Range"
    method descr = "Iterate over term constrained by a finite interval"

    method search push (hyps,goal) =
      let ranged = Range.ranges (Range.compute hyps) in
      Tmap.iter
        (fun e (a,b) ->
           if Strategy.occurs_p e goal && b-a <= 1024 then
             let selection = Tactical.(Inside(Goal goal,e)) in
             push (range selection ~vmin:a ~vmax:b)
        ) ranged
  end

let auto_range = Strategy.export (new autorange)

(* -------------------------------------------------------------------------- *)
(* --- Heuristics: Auto-Split                                             --- *)
(* -------------------------------------------------------------------------- *)

class autosplit =
  object
    method id = "Wp.autosplit"
    method title = "Auto Split"
    method descr = "Split on any branch (priority to goal variables)"
    method search push seq =
      let target = Lang.F.varsp (snd seq) in
      Conditions.iter
        (fun s ->
           let open Lang in
           let open Conditions in
           match s.condition with
           | Branch(_,sa,sb) ->
               let priority =
                 if F.Vars.intersect target (Conditions.vars_hyp sa) ||
                    F.Vars.intersect target (Conditions.vars_hyp sb)
                 then 2.0 else 0.5 in
               let selection = Tactical.(Clause(Step s)) in
               push (split ~priority selection)
           | _ -> ()
        ) (fst seq)
  end

let auto_split = Strategy.export (new autosplit)

(**************************************************************************)
