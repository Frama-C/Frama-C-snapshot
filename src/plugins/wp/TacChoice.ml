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

(* -------------------------------------------------------------------------- *)
(* --- Choice Tactical                                                    --- *)
(* -------------------------------------------------------------------------- *)

class choice =
  object
    inherit Tactical.make
        ~id:"Wp.choice"
        ~title:"Choice"
        ~descr:"Select a Goal Alternative"
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      match s with
      | Inside(Goal p,q) ->
          begin
            match F.e_expr p with
            | Qed.Logic.Or qs when List.memq q qs ->
                Applicable (fun (hs,_) -> ["Choice",(hs,F.p_bool q)])
            | _ -> Not_applicable
          end
      | Empty | Compose _ | Clause _ | Inside(Step _,_) ->
          Not_applicable
  end

class absurd =
  object
    inherit Tactical.make
        ~id:"Wp.absurd"
        ~title:"Absurd"
        ~descr:"Contradict an Hypothesis"
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      match s with
      | Empty | Compose _ | Inside _ | Clause(Goal _)
        -> Not_applicable
      | Clause(Step s) ->
          begin
            match s.condition with
            | Have p | When p | Core p ->
                let absurd seq =
                  let emp = Conditions.(step (Have F.p_true)) in
                  let seq = Conditions.replace ~at:s.id emp seq in
                  [ "Absurd" , (fst seq , F.p_not p) ]
                in Applicable absurd
            | Init _ | Type _ | Branch _ | Either _ | State _ ->
                Not_applicable
          end
  end

class contrapose =
  object
    inherit Tactical.make
        ~id:"Wp.contrapose"
        ~title:"Contrapose"
        ~descr:"Swap and Negate Hypothesis with Conclusion"
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      match s with
      | Empty | Compose _ | Inside _ | Clause(Goal _)
        -> Not_applicable
      | Clause(Step s) ->
          begin
            match s.condition with
            | Have p | When p | Core p ->
                let contrapose (hs,goal) =
                  let descr = "Contrapose" in
                  let goal = F.p_not goal in
                  let goal = Conditions.(step ~descr (Have goal)) in
                  let hs = Conditions.replace ~at:s.id goal (hs , F.p_false) in
                  [ "Contrapose" , (fst hs , F.p_not p) ]
                in Applicable contrapose
            | Init _ | Type _ | Branch _ | Either _ | State _ ->
                Not_applicable
          end

  end

module Choice =
struct
  let tactical = Tactical.export (new choice)
  let strategy = Strategy.make tactical
end

module Absurd =
struct
  let tactical = Tactical.export (new absurd)
  let strategy = Strategy.make tactical
end

module Contrapose =
struct
  let tactical = Tactical.export (new contrapose)
  let strategy = Strategy.make tactical
end
