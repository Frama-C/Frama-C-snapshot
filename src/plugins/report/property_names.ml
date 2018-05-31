(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil_types

type part =
  | B of behavior
  | K of kernel_function
  | A of string
  | I of identified_predicate
  | P of predicate
  | T of term
  | S of stmt

let is_name s = s <> "" && s <> "_"
let join ps = String.concat "_" (List.filter is_name ps)

let string_of_part = function
  | B bhv -> if Cil.is_default_behavior bhv then "" else bhv.b_name
  | K kf -> Kernel_function.get_name kf
  | A msg -> msg
  | S stmt -> Printf.sprintf "s%d" stmt.sid
  | I { ip_content = { pred_name = a } }
  | P { pred_name = a } | T { term_name = a } -> join a

let string_of_parts ps = join (List.map string_of_part ps)

let rec parts_of_property ip : part list =
  let open Property in
  match ip with
  | IPBehavior _ -> []
  | IPPredicate (PKAssumes _,_,_,_) -> []
  | IPPredicate (PKRequires bhv,kf,_,ip) ->
    [ K kf ; B bhv ; A "requires" ; I ip ]
  | IPPredicate (PKEnsures(bhv,Normal),kf,_,ip) ->
    [ K kf ; B bhv ; A "ensures" ; I ip ]
  | IPPredicate (PKEnsures(bhv,Exits),kf,_,ip) ->
    [ K kf ; B bhv ; A "exits" ; I ip ]
  | IPPredicate (PKEnsures(bhv,Breaks),kf,_,ip) ->
    [ K kf ; B bhv ; A "breaks" ; I ip ]
  | IPPredicate (PKEnsures(bhv,Continues),kf,_,ip) ->
    [ K kf ; B bhv ; A "continues" ; I ip ]
  | IPPredicate (PKEnsures(bhv,Returns),kf,_,ip) ->
    [ K kf ; B bhv ; A "returns" ; I ip ]
  | IPPredicate (PKTerminates,kf,_,ip) ->
    [ K kf ; A "terminates" ; I ip ]

  | IPAllocation(kf,_,Id_contract(_,bhv),_) ->
    [ K kf ; B bhv ; A "allocates" ]
  | IPAllocation(kf,_,Id_loop _,_) ->
    [ K kf ; A "loop_allocates" ]
                             
  | IPAssigns(kf,_,Id_contract(_,bhv),_) ->
    [ K kf ; B bhv ; A "assigns" ]
                             
  | IPAssigns(kf,_,Id_loop _,_) ->
    [ K kf ; A "loop_assigns" ]

  | IPFrom(kf,_,Id_contract(_,bhv),_) ->
    [ K kf ; B bhv ; A "assigns_from" ]
                             
  | IPFrom(kf,_,Id_loop _,_) ->
    [ K kf ; A "loop_assigns_from" ]

  | IPDecrease (kf,_,None,_) ->
    [ K kf ; A "variant" ]

  | IPDecrease (kf,_,Some _,_) ->
    [ K kf ; A "loop_variant" ]

  | IPCodeAnnot (_,_, { annot_content = AStmtSpec _ } ) -> []
  | IPCodeAnnot (_,_, { annot_content = APragma _ | AExtended _ } ) -> []
  | IPCodeAnnot (kf,_, { annot_content = AAssert(_,p) } ) ->
    [K kf ; A "assert" ; P p ]
  | IPCodeAnnot (kf,_, { annot_content = AInvariant(_,true,p) } ) ->
    [K kf ; A "loop_invariant" ; P p ]
  | IPCodeAnnot (kf,_, { annot_content = AInvariant(_,false,p) } ) ->
    [K kf ; A "invariant" ; P p ]
  | IPCodeAnnot (kf,_, { annot_content = AVariant(e,_) } ) ->
    [K kf ; A "loop_variant" ; T e ]
  | IPCodeAnnot (kf,_, { annot_content = AAssigns _ } ) ->
    [K kf ; A "loop_assigns" ]
  | IPCodeAnnot (kf,_, { annot_content = AAllocation _ } ) ->
    [K kf ; A "loop_allocates" ]
    
  | IPComplete (kf,_,_,cs) ->
    (K kf :: A "complete" :: List.map (fun a -> A a) cs)
  | IPDisjoint(kf,_,_,cs) ->
    (K kf :: A "disjoint" :: List.map (fun a -> A a) cs)

  | IPReachable (None, _, _) -> []
  | IPReachable (Some kf,Kglobal,Before) ->
    [ K kf ; A "reachable" ]
  | IPReachable (Some kf,Kglobal,After) ->
    [ K kf ; A "reachable_post" ]
  | IPReachable (Some kf,Kstmt s,Before) ->
    [ K kf ; S s ; A "reachable" ]
  | IPReachable (Some kf,Kstmt s,After) ->
    [ K kf ; S s ; A "reachable_after" ]
    
  | IPAxiomatic _
  | IPAxiom _ -> []
  | IPLemma(name,_,_,_,_) ->
    [ A "lemma" ; A name ]

  | IPTypeInvariant(name,_,_,_)
  | IPGlobalInvariant(name,_,_) ->
    [ A "invariant" ; A name]
    
  | IPOther(name,Some kf,_) ->
    [ K kf ; A name ]

  | IPOther(name,None,_) ->
    [ A name ]

  | IPPropertyInstance (_, _, _, ip) ->
    parts_of_property ip

  | IPExtended(kf,Kglobal,(_,name,_)) -> [ K kf ; A name ]
  | IPExtended(kf,Kstmt s,(_,name,_)) -> [ K kf ; S s ; A name ]

(**************************************************************************)
