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
open Tactical
open Conditions

(* -------------------------------------------------------------------------- *)
(* --- Cut Tactical                                                       --- *)
(* -------------------------------------------------------------------------- *)

let fclause,pclause =
  Tactical.composer
    ~id:"clause"
    ~title:"Clause"
    ~descr:"Clause to Cut with"
    ~filter:F.is_prop
    ()

type mode = CASES | MODUS

let fmode,pmode =
  Tactical.selector
    ~id:"case"
    ~title:"Mode"
    ~descr:"Select How the Clause is Used"
    ~default:MODUS
    ~options:Tactical.[
        { title="Case Analysis" ; descr="" ; vid="CASES" ; value=CASES } ;
        { title="Modus Ponens" ; descr="" ; vid="MODUS" ; value=MODUS } ;
      ] ()

class cut =
  object(self)
    inherit Tactical.make ~id:"Wp.cut"
        ~title:"Cut"
        ~descr:"Use Intermerdiate Hypothesis"
        ~params:[pmode;pclause]

    method select feedback sel =
      let mode =
        match sel with
        | Clause(Goal p) when p != F.p_false ->
            feedback#update_field ~enabled:false fmode ; CASES
        | _ ->
            feedback#update_field ~enabled:true fmode ;
            self#get_field fmode in
      let cut = self#get_field fclause in
      if Tactical.is_empty cut then
        Not_configured
      else
        match mode with
        | MODUS ->
            feedback#set_descr "Prove then Insert the Clause" ;
            let clause = F.p_bool (Tactical.selected cut) in
            let step = Conditions.step ~descr:"Cut" (Have clause) in
            let at = Tactical.at sel in
            Applicable
              begin fun sequent ->
                let assume = Conditions.insert ?at step sequent in
                [ "Clause" , (fst sequent,clause) ;
                  "Assume" , (fst assume,snd sequent) ]
              end
        | CASES ->
            feedback#set_descr "Proof by Case in the Clause" ;
            let positive = F.p_bool (Tactical.selected cut) in
            let negative = F.p_not positive in
            Applicable
              begin fun (hs,goal) ->
                [ "Positive" , (hs,F.p_imply positive goal) ;
                  "Negative" , (hs,F.p_imply negative goal) ]
              end
  end

let tactical = Tactical.export (new cut)

let strategy ?(priority=1.0) ?(modus=true) selection =
  Strategy.{
    priority ;
    tactical ;
    selection ;
    arguments = [ arg fmode (if modus then MODUS else CASES) ] ;
  }
