(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
open Definitions

module L = Qed.Logic

(* -------------------------------------------------------------------------- *)
(* --- Browser                                                           ---- *)
(* -------------------------------------------------------------------------- *)

let named l = {
  vid = l.l_name ;
  title = l.l_name ;
  descr = Pretty_utils.to_string F.pp_pred l.l_lemma ;
  value = l ;
}

class browser ?on_cluster f cluster =
  object(self)

    (* Base Visitor *)
    inherit Definitions.visitor cluster
    method section _ = ()
    method on_library _ = ()
    method on_type _ _ = ()
    method on_comp _ _ = ()
    method on_dfun _ = ()
    method! vtypes = ()
    method! vsymbols = ()
    method! vself = self#vlemmas

    (* Cluster & Lemmas *)

    method on_cluster c =
      match on_cluster with
      | None -> (new browser ~on_cluster:self#vcluster f c)#vlemmas
      | Some visitor -> visitor c

    method on_dlemma l = f (named l)
  end

let browse f s =
  if WpContext.is_defined () then
    begin
      let main = Definitions.cluster ~id:"browser" () in
      let visitor = new browser f main in
      let selection = Tactical.selected s in
      visitor#vterm selection
    end

(* -------------------------------------------------------------------------- *)
(* --- Search Lemma Tactical                                              --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  feedback : Tactical.feedback ;
  lemma : F.pred ;
  descr : string ;
}

type lemma = Definitions.dlemma Tactical.named

let find thm =
  try Some (named (Definitions.find_name thm))
  with Not_found -> None

let search,psearch =
  Tactical.search ~id:"lemma" ~title:"Lemma" ~descr:"Lemma to Instantiate"
    ~browse ~find:Definitions.find_name ()


let fresh pool { l_forall ; l_lemma } =
  let vars = List.map (F.alpha pool) l_forall in
  let sigma = Lang.subst l_forall (List.map F.e_var vars) in
  vars , F.p_subst sigma l_lemma

class instance =
  object(self)
    inherit Tactical.make ~id:"Wp.lemma"
        ~title:"Lemma"
        ~descr:"Search & Instantiate Lemma"
        ~params:(psearch :: TacInstance.params)

    method private hide (feedback : Tactical.feedback) fields =
      List.iter
        (fun fd -> feedback#update_field ~enabled:false fd)
        fields

    method private wrap env vars fields =
      match vars , fields with
      | x::xs , fd::fields ->
          let title = Pretty_utils.to_string F.pp_var x in
          let value = self#get_field fd in
          let tau = F.tau_of_var x in
          env.feedback#update_field ~enabled:true
            ~title ~tooltip:env.descr
            ~range:(match tau with L.Int -> true | _ -> false)
            ~filter:(TacInstance.filter tau) fd ;
          let bindings,lemma = self#wrap env xs fields in
          (x,value)::bindings , lemma
      | _ ->
          self#hide env.feedback fields ;
          [] , F.p_forall vars env.lemma

    method select feedback = function
      | Empty -> Not_applicable
      | selection ->
          begin match self#get_field search with
            | None ->
                self#hide feedback TacInstance.fields ;
                Not_configured
            | Some Tactical.{ title ; value = dlem } ->
                let fields = TacInstance.fields in
                let vars,lemma = fresh feedback#pool dlem in
                let descr = Pretty_utils.to_string F.pp_pred lemma in
                let bindings,lemma =
                  self#wrap { feedback ; descr ; lemma } vars fields in
                match TacInstance.cardinal 1000 bindings with
                | Some n ->
                    if n > 1 then
                      feedback#set_descr "Generates %d instances" n ;
                    let at = Tactical.at selection in
                    Applicable
                      (TacInstance.instance_have ~title ?at bindings lemma)
                | None ->
                    feedback#set_error "More than 1,000 instances" ;
                    Not_configured
          end
  end

let tactical = Tactical.export (new instance)

let strategy ?(priority=1.0) ?(at = Tactical.int 0) lemma values =
  Strategy.{
    priority ; tactical ; selection = at ;
    arguments =
      arg search (find lemma) ::
      TacInstance.wrap TacInstance.fields values ;
  }

(* -------------------------------------------------------------------------- *)
