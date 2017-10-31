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

module L = Qed.Logic

(* -------------------------------------------------------------------------- *)
(* --- Instance Tactical                                                  --- *)
(* -------------------------------------------------------------------------- *)

let descr = function
  | 1 -> "First Parameter"
  | 2 -> "Second Parameter"
  | 3 -> "Third Parameter"
  | n -> Printf.sprintf "%d-th Parameter" n

let mkparam k =
  Tactical.composer
    ~id:(Printf.sprintf "P%d" k)
    ~title:(Printf.sprintf "#%d" k)
    ~descr:(descr k) ()

let fields,params = List.split (List.map mkparam [1;2;3;4;5;6;7;8;9;10])

let rec has_binder q p =
  match F.repr p with
  | L.Bind(q0,_,_) -> q = q0
  | L.Imply(_,p) -> has_binder q p
  | _ -> false

type bindings = (F.var * selection) list
type env = {
  binder : L.binder ;
  feedback : Tactical.feedback ;
  pool : Lang.F.pool ;
  mutable index : int ;
}

let rec complexity = function
  | [] -> Integer.one
  | (_,v) :: bindings ->
      match v with
      | Tactical.Compose(Tactical.Range(a,b)) when a < b ->
          let n = Integer.of_int (b+1-a) in
          Integer.mul n (complexity bindings)
      | _ -> complexity bindings

let cardinal limit bindings =
  let n = complexity bindings in
  if Integer.le n (Integer.of_int limit)
  then Some (Integer.to_int n) else None

let rec bind_exists bindings property =
  match bindings with
  | [] -> property
  | (x,v) :: bindings ->
      let closed =
        if Tactical.is_empty v then
          Lang.F.p_bind L.Exists x property
        else
          let value = Tactical.selected v in
          Lang.F.p_apply x value property
      in bind_exists bindings closed

let rec range x a b w =
  if a <= b then
    ( Printf.sprintf "%s-%d" (fst w) a , F.p_apply x (F.e_int a) (snd w) )
    :: range x (succ a) b w
  else []

let rec bind_ranges pool = function
  | [] -> pool
  | (x,a,b) :: ranges ->
      bind_ranges (List.concat (List.map (range x a b) pool)) ranges

let rec bind_forall ranges bindings property =
  match bindings with
  | (x,v) :: bindings ->
      begin
        match v with
        | Tactical.Compose(Tactical.Range(a,b)) when a < b ->
            bind_forall ((x,a,b)::ranges) bindings property
        | Tactical.Empty ->
            bind_forall ranges bindings (Lang.F.p_bind L.Forall x property)
        | _ ->
            let value = Tactical.selected v in
            bind_forall ranges bindings (Lang.F.p_apply x value property)
      end
  | [] ->
      bind_ranges [ "Instance" , property ] ranges

let instance_goal ?(title="Witness") bindings property sequent =
  [ title, (fst sequent , bind_exists bindings property) ]

let instance_have ?(title="Instance") ?at bindings property sequent =
  let clauses = List.map
      (fun (descr,p) -> Conditions.(step ~descr (Have p)))
      (bind_forall [] bindings property) in
  let step = match clauses with
    | [single] -> single
    | _ -> Conditions.(step (Either [sequence clauses]))
  in [ title , Conditions.insert ?at step sequent ]

let bind ~side bindings property : Tactical.process =
  match side with
  | None ->
      instance_goal ~title:"Witness" bindings property
  | Some s ->
      let open Conditions in
      instance_have ?title:s.descr ~at:s.id bindings property

let filter x e =
  try F.Tau.equal (F.tau_of_var x) (F.typeof e)
  with Not_found -> true (* allowed to not restrict usage *)

let fieldname k x =
  Pretty_utils.sfprintf "%s (%a)" (descr k) F.Tau.pretty (F.tau_of_var x)

class instance =
  object(self)
    inherit Tactical.make ~id:"Wp.instance"
        ~title:"Instance"
        ~descr:"Instantiate properties"
        ~params

    method private wrap env lemma fields =
      match F.repr lemma , fields with
      | L.Imply(hs,p) , _ when env.binder = L.Forall &&
                               has_binder env.binder p ->
          let bindings,property = self#wrap env p fields in
          bindings, F.e_imply hs property
      | L.Bind(q,tau,phi) , fd :: fields when q = env.binder ->
          let x = F.fresh env.pool tau in
          let v = self#get_field fd in
          env.index <- succ env.index ;
          env.feedback#update_field
            ~tooltip:(fieldname env.index x)
            ~enabled:true
            ~range:(match tau with L.Int -> true | _ -> false)
            ~filter:(filter x) fd ;
          let lemma = F.QED.lc_open x phi in
          let bindings,property = self#wrap env lemma fields in
          (x,v) :: bindings , property
      | _ ->
          List.iter (env.feedback#update_field ~enabled:false) fields ;
          [] , lemma

    method private configure ~side feedback p =
      let binder = match side with None -> L.Exists | Some _ -> L.Forall in
      let lemma = F.e_prop p in
      if has_binder binder lemma then
        let vars = F.vars lemma in
        let pool = Lang.new_pool ~vars () in
        let env = { index = 0 ; feedback ; binder ; pool } in
        let bindings,phi = self#wrap env lemma fields in
        if List.exists (fun (_,v) -> not (Tactical.is_empty v)) bindings
        then
          match cardinal 1000 bindings with
          | Some n ->
              if n > 1 then
                feedback#set_descr "Generates %d instances" n ;
              Applicable (bind ~side bindings (F.p_bool phi))
          | None ->
              feedback#set_error "More than 1,000 instances" ;
              Not_configured
        else Not_configured
      else
        Not_applicable

    method select feedback sel =
      match sel with
      | Inside(Step s,t) when F.is_prop t ->
          let hs = Conditions.have s in
          let p = F.p_bool t in
          begin match F.p_expr hs with
            | L.And ps when List.memq p ps ->
                self#configure ~side:(Some s) feedback p
            | _ -> Not_applicable
          end
      | Clause(Step s) ->
          let open Conditions in
          begin match s.condition with
            | Have p | When p | Init p | Core p ->
                self#configure ~side:(Some s) feedback p
            | _ -> Not_applicable
          end
      | Clause(Goal p) ->
          self#configure ~side:None feedback p
      | _ -> Not_applicable

  end

let tactical = Tactical.export (new instance)

let rec wrap fs vs =
  match fs , vs with
  | f :: fs , v :: vs ->
      Strategy.arg f v :: (wrap fs vs)
  | fs , _ ->
      List.map (fun f -> Strategy.arg f Empty) fs

let strategy ?(priority=1.0) selection values =
  Strategy.{
    priority ; tactical ; selection ;
    arguments = wrap fields values ;
  }
