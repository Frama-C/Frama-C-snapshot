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

open Cil_types
open Lang
open Lang.F
open Memory

(* -------------------------------------------------------------------------- *)
(* --- State Registry                                                     --- *)
(* -------------------------------------------------------------------------- *)

type label = {
  id : int ; (* index in the sequent, unique *)
  name : string ; (* (almost) unique *)
  stmt : Cil_types.stmt option ; (* if defined in the sequent *)
  state : Mstate.state ;
  mutable flag : bool ;
  mutable prev : label list ;
  mutable next : label list ;
}

type value =
  | Term
  | Addr of Memory.lval
  | Lval of Memory.lval * label
  | Chunk of string * label

module Imap = Datatype.Int.Map

type env = {
  mutable kid : int ; (* counter for anonymous labels *)
  mutable cfg : label list ; (* sorted by dominators *)
  mutable values : value Tmap.t ; (* cache *)
  mutable labels : label Imap.t ;
}

let label env ~id ?stmt ?descr state =
  let name =
    let open Cil_types in
    match descr , stmt with
    | Some lbl , _ -> lbl
    | None , Some { labels = Label(lbl,_,_) :: _ } -> lbl
    | _ -> env.kid <- succ env.kid ; Printf.sprintf "L%d" env.kid
  in
  { id ; stmt ; name ; state ; flag = false ; prev = [] ; next = [] }

let insert env label =
  begin
    env.labels <- Imap.add label.id label env.labels ;
    env.cfg <- label :: env.cfg ;
  end
  
let create () = {
  kid = 0 ; cfg = [] ;
  values = Tmap.empty ;
  labels = Imap.empty ;
}

let at env ~id = Imap.find id env.labels
let flag lbl = lbl.flag <- true ; lbl
let visible lbl = lbl.flag

let rec find env e =
  try Tmap.find e env.values
  with Not_found ->
    env.values <- Tmap.add e Term env.values ;
    if F.is_primitive e then Term else
      let v = lookup env e env.cfg in
      env.values <- Tmap.add e v env.values ; v

and lookup env e = function
  | [] -> Term
  | lbl :: others ->
      try match Mstate.lookup lbl.state e with
        | Memory.Mterm -> raise Not_found
        | Memory.Maddr lv -> Addr lv
        | Memory.Mlval lv -> Lval(lv,flag lbl)
        | Memory.Mchunk m -> Chunk(m,flag lbl)
      with Not_found -> lookup env e others

let is_ref x k = (k == F.e_zero) && Cil.isPointerType x.vtype
let is_atomic = function
  | Mvar x , [Mindex k] -> is_ref x k
  | Mvar _ , [] -> true
  | _ -> false

let iter f lbl = Mstate.iter f lbl.state

let is_copy env lbl = function
  | Memory.Mstore( lv , value ) ->
      begin
        match find env value with
        | Lval(lv0,lbl0) -> lbl0 == lbl && Mstate.equal lv lv0
        | _ -> false
      end
    
let updates env seq vars =
  Bag.filter
    (fun upd -> not (is_copy env seq.pre upd))
    (Mstate.updates { pre = seq.pre.state ; post = seq.post.state } vars)

(* -------------------------------------------------------------------------- *)
(* --- Label Control Flow                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prev lbl = lbl.prev
let next lbl = lbl.next
let branching = function { next = [_] } -> false | _ -> true

let sequence_point a b =
  if a != b then
    match a,b with
    | Some p , Some q ->
        if not (List.memq q p.next) then p.next <- q :: p.next ;
        if not (List.memq p q.prev) then q.prev <- p :: q.prev ;
    | None , _ | _ , None -> ()
      
let rec control env prev sequence next =
  ignore (ctrl env prev (Conditions.list sequence) next)
    
and ctrl env prev steps next = match steps with
  | [] -> next
  | s :: others -> 
      let open Conditions in
      match s.condition with
      | Type _ | Have _ | When _ | Core _ | Init _ ->
          (* Sequence of Labels on Hyp *)
          ctrl env prev others next
      | Branch(_,cthen,celse) ->
          let next = ctrl env None others next in
          control env prev cthen next ;
          control env prev celse next ;
          None
      | Either cases ->
          let next = ctrl env None others next in
          List.iter (fun s -> control env prev s next) cases ;
          None
      | State _ ->
          try
            let here = Some (at env ~id:s.id) in
            sequence_point prev here ;
            let next = ctrl env here others next in
            sequence_point here next ;
            here
          with Not_found ->
            ctrl env prev others next

(* -------------------------------------------------------------------------- *)
(* --- Priority Queue                                                     --- *)
(* -------------------------------------------------------------------------- *)

let register seq =
  ignore (Conditions.steps seq) ;
  let env = create () in
  let queue = Queue.create () in
  let push s = Queue.add s queue in
  let pop () = try Some (Queue.pop queue) with Queue.Empty -> None in
  let api = ref [] in (* Pre & Post *)
  let cfg = ref [] in (* Other labels *)
  let pool = function Some ("Pre"|"Post") -> api | _ -> cfg in
  let rec compile seq =
    Conditions.iter
      (fun s ->
         let open Conditions in
         match s with
         | { id ; stmt ; descr ; condition = State m } ->
             let label = label env ~id ?stmt ?descr m in
             let r = pool descr in r := label :: !r
         | { condition = Type _ | Have _ | When _ | Core _ | Init _ } -> ()
         | { condition = Branch(_,cthen,celse) } -> push cthen ; push celse
         | { condition = Either cases } -> List.iter push cases
      ) seq ;
    match pop () with Some s -> compile s | None -> ()
  in compile seq ; 
  let insert = insert env in
  List.iter insert !cfg ; List.iter insert !api ;
  control env None seq None ; env

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printer                                                     --- *)
(* -------------------------------------------------------------------------- *)

class virtual engine =
  object(self)

    method virtual pp_atom : Format.formatter -> F.term -> unit
    method virtual pp_flow : Format.formatter -> F.term -> unit

    (* --- L-Values --- *)

    method is_atomic_lv = is_atomic

    method pp_ofs fmt = function
      | Mfield fd -> Format.fprintf fmt ".%s@," fd.fname
      | Mindex k -> Format.fprintf fmt "[%a]@," self#pp_flow k

    method pp_offset fmt fs = List.iter (self#pp_ofs fmt) fs

    method pp_host fmt = function
      | Mvar x -> Format.pp_print_string fmt x.vname
      | Mmem p -> self#pp_atom fmt p
      | Mval lv -> self#pp_lval fmt lv
    
    method pp_lval fmt = function
      | Mvar x , [] ->
          Format.pp_print_string fmt x.vname
      | Mvar x , [Mindex k] when is_ref x k ->
          Format.fprintf fmt "*%s" x.vname
      | Mvar x , ofs ->
          Format.fprintf fmt "@[<hov 2>%s%a@]" x.vname self#pp_offset ofs
      | host , [] ->
          Format.fprintf fmt "*%a" self#pp_host host
      | host , Mfield fd :: ofs ->
          Format.fprintf fmt "@[<hov 2>%a@,->%s%a@]"
            self#pp_host host fd.fname self#pp_offset ofs
      | host , ((Mindex _ :: _) as ofs) ->
          Format.fprintf fmt "@[<hov 2>%a@,%a@]"
            self#pp_host host self#pp_offset ofs

    method pp_addr fmt = function
      | Mvar x , [] -> Format.fprintf fmt "&%s" x.vname
      | Mmem p , [] -> self#pp_atom fmt p
      | Mmem p , [Mindex k] ->
          Format.fprintf fmt "%a + %a" self#pp_atom p self#pp_atom k
      | lv -> Format.fprintf fmt "&(%a)" self#pp_lval lv

    method pp_label fmt lbl =
      Format.pp_print_string fmt lbl.name

    method pp_chunk fmt m = Format.fprintf fmt "µ:%s" m

  end

open Memory

let rec lv_iter f (h,ofs) = host_iter f h ; List.iter (ofs_iter f) ofs
and host_iter f = function Mvar _ -> () | Mmem e -> f e | Mval lv -> lv_iter f lv
and ofs_iter f = function Mfield _ -> () | Mindex e -> f e

let subterms env f e =
  match find env e with
  | Term -> false
  | Chunk _ -> true
  | Addr lv | Lval(lv,_) -> lv_iter f lv ; true
