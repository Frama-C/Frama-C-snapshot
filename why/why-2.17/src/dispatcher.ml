(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: dispatcher.ml,v 1.35 2008/11/12 16:31:50 moy Exp $ i*)

open Options
open Vcg
open Logic
open Logic_decl

type elem = Logic_decl.t

let stack = ref []

let add_elem e = stack := e :: !stack

let oblig = Queue.create ()
let oblig_h = Hashtbl.create 97

let add_oblig ((_,_,id,_) as o) =
  let so = (List.rev !stack, o) in
  Queue.add so oblig ;
  Hashtbl.add oblig_h id so

let push_decl d = 
  match d with
  | Dgoal (loc, expl, id, s) -> add_oblig (loc, expl, id, s)
  | d -> add_elem d

let iter f = Queue.iter (fun (_,o) -> f o) oblig

(* calling prover *)

open DpConfig

let push_elem p e = 
  if not pruning then 
    assert (match e with Dgoal _ -> false | _ -> true);  
  match p with
  | Simplify -> Simplify.push_decl e
  | Harvey -> Harvey.push_decl e
  | Cvcl -> Cvcl.push_decl e
  | Zenon -> Zenon.push_decl e
  | Rvsat | Yices | Cvc3 | Z3 -> Smtlib.push_decl e
  | Ergo | ErgoSelect -> Pretty.push_decl ~ergo:true e
  | Graph -> Pretty.push_decl e
  | Coq -> Coq.push_decl e

let push_obligation p (loc, expl, id, s) = 
  let g = Dgoal (loc, expl, id, s) in
  match p with
  | Simplify -> Simplify.push_decl g
  | Harvey -> Harvey.push_decl g
  | Cvcl -> Cvcl.push_decl g
  | Zenon -> Zenon.push_decl g
  | Rvsat | Yices | Cvc3 | Z3 -> Smtlib.push_decl g
  | Ergo | ErgoSelect -> Pretty.push_decl g
  | Graph -> Pretty.push_decl g
  | Coq -> Coq.push_decl g

(* output_file is a CRITICAL SECTION *)
(** @parama elems is the List that stores the theory
    @parama o is the proof obligation
**)

let get_project () =
  match !Options.gui_project with
    | Some p -> p
    | None ->
	failwith "For interactive provers, option --project must be set"
	

let output_file ?encoding p (elems,o) =
  begin match encoding with 
      Some e -> set_types_encoding e 
    | None -> () end;
  begin match p with
    | Simplify -> Simplify.reset () 
    | Harvey -> Harvey.reset () 
    | Cvcl -> Cvcl.prelude_done := false; Cvcl.reset ()
    | Zenon -> Zenon.prelude_done := false; Zenon.reset ()
    | Rvsat | Yices | Cvc3 | Z3 -> Smtlib.reset ()
    | Ergo | ErgoSelect -> Pretty.reset ()
    | Graph -> Pretty.reset ()
    | Coq -> () (* Coq.reset () *)
  end;
  
  if pruning then 
    begin  
      (**stores into the declarationQueue 
	 all the elements of the elems list and th obligation**)  
      let declQ = Queue.create () in
      List.iter (fun p -> Queue.add p declQ) elems ;
      let (loc, expl, id, s) = o in
      let g = Dgoal (loc, expl, id, s) in
      Queue.add g declQ;
      if debug then
	Format.printf "Before the pruning dedicated to the PO: %d @." 
	  (Queue.length declQ);
      (** reduce the theory **)
      let q = Theory_filtering.reduce declQ in 
      if debug then
	Format.printf "After the pruning dedicated to the PO: %d @." 
	  (Queue.length q);
      Queue.iter (push_elem p) q ;
      push_obligation p o 
    end
  else
    begin
      List.iter (push_elem p) elems;
      push_obligation p o
    end;
  let f = 
    match p with
      | Coq -> let (_, _, id, _) = o in id 
      | _ -> Filename.temp_file "gwhy" "" 
  in
  match p with
    | Simplify -> Simplify.output_file f; f ^ "_why.sx"
    | Harvey -> Harvey.output_file f; f ^ "_why.rv"
    | Cvcl -> Cvcl.output_file f; f ^ "_why.cvc"
    | Zenon -> Zenon.output_file f; f ^ "_why.znn"
    | Rvsat | Yices | Cvc3 | Z3 -> Smtlib.output_file f; f ^ "_why.smt"
    | Ergo | ErgoSelect -> Pretty.output_file f; f ^ "_why.why"
    | Graph -> Pretty.output_file f; f ^ "_why.why"
    | Coq ->
	let _p = get_project () in 
	(* if necessary, Pretty.output_project "name ?"; *)
	(* file should already be generated ?? *)
	(* Coq.output_file f; *)
	if debug then Format.printf "Reusing coq file %s_why.v@." f;
	f ^ "_why.v" 


	

open Format

let prover_name p = 
  try
    let (info,_) = List.assoc p DpConfig.prover_list in
    info.name
  with
      Not_found -> "(unnamed)"
(*
  | Simplify -> "Simplify" 
  | Harvey -> "haRVey"
  | Cvcl -> "CVC Lite"
  | Zenon -> "Zenon"
  | Rvsat -> "rv-sat"
  | Yices -> "Yices"
  | Ergo -> 
      DpConfig.alt_ergo.DpConfig.name ^ " " ^ 
      DpConfig.alt_ergo.DpConfig.version
  | Cvc3 -> "CVC3"
  | Graph -> "Graph"
  | Z3 -> "Z3"
  | Coq -> DpConfig.coq.DpConfig.name
*)

let call_prover ?(debug=false) ?timeout ?encoding ~obligation:o p =
  let so = try Hashtbl.find oblig_h o with Not_found -> assert false in
  let filename = output_file ?encoding p so in
  if debug then eprintf "calling %s on %s@." (prover_name p) filename;
  let r = match p with
    | Simplify -> 
	Calldp.simplify ~debug ?timeout ~filename () 
    | Harvey -> 
	Calldp.harvey ~debug ?timeout ~filename ()
    | Cvcl ->
	Calldp.cvcl ~debug ?timeout ~filename ()
    | Zenon -> 
	Calldp.zenon ~debug ?timeout ~filename ()
    | Rvsat -> 
	Calldp.rvsat ~debug ?timeout ~filename ()
    | Yices -> 
	Calldp.yices ~debug ?timeout ~filename ()
    | Ergo ->
	Calldp.ergo ~select_hypotheses:false ~debug ?timeout ~filename ()
    | ErgoSelect ->
	Calldp.ergo ~select_hypotheses:true ~debug ?timeout ~filename ()
    | Cvc3 -> 
	Calldp.cvc3 ~debug ?timeout ~filename ()
    | Z3 -> 
	Calldp.z3 ~debug ?timeout ~filename ()
    | Graph -> 
	Calldp.graph  ~debug ?timeout ~filename ()
    | Coq ->
	Calldp.coq ~debug ?timeout ~filename ()
  in
  if not debug then begin 
    match p with
      | Coq -> ()
      | _ -> (try Sys.remove filename with _ -> () )
  end; 
  r

