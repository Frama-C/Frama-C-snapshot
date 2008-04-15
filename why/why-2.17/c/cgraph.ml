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

open Clogic
open Cast

let rec term t =
  match t.term_node with
    | Tconstant _
    | Tstring_literal _ 
    | Tvar _ 
    | Tminint _ 
    | Tmaxint _ ->  []
    | Tapp (f,lt) -> f::(List.fold_left (fun acc x -> term x@acc) []  lt)
    | Tcast (_,t)| Tblock_length t | Tarrlen t | Tstrlen t
    | Toffset t| Tbase_addr t| Tat (t,_)| Told t 
    | Tdot (t,_)  | Tarrow (t,_) | Tunop (_,t) -> term t
    | Tarrget (t1,t2) | Tbinop (t1,_,t2) | Tmin (t1,t2) | Tmax (t1,t2) ->
	(term t1)@(term t2)
    | Tif (t1,t2,t3) -> (term t1)@(term t2)@(term t3)
    | Trange (t,ot1,ot2) -> 
	(term t) @ (begin match ot1,ot2 with
		      | None, None -> []
		      | Some t , None -> term t
		      | None, Some t -> term t
		      | Some t1,Some t2 -> (term t1)@(term t2)
		    end)


let rec predicate p =
  match p.pred_node with
  | Pfalse
  | Ptrue -> []
  | Papp (l,lt) -> l::(List.fold_left (fun acc t -> (term t)@acc) []  lt)
  | Pvalid_index  (t1,t2) | Pfullseparated (t1,t2) | Pseparated (t1,t2) 
  | Pfull_separated (t1,t2) | Prel (t1,_,t2) -> (term t1) @(term t2)
  | Pbound_separated (t1,t2,t3,t4) ->
      (term t1) @(term t2) @(term t3) @(term t4)
  | Pand (p1,p2) | Por (p1,p2)  | Pimplies (p1,p2)| Piff (p1,p2) -> 
      (predicate p1) @(predicate p2)
  | Pold p | Pat (p,_) | Pforall (_,p) | Pexists (_,p)  | Pnamed (_,p)  
  | Pnot p -> predicate p
  | Pif (t,p1,p2) -> (term t) @(predicate p1) @(predicate p2)
  | Pfresh t | Pvalid t -> term t
  | Pvalid_range (t1,t2,t3) -> (term t1) @(term t2) @(term t3)

let spec s = 
  begin
  match s.requires with
    | None -> []
    | Some p -> 
	predicate p
  end @
  begin
  match s.assigns with
    | None -> []
    | Some (_, l) -> List.fold_left (fun acc t -> (term t) @acc)  [] l
  end @
  begin
    match s.ensures with
      | None -> []
      | Some p -> predicate p
  end @
  begin
    match s.decreases with
      | None -> []
      | Some (t,_) -> term t
  end

let loop_annot la =
  begin
    match la.invariant with
    | None -> []
    | Some p -> predicate p
  end @
  begin
  match la.loop_assigns with
    | None -> []
    | Some (_,l) -> List.fold_left (fun acc t -> (term t) @acc)  [] l
  end @
  begin
    match la.variant with
      | None -> []
      | Some (t,_) -> term t
  end 

let rec expr e =
  match e.texpr_node with 
    | TEnop -> []
    | TEconstant _ -> []
    | TEstring_literal _ -> []
    | TEvar (Info.Fun_info f ) -> [f]    
    | TEvar (Info.Var_info _f ) -> []
    | TEdot (e, _) -> expr e
    | TEarrow (e, _) -> expr e
    | TEarrget (e1, e2) -> (expr e1)@(expr e2)
    | TEseq (e1, e2) -> (expr e1)@(expr e2)
    | TEassign (e1, e2) -> (expr e1)@(expr e2)
    | TEassign_op(e1, _, e2) -> (expr e1)@(expr e2)
    | TEunary (_, e) -> expr e
    | TEincr (_, e) -> expr e
    | TEbinary (e1, _, e2) -> (expr e1)@(expr e2)
    | TEcall (e, le) -> (expr e)@
	(List.fold_left (fun acc x -> (expr x)@acc) [] le)
    | TEcond (e1, e2 ,e3) -> (expr e1)@(expr e2)@(expr e3)
    | TEcast (_, e) | TEmalloc (_, e) -> expr e
    | TEsizeof _ -> []

let rec statement s = 
  match s.st_node with  
    | TSnop -> [],[]
    | TSexpr e -> expr e,[]
    | TSif (e, s1, s2) ->
	let (a1,b1) = statement s1 in	
	let (a2,b2) = statement s2 in
	(expr e)@a1@a2,b1@b2  
    | TSwhile (sp,e,s)     | TSdowhile (sp,s,e) -> 
	let (a,b) = statement s in
	(expr e)@a,b@(loop_annot sp)
    | TSfor (sp, e1, e2, e3, s) ->
 	let (a,b) = statement s in
	(expr e1)@(expr e2)@(expr e3)@a,b@(loop_annot sp)
    | TSblock (_, sl) -> 
	List.fold_left 
	  (fun (acc1,acc2) x -> 
	     let (a,b) = statement x in
	     a@acc1,b@acc2) ([],[]) sl 
    | TSreturn (Some e) -> (expr e),[]
    | TSreturn None | TSbreak | TScontinue -> [],[]
    | TSlabel (_, s) -> statement s
    | TSswitch (e, s)     | TScase  (e, s)-> 
 	let (a,b) = statement s in
	(expr e)@a,b
    | TSdefault s -> statement s
    | TSgoto _ -> [] ,[]
    | TSassert _ | TSassume _ -> [],[]
    | TSlogic_label _ -> [],[]
    | TSspec (sp, s) -> 
	let (a,b) = statement s in
	a,(spec sp) @b
    | TSset _ -> [],[]


let make_graph e = 
  match e with    
  | Tfundef (_s, _t, f, st) -> 
      let (a,_b) = statement st in
      f.Info.graph <- a;
      (*f.Info.logic_calls <- (spec s)@b *)
  | _ -> ()

      
let file  = List.iter (fun d -> make_graph d.node) 

open Info




module G = struct 
  type t = (string*Cast.tfile) list
  module V = struct
    type t = fun_info
    let compare f1 f2 = Pervasives.compare f1.fun_tag f2.fun_tag
    let hash f = f.fun_tag 
    let equal f1 f2 = f1.fun_tag == f2.fun_tag
  end
  let iter_vertex iter _files =
    (*let iter_fun  e = 
      match e with    
	| Tfundef (s, t, f, _)  -> iter f
	| _ -> () in*)
      Cenv.iter_sym 
	(fun _n x -> match x with
	   | Var_info _ -> () 
	   | Fun_info f -> iter f) 
    (*List.iter (fun (_,file) ->List.iter (fun d -> iter_fun d.node) file) files*)
  let iter_succ iter _ f =
    List.iter iter f.graph 
  end

module SCC = Graph.Components.Make(G)

let find_comp tfiles =
  let tab_comp = SCC.scc_array tfiles in
  Coptions.lprintf "Call graph by components: @.";
  Array.iteri (fun i l -> Coptions.lprintf "%d: " i;
		 List.iter (fun f -> Coptions.lprintf "%s " f.fun_name) l;
		 Coptions.lprintf "@.")
    tab_comp;
  tab_comp


(*
Local Variables: 
compile-command: "make -j -C .. bin/caduceus.byte"
End: 
*)
