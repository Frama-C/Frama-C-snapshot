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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

open Jc_env
open Jc_fenv
open Jc_ast
open Jc_pervasives
open Jc_iterators

let rec term acc t =
  fold_term 
    (fun acc t -> match t#node with
    | JCTapp app -> app.jc_app_fun::acc
    | _ -> acc
    ) acc t

let tag acc t = match t#node with
  | JCTtag _ | JCTbottom -> acc
  | JCTtypeof(t, _) -> term acc t

let rec assertion acc p =
  match p#node with
  | JCAtrue 
  | JCAfalse -> acc
  | JCArelation(t1,op,t2) ->
      term (term acc t1) t2
  | JCAapp app -> app.jc_app_fun :: (List.fold_left term acc app.jc_app_args)
  | JCAand(pl) | JCAor(pl) -> List.fold_left assertion acc pl
  | JCAimplies (p1,p2) | JCAiff(p1,p2) -> 
      assertion (assertion acc p1) p2
  | JCAif(t1,p2,p3) -> 
      assertion (assertion (term acc t1) p2) p3
  | JCAnot p | JCAold p | JCAat(p,_) | JCAquantifier (_,_,p) -> 
      assertion acc p
  | JCAinstanceof(t,_,_)
  | JCAmutable(t,_,_)
  | JCAbool_term t -> term acc t
  | JCAtagequality(t1, t2, _) ->
      tag (tag acc t1) t2
  | JCAmatch(t, pal) ->
      term (List.fold_left (fun acc (_, a) -> assertion acc a) acc pal) t

(*
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
    | Some l -> List.fold_left (fun acc t -> (term t) @acc)  [] l
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
*)

let loop_annot acc la = 
  let acc = assertion acc la.jc_loop_invariant in
  match la.jc_loop_variant with
  | None -> acc
  | Some t -> term acc t

let expr = 
  IExpr.fold_left 
    (fun acc e -> match e#node with  
       | JCEapp call ->
	   let f = call.jc_call_fun in
	   let (a,b)=acc in
	   begin match f with
	     | JCfun f -> (a,f::b)
	     | JClogic_fun f -> (f::a,b)
	   end
       | JCEloop(spec,s) ->
	   let (a,b) = acc in (loop_annot a spec,b)
       | _ -> acc
    )

let compute_logic_calls f t = 
  let calls =
    match t with
      | JCTerm t -> term [] t 
      | JCAssertion a -> assertion [] a 
      | JCReads r -> []
  in
  f.jc_logic_info_calls <- calls

let compute_calls f s b = 
  let (a,b) = expr ([],[]) b in
  f.jc_fun_info_calls <- b
      
module LogicCallGraph = struct 
  type t = (int, (logic_info * term_or_assertion)) Hashtbl.t 
  module V = struct
    type t = logic_info
    let compare f1 f2 = Pervasives.compare f1.jc_logic_info_tag f2.jc_logic_info_tag
    let hash f = f.jc_logic_info_tag
    let equal f1 f2 = f1 == f2
  end
  let iter_vertex iter =
    Hashtbl.iter (fun _ (f,a) -> iter f) 
  let iter_succ iter _ f =
    List.iter iter f.jc_logic_info_calls 
  end

module LogicCallComponents = Graph.Components.Make(LogicCallGraph)

module CallGraph = struct 
  type t = (int, (fun_info * Loc.position * fun_spec * expr option)) Hashtbl.t
  module V = struct
    type t = fun_info
    let compare f1 f2 = Pervasives.compare f1.jc_fun_info_tag f2.jc_fun_info_tag
    let hash f = f.jc_fun_info_tag
    let equal f1 f2 = f1 == f2
  end
  let iter_vertex iter =
    Hashtbl.iter (fun _ (f,loc,spec,b) -> iter f) 
  let iter_succ iter _ f =
    List.iter iter f.jc_fun_info_calls 
  end

module CallComponents = Graph.Components.Make(CallGraph)

open Format
open Pp

let compute_logic_components ltable =  
  let tab_comp = LogicCallComponents.scc_array ltable in
  Jc_options.lprintf "***********************************\n";
  Jc_options.lprintf "Logic call graph: has %d components\n" 
    (Array.length tab_comp);
  Jc_options.lprintf "***********************************\n";
  Array.iteri 
    (fun i l -> 
       Jc_options.lprintf "Component %d:\n%a@." i
	 (print_list newline 
	    (fun fmt f -> fprintf fmt " %s calls: %a\n" f.jc_logic_info_name
		 (print_list comma 
		    (fun fmt f -> fprintf fmt "%s" f.jc_logic_info_name))
		 f.jc_logic_info_calls))
	 l)
    tab_comp;
  tab_comp


let compute_components table = 
  (* see above *)
  let tab_comp = CallComponents.scc_array table in
  Jc_options.lprintf "******************************\n";
  Jc_options.lprintf "Call graph: has %d components\n" (Array.length tab_comp);
  Jc_options.lprintf "******************************\n";
  Array.iteri 
    (fun i l -> 
      Jc_options.lprintf "Component %d:\n%a@." i
	(print_list newline 
	   (fun fmt f -> fprintf fmt " %s calls: %a\n" f.jc_fun_info_name
	       (print_list comma 
		  (fun fmt f -> fprintf fmt "%s" f.jc_fun_info_name))
	       f.jc_fun_info_calls))
	l)
    tab_comp;
  tab_comp
    
    


(*
Local Variables: 
compile-command: "LC_ALL=C make -C .. bin/jessie.byte"
End: 
*)
