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

(* $Id: jc_separation.ml,v 1.42 2008/12/09 09:14:18 marche Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast
open Jc_fenv

open Jc_constructors
open Jc_pervasives
open Jc_iterators

open Format
open Pp

let current_logic_component = ref None
let set_current_logic_component comp = current_logic_component := Some comp
let reset_current_logic_component () = current_logic_component := None
let in_current_logic_component f1 = 
  match !current_logic_component with 
    | None -> false 
    | Some comp -> 
	List.exists 
	  (fun f2 -> f1.jc_logic_info_tag == f2.jc_logic_info_tag) comp

let current_component = ref None
let set_current_component comp = current_component := Some comp
let reset_current_component () = current_component := None
let in_current_component f1 = 
  match !current_component with 
    | None -> false 
    | Some comp -> 
	List.exists 
	  (fun f2 -> f1.jc_fun_info_tag == f2.jc_fun_info_tag) comp

let single_term rresult t =
  match t#node with
       | JCTvar vi ->	
	   if vi.jc_var_info_name = "\\result" then 
	     Region.unify rresult vi.jc_var_info_region
       | JCTbinary(t1,(_,`Pointer),t2) | JCTif(_,t1,t2) ->
	   Region.unify t1#region t2#region
       | JCTmatch(_, (_, t1)::rem) ->
	   List.iter
	     (fun (_, t2) -> Region.unify t1#region t2#region)
	     rem
       | JCTmatch(_, []) ->
	   ()
       | JCTapp app ->
	   let li = app.jc_app_fun in
	   let param_regions,result_region =
	     if in_current_logic_component li then
	       (* No generalization here, plain unification *)
	       List.map (fun vi -> vi.jc_var_info_region) 
		 li.jc_logic_info_parameters,
	     li.jc_logic_info_result_region 
	     else
	       (* Apply generalization before unification *)
	       let regions = li.jc_logic_info_param_regions in
	       let assoc = RegionList.duplicate regions in
	       app.jc_app_region_assoc <- assoc;
	       let param_regions = 
		 List.map (fun vi -> 
			     if is_dummy_region vi.jc_var_info_region then dummy_region else
			       try RegionList.assoc vi.jc_var_info_region assoc
			       with Not_found -> assert false)
		   li.jc_logic_info_parameters
	       in
	       let result_region = 
		 try RegionList.assoc li.jc_logic_info_result_region assoc
		 with Not_found -> assert false
	       in
	       param_regions,result_region
	   in
	   let arg_regions = 
	     List.map (fun t -> t#region) app.jc_app_args
	   in
	   Jc_options.lprintf "param:%a@." (print_list comma Region.print) param_regions;
	   Jc_options.lprintf "arg:%a@." (print_list comma Region.print) arg_regions;
	   List.iter2 Region.unify param_regions arg_regions;
	   Jc_options.lprintf "param:%a@." Region.print result_region;
	   Jc_options.lprintf "arg:%a@." Region.print t#region;
	   Region.unify result_region t#region
       | JCTconst _ | JCTrange(None,None) | JCTbinary _ | JCTshift _
       | JCTrange _ | JCTunary _ | JCTderef _ | JCTold _ | JCTat _ 
       | JCToffset _ | JCTbase_block _
       | JCTaddress _ | JCTinstanceof _ | JCTcast _ | JCTbitwise_cast _ 
       | JCTrange_cast _ | JCTreal_cast _ ->
	   ()

let term rresult t = iter_term (single_term rresult) t

let single_assertion rresult a =
  match a#node with
       | JCAapp app -> 
	   let li = app.jc_app_fun in
	   let param_regions =
	     if in_current_logic_component li then
	       (* No generalization here, plain unification *)
	       List.map (fun vi -> vi.jc_var_info_region) 
		 li.jc_logic_info_parameters 
	     else
	       (* Apply generalization before unification *)
	       let regions = li.jc_logic_info_param_regions in
	       let assoc = RegionList.duplicate regions in
	       app.jc_app_region_assoc <- assoc;
	       List.map (fun vi -> 
			   if is_dummy_region vi.jc_var_info_region then dummy_region else
			     try RegionList.assoc vi.jc_var_info_region assoc
			     with Not_found -> assert false)
		 li.jc_logic_info_parameters
	   in
	   let arg_regions = 
	     List.map (fun t -> t#region) app.jc_app_args
	   in
	   Jc_options.lprintf "param:%a@." (print_list comma Region.print) param_regions;
	   Jc_options.lprintf "arg:%a@." (print_list comma Region.print) arg_regions;
	   List.iter2 Region.unify param_regions arg_regions
       | JCAtrue | JCAfalse | JCArelation _  | JCAeqtype _ 
       | JCAinstanceof _ | JCAbool_term _ | JCAmutable _ 
       | JCAand _ | JCAor _ | JCAimplies _ | JCAiff _ | JCAif _ | JCAmatch _
       | JCAnot _ | JCAquantifier _ | JCAold _ | JCAat _ | JCAsubtype _ ->
	   ()

let assertion rresult a = 
  iter_term_and_assertion (single_term rresult) (single_assertion rresult) a

let single_location = ignore

let single_location_set = ignore

let location rresult loc =
  iter_location (single_term rresult) single_location single_location_set loc

let single_expr rresult e = 
  match e#node with
       | JCEbinary(e1,_,e2) | JCEif(_,e1,e2) ->
	   Region.unify e1#region e2#region
       | JCEmatch(_, (_, e1)::rem) ->
	   List.iter
	     (fun (_, e2) -> Region.unify e1#region e2#region)
	     rem
       | JCEmatch(_, []) ->
	   ()
       | JCEconst _ | JCEvar _ | JCEshift _ | JCEunary _
       | JCEderef _ | JCEoffset _ | JCEaddress _ | JCEinstanceof _ | JCEcast _ 
       | JCEbitwise_cast _ | JCEbase_block _
       | JCErange_cast _ | JCEreal_cast _ | JCEalloc _ | JCEfree _ 
       | JCElet(_,None,_) ->
	   ()
       | JCElet(vi,Some e,_) | JCEassign_var(vi,e) ->
	   Region.unify vi.jc_var_info_region e#region
    | JCEassign_heap(e1,fi,e2) ->
	let fr = Region.make_field e1#region fi in
	Region.unify fr e2#region
    | JCEthrow(ei,_) ->
	begin match ei.jc_exception_info_type with None -> () | Some ty ->
	  assert(not(is_pointer_type ty)) (* TODO *)
	end
    | JCEapp call -> 
(*	let f = call.jc_call_fun in*)
	let in_current_comp = match call.jc_call_fun with
	  | JClogic_fun f -> false
	  | JCfun f -> in_current_component f
	in
	let params = match call.jc_call_fun with
	  | JClogic_fun f -> f.jc_logic_info_parameters 
	  | JCfun f -> f.jc_fun_info_parameters
	in
	let rregion = match call.jc_call_fun with
	  | JClogic_fun f -> f.jc_logic_info_result_region 
	  | JCfun f -> f.jc_fun_info_return_region
	in
	let param_regions,result_region =	
	  if in_current_comp then
	    (* No generalization here, plain unification *)
	    List.map (fun vi -> vi.jc_var_info_region) params,
	    rregion
	  else
	    (* Apply generalization before unification *)
	    let regions = match call.jc_call_fun with
	      | JClogic_fun f -> f.jc_logic_info_param_regions 
	      | JCfun f -> f.jc_fun_info_param_regions 
	    in
	    let assoc = RegionList.duplicate regions in
	    call.jc_call_region_assoc <- assoc;
	    let param_regions =
	      List.map (fun vi -> 
			  if is_dummy_region vi.jc_var_info_region then dummy_region else
			    try RegionList.assoc vi.jc_var_info_region assoc
			    with Not_found -> assert false)
		params
	    in
	    let result_region = 
	      if is_dummy_region rregion then dummy_region
	      else
		try RegionList.assoc rregion assoc
		with Not_found -> assert false
	    in
	    param_regions,result_region
	in
	let arg_regions = 
	  List.map (fun e -> e#region) call.jc_call_args
	in
	Jc_options.lprintf "param:%a@." (print_list comma Region.print) param_regions;
	Jc_options.lprintf "arg:%a@." (print_list comma Region.print) arg_regions;
	List.iter2 Region.unify param_regions arg_regions;
	Jc_options.lprintf "param:%a@." Region.print result_region;
	Jc_options.lprintf "arg:%a@." Region.print e#region;
	if e#typ = unit_type then
	  () (* Result of call discarded *)
	else Region.unify result_region e#region
    | JCEreturn(ty,e) ->
	Region.unify rresult e#region
    | JCEassert(_behav,_asrt,a) ->
	()
    | JCEcontract(req,dec,vi_result,behs,e) -> 
	assert false (* TODO *)
    | JCEloop(la,_) -> ()
    | JCEblock _ | JCEtry _ 
    | JCEreturn_void | JCEpack _ | JCEunpack _ -> 
	()

let expr rresult e =
  iter_expr_and_term_and_assertion (single_term rresult)
    (single_assertion rresult) single_location single_location_set 
    (single_expr rresult) e

(* let location rresult loc = *)
(*   fold_location  *)
(*     (fold_unit (term rresult)) (fold_unit ignore) (fold_unit ignore) () loc *)

let axiomatic_decl d =
  match d with
    | Jc_typing.ABaxiom(_,_,_,a) -> assertion dummy_region a 

let axiomatic a =
  try
    let l = Hashtbl.find Jc_typing.axiomatics_table a in
    List.iter axiomatic_decl l.Jc_typing.axiomatics_decls
  with Not_found -> assert false

let logic_function f =
  let (f, ta) = 
    Hashtbl.find Jc_typing.logic_functions_table f.jc_logic_info_tag 
  in
  let rresult = f.jc_logic_info_result_region in
  begin match ta with
    | JCTerm t -> 
	begin 
	  term rresult t;
	  Region.unify rresult t#region 
	end
    | JCAssertion a -> assertion rresult a
    | JCReads r -> List.iter (location rresult) r
    | JCInductive l ->
	List.iter (fun (_,_,a) -> assertion rresult a) l
  end;
  Option_misc.iter axiomatic f.jc_logic_info_axiomatic

let generalize_logic_function f =
  let param_regions =
    List.map (fun vi -> vi.jc_var_info_region) f.jc_logic_info_parameters in
  let fun_regions = f.jc_logic_info_result_region :: param_regions in
  f.jc_logic_info_param_regions <- RegionList.reachable fun_regions

let logic_component fls =
  (* Perform plain unification on component *)
  set_current_logic_component fls;
  List.iter logic_function fls;
  reset_current_logic_component ();
  (* Generalize regions accessed *)
  List.iter generalize_logic_function fls;
  (* Fill in association table at each call site *)
  List.iter logic_function fls

let funspec rresult spec =
  iter_funspec (single_term rresult) (single_assertion rresult) single_location
    single_location_set spec

let code_function f =
  let (f, _, spec, body) = 
    Hashtbl.find Jc_typing.functions_table f.jc_fun_info_tag 
  in
  Jc_options.lprintf "Separation: treating function %s@." f.jc_fun_info_name;
  let rresult = f.jc_fun_info_return_region in
  funspec rresult spec;
  Option_misc.iter (expr rresult) body

let generalize_code_function f =
  let param_regions =
    List.map (fun vi -> vi.jc_var_info_region) f.jc_fun_info_parameters in
  let fun_regions = f.jc_fun_info_return_region :: param_regions in
  f.jc_fun_info_param_regions <- RegionList.reachable fun_regions

let code_component fls =
  (* Perform plain unification on component *)
  set_current_component fls;
  List.iter code_function fls;
  reset_current_component ();
  (* Generalize regions accessed *)
  List.iter generalize_code_function fls;
  (* Fill in association table at each call site *)
  List.iter code_function fls

let axiom id (loc,is_axiom,labels,a) = assertion (* labels *) dummy_region a

let regionalize_assertion a assoc =
  map_term_in_assertion (fun t ->
    let t = match t#node with
      | JCTapp app ->
	  let app_assoc = 
	    List.map (fun (rdist,rloc) -> 
	      try (rdist,RegionList.assoc rloc assoc) with Not_found -> (rdist,rloc)
	    ) app.jc_app_region_assoc
	  in
	  let tnode = JCTapp { app with jc_app_region_assoc = app_assoc; } in
	  new term_with ~node:tnode t
      | JCTconst _ | JCTvar _ | JCTshift _ 
      | JCTderef _ | JCTbinary _ | JCTunary _ | JCTold _ | JCTat _ | JCToffset _
      | JCTaddress _ | JCTbase_block _
      | JCTinstanceof _ | JCTcast _ | JCTbitwise_cast _ | JCTrange_cast _ | JCTreal_cast _ | JCTif _ | JCTmatch _ | JCTrange _ ->
	  t
    in
    try new term_with ~region:(RegionList.assoc t#region assoc) t
    with Not_found -> t
  ) a
  
(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
