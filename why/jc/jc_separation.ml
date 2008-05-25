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

(* $Id: jc_separation.ml,v 1.17 2008/04/10 16:05:55 moy Exp $ *)

open Jc_env
open Jc_envset
open Jc_fenv
open Jc_constructors
open Jc_ast
open Format
open Jc_iterators
open Jc_region
open Jc_pervasives
open Pp

let term rresult t =
  ITerm.iter (fun t -> match t#node with
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
	let arg_regions = 
	  List.map (fun t -> t#region) app.jc_app_args
	in
	Jc_options.lprintf "param:%a@." (print_list comma Region.print) param_regions;
	Jc_options.lprintf "arg:%a@." (print_list comma Region.print) arg_regions;
	List.iter2 Region.unify param_regions arg_regions;
	let result_region = 
	  try RegionList.assoc li.jc_logic_info_result_region assoc
	  with Not_found -> assert false
	in
	Jc_options.lprintf "param:%a@." Region.print result_region;
	Jc_options.lprintf "arg:%a@." Region.print t#region;
	Region.unify result_region t#region
    | JCTconst _ | JCTrange(None,None) | JCTbinary _ | JCTshift _
    | JCTrange _ | JCTunary _ | JCTderef _ | JCTold _ | JCTat _ | JCToffset _
    | JCTinstanceof _ | JCTcast _ | JCTrange_cast _ | JCTreal_cast _ ->
	()
  ) t

let assertion rresult a =
  iter_term_and_assertion (term rresult) 
    (fun a -> match a#node with
      | JCAapp _ -> () (* TODO *)
      | JCAtrue | JCAfalse | JCArelation _  | JCAtagequality _ 
      | JCAinstanceof _ | JCAbool_term _ | JCAmutable _ 
      | JCAand _ | JCAor _ | JCAimplies _ | JCAiff _ | JCAif _ | JCAmatch _
      | JCAnot _ | JCAquantifier _ | JCAold _ | JCAat _ ->
	  ()
    ) a

let expr rresult e = 
  IExpr.iter 
    (fun e -> match e#node with
       | JCEbinary(e1,_,e2) | JCEif(_,e1,e2) ->
	   Region.unify e1#region e2#region
       | JCEmatch(_, (_, e1)::rem) ->
	   List.iter
	     (fun (_, e2) -> Region.unify e1#region e2#region)
	     rem
       | JCEmatch(_, []) ->
	   ()
       | JCEconst _ | JCEvar _ | JCEshift _ | JCEunary _
       | JCEderef _ | JCEoffset _ | JCEinstanceof _ | JCEcast _ 
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
	let regions = match call.jc_call_fun with
	  | JClogic_fun f -> f.jc_logic_info_param_regions 
	  | JCfun f -> f.jc_fun_info_param_regions 
	in
	let params = match call.jc_call_fun with
	  | JClogic_fun f -> f.jc_logic_info_parameters 
	  | JCfun f -> f.jc_fun_info_parameters
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
	let arg_regions = 
	  List.map (fun e -> e#region) call.jc_call_args
	in
	Jc_options.lprintf "param:%a@." (print_list comma Region.print) param_regions;
	Jc_options.lprintf "arg:%a@." (print_list comma Region.print) arg_regions;
	List.iter2 Region.unify param_regions arg_regions;
	let rregion = match call.jc_call_fun with
	  | JClogic_fun f -> f.jc_logic_info_result_region 
	  | JCfun f -> f.jc_fun_info_return_region
	in
	let result_region = 
	  if is_dummy_region rregion then dummy_region
	  else
	    try RegionList.assoc rregion assoc
	    with Not_found -> assert false
	in
	Jc_options.lprintf "param:%a@." Region.print result_region;
	Jc_options.lprintf "arg:%a@." Region.print e#region;
	Region.unify result_region e#region
    | JCEreturn(ty,e) ->
	Region.unify rresult e#region
    | JCEassert a ->
	assertion rresult a
    | JCEloop(la,_) ->
	iter_term_and_assertion_in_loop_annot 
	  (term rresult) (assertion rresult) la
    | JCEblock _ | JCEtry _ 
    | JCEreturn_void | JCEpack _ | JCEunpack _ -> 
	()
  ) e

let logic_function f =
  let (f, ta) = 
    Hashtbl.find Jc_typing.logic_functions_table f.jc_logic_info_tag 
  in
  let rresult = f.jc_logic_info_result_region in
  begin match ta with
    | JCTerm t -> term rresult t
    | JCAssertion a -> assertion rresult a
    | JCReads r -> () (* TODO *)
  end;
  let param_regions =
    List.map (fun vi -> vi.jc_var_info_region) f.jc_logic_info_parameters in
  let fun_regions = f.jc_logic_info_result_region :: param_regions in
  f.jc_logic_info_param_regions <- RegionList.reachable fun_regions

let logic_component fls =
  List.iter logic_function fls

let code_function f =
  let (f, _, spec, body) = 
    Hashtbl.find Jc_typing.functions_table f.jc_fun_info_tag 
  in
  Jc_options.lprintf "Separation: treating function %s@." f.jc_fun_info_name;
  let rresult = f.jc_fun_info_return_region in
  iter_term_and_assertion_in_fun_spec (term rresult) (assertion rresult) spec;
  Option_misc.iter ((*List.iter*) (expr rresult)) body;
  let param_regions =
    List.map (fun vi -> vi.jc_var_info_region) f.jc_fun_info_parameters in
  let fun_regions = f.jc_fun_info_return_region :: param_regions in
  f.jc_fun_info_param_regions <- RegionList.reachable fun_regions

let code_component fls =
  List.iter code_function fls

let axiom id (is_axiom,labels,a) = assertion (* labels *) dummy_region a

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
      | JCTinstanceof _ | JCTcast _ | JCTrange_cast _ | JCTreal_cast _ | JCTif _ | JCTmatch _ | JCTrange _ ->
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
