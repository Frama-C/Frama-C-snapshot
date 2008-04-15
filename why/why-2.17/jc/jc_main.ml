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

(* $Id: jc_main.ml,v 1.135 2008/11/25 08:29:57 marche Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_region
open Jc_ast
open Jc_fenv

open Format

let parse_file f = 
  try
    let c = open_in f in
    let d = Jc_lexer.parse f c in
    close_in c; d
  with
    | Jc_lexer.Lexical_error(l,s) ->
	eprintf "%a: lexical error: %s@." Loc.gen_report_position l s;
	exit 1

let compute_regions logic_components components =
  if !Jc_options.separation_sem = SepRegions then begin
    Jc_options.lprintf "Computation of regions@.";
    (* Preserve order between following calls *)
    Array.iter Jc_separation.logic_component logic_components;
    Hashtbl.iter Jc_separation.axiom Jc_typing.lemmas_table;
    Hashtbl.iter
      (fun _id data ->
	 List.iter 
	   (function Jc_typing.ABaxiom(pos,id,labs,a) ->
	      Jc_separation.axiom id (pos,(* is_axiom = *)true,labs,a)
	   ) data.Jc_typing.axiomatics_decls
      ) Jc_typing.axiomatics_table;
    Array.iter Jc_separation.code_component components
  end

let compute_effects logic_components components =
  Jc_options.lprintf "Computation of effects@.";
  (* Preserve order between following calls *)
  Array.iter Jc_effect.logic_effects logic_components;
  Array.iter Jc_effect.function_effects components

let main () =
  let files = Jc_options.files () in 
  try match files with [file] ->
    let filename = Filename.chop_extension file in

    (*************************************************************************)
    (*                          PART 1: PARSING                              *)
    (*************************************************************************)

    (* phase 1: parsing *)
    Jc_options.lprintf "Parsing@.";
    let ast = parse_file file in

    if Jc_options.debug then
      Format.printf "@\nAST AFTER PARSING:@\n%a@." Jc_poutput.pdecls ast;

    (*************************************************************************)
    (*                          PART 2: ANALYSIS                             *)
    (*************************************************************************)

    (* phase 2: normalization *)
    Jc_options.lprintf "Normalization@.";
    let ast = Jc_norm.decls ast in

    (* phase 3: typing *)

    (* phase 3.1: type logic labels *)
    Jc_options.lprintf "Typing logic labels@.";
    List.iter Jc_typing.type_labels_in_decl ast;

    (* phase 3.2: type code *)
    Jc_options.lprintf "Typing code@.";
    Jc_typing.type_file ast;

    if Jc_options.debug then
      Format.printf "@\nAST AFTER TYPING:@\n%a@." Jc_typing.print_file ();

    (* phase 4: computation of call graph *)
    Jc_options.lprintf "Computation of call graph@.";
    Hashtbl.iter (fun _ (f,t) -> Jc_callgraph.compute_logic_calls f t)
      Jc_typing.logic_functions_table;
    Hashtbl.iter 
      (fun _ (f,_loc,s,b) -> 
	 Option_misc.iter (Jc_callgraph.compute_calls f s) b
      ) Jc_typing.functions_table;
    let logic_components = 
      Jc_callgraph.compute_logic_components Jc_typing.logic_functions_table
    in
    let components = 
      Jc_callgraph.compute_components Jc_typing.functions_table
    in
    
    (* (optional) phase 5: inference of annotations *)
    if !Jc_options.annotation_sem <> AnnotNone then
      begin
	(* phase 5.1: pre-computation of regions *)
	compute_regions logic_components components;

	(* phase 5.2: pre-computation of effects *)
	compute_effects logic_components components;

	(* phase 5.3: inter- or intraprocedural inference of annotations *)
	Jc_options.lprintf "Inference of annotations@.";
	if Jc_options.interprocedural then
	  begin
	    (* record recursive functions *)
	    Hashtbl.iter
	      (fun _ (fi, _, _, _) ->
		 fi.jc_fun_info_is_recursive <- Jc_ai.is_recursive fi) 
	      Jc_typing.functions_table;
	    (* interprocedural analysis over the call graph +
	       intraprocedural analysis of each function called *)
	    Hashtbl.iter
	      (fun _ (fi, loc, fs, sl) ->
		 if fi.jc_fun_info_name = Jc_options.main then
		   Jc_ai.main_function (fi, loc, fs, sl)
	      ) Jc_typing.functions_table;
	  end
	else
	  (* intraprocedural inference of annotations otherwise *)
	  Hashtbl.iter 
	    (fun _ (f, loc, s, b) -> Jc_ai.code_function (f, loc, s, b))
	    Jc_typing.functions_table;
      end;

    (* phase 6: add invariants *)
    Jc_options.lprintf "Adding invariants@.";
    let vil = 
      Hashtbl.fold (fun _tag (vi, _eo) acc -> vi :: acc)
	Jc_typing.variables_table []
    in
    Hashtbl.iter
      (fun _tag (f,loc,s,b) -> Jc_invariants.code_function (f, loc, s, b) vil)
      Jc_typing.functions_table;

    (* phase 7: computation of regions *)
    compute_regions logic_components components;

    (* phase 8: computation of effects *)
    compute_effects logic_components components;

    (* (optional) phase 9: checking structure invariants *)
    begin match !Jc_options.inv_sem with
      | InvOwnership ->
	  Jc_options.lprintf "Adding structure invariants@.";
	  Hashtbl.iter (fun name (_,invs) -> Jc_invariants.check invs)
	    Jc_typing.structs_table
      | InvNone
      | InvArguments -> ()
    end;

    (*************************************************************************)
    (*                    PART 3: GENERATION OF WHY CODE                     *)
    (*************************************************************************)

    let push_decls, fold_decls, pop_decls = 
      let decls = ref [] in
      (fun f -> decls := f !decls), 
      (fun f acc -> let d,acc = f (!decls,acc) in decls := d; acc), 
      (fun () -> !decls)
    in

    (* production phase 1: generation of Why types *)

    (* production phase 1.1: translate logic types *)
    Jc_options.lprintf "Translate logic types@.";
    push_decls
      (Hashtbl.fold (fun _ id acc -> Jc_interp.tr_logic_type id acc)
	 Jc_typing.logic_type_table);

    (* production phase 1.2: translate coding types *)
    Jc_options.lprintf "Translate structures@.";
    push_decls
      (Hashtbl.fold (fun _ (st, _) acc -> Jc_interp.tr_struct st acc)
	 Jc_typing.structs_table);

    Jc_options.lprintf "Translate variants@.";
    push_decls
      (Hashtbl.fold (fun _ -> Jc_interp.tr_root) Jc_typing.roots_table);

    (* production phase 2: generation of Why variables *)

    (* production phase 2.1: translate coding variables *)
    Jc_options.lprintf "Translate variables@.";
    push_decls
      (Hashtbl.fold (fun _ (v,e) acc -> Jc_interp.tr_variable v e acc)
	 Jc_typing.variables_table);

    (* production phase 2.2: translate memories *)
    Jc_options.lprintf "Translate memories@.";
    let regions = 
      fold_decls
	(Hashtbl.fold 
	   (fun _ (fi,r) (acc,regions) -> 
	      let r = Region.representative r in
	      let acc = 
		if RegionSet.mem r regions then acc else 
		  Jc_interp.tr_region r acc 
	      in
	      Jc_interp.tr_memory (fi,r) acc,RegionSet.add r regions)
	   Jc_effect.constant_memories
	) (RegionSet.singleton dummy_region)
    in
    
    (* production phase 2.3: translate allocation tables *)
    Jc_options.lprintf "Translate allocation tables@.";
    let regions =
      fold_decls
	(Hashtbl.fold 
	   (fun _ (a,r) (acc,regions) -> 
	      let r = Region.representative r in
	      let acc = 
		if RegionSet.mem r regions then acc else
		  Jc_interp.tr_region r acc 
	      in
	      Jc_interp.tr_alloc_table (a,r) acc, RegionSet.add r regions)
	   Jc_effect.constant_alloc_tables
	) regions
    in	       	  

    (* production phase 2.4: translate tag tables *)
    Jc_options.lprintf "Translate tag tables@.";
    let _ =
      fold_decls
	(Hashtbl.fold 
	   (fun _ (a,r) (acc,regions) -> 
	      let r = Region.representative r in
	      let acc = 
		if RegionSet.mem r regions then acc else
		  Jc_interp.tr_region r acc 
	      in
	      Jc_interp.tr_tag_table (a,r) acc, RegionSet.add r regions)
	   Jc_effect.constant_tag_tables
	) regions
    in	       	  

    (* production phase 3: generation of Why exceptions *)
    Jc_options.lprintf "Translate exceptions@.";
    push_decls
      (Hashtbl.fold (fun _ ei acc -> Jc_interp.tr_exception ei acc)
	 Jc_typing.exceptions_table);

    (* production phase 1.3: translate enumerated types *)
    (* Yannick: why here and not together with translation of types? *)
    Jc_options.lprintf "Translate enumerated types@.";
    push_decls
      (Hashtbl.fold 
	 (fun _ ri acc -> Jc_interp.tr_enum_type ri acc)
	 Jc_typing.enum_types_table);
    let enumlist = 
      Hashtbl.fold (fun _ ri acc -> ri::acc) Jc_typing.enum_types_table []
    in
    let rec treat_enum_pairs pairs acc = 
      match pairs with
	| [] -> acc
	| ri1 :: rest ->
	    let acc = 
	      List.fold_left 
		(fun acc ri2 ->
		   Jc_interp.tr_enum_type_pair ri1 ri2 acc) acc rest
	    in
	    treat_enum_pairs rest acc
    in
    push_decls (treat_enum_pairs enumlist);

    (* production phase 4.1: generation of Why logic functions *)
    Jc_options.lprintf "Translate logic functions@.";
    push_decls 
      (Hashtbl.fold 
	 (fun _ (li, p) acc ->
	    Jc_options.lprintf "Logic function %s@." li.jc_logic_info_name;
	    Jc_interp.tr_logic_fun li p acc)
	 Jc_typing.logic_functions_table );

    (* production phase 4.2: generation of axiomatic logic decls*)
    Jc_options.lprintf "Translate axiomatic declarations@.";
    push_decls 
      (Hashtbl.fold 
	 (fun a data acc ->
	    Jc_options.lprintf "Axiomatic %s@." a;
	    List.fold_left Jc_interp.tr_axiomatic_decl acc data.Jc_typing.axiomatics_decls)
	 Jc_typing.axiomatics_table);


    (* production phase 4.3: generation of lemmas *)
    Jc_options.lprintf "Translate lemmas@.";
    push_decls
      (Hashtbl.fold 
	 (fun id (loc,is_axiom,labels,p) acc ->
	    Jc_interp.tr_axiom loc id is_axiom labels p acc)
	 Jc_typing.lemmas_table);

    (* (optional) production phase 6: generation of global invariants *)
    if !Jc_options.inv_sem = InvOwnership then
      (Jc_options.lprintf "Generation of global invariants@.";
       push_decls Jc_invariants.make_global_invariants);

    (* production phase 7: generation of Why functions *)
    Jc_options.lprintf "Translate functions@.";
    push_decls
      (Hashtbl.fold 
	 (fun _ (f,loc,s,b) acc ->
	    Jc_options.lprintf "Function %s@." f.jc_fun_info_name;
	    Jc_interp.tr_fun f loc s b acc)
	 Jc_typing.functions_table);
    push_decls
      (Hashtbl.fold 
	 (fun n (fname,param_name_assoc) acc ->
	    Jc_interp.tr_specialized_fun n fname param_name_assoc acc)
	 Jc_interp_misc.specialized_functions);

    (* (optional) production phase 8: generation of global invariants *)
    if !Jc_options.inv_sem = InvOwnership then
      begin
	(* production phase 8.1: "mutable" and "committed" declarations *)
	Jc_options.lprintf "Translate mutable and committed declarations@.";
	push_decls
	  (Hashtbl.fold
	     (fun _ (st, _) acc -> Jc_invariants.mutable_declaration st acc)
	     Jc_typing.structs_table);

	(* production phase 8.2: pack *)
	Jc_options.lprintf "Translate pack@.";
	push_decls
	  (Hashtbl.fold
	     (fun _ (st, _) acc -> Jc_invariants.pack_declaration st acc)
	     Jc_typing.structs_table);

	(* production phase 8.3: unpack *)
	Jc_options.lprintf "Translate unpack@.";
	push_decls
	  (Hashtbl.fold
	     (fun _ (st, _) acc -> Jc_invariants.unpack_declaration st acc)
	     Jc_typing.structs_table)
      end;

    (*************************************************************************)
    (*                       PART 5: OUTPUT FILES                            *)
    (*************************************************************************)

    let decls = pop_decls () in

    (* output phase 1: produce Why file *)
    Jc_options.lprintf "Produce Why file@.";
    Pp.print_in_file 
      (fun fmt -> fprintf fmt "%a@." Output.fprintf_why_decls decls)
      (Lib.file_subdir "why" (filename ^ ".why"));

    (* output phase 2: produce locs file *)
    Jc_options.lprintf "Produce locs file@.";
    let cout_locs,fmt_locs = 
      Pp.open_file_and_formatter (Lib.file_subdir "." (filename ^ ".loc")) 
    in
    Jc_interp.print_locs fmt_locs;
    Output.print_pos fmt_locs; (* Generated annotations. *)
    Pp.close_file_and_formatter (cout_locs,fmt_locs);

    (* output phase 3: produce makefile *)
    Jc_options.lprintf "Produce makefile@.";
    Jc_make.makefile filename
      
    | _ -> Jc_options.usage ()
  with
    | Jc_typing.Typing_error(l,s) when not Jc_options.debug ->
        eprintf "%a: typing error: %s@." Loc.gen_report_position l s;
        exit 1
    | Jc_options.Jc_error(l,s) when not Jc_options.debug ->
	eprintf "%a: %s@." Loc.gen_report_position l s;
	exit 1
    | Assert_failure(f,l,c) as exn when not Jc_options.debug ->
 	eprintf "%a:@." Loc.gen_report_line (f,l,c,c);
 	raise exn
	  
let _ = 
  Sys.catch_break true;
  (* Yannick: [Printexc.catch] deprecated, normal system error seems ok, 
     remove call? *)
  if Jc_options.debug then main () else Printexc.catch main ()

  
(*
  Local Variables: 
  compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
  End: 
*)
