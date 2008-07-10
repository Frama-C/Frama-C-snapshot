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

(* $Id: jc_main.ml,v 1.111 2008/07/08 16:16:37 moy Exp $ *)

open Jc_env
open Jc_fenv
open Jc_region
open Jc_ast
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

let main () =
  let files = Jc_options.files () in try match files with [file] ->
    let filename = Filename.chop_extension file in

    (* phase 1: parsing *)
    Jc_options.lprintf "Parsing@.";
    let ast = parse_file file in

    if Jc_options.debug then
      Format.printf "%a@." Jc_poutput.pdecls ast;

    (* phase 2: normalization *)
    Jc_options.lprintf "Normalization@.";
    let ast = Jc_norm.decls ast in

    (* phase 3: typing *)
    (* phase 3.1: type logic labels *)
    Jc_options.lprintf "Typing logic labels@.";
    List.iter Jc_typing.type_logic_labels_in_decl ast;

    (* phase 3.1: type logic labels *)
    Jc_options.lprintf "Typing@.";
    Jc_typing.type_file ast;

    if Jc_options.debug then
      Format.printf "%a@." Jc_typing.print_file ();

    (* phase ?: adding invariants *)
    Jc_options.lprintf "Adding invariants@.";
    let vil = 
      Hashtbl.fold
	(fun _tag (vi, _eo) acc -> vi :: acc)
	Jc_typing.variables_table []
    in
    Hashtbl.iter
      (fun _tag (f,loc,s,b) -> 
	 Jc_invariants.code_function (f, loc, s, b) vil)
      Jc_typing.functions_table;

    (* phase 4: computation of call graph *)
    Jc_options.lprintf "Computation of call graph@.";
    Hashtbl.iter 
      (fun _ (f,t) -> Jc_callgraph.compute_logic_calls f t)
      Jc_typing.logic_functions_table;
    Hashtbl.iter 
      (fun _ (f,loc,s,b) -> 
	 Option_misc.iter (Jc_callgraph.compute_calls f s) b)
      Jc_typing.functions_table;
    let logic_components = 
      Jc_callgraph.compute_logic_components
	Jc_typing.logic_functions_table
    in
    let components = 
      Jc_callgraph.compute_components Jc_typing.functions_table
    in
    
    (* phase 5: computation of regions *)
    if !Jc_options.separation_sem = SepRegions then begin
      Jc_options.lprintf "Computation of regions@.";
      (* Analyze logic functions before axioms, so that parameter 
       * regions are known before a function is applied. 
       *)
      Array.iter Jc_separation.logic_component logic_components;
      Hashtbl.iter Jc_separation.axiom Jc_typing.axioms_table;
      Array.iter Jc_separation.code_component components
    end;

    (* phase 6: computation of effects *)
    Jc_options.lprintf
      "\nstarting computation of effects of logic functions.@.";
    Array.iter Jc_effect.logic_effects logic_components;
    Jc_options.lprintf
      "\nstarting computation of effects of functions.@.";
    Array.iter Jc_effect.function_effects components;
    
    (* (optional) phase 7: inference of annotations *)
    if !Jc_options.annotation_sem <> AnnotNone then
      begin
	Hashtbl.iter 
	  (fun key (f, loc, s, b) -> 
	     match b with None -> () | Some b ->
	       let b = Jc_ai.normalize_expr b in
	       Hashtbl.replace Jc_typing.functions_table key (f,loc,s,Some b)
	  ) Jc_typing.functions_table;

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
	    (fun _ (f, loc, s, b) -> 
	       Jc_ai.code_function (f, loc, s, b) 
	    ) Jc_typing.functions_table
      end;
    
    (* phase 8: checking structure invariants *)
    begin
      match !Jc_options.inv_sem with
	| InvOwnership ->
	    Jc_options.lprintf
	      "\nstarting checking structure invariants.@.";
	    Hashtbl.iter 
	      (fun name (_,invs) ->
		 Jc_options.lprintf
		   "  Checking structure: %s@." name;
		 Jc_invariants.check invs)
	      Jc_typing.structs_table
	| InvNone
	| InvArguments -> ()
    end;
    
    (* production phase 1.1 : generation of Why logic types *)
    Jc_options.lprintf
      "production phase 1.1: generation of Why logic types@.";
    let d_types =
      Hashtbl.fold 
	(fun _ id acc ->
	   Jc_interp.tr_logic_type id acc)
	Jc_typing.logic_type_table
	[]
    in	       	 

    (* production phase 1.2 : translate stuff *)
    Jc_options.lprintf
      "production phase 1.2.1: translate structures@.";
    let d_memories =
      Hashtbl.fold 
	(fun _ (st, _) acc ->
	   Jc_interp.tr_struct st acc)
	Jc_typing.structs_table
	d_types
    in	       	  
    Jc_options.lprintf
      "production phase 1.2.2: translate variants@.";
    let d_memories =
      Hashtbl.fold
	(fun _ -> Jc_interp.tr_variant)
	Jc_typing.variants_table
	d_memories
    in
    Jc_options.lprintf
      "production phase 1.2.3: translate variables@.";
    let d_memories =
      Hashtbl.fold 
	(fun _ (v,e) acc ->
	   Jc_interp.tr_variable v e acc)
	Jc_typing.variables_table
	d_memories
    in	       	  
    Jc_options.lprintf
      "production phase 1.2.3: translate regions and memories@.";
    let d_memories,regions =
      FieldOrVariantRegionSet.fold 
	(fun (fi,r) (acc,regions) -> 
	   let acc = 
	     if RegionSet.mem r regions then acc else
	       Jc_interp.tr_region r acc 
	   in
	   Jc_interp.tr_memory (fi,r) acc,RegionSet.add r regions)
	(FieldOrVariantRegionSet.map_repr !Jc_effect.constant_memories_set)
	(d_memories,RegionSet.singleton dummy_region)
    in	       	  
    Jc_options.lprintf
      "production phase 1.2.4: translate regions and allocation tables@.";
    let d_memories,_ =
      let arts = StringRegionSet.map_repr
	!Jc_effect.alloc_region_table_set
      in
      StringRegionSet.fold 
	(fun (a,r) (acc,regions) -> 
	   let acc = 
	     if RegionSet.mem r regions then acc else
	       Jc_interp.tr_region r acc 
	   in
	   Jc_interp.tr_alloc_table2 (a,r) acc, RegionSet.add r regions)
	arts
	(d_memories,regions)
    in	       	  

    (* production phase 1.3 : generation of Why exceptions *)
    Jc_options.lprintf
      "production phase 1.3 : generation of Why exceptions@.";
    let d_exc =
      Hashtbl.fold 
	(fun _ ei acc ->
 	   Jc_interp.tr_exception ei acc)
	Jc_typing.exceptions_table
	d_memories
    in

    (* production phase 1.4 : generation of Why enum_types *)
    Jc_options.lprintf "Generation of Why enum_types@.";
    let d =
      Hashtbl.fold 
	(fun _ (ri (* ,to_int,to_int_,of_int *)) acc ->
	   Jc_interp.tr_enum_type ri (* to_int_ of_int *) acc)
	Jc_typing.enum_types_table
	d_exc
    in	       	  
    let enumlist = 
      Hashtbl.fold (fun _ ri acc -> ri::acc) Jc_typing.enum_types_table []
    in
    let rec treat_enum_pairs acc = function
      | [] -> acc
      | ri1 :: rest ->
	  let acc = 
	    List.fold_left 
	      (fun acc ri2 ->
		 Jc_interp.tr_enum_type_pair ri1 ri2 acc) acc rest
	  in
	  treat_enum_pairs acc rest
    in
    let d = treat_enum_pairs d enumlist in

    (* production phase x.x : generation of Why logic constants *)
    Jc_options.lprintf "Generation of Why logic constants@.";
    let d =
      Hashtbl.fold 
	(fun _ (vi,init) acc ->
	   Jc_interp.tr_logic_const vi init acc)
	Jc_typing.logic_constants_table
	d
    in	       	  
    (* production phase 2 : generation of Why logic functions *)
    Jc_options.lprintf "Generation of Why logic functions@.";
    let d_lfuns = 
      Hashtbl.fold 
	(fun _ (li, p) acc ->
	   Jc_interp.tr_logic_fun li p acc)
	Jc_typing.logic_functions_table 
	d
    in
    (* production phase 3 : generation of Why axioms *)
    Jc_options.lprintf "Generation of Why axioms@.";
    let d_axioms = 
      Hashtbl.fold 
	(fun id (is_axiom,labels,p) acc ->
	   Jc_interp.tr_axiom id is_axiom (* labels *) p acc)
	Jc_typing.axioms_table
	d_lfuns
    in	       
    (* production phase 3.5 : generation of globalinvariant predicates *)
    Jc_options.lprintf "Generation of globalinvariant predicates@.";
    let d_axioms =
      if !Jc_options.inv_sem = InvOwnership then
	Jc_invariants.make_global_invariants d_axioms
      else d_axioms
    in
    Jc_options.lprintf
      "production phase 4 : generation of Why functions@.";
    let d_funs = 
      Hashtbl.fold 
	(fun _ (f,loc,s,b) acc ->
	   Jc_options.lprintf
	     "Generation of Why function %s@." f.jc_fun_info_name;
	   Jc_interp.tr_fun f loc s b acc)
	Jc_typing.functions_table
	d_axioms
    in
    let d_inv =
      if !Jc_options.inv_sem = InvOwnership then
	begin
	  Jc_options.lprintf "production phase 5 : (invariants tools)@.";
	  (* production phase 5.1 : "assoc" declaration *)
	  (*let d_inv = Jc_invariants.assoc_declaration::d_funs in *)
	  let d_inv = d_funs in
	  (* production phase 5.2 :
             "mutable" and "committed" declarations *)
	  Jc_options.lprintf "mutable and committed declarations@.";
	  let d_inv =
	    Hashtbl.fold
	      (fun _ (st, _) acc ->
		 Jc_invariants.mutable_declaration st acc)
	      Jc_typing.structs_table
	      d_inv
	  in
	  (* production phase 5.3 : global invariants
             (not mutable implies invariant) *)
          (*let d_inv =
            Hashtbl.fold
            (fun _ (st, _) acc ->
            Jc_invariants.invariants_axioms st acc)
            Jc_typing.structs_table
            d_inv
            in*)
	  (* production phase 5.4 : pack *)
	  Jc_options.lprintf "production phase 5.4: pack@.";
	  let d_inv =
	    Hashtbl.fold
	      (fun _ (st, _) acc ->
		 Jc_invariants.pack_declaration st acc)
	      Jc_typing.structs_table
	      d_inv
	  in
	  (* production phase 5.5 : unpack *)
	  Jc_options.lprintf "production phase 5.5: pack@.";
	  let d_inv =
	    Hashtbl.fold
	      (fun _ (st, _) acc ->
		 Jc_invariants.unpack_declaration st acc)
	      Jc_typing.structs_table
	      d_inv
	  in
	  d_inv
	end
      else
	d_funs
    in
    (* production phase 6.1 : produce Why file *)
    Jc_options.lprintf "production phase 6.1: produce Why file@.";
    Pp.print_in_file 
      (fun fmt -> fprintf fmt "%a@." Output.fprintf_why_decls d_inv)
      (Lib.file_subdir "why" (filename ^ ".why"));
    (* production phase 6.2 : produce locs file *)
    Jc_options.lprintf "production phase 6.2: produce locs file@.";
    let cout_locs,fmt_locs = 
      Pp.open_file_and_formatter (Lib.file_subdir "." (filename ^ ".loc")) in
    Jc_interp.print_locs fmt_locs;
    Output.print_locs fmt_locs; (* Generated annotations. *)
    Pp.close_file_and_formatter (cout_locs,fmt_locs);
    (* production phase 6.3 : produce makefile *)
    Jc_options.lprintf "production phase 6.3: produce makefile@.";
    Jc_make.makefile filename
    
    | _ -> Jc_options.usage ()
	
  with
    | Jc_typing.Typing_error(l,s) when not Jc_options.debug ->
        eprintf "%a: typing error: %s@." Loc.gen_report_position l s;
        exit 1
    | Jc_options.Jc_error(l,s) ->
	eprintf "%a: %s@." Loc.gen_report_position l s;
	exit 1
    | Assert_failure(f,l,c) as exn ->  
 	eprintf "%a:@." Loc.gen_report_line (f,l,c,c); 
 	raise exn 
	  
let _ = 
  Sys.catch_break true;
  Printexc.catch main ()

  
(*
  Local Variables: 
  compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
  End: 
*)
