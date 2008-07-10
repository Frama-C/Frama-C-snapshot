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

(*i $Id: cmain.ml,v 1.101 2008/02/05 12:10:47 marche Exp $ i*)

open Format
open Coptions
open Cerror
open Creport
open Output

let parse_file f =
  let ppf = Cpp.cpp f in
  let c = open_in ppf in
  let p = Clexer.parse c in
  close_in c;
  f, p

let type_file (f,p) = 
  (f, Ctyping.type_file p)


let main () = 
  let t0 = Unix.times () in
  (* parsing *)
  let input_files = files () in
  let pfiles = List.map parse_file input_files in
  if parse_only then exit 0;
  (* typing *)
  let tfiles = List.map type_file pfiles in
  if type_only then exit 0;
  let on_all_files g = List.map (fun (f, dl) -> (f, g dl)) in
  (* initialisation of global variables *)
  let tfiles = on_all_files Cinit.add_init tfiles in
  (* call graph *)
  List.iter (fun (_,p) -> Cgraph.file p) tfiles ;
  if print_graph then Cprint_graph.print_graph ();
  let tab_comp = Cgraph.find_comp tfiles in
  (* normalisation *)
  Cenv.update_fields_type ();
  lprintf "starting normalization of programs.@.";  
  let nfiles = on_all_files Cnorm.file tfiles in
  (* local aliasing analysis *)
  if local_aliasing then Cptr.local_aliasing_transform ();
  (* separation *)  
  lprintf "starting separation of variables.@.";
  List.iter (fun (_,p) -> Cseparation.file p)  nfiles;
  Cseparation.funct [Cinit.invariants_initially_established_info]; 
  Array.iter (fun l -> 
		Cseparation.funct l ) 
    tab_comp;  
  (* typing predicates *)  
  let nfiles = on_all_files Invariant.add_predicates nfiles in
  if print_norm then begin
    let print_fun = ref true in
    List.iter 
      (fun (f,p) -> 
	 let c = open_out (f ^ "norm") in
	 let fmt = Format.formatter_of_out_channel c in
	 Format.fprintf fmt "/* Declarations */@.@.";
	 Format.fprintf fmt "%a@." Cprint.nfile p;
	 if !print_fun then begin
	   Format.fprintf fmt "/* Functions */@.@.";
	   Cprint.nfunctions fmt;
	   Format.fprintf fmt "@.";
	   print_fun := false;
	 end;
	 close_out c) nfiles;
  end;
  (* effects *)
  lprintf "starting computation of effects.@.";
  List.iter (fun (_,p) -> Ceffect.file p) nfiles;
  Array.iter (Ceffect.effect nfiles) tab_comp;
  Queue.iter 
    (fun (loc,msg) ->
       lprintf "%a %s@." Loc.report_position loc msg;
       warning loc "%s" msg)
    Ceffect.warnings;
  lprintf "heap variables: %a@." Ceffect.print_heap_vars ();
  (* Why interpretation *)
  let why_specs =
    List.fold_left 
      (fun specs (_,f) -> 
	 let s = Cinterp.interp f in s @ specs) 
      [] nfiles
  in
  let (why_code,why_specs) = Cinterp.interp_functions why_specs in
  (* Why specs *)
  let first_file = Filename.chop_extension (List.hd input_files) in
  let file = Lib.file "why" (first_file ^ "_spec") in
  Pp.print_in_file 
    (fun fmt -> 
       fprintf fmt 
	 "(* this file was automatically generated; do not edit *)@\n@\n";
       fprintf fmt "(* zone variables *)@.";
       if Coptions.no_zone_type then
	 fprintf fmt "type global@.@."
       else
	 Hashtbl.iter 
	   (fun name z ->
	      match z.Info.repr with 
		| None ->
		    let d = Type (name,[]) in
		    fprintf fmt "@[%a@]" fprintf_why_decls [d]
		| Some _ -> ())
	   Cenv.zone_table;
       fprintf fmt "(* logic types *)@.";
       Cenv.iter_types (fun t -> fprintf fmt "@[type %s@\n@]" t);

       let why_int_types = 
	 Cinterp.make_int_types_decls () @ Cinterp.make_enum_types_decls ()
       in
       if why_int_types <> [] then begin
	 fprintf fmt "(* C integer types *)@.";
	 Output.fprintf_why_decls fmt why_int_types
       end;

       fprintf fmt "(* heap variables *)@.";
       Hashtbl.iter 
	 (fun v bt -> 
	    let d = Param 
	      (false, v, 
	       Ref_type (Base_type (Info.output_why_type ~quote_var:false 
				      bt.Info.var_why_type))) in
	    fprintf fmt "@[%a@]" fprintf_why_decls [d])
	 Ceffect.heap_vars;
       fprintf fmt "(* functions specifications *)@\n";
       Output.fprintf_why_decls fmt why_specs) 
    (file ^ ".tmp");
  Lib.file_copy_if_different (file ^ ".tmp") (file ^ ".why");
  (* function bodies *)
  if separate then begin
    List.iter
      (fun (f,d) ->
	 Cmake.add first_file f;
	 let file = Lib.file "why" (first_file ^ "__" ^ f ) in
	 Pp.print_in_file 
	   (fun fmt -> Output.fprintf_why_decl fmt d) (file ^ ".tmp");
	 Lib.file_copy_if_different (file ^ ".tmp") (file ^ ".why"))
      why_code
  end else begin
    let file = Lib.file "why" first_file in
    let why_code = List.map snd why_code in
    Pp.print_in_file 
      (fun fmt -> Output.fprintf_why_decls fmt why_code) (file ^ ".tmp");
    Lib.file_copy_if_different (file ^ ".tmp") (file ^ ".why")
  end;
  (* print locs *)
  Pp.print_in_file Cinterp.print_locs (Lib.file "." (first_file ^ ".loc"));
  (* makefile *)
  Cmake.makefile first_file;
  if show_time then
    let t1 = Unix.times () in
    printf "Caduceus execution time : %3.2f@." (t1.Unix.tms_utime -. 
					     t0.Unix.tms_utime)
  else
    ()

       
       
 
let rec explain_exception fmt = function
  | Parsing.Parse_error -> 
      fprintf fmt "Syntax error"
  | Stream.Error s -> 
      fprintf fmt "Syntax error: %s" s
  | Error (Some loc, e) ->
      fprintf fmt "%a%a" Loc.report_position loc report e
  | Error (_, e) ->
      report fmt e
  | e ->
      fprintf fmt "Anomaly: %s@." (Printexc.to_string e);
      raise e


(* for debugging *)

(*
let () = main (); exit 1
*) 

let () =
  try
    main ()
  with e ->
    eprintf "%a@." explain_exception e;
    exit 1

(*
Local Variables: 
compile-command: "make -j -C .. bin/caduceus.byte"
End: 
*)
