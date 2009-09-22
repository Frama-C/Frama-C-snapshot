(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(* $Id$ *)

open Promelaast
open Ltl_utils

let promela_file = ref ""
let ya_file = ref ""
let c_file = ref ""
let output_c_file = ref ""
let ltl_tmp_file = ref ""
let ltl_file = ref ""
let dot_file = ref ""
let verbose = ref false
let root = ref ""
let generatesCFile = ref true
let ltl2ba_params = " -l -p -o "
let toBeRemoved = ref []


(* Promela file *)

let ltl_to_ltlLight f_ltl f_out =
  try
    let c = open_in f_ltl in
    let (ltl_form,ltl_exps) = Ltllexer.parse c in
    close_in c;
    Data_for_ltl.setLtl_expressions ltl_exps;
    Ltl_logic.setLtl_expressions ltl_exps;
    Ltl_output.output ltl_form f_out
  with Not_found ->
    Ltl_to_acsl_option.abort "Problem with file : %s" f_ltl
    | Ltllexer.Error (loc,msg) ->
        Ltl_to_acsl_option.error
          "File %S, line %d, characters %d-%d"
          (fst loc).Lexing.pos_fname (fst loc).Lexing.pos_lnum
          ((fst loc).Lexing.pos_cnum - (fst loc).Lexing.pos_bol)
          ((snd loc).Lexing.pos_cnum - (fst loc).Lexing.pos_bol);
        Ltl_to_acsl_option.error "Error when parsing LTL formula";
        Ltl_to_acsl_option.abort "%s" msg

let load_ya_file f  =
  try
    let c = open_in f in
    let (automata,auto_vars,auto_funs) = Yalexer.parse c  in
    close_in c;
    Data_for_ltl.setAutomata automata auto_vars auto_funs;
  with Not_found ->
    Ltl_to_acsl_option.fatal "Problem with file : %s\n" f


let load_promela_file f  =
  try
    let c = open_in f in
    let (automata,auto_vars,auto_funs) = Promelalexer.parse c  in
    close_in c;
    Data_for_ltl.setAutomata automata auto_vars auto_funs;
  with Not_found ->
		Ltl_to_acsl_option.fatal "Problem with file : %s\n" f
(*    Format.printf "Problem with file : %s\n" f;*)
(*    raise ex                                   *)


let load_promela_file_withexps f  =
  try
    let c = open_in f in
    let (automata,auto_vars,auto_funs) = Promelalexer_withexps.parse c  in
    close_in c;
    Data_for_ltl.setAutomata automata auto_vars auto_funs;
  with Not_found ->
        Ltl_to_acsl_option.fatal "Problem with file : %s\n" f
(*    Format.printf "Problem with file : %s\n" f;*)
(*    raise ex                                   *)




let display_status () =
  if !verbose then begin
    Ltl_to_acsl_option.feedback "\n"  ;
    Ltl_to_acsl_option.feedback "C file:            '%s'\n" !c_file ;
    Ltl_to_acsl_option.feedback "Entry point:       '%s'\n" !root ;
    Ltl_to_acsl_option.feedback "LTL property:      '%s'\n" !ltl_file ;
    Ltl_to_acsl_option.feedback "Files to generate: '%s' (Annotated code)\n"  
      (if (!generatesCFile) then !output_c_file else "(none)");
    if (Ltl_to_acsl_option.Dot.get()) then
      Ltl_to_acsl_option.feedback "Dot file:          '%s'\n"  !dot_file;
    Ltl_to_acsl_option.feedback "Tmp files:         '%s' (Light LTL file)\n" !ltl_tmp_file ;
    Ltl_to_acsl_option.feedback "                   '%s' (Promela file)\n" !promela_file ;
    Ltl_to_acsl_option.feedback "\n"  ;
    (*Format.print_flush ()*)
  end


(** Removes temorary files if any. *)
let cleanup_files () =
  List.iter
    (fun f -> if (String.length f)>0 && (Sys.file_exists f) then Sys.remove f)
    !toBeRemoved
(*   if (String.length !ltl_tmp_file) > 0 && (Sys.file_exists !ltl_tmp_file) then  *)
(*     Sys.remove !ltl_tmp_file; *)
(*   if (String.length !promela_file) > 0 && (Sys.file_exists !promela_file) then  *)
(*     Sys.remove !promela_file *)



let init_file_names () =
  (* Intermediate functions for error display or fresh name of file generation *)
  let err= ref false in
  let dispErr mesg f =
    Ltl_to_acsl_option.error "Error. File '%s' %s.\n" f mesg;
    err:=true
  in
  let freshname pre suf =
    let rec fn p s n =
      if not (Sys.file_exists (p^(string_of_int n)^s)) then (p^(string_of_int n)^s)
      else fn p s (n+1)
    in
    if not (Sys.file_exists (pre^suf)) then pre^suf
    else fn pre suf 0
  in

  (* c_file name is given and has to point out a valid file. *)
  c_file := List.hd (Parameters.Files.get ());
  if (!c_file="") then dispErr ": invalid C file name" !c_file;
  if (not (Sys.file_exists !c_file)) then dispErr "not found" !c_file;

  (* The output C file has to be a valid file name if it is used. *)
  output_c_file := (Ltl_to_acsl_option.Output_C_File.get ()) ;
  if (!output_c_file="") then output_c_file:=freshname ((Filename.chop_extension !c_file)^"_annot") ".c";
(*   else if Sys.file_exists !output_c_file then dispErr "already exists" !output_c_file; *)

  if Ltl_to_acsl_option.Dot.get () then
    dot_file:=freshname (Filename.chop_extension !c_file) ".dot";

  if Ltl_to_acsl_option.Ya.get () = "" then
    if Ltl_to_acsl_option.Buchi.get () = "" then
      begin
	(* ltl_file name is given and has to point out a valid file. *)
	ltl_file := Ltl_to_acsl_option.Ltl_File.get ();
	if (!ltl_file="") then dispErr ": invalid LTL file name" !ltl_file;
	if (not (Sys.file_exists !ltl_file)) then dispErr "not found" !ltl_file;
	
	(* The LTL file is always used. *)
	(* The promela file can be given or not. *)
	if Ltl_to_acsl_option.To_Buchi.get () <> "" then
	  begin
	    ltl_tmp_file:=
	      freshname
		(Filename.chop_extension
		   (Ltl_to_acsl_option.promela_file ())) ".ltl";
	    promela_file:= Ltl_to_acsl_option.promela_file ();
  	    toBeRemoved:=(!ltl_tmp_file)::!toBeRemoved
	  end
	else
	  begin
	    ltl_tmp_file:=Filename.temp_file (Filename.basename !c_file) ".ltl";
	    promela_file:=(freshname (Filename.chop_extension !ltl_tmp_file) ".promela");
	    
	    toBeRemoved:=(!promela_file)::!toBeRemoved;
  	    toBeRemoved:=(!ltl_tmp_file)::!toBeRemoved
	  end
      end
    else
      begin
	if Ltl_to_acsl_option.To_Buchi.get () <> "" &&
	  Ltl_to_acsl_option.Ltl_File.get () <> ""
	then begin
	  Ltl_to_acsl_option.error "Error. '-buchi' option is incompatible with '-to-buchi' and '-ltl' options.";
	  err:=true
	end;
	
	(* The promela file is used only if the process does not terminate after LTL generation. *)
	promela_file := Ltl_to_acsl_option.promela_file ();
      end
  else
    begin
      ya_file := Ltl_to_acsl_option.Ya.get ();
      if (!ya_file="") then dispErr ": invalid Ya file name" !ya_file;
      if (not (Sys.file_exists !ya_file)) then dispErr "not found" !ya_file
    end;


  if Globals.has_entry_point () then begin
    let ep,_ =Globals.entry_point () in
    root := Kernel_function.get_name ep
  end else begin
    Ltl_to_acsl_option.abort
      "The file '%s' does not have any entry point. Generation stopped" !c_file;

  end;
  display_status ();
  !err

let init_test () =
  match Ltl_to_acsl_option.Test.get () with
  | 1 ->
      generatesCFile := false;
  | _ ->
      generatesCFile := true

let printverb (s:string) =
  if !verbose then (Ltl_to_acsl_option.feedback "%s" s)

let run () =
  if Ltl_to_acsl_option.Verbose.get () > 1 then verbose:=true;
  let display_op_specs =
    (Ltl_to_acsl_option.Verbose.get () > 2)
    || (Ltl_to_acsl_option.Output_Spec.get ()) in

  Ltl_to_acsl_option.result ~level:0 "Welcome in the LTL to ACSL plugin@.";
  init_test ();


(* Step 1 : Capture files names *)
  let error_status = init_file_names () in
  (* Treatment is done only if parameters are valid *)
  if error_status then
    Ltl_to_acsl_option.error "Generation stopped."
  else

(* Step 2 : Work in our own project, initialized by a copy of the main one. *)
    let prj =
      File.create_project_from_visitor "ltl_to_acsl"
	(fun prj -> new Visitor.frama_c_copy prj)
    in
    Project.copy ~only:(Plugin.get_selection ()) prj;
    Project.set_current prj;
    let file = Ast.get () in
    Ltl_utils.initFile file;
    printverb "C file loading         : done\n";
    if Ltl_to_acsl_option.Ya.get () = "" then
      if Ltl_to_acsl_option.Buchi.get () = "" then begin
	ltl_to_ltlLight !ltl_file !ltl_tmp_file;
	printverb "LTL loading            : done\n";
	ignore
	  (Sys.command (Format.sprintf "ltl2ba %s -F %s > %s"
			  ltl2ba_params !ltl_tmp_file !promela_file ));
	printverb "LTL ~> Promela (ltl2ba): done\n"
      end;
    if Ltl_to_acsl_option.To_Buchi.get () <> "" then
      printverb ("Finished.\nGenerated file: '"^(!promela_file)^"'\n")
    else
      begin
(* Step 3 : Loading promela_file and checking the consistency between informations from C code and LTL property *)
(*          Such as functions name and global variables. *)

	if Ltl_to_acsl_option.Buchi.get () <> "" then
	  load_promela_file_withexps !promela_file
        else if Ltl_to_acsl_option.Ya.get  () <> "" then
          load_ya_file !ya_file
	else
	  load_promela_file !promela_file;
	printverb "Loading promela        : done\n";

(* Computing the list of ignored functions *)
(* 	Ltl_to_acsl_visitors.compute_ignored_functions file; *)


(* Promelaoutput.print_raw_automata (Data_for_ltl.getAutomata());  *)
(* Data_for_ltl.debug_ltl_expressions (); *)


	if (Ltl_to_acsl_option.Axiomatization.get()) then
	  begin
(* Step 4 : Computing the set of possible pre-states and post-states of each function *)
(*          And so for pre/post transitions *)
	    Ltl_to_acsl_visitors.compute_abstract file !root (Ltl_to_acsl_option.ConsiderAcceptance.get());
	    printverb "Abstracting pre/post   : done\n";

(* 	(display_operations_spec ()); *)


(* Step 5 : incrementing pre/post conditions with states and transitions information *)
	    printverb "Refining pre/post      : \n";
	    if (Ltl_to_acsl_option.AbstractInterpretation.get()) then
	      begin
		(* Repeat until reach a fix-point *)
		while
		  Abstract_ai.propagates_pre_post_constraints file !root
		do () done;
		printverb "    Forward/backward abstract specification        : done\n";
	      end
	    else
	      printverb "    Forward/backward abstract specification        : skiped\n";

(*	(display_operations_spec ());*)


            Bycase_ai.init_specification();
	    if (Ltl_to_acsl_option.advance_abstract_interpretation ())
	    then
	      begin
		(* Repeat until reach a fix-point *)
		while
		  Bycase_ai.propagates_pre_post_constraints_bycase file !root;
     		do () done;
		printverb "    Consider links between input and output states : done\n";


(*		(* Repeat until reach a fix-point *)
		while
		  ControlFlow_ai.propagates_pre_post_constraints file !root;
     		do () done;
		Callgraph.printGraph stdout (Callgraph.computeGraph file);
		printverb "    Forward/backward AI according to control flow  : skiped\n"*)
	      end
	    else
	      begin
		printverb "    Consider links between input and output states : skiped\n";
		(*printverb "    Forward/backward AI according to control flow  : skiped\n";*)
	      end;

(*	(display_operations_spec_bycase ());*)


(* Step 6 : Removing transitions never crossed *)
	    (*Promelaoutput.print_raw_automata (Data_for_ltl.getAutomata()); *)
	    if (Ltl_to_acsl_option.AutomataSimplification.get()) then
	      begin
		Data_for_ltl.removeUnusedTransitionsAndStates ();
		(* Promelaoutput.print_raw_automata (Data_for_ltl.getAutomata()); *)
		printverb "Removing unused trans  : done\n";
	      end
	    else
		printverb "Removing unused trans  : skiped\n";


(* Step 7 : Labeling abstract file *)
	    (* Finally the information is added into the Cil automata. *)
	    Ltl_utils.initGlobals !root (Ltl_to_acsl_option.Axiomatization.get());
 	    Ltl_to_acsl_visitors.add_sync_with_buch file;
	    Ltl_to_acsl_visitors.add_pre_post_from_buch file
	      (not (Ltl_to_acsl_option.advance_abstract_interpretation ()));
	    printverb "Annotation of Cil      : done\n"
	  end
	else
	  begin
(* Step 4': Computing the set of possible pre-states and post-states of each function *)
(*          And so for pre/post transitions *)
	    printverb "Abstracting pre/post   : skiped\n";

(* Step 5': incrementing pre/post conditions with states and transitions information *)
	    printverb "Refining pre/post      : skiped\n";


(* Step 6 : Removing transitions never crossed *)
            printverb "Removing unused trans  : skiped\n";

(* Step 7 : Labeling abstract file *)
	    (* Finally the information is added into the Cil automata. *)
	    Ltl_utils.initGlobals !root (Ltl_to_acsl_option.Axiomatization.get());
 	    Ltl_to_acsl_visitors.add_sync_with_buch file;
	    printverb "Annotation of Cil      : partial\n"
	  end;


(* Step 9 : Generating resulting files *)
	(* Dot file *)
	if (Ltl_to_acsl_option.Dot.get()) then
	  begin
	    Promelaoutput.output_dot_automata (Data_for_ltl.getAutomata ()) !dot_file;
	    printverb "Generating dot file    : done\n"
	  end;

	(* C file *)
	if (not !generatesCFile) then
	  printverb "C file generation      : skiped\n"
	else
	  begin
            let cout = open_out !output_c_file in
	    Cil.print_utf8:=false;
	    Cil.dumpFile (new Printer.print ())  cout "test_string" file;
	    close_out cout;
	    printverb "C file generation      : done\n";
	  end;

	printverb "Finished.\n";

        if display_op_specs then (Ltl_utils.display_operations_spec_sorted_bycase ());

	(* Some test traces. *)
	match Ltl_to_acsl_option.Test.get () with
	  | 1 -> Ltl_utils.debug_display_all_specs ()
	  | _ -> () (* 0 is no test *)
      end ;
      cleanup_files()

(* Plugin registration *)

let run =
  Dynamic.register "Ltl_to_acsl.run" (Type.func Type.unit Type.unit)
    ~journalize:true
    run

let main _fmt = if Ltl_to_acsl_option.is_on () then run ()
let () = Db.Main.extend main


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
