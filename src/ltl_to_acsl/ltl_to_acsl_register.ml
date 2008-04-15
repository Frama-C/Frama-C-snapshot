(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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
(**************************************************************************)

(* $Id: ltl_to_acsl_register.ml,v 1.4 2008/11/06 13:03:28 uid568 Exp $ *)

open Promelaast
open Ltl_utils
 
let promela_file = ref ""
let c_file = ref ""
let output_c_file = ref ""
let ltl_tmp_file = ref ""
let ltl_file = ref ""
let dot_file = ref ""
let verbose = ref false
let root = ref ""

(* Promela file *)

let load_promela_file f  = 
  try 
    let c = open_in f in
    let (automata,auto_vars,auto_funs) = Promelalexer.parse c in
    close_in c;
    Data_for_ltl.setAutomata automata auto_vars auto_funs
  with Not_found as ex -> 
    Format.printf "Problem with file : %s\n" f;
    raise ex

let ltl_to_promela f_ltl f_out = 
  try 
    let c = open_in f_ltl in
    let (ltl_form,ltl_exps) = Ltllexer.parse c in
    close_in c;
    Data_for_ltl.setLtl_expressions ltl_exps;
    Ltl_output.output ltl_form f_out
  with Not_found as ex -> 
    Format.printf "Problem with file : %s\n" f_ltl;
    raise ex







let display_status () =
  if !verbose then begin
    Format.printf "\n"  ;
    Format.printf "C file:            '%s'\n" !c_file ;
    Format.printf "Entry point:       '%s'\n" !root ;
    Format.printf "LTL property:      '%s'\n" !ltl_file ;
    Format.printf "Files to generate: '%s' (Annotated code)\n"  !output_c_file ; 
    if (Cmdline.Ltl_to_acsl.Dot.get()) then 
      Format.printf "Dot file:          '%s'\n"  !dot_file; 
    Format.printf "Tmp files:         '%s' (Light LTL file)\n" !ltl_tmp_file ;
    Format.printf "                   '%s' (Promela file)\n" !promela_file ;
    Format.printf "\n"  ;
    Format.print_flush ()
  end

let display_operations_spec () =
  begin
    Format.printf "\n########\n# Operations specification:\n#\n";
    Globals.Functions.iter 
      (fun f -> 
	 let name = (Kernel_function.get_name f) in
	 Format.printf "#   ";
	 Ltl_utils.debug_display_stmt_all_pre (Data_for_ltl.get_func_pre name);
	 Format.printf " %s  " name;
	 Ltl_utils.debug_display_stmt_all_pre_bycase (Data_for_ltl.get_func_post_bycase name);
	 Format.printf "\n";
      );
    Format.printf "#\n# End of operations specification\n########\n";
    Format.printf "\n";
  end

let init_file_names () =

  (* Intermediate functions for error display or fresh name of file generation *)
  let err= ref false in 
  let dispErr mesg f = 
    Format.printf "Error. File '%s' %s.\n" f mesg;
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
  c_file := List.hd (Cmdline.Files.get ());
  if (!c_file="") then dispErr ": invalid C file name" !c_file;
  if (not (Sys.file_exists !c_file)) then dispErr "not found" !c_file;

  (* The output C file has to be a valid file name if it is used. *)
  output_c_file := (Cmdline.Ltl_to_acsl.Output_C_File.get ()) ; 
  if (!output_c_file="") then output_c_file:=freshname ((Filename.chop_extension !c_file)^"_annot") ".c" 
  else if Sys.file_exists !output_c_file then dispErr "already exists" !output_c_file;

  if (Cmdline.Ltl_to_acsl.Dot.get()) then 
    dot_file:=freshname (Filename.chop_extension !c_file) ".dot";

  if not (Cmdline.Ltl_to_acsl.OnlyFromPromela.get ()) then
    begin
      (* ltl_file name is given and has to point out a valid file. *)
      ltl_file := Cmdline.Ltl_to_acsl.Ltl_File.get ();
      if (!ltl_file="") then dispErr ": invalid LTL file name" !ltl_file;
      if (not (Sys.file_exists !ltl_file)) then dispErr "not found" !ltl_file;

      (* The tmp ltl file can be given or not. *)
      if Cmdline.Ltl_to_acsl.OnlyToLTL.get () then 
	ltl_tmp_file:=(Cmdline.Ltl_to_acsl.Promela_File.get ())
      else
	ltl_tmp_file:=Filename.temp_file !c_file ".ltl"; 

      (* The promela file is used only if the process does not terminate after LTL generation. *)
      promela_file:=freshname (Filename.chop_extension !ltl_tmp_file) ".promela" ;
    end
  else
    begin
      if (Cmdline.Ltl_to_acsl.OnlyToLTL.get ()) || ((Cmdline.Ltl_to_acsl.Ltl_File.get ()) <>"") then begin
	Format.printf "Error. '-buchi' option is incompatible with '-to-buchi' and '-ltl' options.";
	err:=true
      end;

      (* The promela file is used only if the process does not terminate after LTL generation. *)
      promela_file:= Cmdline.Ltl_to_acsl.Promela_File.get ();
    end;

  if Globals.has_entry_point () then 
    begin
      let ep,_ =Globals.entry_point () in
      root:=Kernel_function.get_name ep
    end
  else
    begin
      Format.printf "The file '%s' seems to do not have any entry point." !c_file;
      Format.printf "Generation stopped.";
      assert false;
    end;
  display_status ();
  !err

let run () =
  if Cmdline.Ltl_to_acsl.Verbose.get () then verbose:=true;
  Format.printf "Welcome in the LTL to ACSL plugin\n";
  (* Step 1 : Capture files names *)
  let error_status = init_file_names () in
  (* Treatment is done only if parameters are valid *)
  if error_status then 
    Format.printf "Generation stopped."
  else
    (* Step 2 : Work in our own project, initialized by a copy of the main one. *)
    let prj = Project.create "ltl_to_acsl" in
    File.init_project_from_visitor prj (fun prj -> new Visitor.frama_c_copy prj);
    Project.copy ~only:(Cmdline.get_selection ()) prj;
    Project.set_current prj;
    let file = Cil_state.file () in
    Ltl_utils.initFile file;
    if !verbose then Format.printf "C file loading         : done\n";
    if not (Cmdline.Ltl_to_acsl.OnlyFromPromela.get ()) then begin
      ltl_to_promela !ltl_file !ltl_tmp_file;
      if !verbose then Format.printf "LTL loading            : done\n";
    end;
    if Cmdline.Ltl_to_acsl.OnlyToLTL.get ()  then 
      begin
	if !verbose then 
	  Format.printf "Finished.\nGenerated file: '%s'\n" !ltl_tmp_file 
      end
    else
      begin
	if not (Cmdline.Ltl_to_acsl.OnlyFromPromela.get ()) then begin
	  ignore (Sys.command (Format.sprintf "ltl2ba -F %s > %s" !ltl_tmp_file !promela_file ));
	  if !verbose then Format.printf "LTL ~> Promela (ltl2ba): done\n"
	end;
(* Step 2 : Loading promela_file and checking the consistency between informations from C code and LTL property *)
(*          Such as functions name and global variables. *)
	load_promela_file !promela_file;
	if !verbose then Format.printf "Loading promela        : done\n";
	if (Cmdline.Ltl_to_acsl.Dot.get()) then 
	  begin
	    Promelaoutput.output_dot_automata (Data_for_ltl.getAutomata ()) !dot_file;
	    if !verbose then Format.printf "Generating dot file    : done\n"
	  end; 
(* Step 3 : Computing the set of possible pre-states and post-states of each function *)
(*          And so for pre/post transitions *)
	Ltl_utils.initGlobals !root; 
	Ltl_to_acsl_visitors.compute_abstract file !root;
	if !verbose then Format.printf "Abstracting pre/post   : done\n";
(* Step 4 : incrementing pre/post conditions with states and transitions information *)
	if !verbose then Format.printf "Refining pre/post      : (Without switch instruction)\n";
	if not (Cmdline.Ltl_to_acsl.AbstractInterpretation.get()) then
	  begin
	    (* Repeat until reach a fix-point *)
            while 
	      Abstract_ai.propagates_pre_post_constraints file !root 
	    do () done;
	    if !verbose then Format.printf "    Forward/backward abstract specification        : done\n";
	  end
	else
	  if !verbose then Format.printf "    Forward/backward abstract specification        : skiped\n";

(*      Promelaoutput.print_raw_automata (Data_for_ltl.getAutomata()); *)

        Bycase_ai.init_specification();
	if not (Cmdline.Ltl_to_acsl.AdvanceAbstractInterpretation.get()) then 
	  begin
            (* Repeat until reach a fix-point *)
            while 
	      Bycase_ai.propagates_pre_post_constraints_bycase file !root ;
     	    do () done;
	    if !verbose then Format.printf "    Consider links between input and output states : done\n";
	  end
	else
	  if !verbose then Format.printf "    Consider links between input and output states : skiped\n";

(* Step 5 : Generating resulting file *)

	(* Finally the information is added into the Cil automata. *)
 	Ltl_to_acsl_visitors.add_sync_with_buch file;
	Ltl_to_acsl_visitors.add_pre_post_from_buch file ;
	if !verbose then Format.printf "Annotation of Cil      : done\n";

	(* Generating the annotated C file *)
        let cout = open_out !output_c_file in
	Cil.print_utf8:=false;
	Cil.dumpFile (new Printer.print ())  cout "test_string" file;
	close_out cout;
	if !verbose then Format.printf "C file generation      : done\n";

	if !verbose then Format.printf "Finished.\n";

        if Cmdline.Ltl_to_acsl.Output_Spec.get ()  then (display_operations_spec ());
	
      end

(* Plugin registration *)
 
let main _fmt =
  if Cmdline.Ltl_to_acsl.Analysis.get () then !Db.Ltl_to_acsl.run ()

let () = Db.Main.extend main
let () = Db.Ltl_to_acsl.run:= run
let () = 
  Options.add_plugin
    ~name:"Aorai (Aka ltl_to_acsl -- EXPERIMENTAL)"
    ~descr:"Plugin for verification of LTL property on C code"
    ["-ltl",
     Arg.Tuple[
       Arg.Unit Cmdline.SimplifyCfg.on;
       Arg.Unit Cmdline.KeepSwitch.on;
       Arg.Unit Cmdline.Ltl_to_acsl.Analysis.on;
       Arg.String Cmdline.Ltl_to_acsl.Ltl_File.set
     ],
     "<s> : Specifies file name for LTL property";
     
     "-to-buchi",
     Arg.Tuple[
       Arg.Unit Cmdline.SimplifyCfg.on;
       Arg.Unit Cmdline.KeepSwitch.on;
       Arg.Unit Cmdline.Ltl_to_acsl.OnlyToLTL.on;
       Arg.String Cmdline.Ltl_to_acsl.Promela_File.set
     ],
     "<s> : Only generates the buchi automata (in Promela language) in file 's'.";

     "-buchi",
     Arg.Tuple[
       Arg.Unit Cmdline.SimplifyCfg.on;
       Arg.Unit Cmdline.KeepSwitch.on;
       Arg.Unit Cmdline.Ltl_to_acsl.Analysis.on;
       Arg.Unit Cmdline.Ltl_to_acsl.OnlyFromPromela.on;
       Arg.String Cmdline.Ltl_to_acsl.Promela_File.set
     ],
     "<s> : Considers the property described by the buchi automata (in Promela language) from file 's'.";

     "-ltl-verbose",
     Arg.Unit Cmdline.Ltl_to_acsl.Verbose.on,
     ": Gives some information during computation.";

     "-show-op-spec",
     Arg.Unit Cmdline.Ltl_to_acsl.Output_Spec.on,
     ": Displays computed pre and post-condition of each operation.";

     
     "-output-c-file",
     Arg.String Cmdline.Ltl_to_acsl.Output_C_File.set,
     "<s> : Specifies generated file name for annotated C code";

     "-ltl-dot",
     Arg.Unit Cmdline.Ltl_to_acsl.Dot.on,
     ": Generates a dot file of the Buchi automata.";

     "-ltl-AI-off",
     Arg.Unit Cmdline.Ltl_to_acsl.AbstractInterpretation.on,
     ": Does not use abstract interpretation";

     "-ltl-advance-AI-off",
     Arg.Unit Cmdline.Ltl_to_acsl.AdvanceAbstractInterpretation.on,
     ": Does not use advance abstract interpretation";
    ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
