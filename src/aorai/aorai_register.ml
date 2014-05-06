(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

open Logic_ptree
open Promelaast

(* [VP] Need to get rid of those global references at some point. *)
let promela_file = ref ""
let ya_file = ref ""
let c_file = ref ""
let output_c_file = ref ""
let ltl_tmp_file = ref ""
let ltl_file = ref ""
let dot_file = ref ""
let generatesCFile = ref true
let ltl2ba_params = " -l -p -o "

let ltl_to_promela = Hashtbl.create 7

let set_ltl_correspondance h =
  Hashtbl.clear ltl_to_promela;
  Hashtbl.iter (fun x y -> Hashtbl.add ltl_to_promela x y) h

let convert_ltl_exprs t =
  let rec convert_cond cond =
    match cond with
        POr(c1,c2) -> POr (convert_cond c1, convert_cond c2)
      | PAnd(c1,c2) -> PAnd(convert_cond c1, convert_cond c2)
      | PNot c -> PNot (convert_cond c)
      | PCall _ | PReturn _ | PTrue | PFalse -> cond
      | PRel(Neq,PVar x,PCst _) ->
        (try 
           let (rel,t1,t2) = Hashtbl.find ltl_to_promela x in PRel(rel,t1,t2)
         with Not_found -> cond)
      | PRel _ -> cond
  in
  let rec convert_seq_elt e =
    { e with 
      condition = Extlib.opt_map convert_cond e.condition;
      nested = convert_seq e.nested; }
  and convert_seq s = List.map convert_seq_elt s in
  let convert_parsed c =
    match c with
        Seq l -> Seq (convert_seq l)
      | Otherwise -> Otherwise
  in
  let convert_trans t = { t with cross = convert_parsed t.cross } in
  List.map convert_trans t

(* Promela file *)

let syntax_error loc msg =
  Aorai_option.abort
    "File %S, line %d, characters %d-%d:@\nSyntax error: %s"
    (Filepath.pretty (fst loc).Lexing.pos_fname) (fst loc).Lexing.pos_lnum
    ((fst loc).Lexing.pos_cnum - (fst loc).Lexing.pos_bol)
    ((snd loc).Lexing.pos_cnum - (fst loc).Lexing.pos_bol)
    msg

let ltl_to_ltlLight f_ltl f_out =
  try
    let c = open_in f_ltl in
    let (ltl_form,exprs) = Ltllexer.parse c in
    close_in c;
    Ltl_output.output ltl_form f_out;
    set_ltl_correspondance exprs
  with 
    | Not_found -> Aorai_option.abort "Unknown LTL file %s" f_ltl
    | Ltllexer.Error (loc,msg) -> syntax_error loc msg

let load_ya_file f  =
  try
    let c = open_in f in
    let automata = Yalexer.parse c  in
    close_in c;
    Data_for_aorai.setAutomata automata;
  with
    | Not_found -> Aorai_option.abort "Unknown Ya file %s" f
    | Yalexer.Error (loc,msg) -> syntax_error loc msg

let load_promela_file f  =
  try
    let c = open_in f in
    let (s,t) = Promelalexer.parse c  in
    let t = convert_ltl_exprs t in
    close_in c;
    Data_for_aorai.setAutomata (s,t);
  with 
    | Not_found -> Aorai_option.abort "Unknown Promela file %s" f
    | Promelalexer.Error(loc,msg) -> syntax_error loc msg

let load_promela_file_withexps f  =
  try
    let c = open_in f in
    let automata = Promelalexer_withexps.parse c  in
    close_in c;
    Data_for_aorai.setAutomata automata;
  with 
    | Not_found -> Aorai_option.abort "Unknown Promela file %s" f
    | Promelalexer_withexps.Error(loc,msg) -> syntax_error loc msg

let display_status () =
  if Aorai_option.verbose_atleast 2 then begin
    Aorai_option.feedback "\n"  ;
    Aorai_option.feedback "C file:            '%s'\n" !c_file ;
    Aorai_option.feedback "Entry point:       '%a'\n" 
      Kernel_function.pretty (fst (Globals.entry_point())) ;
    Aorai_option.feedback "LTL property:      '%s'\n" !ltl_file ;
    Aorai_option.feedback "Files to generate: '%s' (Annotated code)\n"
      (if !generatesCFile then !output_c_file else "(none)");
    if Aorai_option.Dot.get () then
      Aorai_option.feedback "Dot file:          '%s'\n"  !dot_file;
    Aorai_option.feedback "Tmp files:         '%s' (Light LTL file)\n"
      !ltl_tmp_file ;
    Aorai_option.feedback "                   '%s' (Promela file)\n"
      !promela_file ;
    Aorai_option.feedback "\n"
  end

let init_file_names () =
  (* Intermediate functions for error display or fresh name of file
     generation *)
  let err= ref false in
  let dispErr mesg f =
    Aorai_option.error "Error. File '%s' %s.\n" f mesg;
    err:=true
  in
  let freshname pre suf =
    let rec fn p s n =
      if not (Sys.file_exists (p^(string_of_int n)^s)) then (p^(string_of_int n)^s)
      else fn p s (n+1)
    in
    let name =
      if not (Sys.file_exists (pre^suf)) then pre^suf
      else fn pre suf 0
    in name
  in

  (* c_file name is given and has to point out a valid file. *)
  c_file :=
    (match Kernel.Files.get () with
      | [] -> "dummy.i"
      | f :: _ -> f);
  if (!c_file="") then dispErr ": invalid C file name" !c_file;
  if (not (Sys.file_exists !c_file)) then dispErr "not found" !c_file;

  (* The output C file has to be a valid file name if it is used. *)
  output_c_file := (Aorai_option.Output_C_File.get ()) ;
  if (!output_c_file="") then output_c_file:=freshname ((Filename.chop_extension !c_file)^"_annot") ".c";
  (*   else if Sys.file_exists !output_c_file then dispErr "already exists" !output_c_file; *)

  if Aorai_option.Dot.get () then
    dot_file:=freshname (Filename.chop_extension !c_file) ".dot";

  if Aorai_option.Ya.get () = "" then
    if Aorai_option.Buchi.get () = "" then begin
      (* ltl_file name is given and has to point out a valid file. *)
      ltl_file := Aorai_option.Ltl_File.get ();
      if (!ltl_file="") then dispErr ": invalid LTL file name" !ltl_file;
      if (not (Sys.file_exists !ltl_file)) then dispErr "not found" !ltl_file;

      (* The LTL file is always used. *)
      (* The promela file can be given or not. *)
      if Aorai_option.To_Buchi.get () <> "" then begin
	ltl_tmp_file:=
	  freshname
	  (Filename.chop_extension
	     (Aorai_option.promela_file ())) ".ltl";
	promela_file:= Aorai_option.promela_file ();
        Extlib.cleanup_at_exit !ltl_tmp_file
      end else begin
	ltl_tmp_file:=
	  (try
	     Extlib.temp_file_cleanup_at_exit
	       (Filename.basename !c_file) ".ltl"
	   with Extlib.Temp_file_error s ->
	     Aorai_option.abort "cannot create temporary file: %s" s);
	promela_file:=
	  freshname (Filename.chop_extension !ltl_tmp_file) ".promela";
        Extlib.cleanup_at_exit !promela_file;
      end
    end else begin
      if Aorai_option.To_Buchi.get () <> "" &&
	Aorai_option.Ltl_File.get () <> ""
      then begin
	Aorai_option.error
	  "Error. '-buchi' option is incompatible with '-to-buchi' and '-ltl' \
options.";
	err:=true
      end;
      (* The promela file is used only if the process does not terminate after
	 LTL generation. *)
      promela_file := Aorai_option.promela_file ();
    end
  else begin
    ya_file := Aorai_option.Ya.get ();
    if (!ya_file="") then dispErr ": invalid Ya file name" !ya_file;
    if (not (Sys.file_exists !ya_file)) then dispErr "not found" !ya_file
  end;
  display_status ();
  !err

let init_test () =
  match Aorai_option.Test.get () with
  | 1 -> generatesCFile := false;
  | _ -> generatesCFile := true

let printverb s = Aorai_option.feedback ~level:2 "%s" s

let output () =
  (* Dot file *)
  if (Aorai_option.Dot.get()) then
    begin
      Promelaoutput.output_dot_automata (Data_for_aorai.getAutomata ())
        !dot_file;
      printverb "Generating dot file    : done\n"
    end;

  (* C file *)
  if (not !generatesCFile) then
    printverb "C file generation      : skipped\n"
  else
    begin
      let cout = open_out !output_c_file in
      let fmt = Format.formatter_of_out_channel cout in
      Kernel.Unicode.without_unicode
        (fun () ->
          File.pretty_ast ~fmt ();
	  close_out cout;
	  printverb "C file generation      : done\n";
        ) ()
    end;
  
  printverb "Finished.\n";
  (* Some test traces. *)
  Data_for_aorai.debug_computed_state ();
  if !generatesCFile then Kernel.Files.set [!output_c_file]

let work () =
  let file = Ast.get () in
  Aorai_utils.initFile file;
  printverb "C file loading         : done\n";
  if Aorai_option.Ya.get () = "" then
    if Aorai_option.Buchi.get () = "" then begin
      ltl_to_ltlLight !ltl_file !ltl_tmp_file;
      printverb "LTL loading            : done\n";
      let cmd = Format.sprintf "ltl2ba %s -F %s > %s"
	ltl2ba_params !ltl_tmp_file !promela_file
      in if Sys.command cmd <> 0 then
          Aorai_option.abort "failed to run: %s" cmd ;
      printverb "LTL ~> Promela (ltl2ba): done\n"
    end;
  if Aorai_option.To_Buchi.get () <> "" then
    printverb ("Finished.\nGenerated file: '"^(!promela_file)^"'\n")
  else
    begin
	(* Step 3 : Loading promela_file and checking the consistency between informations from C code and LTL property *)
	(*          Such as functions name and global variables. *)

      if Aorai_option.Buchi.get () <> "" then
	load_promela_file_withexps !promela_file
      else if Aorai_option.Ya.get  () <> "" then
        load_ya_file !ya_file
      else
	load_promela_file !promela_file;
      printverb "Loading promela        : done\n";
	(* Computing the list of ignored functions *)
	(* 	Aorai_visitors.compute_ignored_functions file; *)


	(* Promelaoutput.print_raw_automata (Data_for_aorai.getAutomata());  *)
	(* Data_for_aorai.debug_ltl_expressions (); *)

      (*let _ = Path_analysis.test (Data_for_aorai.getAutomata())in*)
      let root = fst (Globals.entry_point ()) in
      if (Aorai_option.Axiomatization.get()) then
	begin
	    (* Step 5 : incrementing pre/post 
               conditions with states and transitions information *)
	  printverb "Refining pre/post      : \n";
          Aorai_dataflow.compute ();
	    (* Step 6 : Removing transitions never crossed *)
	  if (Aorai_option.AutomataSimplification.get()) then
	    begin
	      printverb "Removing unused trans  : done\n";
	      Data_for_aorai.removeUnusedTransitionsAndStates ();
	    end
	  else
	    printverb "Removing unused trans  : skipped\n";
	    (* Step 7 : Labeling abstract file *)
	    (* Finally the information is added into the Cil automata. *)
	  Aorai_utils.initGlobals root (Aorai_option.Axiomatization.get());
 	  Aorai_visitors.add_sync_with_buch file;
	  Aorai_visitors.add_pre_post_from_buch file
	    (Aorai_option.advance_abstract_interpretation ());
	  printverb "Annotation of Cil      : done\n";
	end
      else
	begin
	    (* Step 4': Computing the set of possible pre-states and post-states of each function *)
	    (*          And so for pre/post transitions *)
	  printverb "Abstracting pre/post   : skipped\n";
          
	    (* Step 5': incrementing pre/post conditions with states and transitions information *)
	  printverb "Refining pre/post      : skipped\n";


	    (* Step 6 : Removing transitions never crossed *)
          printverb "Removing unused trans  : skipped\n";

	    (* Step 7 : Labeling abstract file *)
	    (* Finally the information is added into the Cil automata. *)
	  Aorai_utils.initGlobals root (Aorai_option.Axiomatization.get());
 	  Aorai_visitors.add_sync_with_buch file;
	  printverb "Annotation of Cil      : partial\n"
	end;
      
      (* Step 8 : clearing tables whose information has been
         invalidated by our transformations.
      *)
      Cfg.clearFileCFG ~clear_id:false file;
      Cfg.computeFileCFG file;
      Ast.clear_last_decl ();
      let prj =
        File.create_project_from_visitor "aorai"
	  (fun prj -> new Visitor.frama_c_copy prj)
      in
      Project.copy ~selection:(Parameter_state.get_selection ()) prj;
      Project.on prj output ()
    end

let run () =
  Aorai_option.result "Welcome to the Aorai plugin@.";
  init_test ();

  (* Step 1 : Capture files names *)
  let error_status = init_file_names () in
  (* Treatment is done only if parameters are valid *)
  if error_status then
    Aorai_option.error "Generation stopped."
  else

    (* Step 2 : Work in our own project, initialized by a copy of the main
       one. *)
    let work_prj =
      File.create_project_from_visitor "aorai_tmp"
	(fun prj -> new Visitor.frama_c_copy prj)
    in
    Project.copy ~selection:(Parameter_state.get_selection ()) work_prj;
    Project.on work_prj work ();
    Project.remove ~project:work_prj ()

(* Plugin registration *)

let run =
  Dynamic.register
    ~plugin:"Aorai"
    "run"
    (Datatype.func Datatype.unit Datatype.unit)
    ~journalize:true
    run

let run, _ =
  State_builder.apply_once
    "Aorai"
    (let module O = Aorai_option in
     [ O.Ltl_File.self; O.To_Buchi.self; O.Buchi.self;
      O.Ya.self; O.Axiomatization.self; O.ConsiderAcceptance.self;
      O.AutomataSimplification.self; O.AbstractInterpretation.self;
      O.AddingOperationNameAndStatusInSpecification.self ])
    run

let main () = if Aorai_option.is_on () then run ()
let () = Db.Main.extend main


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
