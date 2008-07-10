(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(** Frama-C Main.
    @plugin developer guide *)

open Pretty
open Cil
open Cilutil

open Format

let all_plugins (fmt:formatter) =
  (* Standard output for printing specific code outputs *)
  let out =
    let out_cmdline = Cmdline.CodeOutput.get () in
    if out_cmdline = "" then fmt else
      try
        let out = open_out out_cmdline in
        let fmt = Format.formatter_of_out_channel out in
        at_exit (fun () -> Format.pp_print_flush fmt (); close_out out);
        fmt
      with Sys_error s ->
        fprintf fmt
          "Warning: could not open %s for code output:\n%s.\n\
       I will output the code on stdout instead@."
          out_cmdline s;
        fmt
  in
  let files = Cil_state.file () in
  try begin

    if Cmdline.Obfuscate.get () then begin
      let dictionary = Obfuscate.obfuscate files in
      fprintf out "// Start of dictionary for obfuscation:@\n";
      Hashtbl.iter
        (fun k v -> fprintf out "#define %s %s@\n" k v)
        dictionary;
      fprintf out "// End of dictionary for obfuscation.@\n";
      Format.fprintf out "@[%a@]" (d_file (new Printer.print())) files;
      exit 0
    end;

    if Cmdline.Debug.get () > 0 then
      Format.fprintf out "@[%a@]" (d_file (new Printer.print())) files;

    if Cmdline.Constfold.get () then begin
      visitCilFileSameGlobals (constFoldVisitor true) files
    end;

    if Cmdline.Metrics.is_on () then begin
      let loc = Metrics.sloc files in
      if Cmdline.Metrics.Print.get () then
	fprintf fmt "Syntactic metrics@\n %a@\n" Metrics.pretty loc;
      if Cmdline.Metrics.Dump.is_set () then
	Metrics.dump (Cmdline.Metrics.Dump.get ()) loc
    end;

    let kf () = fst (Globals.entry_point ()) in

    (* Memoization of context free functions *)
    let mem_functions = Cmdline.MemFunctions.get () in
    if Cmdline.MemExecAll.get ()
      || not (StringSet.is_empty mem_functions)
    then begin
      fprintf fmt "====== MEMOIZING FUNCTIONS ======@.";
      Globals.Functions.iter
	(fun kf ->
	   let name = Kernel_function.get_name kf in
	   if (Kernel_function.is_definition kf) &&
	     (Cmdline.MemExecAll.get ()
	      || StringSet.mem name mem_functions)
	   then begin
	     fprintf fmt "== function %a@." Kernel_function.pretty_name kf;
	     try
	       !Db.Value.memoize kf
             with Db.Value.Aborted ->
	       fprintf fmt "Cannot memoize %a: Analysis degenerated@."
		 Kernel_function.pretty_name kf;
	       exit 1
	   end)
    end;

    (* put it in Cmdline. *)
    let force_slicing = Cmdline.Slicing.is_on () in
    let force_sparecode = Cmdline.Sparecode.Analysis.get () in
    let force_pdg = (* force_slicing || force_sparecode
	               || *) Cmdline.Pdg.BuildAll.get ()
      || not (StringSet.is_empty (Cmdline.Pdg.BuildFct.get ()))
    in
    let force_semantic_folding =
      Cmdline.Constant_Propagation.SemanticConstFolding.get ()
      || not (StringSet.is_empty
		(Cmdline.Constant_Propagation.SemanticConstFold.get ()))
    in
    (* **** *)

    let not_quiet = not (Cmdline.Quiet.get ()) in

    (* Value computations *)
    if Cmdline.ForceValues.get () then begin
      !Db.Value.compute ();
      (* !Db.Outputs.compute (kf()); *)
      if not_quiet then fprintf fmt "@\n====== VALUES COMPUTED ======@."
    end;

    if Cmdline.ForceOut.get ()
      || Cmdline.ForceInput.get ()
      || Cmdline.ForceInout.get ()
      || Cmdline.ForceDeref.get ()
      || Cmdline.ForceValues.get ()
    then begin
      !Db.Semantic_Callgraph.topologically_iter_on_functions
	(fun kf ->
	   if Kernel_function.is_definition kf then begin
	     if Cmdline.ForceOut.get () then
               (if not_quiet then !Db.Outputs.display fmt
                else !Db.Outputs.compute) kf;
	     if Cmdline.ForceInput.get () then
	       (if not_quiet then !Db.Inputs.display fmt
                else !Db.Inputs.compute) kf;
	     if Cmdline.ForceInout.get () then
	       (if not_quiet then !Db.InOutContext.display fmt
                else !Db.InOutContext.compute) kf;
	     if Cmdline.ForceDeref.get () then
	       (if not_quiet then !Db.Derefs.display fmt
                else !Db.Derefs.compute) kf;
	     if not_quiet && Cmdline.ForceValues.get () then
	       Db.Value.display fmt kf;
	   end)
    end;
    if Cmdline.ForceDeps.get () then begin
      !Db.Semantic_Callgraph.topologically_iter_on_functions
	(fun kf ->
	   if Kernel_function.is_definition kf && !Db.Value.is_called kf
	   then !Db.From.compute kf) ;
      if not_quiet then
        begin 
          !Db.From.display fmt;
          fprintf fmt "@\n====== DEPENDENCIES COMPUTED ======@."
        end
    end;
    if not_quiet && Cmdline.ForceCallDeps.get ()
    then begin
      fprintf fmt "@\n====== DISPLAYING CALLWISE DEPENDENCIES ======@.";
      !Db.From.Callwise.iter
	(fun ki d ->
           let id,typ =
	     match ki with
               Cil_types.Kglobal ->
                 ("entry point",
                  Kernel_function.get_type (kf ()))
	     | Cil_types.Kstmt s ->
                 (string_of_int s.Cil_types.sid,
                    let called = Db.Value.call_to_kernel_function s in
                    match called with
                      [] -> assert false
                    | f::_ -> Kernel_function.get_type f)
             in
	     fprintf fmt "@[call %s:@ %a@\n@]"
	       id (Function_Froms.pretty_with_type typ) d)
	;
	fprintf fmt "@\n====== END OF CALLWISE DEPENDENCIES ======@."
      end;
      if Cmdline.ForceUsers.get ()
      then begin
	fprintf fmt "@\n====== DISPLAYING USERS ======@.";
	!Db.Semantic_Callgraph.topologically_iter_on_functions
	  (fun kf ->
	     try
	       fprintf fmt "%a: @[%a@]@\n"
		 Kernel_function.pretty_name kf
		 Kernel_function.Set.pretty (!Db.Users.get kf)
	     with Not_found -> () (* k is not called during analysis *))
	;
	fprintf fmt "@\n====== END OF USERS ======@."
      end;
      if Cmdline.ForceMemzones.get () then begin
	fprintf fmt
	  "@[Memory zones:@\n @[%a@]@]"
	  !Db.Memzone.pretty (!Db.Memzone.compute ())
      end;

      if Cmdline.ForceAccessPath.get () then
	!Db.Semantic_Callgraph.topologically_iter_on_functions
	  (fun kf ->
	     if Kernel_function.is_definition kf && !Db.Value.is_called kf then
	       let state =
		 Db.Value.get_state
		   (Cil_types.Kstmt (Kernel_function.find_first_stmt kf))
	       in
	       let inputs = !Db.InOutContext.get_internal kf in
	       let s = !Db.Access_path.compute state
		 (Cvalue_type.Model.fold_base
		    (fun base acc -> BaseUtils.BaseSet.add base acc)
		    (Relations_type.Model.value_state state)
		    BaseUtils.BaseSet.empty)
	       in
	       fprintf fmt
		 "Filtered access_path for %a :@ %a@."
		 Kernel_function.pretty_name kf
		 !Db.Access_path.pretty
		 (!Db.Access_path.filter s
		    (Locations.Zone.filter_base
		       (fun b -> not (Base.is_local b
					(Kernel_function.get_definition kf)))
		       inputs.Inout_type.over_inputs)));

      if force_semantic_folding
      then begin
	fprintf fmt "@\n[constant propagation] in progress...@.";
	let fnames = Cmdline.Constant_Propagation.SemanticConstFold.get () in
        let cast_intro = Cmdline.Constant_Propagation.CastIntro.get () in
	let propagated = !Db.Constant_Propagation.run_propagation fnames cast_intro in
	if Cmdline.Constant_Propagation.SemanticConstFolding.get () then
	  File.pretty out ~prj:propagated;
	fprintf fmt "@\n====== CONSTANT PROPAGATED ======@.";
      end;

      if force_pdg then begin
	fprintf fmt "@\n[pdg] in progress...@.";
	let do_kf_pdg kf =
	  let fname = Kernel_function.get_name kf in
	  if Cmdline.Pdg.BuildAll.get () ||
	    StringSet.mem fname (Cmdline.Pdg.BuildFct.get ())
	  then begin
	    let pdg = !Db.Pdg.get kf in
            let bw  = Cmdline.Pdg.PrintBw.get () in
	    fprintf fmt "@[%a@]@." (!Db.Pdg.pretty ~bw) pdg;
	    if Cmdline.Pdg.DotBasename.get () <> "" then
              !Db.Pdg.extract pdg
		(Cmdline.Pdg.DotBasename.get ()^"."^fname^".dot")
	  end
	in
        !Db.Semantic_Callgraph.topologically_iter_on_functions do_kf_pdg;
	if Cmdline.Pdg.BuildAll.get () then
	  fprintf fmt "@\n====== PDG GRAPH COMPUTED ======@.";
      end;

      if Cmdline.Pdg.DotPostdomBasename.get () <> "" then begin
	let base = Cmdline.Pdg.DotPostdomBasename.get () in
	let print kf = !Db.Postdominators.print_dot base kf in
	!Db.Semantic_Callgraph.topologically_iter_on_functions print
      end;

      if force_sparecode then begin
	fprintf fmt "@\n[sparecode] in progress...@.";
        let select_annot = not (Cmdline.Sparecode.NoAnnot.get ())in
        let select_slice_pragma = true in
	let new_proj = !Db.Sparecode.run select_annot select_slice_pragma in
	File.pretty out ~prj:new_proj ;
	fprintf fmt "@\n====== UNUSED CODE DETECTED ======@."
      end;

      if force_slicing then begin
	fprintf fmt "@\n[slicing] in progress...@.";

        (* have to do the value analysis before the selections
         * because some functions use its results,
         * and the value analysis is not launched automatically. *)
        if not (Db.Value.is_computed ()) then
          !Db.Value.compute ();
        
	let project = Db.Slicing.Project.mk_project "Slicing" in
	  Db.Slicing.Project.set_project (Some project);
	  !Db.Slicing.Request.add_persistent_cmdline project;
          
	  (* Apply all pending requests. *)
	  if Cmdline.Slicing.Mode.Verbose.get () > 2 then
	    fprintf fmt "[slicing] requests:@\n %a@\n"
	      !Db.Slicing.Request.pretty project ;
	  !Db.Slicing.Request.apply_all_internal project;
          
          if Cmdline.Slicing.Mode.Callers.get () then
            !Db.Slicing.Slice.remove_uncalled project;
          
	  let sliced_project = 
	    !Db.Slicing.Project.extract ((!Db.Slicing.Project.get_name project)^ " export") project
	  in
	    if Cmdline.Slicing.Print.get () then
              File.pretty out ~prj:sliced_project;
            
	    fprintf fmt "@\n====== SLICED CODE COMPUTED ======@.";
      end;

      if Cmdline.WpCfg.get () then begin
	!Db.Semantic_Callgraph.topologically_iter_on_functions
	  (fun kf' ->
	     match kf'.Db_types.fundec with
	     | Db_types.Declaration _ -> ()
	     | Db_types.Definition _ ->
		 let kf = kf () in
		 if Kernel_function.equal kf kf' then begin
		   !Db.Properties.compute_wp kf;
		   Db.Properties.prove kf
		 end)
      end;

      if Cmdline.Security.is_on () then begin
	!Db.Value.compute ();
	!Db.Security.run_whole_analysis ()
      end;

      if not (Cmdline.Impact.Pragma.is_empty ()) then 
	!Db.Impact.compute_pragmas ();

      if Cmdline.Occurrence.Print.get () then 
	!Db.Occurrence.print_all ();

      if Cmdline.Jessie.Analysis.get () then begin
	!Db.Jessie.run_analysis ()
      end
    end;

    (* Printing *)
    if Cmdline.PrintCode.get () then
      Format.fprintf out "@[%a@]" (d_file (new Printer.print ())) files;
    Cilutil.flush_all ()

  with Globals.No_such_entry_point msg -> (* Db.entry_point doesn't work *)
    Format.printf "%s@." msg

let global_toplevel_startup_hook () =
  try
    let ignore_files = Options.init_from_options () in
    if not ignore_files then begin
      try File.init_from_cmdline ()
      with Cil_state.Bad_Initialisation _ -> assert false
    end;
    all_plugins Format.std_formatter;

  with Failure "" (*Sys.Break*) as e ->
    warn "user interrupted computations. Dumping whatever we may have at this point.";
    if Cmdline.ForceValues.get ()
      || Cmdline.ForceDeps.get ()
      || Cmdline.ForceOut.get () then
	Globals.Functions.iter
          (fun kf -> if Kernel_function.is_definition kf then begin
	     if Cmdline.ForceOut.get () then
	       !Db.Outputs.display Format.std_formatter kf;
	     if Cmdline.ForceValues.get () then
	       Db.Value.display Format.std_formatter kf;
           end);
    raise e

let () =
  Options.add_plugin
    ~name:"Command Line Interface"
    ~descr:"used for batch processing"
    ~toplevel_init:global_toplevel_startup_hook
    [];
  Db.Toplevel.replay := global_toplevel_startup_hook;
  Db.Toplevel.run_all_plugins := all_plugins

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
