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

(* $id: cvalue.ml,v 1.36 2006/03/17 13:54:56 uid527 Exp $ *)
open Abstract_interp

(*
(** Initialize the default bound for absolute address to max addressable bit *)
let () = Cmdline.max_valid_absolute_address := Utils.memory_size ()
*)

let main fmt =
  let not_quiet = not (Cmdline.Quiet.get ()) in
  (* Memoization of context free functions *)
  let mem_functions = Cmdline.MemFunctions.get () in
  if Cmdline.MemExecAll.get ()
    || not (Cilutil.StringSet.is_empty mem_functions)
  then begin
    if not_quiet then Format.fprintf fmt "====== MEMOIZING FUNCTIONS ======@.";
    Globals.Functions.iter
      (fun kf ->
	 let name = Kernel_function.get_name kf in
	 if Kernel_function.is_definition kf &&
	   (Cmdline.MemExecAll.get () 
	    || Cilutil.StringSet.mem name mem_functions)
	 then begin
	   if not_quiet then 
	     Format.fprintf fmt "== function %a@." 
	       Kernel_function.pretty_name kf;
	   try
	     !Db.Value.memoize kf
           with Db.Value.Aborted ->
	     Format.fprintf fmt "Cannot memoize %a: Analysis degenerated@."
	       Kernel_function.pretty_name kf;
	     exit 1
	 end)
  end;
  (* Value computations *)
  if Cmdline.ForceValues.get () then begin
    !Db.Value.compute ();
    (* !Db.Outputs.compute (kf()); *)
    if not_quiet then 
      begin
        !Db.Scope.check_asserts ();
        Format.fprintf fmt "@\n====== VALUES COMPUTED ======@.";
      end
  end;
  (* Computing others results and displaying results *)
  if Cmdline.ForceOut.get ()
    || Cmdline.ForceInput.get ()
    || Cmdline.ForceInputWithFormals.get ()
    || Cmdline.ForceInout.get ()
    || Cmdline.ForceDeref.get ()
    || Cmdline.ForceValues.get ()
  then begin
    !Db.Semantic_Callgraph.topologically_iter_on_functions
      (fun kf ->
	 if Kernel_function.is_definition kf then begin
	   let result display compute =
	     (if not_quiet then display fmt else compute) kf
	   in
	   if Cmdline.ForceOut.get () then 
	     result !Db.Outputs.display !Db.Outputs.compute;
	   if Cmdline.ForceInput.get () then
	     result !Db.Inputs.display !Db.Inputs.compute;
	   if Cmdline.ForceInputWithFormals.get () then
	     if not_quiet then !Db.Inputs.display_with_formals fmt kf
             (*else  !Db.Inputs.compute_with_formals) kf *) ;
	   if Cmdline.ForceInout.get () then
	     result !Db.InOutContext.display !Db.InOutContext.compute;
	   if Cmdline.ForceDeref.get () then
	     result !Db.Derefs.display !Db.Derefs.compute;
	   if not_quiet && Cmdline.ForceValues.get () then
	     Db.Value.display fmt kf;
	 end)
  end

let () = Db.Main.extend main

let () =
  Options.add_plugin ~name:"value analysis" ~descr:"" ~shortname:"val"
    [
      "-val",
      Arg.Unit Cmdline.ForceValues.on,
      ": force values computations";

      "-mem-exec",
      Arg.String Cmdline.MemFunctions.add,
      "name: do not unroll calls to function name (experimental)"
      ;
      "-mem-exec-all",
      Arg.Unit Cmdline.MemExecAll.on
        ,": (experimental)"
      ;
      "-memory-footprint",
      Arg.Int Cmdline.MemoryFootprint.set,
      Format.sprintf "n : tell the analyser to compromise towards speed or towards low memory use. 1 : small memory; 2 : medium (suitable for recent notebooks); 3 : big (suitable for workstations with 3Gb physical memory or more). Defaults to %d."
        (Cmdline.MemoryFootprint.get ());
      "-float-digits",
      Arg.Int Cmdline.FloatDigits.set,
      Format.sprintf "n : display this number of digits when printing floats. Defaults to %d."
        (Cmdline.FloatDigits.get ());
      "-propagate-top",
      Arg.Unit Cmdline.PropagateTop.on,
      ": do not stop value analysis even if state is degenerating";

      "-plevel",
      Arg.Int Cmdline.ArrayPrecisionLevel.set,
      Format.sprintf "n : use n as the precision level for arrays accesses. Array accesses are precise as long as the interval for the index contains less than n values. (defaults to %d)"
        (Cmdline.ArrayPrecisionLevel.get ());
      "-slevel",
      Arg.Int Cmdline.SemanticUnrollingLevel.set,
      Format.sprintf "n : use n as number of path to explore in parallel EXPERIMENTAL (defaults to %d)"
        (Cmdline.SemanticUnrollingLevel.get ());
      "-wlevel",
      Arg.Int Cmdline.WideningLevel.set,
      Format.sprintf "n : set n as number of iterations before widening (defaults to %d)"
        (Cmdline.WideningLevel.get ());
      (*     "-wvar",
     Arg.String (fun v ->
             Cmdline.widen_variables := Cilutil.StringSet.add v !Cmdline.widen_variables)
             ,"name : widen only on the specified variable name"
             ;
      *)


     "-absolute-valid-range",
      Arg.String (fun s ->
                    try Scanf.sscanf s "%Li-%Li"
                      (fun min max ->
                         Cmdline.MinValidAbsoluteAddress.set
                           (Int.of_int64 (Int64.mul 8L min));
                         Cmdline.MaxValidAbsoluteAddress.set
                           (Int.of_int64
                              (Int64.pred (Int64.mul 8L (Int64.succ max)))))
                    with End_of_file | Scanf.Scan_failure _ | Failure _ as e ->
                     Format.eprintf "Invalid -absolute-valid-range integer-integer: each integer may be in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and has to hold in 64 bits. A correct example is -absolute-valid-range 1-0xFFFFFF0.@\nError was %S@."
                       (Printexc.to_string e);
                      exit 1),
      "min-max : min and max must be integers in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and hold in 64 bits. Assume that that all absolute addresses outside of this range are invalid. Without this option all absolute addresses are assumed to be invalid.";
      "-context-depth",
      Arg.Int Cmdline.AutomaticContextMaxDepth.set,
      Format.sprintf "n : use n as the depth of the default context for value analyses. (defaults to %d)"
        (Cmdline.AutomaticContextMaxDepth.get ());
      "-context-width",
      Arg.Int Cmdline.AutomaticContextMaxWidth.set,
      Format.sprintf "n : use n as the width of the default context for value analyses. (defaults to %d)"
        (Cmdline.AutomaticContextMaxWidth.get ());
      "-context-valid-pointers",
      Arg.Unit Cmdline.AllocatedContextValid.on,
      Format.sprintf ": context generation will only allocate valid pointers until the -context-depth and then use NULL (defaults to %b)"
        (Cmdline.AllocatedContextValid.get ());
     "-no-overflow",
      Arg.Unit Cmdline.IgnoreOverflow.on
       ,": assume that arithmetic operations never overflow"
     ;
     "-no-unspecified-access",
      Arg.Unit Cmdline.IgnoreUnspecified.on,
      Format.sprintf ": assume that all read/write accesses occuring in unspecified order are separated (defaults to %b)" (Cmdline.IgnoreUnspecified.get());
     "-unsafe-arrays",
     Arg.Unit Cmdline.UnsafeArrays.on
       ,": do not assume that accesses to arrays are in bounds."
      ^"\n"; (* A new line as separator for
                next options related to debug. *)
    ]
    ~debug:[
      "-klr",
      Arg.Unit Cmdline.KeepOnlyLastRun.on,
      Format.sprintf ": keep only last run of value analysis. All other computations become relative to the last run. This is a debugging option. (defaults to %b)"
        (Cmdline.KeepOnlyLastRun.get ())
    ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
