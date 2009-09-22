(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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
let () = Parameters.max_valid_absolute_address := Utils.memory_size ()
*)

let main () =

  (* Memoization of context free functions *)
  let mem_functions = Value_parameters.MemFunctions.get () in
  if Value_parameters.MemExecAll.get ()
    || not (Cilutil.StringSet.is_empty mem_functions)
  then begin
    Value_parameters.feedback "====== MEMOIZING FUNCTIONS ======";
    Globals.Functions.iter
      (fun kf ->
	 let name = Kernel_function.get_name kf in
	 if Kernel_function.is_definition kf &&
	   (Value_parameters.MemExecAll.get () 
	    || Cilutil.StringSet.mem name mem_functions)
	 then begin
	   Value_parameters.feedback "== function %a" 
	     Kernel_function.pretty_name kf;
	   try
	     !Db.Value.memoize kf
           with Db.Value.Aborted ->
	     Value_parameters.fatal "Cannot memoize %a: Analysis degenerated@."
	       Kernel_function.pretty_name kf
	 end)
  end;

  (* Value computations *)
  if Value_parameters.ForceValues.get () then begin
    !Db.Value.compute ();
    Value_parameters.result "====== VALUES COMPUTED ======";
  end;

  (* Val display and Inout compute/display *)

  let display_val =
    Value_parameters.verbose_atleast 1 && Value_parameters.ForceValues.get ()
  in
  
  let force_inout = List.exists Parameters.Dynamic.Bool.get [
    "-deref" ;
    "-out" ;
    "-inout" ;
    "-input" ;
    "-out-external" ;
    "-input-with-formals" ;
  ] in

  (* Iteration *)

  if display_val || force_inout then
    begin
      let job opt display compute kf =
	if Parameters.Dynamic.Bool.get opt then
	  if Value_parameters.verbose_atleast 1 
	  then Value_parameters.result "%a" !display kf
	  else !compute kf
      in
      let nothing = ref (fun _kf -> ()) in
      !Db.Semantic_Callgraph.topologically_iter_on_functions
	(fun kf ->
	   if Kernel_function.is_definition kf then 
	     begin
	       job "-out" Db.Outputs.display Db.Outputs.compute kf ;
	       job "-out-external" Db.Outputs.display_external Db.Outputs.compute kf ;
	       job "-input" Db.Inputs.display Db.Inputs.compute kf ;
	       job "-input-with-formals" Db.Inputs.display_with_formals nothing kf ;
	       job "-inout" Db.InOutContext.display Db.InOutContext.compute kf ;
	       job "-deref" Db.Derefs.display Db.Derefs.compute kf ;
	       if display_val then Value_parameters.result "%a" Db.Value.display kf ;
	     end)
    end

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
