(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
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

open Cil_types

(** Experimental module *)


let mk_new_name =
  let prefix = "__fc_clone_" in
  let counter = ref 0 in
  fun name -> incr counter; prefix ^ (string_of_int !counter) ^ "_" ^ name


(** Returns a clone of a defined kernel function to add into the current AST *)
let clone_function_definition old_kf =
  let visitor = new Visitor.frama_c_refresh (Project.current()) in
  let old_fundec = Kernel_function.get_definition old_kf in
  let old_loc = Kernel_function.get_location old_kf in
  let old_funspec = Annotations.funspec ~populate:false old_kf in
  visitor#set_current_kf old_kf;
  visitor#set_current_func old_fundec;
  let new_fundec = Visitor.visitFramacFunction visitor old_fundec in
  (* update the CFG and sallstmts field *)
  Cfg.clearCFGinfo ~clear_id:false new_fundec;
  Cfg.cfgFun new_fundec;
  let new_vi = new_fundec.svar in
  new_vi.vname <- mk_new_name old_fundec.svar.vname;
  let new_funspec =
    Visitor.visitFramacFunspec visitor old_funspec
  in
  (* Creates the kernel function for the clone function. *)
  let new_kf = 
	(* NOTE: it would be better if the replace function would
           return the associated kernel function that is new here *)
    Globals.Functions.replace_by_definition new_funspec new_fundec old_loc;
    try Globals.Functions.get new_fundec.svar
    with Not_found ->
      Kernel.fatal "No clone kernel function for %s(%d)"
        new_fundec.svar.vname new_fundec.svar.vid
  in
  new_kf

(** Returns a clone of a kernel function and adds it into the current AST *)
let clone_defined_kernel_function old_kf =
  let f = Ast.get() in
  let new_kf = clone_function_definition old_kf in
  let new_fundec = Kernel_function.get_definition new_kf in
  let new_loc = Kernel_function.get_location new_kf in
  let gfun = GFun (new_fundec, new_loc) in 

  let old_vi = Kernel_function.get_vi old_kf in
  let is_old_fundec fundec = Cil_datatype.Varinfo.equal fundec.svar old_vi in 
  let is_old_gfun = function
    | GFun (fundec,_) -> is_old_fundec fundec
    | _ -> false
  in 
  (* Scan the globals. Make sure this is tail recursive. *)
  let rec loop (acc: global list) = function
    | [] -> begin
      match f.globinit with
      | Some fundec when is_old_fundec fundec -> 
	(* The clone function is the global initializer function. 
	   Adds it at the end of the list of globals. *)
	List.rev_append acc [gfun]
      | _ -> Kernel.fatal "kernel function not found for %s(%d)" old_vi.vname old_vi.vid
    end
    | g :: restg when is_old_gfun g -> List.rev_append acc (g:: gfun ::restg) 
    | g :: restg -> loop (g::acc) restg
  in
  (* Updates the list of globals *)
  f.globals <- loop [] f.globals;
  Ast.mark_as_grown();
  new_kf

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
