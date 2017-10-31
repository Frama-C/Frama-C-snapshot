(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

let main () =
  if SlicingParameters.is_on () then begin
    SlicingParameters.feedback ~level:1 "slicing requests in progress...";

    (* have to do the value analysis before the selections
     * because some functions use its results,
     * and the value analysis is not launched automatically. *)
    !Db.Value.compute ();

    let project_name = SlicingParameters.ProjectName.get () in
    Api.Project.reset_slicing ();
    Api.Request.add_persistent_cmdline ();
      (* Apply all pending requests. *)
    if Api.Request.is_request_empty_internal () then
      begin
 	SlicingParameters.warning "No internal slicing request from the command line." ;
	if SlicingParameters.Mode.Callers.get () then
          let kf_entry, _library = Globals.entry_point () in
	  SlicingParameters.warning "Adding an extra request on the entry point of function: %a." Kernel_function.pretty kf_entry;
	  let set = Api.Select.empty_selects in
	  let set = Api.Select.select_func_calls_into set true kf_entry in
	  Api.Request.add_persistent_selection set
      end;

    Api.Request.apply_all_internal ();

    if SlicingParameters.Mode.Callers.get () then
      Api.Slice.remove_uncalled ();
    let sliced_project_name =
      project_name ^ (SlicingParameters.ExportedProjectPostfix.get ())
    in
    SlicingParameters.set_off ();
    let sliced_project = Api.Project.extract sliced_project_name in
    Project.on sliced_project SlicingParameters.clear ();
    SlicingParameters.feedback ~level:2 "done (slicing requests in progress).";
  end

(** Register the function [main] as a main entry point. *)
let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
