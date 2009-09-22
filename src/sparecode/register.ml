(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil

(** {2 Internal State} *)

module Result =
  Computation.Hashtbl
    (Datatype.Couple(Datatype.Bool)(Datatype.Bool))
    (Datatype.Project)
    (struct
       let name = "Sparecode"
       let size = 7
       let dependencies = [] (* delayed, see below *)
     end)


module P = Sparecode_params
(*
let () =
  Cmdline.run_after_extending_stage
    (fun () ->
       let add = Project.Computation.add_dependency Result.self in
       add !Db.Pdg.self;
       add !Db.Outputs.self_external)
*)
(** {2 Computation} *)

let unjournalized_rm_unused_globals project () =
  P.feedback "remove unused global declarations from project '%s'"
    (Project.name project);
  let new_name = Project.name project ^ " (without unused globals)" in
  P.result "removed unused global declarations in new project '%s'" new_name;
  Project.on project Globs.rm_unused_decl new_name

let journalized_rm_unused_globals =
  Journal.register
    "!Db.Sparecode.rm_unused_globals"
    (Type.func ~label:("project", Some Project.current)
       Project.ty (Type.func Type.unit  Project.ty))
    unjournalized_rm_unused_globals

let rm_unused_globals ?(project=Project.current ()) () =
  journalized_rm_unused_globals project ()

let run select_annot select_slice_pragma =
  P.feedback "remove unused code...";
  (*let initial_file = Ast.get () in*)
  let kf_entry, _library = Globals.entry_point () in

  let proj = Marks.select_usefull_things
               ~select_annot ~select_slice_pragma kf_entry in

  let old_proj_name = Project.name (Project.current ()) in
  let new_proj_name = (old_proj_name^" without sparecode") in

    P.feedback "remove unused global declarations...";
  let tmp_prj = Transform.Info.build_cil_file "tmp_prj" proj in
  let new_prj = Project.on tmp_prj Globs.rm_unused_decl new_proj_name in
    P.result "result in new project '%s'." (Project.name new_prj);
    Project.remove ~project:tmp_prj ();
  let ctx = Parameters.get_selection_context () in
    Project.copy ~only:ctx new_prj;
    new_prj

let journalized_get =
  Journal.register
    "!Db.Sparecode.get"
    (Type.func ~label:("select_annot", None) Type.bool
       (Type.func ~label:("select_slice_pragma", None) Type.bool Project.ty))
    (fun select_annot select_slice_pragma ->
       Result.memo
	 (fun _ -> run select_annot select_slice_pragma)
	 (select_annot, select_slice_pragma))

(* add labels *)
let get ~select_annot ~select_slice_pragma =
  journalized_get select_annot select_slice_pragma

(** {2 Initialisation of the sparecode plugin } *)

let () =
  (* journalization already done. *)
  Db.register Db.Journalization_not_required Db.Sparecode.get get;
  Db.register Db.Journalization_not_required
    Db.Sparecode.rm_unused_globals rm_unused_globals

let main _fmt =
  if Sparecode_params.Analysis.get () then begin
    let select_annot = Sparecode_params.Annot.get () in
    let select_slice_pragma = true in
    let new_proj = !Db.Sparecode.get select_annot select_slice_pragma in
    File.pretty ~prj:new_proj ()
  end
  else if Sparecode_params.GlobDecl.get () then begin
    let new_proj = rm_unused_globals () in
    File.pretty  ~prj:new_proj ()
  end

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
