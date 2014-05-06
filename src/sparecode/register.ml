(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** {2 Internal State} *)

module Result_pair =
  Datatype.Pair_with_collections(Datatype.Bool)(Datatype.Bool)
    (struct let module_name = "Sparecode.Register.Result_pair.t" end)
module Result =
  State_builder.Hashtbl
    (Datatype.Hashtbl
       (Result_pair.Hashtbl)
       (Result_pair)
       (struct let module_name = "Sparecode" end))
    (Project.Datatype)
    (struct
       let name = "Sparecode"
       let size = 7
       let dependencies = [ Ast.self; Db.Value.self ] (* delayed, see below *)
     end)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:Result.self
         [ !Db.Pdg.self; !Db.Outputs.self_external ])

module P = Sparecode_params

(** {2 State_builder} *)

let unjournalized_rm_unused_globals new_proj_name project =
  P.feedback "remove unused global declarations from project '%s'"
    (Project.get_name project);
  P.result "removed unused global declarations in new project '%s'" new_proj_name;
  Project.on project Globs.rm_unused_decl new_proj_name

let journalized_rm_unused_globals  =
  Journal.register
    "!Db.Sparecode.rm_unused_globals"
    (Datatype.func2
       ~label1:("new_proj_name", None) Datatype.string
       ~label2:("project", Some Project.current) Project.ty
       Project.ty)
    unjournalized_rm_unused_globals

let rm_unused_globals ?new_proj_name ?(project=Project.current ()) () =
  let new_proj_name = 
    match new_proj_name with 
    | Some name -> name 
    | None -> (Project.get_name project)^ " (without unused globals)" 
  in
  journalized_rm_unused_globals new_proj_name project

let run select_annot select_slice_pragma =
  P.feedback "remove unused code...";
  (*let initial_file = Ast.get () in*)
  let kf_entry, _library = Globals.entry_point () in
  let proj =
    Spare_marks.select_useful_things
      ~select_annot ~select_slice_pragma kf_entry
  in
  let old_proj_name = Project.get_name (Project.current ()) in
  let new_proj_name = (old_proj_name^" without sparecode") in

  P.feedback "remove unused global declarations...";
  let tmp_prj = Transform.Info.build_cil_file "tmp_prj" proj in
  let new_prj = Project.on tmp_prj Globs.rm_unused_decl new_proj_name in

  P.result "result in new project '%s'." (Project.get_name new_prj);
  Project.remove ~project:tmp_prj ();
  let ctx = Parameter_state.get_selection_context () in
  Project.copy ~selection:ctx new_prj;
  new_prj

let journalized_get =
  Journal.register
    "!Db.Sparecode.get"
    (Datatype.func2
       ~label1:("select_annot", None) Datatype.bool
       ~label2:("select_slice_pragma", None) Datatype.bool
       Project.ty)
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

let main () =
  if Sparecode_params.Analysis.get () then begin
    let select_annot = Sparecode_params.Annot.get () in
    let select_slice_pragma = true in
    let new_proj = !Db.Sparecode.get select_annot select_slice_pragma in
    File.pretty_ast ~prj:new_proj ()
  end
  else if Sparecode_params.GlobDecl.get () then begin
    let new_proj = rm_unused_globals () in
    File.pretty_ast ~prj:new_proj ()
  end

let () = Db.Main.extend main

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
