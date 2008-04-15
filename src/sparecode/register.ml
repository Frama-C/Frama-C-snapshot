(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

let () =
  Options.register_plugin_init
    (fun () ->
       let add = Project.Computation.add_dependency Result.self in
       add !Db.Pdg.self;
       add !Db.Outputs.self_external)

(** {2 Computation} *)

let rm_unused_globals ?project () = 
  let project = 
    match project with Some prj -> prj | None -> (Project.current()) in
  Debug.debug 0 
    "[sparecode] remove unused global declarations in project '%s'@."
    (Project.name project);
  Project.on project Globs.rm_unused_decl () 

let run select_annot select_slice_pragma =
  Debug.debug 1 "[sparecode] running@.";
  (*let initial_file = Cil_state.file () in*)
  let kf_entry, _library = Globals.entry_point () in

  let proj = Marks.select_usefull_things 
               ~select_annot ~select_slice_pragma kf_entry in

  let new_proj_name = "unused_removed" in
  let new_prj = Project.create new_proj_name in
  Transform.Info.build_cil_file new_prj proj;
  Debug.debug 1 "[sparecode] remove unused global declarations...@.";
  let _ = Project.on new_prj Globs.rm_unused_decl () in
  Debug.debug 0 "[sparecode] done. Result in new project '%s'.@."
    (Project.name new_prj);
  new_prj


let journalized_get =
  Journal.register
    "!Db.Sparecode.get"
    (Type.func Type.bool (Type.func Type.bool Project.repr))
    (fun select_annot select_slice_pragma -> 
       Result.memo 
	 (fun _ -> run select_annot select_slice_pragma)
	 (select_annot, select_slice_pragma))

(* add labels *)
let get ~select_annot ~select_slice_pragma = 
  journalized_get select_annot select_slice_pragma
    
(** {2 Initialisation of the sparecode plugin } *)

let () = 
  Db.register ~journalize:None Db.Sparecode.get get;
  Db.Sparecode.rm_unused_globals := rm_unused_globals

let main fmt =
  if Cmdline.Sparecode.Analysis.get () then begin
    Format.fprintf fmt "@\n[sparecode] in progress...@.";
    let select_annot = not (Cmdline.Sparecode.NoAnnot.get ())in
    let select_slice_pragma = true in
    let new_proj = !Db.Sparecode.get select_annot select_slice_pragma in
    File.pretty fmt ~prj:new_proj ;
    Format.fprintf fmt "@\n====== UNUSED CODE DETECTED ======@."
  end 
  else if Cmdline.Sparecode.GlobDecl.get () then begin
    rm_unused_globals ();
    File.pretty fmt 
  end

let () = Db.Main.extend main

let () =
  Options.add_plugin
    ~name:"Spare Code (experimental)"
    ~descr:"unused code detection"
    [ "-sparecode-analysis",
      Arg.Unit Cmdline.Sparecode.Analysis.on,
      ": perform a spare code analysis";
      "-sparecode-no-annot",
      Arg.Unit Cmdline.Sparecode.NoAnnot.on,
      ": don't select more things to keep every reachable annotation";
      "-rm-unused-globals",
      Arg.Unit Cmdline.Sparecode.GlobDecl.on,
      ": only remove unused global types and variables (automatically done by -sparecode-analysis)"
    ]

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
