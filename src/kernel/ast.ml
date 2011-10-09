(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module Initial_datatype = Datatype

include
  State_builder.Option_ref
    (Cil_datatype.File)
    (struct
       let name = "AST"
       let dependencies =
         [ Cil.selfMachine;
           Kernel.SimplifyCfg.self;
           Kernel.KeepSwitch.self;
           Kernel.UnrollingLevel.self;
           Kernel.Constfold.self;
           Kernel.ReadAnnot.self;
           Kernel.PreprocessAnnot.self;
           Kernel.Files.self;
           Cil.selfFormalsDecl;
         ]
       let kind = `Internal
     end)

let mark_as_computed () = mark_as_computed () (* eta-expansion required *)

let () =
  State_dependency_graph.Static.add_dependencies
    ~from:self [ Cil_datatype.Stmt.Hptset.self;
                 Cil_datatype.Varinfo.Hptset.self ];
  Cil.register_ast_dependencies self;
  Logic_env.init_dependencies self;

exception Bad_Initialization of string

exception NoUntypedAst

let default_initialization =
  ref (fun () -> raise (Bad_Initialization "Cil file not initialized"))

let set_default_initialization f = default_initialization := f

let force_compute () =
  Kernel.feedback ~level:2 "computing the AST";
  !default_initialization ();
  get ()

let get () = memo (fun () -> force_compute ())

let is_computed () = is_computed ()

let compute () = if not (is_computed ()) then ignore (force_compute ())

let set_file file =
  let change old_file =
    if old_file == file then old_file
    else raise (Bad_Initialization "Too many AST initializations")
  in
  ignore (memo ~change (fun () -> mark_as_computed (); file))

module UntypedFiles = struct

  let compute_untyped () =
    if not (is_computed()) then ignore (force_compute())
    else raise NoUntypedAst

  include State_builder.Option_ref
    (Initial_datatype.List(Cil_datatype.Cabs_file))
    (struct
       let name = "Untyped AST"
       let dependencies = (* the others delayed until file.ml *)
         [ Cil.selfMachine;
           self (* can't be computed without the AST *) ]
       let kind = `Internal
     end)

  let get () = memo (fun () -> compute_untyped (); get ())

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
