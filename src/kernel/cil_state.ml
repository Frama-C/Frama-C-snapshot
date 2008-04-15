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

(* $Id: cil_state.ml,v 1.16 2008/10/03 13:09:16 uid568 Exp $ *)


module UntypedFiles =
  Computation.OptionRef
    (Cil_datatype.UntypedFiles)
    (struct
       let name = "Untyped AST"
       let dependencies = [ Cil.selfMachine ] (* delayed until file.ml *)
     end)

include
  Computation.OptionRef
    (Cil_datatype.File)
    (struct
       let name = "AST"
       let dependencies = [ Cil.selfMachine ] (* delayed until boot.ml *)
     end)

let () =
  Messages_manager.depend self;
  Logic_env.init_dependencies self;
  Project.Computation.add_dependency Cil.varinfos_self self;
  Project.Computation.add_dependency Cil.selfFormalsDecl self

exception Bad_Initialisation of string

let default_initialization =
  ref (fun () -> raise (Bad_Initialisation "Cil file not initialized"))

let set_default_initialization f =
  default_initialization := f

let compute () = !default_initialization ()

let file () = memo (fun () -> compute (); get ())

let is_computed () = is_computed ()

let set_file file =
  let change old_file =
    if old_file == file then old_file
    else raise (Bad_Initialisation "Too many initializations of the AST")
  in
  ignore (memo ~change (fun () -> mark_as_computed (); file))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
