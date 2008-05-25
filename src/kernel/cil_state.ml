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

(* $Id: cil_state.ml,v 1.8 2008/04/10 15:48:06 uid562 Exp $ *)

include
  Computation.OptionRef
    (Kernel_datatype.File)
    (struct
       let name = Project.Computation.Name.make "cil_file"
       let dependencies = []
     end)

let () = Messages_manager.depend self; Logic_env.init_dependencies self

exception Bad_Initialisation of string

let file () =
  memo (fun () -> raise (Bad_Initialisation "Cil file not initialized"))

let set_file file =
  let change old_file =
    if old_file == file then old_file
    else raise (Bad_Initialisation "Too many initialization")
  in
  ignore (memo ~change (fun () -> file))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
