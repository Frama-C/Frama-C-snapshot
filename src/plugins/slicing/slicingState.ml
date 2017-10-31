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

module P =
  State_builder.Option_ref
    (SlicingTypes.Sl_project)
    (struct
      let name = "Slicing.Project"
      let dependencies = [] (* others delayed in Register *)
    end)

let self = P.self
let () =
  Cmdline.run_after_extended_stage
    (fun () ->
      State_dependency_graph.add_codependencies
        ~onto:self
        [ !Db.Pdg.self; !Db.Inputs.self_external; !Db.Outputs.self_external ])

let get () =
  try P.get ()
  with Not_found -> SlicingParameters.fatal "slicing not initialized."

let may f = match P.get_option () with
  | None -> ()
  | Some _ -> f ()

let may_map ~dft f = match P.get_option () with
  | None -> dft
  | Some _ -> f ()

let reset_slicing () =
  !Db.Value.compute () ;
  let initialized = match P.get_option () with | None -> false | Some _ -> true in
  if not initialized then
    SlicingParameters.feedback ~level:1 "initializing slicing ..."
  else
    SlicingParameters.feedback ~level:1 "reinitializing slicing ...";
  P.set
    SlicingInternals.{ functions = Cil_datatype.Varinfo.Hashtbl.create 17;
                       actions = [] };
  if not initialized then
    SlicingParameters.feedback ~level:2 "done (initializing slicing)."
  else
    SlicingParameters.feedback ~level:2 "done (reinitializing slicing)."
