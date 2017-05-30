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

open Cil_types

(* store the E-ACSL built-ins by associating a varinfo to its name. *)
let tbl
    : varinfo ref Datatype.String.Hashtbl.t
    = Datatype.String.Hashtbl.create 7

let mem = Datatype.String.Hashtbl.mem tbl
let find s = !(Datatype.String.Hashtbl.find tbl s)

(* the initial varinfos in the table belong to the original project. At the time
   of code generation, we need to update them to the ones of the new project. *)
let update s vi =
  try
    let vref = Datatype.String.Hashtbl.find tbl s in
    vref := vi
  with Not_found ->
    ()

(* add [vi] in the built-in table if it is an E-ACSL built-in which is not
   [already] registered. *)
let add_builtin vi already =
  if not already then
    let bl_name = vi.vname in
    if Options.Builtins.mem bl_name then
      match Cil.unrollType vi.vtype with
      | TFun(ret_typ, param_typs, _, _) ->
        let bl_type = match Cil.unrollType ret_typ with
          | TVoid _ ->
            Options.fatal
              "Expecting a non-void return type for the E-ACSL built-in %s"
              bl_name
          | _ -> Some (Ctype ret_typ)
        in
        let bl_profile = match param_typs with
          | None -> []
          | Some l -> List.map (fun (name, ty, _) -> (name, Ctype ty)) l
        in
        let bli =
          { bl_name; bl_labels = []; bl_params = []; bl_type; bl_profile }
        in
        (* add the built-in locally as an E-ACSL built-in, but also as a new
           Frama-C built-in. This way, the annotated C code will be parsed when
           using it *)
        Logic_builtin.add bli;
        Datatype.String.Hashtbl.add tbl bl_name (ref vi)
      | _ ->
        Options.fatal "Expecting a function type for the E-ACSL built-in %s"
          bl_name

let init () =
  Datatype.String.Hashtbl.clear tbl;
  if not (Options.Builtins.is_empty ()) then
    (* every time a new global is visited by [Cabs2cil], check if we must add it
       as a new E-ACSL built-in *)
    Cabs2cil.register_new_global_hook add_builtin

(* Initialization of the database must be done before anything else, but parsing
   the command line *)
let () = Cmdline.run_after_configuring_stage init

(*
Local Variables:
compile-command: "make"
End:
*)
