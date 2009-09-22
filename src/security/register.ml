(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: register.ml,v 1.38 2009-02-13 07:59:29 uid562 Exp $ *)

open Lattice
open Cil_types

let raise_invalid_lattice () = Options.abort "invalid security lattice"

let security_name = "security_status_t"

let security_type = { lt_name = security_name;
                      lt_params = [];
                      lt_ctors = None;
                    }

let last_annotations = ref []

open Cil_types
let add_annotation a =
  Cil.registerAttribute a Cil.AttrType;
  if not (Logic_env.is_logic_type security_name) then begin
    Logic_env.add_builtin_logic_type security_name security_type
  end;
  if not (Logic_env.is_logic_function a) then begin
    Logic_builtin.add { bl_name = a; bl_params = []; bl_labels = [];
                        bl_type = Some (Ltype (security_type, []));
                        bl_profile = [];
                      }
  end

let extend_logic_language () =
  Options.feedback ~level:3 "adding built-in security annotations";
  (* Extend logic typing with predefined security names. *)
  if not (Logic_env.is_logic_type security_name) then begin
    Logic_env.add_builtin_logic_type security_name security_type
  end;
  if not (Logic_env.is_logic_function Model.state_name) then begin
    Logic_builtin.add
      { bl_name = Model.state_name; bl_type =  Some (Ltype (security_type,[]));
        bl_profile =  [("x", Lvar "a")]; bl_params = [ "a" ];
        bl_labels = [];
      }
  end;
  (* Extend parser and logic typing with possible annotations. *)
  let annotations =
    let lattice =
      if not (Options.LogicAnnotation.is_default ()) then
	Options.LogicAnnotation.get ()
      else
	Options.Lattice.get ()
    in
    match lattice with
    | "weak" -> Weak.possible_annotations
    | "medium" -> Medium.possible_annotations
    | "strong" -> Strong.possible_annotations
    | _ -> raise_invalid_lattice ()
  in
  last_annotations := annotations;
  List.iter add_annotation annotations

let init =
  (* Dynamic modules do not work (see BTS #152) *)
  let module Weak = Analysis.Make(Weak) in
  let module Medium = Analysis.Make(Medium) in
  let module Strong = Analysis.Make(Strong) in
  fun () ->
    if Options.is_on () || Options.LogicAnnotation.is_set ()
    then Logic_env.Builtins.extend extend_logic_language;
    match Options.Lattice.get () with
    | "weak" -> Weak.init ()
    | "medium" -> Medium.init ()
    | "strong" -> Strong.init ()
    | _ -> raise_invalid_lattice ()

let () = Cmdline.run_after_configuring_stage init

let main _fmt =
  if Options.is_on () then begin
    !Db.Value.compute ();
    !Db.Security.run_whole_analysis ()
  end

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
