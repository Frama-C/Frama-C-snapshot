(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: register.ml,v 1.36 2008/11/06 13:03:28 uid568 Exp $ *)

open Lattice

let raise_invalid_lattice () =
  invalid_arg "Security.Register.run: invalid security lattice."

let security_name = "security_status"

let last_annotations = ref []

open Cil_types
let add_annotation a =
  Cil.registerAttribute a Cil.AttrType;
  if not (Logic_env.LogicInfo.mem a) then begin
    Logic_env.add_builtin_logic_function
      { l_name = a;
	l_tparams = [];
        l_type = Some (Ltype (security_name, []));
        l_profile = [];
        l_labels = [];
        l_body = LBreads [] };
  end

let extend_logic_language () =
  (* Extend logic typing with predefined security names. *)
  let x = Cil.make_logic_var "x" (Lvar "a") in
  if not (Logic_env.LogicInfo.mem Model.state_name) then begin
    Logic_env.add_builtin_logic_function
      { l_name = Model.state_name;
	l_tparams = [];
        l_type = Some (Ltype (security_name,[]));
        l_profile = [x];
        l_labels = [];
        l_body = LBreads [ TSSingleton (TSLval (TSVar x, TSNoOffset)) ] };
  end;
  (* Extend parser and logic typing with possible annotations. *)
  let annotations =
    let lattice =
      if Cmdline.Security.LogicAnnotation.is_set () then
	Cmdline.Security.LogicAnnotation.get ()
      else
	Cmdline.Security.Lattice.get ()
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
    if Cmdline.Security.is_on () || Cmdline.Security.LogicAnnotation.is_set ()
    then Logic_env.Builtins.extend extend_logic_language;
    match Cmdline.Security.Lattice.get () with
    | "weak" -> Weak.init ()
    | "medium" -> Medium.init ()
    | "strong" -> Strong.init ()
    | _ -> raise_invalid_lattice ()

let main _fmt =
  if Cmdline.Security.is_on () then begin
    !Db.Value.compute ();
    !Db.Security.run_whole_analysis ()
  end

let () = Db.Main.extend main

(* Do not put this open statement above for preventing conflict with
   calls to [Analysis]. *)
open Cmdline.Security

let debug_options =
  [ "-debug", Arg.Int Debug.set, ": set level of debug" ]

let options =
  [ "-security-analysis",
    Arg.Unit Analysis.on,
    ": perform security analysis";

    "-security-no-annotation",
    Arg.Unit (fun () -> LogicAnnotation.set ""),
    ": do not recognize security annotations";

    "-security-annotation",
    Arg.String LogicAnnotation.set,
    "<s> : recognize security annotations of the specified lattice";

    "-security-lattice",
    Arg.String Lattice.set,
    "<s> : specify security lattice";

    "-security-propagate-assertions",
    Arg.Unit PropagateAssertions.on,
    ": propagate security assertions when possible";

    "-security-slicing",
    Arg.Unit Slicing.on,
    ": perfom the security slicing analysis" ]

let () =
  Options.add_plugin
    ~name:"security (experimental)" ~descr:"security analysis" ~init
    ~shortname:"security" ~debug:debug_options
    options

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
