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

let check () =
  Visitor.visitFramacFileSameGlobals (Visit.do_visit false) (Ast.get ());
  let t = Error.nb_untypable () in
  let n = Error.nb_not_yet () in
  let print msg n =
    Options.result "@[%d annotation%s %s ignored,@ being %s.@]"
      n
      (if n > 1 then "s" else "")
      (if n > 1 then "were" else "was")
      msg
  in
  print "untypable" t;
  print "unsupported" n;
  n + t = 0

let check =
  Dynamic.register
    ~plugin:"e-acsl"
    ~journalize:true
    "check"
    (Datatype.func Datatype.unit Datatype.bool)
    check

type extended_project =
  | To_be_extended
  | Already_extended of Project.t option (* None = keep the current project *)

let extended_ast_project: extended_project ref = ref To_be_extended

let unmemoized_extend_ast () =
  let extend () =
    let share = Options.Share.dir ~error:true () in
    Options.feedback ~level:3 "setting kernel options for E-ACSL.";
    Kernel.CppExtraArgs.add
      (Format.asprintf " -DE_ACSL_MACHDEP=%s -I%s/memory_model"
         (Kernel.Machdep.get ())
         share);
    Kernel.Keep_unused_specified_functions.off ();
    if Plugin.is_present "variadic-translation" then
      Dynamic.Parameter.Bool.off "-variadic-translation" ();
    let ppc, ppk = File.get_preprocessor_command () in
    let register s =
      File.pre_register
        (File.NeedCPP
           (s,
            ppc
            ^ Format.asprintf " -I%s" share,
            ppk))
    in
    List.iter register (Misc.library_files ())
  in
  if Ast.is_computed () then begin
    (* do not modify the existing project: work on a copy.
       Must also extend the current AST with the E-ACSL's library files. *)
    Options.feedback ~level:2 "AST already computed: \
E-ACSL is going to work on a copy.";
    let name = Project.get_name (Project.current ()) in
    let tmpfile =
      Extlib.temp_file_cleanup_at_exit ("e_acsl_" ^ name) ".i" in
    let cout = open_out tmpfile in
    let fmt = Format.formatter_of_out_channel cout in
    File.pretty_ast ~fmt ();
    let selection =
      State_selection.diff
        State_selection.full
        (State_selection.with_dependencies Ast.self)
    in
    let prj =
      Project.create_by_copy
        ~last:false
        ~selection
        (Format.asprintf "%s for E-ACSL" name)
    in
    Project.on prj
      (fun () ->
        Kernel.Files.set [ tmpfile ];
        extend ())
      ();
    Some prj
  end else begin
    extend ();
    None
  end

let extend_ast () = match !extended_ast_project with
  | To_be_extended ->
    let prj = unmemoized_extend_ast () in
    extended_ast_project := Already_extended prj;
    (match prj with
    | None -> Project.current ()
    | Some prj -> prj)
  | Already_extended None ->
    Project.current ()
  | Already_extended(Some prj) ->
    prj

let apply_on_e_acsl_ast f x =
  let tmp_prj = extend_ast () in
  let res = Project.on tmp_prj f x in
  (match !extended_ast_project with
  | To_be_extended -> assert false
  | Already_extended None -> ()
  | Already_extended (Some prj) ->
    assert (Project.equal prj tmp_prj);
    extended_ast_project := To_be_extended;
    if Options.Debug.get () = 0 then Project.remove ~project:tmp_prj ());
  res

module Resulting_projects =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Project.Datatype)
    (struct
      let name = "E-ACSL resulting projects"
      let size = 7
      let dependencies = [ Ast.self ]
     end)

let () =
  State_dependency_graph.add_dependencies
    ~from:Resulting_projects.self
    [ Label.self ]

let generate_code =
  Resulting_projects.memo
    (fun name ->
      apply_on_e_acsl_ast
        (fun () ->
          Options.feedback "beginning translation.";
          Temporal.enable (Options.Temporal_validity.get ());
          let prepared_prj = Prepare_ast.prepare () in
          let res =
            Project.on prepared_prj
            (fun () ->
              let dup_prj = Dup_functions.dup () in
              let res =
                Project.on
                  dup_prj
                  (fun () ->
                    Gmpz.init_t ();
                    Mmodel_analysis.reset ();
                    let visit prj = Visit.do_visit ~prj true in
                    let prj = File.create_project_from_visitor name visit in
                    Loops.apply_after_transformation prj;
                    (* remove the RTE's results computed from E-ACSL: their are
                       partial and associated with the wrong kernel function
                       (the one of the old project). *)
                    let selection =
                      State_selection.with_dependencies !Db.RteGen.self
                    in
                    Project.clear ~selection ~project:prj ();
                    Resulting_projects.mark_as_computed ();
                    Project.copy
                      ~selection:(State_selection.singleton Kernel.Files.self)
                      prj;
                    prj)
                ()
              in
              if Options.Debug.get () = 0 then
                Project.remove ~project:dup_prj ();
              res)
              ()
          in
          if Options.Debug.get () = 0 then begin
            Project.remove ~project:prepared_prj ();
          end;
          Options.feedback "translation done in project \"%s\"."
            (Options.Project_name.get ());
          res)
        ())

let generate_code =
  Dynamic.register
    ~plugin:"E_ACSL"
    ~journalize:true
    "generate_code"
    (Datatype.func Datatype.string Project.ty)
    generate_code

let predicate_to_exp =
  Dynamic.register
    ~plugin:"E_ACSL"
    ~journalize:false
    "predicate_to_exp"
    (Datatype.func2
       Kernel_function.ty Cil_datatype.Predicate.ty Cil_datatype.Exp.ty)
    Translate.predicate_to_exp

let add_e_acsl_library _files =
  if Options.must_visit () || Options.Prepare.get () then ignore (extend_ast ())

(* extending the AST as soon as possible reduce the amount of time the AST is
   duplicated:
   - that is faster
   - locations are better (indicate an existing file, and not a temp file) *)
let () = Cmdline.run_after_configuring_stage add_e_acsl_library

(* The Frama-C standard library contains specific built-in variables prefixed by
   "__fc_" and declared as extern: they prevent the generated code to be
   linked. This modification of the default printer replaces them by their
   original version from the stdlib. For instance, [__fc_stdout] is replaced by
   [stdout]. That is very hackish since it modifies the default Frama-C
   printer.

   TODO: should be done by the Frama-C default printer at some points. *)
let change_printer =
  (* not projectified on purpose: this printer change is common to each
     project. *)
  let first = ref true in
  fun () ->
    if !first then begin
      first := false;
      let r = Str.regexp "^__fc_" in
      let module Printer_class(X: Printer.PrinterClass) = struct
        class printer = object
          inherit X.printer as super
          method !varinfo fmt vi =
            if vi.Cil_types.vghost || vi.Cil_types.vstorage <> Cil_types.Extern
            then
              super#varinfo fmt vi
            else
              let s = Str.replace_first r "" vi.Cil_types.vname in
              Format.fprintf fmt "%s" s
        end
      end in
      Printer.update_printer (module Printer_class: Printer.PrinterExtension)
    end

let main () =
  if Options.Run.get () then begin
    change_printer ();
    ignore (generate_code (Options.Project_name.get ()))
  end else
    if Options.Check.get () then
      apply_on_e_acsl_ast
        (fun () ->
          Gmpz.init_t ();
          ignore (check ()))
        ()

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make"
End:
*)
