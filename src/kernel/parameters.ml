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

(** Bunch of values which may be initialized through command line. *)

open Extlib

(* ************************************************************************* *)
(** {2 General purpose options} *)
(* ************************************************************************* *)

let check_range name ~min ~max =
  Kernel.deprecated "check_range" ~now:"Plugin.Int.set_range"
    (fun v ->
       if v < min || v > max then
	 Kernel.abort
	   "invalid argument for %s option, not in range %d-%d"
	   name min max)

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Exiting
let () = Plugin.do_not_journalize ()
module GeneralHelp =
  Kernel.False
    (struct
      let option_name = "-help"
      let help = "display a general help"
      let module_name = "GeneralHelp"
      let kind = `Irrelevant
     end)

let run_help () = if GeneralHelp.get () then Cmdline.help () else Cmdline.nop
let () = Cmdline.run_after_exiting_stage run_help
let () = GeneralHelp.add_aliases [ "--help"; "-h" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintVersion =
  Kernel.False
    (struct
       let option_name = "-version"
       let module_name = "PrintVersion"
       let help = "print version information"
       let kind = `Irrelevant
     end)
let () = PrintVersion.add_aliases [ "-v"; "--version" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintShare =
  Kernel.False(struct
	  let option_name = "-print-share-path"
	  let module_name = "PrintShare"
	  let help = "print the Frama-C share path"
          let kind = `Irrelevant
	end)
let () = PrintShare.add_aliases [ "-print-path" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintLib =
  Kernel.False(struct
	  let option_name = "-print-lib-path"
	  let module_name = "PrintLib"
	  let help = "print the path of the Frama-C kernel library"
          let kind = `Irrelevant
	end)
let () = PrintLib.add_aliases [ "-print-libpath" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintPluginPath =
  Kernel.False
    (struct
       let option_name = "-print-plugin-path"
       let module_name = "PrintPluginPath"
       let help =
	 "print the path where the Frama-C dynamic plug-ins are searched into"
       let kind = `Irrelevant
     end)

let () = Plugin.set_group Kernel.help
let () = Plugin.set_negative_option_name ""
module DumpDependencies =
  Kernel.EmptyString
    (struct
       let module_name = "DumpDependencies"
       let option_name = "-dump-dependencies"
       let help = "undocumented"
       let arg_name = ""
       let kind = `Irrelevant
     end)
let () =
  at_exit
    (fun () ->
       if not (DumpDependencies.is_default ()) then
	 State_dependency_graph.Dynamic.dump (DumpDependencies.get ()))

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

let () = Plugin.set_group Kernel.messages
let () = Plugin.do_not_projectify ()
let () = Plugin.do_not_journalize ()
let () = Plugin.set_cmdline_stage Cmdline.Early
let () = Plugin.is_visible ()
module GeneralVerbose =
    Kernel.Int
      (struct
	 let default = 1
	 let option_name = "-verbose"
	 let arg_name = "n"
	 let help = "general level of verbosity"
	 let module_name = "GeneralVerbose"
         let kind = `Irrelevant
       end)
let () =
  (* line order below matters *)
  GeneralVerbose.set_range ~min:0 ~max:max_int;
  GeneralVerbose.add_set_hook (fun _ n -> Cmdline.verbose_level_ref := n);
  GeneralVerbose.set !Cmdline.verbose_level_ref

let () = Plugin.set_group Kernel.messages
let () = Plugin.do_not_projectify ()
let () = Plugin.do_not_journalize ()
let () = Plugin.set_cmdline_stage Cmdline.Early
let () = Plugin.is_visible ()
module GeneralDebug =
  Kernel.Zero
    (struct
       let option_name = "-debug"
       let arg_name = "n"
       let help = "general level of debug"
       let module_name = "GeneralDebug"
       let kind = `Irrelevant
     end)
let () =
  (* line order below matters *)
  GeneralDebug.set_range ~min:0 ~max:max_int;
  GeneralDebug.add_set_hook
    (fun old n ->
       if n = 0 then decr Plugin.positive_debug_ref
       else if old = 0 then incr Plugin.positive_debug_ref;
       Cmdline.debug_level_ref := n);
  GeneralDebug.set !Cmdline.debug_level_ref

let () = Plugin.set_group Kernel.messages
let () = Plugin.set_negative_option_name ""
let () = Plugin.set_cmdline_stage Cmdline.Early
let () = Plugin.is_visible ()
let () = Plugin.do_not_projectify ()
let () = Plugin.do_not_journalize ()
module Quiet =
  Kernel.Bool
    (struct
       let default = Cmdline.quiet
       let option_name = "-quiet"
       let module_name = "Quiet"
       let help = "sets -verbose and -debug to 0"
       let kind = `Irrelevant
     end)
let () =
  Quiet.add_set_hook
    (fun _ b -> assert b; GeneralVerbose.set 0; GeneralDebug.set 0)

let () = Plugin.set_group Kernel.messages
let () = Plugin.do_not_journalize ()
let () = Plugin.do_not_projectify ()
module UseUnicode =
  Kernel.True
    (struct
       let option_name = "-unicode"
       let module_name = "UseUnicode"
       let help = "use utf8 in messages"
       let kind = `Irrelevant
     end)
let () = UseUnicode.add_set_hook (fun _ b -> Cil.print_utf8 := b)

let () = Plugin.set_group Kernel.messages
module Time =
  Kernel.EmptyString
    (struct
       let module_name = "Time"
       let option_name = "-time"
       let arg_name = "filename"
       let help = "append user time and date to <filename> at exit"
       let kind = `Irrelevant
     end)

let () = Plugin.set_group Kernel.messages
let () = Plugin.set_negative_option_name "-do-not-collect-messages"
let () = Plugin.do_not_projectify ()
let () = Plugin.set_cmdline_stage Cmdline.Early
module Collect_messages =
  Kernel.Bool
    (struct
      let module_name = "Collect_messages"
      let option_name = "-collect-messages"
      let help = "collect warning and error messages for displaying them in \
the GUI (set by default iff the GUI is launched)"
      let kind = `Irrelevant
      let default = !Config.is_gui (* ok: Config.is_gui already initialised *)
     end)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

let inout_source = Kernel.add_group "Input/Output Source Code"

let () = Plugin.set_group inout_source
module PrintCode =
  Kernel.False
    (struct
      let module_name = "PrintCode"
      let option_name = "-print"
      let help = "pretty print original code with its comments"
      let kind = `Irrelevant
     end)

let () = Plugin.set_group inout_source
module PrintComments =
  Kernel.False(struct
	  let module_name = "PrintComments"
	  let option_name = "-keep-comments"
	  let help = "try to keep comments in C code"
          let kind = `Irrelevant
	end)
let () =
  (* simple mirror *)
  PrintComments.add_set_hook
    (fun _old b -> Clexer.keepComments := b) ;
  (* projectified mirror *)
  Project.register_after_set_current_hook
    ~user_only:false
    (fun _ -> Clexer.keepComments := PrintComments.get ())

module CodeOutput = struct

  let () = Plugin.set_group inout_source
  include Kernel.EmptyString
    (struct
       let module_name = "CodeOutput"
       let option_name = "-ocode"
       let arg_name = "filename"
       let help =
	 "when printing code, redirects the output to file <filename>"
       let kind = `Irrelevant
     end)

  let streams = Hashtbl.create 7

  let output msg =
    let file = get () in
    if file = ""
    then Log.print_delayed msg
    else
      try
	let fmt =
	  try fst (Hashtbl.find streams file)
	  with Not_found ->
	    let out = open_out file in
	    let fmt = Format.formatter_of_out_channel out in
	    Hashtbl.add streams file (fmt,out) ; fmt
	in
	Format.fprintf fmt msg
      with Sys_error s ->
	Kernel.warning
	  "Fail to open file \"%s\" for code output@\nSystem error: %s.@\n\
             Code is output on stdout instead." file s ;
	Log.print_delayed msg

  let close_all () =
    Hashtbl.iter
      (fun file (fmt,cout) ->
	 try
	   Format.pp_print_flush fmt () ;
	   close_out cout ;
	 with Sys_error s ->
	   Kernel.failure
	     "Fail to close output file \"%s\"@\nSystem error: %s."
	     file s)
      streams

  let () = at_exit close_all

end

let () = Plugin.set_group inout_source
module FloatNormal =
  Kernel.False
    (struct
       let option_name = "-float-normal"
       let module_name = "FloatNormal"
       let help = "display floats with internal routine"
       let kind = `Irrelevant
     end)

let () = Plugin.set_group inout_source
module FloatRelative =
  Kernel.False
    (struct
       let option_name = "-float-relative"
       let module_name = "FloatRelative"
       let help = "display float intervals as [lower_bound ++ width]"
       let kind = `Irrelevant
     end)

let () = Plugin.set_group inout_source
module FloatHex =
  Kernel.False
    (struct
       let option_name = "-float-hex"
       let module_name = "FloatHex"
       let help = "display floats as hexadecimal"
       let kind = `Irrelevant
     end)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

let saveload = Kernel.add_group "Saving or Loading Data"

let () = Plugin.set_group saveload
module SaveState =
  Kernel.EmptyString
    (struct
       let module_name = "SaveState"
       let option_name = "-save"
       let arg_name = "filename"
       let help = "at exit, save the session into file <filename>"
       let kind = `Irrelevant
     end)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Loading
module LoadState =
  Kernel.EmptyString
    (struct
       let module_name = "LoadState"
       let option_name = "-load"
       let arg_name = "filename"
       let help = "load a previously-saved session from file <filename>"
       let kind = `Correctness
     end)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module AddPath =
  Kernel.StringSet
    (struct
       let option_name = "-add-path"
       let module_name = "AddPath"
       let arg_name = "p1, ..., pn"
       let help = "add paths which dynamic plugins are searched in"
       let kind = `Irrelevant
     end)
let () = AddPath.add_set_hook (fun _ _ -> AddPath.iter Dynamic.add_path)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module LoadModule =
  Kernel.StringSet
    (struct
       let option_name = "-load-module"
       let module_name = "LoadModule"
       let arg_name = "m1, ..., mn"
       let help = "load the given modules dynamically"
       let kind = `Irrelevant
     end)
let () =
  LoadModule.add_set_hook (fun _ _ -> LoadModule.iter Dynamic.load_module)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module Dynlink =
  Kernel.True
    (struct
      let option_name = "-dynlink"
      let module_name = "Dynlink"
      let help = "load all the found dynamic plug-ins (default); \
otherwise, ignore all plug-ins in default directories"
      let kind = `Irrelevant
     end)
let () = Dynlink.add_set_hook (fun _ -> Dynamic.set_default)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module LoadScript =
  Kernel.StringSet(struct
	      let option_name = "-load-script"
	      let module_name = "LoadScript"
	      let arg_name = "m1, ..., mn"
	      let help = "load the given OCaml scripts dynamically"
              let kind = `Irrelevant
	    end)
let () =
  LoadScript.add_set_hook (fun _ _ -> LoadScript.iter Dynamic.load_script)

module Journal = struct
  let () = Plugin.set_negative_option_name "-journal-disable"
  let () = Plugin.set_cmdline_stage Cmdline.Early
  let () = Plugin.set_group saveload
  module Enable = struct
    include Kernel.Bool
      (struct
	 let module_name = "Journal.Enable"
	 let default = Cmdline.journal_enable
	 let option_name = "-journal-enable"
	 let help = "dump a journal while Frama-C exit"
         let kind = `Irrelevant
       end)
    let is_set () = Cmdline.journal_isset
  end
  let () = Plugin.set_group saveload
  module Name =
    Kernel.String
      (struct
	 let module_name = "Journal.Name"
	 let option_name = "-journal-name"
	 let default = Journal.get_name ()
	 let arg_name = "s"
	 let help =
	   "set the filename of the journal (do not write any extension)"
         let kind = `Irrelevant
       end)
end

(* ************************************************************************* *)
(** {2 Customizing Normalization} *)
(* ************************************************************************* *)

let normalisation = Kernel.add_group "Customizing Normalization"

let () = Plugin.set_group normalisation
module UnrollingLevel =
  Kernel.Zero
    (struct
       let module_name = "UnrollingLevel"
       let option_name = "-ulevel"
       let arg_name = "l"
       let help = "unroll loops n times (defaults to 0) before analyzes"
       let kind = `Tuning
     end)

let () = Plugin.set_group normalisation
module Machdep =
  Kernel.EmptyString
    (struct
       let module_name = "Machdep"
       let option_name = "-machdep"
       let arg_name = "machine"
       let help = "use <machine> as the current machine dependent configuration. Use -machdep help to see the list of available machines"
       let kind = `Correctness
     end)
let () =
  State_dependency_graph.Static.add_dependencies
    ~from:Machdep.self
    [ Cil.selfMachine ]

let () = Plugin.set_group normalisation
module ReadAnnot =
  Kernel.True(struct
	 let module_name = "ReadAnnot"
	 let option_name = "-annot"
	 let help = "read annotation"
         let kind = `Correctness
       end)
let () =
  Clexer.annot_char := '@';
  ReadAnnot.add_set_hook
    (fun _ x ->
       (* prevent the C lexer interpretation of comments *)
       if x then Clexer.annot_char := '@' else Clexer.annot_char := '\000')

let () = Plugin.set_group normalisation
module PreprocessAnnot =
  Kernel.False(struct
	  let module_name = "PreprocessAnnot"
	  let option_name = "-pp-annot"
	  let help = "pre-process annotations (if they are read)"
          let kind = `Correctness
	end)

let () = Plugin.set_group normalisation
module CppCommand =
  Kernel.EmptyString
    (struct
       let module_name = "CppCommand"
       let option_name = "-cpp-command"
       let arg_name = "cmd"
       let help = "<cmd> is used to build the preprocessing command.\n\
Default to $CPP environment variable or else \"gcc -C -E -I.\".\n\
If unset, the command is built as follow:\n\
  CPP -o <preprocessed file> <source file>\n\
%1 and %2 can be used into CPP string to mark the position of <source file> \
and <preprocessed file> respectively"
       let kind = `Correctness
     end)

let () = Plugin.set_group normalisation
module CppExtraArgs =
  Kernel.StringSet
    (struct
       let module_name = "CppExtraArgs"
       let option_name = "-cpp-extra-args"
       let arg_name = "args"
       let help = "additional arguments passed to the preprocessor while \
preprocessing the C code but not while preprocessing annotations"
       let kind = `Correctness
     end)

let () = Plugin.set_group normalisation
let () = Plugin.set_negative_option_name ""
module TypeCheck =
  Kernel.False(struct
          let module_name = "TypeCheck"
          let option_name = "-typecheck"
          let help = "only typechecks the source files"
          let kind = `Tuning
        end)

let () = Plugin.set_group normalisation
module ContinueOnAnnotError =
  Kernel.False(struct
          let module_name = "ContinueOnAnnotError"
          let option_name = "-continue-annot-error"
          let help = "When an annotation fails to type-check, just emits \
                         a warning and discards the annotation instead of \
                         generating an error (errors in C are still fatal)"
          let kind = `Tuning
        end)
let () =
  ContinueOnAnnotError.add_set_hook
    (fun _ b ->
       if b then
         Cabshelper.continue_annot_error_set ()
       else Cabshelper.continue_annot_error_unset())

let () = Plugin.set_group normalisation
module SimplifyCfg =
  Kernel.False
    (struct
       let module_name = "SimplifyCfg"
       let option_name = "-simplify-cfg"
       let help =
	 "remove break, continue and switch statement before analyzes"
       let kind = `Tuning
     end)

let () = Plugin.set_group normalisation
module KeepSwitch =
  Kernel.False(struct
	  let option_name = "-keep-switch"
	  let module_name = "KeepSwitch"
	  let help = "keep switch statements despite -simplify-cfg"
          let kind = `Tuning
	end)

let () = Plugin.set_group normalisation
module Constfold =
  Kernel.False
    (struct
       let option_name = "-constfold"
       let module_name = "Constfold"
       let help = "fold all constant expressions in the code before analysis"
       let kind = `Tuning
     end)

module Files = struct

  let () = Plugin.is_invisible ()
  include Kernel.StringList
    (struct
       let option_name = ""
       let module_name = "Files"
       let arg_name = ""
       let help = ""
       let kind = `Correctness
     end)
  let () = Cmdline.use_cmdline_files set

  let () = Plugin.set_group normalisation
  module Check =
    Kernel.False(struct
	    let option_name = "-check"
	    let module_name = "Files.Check"
	    let help = "performs consistency checks over cil files"
            let kind = `Irrelevant
	  end)

  let () = Plugin.set_group normalisation
  module Copy =
    Kernel.False(struct
	    let option_name = "-copy"
	    let module_name = "Files.Copy"
	    let help =
	      "always perform a copy of the original AST before analysis begin"
            let kind = `Irrelevant
	  end)

  let () = Plugin.set_group normalisation
  module Orig_name =
    Kernel.False(struct
	    let option_name = "-orig-name"
	    let module_name = "Files.Orig_name"
	    let help = "prints a message each time a variable is renamed"
            let kind = `Irrelevant
	  end)

end

let () = Plugin.set_group normalisation
module AllowDuplication =
  Kernel.True(struct
                let option_name = "-allow-duplication"
                let module_name = "AllowDuplication"
                let help =
                  "allow duplication of small blocks during normalization"
                let kind = `Tuning
              end)
let () =
  AllowDuplication.add_set_hook (fun _ flag -> Cabs2cil.allowDuplication:=flag);
  (* Ensures that there's no mismatch between default value in
     Cabs2cil and here.
     TODO: Store the state only here, and make Cabs2cil depend on Parameters
   *)
 AllowDuplication.set (AllowDuplication.get())

let () = Plugin.set_group normalisation
module DoCollapseCallCast =
  Kernel.True(struct
                let option_name = "-collapse-call-cast"
                let module_name = "DoCollapseCallCast"
                let help =
                  "Allow implicit cast between returned value of a function \
                   and the lval it is assigned to."
                let kind = `Tuning
              end)
let () =
  DoCollapseCallCast.add_set_hook
    (fun _ flag -> Cabs2cil.doCollapseCallCast:=flag);
  (* see AllowDuplication *)
  DoCollapseCallCast.set (DoCollapseCallCast.get())

let () = Plugin.set_group normalisation
module ForceRLArgEval =
  Kernel.False(struct
                 let option_name = "-force-rl-arg-eval"
                 let module_name = "ForceRLArgEval"
                 let help = "Force right to left evaluation order for \
                              arguments of function calls"
                 let kind = `Correctness
               end)
let () =
  ForceRLArgEval.add_set_hook
    (fun _ flag -> Cabs2cil.forceRLArgEval:= flag);
  ForceRLArgEval.set (ForceRLArgEval.get())

(* ************************************************************************* *)
(** {2 Analysis Options} *)
(* ************************************************************************* *)

let analysis_options = Kernel.add_group "Analysis Options"

let () = Plugin.set_group analysis_options
module MainFunction =
  Kernel.String
    (struct
       let module_name = "MainFunction"
       let default = "main"
       let option_name = "-main"
       let arg_name = "f"
       let help = "set to name the entry point for analysis. Use -lib-entry if this is not for a complete application. Defaults to main"
       let kind = `Correctness
     end)

let () = Plugin.set_group analysis_options
module LibEntry =
  Kernel.False
    (struct
       let module_name = "LibEntry"
       let option_name = "-lib-entry"
       let help ="run analysis for an incomplete application e.g. an API call. See the -main option to set the entry point name"
       let kind = `Correctness
     end)

let () = Plugin.set_group analysis_options
module UnspecifiedAccess =
  Kernel.False(struct
	 let module_name = "UnspecifiedAccess"
	 let option_name = "-unspecified-access"
	 let help = "assume that all read/write accesses occuring in unspecified order are not separated"
         let kind = `Correctness
       end)

let () = Plugin.set_group analysis_options
module Overflow =
  Kernel.True(struct
	 let module_name = "Overflow"
	 let option_name = "-overflow"
	 let help = "assume that arithmetic operations overflow"
         let kind = `Correctness
       end)

let () = Plugin.set_negative_option_name "-unsafe-arrays"
let () = Plugin.set_group analysis_options
module SafeArrays =
  Kernel.True
    (struct
       let module_name = "SafeArrays"
       let option_name = "-safe-arrays"
       let help = "for arrays that are fields inside structs, assume that accesses are in bounds"
       let kind = `Correctness
     end)

let () = Plugin.set_group analysis_options
module AbsoluteValidRange = struct
  module Info = struct
    let option_name = "-absolute-valid-range"
    let arg_name = "min-max"
    let help = "min and max must be integers in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and fit in 64 bits. Assume that that all absolute addresses outside of the [min-max] range are invalid. In the absence of this option, all absolute addresses are assumed to be invalid"
    let default = ""
    let module_name = "AbsoluteValidRange"
    let kind = `Correctness
  end
  include Kernel.String(Info)
  let is_set _x = assert false
end

let () = Plugin.set_group analysis_options
module FloatFlushToZero =
  Kernel.False
    (struct
      let option_name = "-float-flush-to-zero"
      let help = "Floating-point operations flush to zero"
      let module_name = "FloatFlushToZero"
      let kind = `Correctness
    end)

(* ************************************************************************* *)
(** {2 Others options} *)
(* ************************************************************************* *)

let misc = Kernel.add_group "Miscellaneous Options"

let () =
  Cmdline.add_option_without_action
    "-then"
    ~plugin:""
    ~group:(misc :> Cmdline.Group.t)
    ~help:(Some "parse options before `-then' and execute Frama-C \
accordingly, then parse options after `-then' and re-execute Frama-C")
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-then-on"
    ~plugin:""
    ~argname:"p"
    ~group:(misc :> Cmdline.Group.t)
    ~help:(Some "like `-then', but the second group of actions is executed \
on project <p>")
    ~ext_help:""
    ()

let () = Plugin.set_group misc
let () = Plugin.set_negative_option_name ""
let () = Plugin.set_cmdline_stage Cmdline.Early
module NoType =
  Kernel.Bool
    (struct
       let module_name = "NoType"
       let default = not Cmdline.use_type
       let option_name = "-no-type"
       let help = "undocumented but disable some features"
       let kind = `Irrelevant
     end)

let () = Plugin.set_group misc
let () = Plugin.set_negative_option_name ""
let () = Plugin.set_cmdline_stage Cmdline.Early
module NoObj =
  Kernel.Bool
    (struct
       let module_name = "NoObj"
       let default = not Cmdline.use_obj
       let option_name = "-no-obj"
       let help = "-no-type + disable some additional features"
       let kind = `Irrelevant
     end)

(* ************************************************************************* *)
(** {2 Interface for dynamic plugins} *)
(* ************************************************************************* *)

module Dynamic = struct

    module type Common = sig
      type t
      val get: string -> t
      val set: string -> t  -> unit
      val clear: string -> unit -> unit
      val is_set: string  -> bool
      val is_default: string  -> bool
    end

    let apply modname name s ty1 ty2 =
      Dynamic.get
	~plugin:""
	(Plugin.dynamic_name modname s name)
	(Datatype.func ty1 ty2)

    module Common(X: sig type t val modname:string val ty: t Type.t end ) =
    struct
      type t = X.t
      let ty = X.ty
      let get name = apply X.modname name "get" Datatype.unit ty ()
      let set name = apply X.modname name "set" ty Datatype.unit
      let clear name = apply X.modname name "clear" Datatype.unit Datatype.unit
      let is_set name =
	apply X.modname name "is_set" Datatype.unit Datatype.bool ()
      let is_default name =
	apply X.modname name "is_default" Datatype.unit Datatype.bool ()
    end

    module Bool = struct
      include Common
	(struct type t = bool let ty = Datatype.bool let modname = "Bool"end )
      let on name = apply "Bool" name "on" Datatype.unit Datatype.unit
      let off name = apply "Bool" name "off" Datatype.unit Datatype.unit
    end

    module Int = struct
      include Common
	(struct type t = int let ty = Datatype.int let modname = "Int" end )
      let incr name = apply "Int" name "incr" Datatype.unit Datatype.unit
    end

    module String =
      Common
	(struct
	   type t = string
	   let ty = Datatype.string
	   let modname = "String"
	 end)

    module StringSet = struct
      include Common
	(struct include Datatype.String.Set let modname = "StringSet" end)
      let add name = apply "StringSet" name "add" Datatype.string Datatype.unit
      let remove name =
	apply "StringSet" name "remove" Datatype.string Datatype.unit
      let is_empty name =
	apply "StringSet" name "is_empty" Datatype.unit Datatype.bool ()
      let iter name =
	apply "StringSet" name "iter"
	  (Datatype.func Datatype.string Datatype.unit) Datatype.unit
    end
(*
    module IndexedVal(X: sig val ty_name: string end) = struct
      include Common(struct type t = string let ty = string end)
      type value = Type.ty
      let ty = Type.get_abstract X.ty_name
      let add_choice name =
	StringTbl.find tbl (name ^ ".add_choice") (func string (func ty unit))
      let get_val name =
	StringTbl.find tbl (name ^ ".get_val") (func unit ty) ()
    end
      *)

end

(* ************************************************************************* *)
(** {2 Options which define context of analyses } *)
(* ************************************************************************* *)

let get_selection_context () =
  let has_dependencies s =
    State_dependency_graph.Dynamic.G.out_degree
      State_dependency_graph.Dynamic.graph
      s
    > 0
  in
  (* automatically select all options which have some dependencies:
     they have an impact of some analysis. *)
  let states =
    State_selection.Dynamic.fold
      (fun s acc -> if has_dependencies s then s :: acc else acc)
      (Plugin.get_selection ())
      [ Files.Check.self; Files.Copy.self ]
  in
  State_selection.of_list states

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
