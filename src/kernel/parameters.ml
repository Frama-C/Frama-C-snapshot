(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
  Kernel.False(struct
	  let option_name = "-help"
	  let descr = "display a general help"
	  let module_name = "GeneralHelp"
	end)

let run_help () = if GeneralHelp.get () then Cmdline.help () else Cmdline.nop
let () = Cmdline.run_after_exiting_stage run_help
let () = GeneralHelp.add_alias [ "--help"; "-h" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintVersion =
  Kernel.False
    (struct
       let option_name = "-version"
       let module_name = "PrintVersion"
       let descr = "print version information"
     end)
let () = PrintVersion.add_alias [ "-v"; "--version" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintShare =
  Kernel.False(struct
	  let option_name = "-print-share-path"
	  let module_name = "PrintShare"
	  let descr = "print the Frama-C share path"
	end)
let () = PrintShare.add_alias [ "-print-path" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintLib =
  Kernel.False(struct
	  let option_name = "-print-lib-path"
	  let module_name = "PrintLib"
	  let descr = "print the path of the Frama-C kernel library"
	end)
let () = PrintLib.add_alias [ "-print-libpath" ]

let () = Plugin.set_group Kernel.help
let () = Plugin.set_cmdline_stage Cmdline.Early
module PrintPluginPath =
  Kernel.False
    (struct
       let option_name = "-print-plugin-path"
       let module_name = "PrintPluginPath"
       let descr =
	 "print the path where the Frama-C dynamic plug-ins are searched into"
     end)

let () = Plugin.set_group Kernel.help
let () = Plugin.set_negative_option_name ""
module DumpDependencies =
  Kernel.EmptyString
    (struct
       let module_name = "DumpDependencies"
       let option_name = "-dump-dependencies"
       let descr = "undocumented"
       let arg_name = ""
     end)
let () =
  at_exit
    (fun () ->
       if not (DumpDependencies.is_default ()) then
	 Project.Computation.dump_dependencies (DumpDependencies.get ()))

let () = Plugin.set_negative_option_name ""
module DumpDynamicDependencies =
  Kernel.EmptyString
    (struct
       let module_name = "DumpDynamicDependencies"
       let option_name = "-dump-dynamic-dependencies"
       let descr = "undocumented"
       let arg_name = ""
     end)
let () =
  at_exit
    (fun () ->
       if not (DumpDynamicDependencies.is_default ()) then
	 Project.Computation.dump_dynamic_dependencies
	   (DumpDynamicDependencies.get ()))

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
	 let descr = "general level of verbosity"
	 let module_name = "GeneralVerbose"
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
       let descr = "general level of debug"
       let module_name = "GeneralDebug"
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
module Quiet =
  Kernel.Bool
    (struct
       let default = Cmdline.quiet
       let option_name = "-quiet"
       let module_name = "Quiet"
       let descr = "sets -verbose and -debug to 0"
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
       let descr = "use utf8 in messages"
     end)
let () = UseUnicode.add_set_hook (fun _ b -> Cil.print_utf8 := b)

let () = Plugin.set_group Kernel.messages
module Time =
  Kernel.EmptyString
    (struct
       let module_name = "Time"
       let option_name = "-time"
       let arg_name = "filename"
       let descr = "append user time and date to <filename> at exit"
     end)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

let inout_source = Kernel.add_group "Input/Output Source Code"

let () = Plugin.set_group inout_source
module PrintCode =
  Kernel.False(struct
	  let module_name = "PrintCode"
	  let option_name = "-print"
	  let descr = "pretty print original code with its comments"
	end)

let () = Plugin.set_group inout_source
module PrintComments =
  Kernel.False(struct
	  let module_name = "PrintComments"
	  let option_name = "-keep-comments"
	  let descr = "try to keep comments in C code"
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
       let descr =
	 "when printing code, redirects the output to file <filename>"
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
module Obfuscate =
  Kernel.False
    (struct
       let option_name = "-obfuscate"
       let module_name = "Obfuscate"
       let descr = "print an obfuscated version of files and exit"
     end)

let () = Plugin.set_group inout_source
module FloatDigits =
  Kernel.Int
    (struct
       let module_name = "FloatDigits"
       let option_name = "-float-digits"
       let default = 12
       let arg_name = "n"
       let descr =
	 "display this number of digits when printing floats. Defaults to "
	 ^ string_of_int default
     end)

let () = Plugin.set_group inout_source
module FloatRelative =
  Kernel.False
    (struct
       let option_name = "-float-relative"
       let module_name = "FloatRelative"
       let descr = "display float intervals as [lower_bound ++ width]"
     end)

let () = Plugin.set_group inout_source
module FloatHex =
  Kernel.False
    (struct
       let option_name = "-float-hex"
       let module_name = "FloatHex"
       let descr = "display floats as hexadecimal"
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
       let descr = "at exit, save the session into file <filename>"
     end)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Loading
module LoadState =
  Kernel.EmptyString
    (struct
       let module_name = "LoadState"
       let option_name = "-load"
       let arg_name = "filename"
       let descr = "load a previously-saved session from file <filename>"
     end)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module AddPath =
  Kernel.StringSet
    (struct
       let option_name = "-add-path"
       let module_name = "AddPath"
       let arg_name = "p1, ..., pn"
       let descr = "add paths which dynamic plugins are searched in"
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
       let descr = "load the given modules dynamically"
     end)
let () =
  LoadModule.add_set_hook (fun _ _ -> LoadModule.iter Dynamic.load_module)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module Dynlink =
  Kernel.True(struct
	 let option_name = "-dynlink"
	 let module_name = "Dynlink"
	 let descr = "load all the found dynamic plug-ins (default); \
otherwise, ignore all plug-ins in default directories"
       end)
let () = Dynlink.add_set_hook (fun _ -> Dynamic.set_default)

let () = Plugin.set_group saveload
let () = Plugin.set_cmdline_stage Cmdline.Extending
module LoadScript =
  Kernel.StringSet(struct
	      let option_name = "-load-script"
	      let module_name = "LoadScript"
	      let arg_name = "m1, ..., mn"
	      let descr = "load the given ocaml scripts dynamically"
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
	 let descr = "dump a journal while Frama-C exit"
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
	 let descr =
	   "set the filename of the journal (do not write any extension)"
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
       let descr = "unroll loops n times (defaults to 0) before analyzes"
     end)

let () = Plugin.set_group normalisation
module Machdep =
  Kernel.EmptyString
    (struct
       let module_name = "Machdep"
       let option_name = "-machdep"
       let arg_name = "machine"
       let descr = "use <machine> as the current machine dependent configuration. Use -machdep help to see the list of available machines"
     end)
let () = Project.Computation.add_dependency Cil.selfMachine Machdep.self

let () = Plugin.set_group normalisation
module ReadAnnot =
  Kernel.True(struct
	 let module_name = "ReadAnnot"
	 let option_name = "-annot"
	 let descr = "read annotation"
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
	  let descr = "pre-process annotations (if they are read)"
	end)

let () = Plugin.set_group normalisation
module CppCommand =
  Kernel.EmptyString
    (struct
       let module_name = "CppCommand"
       let option_name = "-cpp-command"
       let arg_name = "cmd"
       let descr = "<cmd> is used to build the preprocessing command.
Default to $CPP environment variable or else \"gcc -C -E -I.\".
If unset, the command is built as follow:
  CPP -o <preprocessed file> <source file>
%1 and %2 can be used into CPP string to mark the position of <source file> and <preprocessed file> respectively"
     end)

let () = Plugin.set_group normalisation
module CppExtraArgs =
  Kernel.StringSet
    (struct
       let module_name = "CppExtraArgs"
       let option_name = "-cpp-extra-args"
       let arg_name = "args"
       let descr = "additional arguments passed to the preprocessor while preprocessing the C code but not while preprocessing annotations"
     end)

let () = Plugin.set_group normalisation
let () = Plugin.set_negative_option_name ""
module TypeCheck =
  Kernel.False(struct
          let module_name = "TypeCheck"
          let option_name = "-typecheck"
          let descr = "only typechecks the source files"
        end)

let () = Plugin.set_group normalisation
module ContinueOnAnnotError =
  Kernel.False(struct
          let module_name = "ContinueOnAnnotError"
          let option_name = "-continue-annot-error"
          let descr = "When an annotation fails to type-check, just emits \
                         a warning and discards the annotation instead of \
                         generating an error (errors in C are still fatal)"
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
       let descr =
	 "remove break, continue and switch statement before analyzes"
     end)

let () = Plugin.set_group normalisation
module KeepSwitch =
  Kernel.False(struct
	  let option_name = "-keep-switch"
	  let module_name = "KeepSwitch"
	  let descr = "keep switch statements despite -simplify-cfg"
	end)

let () = Plugin.set_group normalisation
module Constfold =
  Kernel.False
    (struct
       let option_name = "-constfold"
       let module_name = "Constfold"
       let descr = "fold all constant expressions in the code before analysis"
     end)

module Files = struct

  include Kernel.StringList
    (struct
       let option_name = ""
       let module_name = "Files"
       let arg_name = ""
       let descr = ""
     end)
  let () = Cmdline.use_cmdline_files set

  let () = Plugin.set_group normalisation
  module Check =
    Kernel.False(struct
	    let option_name = "-check"
	    let module_name = "Files.Check"
	    let descr = "performs consistency checks over cil files"
	  end)

  let () = Plugin.set_group normalisation
  module Copy =
    Kernel.False(struct
	    let option_name = "-copy"
	    let module_name = "Files.Copy"
	    let descr =
	      "always perform a copy of the original AST before analysis begin"
	  end)

  let () = Plugin.set_group normalisation
  module Orig_name =
    Kernel.False(struct
	    let option_name = "-orig-name"
	    let module_name = "Files.Orig_name"
	    let descr = "prints a message each time a variable is renamed"
	  end)

end

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
       let descr = "set to name the entry point for analysis. Use -lib-entry if this is not for a complete application. Defaults to main"
     end)

let () = Plugin.set_group analysis_options
module LibEntry =
  Kernel.False(struct
	  let module_name = "LibEntry"
	    let option_name = "-lib-entry"
	    let descr ="run analysis for an incomplete application e.g. an API call. See the -main option to set the entry point name"
	end)

let () = Plugin.set_group analysis_options
module UnspecifiedAccess =
  Kernel.False(struct
	 let module_name = "UnspecifiedAccess"
	 let option_name = "-unspecified-access"
	 let descr = "assume that all read/write accesses occuring in unspecified order are not separated"
       end)

let () = Plugin.set_group analysis_options
module Overflow =
  Kernel.True(struct
	 let module_name = "Overflow"
	 let option_name = "-overflow"
	 let descr = "assume that arithmetic operations overflow"
       end)

let () = Plugin.set_negative_option_name "-unsafe-arrays"
let () = Plugin.set_group analysis_options
module SafeArrays =
  Kernel.True
    (struct
       let module_name = "SafeArrays"
       let option_name = "-safe-arrays"
       let descr = "for arrays that are fields inside structs, assume that accesses are in bounds"
     end)

let () = Plugin.set_group analysis_options
module AbsoluteValidRange = struct
  module Info = struct
    let option_name = "-absolute-valid-range"
    let arg_name = "min-max"
    let descr = "min and max must be integers in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and fit in 64 bits. Assume that that all absolute addresses outside of the [min-max] range are invalid. In the absence of this option, all absolute addresses are assumed to be invalid"
    let default = ""
    let module_name = "AbsoluteValidRange"
  end
  include Kernel.String(Info)
  let is_set _x = assert false
end

let () = Plugin.set_group analysis_options
module FloatFlushToZero =
  Kernel.False
    (struct
      let option_name = "-float-flush-to-zero"
      let descr = "Floating-point operations flush to zero"
      let module_name = "FloatFlushToZero"
    end)

(* ************************************************************************* *)
(** {2 Others options} *)
(* ************************************************************************* *)

let misc = Kernel.add_group "Miscellaneous Options"

let () = Plugin.set_group misc
let () = Plugin.set_negative_option_name ""
let () = Plugin.set_cmdline_stage Cmdline.Early
module NoType =
  Kernel.Bool
    (struct
       let module_name = "NoType"
       let default = not Cmdline.use_type
       let option_name = "-no-type"
       let descr = "undocumented but disable some features"
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
       let descr = "-no-type + disable some additional features"
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
	~plugin:(Plugin.dynamic_plugin_name modname)
	(Plugin.dynamic_function_name s name)
	(Type.func ty1 ty2)

    module Common(X: sig type t val modname:string val ty: t Type.t end ) =
    struct
      type t = X.t
      let ty = X.ty
      let get name = apply X.modname name "get" Type.unit ty ()
      let set name = apply X.modname name "set" ty Type.unit
      let clear name = apply X.modname name "clear" Type.unit Type.unit
      let is_set name = apply X.modname name "is_set" Type.unit Type.bool ()
      let is_default name =
	apply X.modname name "is_default" Type.unit Type.bool ()
    end

    module Bool = struct
      include Common
	(struct type t = bool let ty = Type.bool let modname = "Bool"end )
      let on name = apply "Bool" name "on" Type.unit Type.unit
      let off name = apply "Bool" name "off" Type.unit Type.unit
    end

    module Int = struct
      include Common
	(struct type t = int let ty = Type.int let modname = "Int" end )
      let incr name = apply "Int" name "incr" Type.unit Type.unit
    end

    module String =
      Common
	(struct
	   type t = string
	   let ty = Type.string
	   let modname = "String"
	 end)

    module StringSet = struct
      include Common
	(struct
	   type t = Cilutil.StringSet.t
	   let ty = Kernel_type.string_set
	   let modname = "StringSet"
	 end)
      let add name = apply "StringSet" name "add" Type.string Type.unit
      let remove name = apply "StringSet" name "remove" Type.string Type.unit
      let is_empty name =
	apply "StringSet" name "is_empty" Type.unit Type.bool ()
      let iter name =
	apply "StringSet" name "iter"
	  (Type.func Type.string Type.unit) Type.unit
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
  let a o = Project.Selection.add o Kind.Do_Not_Select_Dependencies in
  let has_dependencies o =
    try
      Project.Selection.iter
	(fun _ -> raise Exit)
	(Project.Selection.singleton o Kind.Only_Select_Dependencies);
      false
    with Exit ->
      true
  in
  (* automatically select all options which have some dependencies:
     they have an impact of some analysis. *)
  let sel_ctx =
    Project.Selection.fold
      (fun o _ acc -> if has_dependencies o then a o acc else acc)
      (Plugin.get_selection ())
      Project.Selection.empty
  in
  (* General options *)
  ((a Files.Check.self) $
     (a Files.Copy.self))
    sel_ctx

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
