(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(* ************************************************************************* *)
(** {2 Kernel as an almost standard plug-in} *)
(* ************************************************************************* *)

module CamlString = String
module Fc_config = Config

let () = Plugin.register_kernel ()

module P = Plugin.Register
  (struct
     let name = ""
     let shortname = ""
     let help = "General options provided by the Frama-C kernel"
   end)

include (P: Plugin.S)
include Cmdline.Kernel_log

(* ************************************************************************* *)
(** {2 Specialised functors for building kernel parameters} *)
(* ************************************************************************* *)

module type Input = sig
  include Parameter_sig.Input
  val module_name: string
end

module type Input_with_arg = sig
  include Parameter_sig.Input_with_arg
  val module_name: string
end

module Bool(X:sig include Input val default: bool end) =
  P.Bool
    (struct 
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module False(X: Input) =
  P.False
    (struct
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module True(X: Input) =
  P.True
    (struct
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module Int (X: sig val default: int include Input_with_arg end) =
  P.Int
    (struct 
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module Zero(X:Input_with_arg) =
  P.Zero
    (struct
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module String
  (X: sig include Input_with_arg val default: string end) =
  P.String
    (struct 
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module EmptyString(X: Input_with_arg) =
  P.Empty_string
    (struct
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module String_set(X: Input_with_arg) =
  P.String_set
    (struct
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

module String_list(X: Input_with_arg) =
  P.String_list
    (struct
      let () = Parameter_customize.set_module_name X.module_name 
      include X 
     end)

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.set_negative_option_name ""
module GeneralHelp =
  False
    (struct
      let option_name = "--help"
      let help = "display a general help"
      let module_name = "GeneralHelp"
     end)
let run_help () = if GeneralHelp.get () then Cmdline.help () else Cmdline.nop
let () = Cmdline.run_after_exiting_stage run_help
let () = GeneralHelp.add_aliases [ "-h"; "-help"]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.set_negative_option_name ""
module ListPlugins =
  False
    (struct
      let option_name = "--list-plugins"
      let help = "display a general help"
      let module_name = "ListPlugins"
    end)
let run_list_plugins () =
  if ListPlugins.get () then Cmdline.list_plugins () else Cmdline.nop
let () = Cmdline.run_after_exiting_stage run_list_plugins
let () = ListPlugins.add_aliases ["-plugins"; "--plugins"]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintConfig =
  False
    (struct
       let option_name = "-print-config"
       let module_name = "PrintConfig"
       let help = "print full config information"
     end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintVersion =
  False(struct
          let option_name = "-print-version"
          let module_name = "PrintVersion"
          let help = "print the Frama-C version"
        end)
let () = PrintVersion.add_aliases [ "-v"; "-version" ; "--version" ]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintShare =
  False(struct
          let option_name = "-print-share-path"
          let module_name = "PrintShare"
          let help = "print the Frama-C share path"
        end)
let () = PrintShare.add_aliases [ "-print-path" ]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintLib =
  False(struct
          let option_name = "-print-lib-path"
          let module_name = "PrintLib"
          let help = "print the path of the Frama-C kernel library"
        end)
let () = PrintLib.add_aliases [ "-print-libpath" ]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintPluginPath =
  False
    (struct
       let option_name = "-print-plugin-path"
       let module_name = "PrintPluginPath"
       let help =
         "print the path where the Frama-C dynamic plug-ins are searched into"
     end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_negative_option_name ""
module DumpDependencies =
  P.Empty_string
    (struct
       let option_name = "-dump-dependencies"
       let help = ""
       let arg_name = ""
     end)
let () =
  at_exit
    (fun () ->
       if not (DumpDependencies.is_default ()) then
         State_dependency_graph.dump (DumpDependencies.get ()))

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.do_iterate ()
module GeneralVerbose =
    Int
      (struct
         let default = 1
         let option_name = "-verbose"
         let arg_name = "n"
         let help = "general level of verbosity"
         let module_name = "GeneralVerbose"
       end)
let () =
  (* line order below matters *)
  GeneralVerbose.set_range ~min:0 ~max:max_int;
  GeneralVerbose.add_set_hook (fun _ n -> Cmdline.Verbose_level.set n);
  match !Cmdline.Verbose_level.value_if_set with
  | None -> ()
  | Some n -> GeneralVerbose.set n

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.do_iterate ()
module GeneralDebug =
  Zero
    (struct
       let option_name = "-debug"
       let arg_name = "n"
       let help = "general level of debug"
       let module_name = "GeneralDebug"
     end)
let () =
  (* line order below matters *)
  GeneralDebug.set_range ~min:0 ~max:max_int;
  GeneralDebug.add_set_hook
    (fun old n ->
       if n = 0 then decr Plugin.positive_debug_ref
       else if old = 0 then incr Plugin.positive_debug_ref;
       Cmdline.Debug_level.set n);
  match !Cmdline.Debug_level.value_if_set with
  | None -> ()
  | Some n -> GeneralDebug.set n

let () = Parameter_customize.set_group messages
let () = Parameter_customize.set_negative_option_name ""
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.do_iterate ()
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_journalize ()
module Quiet =
  Bool
    (struct
       let default = Cmdline.quiet
       let option_name = "-quiet"
       let module_name = "Quiet"
       let help = "sets -verbose and -debug to 0"
     end)
let () =
  Quiet.add_set_hook
    (fun _ b -> assert b; GeneralVerbose.set 0; GeneralDebug.set 0)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_journalize ()
module Permissive =
  Bool
    (struct
        let default = !Parameter_customize.is_permissive_ref
        let option_name = "-permissive"
        let module_name = "Permissive"
        let help =
          "performs less verification on validity of command-line options"
     end)
let () =
  Permissive.add_set_hook
    (fun _ b -> Parameter_customize.is_permissive_ref := b)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.set_cmdline_stage Cmdline.Extended
let () = Parameter_customize.do_not_journalize ()
let () = Parameter_customize.do_not_projectify ()
module Unicode = struct
  include True
    (struct
       let option_name = "-unicode"
       let module_name = "Unicode"
       let help = "use utf8 in messages"
     end)
  (* This function behaves nicely with the Gui, that detects if command-line
     arguments have been set by the user at some point. One possible improvment
     would be to bypass journalization entirely, but this requires an API
     change in Plugin *)
  let without_unicode f arg =
    let old, default = get (), not (is_set ()) in
    off ();
    let r = f arg in
    if default then clear () else set old;
    r
end

module UseUnicode = struct
  include Unicode
  let set = deprecated "UseUnicode.set" ~now:"Unicode.set" set
  let on = deprecated "UseUnicode.on" ~now:"Unicode.on" on
  let off = deprecated "UseUnicode.off" ~now:"Unicode.off" off
  let get = deprecated "UseUnicode.get" ~now:"Unicode.get" get
end

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
module TTY =
  True
    (struct
      let option_name = "-tty"
      let module_name = "TTY"
      let help = "use terminal capabilities for feedback (when available)"
    end)
let () = Log.tty := TTY.get

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
module Time =
  P.Empty_string
    (struct
       let option_name = "-time"
       let arg_name = "filename"
       let help = "append process time and timestamp to <filename> at exit"
     end)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.set_negative_option_name "-do-not-collect-messages"
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module Collect_messages =
  Bool
    (struct
      let module_name = "Collect_messages"
      let option_name = "-collect-messages"
      let help = "collect warning and error messages for displaying them in \
the GUI (set by default iff the GUI is launched)"
      let default = !Fc_config.is_gui
     (* ok: Config.is_gui already initialised by Gui_init *)
     end)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
module SymbolicPath =
  String_set (* TODO: to be replaced by an hashtbl *)
    (struct
       let option_name = "-add-symbolic-path"
       let module_name = "SymbolicPath"
       let arg_name = "name_1:path_1,...,name_n:path_n"
       let help =
         "When displaying file locations, replace (absolute) path by the \
          corresponding symbolic name"
     end)


(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

let inout_source = add_group "Input/Output Source Code"

let () = Parameter_customize.set_group inout_source
module PrintCode =
  False
    (struct
      let module_name = "PrintCode"
      let option_name = "-print"
      let help = "pretty print original code with its comments"
     end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module PrintComments =
  False
    (struct
      let module_name = "PrintComments"
      let option_name = "-keep-comments"
      let help = "try to keep comments in C code"
     end)

module CodeOutput = struct

  let () = Parameter_customize.set_group inout_source
  include P.Empty_string
    (struct
       let option_name = "-ocode"
       let arg_name = "filename"
       let help =
         "when printing code, redirects the output to file <filename>"
     end)

  let streams = Hashtbl.create 7

  let output job =
    let file = get () in
    if file = ""
    then Log.print_delayed job
    else
      try
        let fmt =
          try fst (Hashtbl.find streams file)
          with Not_found ->
            let out = open_out file in
            let fmt = Format.formatter_of_out_channel out in
            Hashtbl.add streams file (fmt,out) ; fmt
        in
        job fmt
      with Sys_error s ->
        warning
          "Fail to open file \"%s\" for code output@\nSystem error: %s.@\n\
             Code is output on stdout instead." file s ;
        Log.print_delayed job

  let close_all () =
    Hashtbl.iter
      (fun file (fmt,cout) ->
         try
           Format.pp_print_flush fmt () ;
           close_out cout ;
         with Sys_error s ->
           failure
             "Fail to close output file \"%s\"@\nSystem error: %s."
             file s)
      streams

  let () = at_exit close_all

end

let add_path s =
  try
    let n = CamlString.index s ':' in
    let name = CamlString.sub s 0 n in
    let path = CamlString.sub s (n+1) (CamlString.length s - (n+1)) in
    Filepath.add_symbolic_dir name path
  with Not_found ->
    warning "%s is not a valid option argument for -add-symbolic-path. \
             It will be ignored" s

let () =
  SymbolicPath.add_set_hook 
    (fun o n ->
      let d = Datatype.String.Set.diff n o in
      Datatype.String.Set.iter add_path d)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module FloatNormal =
  False
    (struct
       let option_name = "-float-normal"
       let module_name = "FloatNormal"
       let help = "display floats with internal routine"
     end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module FloatRelative =
  False
    (struct
       let option_name = "-float-relative"
       let module_name = "FloatRelative"
       let help = "display float intervals as [lower_bound ++ width]"
     end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module FloatHex =
  False
    (struct
       let option_name = "-float-hex"
       let module_name = "FloatHex"
       let help = "display floats as hexadecimal"
     end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module BigIntsHex =
  Int(struct
         let module_name = "BigIntsHex"
         let option_name = "-big-ints-hex"
         let arg_name = "max"
	 let help = "display integers larger than <max> using hexadecimal \
notation"
         let default = -1
       end)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

let saveload = add_group "Saving or Loading Data"

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module SaveState =
  P.Empty_string
    (struct
       let option_name = "-save"
       let arg_name = "filename"
       let help = "at exit, save the session into file <filename>"
     end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Loading
(* must be projectified: when loading, this option will be automatically 
   reset *) 
(*let () = Parameter_customize.do_not_projectify ()*)
module LoadState =
  P.Empty_string
    (struct
       let option_name = "-load"
       let arg_name = "filename"
       let help = "load a previously-saved session from file <filename>"
     end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.do_not_projectify ()
module AddPath =
  String_list
    (struct
       let option_name = "-add-path"
       let module_name = "AddPath"
       let arg_name = "DIR,..."
       let help = "Prepend directories to FRAMAC_PLUGIN for loading dynamic plug-ins"
     end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.do_not_projectify ()
module LoadModule =
  String_list
    (struct
       let option_name = "-load-module"
       let module_name = "LoadModule"
       let arg_name = "SPEC,..."
       let help = "Dynamically load plug-ins, modules and scripts. \
                   Each <SPEC> can be an OCaml source or object file, with \
                   or without extension, or a directory of oject OCaml \
                   files to load, or a Findlib package. Loading order is preserved \
                   and additional dependencies can be listed in *.depend files."
    end)
let () = LoadModule.add_aliases [ "-load-script" ]

let bootstrap_loader () =
  begin
    Dynamic.load_plugin_path (AddPath.get()) ;
    List.iter Dynamic.load_module (LoadModule.get()) ;
  end

let () = Cmdline.load_all_plugins := bootstrap_loader 

module Journal = struct
  let () = Parameter_customize.set_negative_option_name "-journal-disable"
  let () = Parameter_customize.set_cmdline_stage Cmdline.Early
  let () = Parameter_customize.set_group saveload
  let () = Parameter_customize.do_not_projectify ()
  module Enable = struct
    include Bool
      (struct
         let module_name = "Journal.Enable"
         let default = Cmdline.journal_enable
         let option_name = "-journal-enable"
         let help = "dump a journal while Frama-C exit"
       end)
    let is_set () = Cmdline.journal_isset
  end
  let () = Parameter_customize.set_group saveload
  let () = Parameter_customize.do_not_projectify ()
  module Name =
    String
      (struct
         let module_name = "Journal.Name"
         let option_name = "-journal-name"
         let default = 
	   let dir =
	     (* duplicate code from Plugin.Session *)
	     if Session.Dir_name.is_set () then Session.Dir_name.get ()
	     else
	       try Sys.getenv "FRAMAC_SESSION"
	       with Not_found -> "./.frama-c"
	   in
	   dir ^ "/frama_c_journal.ml"
         let arg_name = "s"
         let help = "set the filename of the journal"
       end)
  let () = Name.add_set_hook (fun _ s -> Journal.set_name s);
end

let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module Session_dir =
  P.Empty_string
    (struct
      let option_name = "-session"
      let arg_name = ""
      let help = "directory in which session files are searched"
     end)
let () = Plugin.session_is_set_ref := Session_dir.is_set
let () = Plugin.session_ref := Session_dir.get

let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module Config_dir =
  P.Empty_string
    (struct
      let option_name = "-config"
      let arg_name = ""
      let help = "directory in which configuration files are searched"
     end)
let () = Plugin.config_is_set_ref := Config_dir.is_set
let () = Plugin.config_ref := Config_dir.get

(* ************************************************************************* *)
(** {2 Parsing} *)
(* ************************************************************************* *)

let parsing = add_group "Parsing"

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Extended
module Machdep =
  String
    (struct
       let module_name = "Machdep"
       let option_name = "-machdep"
       let default = "x86_32"
       let arg_name = "machine"
       let help =
         "use <machine> as the current machine dependent configuration. \
          See \"-machdep help\" for a list"
     end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module CustomAnnot =
  P.Empty_string(
      struct
        let option_name = "-custom-annot-char"
        let help = "use a custom character <c> for starting ACSL annotations"
        let arg_name = "c"
      end)
let () = CustomAnnot.add_set_hook 
           (fun _ s ->
            if CamlString.length s <> 1 then
              abort
                "-custom-annot expects a single character. Invalid argument %s"
                s)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module ReadAnnot =
  True(struct
         let module_name = "ReadAnnot"
         let option_name = "-annot"
         let help = "read and parse annotations"
       end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module PreprocessAnnot =
  False(struct
          let module_name = "PreprocessAnnot"
          let option_name = "-pp-annot"
          let help =
            "pre-process annotations (if they are read). Set by default if \
             the pre-processor is GNU-like (see option -cpp-gnu-like)"
        end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module CppCommand =
  P.Empty_string
    (struct
       let option_name = "-cpp-command"
       let arg_name = "cmd"
       let help = "<cmd> is used to build the preprocessing command.\n\
Default to $CPP environment variable or else \"gcc -C -E -I.\".\n\
If unset, the command is built as follows:\n\
  CPP -o <preprocessed file> <source file>\n\
%1 and %2 can be used into CPP string to mark the position of <source file> \
and <preprocessed file> respectively"
     end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.no_category ()
module CppExtraArgs =
  String_list
    (struct
       let module_name = "CppExtraArgs"
       let option_name = "-cpp-extra-args"
       let arg_name = "args"
       let help = "additional arguments passed to the preprocessor while \
preprocessing the C code but not while preprocessing annotations"
     end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module CppGnuLike =
  True
    (struct
      let module_name = "CppGnuLike"
      let option_name = "-cpp-gnu-like"
      let help = 
        "indicates that a custom pre-processor (see option -cpp-command) \
         accepts the same set of options as GNU cpp. Set it to false if you \
         have pre-processing issues with a custom pre-processor."
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module FramaCStdLib =
  True
    (struct
      let module_name = "FramaCStdLib"
      let option_name = "-frama-c-stdlib"
      let help =
        "adds -I$FRAMAC_SHARE/libc to the options given to the cpp command. \
         If -cpp-gnu-like is not false, also adds -nostdinc to prevent \
         inconsistent mix of system and Frama-C header files"
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module ContinueOnAnnotError =
  False(struct
          let module_name = "ContinueOnAnnotError"
          let option_name = "-continue-annot-error"
          let help = "When an annotation fails to type-check, emit \
                         a warning and discard the annotation instead of \
                         generating an error (errors in C are still fatal)"
        end)

let () = Parameter_customize.set_group parsing
module Orig_name =
  False(struct
    let option_name = "-orig-name"
    let module_name = "Orig_name"
    let help = "prints a message each time a variable is renamed"
  end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module WarnUndeclared =
  True(struct
    let option_name = "-warn-undeclared-callee"
    let help = "Warn when a function is called before it has been declared."
    let module_name = "WarnUndeclared"
  end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module WarnDecimalFloat =
  String(struct
    let option_name = "-warn-decimal-float"
    let arg_name = "freq"
    let help = "Warn when floating-point constants cannot be exactly \
              represented; freq must be one of none, once or all"
    let default = "once"
    let module_name = "WarnDecimalFloat"
  end)
let () = WarnDecimalFloat.set_possible_values ["none"; "once"; "all"]

(* ************************************************************************* *)
(** {2 Customizing Normalization} *)
(* ************************************************************************* *)

let normalisation = add_group "Customizing Normalization"

let () = Parameter_customize.set_group normalisation
module UnrollingLevel =
  Zero
    (struct
       let module_name = "UnrollingLevel"
       let option_name = "-ulevel"
       let arg_name = "l"
       let help =
         "unroll loops n times (defaults to 0) before analyzes. \
          A negative value hides UNROLL loop pragmas."
     end)

let () = Parameter_customize.set_group normalisation
module UnrollingForce =
  Bool
    (struct
       let module_name = "UnrollingForce"
       let default = false
       let option_name = "-ulevel-force"
       let help =
         "ignore UNROLL loop pragmas disabling unrolling."
     end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module Enums =
  P.Empty_string
    (struct
      let option_name = "-enums"
      let arg_name = "repr"
      let help = 
        "use <repr> to decide how enumerated types should be represented. \
         -enums help gives the list of available representations"
     end)
let enum_reprs = ["gcc-enums"; "gcc-short-enums"; "int";]
let () = Enums.set_possible_values ("help"::enum_reprs)
let () =
  Enums.add_set_hook
    (fun _ o -> if o = "help" then
        feedback "Possible enums representation are: %a"
          (Pretty_utils.pp_list ~sep:", " Format.pp_print_string)
          enum_reprs)

let () = Parameter_customize.set_group normalisation
module SimplifyCfg =
  False
    (struct
       let module_name = "SimplifyCfg"
       let option_name = "-simplify-cfg"
       let help =
         "remove break, continue and switch statements before analyses"
     end)

let () = Parameter_customize.set_group normalisation
module KeepSwitch =
  False(struct
          let option_name = "-keep-switch"
          let module_name = "KeepSwitch"
          let help = "keep switch statements despite -simplify-cfg"
        end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.set_negative_option_name "-remove-unused-specified-functions"
module Keep_unused_specified_functions =
  True(struct
          let option_name = "-keep-unused-specified-functions"
          let module_name = "Keep_unused_specified_functions"
          let help = "keep specified-but-unused functions"
        end)

let () = Parameter_customize.set_group normalisation
module SimplifyTrivialLoops =
  True(struct
          let option_name = "-simplify-trivial-loops"
          let module_name = "SimplifyTrivialLoops"
          let help = "simplify trivial loops, such as do ... while(0) loops"
       end)

let () = Parameter_customize.set_group normalisation
module Constfold =
  False
    (struct
       let option_name = "-constfold"
       let module_name = "Constfold"
       let help = "fold all constant expressions in the code before analysis"
     end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module InitializedPaddingLocals =
  True
    (struct
       let option_name = "-initialized-padding-locals"
       let module_name = "InitializedPaddingLocals"
       let help = "Implicit initialization of locals sets padding bits to 0. \
                   If false, padding bits are left uninitialized. \
                   Defaults to true."
     end)

let () = Parameter_customize.set_group normalisation
module AggressiveMerging =
  False
    (struct
       let option_name = "-aggressive-merging"
       let module_name = "AggressiveMerging"
       let help = "merge function definitions modulo renaming"
     end)

let () = Parameter_customize.set_group normalisation
module RemoveExn =
  False
    (struct
        let option_name = "-remove-exn"
        let module_name = "RemoveExn"
        let help =
          "transforms throw and try/catch statements to normal C functions. \
           Disabled by default, unless input source language has \
           has an exception mechanism."
     end)

module Files = struct

  let () = Parameter_customize.is_invisible ()
  let () = Parameter_customize.no_category ()
  include String_list
    (struct
       let option_name = ""
       let module_name = "Files"
       let arg_name = ""
       let help = ""
     end)
  let () = Cmdline.use_cmdline_files set

end

let () = Parameter_customize.set_group normalisation
module AllowDuplication =
  True(struct
    let option_name = "-allow-duplication"
    let module_name = "AllowDuplication"
    let help =
      "allow duplication of small blocks during normalization"
  end)

let () = Parameter_customize.set_group normalisation
module DoCollapseCallCast =
  True(struct
    let option_name = "-collapse-call-cast"
    let module_name = "DoCollapseCallCast"
    let help =
      "Allow some implicit casts between returned value of a function \
                   and the lvalue it is assigned to."
  end)

let () = Parameter_customize.set_group normalisation
module ForceRLArgEval =
  False(struct
    let option_name = "-force-rl-arg-eval"
    let module_name = "ForceRLArgEval"
    let help = "Force right to left evaluation order for \
                              arguments of function calls"
  end)

let normalization_parameters = [
  ForceRLArgEval.parameter;
  UnrollingLevel.parameter;
  Machdep.parameter;
  CppCommand.parameter;
  CppExtraArgs.parameter;
  SimplifyCfg.parameter;
  KeepSwitch.parameter;
  Keep_unused_specified_functions.parameter;
  Constfold.parameter;
  AllowDuplication.parameter;
  DoCollapseCallCast.parameter;
]

(* ************************************************************************* *)
(** {2 Analysis Options} *)
(* ************************************************************************* *)

let analysis_options = add_group "Analysis Options"

let () = Parameter_customize.set_group analysis_options
module MainFunction =
  String
    (struct
       let module_name = "MainFunction"
       let default = "main"
       let option_name = "-main"
       let arg_name = "f"
       let help = "use <f> as entry point for analysis. See \"-lib-entry\" \
if this is not for a complete application. Defaults to main"
     end)

let () = Parameter_customize.set_group analysis_options
module LibEntry =
  False
    (struct
       let module_name = "LibEntry"
       let option_name = "-lib-entry"
       let help ="run analysis for an incomplete application e.g. an API call. See the -main option to set the entry point"
     end)

let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.set_negative_option_name "-const-writable"
module ConstReadonly =
  True
    (struct
       let module_name = "ConstReadonly"
       let option_name = "-const-readonly"
       let help = "variables with the 'const' qualifier must be actually constant"
     end)

let () = Parameter_customize.set_group analysis_options
module UnspecifiedAccess =
  False(struct
         let module_name = "UnspecifiedAccess"
         let option_name = "-unspecified-access"
         let help = "do not assume that read/write accesses occuring \
between sequence points are separated"
       end)

let () = Parameter_customize.set_negative_option_name "-unsafe-arrays"
let () = Parameter_customize.set_group analysis_options
module SafeArrays =
  True
    (struct
       let module_name = "SafeArrays"
       let option_name = "-safe-arrays"
       let help = "for multidimensional arrays or arrays that are fields \
                   inside structs, assume that accesses are in bounds"
     end)

let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module AbsoluteValidRange = struct
  module Info = struct
    let option_name = "-absolute-valid-range"
    let arg_name = "min-max"
    let help = "min and max must be integers in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and fit in 64 bits. Assume that that all absolute addresses outside of the [min-max] range are invalid. In the absence of this option, all absolute addresses are assumed to be invalid"
    let default = ""
    let module_name = "AbsoluteValidRange"
  end
  include String(Info)
end

(* Signed overflows are undefined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module SignedOverflow =
  True
    (struct
      let module_name = "SignedOverflow"
      let option_name = "-warn-signed-overflow"
      let help = "generate alarms for signed operations that overflow."
     end)

(* Unsigned overflows are ok, but might not always be a behavior the programmer
   wants. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module UnsignedOverflow =
  False
    (struct
      let module_name = "UnsignedOverflow"
      let option_name = "-warn-unsigned-overflow"
      let help = "generate alarms for unsigned operations that overflow"
     end)

(* Signed downcast are implementation-defined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module SignedDowncast =
  False
    (struct
      let module_name = "SignedDowncast"
      let option_name = "-warn-signed-downcast"
      let help = "generate alarms when signed downcasts may exceed the \
destination range"
     end)

(* Unsigned downcasts are ok, but might not always be a behavior the programmer
   wants. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module UnsignedDowncast =
  False
    (struct
      let module_name = "UnsignedDowncast"
      let option_name = "-warn-unsigned-downcast"
      let help = "generate alarms when unsigned downcasts may exceed the \
destination range"
     end)

(* ************************************************************************* *)
(** {2 Others options} *)
(* ************************************************************************* *)

let misc = add_group "Miscellaneous Options"

let () =
  Cmdline.add_option_without_action
    "-then"
    ~plugin:""
    ~group:(misc :> Cmdline.Group.t)
    ~help:"parse options before `-then' and execute Frama-C \
accordingly, then parse options after `-then' and re-execute Frama-C"
    ~visible:true
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-then-last"
    ~plugin:""
    ~group:(misc :> Cmdline.Group.t)
    ~help:"like `-then', but the second group of actions is executed \
on the last project created by a program transformer."
    ~visible:true
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-then-on"
    ~plugin:""
    ~argname:"p"
    ~group:(misc :> Cmdline.Group.t)
    ~help:"like `-then', but the second group of actions is executed \
on project <p>"
    ~visible:true
    ~ext_help:""
    ()

let () = Parameter_customize.set_group misc
let () = Parameter_customize.set_negative_option_name ""
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module NoType =
  Bool
    (struct
       let module_name = "NoType"
       let default = not Cmdline.use_type
       let option_name = "-no-type"
       let help = ""
     end)

let () = Parameter_customize.set_group misc
let () = Parameter_customize.set_negative_option_name ""
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module NoObj =
  Bool
    (struct
       let module_name = "NoObj"
       let default = not Cmdline.use_obj
       let option_name = "-no-obj"
       let help = ""
     end)

(* ************************************************************************* *)
(** {2 Checks} *)
(* ************************************************************************* *)

let checks = add_group "Checks"

let () = Parameter_customize.set_group checks
let () = Parameter_customize.do_not_reset_on_copy ()
module Check =
  False(struct
    let option_name = "-check"
    let module_name = "Check"
    let help = "performs consistency checks over the Abstract Syntax \
                        Tree"
  end)

let () = Parameter_customize.set_group checks
module Copy =
  False(struct
    let option_name = "-copy"
    let module_name = "Copy"
    let help =
      "always perform a copy of the original AST before analysis begin"
  end)

let () = Parameter_customize.set_group checks
let () = Parameter_customize.set_negative_option_name ""
module TypeCheck =
  True(struct
          let module_name = "TypeCheck"
          let option_name = "-typecheck"
          let help = "forces typechecking of the source files"
        end)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
