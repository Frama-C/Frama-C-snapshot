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

open Format

module At_exit : sig
  val extend: (unit -> unit) -> unit
  val clear: unit -> unit
end = struct
  include Hook.Make(struct end)
  let () = at_exit apply
end

module Actions : sig
  val apply: unit -> unit
  val load: unit -> unit
end = struct

  module Hook = Hook.Make(struct end)

    (* CIL initialization and customization *)
  let boot_cil () =
    Cil.initCIL ();
    Cabs2cil.forceRLArgEval := false;
    Cil.lineDirectiveStyle := None;
    (*  Cil.lineDirectiveStyle := Some LinePreprocessorInput;*)
    Cil.printCilAsIs := Cmdline.Debug.get () > 0;
    Mergecil.ignore_merge_conflicts := true;
    Cil.useLogicalOperators := false; (* do not use lazy LAND and LOR *)
    Pretty.flushOften := true

  let apply () =
    Hook.apply ();
    if not (Cmdline.LoadState.is_set ()) then boot_cil ()

  let version () =
    if Cmdline.PrintVersion.get () then
      Format.printf "Version: %s@\nCompilation date: %s@\nFrama-C library path: %s (may be overridden with FRAMAC_SHARE variable)@."
	Version.version Version.date Version.dataroot
  let () = Hook.extend version

  let time () =
    let filename = Cmdline.Time.get () in
    if filename <> "" then
      At_exit.extend
	(fun () ->
           let oc =
	     open_out_gen
	       [ Open_append; Open_creat; Open_binary] 0b111100100 filename
           in
           let {Unix.tms_utime = time } = Unix.times () in
           let now = Unix.localtime (Unix.time ()) in
           Printf.fprintf oc "%02d/%02d/%02d %02d:%02d:%02d %f\n"
             now.Unix.tm_mday
             (now.Unix.tm_mon+1)
             (now.Unix.tm_year - 100)
             now.Unix.tm_hour
             now.Unix.tm_min
             now.Unix.tm_sec
             time;
           flush oc;
           close_out oc)
  let () = Hook.extend time

  let save () =
    let filename = Cmdline.SaveState.get () in
    if filename <> "" then
      At_exit.extend
	(fun () ->
	   try Project.save_all filename
	   with Project.IOError s -> Cil.log "Problem when saving: %s@." s)
  let () = Hook.extend save

  let load () =
    let filename = Cmdline.LoadState.get () in
    if filename <> "" then begin
      if Cmdline.Files.is_set () then
	Cil.log "Warning: ignoring source files specified on command line while loading.";
      try Project.load_all filename
      with Project.IOError s ->
	Cil.log "Problem when loading: %s.@.Exiting.@." s;
	exit 2
    end

  let machdep () =
    (match Cmdline.Machdep.get () with
    | "" -> ()
    | "x86_16" -> let module M = Machdep.DEFINE(Machdep_x86_16) in ()
    | "x86_32" -> let module M = Machdep.DEFINE(Machdep_x86_32) in ()
    | "x86_64" -> let module M = Machdep.DEFINE(Machdep_x86_64) in ()
    | "ppc_32" -> let module M = Machdep.DEFINE(Machdep_ppc_32) in ()
    | "ppc_32_diab" -> let module M = Machdep.DEFINE(Machdep_ppc_32_diab) in ()
    | s -> Format.printf "Unsupported machine %s@." s; exit 1)

  let () = Hook.extend machdep

end

type section = { fullname: string;
                 mutable cmdline: (Arg.key * Arg.spec * Arg.doc) list ;
                 shortname: string;
                 mutable debug: (Arg.key * Arg.spec * Arg.doc) list }

type plugin = { name: string; descr: string; config: unit -> unit }

let available_plugins : plugin list ref = ref []
let available_sections : section list ref = ref []

let has_plugin name = List.exists (fun x -> x.name = name) !available_plugins
let has_section name =
  List.exists (fun x -> x.shortname = name) !available_sections

let find_section ~shortname =
  assert (has_section shortname);
  List.find (fun x -> x.shortname = shortname) !available_sections

let usage () =
  let usage = "toplevel options files..." in
  let plugins =
    match !available_plugins with
    | [] -> usage
    | l ->
        List.fold_left
          (fun s plg ->
             Cil.fprintf_to_string "@[%s@\n  @[%s:@ %a@]@]"
               s plg.name Pretty_utils.pp_print_string_fill plg.descr)
          (usage ^ "\n\nPLUGINS BUILT:") l
  in
  plugins ^ "\n\nAVAILABLE OPTIONS:"

let add_file f = Cmdline.Files.set (f :: Cmdline.Files.get ())

let section name =
  "", Arg.Unit (fun () -> assert false), "\n*** " ^ String.uppercase name

(* The list of global options *)
let cmdline_to_parse = ref []

let add_cmdline x = cmdline_to_parse := !cmdline_to_parse @ x

let add_to_cmdline_section ~shortname ~debug options =
  let section = find_section shortname in
  section.cmdline <- section.cmdline @ options ;
  section.debug <- section.debug @ debug

let add_cmdline_section ~name ~shortname ~debug options =
  let section =
    { fullname = String.capitalize name ; cmdline = options ;
      shortname = String.uncapitalize shortname ; debug = debug }
  in
  available_sections := section :: !available_sections

let add_cmdline ?name ?shortname ?(debug=[]) options =
  match name with
  | None ->
      begin
        match shortname with
        | None -> assert (debug = []); add_cmdline options
        | Some shortname -> add_to_cmdline_section ~shortname ~debug options
      end
  | Some name ->
      begin
        match shortname with
        | None -> add_cmdline_section ~name ~shortname:name ~debug options
        | Some shortname -> add_cmdline_section ~name ~shortname ~debug options
      end

let () = add_cmdline
  [ section "general options" ;

    "-version", Arg.Unit Cmdline.PrintVersion.on, ": print version information";

    "-no-unicode",
    Arg.Unit Cmdline.UseUnicode.off,
    ": do not use utf8 in messages";

    "-save",
    Arg.String Cmdline.SaveState.set,
    "filename : save the state into file [filename] after computations.";

    "-load",
    Arg.String Cmdline.LoadState.set,
    "filename : load an initial (previously saved) state from file [filename].";

    "-time",
    Arg.String Cmdline.Time.set,
    "filename : append user time and date to [filename] at exit.";

    "-quiet",
    Arg.Unit Cmdline.Quiet.on,
    ": do not print results of analyzes on stdout.";

    "-ocode",
    Arg.String Cmdline.CodeOutput.set,
    "filename : when printing code, redirects the output to file [filename].";

    "-lib-entry",
    Arg.Unit (fun () -> Cmdline.LibEntry.unsafe_set true),
    ": run analysis for an incomplete application e.g. an API call. See the -main option to set the entry point name.";

    "-main",
    Arg.String Cmdline.MainFunction.unsafe_set,
    "name : set to name the entry point for analysis. Use -lib-entry if this is not for a complete application. Defaults to main";

    "-machdep",
    Arg.String Cmdline.Machdep.set,
    "machine : use [machine] as the current machine dependent configuration.";

    "-msvc",
    Arg.Set Cil.msvcMode,
    ": switch to MSVC mode. Default mode is gcc.";

    "-debug",
    Arg.Int Cmdline.Debug.set,
    Format.sprintf "n : level of debug (defaults to %d)."
      (Cmdline.Debug.get ());

    section "syntactical tools" ;

    "-print",
    Arg.Unit Cmdline.PrintCode.on,
    " : pretty print original code with its comments.";

    "-simplify-cfg",
    Arg.Unit Cmdline.SimplifyCfg.on,
    ": remove break, continue and switch statement before analyzes.";

    "-keep-switch",
    Arg.Unit Cmdline.KeepSwitch.on,
    ": keep switch statements despite -simplify-cfg.";

    "-keep-comments",
    Arg.Unit Cmdline.PrintComments.on,
    Format.sprintf
      ": try to keep comments in C code (defaults to %b)."
      (Cmdline.PrintComments.get ());

    "-ulevel",
    Arg.Int Cmdline.UnrollingLevel.set,
    Format.sprintf "n : unroll loops n times (defaults to %d) before analyzes."
      (Cmdline.UnrollingLevel.get ());

    "-constfold",
    Arg.Unit Cmdline.Constfold.on,
    ": fold all constant expressions in the code before analysis.";

    "-obfuscate",
    Arg.Unit Cmdline.Obfuscate.on,
    ": print an obfuscated version of files to standard output and exit.";

    "-metrics",
    Arg.Unit Cmdline.Metrics.Print.on,
    ": print some metrics on stdout.";

    "-metrics-dump",
    Arg.String Cmdline.Metrics.Dump.set,
    "<s> : print some metrics into the specified file."; ]

module Startup_Hook = Hook.Make(struct end)
let initialize_toplevels = Startup_Hook.apply

module Init_Hook = Hook.Make(struct end)

let register_plugin_init = Init_Hook.extend

let () =
  Init_Hook.extend (fun () -> Project.set_current (Project.create "default"))

let add_plugin ~name ~descr
    ?plugin_init ?(init=fun () -> ()) ?toplevel_init ?(shortname=name)
    ?(debug=[])
    options =
  let plugin =
    { name = String.capitalize name; descr = descr; config = init }
  in
  available_plugins := plugin :: !available_plugins;
  add_cmdline_section ~name:plugin.name ~shortname ~debug options;
  begin match toplevel_init with
    | None -> ()
    | Some f -> Startup_Hook.extend f
  end;
  match plugin_init with
  | None -> ()
  | Some f -> Init_Hook.extend f

let build_debug_options plugin options =
  let name = "-"^plugin^"-debug" in
  let parse_debug s =
    let argv =
      Array.of_list (Sys.argv.(0)::(Str.split (Str.regexp "[ \t;]+") s))
    in
    let current = ref 0 in
      (try
        Arg.parse_argv ~current argv options
          (fun s ->
	     raise
	       (Arg.Bad (Format.sprintf "Invalid argument to %S:'%S'" name s)))
          ("This is the list of internal options of " ^name ^ ".")
      with
        | Arg.Bad mesg -> Format.eprintf "%s@." mesg;exit 2
        | Arg.Help mesg -> Format.eprintf "%s@." mesg;exit 0)
  in
  name,
  Arg.String parse_debug,
  ("sub-options: use '" ^ name
   ^ " -help' to get information about internal sub-options of " ^ plugin)

let parse_cmdline () =
  (* Execute plugin actions registered with "~plugin_init" *)
  Init_Hook.apply ();
  (* Parse the cmdline *)
  let cmdline =
    List.fold_left
      (fun s sect -> match sect.cmdline, sect.debug with
       | [], [] -> s
       | cmdline, [] -> section sect.fullname :: cmdline @ s
       | cmdline, debug ->
           let debug = build_debug_options sect.shortname debug in
	   section sect.fullname :: cmdline @ ( debug :: s))
      [ section "HELP" ]
      !available_sections
  in
  Arg.parse (!cmdline_to_parse @ cmdline) add_file (usage());
  Actions.apply ()

let init_from_options =
  let first_run = ref true in
  fun () ->
    let res =
      if Cmdline.LoadState.is_set () then begin
        At_exit.clear ();
	let old_save = Cmdline.SaveState.get () in
	Actions.load ();
	Cmdline.set_selected_options ();
	(* do not remember -save set by -load, nor current -load *)
	Cmdline.SaveState.set old_save;
	Cmdline.LoadState.set "";
        (* Execute predefined actions set by options *)
        Actions.apply ();
	true
      end else
	not !first_run
    in
    first_run := false;
    (* Execute plugin actions registered with "~init" *)
    List.iter
      (fun x ->
	 try
           x.config ();
           if Cmdline.Debug.get () > 0 then
             Printf.eprintf "%s registered\n%!" x.name;
	 with exn ->
           Printf.eprintf
             "Warning, configuration of %s failed:\n%s\n%!"
             x.name (Printexc.to_string exn);
           raise exn
      )
      !available_plugins;
    res

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
