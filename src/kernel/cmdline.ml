(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** This file implements how the command line is parsed.

    The parsing of the Frama-C command line is done in several stages. The first
    one is done when this module is loaded by caml (that is very early).

    At each stage [s], each option [o] put on the command line are checked again
    the recognized options at stage [s]. If [o] is recognized, then its
    associated action is performed. Otherwise [o] will be proceed by the next
    stage.

    Complexity of this algorithm is [O(2*s*o)] where [s] is the number of stages
    and [o] is the number of options puts on the command line options. That is
    quite bad and that could be improved. However it should be good enough in
    practice because there are not so many options put on the command line and
    others Frama-C algorithms take much more time. Parsing the command line
    option is not the more difficult/longer stuff for Frama-C ;-). *)

(* ************************************************************************* *)
(** {2 Global declarations} *)
(* ************************************************************************* *)

module type Level = sig
  val value_if_set: int option ref
  val get: unit -> int
  val set: int -> unit
end

module Make_level(X: sig val default: int end) = struct
  let value_if_set = ref None
  let get () = match !value_if_set with None -> X.default | Some x -> x
  let set n = value_if_set := Some n
end

module Debug_level = Make_level(struct let default = 0 end)
module Verbose_level = Make_level(struct let default = 1 end)
module Kernel_debug_level = Make_level(struct let default = 0 end)
module Kernel_verbose_level = Make_level(struct let default = 1 end)
let kernel_debug_atleast_ref = ref (fun n -> Kernel_debug_level.get () >= n)
let kernel_verbose_atleast_ref = ref (fun n -> Kernel_verbose_level.get () >= n)

module Kernel_log =
  Log.Register
    (struct
      let channel = Log.kernel_channel_name
      let label = Log.kernel_label_name
      let debug_atleast level = !kernel_debug_atleast_ref level
      let verbose_atleast level = !kernel_verbose_atleast_ref level
     end)
let dkey = Kernel_log.register_category "cmdline"

let quiet_ref = ref false
let journal_enable_ref = ref !Config.is_gui
let journal_isset_ref = ref false
let use_obj_ref = ref true
let use_type_ref = ref true

(* ************************************************************************* *)
(** {2 Handling errors} *)
(* ************************************************************************* *)

let long_plugin_name s =
  if s = Log.kernel_label_name then "Frama-C" else "Plug-in " ^ s

let additional_info () =
  if !Config.is_gui then
    "\nReverting to previous state.\n\
Look at the console for additional information (if any)."
  else
    ""

let get_backtrace () =
  (* Get the backtrace before potentially destroying it in the handler below *)
  let bt = Printexc.get_backtrace () in
  let current_src_string =
    try
      let src = Log.get_current_source() in
      Pretty_utils.sfprintf "Current source was: %s:%d@."
        (Filepath.pretty src.Lexing.pos_fname) src.Lexing.pos_lnum
    with Not_found -> "Current source was not set\n"
  in
  current_src_string ^ "The full backtrace is:\n" ^ bt

let request_crash_report =
  Format.sprintf
    "Please report as 'crash' at http://bts.frama-c.com/.\n\
   Your Frama-C version is %s.\n\
   Note that a version and a backtrace alone often do not contain enough\n\
   information to understand the bug. Guidelines for reporting bugs are at:\n\
   http://bts.frama-c.com/dokuwiki/doku.php?id=mantis:frama-c:bug_reporting_guidelines\n"
    Config.version

let protect = function
  | Sys.Break -> 
    "User Interruption (Ctrl-C)" 
    ^ if Kernel_debug_level.get () > 0 then "\n" ^ get_backtrace () else ""
  | Sys_error s -> Printf.sprintf "System error: %s" s
  | Unix.Unix_error(err, a, b) ->
    let error = Printf.sprintf "System error: %s" (Unix.error_message err) in
    (match a, b with
    | "", "" -> error
    | "", t | t, "" -> Printf.sprintf "%s (%s)" error t
    | f, x -> Printf.sprintf "%s (%s %S)" error f x)
  | Log.AbortError p ->
    Printf.sprintf "%s aborted: invalid user input.%s"
      (long_plugin_name p) (additional_info ())
  | Log.AbortFatal p ->
    Printf.sprintf
      "%s\n%s aborted: internal error.%s\n%s"
      (get_backtrace ())
      (long_plugin_name p)
      (additional_info ())
      request_crash_report
  | Log.FeatureRequest(p, m) ->
    let name = long_plugin_name p in
    Printf.sprintf
      "%s aborted: unimplemented feature.%s\n\
         You may send a feature request at http://bts.frama-c.com with:\n\
         '[%s] %s'."
      name (additional_info ()) name m
  | e ->
    Printf.sprintf
      "%s\nUnexpected error (%s).\n%s"
      (get_backtrace ())
      (Printexc.to_string e)
      request_crash_report

(* ************************************************************************* *)
(** {2 Exiting Frama-C} *)
(* ************************************************************************* *)

module NormalExit = Hook.Make(struct end)
let at_normal_exit = NormalExit.extend
let run_normal_exit_hook = NormalExit.apply

module ErrorExit = Hook.Build(struct type t = exn end)
let at_error_exit = ErrorExit.extend
let run_error_exit_hook = ErrorExit.apply
let error_occurred_ref = ref None
let error_occurred exn = error_occurred_ref := Some exn

type exit = unit
exception Exit
let nop = ()

let catch_at_toplevel = function
  | Log.AbortError _ -> true
  | Log.FeatureRequest _ -> true
  | _ -> Kernel_debug_level.get () = 0

let exit_code = function
  | Log.AbortError _ -> 1
  | Sys.Break -> 2
  | Log.FeatureRequest _ -> 3
  | Log.AbortFatal _ -> 4
  | _ -> 125

let bail_out_ref = ref (fun _ -> assert false)
let bail_out () =
  !bail_out_ref (); (* bail_out_ref must call exit 0 *)
  assert false

let catch_toplevel_run ~f ~quit ~at_normal_exit ~on_error =
  (* both functions below handle errors at exit hooks *)
  let run_at_normal_exit () =
    try
      at_normal_exit ()
    with exn ->
      Kernel_log.feedback ~level:0
        "error occurring when exiting Frama-C: stopping exit procedure.\n%s@."
        (protect exn);
      exit 5
  in
  let run_on_error exn =
    try
      on_error exn
    with exn' ->
      Kernel_log.feedback ~level:0
        "error occurring when handling error: stopping error handling \
procedure.\n%s@."
        (protect exn');
      exit 6
  in
  let bail_out () =
    (match !error_occurred_ref with
    | None -> run_at_normal_exit ()
    | Some exn -> run_on_error exn);
    (* even if an error occurred somewhere, Frama-C stops normally. *)
    exit 0
  in
  bail_out_ref := bail_out;
  try
    f ();
    (* write again on stdout *)
    Log.set_output
      (Pervasives.output stdout) (fun () -> Pervasives.flush stdout);
    (if quit then bail_out else run_at_normal_exit) ()
  with
    | Exit ->
        bail_out ()
    | exn when catch_at_toplevel exn ->
        Kernel_log.feedback ~level:0 "%s" (protect exn);
        run_on_error exn;
        exit (exit_code exn)
    | exn ->
        run_on_error exn;
        raise exn

(* ************************************************************************* *)
(** {2 Generic parsing way} *)
(* ************************************************************************* *)

type option_setting =
  | Unit of (unit -> unit)
  | Int of (int -> unit)
  | String of (string -> unit)
  | String_list of (string list -> unit)

exception Cannot_parse of string * string
let raise_error name because = raise (Cannot_parse(name, because))

let error name msg =
  let bin_name = Sys.argv.(0) in
  Kernel_log.abort
    "option `%s' %s.@\nuse `%s -help' for more information."
    name msg bin_name

let all_options = match Array.to_list Sys.argv with
  | [] -> assert false
  | _binary :: l -> l

let get_option_and_arg option arg =
  try
    let k = String.index option '=' in
    let p = succ k in
    String.sub option 0 k ,
    String.sub option p (String.length option - p) , true
  with Not_found ->
    option, arg, false

let parse known_options_list then_expected options_list =
  let known_options = Hashtbl.create 17 in
  List.iter (fun (n, s) -> Hashtbl.add known_options n s) known_options_list;
  let parse_one_option unknown_options option arg =
    let option, arg, explicit = get_option_and_arg option arg in
    let check_string_argname () =
      if not explicit && (arg = "" || arg.[0] = '-') then
        raise_error option "requires a string as argument";
    in
    try
      let setting = Hashtbl.find known_options option in
      let use_arg = match setting with
        | Unit f ->
          if explicit then raise_error option "does not accept any argument";
          f ();
          false
        | Int f ->
          let n =
            try int_of_string arg
            with Failure _ ->
              raise_error option "requires an integer as argument"
          in
          f n;
          true
        | String f ->
          check_string_argname ();
          f arg;
          true
        | String_list f ->
          check_string_argname ();
          f (Str.split (Str.regexp "[ \t]*,[ \t]*") arg);
          true
      in
      unknown_options, use_arg && not explicit, true
    with Not_found ->
      let o = if explicit then option ^ "=" ^ arg else option in
      o :: unknown_options, false, false
  in
  let rec go unknown_options nb_used = function
    | [] -> unknown_options, nb_used, None
    | [ "-then" ] when then_expected ->
      Kernel_log.warning "ignoring last option `-then'";
      unknown_options, nb_used, None
    | [ "-then-on" ] when then_expected ->
      raise_error "-then-on" "requires a string as argument"
    | [ option ] ->
      let unknown, use_arg, is_used =
        parse_one_option unknown_options option ""
      in
      assert (not use_arg);
      unknown, (if is_used then succ nb_used else nb_used), None
    | "-then" :: then_options when then_expected ->
      unknown_options, nb_used, Some (then_options, None)
    | "-then-on" :: project_name :: then_options when then_expected ->
      unknown_options, nb_used, Some (then_options, Some project_name)
    | option :: (arg :: next_options as arg_next) ->
      let unknown, use_arg, is_used =
        parse_one_option unknown_options option arg
      in
      let next = if use_arg then next_options else arg_next in
      go
        unknown
        (if is_used then succ nb_used else nb_used)
        next
  in
  try
    let unknown_options, nb_used, then_options = go [] 0 options_list in
    List.rev unknown_options, nb_used, then_options
  with Cannot_parse(name, msg) ->
    error name msg

(* ************************************************************************* *)
(** {2 First parsing stage at the very beginning of the initialization step} *)
(* ************************************************************************* *)

let non_initial_options_ref = ref []

let () =
  let set_journal b =
    journal_enable_ref := b;
    journal_isset_ref := true
  in
  let first_parsing_stage () =
    parse
    [ "-journal-enable", Unit (fun () -> set_journal true);
      "-journal-disable", Unit (fun () -> set_journal false);
      "-no-obj", Unit (fun () -> use_obj_ref := false);
      "-no-type", Unit (fun () -> use_type_ref := false);
      "-quiet",
      Unit (fun () ->
        quiet_ref := true;
        Verbose_level.set 0;
        Debug_level.set 0);
      "-verbose", Int (fun n -> Verbose_level.set n);
      "-debug", Int (fun n -> Debug_level.set n);
      "-kernel-verbose", Int (fun n -> Kernel_verbose_level.set n);
      "-kernel-debug", Int (fun n -> Kernel_debug_level.set n)
    ]
      false
      all_options
  in
  catch_toplevel_run
    ~f:(fun () ->
      let remaining_options, _, _ = first_parsing_stage () in
      non_initial_options_ref := remaining_options)
    ~quit:false
    ~at_normal_exit:(fun () -> ())
    ~on_error:run_error_exit_hook

let () =
  if not !use_obj_ref then use_type_ref := false;
  if not !use_type_ref then begin
    Type.no_obj ();
    if !journal_enable_ref then begin
      Kernel_log.warning "disabling journal in the 'no obj' mode";
      journal_enable_ref := false
    end
  end

let quiet = !quiet_ref

let journal_enable = !journal_enable_ref
let journal_isset = !journal_isset_ref
let use_obj = !use_obj_ref
let use_type = !use_type_ref

(* ************************************************************************* *)
(** {2 Plugin} *)
(* ************************************************************************* *)

type cmdline_option =
    { oname: string;
      argname: string;
      ohelp: string;
      ovisible: bool;
      ext_help: (unit,Format.formatter,unit) format;
      setting: option_setting }

module Plugin: sig
  type t = private
      { name: string;
        help: string;
        short: string;
        groups: (string, cmdline_option list ref) Hashtbl.t }
  val all_plugins: unit -> t list
  val add: ?short:string -> string -> help:string -> unit
  val add_group: ?memo:bool -> plugin:string -> string -> string * bool
  val add_option: string -> group:string -> cmdline_option -> unit
  val add_aliases:
    orig:string -> string -> group:string -> string list -> cmdline_option list
  val find: string -> t
  val find_option_aliases: cmdline_option -> cmdline_option list
  val is_option_alias: cmdline_option -> bool
end = struct

  type t =
    { name: string;
      help: string;
      short: string;
      groups: (string, cmdline_option list ref) Hashtbl.t }

  (* all the registered plug-ins indexed by their shortnames *)
  let plugins : (string, t) Hashtbl.t = Hashtbl.create 17

  let all_plugins () = Hashtbl.fold (fun _ p acc -> p :: acc) plugins []

  let add ?short name ~help =
    let short = match short with None -> name | Some s -> s in
    if Hashtbl.mem plugins short then
      invalid_arg ("a plug-in " ^ short ^ " is already registered.");
    let groups = Hashtbl.create 7 in
    Hashtbl.add groups "" (ref []);
    Hashtbl.add
      plugins
      short
      { name = name; short = short; help = help; groups = groups }

  let find p =
    try Hashtbl.find plugins p
    with Not_found -> Kernel_log.fatal "Plug-in %s not found" p

  let add_group ?(memo=false) ~plugin name =
    let groups = (find plugin).groups in
    name,
    if Hashtbl.mem groups name then begin
      if not memo then
        Kernel_log.abort
	  "A group of name %s already exists for plug-in %s" name plugin;
      false
    end else begin
      Hashtbl.add groups name (ref []);
      true
    end

  let find_group p g =
    try Hashtbl.find (find p).groups g
    with Not_found -> Kernel_log.fatal "Group %s not found for plug-in %s" g p

  module Option_names : sig
    val add: string -> bool -> unit
    val is_option_alias: string -> bool
  end = struct

    let tbl = Hashtbl.create 7

    let check s =
      if Hashtbl.mem tbl s then
        invalid_arg
          (Format.sprintf "an option with the name %S is already registered." s)

    let add s b =
      check s;
      Hashtbl.add tbl s b

    let is_option_alias s =
      try Hashtbl.find tbl s with Not_found -> assert false

  end

  let add_option shortname ~group option =
    assert (option.oname <> "");
    Option_names.add option.oname false;
    let g = find_group shortname group in
    g := option :: !g

  (* table name_of_the_original_option --> aliases *)
  let aliases_tbl = Hashtbl.create 7

  let add_aliases ~orig shortname ~group names =
    (* mostly inline [add_option] and perform additional actions *)
    let options_group = find_group shortname group in
    let option = List.find (fun o -> o.oname = orig) !options_group in
    let get_one name =
      if name = "" then invalid_arg "empty alias name";
      Option_names.add name true;
      let alias = { option with oname = name } in
      options_group := alias :: !options_group;
      alias
    in
    let aliases = List.map get_one names in
    (try
       let l = Hashtbl.find aliases_tbl orig in
       l := aliases @ !l;
     with Not_found ->
       Hashtbl.add aliases_tbl orig (ref aliases));
    aliases

  let find_option_aliases o =
    try !(Hashtbl.find aliases_tbl o.oname) with Not_found -> []

  let is_option_alias o = Option_names.is_option_alias o.oname

end

let add_plugin = Plugin.add

module Group = struct
  type t = string
  let add = Plugin.add_group
  let default = ""
  let name x = x
end

(* ************************************************************************* *)
(** {2 Parsing} *)
(* ************************************************************************* *)

module Make_Stage
  (S: sig
    val exclusive: bool
    val name: string
    val then_expected: bool
  end) =
struct

  let nb_actions = ref 0
  let is_going_to_run () = incr nb_actions

  module H = Hook.Make(struct end)

  let options  : (string, cmdline_option) Hashtbl.t = Hashtbl.create 17

  let add_for_parsing option = Hashtbl.add options option.oname option

  let add name plugin ?(argname="") help visible ext_help setting =
(*    L.debug ~level:4 "Cmdline: [%s] registers %S for stage %s."
      plugin name S.name;*)
    let help = if help = "" then "undocumented" else help in
    let o =
      { oname = name; 
	argname = argname;
        ohelp = help; 
	ext_help = ext_help; 
	ovisible = visible; 
	setting = setting }
    in
    add_for_parsing o;
    Plugin.add_option plugin o

  let parse options_list =
    Kernel_log.feedback ~dkey
      "parsing command line options of stage %S." 
      S.name;
    let options, nb_used, then_options =
      parse
        (Hashtbl.fold (fun _ o acc -> (o.oname, o.setting) :: acc) options [])
          S.then_expected
        options_list
    in
    let nb_used = nb_used + !nb_actions in
    if S.exclusive && nb_used > 1 then begin
      Kernel_log.abort "at most one %s action must be specified." S.name;
    end;
    H.apply ();
    options, nb_used, then_options

end

module Early_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "early"
      let then_expected = false
     end)

module Extending_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "extending"
      let then_expected = false
     end)

module Extended_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "extended"
      let then_expected = true
     end)

module Exiting_Stage =
  Make_Stage
    (struct
      let exclusive = true
      let name = "exiting"
      let then_expected = false
     end)

module Loading_Stage =
  Make_Stage
    (struct
      let exclusive = true
      let name = "loading"
      let then_expected = false
     end)

let is_going_to_load = Loading_Stage.is_going_to_run

module Configuring_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "configuring"
      let then_expected = false
     end)

let run_after_early_stage = Early_Stage.H.extend
let run_during_extending_stage = Extending_Stage.H.extend
let run_after_extended_stage = Extended_Stage.H.extend
let run_after_exiting_stage = Exiting_Stage.H.extend
let run_after_loading_stage = Loading_Stage.H.extend
let run_after_configuring_stage = Configuring_Stage.H.extend

module After_setting = Hook.Build(struct type t = string list end)
let run_after_setting_files = After_setting.extend

type stage = Early | Extending | Extended | Exiting | Loading | Configuring

let add_option 
    name ~plugin ~group stage ?argname ~help ~visible ~ext_help setting =
  if name <> "" then
    let add = match stage with
      | Early -> Early_Stage.add
      | Extending -> Extending_Stage.add
      | Extended -> Extended_Stage.add
      | Exiting -> Exiting_Stage.add
      | Loading -> Loading_Stage.add
      | Configuring -> Configuring_Stage.add
    in
    add name plugin ~group ?argname help visible ext_help setting

let add_option_without_action
    name ~plugin ~group ?(argname="") ~help ~visible ~ext_help () =
  Plugin.add_option
    plugin
    ~group
    { oname = name; argname = argname;
      ohelp = help; ext_help = ext_help; ovisible = visible;
      setting = Unit (fun () -> assert false) }

let add_aliases orig ~plugin ~group stage aliases =
  let l = Plugin.add_aliases ~orig plugin ~group aliases in
  let add = match stage with
    | Early -> Early_Stage.add_for_parsing
    | Extending -> Extending_Stage.add_for_parsing
    | Extended -> Extended_Stage.add_for_parsing
    | Exiting -> Exiting_Stage.add_for_parsing
    | Loading -> Loading_Stage.add_for_parsing
    | Configuring -> Configuring_Stage.add_for_parsing
  in
  List.iter add l

module On_Files = Hook.Build(struct type t = string list end)
let use_cmdline_files = On_Files.extend

let set_files used_loading l =
  Kernel_log.feedback ~dkey "setting files from command lines.";
  List.iter
    (fun s ->
      if s = "" then error "" "has no name. What do you exactly have in mind?";
      if s.[0] = '-' then error s "is unknown")
    l;
  assert
    (Kernel_log.verify
       (not (On_Files.is_empty ()))
       "no function uses the files provided on the command line");
  if List.length l > 0 then
    if used_loading then
      Kernel_log.warning
        "ignoring source files specified on the command line \
while loading a global initial context."
    else begin
      On_Files.apply l;
      After_setting.apply l
    end

let nb_used_ref = ref 0
let nb_used_relevant = ref false
let nb_given_options () =
  assert
    (Kernel_log.verify
       !nb_used_relevant "function `nb_given_options' called too early");
  !nb_used_ref

let rec play_in_toplevel on_from_name nb_used play options =
  let options, nb_used_extended, then_options_extended =
    Extended_Stage.parse options
  in
  let options, nb_used_exiting, then_options_exiting =
    Exiting_Stage.parse options
  in
  assert (then_options_exiting = None);
  if nb_used_exiting > 0 then
    Kernel_log.fatal "setting an option at the exiting stage must stop Frama-C";
  let options, nb_used_loading, then_options_loading =
    Loading_Stage.parse options
  in
  assert (then_options_loading = None);
  let files, nb_used_config, then_options_configuring =
    Configuring_Stage.parse options
  in
  assert (then_options_configuring = None);
  nb_used_relevant := true;
  nb_used_ref :=
    nb_used
  + nb_used_extended
  + nb_used_exiting
  + nb_used_loading
  + nb_used_config ;
  set_files (nb_used_loading > 0) files;
  Kernel_log.feedback ~dkey "running plug-in mains.";
  play ();
  match then_options_extended with
  | None -> ()
  | Some(options, project_name) ->
    on_from_name
      project_name
      (fun () -> play_in_toplevel on_from_name nb_used play options)

let parse_and_boot on_from_name get_toplevel play =
  let options, nb_used_early, then_options_early =
    Early_Stage.parse !non_initial_options_ref
  in
  assert (then_options_early = None);
  let options, nb_used_extending, then_options_extending =
    Extending_Stage.parse options
  in
  assert (then_options_extending = None);
  get_toplevel
    ()
    (* the extending stage may change the toplevel: applying [get_toplevel]
       provides the good one. *)
    (fun () ->
      play_in_toplevel
        on_from_name
        (nb_used_early + nb_used_extending)
        play
        options)

(* ************************************************************************* *)
(** {2 Help}

    Implement a not very efficient algorithm but it is enough for displaying
    help and exiting. *)
(* ************************************************************************* *)

let print_helpline fmt head help ext_help =
  let n = max 1 (19 - String.length head) in
  Format.fprintf fmt "@[<hov 20>%s%s %t%t@]@\n"
    head
    (* let enough spaces *)
    (String.make n ' ')
    (* the description *)
    (fun fmt ->
      (* add a cutting point at each space *)
      let cut_space fmt s =
        let rec cut_list fmt = function
          | [] -> ()
          | [ s ] -> Format.fprintf fmt "%s" s
          | s :: tl -> Format.fprintf fmt "%s@ %a" s cut_list tl
        in
        cut_list fmt (Str.split (Str.regexp_string " ") s)
      in
      (* replace each '\n' by '@\n' (except for the last one) *)
      let rec cut_newline fmt = function
        | [] -> ()
        | [ s ] -> Format.fprintf fmt "%a" cut_space s
        | s :: tl ->
          Format.fprintf fmt "%a@\n%a" cut_space s cut_newline tl
      in
      cut_newline fmt (Str.split (Str.regexp_string "\n") help))
    (* the extended description *)
    (fun fmt -> Format.fprintf fmt ext_help)

let low_print_option_help fmt print_invisible o =
  if Plugin.is_option_alias o then begin
    false
  end else
    let ty =
      let s = o.argname in
      if s = "" then
        match o.setting with
        | Unit _ -> ""
        | Int _ -> " <n>"
        | String _ -> " <s>"
        | String_list _ -> " <s1, ..., sn>"
      else
        " <" ^ s ^ ">"
    in
    let name = o.oname in
    if print_invisible || o.ovisible then begin
      print_helpline fmt (name ^ ty) o.ohelp o.ext_help;
      List.iter
        (fun o ->
          print_helpline fmt (o.oname ^ ty) (" alias for option " ^ name) "")
        (Plugin.find_option_aliases o)
    end;
    true

let print_option_help fmt ~plugin ~group name =
  let p = Plugin.find plugin in
  let options = 
    try Hashtbl.find p.Plugin.groups group 
    with Not_found -> 
      Kernel_log.fatal "[Cmdline.print_option_help] no group %s" group
  in
  (* linear search... *)
  let rec find_then_print = function
    | [] -> Kernel_log.fatal "[Cmdline.print_option_help] no option %s" name
    | o :: tl -> 
      if o.oname = name then ignore (low_print_option_help fmt true o)
      else find_then_print tl
  in
  find_then_print !options

let option_intro short =
  let first =
    if short <> "" then begin
      let short = "-" ^ short in
      Format.sprintf
        "Most options of the form '%s-option-name'@ and without any \
parameter@ have an opposite with the name '%s-no-option-name'.@\n@\n"
        short short
    end else
      ""
  in
  Format.sprintf
    "%sMost options of the form '-option-name' and without any parameter@ \
have an opposite with the name '-no-option-name'.@\n@\n\
Options taking a string as argument should preferably be written@ \
-option-name=\"argument\"."
    first

let plugin_help shortname =
  let p = Plugin.find shortname in
  if p.Plugin.name <> "" then begin
    assert (p.Plugin.short <> "");
    Log.print_on_output 
      (fun fmt ->
	 Format.fprintf fmt "@[%s:@ %s@]@\n@[%s:@ %s@]@\n"
	   "Plug-in name" p.Plugin.name
	   "Plug-in shortname" shortname)
  end;
  Log.print_on_output
    (fun fmt ->
       Format.fprintf fmt
	 "@[@[%s:@ %s@]@\n@\n%s@\n@\n%s:@\n@\n@[%t@]@]@?"
	 "Description" p.Plugin.help
	 (option_intro shortname)
	 "***** LIST OF AVAILABLE OPTIONS"
	 (fun fmt ->
	    let print_options l =
              List.fold_left
		(fun b o ->
		  let b' = low_print_option_help fmt false o in
		  b || b')
		false
		(List.sort (fun o1 o2 -> String.compare o1.oname o2.oname) l)
	    in
	    let printed = print_options !(Hashtbl.find p.Plugin.groups "") in
	    if printed then Format.pp_print_newline fmt ();
	    let sorted_groups =
              List.sort
		(fun (s1, _) (s2, _) -> String.compare s1 s2)
		(Hashtbl.fold
		   (fun s l acc -> if s = "" then acc else (s, l) :: acc)
		   p.Plugin.groups
		   [])
	    in
	    match sorted_groups with
	      | [] -> ()
	      | g :: l ->
		let print_group newline (s, o) =
		  if newline then Format.pp_print_newline fmt ();
		  Format.fprintf fmt "@[*** %s@]@\n@\n" (String.uppercase s);
		  ignore (print_options !o)
		in
		print_group false g;
		List.iter (print_group true) l));
  raise Exit

let help () =
  let iter_on_plugins f =
    List.iter
      (fun p -> if p.Plugin.name <> "" then f p)
      (List.sort
         (fun p1 p2 -> 
	   String.compare 
	     (String.lowercase p1.Plugin.name) 
	     (String.lowercase p2.Plugin.name))
         (Plugin.all_plugins ()))
  in
  Log.print_on_output
    (fun fmt -> 
       Format.fprintf fmt
	 "@[%t@\n%t@\n@\n%s@\n@\n@[%t@]@]@?"
	 (fun fmt ->
	    Format.fprintf fmt "@[Usage: %s [options and files...]@]" 
	      Sys.argv.(0))
	 (fun fmt ->
	    Format.fprintf fmt
              "@[`%s -kernel-help' provides a description of the general \
options of frama-c@]"
              Sys.argv.(0))
	 "***** LIST OF AVAILABLE PLUG-INS"
	 (fun fmt ->
	    iter_on_plugins
              (fun p ->
		 print_helpline
		   fmt
		   p.Plugin.name
		   (p.Plugin.help
		    ^ ";\n use -" ^ p.Plugin.short
		    ^ "-help for specific options.")
		   "")));
  raise Exit

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
