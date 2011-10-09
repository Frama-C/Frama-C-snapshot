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

(** the options to launch the toplevel with if the test file is not
     annotated with test options *)
let default_options = "-val -out -input -deps -journal-disable"

let system =
  if Sys.os_type = "Win32" then
    fun f ->
      Unix.system (Format.sprintf "bash -c %S" f)
  else
    fun f ->
      Unix.system f
module Filename = struct
  include Filename
  let concat =
    if Sys.os_type = "Win32" then
      fun a b -> a ^ "/" ^ b
    else
      concat

  let cygpath r =
    let cmd =
      Format.sprintf
        "bash -c \"cygpath -m %s\""
        (String.escaped (String.escaped r))
    in
    let in_channel  = Unix.open_process_in cmd in
    let result = input_line in_channel in
    ignore(Unix.close_process_in in_channel);
    result

  let temp_file =
    if Sys.os_type = "Win32" then
      fun a b -> let r = temp_file a b in
        cygpath r
    else
      fun a b -> temp_file a b
end

let default_env = ref []

let add_default_env x y = default_env:=(x,y)::!default_env

let add_env var value =
    add_default_env var value;
    Unix.putenv var value

let print_default_env fmt =
  match !default_env with
      [] -> ()
    | l ->
        Format.fprintf fmt "@[Env:@\n";
        List.iter (fun (x,y) -> Format.fprintf fmt "%s = \"%s\"@\n"  x y) l;
        Format.fprintf fmt "@]"

let default_env var value =
  try ignore (Unix.getenv var) with Not_found -> add_env var value

let test_paths = [ "tests"; "../../tests" ]

exception Path of string
let test_path =
  try
    List.iter
      (fun p -> if Sys.file_exists p && Sys.is_directory p then raise (Path p))
      test_paths;
    Format.eprintf "No test path found@.";
    exit 1
  with Path p ->
    p

(** the name of the directory-wide configuration file*)
let dir_config_file = "test_config"

(** the files in [suites] whose name matches
    the pattern [test_file_regexp] will be considered as test files *)
let test_file_regexp = ".*\\.\\(c\\|i\\)$"

(** the pattern that ends the parsing of options in a test file *)
let end_comment = Str.regexp ".*\\*/"

let regex_opt = Str.regexp ("\\([^/]+\\)[.]opt\\($\\|[ \t]\\)")
let regex_cmxs = Str.regexp ("\\([^/]+\\)[.]cmxs\\($\\|[ \t]\\)")

let opt_to_byte toplevel =
  if toplevel = "frama-c" then "frama-c.byte"
  else Str.global_replace regex_opt "\\1.byte\\2" toplevel

let opt_to_byte_options options =
  Str.global_replace regex_cmxs "\\1.cmo\\2" options

let needs_byte options =
  Ptests_config.no_native_dynlink &&
    (Str.string_match (Str.regexp ".*-load-script") options 0 ||
    Str.string_match (Str.regexp ".*-load-module") options 0)

let execnow_needs_byte cmd =
  Ptests_config.no_native_dynlink &&
    Str.string_match (Str.regexp ".*make.*[.]cmxs") cmd 0

let execnow_opt_to_byte cmd =
  let cmd = opt_to_byte cmd in
  opt_to_byte_options cmd

let base_path = Filename.current_dir_name
(*    (Filename.concat
        (Filename.dirname Sys.executable_name)
        Filename.parent_dir_name)
*)

let ptests_config = "ptests_local_config.cmo"

(** Command-line flags *)

type behavior = Examine | Update | Run | Show
let behavior = ref Run
let verbosity = ref 0
let use_byte = ref false
let use_diff_as_cmp = ref (Sys.os_type = "Win32")
let do_diffs = ref (if Sys.os_type = "Win32" then "diff --strip-trailing-cr -u"
                    else "diff -u")
let do_cmp = ref (if Sys.os_type="Win32" then !do_diffs
                  else "cmp -s")
let do_make = ref "make"
let n = ref 4    (* the level of parallelism *)
let suites = ref []
(** options given to toplevel for all tests *)
let additional_options = ref ""
(** special configuration, with associated oracles *)
let special_config = ref ""
let do_error_code = ref false

let exclude_suites = ref []

let exclude s = exclude_suites := s :: !exclude_suites

let xunit = ref false

let io_mutex = Mutex.create ()

let lock_fprintf f =
  Mutex.lock io_mutex;
  Format.kfprintf (fun _ -> Mutex.unlock io_mutex) f

let lock_printf s = lock_fprintf Format.std_formatter s
let lock_eprintf s = lock_fprintf Format.err_formatter s

let make_test_suite s =
  suites := s :: !suites

let () =
  if Sys.file_exists ptests_config then
    try
      Dynlink.loadfile ptests_config
    with Dynlink.Error e ->
      Format.eprintf "Could not load dynamic configuration %s: %s@."
        ptests_config (Dynlink.error_message e)
;;

let () =
  default_env "FRAMAC_SHARE" !Ptests_config.framac_share;
  default_env "FRAMAC_PLUGIN" !Ptests_config.framac_plugin;
  default_env "FRAMAC_LIB" !Ptests_config.framac_lib;
  default_env "FRAMAC_PLUGIN_GUI" !Ptests_config.framac_plugin_gui;
  default_env "OCAMLRUNPARAM" "";
  default_env "FRAMAC_OPT" !Ptests_config.toplevel_path;
  default_env "FRAMAC_BYTE" (opt_to_byte !Ptests_config.toplevel_path);
  Unix.putenv "LC_ALL" "C" (* some oracles, especially in Jessie, depend on the
                              locale *)
;;

let example_msg =
  Format.sprintf
    "@.@[<v 0>\
    A test suite can be the name of a directory in ./tests or \
    the path to a file.@ @ \
    @[<v 1>\
    Examples:@ \
     ptests@ \
     ptests -diff \"echo diff\" -examine        \
     # see again the list of tests that failed@ \
     ptests misc                              \
     # for a single test suite@ \
     ptests tests/misc/alias.c                \
     # for a single test@ \
     ptests -examine tests/misc/alias.c       \
     # to see the differences again@ \
     ptests -v -j 1                           \
     # to check the time taken by each test\
    @]@ @]"
;;

let umsg = "Usage: ptests [options] [names of test suites]";;

let rec argspec =
  [
  "-examine", Arg.Unit (fun () -> behavior := Examine) ,
  " Examine the logs that are different from oracles.";
  "-update", Arg.Unit (fun () -> behavior := Update) ,
  " Take the current logs as oracles.";
  "-show", Arg.Unit (fun () -> behavior := Show; use_byte := true) ,
  " Show the results of the tests. Sets -byte.";
  "-run", Arg.Unit (fun () -> behavior := Run) ,
  " (default) Delete logs, run tests, then examine logs different from \
  oracles.";
  "-v", Arg.Unit (fun () -> incr verbosity),
  " Increase verbosity (up to  twice)" ;
  "-diff", Arg.String (fun s -> do_diffs := s;
    if !use_diff_as_cmp then do_cmp := s),
  "<command>  Use command for diffs" ;
  "-cmp", Arg.String (fun s -> do_cmp:=s),
  "<command>  Use command for comparison";
  "-make", Arg.String (fun s -> do_make := s;),
  "<command> Use command instead of make";
  "-use-diff-as-cmp",
   Arg.Unit (fun () -> use_diff_as_cmp:=true; do_cmp:=!do_diffs),
  " Use the diff command for performing comparisons";
  "-j", Arg.Int
    (fun i -> if i>=0
      then n := i
      else ( lock_printf "Option -j requires nonnegative argument@.";
             exit (-1))),
  "<n>  Use nonnegative integer n for level of parallelism" ;
  "-byte", Arg.Set use_byte,
  " Use bytecode toplevel";
  "-opt", Arg.Clear use_byte,
  " Use native toplevel (default)";
  "-config", Arg.Set_string special_config,
  " Use special configuration and oracles";
  "-add-options", Arg.Set_string additional_options,
  " Add additional options to be passed to the toplevels that will be launched";
  "-exclude", Arg.String exclude,
  " Exclude a test or a suite from the run";
  "-xunit", Arg.Set xunit,
  " Create a xUnit file named xunit.xml collecting results";
  "-error-code", Arg.Set do_error_code,
  " Exit with error code 1 if tests failed (useful for scripts";
]
and help_msg () = Arg.usage (Arg.align argspec) umsg;;

let () =
  Arg.parse
    ((Arg.align
        (List.sort
        (fun (optname1, _, _) (optname2, _, _) ->
          Pervasives.compare optname1 optname2
        ) argspec)
     ) @ ["", Arg.Unit (fun () -> ()), example_msg;])
    make_test_suite umsg
;;

(* redefine config file if special configuration expected *)
let dir_config_file =
  if !special_config = "" then dir_config_file else
    dir_config_file ^ "_" ^ !special_config

let make_toplevel_path exec = exec
(*  if Filename.is_relative exec then
    Filename.concat (Filename.concat base_path "bin") exec
  else exec
*)

(* redefine oracle directory if special configuration expected *)
let oracle_dirname =
  if !special_config = "" then "oracle" else
    "oracle_" ^ !special_config

(* redefine result directory if special configuration expected *)
let result_dirname =
  if !special_config = "" then "result" else
    "result_" ^ !special_config

let gen_make_file s dir file = Filename.concat (Filename.concat dir s) file
let make_result_file = gen_make_file result_dirname
let make_oracle_file = gen_make_file oracle_dirname

let toplevel_regex = Str.regexp "\\(.*\\)@frama-c@\\(.*\\)"

type execnow =
    {
      ex_cmd: string;      (** command to launch *)
      ex_log: string list; (** log files *)
      ex_bin: string list; (** bin files *)
      ex_dir: string;      (** directory of test suite *)
    }

(** configuration of a directory/test. *)
type config =
    {
      dc_test_regexp: string; (** regexp of test files. *)
      dc_execnow    : execnow list; (** command to be launched before
                                         the toplevel(s)
                                     *)
      dc_default_toplevel   : string;
      (** full path of the default toplevel. *)
      dc_filter     : string option; (** optional filter to apply to
                              standard output *)
      dc_toplevels    : (string * string) list;
      (** toplevel full path and options to launch the toplevel on *)
      dc_dont_run   : bool;
      dc_is_explicit_test: bool
        (** set to true for single test files that are explicitly
            mentioned on the command line. Overrides dc_dont_run. *)
    }

let default_config =
  { dc_test_regexp = test_file_regexp ;
    dc_execnow = [];
    dc_filter = None ;
    dc_default_toplevel = !Ptests_config.toplevel_path;
    dc_toplevels = [ !Ptests_config.toplevel_path, default_options ];
    dc_dont_run = false;
    dc_is_explicit_test = false
  }

let launch command_string =
  let result = system command_string in
  match result with
  | Unix.WEXITED 127 ->
      lock_printf "%% Couldn't execute command. Retrying once.@.";
      Thread.delay 0.1;
      ( match system command_string with
        Unix.WEXITED r when r <> 127 -> r
      | _ -> lock_printf "%% Retry failed with command:@\n%s@\nStopping@."
          command_string ;
          exit 1 )
  | Unix.WEXITED r -> r
  | Unix.WSIGNALED s ->
      lock_printf
        "%% SIGNAL %d received while executing command:@\n%s@\nStopping@."
        s command_string ;
      exit 1
  | Unix.WSTOPPED s ->
      lock_printf
        "%% STOP %d received while executing command:@\n%s@\nStopping@."
        s command_string;
      exit 1

let replace_toplevel s =
  if Str.string_match toplevel_regex s 0 then
    Str.replace_matched ("\\1" ^ !Ptests_config.toplevel_path ^ "\\2") s
  else s

let scan_execnow dir (s:string) =
  let rec aux (s:execnow) =
    try
      Scanf.sscanf s.ex_cmd "%_[ ]LOG%_[ ]%[A-Za-z0-9_',+=:.\\-]%_[ ]%s@\n"
        (fun name cmd ->
          let cmd = replace_toplevel cmd in
          aux { s with ex_cmd = cmd; ex_log = name :: s.ex_log })
    with Scanf.Scan_failure _ ->
      try
        Scanf.sscanf s.ex_cmd "%_[ ]BIN%_[ ]%[A-Za-z0-9_.\\-]%_[ ]%s@\n"
          (fun name cmd ->
            let cmd = replace_toplevel cmd in
            aux { s with ex_cmd = cmd; ex_bin = name :: s.ex_bin })
      with Scanf.Scan_failure _ ->
        try
          Scanf.sscanf s.ex_cmd "%_[ ]make%_[ ]%s@\n"
            (fun cmd ->
              let cmd = replace_toplevel cmd in
              let s = aux ({ s with ex_cmd = cmd; }) in
              { s with ex_cmd = !do_make^" "^cmd; } )
        with Scanf.Scan_failure _ ->
          s
  in
  aux { ex_cmd = s; ex_log = []; ex_bin = []; ex_dir = dir }

(* the default toplevel for the current level of options. *)
let current_default_toplevel = ref !Ptests_config.toplevel_path
let current_default_cmds = ref [!Ptests_config.toplevel_path,default_options]

let make_custom_opts stdopts s =
  let rec aux opts s =
    try
      Scanf.sscanf s "%_[ ]%1[+\\-]%_[ ]\"%s@\"%_[ ]%s@\n"
        (fun c opt rem ->
           match c with
             | "+" -> aux (opt :: opts) rem
             | "-" -> aux (List.filter (fun x -> x <> opt) opts) rem
             | _ -> assert false (* format of scanned string disallow it *))
    with
        Scanf.Scan_failure _ ->
          if s <> "" then
            lock_eprintf "unknown STDOPT configuration string: %s\n%!" s;
          opts
      | End_of_file -> opts
  in
  (* NB: current settings does not allow to remove a multiple-argument
     option (e.g. -verbose 2).
   *)
  (* revert the initial list, as it will be reverted back in the end. *)
  let opts =
    aux (List.rev (Str.split (Str.regexp " ") stdopts)) s
  in
  (* preserve options ordering *)
  List.fold_right (fun x s -> s ^ " " ^ x) opts ""

(* how to process options *)
let config_options =
  [ "CMD",
    (fun _ s current ->

       let toplevel =
         if Str.string_match toplevel_regex s 0 then
           Str.replace_matched ("\\1" ^ !Ptests_config.toplevel_path ^ "\\2") s
       else
         make_toplevel_path s
       in
       { current with dc_default_toplevel = toplevel});

    "OPT",
    (fun _ s current ->
       let t = current.dc_default_toplevel, s in
       { current with
(*           dc_default_toplevel = !current_default_toplevel;*)
           dc_toplevels = t :: current.dc_toplevels });

    "STDOPT",
    (fun _ s current ->
       let new_top =
         List.map (fun (cmd,opts) -> cmd, make_custom_opts opts s)
           !current_default_cmds
       in
       { current with dc_toplevels =
           List.rev_append new_top current.dc_toplevels});

    "FILEREG",
    (fun _ s current -> { current with dc_test_regexp = s });

    "FILTER",
    (fun _ s current -> { current with dc_filter = Some s });

    "GCC",
    (fun _ _ acc -> acc);

    "COMMENT",
    (fun _ _ acc -> acc);

    "DONTRUN",
    (fun _ s current ->
       if current.dc_is_explicit_test then current
       else { current with dc_dont_run = true });

    "EXECNOW",
    (fun dir s current ->
       let execnow = scan_execnow dir s in
       { current with dc_execnow = execnow::current.dc_execnow  });
  ]

let scan_options dir scan_buffer default =
  let r =
    ref { default with dc_toplevels = [] }
  in
  current_default_toplevel := default.dc_default_toplevel;
  current_default_cmds := List.rev default.dc_toplevels;
  let treat_line s =
    try
      Scanf.sscanf s "%[ *]%[A-Za-z0-9]:%s@\n"
        (fun _ name opt ->
          try
            r := (List.assoc name config_options) dir opt !r
          with Not_found ->
            lock_eprintf "@[unknown configuration option: %s@\n%!@]" name)
    with Scanf.Scan_failure _ ->
      if Str.string_match end_comment s 0
      then raise End_of_file
      else ()
  in
  try
    while true do
      Scanf.bscanf scan_buffer "%s@\n" treat_line
    done;
    assert false
  with
    End_of_file ->
      (match !r.dc_toplevels with
        | [] -> { !r with dc_toplevels = default.dc_toplevels }
        | l -> { !r with dc_toplevels = List.rev l })

let scan_test_file default dir f =
  let f = Filename.concat dir f in
  let exists_as_file =
    try
      (Unix.lstat f).Unix.st_kind = Unix.S_REG
    with Unix.Unix_error _ | Sys_error _ -> false
  in
    if exists_as_file then begin
        let scan_buffer = Scanf.Scanning.from_file f in
        let rec scan_config () =
          (* space in format string matches any number of whitespace *)
          Scanf.bscanf scan_buffer " /* run.config%s "
            (fun name ->
               if not
                 (!special_config = "" && name = ""
                     || name = "_" ^ !special_config)
               then
                 (ignore (scan_options dir scan_buffer default);
                  scan_config ()))
        in
        try
          scan_config ();
          scan_options dir scan_buffer default
        with End_of_file | Scanf.Scan_failure _ -> default
      end else
      (* if the file has disappeared, don't try to run it... *)
      { default with dc_dont_run = true }

type toplevel_command =
    { file : string ;
      options : string ;
      toplevel: string ;
      filter : string option ;
      directory : string ;
      n : int }

type command =
  | Toplevel of toplevel_command
  | Target of execnow * command Queue.t

type log = Err | Res

type diff =
  | Command_error of toplevel_command * log
  | Target_error of execnow
  | Log_error of string (** directory *) * string (** file *)

type cmps =
  | Cmp_Toplevel of toplevel_command
  | Cmp_Log of string (** directory *) * string (** file *)

type shared =
    { lock : Mutex.t ;
      mutable building_target : bool ;
      target_queue : command Queue.t ;
      commands_empty : Condition.t ;
      work_available : Condition.t ;
      diff_available : Condition.t ;
      mutable commands : command Queue.t ; (* file, options, number *)
      cmps : cmps Queue.t ;
      (* command that has finished its execution *)
      diffs : diff Queue.t ;
      (* cmp that showed some difference *)
      mutable commands_finished : bool ;
      mutable cmp_finished : bool ;
      mutable summary_run : int ;
      mutable summary_ok : int ;
      mutable summary_log : int;
    }

let shared =
  { lock = Mutex.create () ;
    building_target = false ;
    target_queue = Queue.create () ;
    commands_empty = Condition.create () ;
    work_available = Condition.create () ;
    diff_available = Condition.create () ;
    commands = Queue.create () ;
    cmps = Queue.create () ;
    diffs = Queue.create () ;
    commands_finished = false ;
    cmp_finished = false ;
    summary_run = 0 ;
    summary_ok = 0 ;
    summary_log = 0 }

let unlock () = Mutex.unlock shared.lock

let lock () = Mutex.lock shared.lock

let catenate_number prefix n =
  if n > 0
  then prefix ^ "." ^ (string_of_int n)
  else prefix

let name_without_extension command =
  try
    (Filename.chop_extension command.file)
  with
    Invalid_argument _ ->
      failwith ("This test file does not have any extension: " ^
                   command.file)

let gen_prefix s cmd =
  let prefix = gen_make_file s cmd.directory (name_without_extension cmd) in
  catenate_number prefix cmd.n

let log_prefix = gen_prefix result_dirname
let oracle_prefix = gen_prefix oracle_dirname

let basic_command_string command =
  let is_framac_toplevel =
    Str.string_match (Str.regexp ".*toplevel.*") command.toplevel 0
    || Str.string_match (Str.regexp ".*frama-c.*") command.toplevel 0
  in
  command.toplevel ^ " " ^
    (Filename.concat command.directory command.file) ^ " " ^
    command.options ^
    (if is_framac_toplevel then " " ^ !additional_options else "")

let command_string command =
  let log_prefix = log_prefix command in
  let errlog = log_prefix ^ ".err.log" in
  let stderr = match command.filter with
      None -> errlog
    | Some _ ->
        let stderr =
          Filename.temp_file (Filename.basename log_prefix) ".err.log"
        in
        at_exit
          (fun () ->  try Unix.unlink stderr with Unix.Unix_error _ -> ());
        stderr
  in
  let filter = match command.filter with
    | None -> None
    | Some filter ->
        let len = String.length filter in
        let rec split_filter i =
          if i < len && filter.[i] = ' ' then split_filter (i+1)
          else
            try
              let idx = String.index_from filter i ' ' in
              String.sub filter i idx,
              String.sub filter idx (len - idx)
            with Not_found ->
              String.sub filter i (len - i), ""
        in
        let exec_name, params = split_filter 0 in
        let exec_name =
          if Sys.file_exists exec_name || not (Filename.is_relative exec_name)
          then exec_name
          else
            Filename.concat
              (Filename.dirname (Filename.dirname log_prefix))
              (Filename.basename exec_name)
        in
        Some (exec_name ^ params)
  in
  let command_string = basic_command_string command in
  let command_string =
    command_string ^ " 2>" ^ stderr
  in
  let command_string = match filter with
    | None -> command_string
    | Some filter -> command_string ^ " | " ^ filter
  in
  let command_string = command_string ^ " >" ^ log_prefix ^ ".res.log" in
  let command_string = match filter with
    | None -> command_string
    | Some filter ->
        Printf.sprintf "%s && %s < %s > %s && rm -f %s"
          command_string filter stderr errlog stderr
  in
  command_string

let update_toplevel_command command =
  let log_prefix = log_prefix command in
  let oracle_prefix = oracle_prefix command in
  let command_string =
    "mv " ^
      log_prefix ^ ".res.log " ^
      oracle_prefix ^ ".res.oracle"
  in
  ignore (launch command_string);
 let command_string =
    "mv " ^
      log_prefix ^ ".err.log " ^
      oracle_prefix ^ ".err.oracle"
  in
  ignore (launch command_string)

let update_log_files dir file =
  let command_string =
    "mv " ^ make_result_file dir file ^ " " ^ make_oracle_file dir file
  in
  ignore (launch command_string)

let rec update_command = function
    Toplevel cmd -> update_toplevel_command cmd
  | Target (execnow,cmds) ->
      List.iter (update_log_files execnow.ex_dir) execnow.ex_log;
      Queue.iter update_command cmds

let remove_execnow_results execnow =
  List.iter
    (fun f ->
       try Unix.unlink (make_result_file execnow.ex_dir f)
       with Unix.Unix_error _ -> ())
    (execnow.ex_bin @ execnow.ex_log)

module Make_Report(M:sig type t end)=struct
  module H=Hashtbl.Make
    (struct
      type t = toplevel_command
      let project cmd = (cmd.directory,cmd.file,cmd.n)
      let compare c1 c2 = Pervasives.compare (project c1) (project c2)
      let equal c1 c2 =  (project c1)=(project c2)
      let hash c = Hashtbl.hash (project c)
     end)
  let tbl = H.create 774
  let m = Mutex.create ()
  let record cmd (v:M.t) =
    if !xunit then begin
      Mutex.lock m;
      H.add tbl cmd v;
      Mutex.unlock m
    end
  let iter f =
    Mutex.lock m;
    H.iter f tbl;
    Mutex.unlock m
  let find k = H.find tbl k
  let remove k = H.remove tbl k

end
module Report_run=Make_Report(struct type t=int
(* At some point will contain the running time*)
end)

let report_run cmp r = Report_run.record cmp r
module Report_cmp=Make_Report(struct type t=int*int end)
let report_cmp = Report_cmp.record
let pretty_report fmt =
  Report_run.iter
    (fun test _run_result ->
      Format.fprintf fmt
        "<testcase classname=%S name=%S>%s</testcase>@."
        (Filename.basename test.directory) test.file
        (let res,err = Report_cmp.find test in
         Report_cmp.remove test;
         (if res=0 && err=0 then "" else
             Format.sprintf "<failure type=\"Regression\">%s</failure>"
               (if res=1 then "Stdout oracle difference"
                else if res=2 then "Stdout System Error (missing oracle?)"
                else if err=1 then "Stderr oracle difference"
                else if err=2 then "Stderr System Error (missing oracle?)"
                else "Unexpected errror"))));
  (* Test that were compared bu not runned *)
  Report_cmp.iter
    (fun test (res,err) ->
      Format.fprintf fmt
        "<testcase classname=%S name=%S>%s</testcase>@."
        (Filename.basename test.directory) test.file
        (if res=0 && err=0 then "" else
            Format.sprintf "<failure type=\"Regression\">%s</failure>"
              (if res=1 then "Stdout oracle difference"
               else if res=2 then "Stdout System Error (missing oracle?)"
               else if err=1 then "Stderr oracle difference"
               else if err=2 then "Stderr System Error (missing oracle?)"
               else "Unexpected errror")))
let xunit_report () =
  if !xunit then begin
    let out = open_out_bin "xunit.xml" in
    let fmt = Format.formatter_of_out_channel out in
    Format.fprintf fmt
      "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\
      <testsuite errors=\"0\" failures=\"%d\" name=\"%s\" tests=\"%d\" time=\"0.0\" timestamp=\"%f\">%t@\n\
       </testsuite>@."
      (shared.summary_log-shared.summary_ok)
      "Frama-C"
      shared.summary_log
      (Unix.gettimeofday ())
      pretty_report;
    close_out out;
  end


let do_command command =
  match command with
    Toplevel command ->
      (* Update : copy the logs. Do not enqueue any cmp
         Run | Show: launch the command, then enqueue the cmp
         Examine : just enqueue the cmp *)
      if !behavior = Update
      then update_toplevel_command command
      else begin
          (* Run, Show or Examine *)
          if !behavior <> Examine
          then begin
              let command_string = command_string command in
              if !verbosity >= 1
              then lock_printf "%% launch %s@." command_string ;
              report_run command (launch command_string)
          end;
          lock ();
          shared.summary_run <- succ shared.summary_run ;
          shared.summary_log <- shared.summary_log + 2 ;
          Queue.push (Cmp_Toplevel command) shared.cmps;
          unlock ()
        end
  | Target (execnow, cmds) ->
      let continue res =
          lock();
          shared.summary_log <- succ shared.summary_log;
          if res = 0
          then begin
              shared.summary_ok <- succ shared.summary_ok;
            Queue.transfer shared.commands cmds;
            shared.commands <- cmds;
            shared.building_target <- false;
            Condition.broadcast shared.work_available;
            if !behavior = Examine || !behavior = Run
            then begin
              List.iter
                (fun f -> Queue.push (Cmp_Log(execnow.ex_dir, f)) shared.cmps)
                execnow.ex_log
            end
          end
          else begin
            let rec treat_cmd = function
                Toplevel cmd ->
                  shared.summary_run <- shared.summary_run + 1;
                  let log_prefix = log_prefix cmd in
                  begin try
                    Unix.unlink (log_prefix ^ ".res.log ")
                  with Unix.Unix_error _ -> ()
                  end;
              | Target (execnow,cmds) ->
                  shared.summary_run <- succ shared.summary_run;
                  remove_execnow_results execnow;
                  Queue.iter treat_cmd cmds
            in
            Queue.iter treat_cmd cmds;
            Queue.push (Target_error execnow) shared.diffs;
            shared.building_target <- false;
            Condition.signal shared.diff_available
          end;
          unlock()
      in

      if !behavior = Update then begin
          update_command command;
          lock ();
          shared.building_target <- false;
          Condition.signal shared.work_available;
          unlock ();
        end else
        begin
            if !behavior <> Examine
            then begin
              remove_execnow_results execnow;
              let cmd =
                if !use_byte || execnow_needs_byte execnow.ex_cmd then
                  execnow_opt_to_byte execnow.ex_cmd
                else
                  execnow.ex_cmd
              in
              let r = launch cmd in
              continue r
            end
            else
              continue 0
        end

let log_ext = function Res -> ".res" | Err -> ".err"

let compare_one_file cmp log_prefix oracle_prefix log_kind =
  if !behavior = Show
  then begin
    lock();
    Queue.push (Command_error(cmp,log_kind)) shared.diffs;
    Condition.signal shared.diff_available;
    unlock();
    -1
  end else
    let ext = log_ext log_kind in
    let log_file = log_prefix ^ ext ^ ".log " in
    let oracle_file = oracle_prefix ^ ext ^ ".oracle" in
    let cmp_string =
      !do_cmp ^ " " ^ log_file ^ oracle_file ^ " > /dev/null 2> /dev/null"
    in
    if !verbosity >= 2 then lock_printf "%% cmp%s (%d) :%s@."
      ext
      cmp.n
      cmp_string;
    match launch cmp_string with
      0 ->
        lock();
        shared.summary_ok <- shared.summary_ok + 1;
        unlock();
        0
    | 1 ->
        lock();
        Queue.push (Command_error (cmp,log_kind)) shared.diffs;
        Condition.signal shared.diff_available;
        unlock();
        1
    | 2 ->
        lock_printf
          "%% System error while comparing. Maybe one of the files is missing...@\n%s or %s@."
          log_file oracle_file;
         2
    | n ->
        lock_printf
          "%% Comparison function exited with code %d for files %s and %s. \
           Allowed exit codes are 0 (no diff), 1 (diff found) and \
           2 (system error). This is a fatal error.@." n log_file oracle_file;
        exit 2

let compare_one_log_file dir file =
  if !behavior = Show
  then begin
    lock();
    Queue.push (Log_error(dir,file)) shared.diffs;
    Condition.signal shared.diff_available;
    unlock()
  end else
    let log_file = make_result_file dir file in
    let oracle_file = make_oracle_file dir file in
    let cmp_string = !do_cmp ^ " " ^ log_file ^ " " ^ oracle_file ^ " > /dev/null 2> /dev/null" in
    if !verbosity >= 2 then lock_printf "%% cmplog: %s / %s@." dir file;
    shared.summary_log <- succ shared.summary_log;
    match launch cmp_string with
      0 ->
        lock();
        shared.summary_ok <- shared.summary_ok + 1;
        unlock()
    | 1 ->
        lock();
        Queue.push (Log_error (dir,file)) shared.diffs;
        Condition.signal shared.diff_available;
        unlock()
    | 2 ->
        lock_printf
          "%% System error while comparing. Maybe one of the files is missing...@\n%s or %s@."
          log_file oracle_file;
    | n ->
        lock_printf
          "%% Diff function exited with code %d for files %s and %s. \
           Allowed exit codes are 0 (no diff), 1 (diff found) and \
           2 (system error). This is a fatal error.@." n log_file oracle_file;
        exit 2

let do_cmp = function
  | Cmp_Toplevel cmp ->
      let log_prefix = log_prefix cmp in
      let oracle_prefix = oracle_prefix cmp in
      let res = compare_one_file cmp log_prefix oracle_prefix Res in
      let err = compare_one_file cmp log_prefix oracle_prefix Err in
      report_cmp cmp (res,err)
  | Cmp_Log(dir, f) ->
      ignore (compare_one_log_file dir f)

let worker_thread () =
  while true do
    lock () ;
    if (Queue.length shared.commands) + (Queue.length shared.cmps) < !n
    then Condition.signal shared.commands_empty;
    try
      let cmp = Queue.pop shared.cmps in
      unlock () ;
      do_cmp cmp
    with Queue.Empty ->
      try
        let rec real_command () =
          let command =
            try
              if shared.building_target then raise Queue.Empty;
              Queue.pop shared.target_queue
            with Queue.Empty ->
              Queue.pop shared.commands
          in
          match command with
            Target _ ->
              if shared.building_target
              then begin
                  Queue.push command shared.target_queue;
                  real_command()
                end
              else begin
                  shared.building_target <- true;
                  command
                end
          | _ -> command
        in
        let command = real_command() in
        unlock () ;
        do_command command
      with Queue.Empty ->
        if shared.commands_finished
          && Queue.is_empty shared.target_queue
          && not shared.building_target
          (* a target being built would mean work can still appear *)

        then (unlock () ; Thread.exit ());

        Condition.signal shared.commands_empty;
        (* we still have the lock at this point *)

        Condition.wait shared.work_available shared.lock;
          (* this atomically releases the lock and suspends
             the thread on the condition work_available *)

        unlock ();
  done

let do_diff = function
    | Command_error (diff, kind) ->
      let log_prefix = log_prefix diff in
      let log_ext = log_ext kind in
      let command_string = command_string diff in
      lock_printf "%tCommand:@\n%s@." print_default_env command_string;
      if !behavior = Show
      then ignore (launch ("cat " ^ log_prefix ^ log_ext ^ ".log"))
      else
        let oracle_prefix = oracle_prefix diff in
        let diff_string =
          !do_diffs ^ " " ^
            oracle_prefix ^ log_ext ^ ".oracle " ^
            log_prefix ^ log_ext ^ ".log"
        in
        ignore (launch diff_string)
  | Target_error execnow ->
      lock_printf "Custom command failed: %s@\n" execnow.ex_cmd
  | Log_error(dir, file) ->
      let result_file = make_result_file dir file in
      lock_printf "Log of %s:@." result_file;
      if !behavior = Show
      then ignore (launch ("cat " ^ result_file))
      else
        let diff_string =
          !do_diffs ^ " " ^ make_oracle_file dir file ^ " " ^ result_file
        in
        ignore (launch diff_string)


let diff_thread () =
  lock () ;
  while true do
    try
      let diff = Queue.pop shared.diffs in
      unlock ();
      do_diff diff;
      lock ()
    with Queue.Empty ->
      if shared.cmp_finished then (unlock () ; Thread.exit ());

      Condition.wait shared.diff_available shared.lock
      (* this atomically releases the lock and suspends
         the thread on the condition cmp_available *)
  done

let test_pattern config =
  let regexp = Str.regexp config.dc_test_regexp in
  fun file ->
    Str.string_match regexp file 0

let files = Queue.create ()

(* test for a possible toplevel configuration. *)
let default_config =
  let general_config_file = Filename.concat test_path dir_config_file in
    if Sys.file_exists general_config_file
    then begin
      let scan_buffer = Scanf.Scanning.from_file general_config_file in
      scan_options Filename.current_dir_name scan_buffer default_config
    end
    else default_config

let () =
  (* enqueue the test files *)
  let suites =
    match !suites with
      [] ->
        let priority = "idct" in
        let default = !Ptests_config.default_suites in
        if List.mem priority default
        then priority :: (List.filter (fun name -> name <> priority) default)
        else default
    | l -> List.rev l
  in
  let interpret_as_file suite =
        try
          let ext = Filename.chop_extension suite in
          ext <> ""
        with Invalid_argument _ -> false
      in
  let exclude_suite, exclude_file =
    List.fold_left
      (fun (suite,test) x ->
         if interpret_as_file x then (suite,x::test) else (x::suite,test))
      ([],[]) !exclude_suites
  in
  List.iter
    (fun suite ->
       if !verbosity >= 2 then lock_printf "%% producer now treating test %s\n%!" suite;
      (* the "suite" may be a directory in [test_path] or a single file *)
       let interpret_as_file = interpret_as_file suite in
       let directory =
         if interpret_as_file
         then
           Filename.dirname suite
         else
           Filename.concat test_path suite
       in
       let config = Filename.concat directory dir_config_file in
       let dir_config =
         if Sys.file_exists config
         then begin
           let scan_buffer = Scanf.Scanning.from_file config in
           scan_options directory scan_buffer default_config
         end
         else default_config
       in
       if interpret_as_file
       then begin
         if not (List.mem suite exclude_file) then
           Queue.push (Filename.basename suite, directory,
                       { dir_config with dc_is_explicit_test = true}) files
       end
       else begin
         if not (List.mem suite exclude_suite) then begin
           let dir_files = Sys.readdir directory in
           for i = 0 to pred (Array.length dir_files) do
             let file = dir_files.(i) in
             assert (Filename.is_relative file);
             if test_pattern dir_config file &&
               (not (List.mem (Filename.concat directory file) exclude_file))
             then Queue.push (file, directory, dir_config) files;
           done
         end
        end)
    suites

let dispatcher () =
  try
    while true
    do
      lock ();
      while (Queue.length shared.commands) + (Queue.length shared.cmps) >= !n
      do
        Condition.wait shared.commands_empty shared.lock;
      done;
      (* we have the lock *)
      let file, directory, config = Queue.pop files in
      let config =
        scan_test_file config directory file in
      let i = ref 0 in
      let make_toplevel_cmd (toplevel, options) =
        let toplevel, options =
          if !use_byte || needs_byte options
          then opt_to_byte toplevel, opt_to_byte_options options
          else toplevel,options
        in
        {file=file; options = options; toplevel = toplevel;
         n = !i; directory = directory;
         filter = config.dc_filter}
      in
      let treat_option q option =
        Queue.push
          (Toplevel (make_toplevel_cmd option))
          q;
        incr i
      in
      if not config.dc_dont_run
      then begin
        (match config.dc_execnow with
         | hd :: tl ->
             let subworkqueue = Queue.create () in
             List.iter (treat_option subworkqueue) config.dc_toplevels;
             let target =
               List.fold_left
                 (fun current_target execnow ->
                    let subworkqueue = Queue.create () in
                    Queue.add current_target subworkqueue;
                    Target(execnow,subworkqueue))
                 (Target(hd,subworkqueue)) tl
             in
             Queue.push target shared.commands
         | [] ->
             List.iter
               (treat_option shared.commands)
               config.dc_toplevels);
        Condition.broadcast shared.work_available;
      end;
      unlock () ;
    done
  with Queue.Empty ->
    shared.commands_finished <- true;
    unlock ()

let () =
  let worker_ids = Array.init !n
    (fun _ -> Thread.create worker_thread ())
  in
  let diff_id = Thread.create diff_thread () in

  dispatcher ();
  if !behavior = Run
  then
    lock_printf "%% Dispatch finished, waiting for workers to complete@.";
  ignore (Thread.create
    (fun () ->
      while true do
        Condition.broadcast shared.work_available;
        Thread.delay 0.5;
      done)
    ());
  Array.iter Thread.join worker_ids;

  if !behavior = Run
  then
    lock_printf "%% Comparisons finished, waiting for diffs to complete@.";
  lock();
  shared.cmp_finished <- true;
  unlock();
  ignore (Thread.create
    (fun () ->
      while true do
        Condition.broadcast shared.diff_available;
        Thread.delay 0.5;
      done)
    ());
  Thread.join diff_id;
  if !behavior = Run
  then
    lock_printf "%% Diffs finished. Summary:@\nRun = %d@\nOk  = %d of %d@."
      shared.summary_run shared.summary_ok shared.summary_log;
  xunit_report ();
  let error_code = 
    if !do_error_code && shared.summary_log <> shared.summary_ok
    then 1
    else 0
  in
  exit error_code

(*
Local Variables:
compile-command: "LC_ALL=C make -C .. ptests"
End:
*)
