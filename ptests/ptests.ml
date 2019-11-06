(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
let default_options = "-journal-disable -check"

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

let string_del_suffix suffix s =
  let lsuffix = String.length suffix in
  let ls = String.length s in
  if ls >= lsuffix && String.sub s (ls - lsuffix) lsuffix = suffix then
    Some (String.sub s 0 (ls - lsuffix))
  else None

let str_mutex = Mutex.create()

let str_global_replace regex repl s =
  Mutex.lock str_mutex;
  let res = Str.global_replace regex repl s in
  Mutex.unlock str_mutex; res

let str_string_match regex s n =
  Mutex.lock str_mutex;
  let res = Str.string_match regex s n in
  Mutex.unlock str_mutex; res

let str_split regex s =
  Mutex.lock str_mutex;
  let res = Str.split regex s in
  Mutex.unlock str_mutex; res

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
  try
    let v = Unix.getenv var in
    add_default_env (var ^ " (set from outside)") v 
  with Not_found -> add_env var value

(** the name of the directory-wide configuration file*)
let dir_config_file = "test_config"

(** the files in [suites] whose name matches
    the pattern [test_file_regexp] will be considered as test files *)
let test_file_regexp = ".*\\.\\(c\\|i\\)$"

(** the pattern that ends the parsing of options in a test file *)
let end_comment = Str.regexp ".*\\*/"

let regex_cmxs = Str.regexp ("\\([^/]+\\)[.]cmxs\\($\\|[ \t]\\)")

let opt_to_byte toplevel =
  match string_del_suffix "frama-c" toplevel with
  | Some path -> path ^ "frama-c.byte"
  | None ->
    match string_del_suffix "toplevel.opt" toplevel with
    | Some path -> path ^ "toplevel.byte"
    | None ->
      match string_del_suffix "frama-c-gui" toplevel with
      | Some path -> path ^ "frama-c-gui.byte"
      | None ->
        match string_del_suffix "viewer.opt" toplevel with
        | Some path -> path ^ "viewer.byte"
        | None -> toplevel

let opt_to_byte_options options =
  str_global_replace regex_cmxs "\\1.cmo\\2" options

let execnow_opt_to_byte cmd =
  let cmd = opt_to_byte cmd in
  opt_to_byte_options cmd


let output_unix_error (exn : exn) =
  match exn with
  | Unix.Unix_error (error, _function, arg) ->
    let message = Unix.error_message error in
    if arg = "" then
      Format.eprintf "%s@." message
    else 
      Format.eprintf "%s: %s@." arg message
  | _ -> assert false

let mv src dest =
  try
    Unix.rename src dest
  with Unix.Unix_error _ as e ->
    output_unix_error e

let unlink ?(silent = true) file =
  let open Unix in
  try
    Unix.unlink file
  with
  | Unix_error _ when silent -> ()
  | Unix_error (ENOENT,_,_) -> () (* Ignore "Not such file or directory" *)
  | Unix_error _ as e -> output_unix_error e

let is_file_empty_or_nonexisting filename =
  let open Unix in
  try
    (Unix.stat filename).st_size = 0
  with
  | Unix_error (UnixLabels.ENOENT, _, _) -> (* file does not exist *)
    true
  | Unix_error _ as e ->
    output_unix_error e;
    raise e

let base_path = Filename.current_dir_name
(*    (Filename.concat
        (Filename.dirname Sys.executable_name)
        Filename.parent_dir_name)
*)

(** Command-line flags *)

type behavior = Examine | Update | Run | Show | Gui
let behavior = ref Run
let verbosity = ref 0
let dry_run = ref false
let use_byte = ref false
let use_diff_as_cmp = ref (Sys.os_type = "Win32")
let do_diffs = ref (if Sys.os_type = "Win32" then "diff --strip-trailing-cr -u"
                    else "diff -u")
let do_cmp = ref (if Sys.os_type="Win32" then !do_diffs
                  else "cmp -s")
let do_make = ref "make"
let n = ref 4    (* the level of parallelism *)
let suites = ref []
(** options appended to toplevel for all tests *)
let additional_options = ref ""
(** options prepended to toplevel for all tests *)
let additional_options_pre = ref ""

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

(* Those variables are read from a ptests_config file *)
let default_suites = ref []
let toplevel_path = ref ""

let change_toplevel_to_gui () =
  let s = !toplevel_path in
  match string_del_suffix "toplevel.opt" s with
  | Some s -> toplevel_path := s ^ "viewer.opt"
  | None ->
    match string_del_suffix "toplevel.byte" s with
    | Some s -> toplevel_path := s ^ "viewer.byte"
    | None ->
      match string_del_suffix "frama-c" s with
      | Some s -> toplevel_path := s ^ "frama-c-gui"
      | None ->
        match string_del_suffix "frama-c.byte" s with
        | Some s -> toplevel_path := s ^ "frama-c-gui.byte"
        | None -> ()


let () =
  Unix.putenv "LC_ALL" "C" (* some oracles, especially in Jessie, depend on the
                              locale *)
let example_msg =
  Format.sprintf
    "@.@[<v 0>\
    A test suite can be the name of a directory in ./tests or \
    the path to a file.@ @ \
    @[<v 1>\
    Some variables can be used in test command:@ \
     @@PTEST_CONFIG@@    \
     # test configuration suffix@ \
     @@PTEST_FILE@@   \
     # substituted by the test filename@ \
     @@PTEST_DIR@@    \
     # dirname of the test file@ \
     @@PTEST_NAME@@   \
     # basename of the test file@ \
     @@PTEST_NUMBER@@ \
     # test command number@] @ \
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
  "-gui", Arg.Unit (fun () ->
      behavior := Gui;
      n := 1; (* Disable parallelism to see which GUI is launched *)
    ) ,
  " Start the tests in Frama-C's gui.";
  "-update", Arg.Unit (fun () -> behavior := Update) ,
  " Take the current logs as oracles.";
  "-show", Arg.Unit (fun () -> behavior := Show) ,
  " Show the results of the tests.";
  "-run", Arg.Unit (fun () -> behavior := Run) ,
  " (default) Delete logs, run tests, then examine logs different from \
  oracles.";
  "-v", Arg.Unit (fun () -> incr verbosity),
  " Increase verbosity (up to  twice)" ;
  "-dry-run", Arg.Unit (fun () -> dry_run := true),
  " Do not run commands (use with -v to print all commands which would be run)" ;
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
  " <name> Use special configuration and oracles";
  "-add-options", Arg.Set_string additional_options,
  "<options> Add additional options to be passed to the toplevels \
   that will be launched. <options> are added after standard test options";
  "-add-options-pre", Arg.Set_string additional_options_pre,
  "<options> Add additional options to be passed to the toplevels \
   that will be launched. <options> are added before standard test options.";
  "-add-options-post", Arg.Set_string additional_options,
  "Synonym of -add-options";
  "-exclude", Arg.String exclude,
  "<name> Exclude a test or a suite from the run";
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
          compare optname1 optname2
        ) argspec)
     ) @ ["", Arg.Unit (fun () -> ()), example_msg;])
    make_test_suite umsg
;;


let fail s =
  Format.printf "Error: %s@." s;
  exit 2

(** split the filename into before including "tests" dir and after including "tests" dir
    NOTA: both part contains "tests" (one as suffix the other as prefix).
 *)
let rec get_upper_test_dir initial dir =
  let tests = Filename.dirname dir in
  if tests = dir then
     (* root directory *)
    (fail (Printf.sprintf "Can't find a tests directory below %s" initial))
  else
    let base = Filename.basename dir in
  if base = "tests" then
    dir, "tests"
  else
    let tests, suffix = get_upper_test_dir initial tests in
    tests, Filename.concat suffix base

let rec get_test_path = function
  | [] ->
    if Sys.file_exists "tests" && Sys.is_directory "tests" then "tests", []
    else begin
      Format.eprintf "No test path found. Aborting@.";
      exit 1
    end
  | [f] -> let tests, suffix = get_upper_test_dir f f in
    tests, [suffix]
  | a::l ->
    let tests, l = get_test_path l in
    let a_tests, a = get_upper_test_dir a a in
    if a_tests <> tests
    then fail (Printf.sprintf "All the tests should be inside the same tests directory")
    else tests, a::l

let test_path =
  let files, names = List.partition Sys.file_exists !suites in
  let tests, l = get_test_path files in
  let names = List.map (Filename.concat tests) names in
  suites := names@l;
  Sys.chdir (Filename.dirname tests);
  "tests"

let parse_config_line =
  let regexp_blank = Str.regexp "[ ]+" in
  fun (key, value) ->
    match key with
    | "DEFAULT_SUITES" ->
      let l = Str.split regexp_blank value in
      default_suites := List.map (Filename.concat test_path) l
    | "TOPLEVEL_PATH" ->
      toplevel_path := value
    | _ -> default_env key value (* Environnement variable that Frama-C reads*)


(** parse config files *)
let () =
  let config = "tests/ptests_config" in
  if Sys.file_exists config then begin
    try
      (*Parse the plugin configuration file for tests. Format is 'Key=value' *)
      let ch = open_in config in
      let regexp = Str.regexp "\\([^=]+\\)=\\(.*\\)" in
      while true do
        let line = input_line ch in
        if Str.string_match regexp line 0 then
          let key = Str.matched_group 1 line in
          let value = Str.matched_group 2 line in
          parse_config_line (key, value)
        else begin
          Format.eprintf "Cannot interpret line '%s' in ptests_config@." line;
          exit 1
        end
      done
    with
    | End_of_file ->
      if !toplevel_path = "" then begin
        Format.eprintf "Missing TOPLEVEL_PATH variable. Aborting.@.";
        exit 1
      end
  end
  else begin
    Format.eprintf
      "Cannot find configuration file %s. Aborting.@." config;
    exit 1
  end

(** Must be done after reading config *)
let () = if !behavior = Gui then change_toplevel_to_gui ()

(* redefine name if special configuration expected *)
let redefine_name name =
  if !special_config = "" then name else
    name ^ "_" ^ !special_config

let dir_config_file = redefine_name dir_config_file

let gen_make_file s dir file =
  Filename.concat (Filename.concat dir s) file

module SubDir: sig
  type t

  val get: t -> string

  val create: ?with_subdir:bool -> string (** dirname *) -> t
  (** By default, creates the needed subdirectories if absent.
      Anyway, fails if the given dirname doesn't exists *)

  val make_oracle_file: t -> string -> string
  val make_result_file: t -> string -> string
  val make_file: t -> string -> string
end = struct
  type t = string

  let get s = s

  let create_if_absent dir =
    if not (Sys.file_exists dir)
    then Unix.mkdir dir 0o750 (** rwxr-w--- *)
    else if not (Sys.is_directory dir)
    then fail (Printf.sprintf "the file %s exists but is not a directory" dir)

  let oracle_dirname = redefine_name "oracle"
  let result_dirname = redefine_name "result"

  let make_result_file = gen_make_file result_dirname
  let make_oracle_file = gen_make_file oracle_dirname
  let make_file = Filename.concat

  let create ?(with_subdir=true) dir =
    if not (Sys.file_exists dir && Sys.is_directory dir)
    then fail (Printf.sprintf "the directory %s must be an existing directory" dir);
    if (with_subdir) then begin
      create_if_absent (Filename.concat dir result_dirname);
      create_if_absent (Filename.concat dir oracle_dirname)
    end;
    dir

end

type execnow =
    {
      ex_cmd: string;      (** command to launch *)
      ex_log: string list; (** log files *)
      ex_bin: string list; (** bin files *)
      ex_dir: SubDir.t;    (** directory of test suite *)
      ex_once: bool;       (** true iff the command has to be executed only once
                               per config file (otherwise it is executed for
                               every file of the test suite) *)
      ex_done: bool ref;   (** has the command been already fully executed.
                               Shared between all copies of this EXECNOW. Do
                               NOT use a mutable field here, as execnows
                               are duplicated using OCaml 'with' syntax. *)
    }


module Macros =
struct
  module StringMap = Map.Make (String)
  open StringMap

  type t = string StringMap.t

  let empty = StringMap.empty

  let macro_regex = Str.regexp "\\([^@]*\\)@\\([^@]*\\)@\\(.*\\)"

  let does_expand macros s =
    if !verbosity >=2 then begin
      lock_printf "looking for macros in string %s\n%!" s;
      lock_printf "Existing macros:\n%!";
      iter (fun s1 s2 -> lock_printf "%s => %s\n%!" s1 s2) macros;
      lock_printf "End macros\n%!";
    end;
    let rec aux n (ptest_file_matched,s as acc) =
      if Str.string_match macro_regex s n then begin
        let macro = Str.matched_group 2 s in
        let ptest_file_matched = ptest_file_matched || macro = "PTEST_FILE" in
        let start = Str.matched_group 1 s in
        let rest = Str.matched_group 3 s in
        let new_n = Str.group_end 1 in
        let n, new_s =
          if macro = "" then begin
            new_n + 1, String.sub s 0 new_n ^ "@" ^ rest
          end else begin
            try
              if !verbosity >= 2 then lock_printf "macro is %s\n%!" macro;
              let replacement =  find macro macros in
              if !verbosity >= 1 then
                lock_printf "replacement for %s is %s\n%!" macro replacement;
              new_n,
              String.sub s 0 n ^ start ^ replacement ^ rest
            with
            | Not_found -> Str.group_end 2 + 1, s
          end
        in
        if !verbosity >= 2 then lock_printf "new string is %s\n%!" new_s;
        let new_acc = ptest_file_matched, new_s in
        if n <= String.length new_s then aux n new_acc else new_acc
      end else acc
    in
    Mutex.lock str_mutex;
    try
      let res = aux 0 (false,s) in
      Mutex.unlock str_mutex; res
    with e ->
      lock_eprintf "Uncaught exception %s\n%!" (Printexc.to_string e);
      Mutex.unlock str_mutex;
      raise e

  let expand macros s =
    snd (does_expand macros s)

  let get ?(default="") name macros =
    try find name macros with Not_found -> default

  let add_list l map =
    List.fold_left (fun acc (k,v) -> add k v acc) map l

  let add_expand name def macros =
    add name (expand macros def) macros

  let append_expand name def macros =
    add name (get name macros ^ expand macros def) macros
end


(** configuration of a directory/test. *)
type config =
    {
      dc_test_regexp: string; (** regexp of test files. *)
      dc_execnow    : execnow list; (** command to be launched before
                                         the toplevel(s)
                                     *)
      dc_macros: Macros.t; (** existing macros. *)
      dc_default_toplevel   : string;
      (** full path of the default toplevel. *)
      dc_filter     : string option; (** optional filter to apply to
                              standard output *)
      dc_toplevels    : (string * string * string list * Macros.t) list;
      (** toplevel full path, options to launch the toplevel on, and list
       of output files to monitor beyond stdout and stderr. *)
      dc_dont_run   : bool;
      dc_default_log: string list;
    }

let default_macros () =
  let l = [
    "frama-c", !toplevel_path;
    "PTEST_MAKE_MODULE", "make -s"
  ] in
  Macros.add_list l Macros.empty

let default_config () =
  { dc_test_regexp = test_file_regexp ;
    dc_macros = default_macros ();
    dc_execnow = [];
    dc_filter = None ;
    dc_default_toplevel = !toplevel_path;
    dc_toplevels = [ !toplevel_path, default_options, [], Macros.empty ];
    dc_dont_run = false;
    dc_default_log = []
  }

let launch command_string =
  if !dry_run then 0 (* do not run command; return as if no error *)
  else
    let result = system command_string in
    match result with
    | Unix.WEXITED 127 ->
      lock_printf "%% Couldn't execute command. Retrying once.@.";
      Thread.delay 0.125;
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


let scan_execnow ~once dir (s:string) =
  let rec aux (s:execnow) =
    try
      Scanf.sscanf s.ex_cmd "%_[ ]LOG%_[ ]%[-A-Za-z0-9_',+=:.\\@@]%_[ ]%s@\n"
        (fun name cmd ->
          aux { s with ex_cmd = cmd; ex_log = name :: s.ex_log })
    with Scanf.Scan_failure _ ->
      try
        Scanf.sscanf s.ex_cmd "%_[ ]BIN%_[ ]%[A-Za-z0-9_.\\-@@]%_[ ]%s@\n"
          (fun name cmd ->
            aux { s with ex_cmd = cmd; ex_bin = name :: s.ex_bin })
      with Scanf.Scan_failure _ ->
        try
          Scanf.sscanf s.ex_cmd "%_[ ]make%_[ ]%s@\n"
            (fun cmd ->
              let s = aux ({ s with ex_cmd = cmd; }) in
              { s with ex_cmd = !do_make^" "^cmd; } )
        with Scanf.Scan_failure _ ->
          s
  in
  aux
    { ex_cmd = s;
      ex_log = [];
      ex_bin = [];
      ex_dir = dir;
      ex_once = once;
      ex_done = ref false }

(* the default toplevel for the current level of options. *)
let current_default_toplevel = ref !toplevel_path
let current_default_log = ref []
let current_default_cmds = ref [!toplevel_path,default_options,[], Macros.empty]

let make_custom_opts =
  let space = Str.regexp " " in
  fun stdopts s ->
    let rec aux opts s =
      try
	Scanf.sscanf s "%_[ ]%1[+#\\-]%_[ ]%S%_[ ]%s@\n"
          (fun c opt rem ->
	    match c with
            | "+" -> aux (opt :: opts) rem
            | "#" -> aux (opts @ [ opt ]) rem
            | "-" -> aux (List.filter (fun x -> x <> opt) opts) rem
            | _ -> assert false (* format of scanned string disallow it *))
      with
      | Scanf.Scan_failure _ ->
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
      aux (List.rev (str_split space stdopts)) s
    in
    (* preserve options ordering *)
    List.fold_right (fun x s -> s ^ " " ^ x) opts ""


(* how to process options *)
let config_exec ~once dir s current =
  { current with dc_execnow = scan_execnow ~once dir s :: current.dc_execnow }

let config_macro _dir s current =
  let regex = Str.regexp "[ \t]*\\([^ \t@]+\\)\\([ \t]+\\(.*\\)\\|$\\)" in
  Mutex.lock str_mutex;
  if Str.string_match regex s 0 then begin
    let name = Str.matched_group 1 s in
    let def = 
      try Str.matched_group 3 s with Not_found -> (* empty text *) ""
    in
    Mutex.unlock str_mutex;
    if !verbosity >= 1 then
      lock_printf "new macro %s with definition %s\n%!" name def;
    { current with dc_macros = Macros.add_expand name def current.dc_macros }
  end else begin
    Mutex.unlock str_mutex;
    lock_eprintf "cannot understand MACRO definition: %s\n%!" s;
    current
  end

let config_module dir s current =
  let make_cmd = "@PTEST_MAKE_MODULE@ " ^ s in
  let make_cmd = Macros.expand current.dc_macros make_cmd in
  let current = config_exec ~once:true dir make_cmd current in
  let k = "PTEST_LOAD_MODULES" and v = " -load-module " ^ s in
  { current with dc_macros = Macros.append_expand k v current.dc_macros }

let config_options =
  [ "CMD",
    (fun _ s current -> 
       { current with dc_default_toplevel = s});

    "OPT",
    (fun _ s current ->
       let t = current.dc_default_toplevel, s, current.dc_default_log, current.dc_macros in
       { current with
(*           dc_default_toplevel = !current_default_toplevel;*)
           dc_default_log = !current_default_log;
           dc_toplevels = t :: current.dc_toplevels });

    "STDOPT",
    (fun _ s current ->
       let new_top =
         List.map
           (fun (cmd,opts, log, macros) -> cmd, make_custom_opts opts s, log, current.dc_macros)
           !current_default_cmds
       in
       { current with dc_toplevels = new_top @ current.dc_toplevels;
                      dc_default_log = !current_default_log @
                                       current.dc_default_log });

    "FILEREG",
    (fun _ s current -> { current with dc_test_regexp = s });

    "FILTER",
    (fun _ s current -> { current with dc_filter = Some s });

    "GCC",
    (fun _ _ acc -> acc);

    "COMMENT",
    (fun _ _ acc -> acc);

    "DONTRUN",
    (fun _ s current -> { current with dc_dont_run = true });

    "EXECNOW", config_exec ~once:true;
    "EXEC", config_exec ~once:false;
    "MACRO", config_macro;
    "MODULE", config_module;
    "LOG",
    (fun _ s current ->
     { current with dc_default_log = s :: current.dc_default_log })

  ]

let scan_options dir scan_buffer default =
  let r =
    ref { default with dc_toplevels = [] }
  in
  current_default_toplevel := default.dc_default_toplevel;
  current_default_log := default.dc_default_log;
  current_default_cmds := List.rev default.dc_toplevels;
  let treat_line s =
    try
      Scanf.sscanf s "%[ *]%[A-Za-z0-9]: %s@\n"
        (fun _ name opt ->
          try
            r := (List.assoc name config_options) dir opt !r
          with Not_found ->
            lock_eprintf "@[unknown configuration option: %s@\n%!@]" name)
    with Scanf.Scan_failure _ ->
      if str_string_match end_comment s 0
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

let split_config = Str.regexp ",[ ]*"

let scan_test_file default dir f =
  let f = SubDir.make_file dir f in
  let exists_as_file =
    try
      (Unix.lstat f).Unix.st_kind = Unix.S_REG
    with Unix.Unix_error _ | Sys_error _ -> false
  in
    if exists_as_file then begin
        let scan_buffer = Scanf.Scanning.open_in f in
        let rec scan_config () =
          (* space in format string matches any number of whitespace *)
          Scanf.bscanf scan_buffer " /* %s@\n"
            (fun names ->
               let is_current_config name =
                 name = "run.config*" ||
                 name = "run.config" && !special_config = ""  ||
                 name = "run.config_" ^ !special_config
               in
               let configs = Str.split split_config (String.trim names) in
               if List.exists is_current_config configs then
                 (* Found options for current config! *)
                 scan_options dir scan_buffer default
               else (* config name does not match: eat config and continue.
                       But only if the comment is still opened by the end of
                       the line...
                    *)
                 (if not (str_string_match end_comment names 0) then
                    ignore (scan_options dir scan_buffer default);
                  scan_config ()))
        in
        try
          let options =  scan_config () in
          Scanf.Scanning.close_in scan_buffer;
          options
        with End_of_file | Scanf.Scan_failure _ -> 
          Scanf.Scanning.close_in scan_buffer;
          default
      end else
      (* if the file has disappeared, don't try to run it... *)
      { default with dc_dont_run = true }

type toplevel_command =
    { macros: Macros.t;
      mutable log_files: string list;
      file : string ;
      nb_files : int ;
      options : string ;
      toplevel: string ;
      filter : string option ;
      directory : SubDir.t ;
      n : int;
      execnow:bool
    }

type command =
  | Toplevel of toplevel_command
  | Target of execnow * command Queue.t

type log = Err | Res

type diff =
  | Command_error of toplevel_command * log
  | Target_error of execnow
  | Log_error of SubDir.t (** directory *) * string (** file *)

type cmps =
  | Cmp_Toplevel of toplevel_command
  | Cmp_Log of SubDir.t (** directory *) * string (** file *)

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
      mutable summary_time : float ;
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
    summary_time = (Unix.times()).Unix.tms_cutime ;
    summary_run = 0 ;
    summary_ok = 0 ;
    summary_log = 0 }

let unlock () = Mutex.unlock shared.lock

let lock () = Mutex.lock shared.lock

let catenate_number nb_files prefix n =
  if nb_files > 1
  then prefix ^ "." ^ (string_of_int n)
  else prefix

let name_without_extension command =
  try
    (Filename.chop_extension command.file)
  with
    Invalid_argument _ ->
      fail ("this test file does not have any extension: " ^
                   command.file)

let gen_prefix gen_file cmd =
  let prefix = gen_file cmd.directory (name_without_extension cmd) in
  catenate_number cmd.nb_files prefix cmd.n

let log_prefix = gen_prefix SubDir.make_result_file
let oracle_prefix = gen_prefix SubDir.make_oracle_file

let get_ptest_file cmd = SubDir.make_file cmd.directory cmd.file

let get_macros cmd =
  let ptest_config =
    if !special_config = "" then "" else "_" ^ !special_config
  in
  let ptest_file = get_ptest_file cmd in
  let ptest_name =
    try Filename.chop_extension cmd.file
    with Invalid_argument _ -> cmd.file
  in
  let macros =
    [ "PTEST_CONFIG", ptest_config;
      "PTEST_DIR", SubDir.get cmd.directory;
      "PTEST_RESULT",
      SubDir.get cmd.directory ^ "/" ^ redefine_name "result";
      "PTEST_FILE", ptest_file;
      "PTEST_NAME", ptest_name;
      "PTEST_NUMBER", string_of_int cmd.n;
    ]
  in
  Macros.add_list macros cmd.macros

let basic_command_string =
  let contains_toplevel_or_frama_c = 
    Str.regexp "[^( ]*\\(\\(toplevel\\)\\|\\(viewer\\)\\|\\(frama-c\\)\\).*"
  in
  fun command ->
    let macros = get_macros command in
    let logfiles = List.map (Macros.expand macros) command.log_files in
    command.log_files <- logfiles;
    let has_ptest_file_t, toplevel = Macros.does_expand macros command.toplevel in
    let has_ptest_file_o, options = Macros.does_expand macros command.options in
    let toplevel = if !use_byte then opt_to_byte toplevel else toplevel in
    let options =
      if str_string_match contains_toplevel_or_frama_c command.toplevel 0
      then begin
        let opt_modules = Macros.expand macros
            (Macros.get "PTEST_LOAD_MODULES" macros) in
        let opt_pre = Macros.expand macros !additional_options_pre in
        let opt_post = Macros.expand macros !additional_options in
        "-check " ^ opt_modules ^ " " ^ opt_pre ^ " " ^ options ^ " " ^ opt_post
      end else options
    in
    let options = if !use_byte then opt_to_byte_options options else options in
    if has_ptest_file_t || has_ptest_file_o || command.execnow then
      toplevel ^ " " ^ options
    else
      toplevel ^ " " ^ get_ptest_file command ^ " " ^ options

(* Searches for executable [s] in the directories contained in the PATH
   environment variable. Returns [None] if not found, or
   [Some <fullpath>] otherwise. *)
let find_in_path s =
  let trim_right s =
    let n = ref (String.length s - 1) in
    let last_char_to_keep =
      try
        while !n > 0 do
          if String.get s !n <> ' ' then raise Exit;
          n := !n - 1
        done;
        0
      with Exit -> !n
    in
    String.sub s 0 (last_char_to_keep+1)
  in
  let s = trim_right s in
  let path_separator = if Sys.os_type = "Win32" then ";" else ":" in
  let re_path_sep = Str.regexp path_separator in
  let path_dirs = Str.split re_path_sep (Sys.getenv "PATH") in
  let found = ref "" in
  try
    List.iter (fun dir ->
        let fullname = dir ^ Filename.dir_sep ^ s in
        if Sys.file_exists fullname then begin
          found := fullname;
          raise Exit
        end
      ) path_dirs;
    None
  with Exit ->
    Some !found

let command_string command =
  let log_prefix = log_prefix command in
  let errlog = log_prefix ^ ".err.log" in
  let stderr = match command.filter with
      None -> errlog
    | Some _ ->
        let stderr =
          Filename.temp_file (Filename.basename log_prefix) ".err.log"
        in
        at_exit (fun () ->  unlink stderr);
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
            match find_in_path exec_name with
            | Some full_exec_name -> full_exec_name
            | None ->
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
        Printf.sprintf "%s && %s < %s >%s && rm -f %s"
          command_string filter stderr errlog stderr
  in
  command_string

let update_log_files dir file =
  mv (SubDir.make_result_file dir file) (SubDir.make_oracle_file dir file)

let update_toplevel_command command =

  let log_prefix = log_prefix command in
  let oracle_prefix = oracle_prefix command in
  (* Update oracle *)
  mv (log_prefix ^ ".res.log") (oracle_prefix ^ ".res.oracle");
  (* Is there an error log ? *)
  begin try
    let log = log_prefix ^ ".err.log"
    and oracle = oracle_prefix ^ ".err.oracle"
    in
    if is_file_empty_or_nonexisting log then
      (* No, remove the error oracle *)
      unlink ~silent:false oracle
    else
      (* Yes, update the error oracle*)
      mv log oracle
  with (* Possible error in [is_file_empty] *)
    Unix.Unix_error _ -> ()
  end;
  let macros = get_macros command in
  let log_files = List.map (Macros.expand macros) command.log_files
  in
  List.iter (update_log_files command.directory) log_files

let rec update_command = function
    Toplevel cmd -> update_toplevel_command cmd
  | Target (execnow,cmds) ->
      List.iter (update_log_files execnow.ex_dir) execnow.ex_log;
      Queue.iter update_command cmds

let remove_execnow_results execnow =
  List.iter
    (fun f -> unlink (SubDir.make_result_file execnow.ex_dir f))
    (execnow.ex_bin @ execnow.ex_log)

module Make_Report(M:sig type t end)=struct
  module H=Hashtbl.Make
    (struct
      type t = toplevel_command
      let project cmd = (cmd.directory,cmd.file,cmd.n)
      let compare c1 c2 = compare (project c1) (project c2)
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
module Report_run=Make_Report(struct type t=int*float
(* At some point will contain the running time*)
end)

let report_run cmp r = Report_run.record cmp r
module Report_cmp=Make_Report(struct type t=int*int end)
let report_cmp = Report_cmp.record
let pretty_report fmt =
  Report_run.iter
    (fun test (_run_result,time_result) ->
      Format.fprintf fmt
        "<testcase classname=%S name=%S time=\"%f\">%s</testcase>@."
        (Filename.basename (SubDir.get test.directory)) test.file time_result
        (let res,err = Report_cmp.find test in
         Report_cmp.remove test;
         (if res=0 && err=0 then "" else
             Format.sprintf "<failure type=\"Regression\">%s</failure>"
               (if res=1 then "Stdout oracle difference"
                else if res=2 then "Stdout System Error (missing oracle?)"
                else if err=1 then "Stderr oracle difference"
                else if err=2 then "Stderr System Error (missing oracle?)"
                else "Unexpected errror"))));
  (* Test that were compared but not runned *)
  Report_cmp.iter
    (fun test (res,err) ->
      Format.fprintf fmt
        "<testcase classname=%S name=%S>%s</testcase>@."
        (Filename.basename (SubDir.get test.directory)) test.file
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
      @\n<testsuite errors=\"0\" failures=\"%d\" name=\"%s\" tests=\"%d\" time=\"%f\" timestamp=\"%f\">\
      @\n%t</testsuite>@."
      (shared.summary_log-shared.summary_ok)
      "Frama-C"
      shared.summary_log
      ((Unix.times()).Unix.tms_cutime -. shared.summary_time)
      (Unix.gettimeofday ())
      pretty_report;
    close_out out;
  end


let do_command command =
  match command with
  | Toplevel command ->
      (* Update : copy the logs. Do not enqueue any cmp
         Run | Show: launch the command, then enqueue the cmp
         Gui: launch the command in the gui
         Examine : just enqueue the cmp *)
      if !behavior = Update
      then update_toplevel_command command
      else begin
        (* Run, Show, Gui or Examine *)
        if !behavior = Gui then begin
          (* basic_command_string does not redirect the outputs, and does
             not overwrite the result files *)
          let basic_command_string = basic_command_string command in
          lock_printf "%% launch %s@." basic_command_string ;
          ignore (launch basic_command_string)
        end
        else begin
          (* command string also replaces macros in logfiles names, which
             is useful for Examine as well. *)
          let command_string = command_string command in
          if !behavior <> Examine
          then begin
            if !verbosity >= 1
            then lock_printf "%% launch %s@." command_string ;
            let launch_result = launch command_string in
            let time = 0. (* Individual time is difficult to compute correctly
                             for now, and currently unused *) in
            report_run command (launch_result, time)
          end;
          lock ();
          shared.summary_run <- succ shared.summary_run ;
          Queue.push (Cmp_Toplevel command) shared.cmps;
          List.iter
            (fun f -> Queue.push (Cmp_Log (command.directory, f)) shared.cmps)
            command.log_files;
          unlock ()
        end
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
                  unlink (log_prefix ^ ".res.log ")
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
          if !behavior <> Examine && not (!(execnow.ex_done) && execnow.ex_once)
            then begin
              remove_execnow_results execnow;
              let cmd =
                if !use_byte then
                  execnow_opt_to_byte execnow.ex_cmd
                else
                  execnow.ex_cmd
              in
              if !verbosity >= 1 then begin
                lock_printf "%% launch %s@." cmd;
              end;
              let r = launch cmd in
              (* mark as already executed. For EXECNOW in test_config files,
                 other instances (for example another test of the same
                 directory), won't relaunch the command. For EXECNOW in
                 stand-alone tests, there is only one copy of the EXECNOW
                 anyway *)
              execnow.ex_done := true;
              continue r
            end
            else
              continue 0
        end

let log_ext = function Res -> ".res" | Err -> ".err"

let launch_and_check_compare_file diff ~cmp_string ~log_file ~oracle_file =
  lock();
  shared.summary_log <- shared.summary_log + 1;
  unlock();
  let res = launch cmp_string in
  begin
    match res with
      0 ->
      lock();
      shared.summary_ok <- shared.summary_ok + 1;
      unlock()
    | 1 ->
      lock();
      Queue.push diff shared.diffs;
      Condition.signal shared.diff_available;
      unlock()
    | 2 ->
      lock_printf
        "%% System error while comparing. Maybe one of the files is missing...@\n%s or %s@."
        log_file oracle_file
    | n ->
      lock_printf
        "%% Comparison function exited with code %d for files %s and %s. \
         Allowed exit codes are 0 (no diff), 1 (diff found) and \
         2 (system error). This is a fatal error.@." n log_file oracle_file;
      exit 2
  end;
  res

let check_file_is_empty_or_nonexisting diff ~log_file =
  if is_file_empty_or_nonexisting log_file then
    0
  else begin
    lock();
    Queue.push diff shared.diffs;
    Condition.signal shared.diff_available;
    unlock();
    1
  end

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
    let log_file = log_prefix ^ ext ^ ".log" in
    let oracle_file = oracle_prefix ^ ext ^ ".oracle" in
    if log_kind = Err && not (Sys.file_exists oracle_file) then
      check_file_is_empty_or_nonexisting (Command_error (cmp,log_kind)) ~log_file
    else begin
      let cmp_string =
        !do_cmp ^ " " ^ log_file ^ " " ^ oracle_file ^ " > /dev/null 2> /dev/null"
      in
      if !verbosity >= 2 then lock_printf "%% cmp%s (%d) :%s@."
          ext
          cmp.n
          cmp_string;
      launch_and_check_compare_file (Command_error (cmp,log_kind))
        ~cmp_string ~log_file ~oracle_file
    end

let compare_one_log_file dir file =
  if !behavior = Show
  then begin
    lock();
    Queue.push (Log_error(dir,file)) shared.diffs;
    Condition.signal shared.diff_available;
    unlock()
  end else
    let log_file = SubDir.make_result_file dir file in
    let oracle_file = SubDir.make_oracle_file dir file in
    let cmp_string = !do_cmp ^ " " ^ log_file ^ " " ^ oracle_file ^ " > /dev/null 2> /dev/null" in
    if !verbosity >= 2 then lock_printf "%% cmplog: %s / %s@." (SubDir.get dir) file;
    ignore (launch_and_check_compare_file (Log_error (dir,file))
              ~cmp_string ~log_file ~oracle_file)

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
      let result_file = SubDir.make_result_file dir file in
      lock_printf "Log of %s:@." result_file;
      if !behavior = Show
      then ignore (launch ("cat " ^ result_file))
      else
        let diff_string =
          !do_diffs ^ " " ^ SubDir.make_oracle_file dir file ^ " " ^ result_file
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
  fun file -> str_string_match regexp file 0

let files = Queue.create ()

(* test for a possible toplevel configuration. *)
let default_config () =
  let general_config_file = Filename.concat test_path dir_config_file in
  if Sys.file_exists general_config_file
  then begin
    let scan_buffer = Scanf.Scanning.from_file general_config_file in
    scan_options (SubDir.create ~with_subdir:false Filename.current_dir_name) scan_buffer (default_config ())
  end
  else default_config ()

let () =
  (* enqueue the test files *)
  let default_suites () =
    let priority = "tests/idct" in
    let default = !default_suites in
    if List.mem priority default
    then priority :: (List.filter (fun name -> name <> priority) default)
    else default
  in
  let suites =
    match !suites with
    | [] -> default_suites ()
    | l ->
      List.fold_left (fun acc x ->
          if x = "tests"
          then (default_suites ()) @ acc
          else x::acc
        ) [] l
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
      (* the "suite" may be a directory or a single file *)
       let interpret_as_file = interpret_as_file suite in
       let directory =
         SubDir.create (if interpret_as_file
                        then
                          Filename.dirname suite
                        else
                          suite)
       in
       let config = SubDir.make_file directory dir_config_file in
       let dir_config =
         if Sys.file_exists config
         then begin
           let scan_buffer = Scanf.Scanning.from_file config in
           scan_options directory scan_buffer (default_config ())
         end
         else default_config ()
       in
       if interpret_as_file
       then begin
         if not (List.mem suite exclude_file) then
           Queue.push (Filename.basename suite, directory, dir_config) files
       end
       else begin
         if not (List.mem suite exclude_suite) then begin
           let dir_files = Sys.readdir (SubDir.get directory) in
           for i = 0 to pred (Array.length dir_files) do
             let file = dir_files.(i) in
             assert (Filename.is_relative file);
             if test_pattern dir_config file &&
               (not (List.mem (SubDir.make_file directory file) exclude_file))
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
      let e = ref 0 in
      let nb_files = List.length config.dc_toplevels in
      let make_toplevel_cmd (toplevel, options, log_files, macros) =
        let n = !i in
        {file; options; toplevel; nb_files; directory; n; log_files;
         filter = config.dc_filter; macros;
         execnow=false;
        }
      in
      let mk_cmd s =
          {
            file = file;
            nb_files = nb_files;
            log_files = [];
            options = "";
            toplevel = s;
            n = !e;
            directory = directory;
            filter = config.dc_filter;
            macros = config.dc_macros;
            execnow = true;
          }
      in
      let process_macros_cmd s = basic_command_string (mk_cmd s) in
      let macros = get_macros (mk_cmd "/bin/true") in
      let process_macros s = Macros.expand macros s in
      let make_execnow_cmd execnow =
        let res =
          {
            ex_cmd = process_macros_cmd execnow.ex_cmd;
            ex_log = List.map process_macros execnow.ex_log;
            ex_bin = List.map process_macros execnow.ex_bin;
            ex_dir = execnow.ex_dir;
            ex_once = execnow.ex_once;
            ex_done = execnow.ex_done;
          }
        in
        incr e; res
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
                    Target(make_execnow_cmd execnow,subworkqueue))
                 (Target(make_execnow_cmd hd,subworkqueue)) tl
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
    lock_printf "%% Diffs finished. Summary:@\nRun = %d@\nOk   = %d of %d@\nTime = %f s.@."
      shared.summary_run shared.summary_ok shared.summary_log
      ((Unix.times()).Unix.tms_cutime -. shared.summary_time);
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
