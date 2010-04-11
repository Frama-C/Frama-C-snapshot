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

let debug_level_ref = ref 0
let verbose_level_ref = ref 1
let kernel_debug_level_ref = ref 0
let kernel_verbose_level_ref = ref 1
let kernel_debug_atleast_ref = ref (fun n -> !kernel_debug_level_ref >= n)
let kernel_verbose_atleast_ref = ref (fun n -> !kernel_verbose_level_ref >= n)
let quiet_ref = ref false
let journal_enable_ref = ref !Config.is_gui
let journal_isset_ref = ref false
let journal_name_ref = ref "frama_c_journal"
let use_obj_ref = ref true
let use_type_ref = ref true

module L =
  Log.Register
    (struct
       let channel = Log.kernel_channel_name
       let label = Log.kernel_label_name
       let verbose_atleast n = !verbose_level_ref >= n
       let debug_atleast n = !debug_level_ref >= n
     end)
include L

(* ************************************************************************* *)
(** {2 Handling errors} *)
(* ************************************************************************* *)

let long_plugin_name s =
  if s = Log.kernel_label_name then "Frama-C" else "Plug-in " ^ s

let additional_info () =
  if !Config.is_gui then
    "\nLook at the console for additional information (if any)."
  else
    ""

let get_backtrace () =
  try "The full backtrace is:\n" ^ Printexc_common_interface.get_backtrace ()
  with Printexc_common_interface.No_backtrace ->
    "No backtrace available (ocaml version is lower than 3.11.0)"

let protect = function
  | Sys.Break -> "User Interruption (Ctrl-C)"
  | Sys_error s ->
      Printf.sprintf "%s\nSystem error: %s"
	(get_backtrace ())
	s
  | Unix.Unix_error(err, a, b) ->
      let error =
	Printf.sprintf "%s\nSystem error: %s"
	  (get_backtrace ()) (Unix.error_message err)
      in
      (match a, b with
	 | "", "" -> error
	 | "", t | t, "" -> Printf.sprintf "%s (%s)" error t
	 | f, x -> Printf.sprintf "%s (%s %S)" error f x)
  | Log.AbortError p ->
      Printf.sprintf "%s aborted because of an invalid user input.%s"
	(long_plugin_name p) (additional_info ())
  | Log.AbortFatal p ->
      Printf.sprintf
	"%s\n%s aborted because of an internal error.%s\n\
         Please report as 'crash' at http://bts.frama-c.com"
	(get_backtrace ()) (long_plugin_name p) (additional_info ())
  | Log.FeatureRequest(p, m) ->
      let name = long_plugin_name p in
      Printf.sprintf
	"%s aborted because of a not-yet implemented feature.%s\n\
         Please send a feature request at http://bts.frama-c.com with:\n\
         '[%s] %s'."
	name (additional_info ()) name m
  | Extlib.NotYetImplemented m ->
      Printf.sprintf
	"Computation aborted because of a not-yet implemented feature.\n\
         Please send a feature request at http://bts.frama-c.com with:\n\
         '%s'."
	m
  | e ->
      Printf.sprintf
	"%s\nUnexpected error (%s).\n\
         Please report as 'crash' at http://bts.frama-c.com"
	(get_backtrace ()) (Printexc.to_string e)

(* ************************************************************************* *)
(** {2 Exiting Frama-C} *)
(* ************************************************************************* *)

module NormalExit = Hook.Make(struct end)
let at_normal_exit = NormalExit.extend
let run_normal_exit_hook = NormalExit.apply

module ErrorExit = Hook.Make(struct end)
let at_error_exit = ErrorExit.extend
let run_error_exit_hook = ErrorExit.apply
let error_occured_ref = ref false
let error_occured () = error_occured_ref := true

type exit = unit
exception Exit
let nop = ()

let catch_at_toplevel = function
  | Log.AbortError _ -> true
  | Log.FeatureRequest _ -> true
  | _ -> !kernel_debug_level_ref = 0

let exit_code = function
  | Log.AbortError _ -> 1
  | Sys.Break -> 2
  | Log.FeatureRequest _ | Extlib.NotYetImplemented _ -> 3
  | Log.AbortFatal _ -> 4
  | _ -> 5

let bail_out_ref = ref (fun _ -> assert false)
let bail_out () =
  !bail_out_ref (); (* bail_out_ref must call exit 0 *)
  assert false

let catch_toplevel_run ~f ~quit ~at_normal_exit ~on_error =
  let bail_out () =
    (if !error_occured_ref then on_error else at_normal_exit) ();
    (* even if an error occured somewhere, Frama-C stops normally. *)
    exit 0
  in
  bail_out_ref := bail_out;
  try
    f ();
    (if quit then bail_out else at_normal_exit) ()
  with
    | Exit ->
	bail_out ()
    | exn when catch_at_toplevel exn ->
	L.feedback ~level:0 "%s" (protect exn);
	on_error ();
	exit (exit_code exn)
    | exn ->
	on_error ();
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
  L.abort
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
  with Not_found -> option,arg,false

let parse known_options_list options_list =
  let known_options = Hashtbl.create 17 in
  List.iter (fun (n, s) -> Hashtbl.add known_options n s) known_options_list;
  let parse_one_option unknown_options option arg =
    let option, arg, explicit = get_option_and_arg option arg in
    let check_string_argname () =
      if not explicit && ((String.length arg = 0) || (arg.[0] = '-')) then
	raise_error option "requires a string as argument";
    in
    try
      let setting = Hashtbl.find known_options option in
      let use_arg = match setting with
	| Unit f ->
	    if explicit then
	      raise_error option "does not accept any argument";
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
    | [] -> unknown_options, nb_used
    | [ option ] ->
	let unknown, use_arg, is_used =
	  parse_one_option unknown_options option ""
	in
	assert (not use_arg);
	unknown, if is_used then succ nb_used else nb_used
    | option :: (arg :: next_options as arg_next) ->
	let unknown, use_arg, is_used =
	  parse_one_option unknown_options option arg
	in
	let next = if use_arg then next_options else arg_next in
	go unknown (if is_used then succ nb_used else nb_used) next
  in
  try
    let unknown_options, nb_used = go [] 0 options_list in
    List.rev unknown_options, nb_used
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
    fst
      (parse
	   [ "-journal-enable", Unit (fun () -> set_journal true);
	     "-journal-disable", Unit (fun () -> set_journal false);
	     "-journal-name", String (fun s -> journal_name_ref := s);
	     "-no-obj", Unit (fun () -> use_obj_ref := false);
	     "-no-type", Unit (fun () -> use_type_ref := false);
	     "-quiet",
	     Unit (fun () ->
		     quiet_ref := true;
		     verbose_level_ref := 0;
		     debug_level_ref := 0);
	     "-verbose", Int (fun n -> verbose_level_ref := n);
	     "-debug", Int (fun n -> debug_level_ref := n);
	     "-kernel-verbose", Int (fun n -> kernel_verbose_level_ref := n);
	     "-kernel-debug", Int (fun n -> kernel_debug_level_ref := n)
	   ]
	     all_options)
  in
  catch_toplevel_run
    ~f:(fun () ->
	  let remaining_options = first_parsing_stage () in
	  non_initial_options_ref := remaining_options)
    ~quit:false
    ~at_normal_exit:(fun () -> ())
    ~on_error:run_error_exit_hook

let non_initial_options = !non_initial_options_ref

let () =
  if not !use_obj_ref then use_type_ref := false;
  if not !use_type_ref then begin
    Type.no_obj ();
    if !journal_enable_ref then begin
      warning "disabling journal in the 'no obj' mode";
      journal_enable_ref := false
    end
  end

let quiet = !quiet_ref
let debug_level = !debug_level_ref
let verbose_level = !verbose_level_ref

let kernel_debug_level = !kernel_debug_level_ref
let kernel_verbose_level = !kernel_verbose_level_ref

let journal_enable = !journal_enable_ref
let journal_isset = !journal_isset_ref
let journal_name = !journal_name_ref
let use_obj = !use_obj_ref
let use_type = !use_type_ref

(* ************************************************************************* *)
(** {2 Plugin} *)
(* ************************************************************************* *)

type cmdline_option =
    { oname: string; argname: string;
      odescr: string option; ext_descr: (unit,Format.formatter,unit) format;
      setting: option_setting }

module Plugin: sig
  type t = private
      { name: string;
	descr: string;
	short: string;
	groups: (string, cmdline_option list ref) Hashtbl.t }
  val all_plugins: unit -> t list
  val add: ?short:string -> string -> descr:string -> unit
  val add_group: plugin:string -> string -> string
  val add_option: string -> group:string -> cmdline_option -> unit
  val find: string -> t
  val find_option_aliases: cmdline_option -> cmdline_option list
  val is_option_alias: cmdline_option -> bool
end = struct

  type t =
    { name: string;
      descr: string;
      short: string;
      groups: (string, cmdline_option list ref) Hashtbl.t }

  (* all the registered plug-ins indexed by their shortnames *)
  let plugins : (string, t) Hashtbl.t = Hashtbl.create 17

  let all_plugins () = Hashtbl.fold (fun _ p acc -> p :: acc) plugins []

  let add ?short name ~descr =
    let short = match short with None -> name | Some s -> s in
    if Hashtbl.mem plugins short then
      invalid_arg ("a plug-in shortname " ^ short ^ " is already registered.");
    let groups = Hashtbl.create 7 in
    Hashtbl.add groups "" (ref []);
    Hashtbl.add
      plugins
      short
      { name = name; short = short; descr = descr; groups = groups }

  let find p =
    try Hashtbl.find plugins p
    with Not_found -> fatal "Plug-in %s not found" p

  let add_group ~plugin name =
    let groups = (find plugin).groups in
    if Hashtbl.mem groups name then
      abort "A group of name %s already exists for plug-in %s" name plugin;
    Hashtbl.add groups name (ref []);
    name

  let find_group p g =
    try Hashtbl.find (find p).groups g
    with Not_found -> fatal "Group %s not found for plug-in %s" g p

  module Alias_By_Option =
    Hashtbl.Make
      (struct
	 (* ordered by description: if two options have the same description,
	    they are aliased *)
	 type t = cmdline_option
	 let hash o = Hashtbl.hash o.odescr
	 let equal o1 o2 = match o1.odescr, o2.odescr with
	   | None, _ | _, None -> false (* no given description *)
	   | Some s1, Some s2 -> s1 = s2
       end)

  let alias_by_option = Alias_By_Option.create 7

  module All_Options =
    Hashtbl.Make
      (struct
	 type t = cmdline_option
	 let hash o = Hashtbl.hash o.oname
	 let equal o1 o2 = o1.oname = o2.oname
       end)

  let all_options = All_Options.create 7

  let add_option shortname ~group option =
    assert (option.oname <> "");
    let g = find_group shortname group in
    g := option :: !g;
    if All_Options.mem all_options option then
      invalid_arg
	("an option with the name \"" ^ option.oname
	 ^ "\" is already registered.");
    try
      let l = Alias_By_Option.find alias_by_option option in
      l := option :: !l;
      All_Options.add all_options option true
    with Not_found ->
      Alias_By_Option.add alias_by_option option (ref []);
      All_Options.add all_options option false

  let find_option_aliases o =
    try !(Alias_By_Option.find alias_by_option o)
    with Not_found -> []

  let is_option_alias o =
    try All_Options.find all_options o
    with Not_found -> assert false

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

module Make_Stage(S: sig val exclusive: bool val name: string end) = struct

  let nb_actions = ref 0
  let is_going_to_run () = incr nb_actions

  module H = Hook.Make(struct end)

  let options  : (string, cmdline_option) Hashtbl.t = Hashtbl.create 17

  let add name plugin ?(argname="") descr ext_descr setting =
    L.debug ~level:4 "Cmdline: [%s] registers %S for stage %s"
      plugin name S.name;
    let o =
      { oname = name; argname = argname;
	odescr = descr; ext_descr = ext_descr; setting = setting }
    in
    Hashtbl.add options name o;
    Plugin.add_option plugin o

  let parse options_list =
    L.debug ~level:3 "Parsing stage %s" S.name ;
    let options, nb_used =
      parse
	(Hashtbl.fold (fun _ o acc -> (o.oname, o.setting) :: acc) options [])
 	options_list
    in
    let nb_used = nb_used + !nb_actions in
    if S.exclusive && nb_used > 1 then begin
      L.abort "at most one %s action must be specified." S.name;
    end;
    H.apply ();
    options, nb_used

end

module Early_Stage =
  Make_Stage(struct let exclusive = false let name = "early" end)

module Extending_Stage =
  Make_Stage(struct let exclusive = false let name = "extending" end)

module Extended_Stage =
  Make_Stage(struct let exclusive = false let name = "extended" end)

module Exiting_Stage =
  Make_Stage(struct let exclusive = true let name = "exiting" end)

module Loading_Stage =
  Make_Stage(struct let exclusive = true let name = "loading" end)

let is_going_to_load = Loading_Stage.is_going_to_run

module Configuring_Stage =
  Make_Stage(struct let exclusive = false let name = "configuring" end)

let run_after_early_stage = Early_Stage.H.extend
let run_during_extending_stage = Extending_Stage.H.extend
let run_after_extended_stage = Extended_Stage.H.extend
let run_after_exiting_stage = Exiting_Stage.H.extend
let run_after_loading_stage = Loading_Stage.H.extend
let run_after_configuring_stage = Configuring_Stage.H.extend

type stage = Early | Extending | Extended | Exiting | Loading | Configuring

(* prefix is not used yet *)
let add_option
    ?(prefix=true) name ~plugin ~group stage ?argname ~descr ~ext_descr setting
    =
  if name <> "" then
    let name = match prefix, plugin with
      | true, "" | false, _ -> name
      | true, _p -> (*p ^*) name
    in
    let add = match stage with
      | Early -> Early_Stage.add
      | Extending -> Extending_Stage.add
      | Extended -> Extended_Stage.add
      | Exiting -> Exiting_Stage.add
      | Loading -> Loading_Stage.add
      | Configuring -> Configuring_Stage.add
    in
    add name plugin ~group ?argname descr ext_descr setting

module On_Files = Hook.Build(struct type t = string list end)
let use_cmdline_files = On_Files.extend

let set_files used_loading l =
  List.iter (fun s -> if s.[0] = '-' then error s "is unknown") l;
  if On_Files.is_empty () then
    (* should never occur *)
    fatal "no function uses the files provided on the command line";
  if List.length l > 0 then
    if used_loading then
      warning
	"ignoring source files specified on the command line \
while loading a global initial context."
    else
      On_Files.apply l

let nb_used_ref = ref 0
let nb_used_relevant = ref false
let nb_given_options () =
  if not !nb_used_relevant then
    fatal "function `nb_given_options' called too early";
  !nb_used_ref

let parse_and_boot get_toplevel play =
  let options, nb_used_early = Early_Stage.parse non_initial_options in
  let options, nb_used_extending = Extending_Stage.parse options in
  get_toplevel
    ()
    (* the extending stage may change the toplevel: applying [get_toplevel]
       provides the good one. *)
    (fun () ->
       let options, nb_used_extended = Extended_Stage.parse options in
       let options, nb_used_exiting = Exiting_Stage.parse options in
       if nb_used_exiting > 0 then
         fatal "setting an option at the exiting stage must stop Frama-C";
       let options, nb_used_loading = Loading_Stage.parse options in
       let files, nb_used_config = Configuring_Stage.parse options in
       nb_used_relevant := true;
       nb_used_ref :=
         nb_used_early
       + nb_used_extending
       + nb_used_extended
       + nb_used_exiting
       + nb_used_loading
       + nb_used_config ;
       set_files (nb_used_loading > 0) files;
       play ())

(* ************************************************************************* *)
(** {2 Help}

    Implement a not very efficient algorithm but it is enough for displaying
    help and exiting. *)
(* ************************************************************************* *)

let print_helpline fmt head descr ext_descr =
  match descr with
  | None -> ()
  | Some descr ->
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
	   cut_newline fmt (Str.split (Str.regexp_string "\n") descr))
	(* the extended description *)
	(fun fmt -> Format.fprintf fmt ext_descr)

let print_option_help fmt o =
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
    print_helpline fmt (name ^ ty) o.odescr o.ext_descr;
    List.iter
      (fun o ->
	 print_helpline fmt
	   (o.oname ^ ty)
	   (Some (" alias for option " ^ name))
	   o.ext_descr)
      (Plugin.find_option_aliases o);
    true

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
have an opposite with the name '-no-option-name'."
    first

let plugin_help shortname =
  let p = Plugin.find shortname in
  if p.Plugin.name <> "" then begin
    assert (p.Plugin.short <> "");
    Log.print_on_output "@[%s:@ %s@]@\n@[%s:@ %s@]@\n"
      "Plug-in name"
      p.Plugin.name
      "Plug-in shortname"
      shortname
  end;
  Log.print_on_output
    "@[@[%s:@ %s@]@\n@\n%s@\n@\n%s:@\n@\n@[%t@]@]@?"
    "Description"
    p.Plugin.descr
    (option_intro shortname)
    "***** LIST OF AVAILABLE OPTIONS"
    (fun fmt ->
       let print_options l =
	 List.fold_left
	   (fun b o ->
	      let b' = print_option_help fmt o in
	      b || b')
	   false
	   (List.sort (fun o1 o2 -> Pervasives.compare o1.oname o2.oname) l)
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
       | (s, o) :: l ->
	   Format.fprintf fmt "@[*** %s@]@\n@\n" (String.uppercase s);
	   ignore (print_options !o);
	   List.iter
	     (fun (s, l) ->
		Format.fprintf fmt "@\n@[*** %s@]@\n@\n" (String.uppercase s);
		ignore (print_options !l))
	     l);
  raise Exit

let help () =
  let iter_on_plugins f =
    List.iter
      (fun p -> if p.Plugin.name <> "" then f p)
      (List.sort
	 (fun p1 p2 -> Pervasives.compare p1.Plugin.name p2.Plugin.name)
	 (Plugin.all_plugins ()))
  in
  Log.print_on_output
    "@[%t@\n%t@\n@\n%s@\n@\n@[%t@]@]@?"
    (fun fmt ->
       Format.fprintf fmt "@[Usage: %s [options and files...]@]" Sys.argv.(0))
    (fun fmt ->
       Format.fprintf fmt
	 "@[`%s -kernel-help' provides a description of the general options of frama-c@]"
	 Sys.argv.(0))
    "***** LIST OF AVAILABLE PLUG-INS"
    (fun fmt ->
       iter_on_plugins
	 (fun p ->
	    print_helpline
	      fmt
	      p.Plugin.name
	      (Some
		 (p.Plugin.descr
		  ^ ";\n use -" ^ p.Plugin.short
		  ^ "-help for specific options."))
	      ""));
  raise Exit

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
