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

let empty_string = ""

let positive_debug_ref = ref 0
let session_is_set_ref = Extlib.mk_fun "session_is_set_ref"
let session_ref = Extlib.mk_fun "session_ref"
let config_is_set_ref = Extlib.mk_fun "config_is_set_ref"
let config_ref = Extlib.mk_fun "config_ref"

(* ************************************************************************* *)
(** {2 Signatures} *)
(* ************************************************************************* *)

module type S = sig
  include Log.Messages
  val add_group: ?memo:bool -> string -> Cmdline.Group.t
  module Help: Parameter_sig.Bool
  module Verbose: Parameter_sig.Int
  module Debug: Parameter_sig.Int
  module Debug_category: Parameter_sig.String_set
  module Share: Parameter_sig.Specific_dir
  module Session: Parameter_sig.Specific_dir
  module Config: Parameter_sig.Specific_dir
  val help: Cmdline.Group.t
  val messages: Cmdline.Group.t
end

module type General_services = sig
  include S
  include Parameter_sig.Builder
end

(* ************************************************************************* *)
(** {2 Optional parameters of functors} *)
(* ************************************************************************* *)

let kernel = ref false
let kernel_ongoing = ref false

let register_kernel =
  let used = ref false in
  fun () ->
    if !used then
      invalid_arg "The Frama-C kernel should be registered only once."
    else begin
      kernel := true;
      used := true
    end

let is_kernel () = !kernel

let share_visible_ref = ref false
let is_share_visible () = share_visible_ref := true

let session_visible_ref = ref false
let is_session_visible () = session_visible_ref := true

let config_visible_ref = ref false
let is_config_visible () = config_visible_ref := true

let plugin_subpath_ref = ref None
let plugin_subpath s = plugin_subpath_ref := Some s

let reset_plugin () =
  kernel := false;
  share_visible_ref := false;
  session_visible_ref := false;
  config_visible_ref := false;
  plugin_subpath_ref := None

(* ************************************************************************* *)
(** {2 Generic functors} *)
(* ************************************************************************* *)

let kernel_name = "kernel"

type plugin =
    { p_name: string;
      p_shortname: string;
      p_help: string;
      p_parameters: (string, Typed_parameter.t list) Hashtbl.t }

let plugins: plugin list ref = ref []
let iter_on_plugins f =
  let cmp p1 p2 =
    (* the kernel is the smaller plug-in *)
    match p1.p_name, p2.p_name with
    | s1, s2 when s1 = kernel_name && s2 = kernel_name -> 0
    | s1, _ when s1 = kernel_name -> -1
    | _, s2 when s2 = kernel_name -> 1
    | s1, s2 -> String.compare s1 s2
  in
  List.iter f (List.sort cmp !plugins)

let get_from_name s = List.find (fun p -> p.p_name = s) !plugins ;;
let get_from_shortname s = List.find (fun p -> p.p_shortname = s) !plugins
let get s = 
  Cmdline.Kernel_log.deprecated
    "Plugin.get" ~now:"Plugin.get_from_name" get_from_name s

(* ************************************************************************* *)
(** {2 The functor [Register]} *)
(* ************************************************************************* *)

module Register
  (P: sig
     val name: string (* the name is "" for the kernel *)
     val shortname: string
     val help: string
   end) =
struct

  let verbose_level = ref (fun () -> 1)
  let debug_level = ref (fun () -> 0)

  (* unused by the kernel: it uses Cmdline.Kernel_log instead; 
     see module [L] below *)
  module Plugin_log = Log.Register
    (struct
       let channel = P.shortname
       let label = P.shortname
       let debug_atleast level = !debug_level () >= level
       let verbose_atleast level = !verbose_level () >= level
     end)

  module L = struct
    module K = Cmdline.Kernel_log
    module P = Plugin_log
    let abort = if is_kernel () then K.abort else P.abort
    let warning = if is_kernel () then K.warning else P.warning
    let feedback =
      if is_kernel () then fun ?level x -> K.feedback ?level x 
      else fun ?level x -> P.feedback ?level x
    let get_all_categories = 
      if is_kernel () then K.get_all_categories else P.get_all_categories
    let get_category = if is_kernel () then K.get_category else P.get_category
    let add_debug_keys = 
      if is_kernel () then K.add_debug_keys else P.add_debug_keys
    let del_debug_keys = 
      if is_kernel () then K.del_debug_keys else P.del_debug_keys
  end

  let plugin =
    let name = if is_kernel () then kernel_name else P.name in
    let tbl = Hashtbl.create 17 in
    Hashtbl.add tbl empty_string [];
    { p_name = name; p_shortname = P.shortname; p_help = P.help; p_parameters = tbl }

  let add_group ?memo name =
    let parameter_groups = plugin.p_parameters in
    let g, new_g = Cmdline.Group.add ?memo ~plugin:P.shortname name in
    if new_g then Hashtbl.add parameter_groups name [];
    g

  let () =
    (try Cmdline.add_plugin P.name ~short:P.shortname ~help:P.help
     with Invalid_argument s ->
       L.abort "cannot register plug-in `%s': %s" P.name s);
    kernel_ongoing := is_kernel ();
    plugins := plugin :: !plugins

  (* ************************************************************************ *)
  (** {3 Generic options for each plug-in} *)
  (* ************************************************************************ *)

  let messages = add_group "Output Messages"

  include Parameter_builder.Make
      (struct
	let shortname = P.shortname
	module L = L
	let messages_group = messages
	let parameters = plugin.p_parameters
       end)

  let prefix =
    if P.shortname = empty_string then "-kernel-" else "-" ^ P.shortname ^ "-"

  let plugin_subpath = match !plugin_subpath_ref with
    | None -> P.shortname
    | Some s -> s

  (* ************************************************************************ *)
  (** {3 Specific directories} *)
  (* ************************************************************************ *)

  module Make_specific_dir
    (O: Parameter_sig.Input_with_arg)
    (D: sig 
      val dir: unit -> string 
      val visible_ref: bool 
      val force_dir: bool 
    end)
    =
  struct

    let is_visible = D.visible_ref
    let force_dir = D.force_dir
    let is_kernel = is_kernel () (* the side effect must be applied right now *)

    let () = 
      Parameter_customize.set_cmdline_stage Cmdline.Extended;
      if is_visible then Parameter_customize.do_iterate () 
      else Parameter_customize.is_invisible ()

    module Dir_name =
      EmptyString
        (struct
          let option_name = prefix ^ O.option_name
          let arg_name = O.arg_name
          let help = if is_visible then O.help else empty_string
         end)

    exception No_dir

    let mk_dir d =
      try
	Unix.mkdir d 0o755;
	L.warning "creating %s directory `%s'" O.option_name d;
	d
      with Unix.Unix_error _ -> 
	L.warning "cannot create %s directory `%s'" O.option_name d;
	raise No_dir

    let get_and_check_dir ?(error=true) d =
      (* DO NOT Filepath.normalize the argument, since it can transform an
         absolute path into a relative one, leading to issues if a chdir occurs
         at some point. *)
      if (try Sys.is_directory d with Sys_error _ -> false) then d
      else
	if error then 
	  L.abort "no %s directory `%s' for plug-in `%s'" 
	    O.option_name
	    d 
	    P.name 
        else begin
	  if force_dir then begin
	    (* create the parent, if it does not exist *)
	    let p = Filename.dirname d in
	    if not (try Sys.is_directory p with Sys_error _ -> false) then
	      ignore (mk_dir p);
	    mk_dir d
	  end else
	    raise No_dir
	end

    let dir ?error () =
      (* get the specified dir if any *)
      let d = if is_visible then Dir_name.get () else empty_string in
      if d = empty_string then
	(* no specified dir: look for the default one. *)
        if is_kernel then get_and_check_dir ?error (D.dir ())
        else get_and_check_dir ?error (D.dir () ^ "/" ^ plugin_subpath)
      else
        get_and_check_dir ?error d

    let file ?error f = dir ?error () ^ "/" ^ f

  end

  module Share = 
    Make_specific_dir
      (struct
	let option_name = "share"
	let arg_name = "dir"
	let help = "set the plug-in share directory to <dir> \
(may be used if the plug-in is not installed at the same place than Frama-C)"
       end)
      (struct 
	let dir () = Config.datadir 
	let visible_ref = !share_visible_ref 
	let force_dir = false
       end)

  module Session = 
    Make_specific_dir
      (struct
	let option_name = "session"
	let arg_name = "dir"
	let help = "set the plug-in session directory to <dir>"
       end)
      (struct 
	let dir () =
	  if !session_is_set_ref () then !session_ref ()
	  else
	    try Sys.getenv "FRAMAC_SESSION"
	    with Not_found -> "./.frama-c"
	let visible_ref = !session_visible_ref
	let force_dir = true
       end)
  let () = 
    if is_kernel () then Journal.get_session_file := Session.file ~error:false

  module Config = 
    Make_specific_dir
      (struct
	let option_name = "config"
	let arg_name = "dir"
	let help = "set the plug-in config directory to <dir> \
(may be used on systems with no default user directory)"
       end)
      (struct 
	let dir () =
	  let d, vis =
	    if !config_is_set_ref () then !config_ref (), false
	    else
	      try Sys.getenv "FRAMAC_CONFIG", false
	      with Not_found ->
		try Sys.getenv "USERPROFILE", false (* Win32 *) 
		with Not_found ->
		  (* Unix like *) 
		  try Sys.getenv "XDG_CONFIG_HOME", true
		  with Not_found -> 
		    try Sys.getenv "HOME" ^ "/.config", true
		    with Not_found -> ".", false
	  in
	  d ^ if vis then "/frama-c" else "/.frama-c"
	let visible_ref = !config_visible_ref
	let force_dir = true
       end)

  let help = add_group "Getting Information"

  let () = Parameter_customize.set_group help
  let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
  let () = if is_kernel () then Parameter_customize.set_module_name "Help"
  module Help =
    False
      (struct
        let option_name = prefix ^ "help"
        let help =
          if is_kernel () then "help of the Frama-C kernel"
          else "help of plug-in " ^ P.name
       end)
  let () =
    Cmdline.run_after_exiting_stage
      (fun () ->
         if Help.get () then Cmdline.plugin_help P.shortname else Cmdline.nop);
    Help.add_aliases [ prefix ^ "h" ]

  let output_mode modname optname =
    Parameter_customize.set_group messages;
    Parameter_customize.do_not_projectify ();
    Parameter_customize.do_not_journalize ();
    Parameter_customize.do_iterate ();
    if is_kernel () then begin
      Parameter_customize.set_cmdline_stage Cmdline.Early;
      Parameter_customize.set_module_name modname;
      "-" ^ kernel_name ^ "-" ^ optname
    end else begin
      Parameter_customize.set_cmdline_stage Cmdline.Extended;
      prefix ^ optname
    end

  let verbose_optname = output_mode "Verbose" "verbose"
  module Verbose = struct
    include
      Int(struct
            let default = !verbose_level ()
            let option_name = verbose_optname
            let arg_name = "n"
            let help =
              (if is_kernel () then "level of verbosity for the Frama-C kernel"
               else "level of verbosity for plug-in " ^ P.name)
              ^ " (default to " ^ string_of_int default ^ ")"
          end)
    let get () = if is_set () then get () else Cmdline.Verbose_level.get ()
    let () =
      verbose_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      if is_kernel () then begin
	Cmdline.kernel_verbose_atleast_ref := (fun n -> get () >= n);
	match !Cmdline.Kernel_verbose_level.value_if_set with
	| None -> ()
	| Some n -> set n
      end
  end

  let debug_optname = output_mode "Debug" "debug"
  module Debug = struct
    include
      Int(struct
            let default = !debug_level ()
            let option_name = debug_optname
            let arg_name = "n"
            let help =
              (if is_kernel () then "level of debug for the Frama-C kernel"
               else "level of debug for plug-in " ^ P.name)
              ^ " (default to " ^ string_of_int default ^ ")"
          end)
    let get () = if is_set () then get () else Cmdline.Debug_level.get ()
    let () =
      debug_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      add_set_hook
        (fun old n ->
	  (* the level of verbose is at least the level of debug *)
	  if n > Verbose.get () then Verbose.set n;
          if n = 0 then Pervasives.decr positive_debug_ref
          else if old = 0 then Pervasives.incr positive_debug_ref);
      if is_kernel () then begin
	Cmdline.kernel_debug_atleast_ref := (fun n -> get () >= n);
	match !Cmdline.Kernel_debug_level.value_if_set with
	| None -> ()
	| Some n -> set n
      end
  end

  let debug_category_optname = output_mode "Msg_key" "msg-key"
  let () = 
    Parameter_customize.set_unset_option_name
      (output_mode "Msg_key" "msg-key-unset");
    Parameter_customize.set_unset_option_help
      "disables message display for categories <k1>,...,<kn>"

  module Debug_category =
    StringSet(struct
      let option_name = debug_category_optname
      let arg_name="k1[,...,kn]"
      let help =
        "enables message display for categories <k1>,...,<kn>. Use "
        ^ debug_category_optname
        ^ " help to get a list of available categories, and * to enable \
              all categories"
    end)

  let () = 
    let module D = Datatype in
    Debug_category.add_set_hook
      (fun before after ->
        if not (D.String.Set.mem "help" before)
          && D.String.Set.mem "help" after then
          (* level 0 just in case user ask to display all categories
             in an otherwise quiet run *)
	  Cmdline.at_normal_exit
	    (fun () ->
	      L.feedback ~level:0
		"@[<v 2>Available message categories are:%a@]"
                (fun fmt set ->
                  Log.Category_set.iter
		    (fun s -> 
		      let s = (s:Log.category:>string) in
		      if s <> empty_string then Format.fprintf fmt "@;%s" s)
		    set)
                (L.get_all_categories ()));
        let add_category c s = D.String.Set.add (c:Log.category:>string) s in
        let subcategory_closure s =
          D.String.Set.fold
            (fun s acc -> Log.Category_set.union (L.get_category s) acc)
            s 
	    Log.Category_set.empty
        in
        let string_of_cat_set s =
          Log.Category_set.fold add_category s D.String.Set.empty
        in
        let remove = D.String.Set.diff before after in
        let added = D.String.Set.diff after before in
        let added = subcategory_closure added in
        let remove = subcategory_closure remove in
        L.add_debug_keys added;
        L.del_debug_keys remove;
        (* we add the subcategories to ourselves *)
        let after = D.String.Set.union after (string_of_cat_set added) in
        let after = D.String.Set.diff after (string_of_cat_set remove) in
        Debug_category.unsafe_set after)

  let () = reset_plugin ()

  include Plugin_log

end (* Register *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
