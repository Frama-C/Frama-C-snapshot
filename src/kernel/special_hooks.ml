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

let version () =
  if Kernel.PrintVersion.get () then begin
    Log.print_on_output 
      (fun fmt -> Format.fprintf fmt "Version: %s@\n\
Compilation date: %s@\n\
Share path: %s (may be overridden with FRAMAC_SHARE variable)@\n\
Library path: %s (may be overridden with FRAMAC_LIB variable)@\n\
Plug-in paths: %t(may be overridden with FRAMAC_PLUGIN variable)%t@."
	 Config.version Config.date Config.datadir Config.libdir
	 (fun fmt -> List.iter 
	    (fun s -> Format.fprintf fmt "%s " s)
	    (Dynamic.default_path ()))
        (fun fmt ->
          if Config.preprocessor = "" then
            Format.fprintf fmt "@\nWARNING: no default pre-processor"
          else if not Config.preprocessor_keep_comments then
            Format.fprintf fmt
              "@\nWARNING: default pre-processor is not able to keep comments \
               (hence ACSL annotations) in its output"
        ));
    raise Cmdline.Exit
  end
let () = Cmdline.run_after_early_stage version

let print_path get dir () =
  if get () then begin
    Log.print_on_output (fun fmt -> Format.fprintf fmt "%s%!" dir) ;
    raise Cmdline.Exit
  end

let print_sharepath = print_path Kernel.PrintShare.get Config.datadir
let () = Cmdline.run_after_early_stage print_sharepath

let print_libpath = print_path Kernel.PrintLib.get Config.libdir
let () = Cmdline.run_after_early_stage print_libpath

let print_pluginpath = print_path Kernel.PrintPluginPath.get Config.plugin_dir
let () = Cmdline.run_after_early_stage print_pluginpath

(* Time *)
let time () =
  let filename = Kernel.Time.get () in
  if filename <> "" then
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
    close_out oc
let () = at_exit time

(* Save Frama-c on disk if required *)
let save_binary keep_name =
  let filename = Kernel.SaveState.get () in
  if filename <> "" then begin
    Kernel.SaveState.clear ();
    let realname =
      if keep_name then filename
      else begin
	let s = filename ^ ".crash" in
	Kernel.warning
	  "attempting to save on crash: modifying filename into `%s'." s;
	s
      end
    in
    try 
      Project.save_all realname
    with Project.IOError s ->
      Kernel.error "problem while saving to file %s (%s)." realname s
  end
let () = 
  (* implement behavior described in BTS #1388: 
     - on normal exit: save
     - on Sys.break, system error, user error or feature request: do not save
     - on fatal error or unexpected error: save, but slighly change the
     generated filename. *)
  Cmdline.at_normal_exit (fun () -> save_binary true);
  Cmdline.at_error_exit
    (function
    | Sys.Break | Sys_error _ | Log.AbortError _ | Log.FeatureRequest _ -> ()
    | _ -> save_binary false)

(* Load Frama-c from disk if required *)
let load_binary () =
  let filename = Kernel.LoadState.get () in
  if filename <> "" then begin
    try
      Project.load_all filename
    with Project.IOError s ->
      Kernel.abort "problem while loading file %s (%s)" filename s
  end
let () = Cmdline.run_after_loading_stage load_binary

(* This hook cannot be registered directly  in Kernel or Cabs2cil, as
   it depends on Ast_info *)
let warn_for_call_to_undeclared_function vi =
  let name = vi.Cil_types.vname in
  if Kernel.WarnUndeclared.get () && not (Ast_info.is_frama_c_builtin name)
  then
    Kernel.warning ~current:true ~once:true
      "Calling undeclared function %s. Old style K&R code?" name

let () =
  Cabs2cil.register_implicit_prototype_hook warn_for_call_to_undeclared_function


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
