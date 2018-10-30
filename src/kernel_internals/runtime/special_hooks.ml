(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(* just after loading all plug-ins, add the dependencies between the AST
   and the command line options that depend on it. *)
let () =
  Cmdline.run_after_extended_stage
    (fun () ->
      State_dependency_graph.add_dependencies
        ~from:Ast.self
        !Parameter_builder.ast_dependencies)

let print_config () =
  if Kernel.PrintConfig.get () then begin
    Log.print_on_output 
      (fun fmt -> Format.fprintf fmt
          "Frama-C %s@\n\
           Environment:@\n  \
           FRAMAC_SHARE  = %S@\n  \
           FRAMAC_LIB    = %S@\n  \
           FRAMAC_PLUGIN = %S%t@."
          Config.version_and_codename
          Config.datadir Config.libdir Config.plugin_path
        (fun fmt ->
          if Config.preprocessor = "" then
            Format.fprintf fmt "@\nWarning: no default pre-processor"
          else if not Config.preprocessor_keep_comments then
            Format.fprintf fmt
              "@\nWarning: default pre-processor is not able to keep comments \
               (hence ACSL annotations) in its output")
        ;
        );
    raise Cmdline.Exit
  end
let () = Cmdline.run_after_early_stage print_config

let print_config get value () =
  if get () then begin
    Log.print_on_output (fun fmt -> Format.fprintf fmt "%s%!" value) ;
    raise Cmdline.Exit
  end

let print_version = print_config Kernel.PrintVersion.get Config.version_and_codename
let () = Cmdline.run_after_early_stage print_version

let print_sharepath = print_config Kernel.PrintShare.get Config.datadir
let () = Cmdline.run_after_early_stage print_sharepath

let print_libpath = print_config Kernel.PrintLib.get Config.libdir
let () = Cmdline.run_after_early_stage print_libpath

let print_pluginpath =
  print_config Kernel.PrintPluginPath.get Config.plugin_path
let () = Cmdline.run_after_early_stage print_pluginpath

let print_machdep () =
  if Kernel.PrintMachdep.get () then begin
    File.pretty_machdep ();
    raise Cmdline.Exit
  end else
    Cmdline.nop
let () = Cmdline.run_after_exiting_stage print_machdep


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
let () = Extlib.safe_at_exit time

(* Save Frama-c on disk if required *)
let save_binary error_extension =
  let filename = Kernel.SaveState.get () in
  if filename <> "" then begin
    Kernel.SaveState.clear ();
    let realname =
      match error_extension with
      | None -> filename
      | Some err_ext ->
        let s = filename ^ err_ext in
        Kernel.warning
          "attempting to save on non-zero exit code: \
           modifying filename into `%s'." s;
        s
    in
    try 
      Project.save_all realname
    with Project.IOError s ->
      Kernel.error "problem while saving to file %s (%s)." realname s
  end
let () = 
  (* implement a refinement of the behavior described in BTS #1388:
     - on normal exit: save
     - on Sys.break, system error or feature request: do not save
     - on user error: save, but add ".error" suffix
     - on fatal error or unexpected error: save, but add ".crash" suffix *)
  Cmdline.at_normal_exit (fun () -> save_binary None);
  Cmdline.at_error_exit
    (function
      | Sys.Break | Sys_error _ | Log.FeatureRequest _ -> ()
      | Log.AbortError _ -> save_binary (Some ".error")
      | _ -> save_binary (Some ".crash"))

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

(* This hook cannot be registered directly in Kernel or Cabs2cil, as it
   depends on Ast_info *)
let on_call_to_undeclared_function vi =
  let name = vi.Cil_types.vname in
  if not (Ast_info.is_frama_c_builtin name) then
    Kernel.warning ~wkey:Kernel.wkey_implicit_function_declaration
      ~current:true ~once:true
      "Calling undeclared function %s. Old style K&R code?" name

let () =
  Cabs2cil.register_implicit_prototype_hook on_call_to_undeclared_function

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
