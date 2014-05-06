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

let cmdline_stage_ref = ref Cmdline.Configuring
let set_cmdline_stage s = cmdline_stage_ref := s

let journalize_ref = ref true
let do_not_journalize () = journalize_ref := false

let negative_option_name_ref = ref None
let set_negative_option_name s = negative_option_name_ref := Some s

let negative_option_help_ref = ref empty_string
let set_negative_option_help s = negative_option_help_ref := s

let unset_option_name_ref = ref empty_string
let set_unset_option_name s = unset_option_name_ref := s

let unset_option_help_ref = ref empty_string
let set_unset_option_help s = unset_option_help_ref := s

let must_save_ref = ref true
let do_not_save () = must_save_ref := false

let reset_on_copy_ref = ref true
let do_not_reset_on_copy () = reset_on_copy_ref := false

let projectify_ref = ref true
let do_not_projectify () =
  projectify_ref := false;
  do_not_save ();
  do_not_reset_on_copy ()

let empty_format = ("": (unit, Format.formatter, unit) format)
let optional_help_ref = ref empty_format
let set_optional_help fmt = optional_help_ref := fmt
let set_optional_help fmt =
  Cmdline.Kernel_log.deprecated
    "Plugin.set_optional_help"
    ~now:"<none>"
    set_optional_help
    fmt

let module_name_ref = ref empty_string
let set_module_name s = module_name_ref := s

let argument_is_function_name_ref = ref false
let argument_is_function_name () = argument_is_function_name_ref := true

let group_ref = ref Cmdline.Group.default
let set_group s = group_ref := s

let do_iterate_ref = ref None
let do_iterate () = do_iterate_ref := Some true
let do_not_iterate () = do_iterate_ref := Some false

let is_visible_ref = ref true
let is_invisible () =
  is_visible_ref := false;
  do_not_iterate ()

let reset () =
  cmdline_stage_ref := Cmdline.Configuring;
  journalize_ref := true;
  negative_option_name_ref := None;
  negative_option_help_ref := empty_string;
  unset_option_name_ref:= empty_string;
  unset_option_help_ref:= empty_string;
  optional_help_ref := empty_format;
  projectify_ref := true;
  must_save_ref := true;
  module_name_ref := empty_string;
  group_ref := Cmdline.Group.default;
  do_iterate_ref := None;
  is_visible_ref := true;
  argument_is_function_name_ref := false;
  reset_on_copy_ref:= true

(* ************************************************************************* *)
(** {2 Delayed Kernel Initialisation} *)
(* ************************************************************************* *)

let function_names_ref: (unit -> string list) ref = ref (fun () -> [])
let set_function_names f = function_names_ref := f

let no_ast_hook = fun _ -> ()
let ast_hook: ((Cil_types.file -> unit) -> unit) ref = ref no_ast_hook
let init_ast_hooks = ref []
let set_ast_hook f = ast_hook := f
let apply_ast_hook f =
  let f _ = f (!function_names_ref ()) in
  let ah = !ast_hook in
  if ah == no_ast_hook then init_ast_hooks := f :: !init_ast_hooks else ah f

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
