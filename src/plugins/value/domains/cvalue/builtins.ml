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

open Cil_types
open Cvalue

exception Invalid_nb_of_args of int

(* 'Always' means the builtin will always be used to replace a function
   with its name. 'OnAuto' means that the function will be replaced only
   if -val-builtins-auto is set. *)
type use_builtin = Always | OnAuto

let table = Hashtbl.create 17

let register_builtin name ?replace f =
  Hashtbl.replace table name (f, None, Always);
  match replace with
  | None -> ()
  | Some fname -> Hashtbl.replace table fname (f, Some name, OnAuto)

let () = Db.Value.register_builtin := register_builtin

(* The functions in _builtin must only return the 'Always' builtins *)

let registered_builtins () =
  let l =
    Hashtbl.fold
      (fun name (f, _, u) acc -> if u = Always then (name, f) :: acc else acc)
      table []
  in
  List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2) l

let () = Db.Value.registered_builtins := registered_builtins

let builtin_names_and_replacements () =
  let stand_alone, replacements =
    Hashtbl.fold (fun name (_, replaced_by, _) (acc1, acc2) ->
        match replaced_by with
        | None -> name :: acc1, acc2
        | Some rep_by -> acc1, (name, rep_by) :: acc2
      ) table ([], [])
  in
  List.sort String.compare stand_alone,
  List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2) replacements

let () =
  Cmdline.run_after_configuring_stage
    (fun () ->
       if Value_parameters.BuiltinsList.get () then begin
         let stand_alone, replacements = builtin_names_and_replacements () in
         Log.print_on_output
           (fun fmt ->
              Format.fprintf fmt "@[*** LIST OF EVA BUILTINS@\n@\n\
                                  ** Replacements set by -val-builtins-auto:\
                                  @\n   unless otherwise specified, \
                                  function <f> is replaced by builtin \
                                  Frama_C_<f>:@\n@\n   @[%a@]@]@\n"
                (Pretty_utils.pp_list ~sep:",@ "
                   (fun fmt (name, rep_by) ->
                      if rep_by = "Frama_C_" ^ name then
                        Format.fprintf fmt "%s" name
                      else
                        Format.fprintf fmt "%s (replaced by: %s)" name rep_by))
                replacements);
         Log.print_on_output
           (fun fmt ->
              Format.fprintf fmt "@\n@[** Full list of builtins \
                                  (configurable via -val-builtin):@\n\
                                  @\n   @[%a@]@]@\n"
                (Pretty_utils.pp_list ~sep:",@ "
                   Format.pp_print_string) stand_alone);
         raise Cmdline.Exit
       end)

let mem_builtin name =
  try
    let _, _, u = Hashtbl.find table name in
    u = Always
  with Not_found -> false

let () = Db.Value.mem_builtin := mem_builtin

let find_builtin_override kf =
  let name =
    try Value_parameters.BuiltinsOverrides.find kf
    with Not_found -> Kernel_function.get_name kf
  in
  try
    let f, _, u = Hashtbl.find table name in
    if u = Always || Value_parameters.BuiltinsAuto.get () then Some f
    else None
  with Not_found -> None

let warn_definitions_overridden_by_builtins () =
  Globals.Functions.iter (fun kf ->
      try
        let bname = Value_parameters.BuiltinsOverrides.find kf in
        if Kernel_function.is_definition kf &&
           not (Cil.hasAttribute "fc_stdlib" (Kernel_function.get_vi kf).vattr)
        then
          let fname = Kernel_function.get_name kf in
          let source = fst (Kernel_function.get_location kf) in
          Value_parameters.warning ~source ~once:true
            "function %s: definition will be overridden by %s@ \
             (use '-no-val-warn-builtin-override' to disable this warning)"
            fname (if fname = bname then "its builtin" else "builtin " ^ bname)
      with Not_found -> ()
    )

(* -------------------------------------------------------------------------- *)
(* --- Returning a clobbered set                                          --- *)
(* -------------------------------------------------------------------------- *)

let clobbered_set_from_ret state ret =
  let aux b _ acc =
    match Model.find_base_or_default b state with
    | `Top -> Base.SetLattice.top
    | `Bottom -> acc
    | `Value m ->
      if Locals_scoping.offsetmap_contains_local m then
        Base.SetLattice.(join (inject_singleton b) acc)
      else acc
  in
  try V.fold_topset_ok aux ret Base.SetLattice.bottom
  with Abstract_interp.Error_Top -> Base.SetLattice.top



(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
