(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Recursion *)

(* Our current treatment for recursion -- use the specification for
   the function that begins the recursive cycle -- is incorrect for
   function with formals whose address is taken. Indeed, we do not know
   which "instance" of the formal is updated by the specification. In
   this case, warn the user. *)
let check_formals_non_referenced kf =
  let formals = Kernel_function.get_formals kf in
  if List.exists (fun vi -> vi.vaddrof) formals then
    Value_parameters.error ~current:true ~once:true
      "function '%a' (involved in a recursive call) has a formal parameter \
       whose address is taken. Analysis may be unsound."
      Kernel_function.pretty kf

let warn_recursive_call kf call_stack =
  if Value_parameters.IgnoreRecursiveCalls.get ()
  then begin
    Value_parameters.error ~current:true ~once:true
      "@[recursive call@ during@ value@ analysis@ of %a \
       @[(%a <- %a)@].@ Assuming@ the call@ has@ no effect.@ \
       The analysis@ will@ be@ unsound.]"
      Kernel_function.pretty kf Kernel_function.pretty kf
      Value_types.Callstack.pretty call_stack ;
    check_formals_non_referenced kf;
    Db.Value.recursive_call_occurred kf;
  end
  else begin
    Value_util.warning_once_current
      "@[@[detected@ recursive@ call@ (%a <- %a)@]@;@[Use %s@ to@ \
       ignore@ (beware@ this@ will@ make@ the analysis@ unsound)@]@]"
      Kernel_function.pretty kf Value_types.Callstack.pretty call_stack
      Value_parameters.IgnoreRecursiveCalls.option_name;
    raise Db.Value.Aborted
  end

(* Check whether the function at the top of the call-stack starts a
   recursive call. *)
let is_recursive_call kf =
  let call_stack = Value_util.call_stack () in
  if List.exists (fun (f, _) -> f == kf) call_stack
  then (warn_recursive_call kf call_stack; true)
  else false

(* Find a spec for a function [kf] that begins a recursive call. If [kf]
   has no existing specification, generate (an incorrect) one, and warn
   loudly. *)
let _spec_for_recursive_call kf =
  let initial_spec = Annotations.funspec ~populate:false kf in
  match Cil.find_default_behavior initial_spec with
  | Some bhv when bhv.b_assigns <> WritesAny -> initial_spec
  | _ ->
    let assigns = Infer_annotations.assigns_from_prototype kf in
    let bhv = Cil.mk_behavior ~assigns:(Writes assigns) () in
    let spec = { (Cil.empty_funspec ()) with spec_behavior = [bhv] } in
    Value_parameters.error ~once:true
      "@[recursive@ call@ on@ an unspecified@ \
       function.@ Using@ potentially@ invalid@ inferred assigns '%t'@]"
      (fun fmt -> match assigns with
         | [] -> Format.pp_print_string fmt "assigns \\nothing"
         | _ :: _ ->
           Pretty_utils.pp_list ~sep:"@ " Printer.pp_from fmt assigns);
    (* Merge existing spec into our custom one with assigns *)
    Logic_utils.merge_funspec
      ~silent_about_merging_behav:true spec initial_spec;
    spec

let empty_spec_for_recursive_call kf =
  let typ_res = Kernel_function.get_return_type kf in
  let empty = Cil.empty_funspec () in
  let assigns =
    if Cil.isVoidType typ_res then
      Writes []
    else
      let res = TResult typ_res, TNoOffset in
      let res = Logic_const.term (TLval res) (Ctype typ_res) in
      let res = Logic_const.new_identified_term res in
      Writes [res, From []]
  in
  let bhv = Cil.mk_behavior ~assigns ~name:Cil.default_behavior_name () in
  empty.spec_behavior <- [bhv];
  empty
