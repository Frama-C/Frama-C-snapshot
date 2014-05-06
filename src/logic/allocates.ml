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

open Cil_types

(** Add [loop allocates \nothing] to the given stmt if no [loop allocates]
    clause currently exists for the default behavior *)
let add_allocates_loop stmt =
  let _behav = Cil.default_behavior_name in
  let all_default _ rca r =
    match rca.annot_content with
      | AAllocation (b, alloc) ->
          r && (b <> [] || alloc = FreeAllocAny)
      | _ -> r
  in
  let all_default = Annotations.fold_code_annot all_default stmt true in
  if all_default then
    let ca = AAllocation ([], FreeAlloc ([], [])) in
    Annotations.add_code_annot Emitter.kernel stmt
      (Logic_const.new_code_annotation ca)

let add_allocates_nothing_funspec kf =
  let behav = Cil.default_behavior_name in
  let all_default _ alloc r = r && alloc = FreeAllocAny in
  let all_default = Annotations.fold_allocates all_default kf behav true in
  if all_default then
    Annotations.add_allocates Emitter.kernel kf behav (FreeAlloc ([], []))

class vis_add_loop_allocates =
object
  inherit Visitor.frama_c_inplace

  method! vstmt s =
    (match s.skind with
      | Loop _ -> add_allocates_loop s;
      | _ -> ()
    );
    Cil.DoChildren

  method! vinst _ = Cil.SkipChildren

end

let add_allocates_nothing () =
  Globals.Functions.iter add_allocates_nothing_funspec;
  let vis = new vis_add_loop_allocates in
  Visitor.visitFramacFileSameGlobals vis (Ast.get ())
