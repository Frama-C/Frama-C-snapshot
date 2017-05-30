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

module Loop_invariants_actions = Hook.Make(struct end)

let apply_after_transformation prj =
  Project.on prj Loop_invariants_actions.apply ()

let mv_invariants env ~old stmt = 
  Options.feedback ~current:true ~level:3
    "keep loop invariants attached to its loop";
  match Env.current_kf env with
  | None -> assert false
  | Some kf ->
    let filter _ ca = match ca.annot_content with 
      | AInvariant(_, b, _) -> b
      | _ -> false
    in
    let l = Annotations.code_annot_emitter ~filter stmt in
    if l != [] then
      Loop_invariants_actions.extend
	(fun () -> 
	  List.iter
	    (fun (ca, e) ->
	      Annotations.remove_code_annot e ~kf old ca;
	      Annotations.add_code_annot e ~kf stmt ca)
	    l)

let preserve_invariant prj env kf stmt = match stmt.skind with
  | Loop(_, ({ bstmts = stmts } as blk), loc, cont, break) -> 
    let rec handle_invariants (stmts, env, _ as acc) = function
      | [] -> 
	(* empty loop body: no need to verify the invariant twice *)
	acc
      | [ last ] -> 
	let invariants, env = Env.pop_loop env in
	let env = Env.push env in
	let env = 
	  Project.on
	    prj
	    (List.fold_left (Translate.translate_named_predicate kf) env)
	    invariants 
	in
	let blk, env = 
	  Env.pop_and_get env last ~global_clear:false Env.Before
	in
	Misc.mk_block prj last blk :: stmts, env, invariants != []
      | s :: tl -> handle_invariants (s :: stmts, env, false) tl
    in
    let env = Env.set_annotation_kind env Misc.Invariant in
    let stmts, env, has_loop = handle_invariants ([], env, false) stmts in
    let new_blk = { blk with bstmts = List.rev stmts } in
    { stmt with skind = Loop([], new_blk, loc, cont, break) }, 
    env, 
    has_loop
  | _ -> stmt, env, false

(*
Local Variables:
compile-command: "make"
End:
*)
