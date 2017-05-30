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
open Cil_datatype

module Build_env(X: sig type t val name: string end): sig
  val add: stmt -> X.t -> unit
  val get: stmt -> X.t
  val get_all: stmt -> X.t list
  val is_empty: unit -> bool
  val clear: unit -> unit
end = struct

  let tbl = Stmt.Hashtbl.create 17
  let add = Stmt.Hashtbl.add tbl

  let get stmt =
    try Stmt.Hashtbl.find tbl stmt
    with Not_found ->
      Options.fatal "unknown stmt %a in %s" Printer.pp_stmt stmt X.name

  let get_all = Stmt.Hashtbl.find_all tbl
  let is_empty () = Stmt.Hashtbl.length tbl = 0
  let clear () = Stmt.Hashtbl.clear tbl

end

(* Mapping of statements to local variables available within that statement's
   scope. The mappings of this structure are used to determine variables which
   need to be removed before goto jumps. Generally, if some goto (with
   scope variables are given by set G') jumps to a labeled statement with
   scope variables given by set L', then the goto exists the scopes of
   variables given via set G' \ L'. Consequently, if those variables are
   tracked, they need to be removed from tracking. *)
module SLocals =
  Build_env(struct type t = Varinfo.Set.t let name = "SLocals" end)

(* Statement to statement mapping indicating source/destination of a jump.
   For instance, break statements are mapped to switches or loops they jump
   out from and goto statements are mapped to their labeled statements. Notably,
   such information does not really be computed for gotos (since they already
   capture references to labelled statements they jumps to). Nevertheless it is
   done for consistency, so all required information is stored uniformly. *)
module Exits =
  Build_env(struct type t = stmt let name = "Exits" end)

(* Map labelled statements back to gotos which lead to them *)
module LJumps =
  Build_env(struct type t = stmt let name = "LJumps" end)

let clear () =
  SLocals.clear ();
  Exits.clear ();
  LJumps.clear ()

let is_empty () =
  SLocals.is_empty () && Exits.is_empty () && LJumps.is_empty ()

let delete_vars stmt =
  match stmt.skind with
  | Goto _ | Break _ | Continue _ ->
    Varinfo.Set.diff (SLocals.get stmt) (SLocals.get (Exits.get stmt))
  | _ ->
    Varinfo.Set.empty

let store_vars stmt =
  let gotos = LJumps.get_all stmt in
  List.fold_left
    (fun acc goto ->
      Varinfo.Set.union
        acc
        (Varinfo.Set.diff (SLocals.get stmt) (SLocals.get goto)))
    Varinfo.Set.empty
    gotos

let unify_sets =
  List.fold_left (fun acc v -> Varinfo.Set.union v acc) Varinfo.Set.empty

class jump_context = object (_)
  inherit Visitor.frama_c_inplace

  val mutable locals = []
  (* Maintained list of local variables within the scope of a currently
     visited statement. Variables within a single scope are given by a
     single set *)

  val jumps = Stack.create ()
  (* Stack of entered switches and loops *)

  method !vblock blk =
    (* Filter out variables which definitions appear later in the code *)
    let vardefs = List.filter (fun vi -> not vi.vdefined) blk.blocals in
    locals <- Varinfo.Set.of_list vardefs :: locals;
    Cil.DoChildrenPost
      (fun blk -> locals <- List.tl locals; blk)

  method !vstmt stmt =
    let add_labels stmt =
      match stmt.labels with
      | [] -> ()
      | _ :: _ -> SLocals.add stmt (unify_sets locals)
    in
    match stmt.skind with
    | Loop _ | Switch _ ->
      SLocals.add stmt (unify_sets locals);
      Stack.push stmt jumps;
      Cil.DoChildrenPost (fun st -> ignore(Stack.pop jumps); st)
    | Break _ | Continue _ ->
      Exits.add stmt (Stack.top jumps);
      SLocals.add stmt (unify_sets locals);
      Cil.DoChildren
    | Goto(sref, _)  ->
      SLocals.add stmt (unify_sets locals);
      Exits.add stmt !sref;
      LJumps.add !sref stmt;
      Cil.DoChildren
    | Instr(Local_init (vi, _, _)) ->
      locals <- (Varinfo.Set.add vi (List.hd locals)) :: List.tl locals;
      add_labels stmt;
      Cil.DoChildren
    | Instr _ | Return _ | If _ | Block _ | UnspecifiedSequence _
    | Throw _ | TryCatch _ | TryFinally _ | TryExcept _ ->
      add_labels stmt;
      Cil.DoChildren
end

let generate fct =
  assert (is_empty ());
  ignore (Cil.visitCilFunction (new jump_context :> Cil.cilVisitor) fct)
