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

(* The keys are the stmts which were previously labeled, whereas the associated
   values are the new stmts containing the same labels. *)
module Labeled_stmts =
  Cil_state_builder.Stmt_hashtbl
    (Cil_datatype.Stmt)
    (struct
      let size = 7
      let dependencies = [] (* delayed *)
      let name = "E-ACSL.Labels"
     end)

let self = Labeled_stmts.self

let new_labeled_stmt stmt = try Labeled_stmts.find stmt with Not_found -> stmt

let move (vis:Visitor.generic_frama_c_visitor) ~old new_stmt =
  let labels = old.labels in
  match labels with
  | [] -> ()
  | _ :: _ ->
    old.labels <- [];
    new_stmt.labels <- labels @ new_stmt.labels;
    Labeled_stmts.add old new_stmt;
    (* update the gotos of the function jumping to one of the labels *)
    let o orig_stmt = object
      inherit Visitor.frama_c_inplace
      (* invariant of this method: [s = Cil.memo_stmt vis#behavior orig_stmt] *)
      method !vstmt_aux s = match s.skind, orig_stmt.skind with
      | Goto(s_ref, loc), Goto(orig_ref, _) ->
        if Cil_datatype.Stmt.equal !s_ref old then
          if s_ref == orig_ref then
            (* The memo_stmt and its origin [orig_stmt] contain a shared
               reference because [orig_stmt] has not yet been visited by [vis]
               (forward goto). Consequently, do not modify the ref directly but
               replace the corresponding stmt in the memoisation table. When
               [orig_stmt] will be visited, the visitor will automatically
               substitute it with the updated stmt. *)
            Cil.set_stmt vis#behavior
              orig_stmt
              { s with skind = Goto(ref new_stmt, loc) }
          else
            (* Backward goto: it has already been visited and there is no more
               sharing. Directly update the reference. *)
            s_ref := new_stmt;
        Cil.SkipChildren
      | _ -> Cil.DoChildren
      (* improve efficiency: skip childrens which cannot contain any label *)
      method !vinst _ = Cil.SkipChildren
      method !vexpr _ = Cil.SkipChildren
      method !vlval _ = Cil.SkipChildren
    end in
    let f = Extlib.the vis#current_func in
    let mv_labels s =
      ignore (Visitor.visitFramacStmt (o s) (Cil.memo_stmt vis#behavior s))
    in
    List.iter mv_labels f.sallstmts

let get_stmt vis = function
  | StmtLabel { contents = stmt } -> stmt
  | BuiltinLabel Here ->
    (match vis#current_stmt with
    | None -> Error.not_yet "label \"Here\" in function contract"
    | Some s -> s)
  | BuiltinLabel(Old | Pre) ->
    (try Kernel_function.find_first_stmt (Extlib.the vis#current_kf)
     with Kernel_function.No_Statement -> assert false)
  | BuiltinLabel(Post) ->
    (try Kernel_function.find_return (Extlib.the vis#current_kf)
     with Kernel_function.No_Statement -> assert false)
  | BuiltinLabel _ | FormalLabel _ -> assert false

(*
Local Variables:
compile-command: "make"
End:
*)
