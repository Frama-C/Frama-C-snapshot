(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
open Cil
open Db_types
open Db
open Cilutil

let debug = true

type postdominator = Value of Cilutil.StmtSet.t | Top

let mem v = function
  | Top -> true
  | Value d -> StmtSet.mem v d

let name = "postdominators"

module State =
  Kernel_computation.IntHashtbl
    (Project.Datatype.Register
       (struct
	  type t = postdominator
	  let map f = function
	    | Top -> Top
	    | Value set -> Value (f set)
	  let copy = map Kernel_datatype.StmtSet.copy
	  let rehash = map Kernel_datatype.StmtSet.rehash
	  include Datatype.Nop
	  let name = Project.Datatype.Name.make "postdominator"
	  let dependencies = [ Kernel_datatype.StmtSet.self ]
	end))
    (struct
       let name = Project.Computation.Name.make "postdominator"
       let dependencies = [ Cil_state.self ]
       let size = 503
     end)

module Computer = struct

  let name = name
  let debug = ref false

  type t = postdominator

  module StmtStartData = State

  let pretty fmt d =
    match d with
    | Top ->
	Format.fprintf fmt "Top"
    | Value d ->
	Format.fprintf fmt "{%a}"
          (Cil.fprintfList ~sep:","
	     (fun fmt s -> Format.fprintf fmt "%d" s.sid))
          (StmtSet.elements d)

  let inter a b = match a,b with
    | Top,Top -> Top
    | Value v, Top | Top, Value v -> Value v
    | Value v, Value v' -> Value (StmtSet.inter v v')

  let equal a b = match a,b with
    | Top,Top -> true
    | Value _v, Top | Top, Value _v -> false
    | Value v, Value v' -> StmtSet.equal v v'

  let add v d = match d with
    | Top -> Top
    | Value d -> Value (StmtSet.add v d)

  let combineStmtStartData _stmt ~old new_ =
    let result = (* inter old *) new_ in
    if equal result old then None else
      Some result

  let combineSuccessors = inter

  let doStmt stmt =
    !Db.progress ();
    if !debug then Cil.log "doStmt : %d\n" stmt.sid;
        match stmt.skind with
          | Return _ -> Dataflow.Done (Value (StmtSet.singleton stmt))
          | _ -> Dataflow.Post (fun data -> add stmt data)

  let doInstr _ _ _ = Dataflow.Default

  let filterStmt _stmt _next = true

  let funcExitData = Value StmtSet.empty

end

let clear () = Computer.StmtStartData.clear()

module Compute = Dataflow.BackwardsDataFlow(Computer)

let compute kf =
  (*clear();*)
  let f = kf.fundec in
  Cil.log "[postdominators] computing for function %a"
    Kernel_function.pretty_name kf;
  let stmts = match f with
             | Definition (f,_) -> f.sallstmts
             | Declaration _ -> invalid_arg
                 "[postdominators] cannot compute for a leaf function"
  in
  List.iter (fun s -> State.add s.sid Top) stmts;
  let return = Kernel_function.find_return kf in
  Compute.compute [return];
  Cil.log "[postdominators] done for function %a"
    Kernel_function.pretty_name kf

let get_stmt_postdominators f stmt =
  let do_it () = State.find stmt.sid in
  try
    do_it ()
  with Not_found ->
    !Postdominators.compute f;
    do_it ()

(** @raise Db.Top_postdominators when the statement postdominators
* have not been computed ie neither the return statement is reachable,
* nor the statement is in a natural loop. *)
let stmt_postdominators f stmt =
    match get_stmt_postdominators f stmt with
    | Value s -> s
    | Top -> raise Db.Postdominators.Top

let is_postdominator f ~opening ~closing =
  let open_postdominators = get_stmt_postdominators f opening in
  mem closing open_postdominators

let display () =
  State.iter
    (fun k v -> Format.printf "Stmt:%d\n%a\n======" k Computer.pretty v)

let print_dot basename kf =
  let filename = basename ^ "." ^ Kernel_function.get_name kf ^ ".dot" in
    Print.build_dot filename kf;
    Format.printf "[postdominators] dot file generated in %s@\n" filename

let () = Db.Postdominators.compute := compute
let () = Db.Postdominators.is_postdominator := is_postdominator
let () = Db.Postdominators.stmt_postdominators := stmt_postdominators
let () = Db.Postdominators.display := display
let () = Db.Postdominators.print_dot := print_dot

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
