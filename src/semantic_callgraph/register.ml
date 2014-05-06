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

open Options

module KfSorted = struct
  type t =  Kernel_function.t
      (* Basic comparison of kernel function compares on vid.
         As this has an impact to results shown to the user, it's better
         to use an ordering which depends only on the input itself, not
         how the numbering of varinfo is done internally
      *)
  let equal = Kernel_function.equal
  let hash kf = Hashtbl.hash (Kernel_function.get_name kf)
  let compare kf1 kf2 =
    if kf1 == kf2 then 0
    else
      let res =
        String.compare
          (Kernel_function.get_name kf1)
          (Kernel_function.get_name kf2)
      in
      if res <> 0 then res
      else (* Backup solution, will compare underlying varinfos ids *)
        Kernel_function.compare kf1 kf2
end

module SetKfSorted = FCSet.Make(KfSorted)

module SGraph =
  Graph.Imperative.Digraph.ConcreteLabeled
    (KfSorted)
    (struct include Cil_datatype.Stmt let default = Cil.dummyStmt end)

module SGState =
  State_builder.Option_ref
    (Datatype.Make
       (struct
         (* [JS 2010/09/27] do better? *)
         include Datatype.Serializable_undefined
         type t = SGraph.t
         let name = "SGraph"
         let reprs = [ SGraph.create () ]
         let mem_project = Datatype.never_any_project
        end))
    (struct
      let name = "SGState"
      let dependencies = [ Db.Value.self ]
     end)

module SCQueue =
  State_builder.Queue
    (Kernel_function)
    (struct
      let name = "SCQueue"
      let dependencies = [ SGState.self ]
     end)

let callgraph () =
  SGState.memo
    (fun () ->
       let g = SGraph.create () in
       !Db.Value.compute ();
       Globals.Functions.iter
         (fun kf ->
            if !Db.Value.is_called kf then SGraph.add_vertex g kf;
            List.iter
              (fun (caller,call_sites) ->
                 List.iter
                   (fun call_site -> SGraph.add_edge_e g (kf,call_site,caller))
                   call_sites)
              (!Db.Value.callers kf));
       g)

module Service =
  Service_graph.Make
    (struct
       let datatype_name = name
       type t = SGraph.t
       module V = struct
	 include Kernel_function
         let id v = (Kernel_function.get_vi v).Cil_types.vid
         let name = Kernel_function.get_name
         let attributes v =
           [ `Style
               [if Kernel_function.is_definition v then `Bold
                else `Dotted] ]
         let entry_point () =
           try Some (fst (Globals.entry_point ()))
           with Globals.No_such_entry_point _ -> None
       end
       let iter_vertex = SGraph.iter_vertex
       let iter_succ = SGraph.iter_succ
       let iter_pred = SGraph.iter_pred
       let fold_pred = SGraph.fold_pred
     end)

module ServiceState =
  State_builder.Option_ref
    (Service.CallG.Datatype)
    (struct
       let name = "SemanticsServicestate"
       let dependencies =
         [ SGState.self; Kernel.MainFunction.self; InitFunc.self ]
     end)

let get_init_funcs () =
  let init_funcs = InitFunc.get () in
  try
    let callees =
      let kf, _ = Globals.entry_point () in
      !Db.Users.get kf
    in
    (** add the entry point as root *)
    let init_funcs =
      Datatype.String.Set.add (Kernel.MainFunction.get ()) init_funcs
    in
    (* add the callees of entry point as roots *)
    Kernel_function.Hptset.fold
      (fun kf acc -> Datatype.String.Set.add (Kernel_function.get_name kf) acc)
      callees
      init_funcs
  with Globals.No_such_entry_point _ ->
    (* always an entry point for the semantic callgraph since value analysis has
       been computed. *)
    assert false

let compute () =
  feedback "beginning analysis";
  let cg =
    Service.compute
      (callgraph ())
      (get_init_funcs ())
  in
  feedback "analysis done";
  ServiceState.mark_as_computed ();
  cg

let get () = ServiceState.memo compute

let dump () =
  let cg = get () in
  let file = Filename.get () in
  feedback ~level:2 "dumping the graph into file %s" file;
  try
    let o = open_out file in
    Service.output_graph o cg;
    close_out o
  with e ->
    error
      "error while dumping the semantic callgraph: %s"
      (Printexc.to_string e)

let () =
  Db.register_guarded_compute
    "Semantic_Callgraph.dump"
    (fun () -> Filename.get () = "" || ServiceState.is_computed ())
    Db.Semantic_Callgraph.dump
    dump

let () =
  (* Do not directly use [dump]: function in [Db] is guarded and apply only if
     required. *)
  Db.Main.extend (fun _fmt -> !Db.Semantic_Callgraph.dump ())

let topologically_iter_on_functions =
  let module T = Graph.Topological.Make_stable(SGraph) in
  fun f ->
    (* compute on need *)
    if SCQueue.is_empty () then T.iter SCQueue.add (callgraph ());
    SCQueue.iter f

let iter_on_callers f kf =
  let cg = callgraph () in
  let visited = Kernel_function.Hashtbl.create 17 in
  let rec aux kf =
    if SGraph.mem_vertex cg kf then
      SGraph.iter_succ
        (fun caller ->
          if not (Kernel_function.Hashtbl.mem visited caller) then begin
            f caller;
            Kernel_function.Hashtbl.add visited caller ();
            aux caller
          end)
        cg
        kf
    else
      Options.warning ~once:true
        "Function %s not registered in semantic callgraph. Skipped."
        (Kernel_function.get_name kf)
  in
  aux kf


let is_local_or_formal_of_caller v kf =
  try
    iter_on_callers
      (fun caller ->
         let formal_or_local =
           (Base.is_formal_or_local v (Kernel_function.get_definition caller))
         in
         if formal_or_local then raise Exit)
      kf;
    false
  with Exit -> true


let accept_base ~with_formals ~with_locals kf v =
  let open Cil_types in
  Base.is_global v
  ||
  (match with_formals, with_locals, kf.fundec with
     | false, false, _ -> false
     | true,  false, Definition (fundec,_) -> Base.is_formal v fundec
     | false, true, Definition (fundec, _) -> Base.is_local v fundec
     | true,  true, Definition (fundec, _) -> Base.is_formal_or_local v fundec
     | false, _, Declaration _ -> false
     | true , _, Declaration (_, vd, _, _) -> Base.is_formal_of_prototype v vd
  )
  || is_local_or_formal_of_caller v kf


let () =
  Db.Semantic_Callgraph.topologically_iter_on_functions :=
    topologically_iter_on_functions;

  let tf = Datatype.func Kernel_function.ty Datatype.Unit.ty in
  Db.Semantic_Callgraph.iter_on_callers :=
    Dynamic.register ~plugin:"Semantic_callgraph" "iter_on_callers"
      (Datatype.func tf tf) ~journalize:false iter_on_callers;

  Db.Semantic_Callgraph.accept_base := accept_base;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
