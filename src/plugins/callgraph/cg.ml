(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(* Kernel functions with a custom function [compare] independent of vids. So the
   callgraph and its iterations are independent from the vids generator and is
   only dependent of the analyzed program itsef. *)
module Kf_sorted = struct
  type t =  Kernel_function.t
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
      else
        (* Backup solution, will compare underlying varinfos ids *)
        Kernel_function.compare kf1 kf2
end

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (Kf_sorted)
    (struct include Cil_datatype.Stmt let default = Cil.dummyStmt end)

module D =
  Datatype.Make(struct
    type t = G.t
    let name = "Callgraph.Cg"
    let reprs = [ G.create () ]
    include Datatype.Serializable_undefined
    let mem_project = Datatype.never_any_project
  end)

(* State for the callgraph *)
module State =
  State_builder.Option_ref
    (D)
    (struct
       let name = "Callgraph.Cg"
       let dependencies = [ Db.Value.self; Globals.Functions.self ]
     end)

let self = State.self
let is_computed () = State.is_computed ()

(** @return the list of functions which address is taken.*)
let get_pointed_kfs =
  (* memoized result *)
  let res = ref None in
  fun () ->
    let compute () =
      let l = ref [] in
      let o = object
        inherit Visitor.frama_c_inplace
        method !vexpr e = match e.enode with
        | AddrOf (Var vi, NoOffset) when Cil.isFunctionType vi.vtype ->
          (* function pointer *)
          let kf =
            try Globals.Functions.get vi
            with Not_found -> assert false
          in
          l := kf :: !l;
          Cil.SkipChildren
        | _ ->
          Cil.DoChildren
      end
      in
      Visitor.visitFramacFileSameGlobals o (Ast.get ());
      !l
    in
    match !res with
    | None ->
      let l = compute () in
      State.mark_as_computed ();
      res := Some l;
      l
    | Some l -> l

let is_entry_point kf =
  try
    let main, _ = Globals.entry_point () in
    Kernel_function.equal kf main
  with Globals.No_such_entry_point _ -> false

(* complexity = O(number of statements);
   approximate function pointers to the set of functions which address is
   taken *)
let syntactic_compute g =
  let o = object (self)
    inherit Visitor.frama_c_inplace

    (* add only-declared functions into the graph *)
    method !vvdec vi =
      try
        let kf = Globals.Functions.get vi in
        if Kernel_function.is_definition kf then Cil.DoChildren
        else begin
          G.add_vertex g kf;
          Cil.SkipChildren
        end
      with Not_found ->
        Cil.SkipChildren

    (* add defined functions into the graph *)
    method !vfunc _f =
      G.add_vertex g (Extlib.the self#current_kf);
      Cil.DoChildren

    (* add edges from callers to callees into the graph *)
    method !vinst = function
    | Call(_, { enode = Lval(Var vi, NoOffset) }, _, _) ->
      (* direct function call *)
      let callee =
        try Globals.Functions.get vi
        with Not_found -> assert false
      in
      let caller = Extlib.the self#current_kf in
      G.add_edge_e g (caller, Extlib.the self#current_stmt, callee);
      Cil.SkipChildren
    | Call _ ->
      (* call via a function pointer: add an edge from each function which
         the address is taken to this callee. *)
      let pointed = get_pointed_kfs () in
      let callee = Extlib.the self#current_kf in
      List.iter
        (fun caller ->
          G.add_edge_e g (caller, Extlib.the self#current_stmt, callee))
        pointed;
      Cil.SkipChildren
    | _ ->
      (* skip childrens for efficiency *)
      Cil.SkipChildren

    (* for efficiency purpose, skip many items *)
    method !vexpr _ = Cil.SkipChildren
    method !vtype _ = Cil.SkipChildren
    method !vannotation _ = Cil.SkipChildren
    method !vcode_annot _ = Cil.SkipChildren
    method !vbehavior _ = Cil.SkipChildren
  end in
  Visitor.visitFramacFileSameGlobals o (Ast.get ());
  (* now remove the potential unrelevant nodes wrt selected options *)
  if not (Options.Uncalled.get () && Options.Uncalled_leaf.get ()) then
    G.iter_vertex
      (fun kf ->
        let has_pred =
          try
            G.iter_pred (fun _ -> raise Exit) g kf;
            false
          with Exit ->
            true
        in
        if not (has_pred (* no caller *) || is_entry_point kf)
        then
          let must_kept =
            Options.Uncalled.get ()  (* uncalled functions must be kept *)
            &&
              (Options.Uncalled_leaf.get () (* uncalled leaf must be kept *)
               || Kernel_function.is_definition kf (* [kf] is a leaf *))
          in
          if not must_kept then G.remove_vertex g kf)
      g

(* complexity = O(number of function calls);
   approximate function pointers as computed by [Value]. *)
let semantic_compute g =
  Globals.Functions.iter
    (fun kf ->
      let callers = !Db.Value.callers kf in
      let must_add =
        callers <> []  (* the function is called *)
        || is_entry_point kf
        ||
          (Options.Uncalled.get () (* uncalled functions must be added *)
           && (Options.Uncalled_leaf.get () (* uncalled leaf must be added *)
               || Kernel_function.is_definition kf) (* [kf] is not a leaf *))
      in
      if must_add then begin
        G.add_vertex g kf;
        List.iter
          (fun (caller, callsites) ->
            List.iter
              (fun stmt -> G.add_edge_e g (caller, stmt, kf)) callsites)
          callers
      end)

let compute () =
  let g = G.create () in
  (* optimize with [Value] when either it is already computed or someone
     requires it anyway *)
  if Dynamic.Parameter.Bool.get "-val" () then begin
    !Db.Value.compute ();
    semantic_compute g
  end else
    (if Db.Value.is_computed () then semantic_compute else syntactic_compute) g;
  g

let get () = State.memo compute
let compute () = ignore (compute ())

module Graphviz_attributes = struct
  include G
  let graph_attributes _ = [ `Ratio (`Float 0.5) ]
  let vertex_name = Kernel_function.get_name
  let vertex_attributes kf =
    [ `Style (if Kernel_function.is_definition kf then `Bold else `Dotted) ]
  let edge_attributes _ = []
  let default_vertex_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
end

let dump () =
  let module GV = Graph.Graphviz.Dot(Graphviz_attributes) in
  let g = get () in
  Options.dump GV.output_graph g

include Journalize.Make
    (struct
      let name = "Cg"
      let dump = dump
      let compute = compute
      type t = G.t
      let ty = D.ty
      let get = get
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
