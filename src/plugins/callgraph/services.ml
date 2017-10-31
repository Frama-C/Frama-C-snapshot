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

let initial_service_roots cg =
  let roots = Options.Service_roots.get () in
  let roots =
    if Kernel_function.Set.is_empty roots then
      (* if possible, use the main function as initial root *)
      try Kernel_function.Set.singleton (fst (Globals.entry_point ()))
      with Globals.No_such_entry_point _ ->
        (* otherwise use every uncalled function *)
        Cg.G.fold_vertex
          (fun v set ->
            if Cg.G.in_degree cg v = 0 then Kernel_function.Set.add v set
            else set)
          cg
          Kernel_function.Set.empty
    else
      roots
  in
  (* Add the callees of initial roots as roots *)
  Kernel_function.Set.fold
    (fun v set -> Cg.G.fold_succ Kernel_function.Set.add cg v set)
    roots
    roots

(* Intermediate module because of Ocaml:
   "The parameter cannot be eliminated in the result type.
   Please bind the argument to a module identifier." *)
module G_for_S = struct
  let datatype_name = "Callgraph.Cg"
  module V = struct
    include Cg.G.V
    let id = Kernel_function.get_id
    let name = Kernel_function.get_name
    let attributes = Cg.Graphviz_attributes.vertex_attributes
    let entry_point () =
      try Some (fst (Globals.entry_point ()))
      with Globals.No_such_entry_point _ -> None
  end
  include (Cg.G: Graph.Sig.G with module V := V and type t = Cg.G.t)
end

module S = Service_graph.Make(G_for_S)
module G = S.Service_graph

module Graphviz_attributes = S.TP
let entry_point = S.entry_point
let is_root kf = (S.vertex kf).Service_graph.is_root

module State =
  State_builder.Option_ref
    (S.Service_graph.Datatype)
    (struct
      let name = "Callgraph.Services"
      let dependencies = [ Cg.self; Kernel.MainFunction.self ]
     end)

(* eta-expansion required to mask optional argument [?project] *)
let is_computed () = State.is_computed ()
let self = State.self

let compute () =
  let cg = Cg.get () in
  let isr = initial_service_roots cg in
  let isr_names =
    Kernel_function.Set.fold
      (fun kf acc -> Datatype.String.Set.add (Kernel_function.get_name kf) acc)
      isr
      Datatype.String.Set.empty
  in
  let sg = S.compute cg isr_names in
  State.mark_as_computed ();
  sg

let get () = State.memo compute
let compute () = ignore (compute ())

module Subgraph =
  Subgraph.Make
    (G)
    (S.Service_graph.Datatype)
    (struct
      let self = State.self
      let name = State.name
      let get = get
      let vertex = S.vertex
     end)

let dump () =
  let sg = Subgraph.get () in
  Service_graph.frama_c_display false;
  Options.dump S.output_graph sg

include Journalize.Make
    (struct
      let name = "Services"
      let dump = dump
      let compute = compute
      type t = S.Service_graph.t
      let ty = S.Service_graph.Datatype.ty
      let get = get
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
