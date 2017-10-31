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

module Make
  (G: sig
    include Graph.Sig.G
    val create: ?size:int -> unit -> t
    val add_edge_e: t -> E.t -> unit
  end)
  (D: Datatype.S with type t = G.t)
  (Info: sig
    val self: State.t
    val name: string
    val get: unit -> G.t
    val vertex: Kernel_function.t -> G.V.t
  end) =
struct

  module S =
    State_builder.Option_ref
      (Datatype.Option(D)) (* none if no root is specified *)
     (struct
        let name = "Subgraph of " ^ Info.name
        let dependencies = [ Info.self; Options.Roots.self ]
       end)

  let self = S.self

  let compute =
    let module HNodes = Hashtbl.Make(G.V) in
    fun () ->
      let g = Info.get () in
      let roots = Options.Roots.get () in
      if Kernel_function.Set.is_empty roots then None
      else
        let subg = G.create () in
        let visited = HNodes.create 17 in
        let rec add_component v =
          (* iter over the connected component of [v] for adding every edge to
             the subgraph *)
          if not (HNodes.mem visited v) then begin
            HNodes.add visited v ();
            G.iter_succ_e
              (fun e ->
                G.add_edge_e subg e;
                add_component (G.E.dst e))
              g
              v
          end
        in
        Kernel_function.Set.iter
          (fun kf -> add_component (Info.vertex kf))
          roots;
        Some subg

  let get () = match S.memo compute with
    | None -> Info.get () (* when no root is specified, use the whole graph *)
    | Some g -> g

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
