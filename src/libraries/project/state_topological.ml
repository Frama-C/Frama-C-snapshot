(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for ocaml                         *)
(*  Copyright (C) 2004-2012                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliâtre and Julien Signoles        *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, as published by the Free Software Foundation.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU Library General Public License version 2.1 for more       *)
(*  details (enclosed in the file licenses/LGPLv2.1).                     *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

module type G = sig
  type t
  val iter_vertex : (State.t -> unit) -> t -> unit
  val iter_succ : (State.t -> unit) -> t -> State.t -> unit
  val in_degree : t -> State.t -> int
end

module Make(G: G) = struct

  module H = State.Hashtbl

  let fold f g acc =
    let degree = H.create 997 in
    let todo = Queue.create () in
    let push x =
      H.remove degree x;
      Queue.push x todo
    in
    let rec walk acc =
      if Queue.is_empty todo then
        (* let's find any node of minimal degree *)
        let min =
          H.fold
            (fun v d acc ->
               match acc with
               | None -> Some (v, d)
               | Some(_, min) -> if d < min then Some (v, d) else acc)
            degree
            None
        in
        match min with
        | None -> acc
        | Some(v, _) -> push v; walk acc
      else
        let v = Queue.pop todo in
        let acc = f v acc in
        G.iter_succ
          (fun x->
             try
               let d = H.find degree x in
               if d = 1 then push x else H.replace degree x (d-1)
             with Not_found ->
               (* [x] already visited *)
               ())
          g v;
        walk acc
    in
    G.iter_vertex
      (fun v ->
         let d = G.in_degree g v in
         if d = 0 then Queue.push v todo
         else H.add degree v d)
      g;
    walk acc

  let iter f g = fold (fun v () -> f v) g ()

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
