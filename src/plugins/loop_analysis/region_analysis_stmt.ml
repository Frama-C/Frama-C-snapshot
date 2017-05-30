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

open Region_analysis

module type M = sig
  val kf: Kernel_function.t

  open Cil_types
  type abstract_value
  val compile_node: stmt -> abstract_value -> (stmt edge * abstract_value) list
  val mu: (abstract_value -> abstract_value) -> abstract_value -> abstract_value
  val join: abstract_value list -> abstract_value
end

(* Helper function to make region analysis on Frama-C stmts. *)
module MakeNode(M:M):Node with type abstract_value = M.abstract_value
                           and type node = Cil_types.stmt =
struct

  include M
  type node = Cil_types.stmt
  let pretty = Cil_datatype.Stmt.pretty
  let equal = Cil_datatype.Stmt.equal
  module Set = Cil_datatype.Stmt.Set;;

  module Graph = struct

    let entry_node = Kernel_function.find_first_stmt M.kf
    let all_nodes =
      (* DFS from the entry node. *)
      let visited = ref Set.empty in
      let rec loop v =
        visited := Set.add v !visited;
        List.iter (fun s ->
            if not (Set.mem s !visited)
            then loop s) v.Cil_types.succs
      in loop entry_node; !visited
    ;;
    
    let exit_nodes =
      let ret = Kernel_function.find_return M.kf in
      if Set.mem ret all_nodes then [ret] else [];;

    let iter_preds n f = List.iter f n.Cil_types.preds
    let iter_succs n f = List.iter f n.Cil_types.succs
  end

  module Dict = struct
    open Cil_datatype.Stmt

    type 'a t = 'a Hashtbl.t * 'a
    let get (hash,default) x =
      try Hashtbl.find hash x
      with Not_found -> default
    ;;
    let set (hash,_) x value = Hashtbl.replace hash x value;;
    let iter (hash,_) f = Hashtbl.iter f hash;;
    let create size default = (Hashtbl.create size, default)
    let copy (h1,def1) = (Hashtbl.copy h1, def1)
  end

  module Edge_Dict = struct
    module Edge_Hash = Hashtbl.Make(struct
        type t = node edge
        let equal a b = match a,b with
          | Exit n1, Exit n2 -> equal n1 n2
          | Edge(from1,to1), Edge(from2,to2) -> equal from1 from2 && equal to1 to2
          | _ -> false

        let hash = function
          | Exit n -> n.Cil_types.sid
          | Edge(from,to_) -> 997 * from.Cil_types.sid + to_.Cil_types.sid
      end)
    let get = Edge_Hash.find
    let set = Edge_Hash.replace
    let create () = Edge_Hash.create 17
    let iter m f = Edge_Hash.iter f m
    type 'a t = 'a Edge_Hash.t
  end

  module DomTree = struct
    let dominates = Dominators.dominates
    let domtree_postfix_iter f =

      (* Reverse the normal domtree. *)
      let dom_tree = Hashtbl.create 17 in

      let find_or_empty x =
        try Hashtbl.find dom_tree x
        with Not_found -> [] in

      Set.iter (fun x ->
          match Dominators.get_idom x with
          | None -> ()
          | Some idom -> Hashtbl.replace dom_tree idom (x::(find_or_empty idom)))
        Graph.all_nodes;

      let rec traverse x =
        let children = find_or_empty x in
        List.iter traverse children;
        f x

      in traverse Graph.entry_node
    ;;
  end


end
