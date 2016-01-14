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

(* ************************************************************************** *)
(* Topological iterators *)
(* ************************************************************************** *)

module Make
  (G:Graph.Sig.G with type V.t = Kernel_function.t)
  (N:sig val name: string end) =
struct

  (* Topological iterations are memoized in order to improve efficiency when
     calling them several times. This has been proved to have a significant
     impact in practice. *)

  module S =
    State_builder.Queue
      (Kernel_function)
      (struct
        let name = "Callgraph.Uses" ^ N.name
        let dependencies = [ Cg.self ]
       end)

  module T = Graph.Topological.Make_stable(G)

  let iter g f =
    if S.is_empty () then T.iter S.add g;
    S.iter f

end

let iter_in_order =
  let module I = Make(Cg.G)(struct let name = "iter_in_order" end) in
  fun f -> I.iter (Cg.get ()) f

let iter_in_rev_order =
  let module I =
        Make
          (struct
            include Cg.G
            (* inverse operations over successors required by
               [Graph.Topological.G] *)
            let iter_succ = iter_pred
            let in_degree = out_degree
           end)
          (struct let name = "iter_in_rev_order" end)
  in
  fun f -> I.iter (Cg.get ()) f

let iter_on_aux iter_dir f kf =
  let cg = Cg.get () in
  if Cg.G.mem_vertex cg kf then
    let visited = Kernel_function.Hashtbl.create 17 in
    let rec aux kf =
      iter_dir
        (fun kf' ->
          if not (Kernel_function.Hashtbl.mem visited kf') then begin
            f kf';
            Kernel_function.Hashtbl.add visited kf' ();
            aux kf'
          end)
        cg
        kf
    in
    aux kf

let iter_on_callers = iter_on_aux Cg.G.iter_pred
let iter_on_callees = iter_on_aux Cg.G.iter_succ

let is_local_or_formal_of_caller v kf =
  try
    iter_on_callers
      (fun caller ->
        if Base.is_formal_or_local v (Kernel_function.get_definition caller)
        then raise Exit)
      kf;
    false
  with Exit ->
    true

let accept_base ~with_formals ~with_locals kf v =
  let open Cil_types in
  Base.is_global v
  ||
  (match with_formals, with_locals, kf.fundec with
     | false, false, _ | false, _, Declaration _ -> false
     | true,  false, Definition (fundec,_) -> Base.is_formal v fundec
     | false, true, Definition (fundec, _) -> Base.is_local v fundec
     | true,  true, Definition (fundec, _) -> Base.is_formal_or_local v fundec
     | true , _, Declaration (_, vd, _, _) -> Base.is_formal_of_prototype v vd)
  || is_local_or_formal_of_caller v kf

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
