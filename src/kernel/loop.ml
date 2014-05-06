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

open Cil_types
open Cil_datatype

let dkey = Kernel.register_category "natural_loops"

module Natural_Loops =
  Kernel_function.Make_Table
    (Stmt.Map.Make(Datatype.List(Stmt)))
    (struct
       let name = "natural_loops"
       let size = 97
       let dependencies = [ Ast.self ]
     end)

let pretty_natural_loops fmt loops =
  Stmt.Map.iter
    (fun start members ->
       Format.fprintf fmt "Loop start: %d <- ( " start.sid;
       List.iter (fun d -> Format.fprintf fmt "%d " d.sid) members;
       Format.fprintf fmt ")@\n";)
    loops

(** Compute the start of the natural loops of the fonction. For each start,
    we also return the origins of the back edges. *)
let findNaturalLoops (f: fundec) =
  let loops =
    List.fold_left
      (fun acc b ->
        (* Iterate over all successors, and see if they are among the
           dominators for this block. Such a successor [s] is a natural loop,
           and [b -> s] is a back-edge. *)
        List.fold_left
          (fun acc s ->
            if Dominators.dominates s b then
              let cur =
                try Stmt.Map.find s acc
                with Not_found -> []
              in
              Stmt.Map.add s (b :: cur) acc
            else
              acc)
          acc
          b.succs)
      Stmt.Map.empty
      f.sallstmts
  in
  Kernel.debug ~dkey "Natural loops:\n%a" pretty_natural_loops loops;
  loops

let get_naturals kf =
  let loops =
    Natural_Loops.memo
      (fun kf ->
         match kf.fundec with
         | Declaration _ ->
             Stmt.Map.empty
         | Definition (cilfundec,_) ->
             Kernel.debug ~dkey "Compute natural loops for '%a'"
               Kernel_function.pretty kf;
             let naturals = findNaturalLoops cilfundec  in
             Kernel.debug ~dkey
               "Done computing natural loops for '%a':@.%a"
               Kernel_function.pretty kf
               pretty_natural_loops naturals;
             naturals
      )
      kf
  in
  loops

let is_natural kf =
  let loops = get_naturals kf in
  fun s -> Stmt.Map.mem s loops

let back_edges kf stmt =
  try Stmt.Map.find stmt (get_naturals kf)
  with Not_found -> []


let get_non_naturals kf =
  let visited = Stmt.Hashtbl.create 17 in
  let current = Stmt.Hashtbl.create 17 in
  let res = ref Stmt.Set.empty in
  let is_natural = is_natural kf in
  let rec aux s =
    if Stmt.Hashtbl.mem visited s then begin
      if Stmt.Hashtbl.mem current s &&  not (is_natural s) then begin
        res := Stmt.Set.add s !res;
        Kernel.warning ~once:true ~source:(fst (Cil_datatype.Stmt.loc s))
          "Non-natural loop detected."
      end
    end
    else begin
      Stmt.Hashtbl.add visited s ();
      Stmt.Hashtbl.add current s ();
      List.iter aux s.Cil_types.succs;
      Stmt.Hashtbl.remove current s;
    end
  in
  aux (Kernel_function.find_first_stmt kf);
  !res

module Non_Natural_Loops =
  Kernel_function.Make_Table
    (Stmt.Set)
    (struct
       let name = "Loop.non_natural_loops"
       let size = 37
       let dependencies = [ Ast.self ]
     end)
let get_non_naturals = Non_Natural_Loops.memo get_non_naturals

let is_non_natural kf s = Stmt.Set.mem s (get_non_naturals kf)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
