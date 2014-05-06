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

open Cil_datatype

(* ************************************************************************* *)
(** {2 Is called} *)
(* ************************************************************************* *)

module Is_Called =
  Kernel_function.Make_Table
    (Datatype.Bool)
    (struct
       let name = "is_called"
       let dependencies = [ Db.Value.self ]
       let size = 17
     end)

let is_called =
  Is_Called.memo
    (fun kf ->
       try Db.Value.is_reachable_stmt (Kernel_function.find_first_stmt kf)
       with Kernel_function.No_Statement -> false)

let mark_as_called kf =
    Is_Called.replace kf true

(* ************************************************************************* *)
(** {2 Callers} *)
(* ************************************************************************* *)

module Callers =
  Kernel_function.Make_Table
    (Kernel_function.Map.Make(Stmt.Set))
    (struct
       let name = "Callers"
       let dependencies = [ Db.Value.self ]
       let size = 17
     end)

let add_caller ~caller:(caller_kf, call_site) kf =
  let add m = Kernel_function.Map.add caller_kf (Stmt.Set.singleton call_site) m
  in
  let change m =
    try
      let call_sites = Kernel_function.Map.find caller_kf m in
      Kernel_function.Map.add caller_kf (Stmt.Set.add call_site call_sites) m
    with Not_found ->
      add m
  in
  ignore (Callers.memo ~change (fun _kf -> add Kernel_function.Map.empty) kf)


let callers kf =
  try
    let m = Callers.find kf in
    Kernel_function.Map.fold
      (fun key v acc -> (key, Stmt.Set.elements v) :: acc)
      m
      []
  with Not_found ->
    []

(* ************************************************************************* *)
(** {2 Registration.} *)
(* ************************************************************************* *)

let () =
  Db.Value.is_called := is_called;
  Db.Value.callers := callers;


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
