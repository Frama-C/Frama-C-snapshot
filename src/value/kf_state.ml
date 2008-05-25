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

(* $Id: kf_state.ml,v 1.18 2008/04/01 09:25:22 uid568 Exp $ *)

open Db
open Cil_types
open Cilutil

(* ************************************************************************* *)
(** {2 Is called} *)
(* ************************************************************************* *)

module Is_Called = 
  Kernel_function.Make_Table
    (Datatype.Bool)
    (struct 
       let name = Project.Computation.Name.make "is_called" 
       let dependencies = [ Value.self ]
       let size = 17
     end)

let is_called =
  Is_Called.memo
    (fun kf -> 
       try Value.is_accessible (Kstmt (Kernel_function.find_first_stmt kf)) 
       with Kernel_function.No_Statement -> false)

let mark_as_called kf = Is_Called.add kf true

(* ************************************************************************* *)
(** {2 Callers} *)
(* ************************************************************************* *)

module KernelFunctionMap = Map.Make(Kernel_function)

module Callers = 
  Kernel_function.Make_Table
    (Datatype.Make_Map
       (KernelFunctionMap)
       (struct 
	  include Kernel_datatype.StmtSet 
	  let compare = Cilutil.StmtSet.compare 
	end))
    (struct 
       let name = Project.Computation.Name.make "Callers"
       let dependencies = [ Value.self ]
       let size = 17
     end)

let add_caller ~caller:(caller_kf,call_site) kf =
  let add m =  KernelFunctionMap.add caller_kf (StmtSet.singleton call_site) m
  in 
  let change m = 
    try 
      let call_sites = KernelFunctionMap.find caller_kf m in
      KernelFunctionMap.add caller_kf (StmtSet.add call_site call_sites) m
    with Not_found -> add m
  in
  ignore (Callers.memo ~change 
            (fun _kf -> 
               add KernelFunctionMap.empty) 
            kf)


let callers kf = 
  try 
    let m = Callers.find kf in
    KernelFunctionMap.fold 
      (fun key v acc  -> 
         (key,StmtSet.elements v)::acc) m [] 
  with Not_found -> []

(* ************************************************************************* *)
(** {2 Never terminates} *)
(* ************************************************************************* *)

module Never_Terminates = 
  Kernel_function.Make_Table
    (Datatype.Bool)
    (struct 
       let name = Project.Computation.Name.make "Never_Terminates" 
       let size = 17
       let dependencies = [ Value.self ]
     end)

let never_terminates kf = 
  try 
    Never_Terminates.find kf 
  with Not_found ->
    assert (not (is_called kf));
    false

let mark_as_terminates kf = Never_Terminates.add kf false

let mark_as_never_terminates kf =
  let noreturn = 
    Cil.hasAttribute "noreturn" (Kernel_function.get_vi kf).vattr 
  in
  try
    if not (Never_Terminates.find kf || noreturn) then
      (* Function marked with "terminates" and has no attribute "noreturn" *)
      CilE.warn_once "one non terminating branch in function %a" 
	Kernel_function.pretty_name kf
  with Not_found ->
    (* Function never marked *)
    if not noreturn then
      CilE.warn_once "non termination detected in function %a" 
	Kernel_function.pretty_name kf;
    Never_Terminates.add kf true

(* ************************************************************************* *)
(** {2 Registration.} *)
(* ************************************************************************* *)

let () = 
  Value.is_called := is_called;
  Value.callers := callers;
  Value.never_terminates := never_terminates;

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
