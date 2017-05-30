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

(* ************************************************************************** *)
(** {2 Generic code} *)
(* ************************************************************************** *)

let apply_rte f x =
  let signed = Kernel.SignedOverflow.get () in
  let unsigned = Kernel.UnsignedOverflow.get () in
  Kernel.SignedOverflow.off ();
  Kernel.UnsignedOverflow.off ();
  let finally () =
    Kernel.SignedOverflow.set signed;
    Kernel.UnsignedOverflow.set unsigned
  in
  Extlib.try_finally ~finally f x

let warn_rte warn exn =
  if warn then
    Options.warning "@[@[cannot run RTE:@ %s.@]@ \
Ignoring potential runtime errors in annotations." 
      (Printexc.to_string exn)

(* ************************************************************************** *)
(** {2 Exported code} *)
(* ************************************************************************** *)

open Cil_datatype

let stmt ?(warn=true) =
  try 
    let f = 
      Dynamic.get
	~plugin:"RteGen" 
	"stmt_annotations" 
	(Datatype.func2 Kernel_function.ty Stmt.ty 
	   (let module L = Datatype.List(Code_annotation) in L.ty))
    in
    (fun x y -> apply_rte (f x) y)
  with Failure _ | Dynamic.Unbound_value _ | Dynamic.Incompatible_type _ as exn
    ->
      warn_rte warn exn;
      fun _ _ -> []

let exp ?(warn=true) =
  try 
    let f = 
      Dynamic.get
	~plugin:"RteGen" 
	"exp_annotations" 
	(Datatype.func3 Kernel_function.ty Stmt.ty Exp.ty 
	   (let module L = Datatype.List(Code_annotation) in L.ty))
    in
    (fun x y z -> apply_rte (f x y) z)
  with Failure _ | Dynamic.Unbound_value _ | Dynamic.Incompatible_type _ as exn
    ->
      warn_rte warn exn;
      fun _ _ _ -> []

(*
Local Variables:
compile-command: "make"
End:
*)
