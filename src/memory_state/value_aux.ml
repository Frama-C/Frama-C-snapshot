(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

type call_site = kernel_function * kinstr

module Callsite =
  Datatype.Pair_with_collections(Kernel_function)(Cil_datatype.Kinstr)
    (struct let module_name = "Value_callbacks.Callpoint" end)


type callstack = call_site list

module Callstack = Datatype.With_collections
  (Datatype.List(Callsite))
  (struct let module_name = "Value_aux.Callstack" end)

type 'a callback_result =
  | Normal of 'a
  | NormalStore of 'a * int
  | Reuse of int



let iter_on_callers f kf =
  let tf = Datatype.func Kernel_function.ty Datatype.Unit.ty in
  let d = Dynamic.get ~plugin:"Semantic_callgraph" "iter_on_callers"
    (Datatype.func tf tf) in
  d f kf


let is_local_or_formal_of_caller v kf =
  try
    iter_on_callers
      (fun caller ->
         let formal_or_local =
           (Base.is_formal_or_local v (Kernel_function.get_definition caller))
         in
         if formal_or_local then raise Exit)
      kf;
    false
  with Exit -> true


let accept_base ~with_formals ~with_locals kf v =
  Base.is_global v
  ||
  (match with_formals, with_locals, kf.fundec with
     | false, false, _ -> false
     | true,  false, Definition (fundec,_) -> Base.is_formal v fundec
     | false, true, Definition (fundec, _) -> Base.is_local v fundec
     | true,  true, Definition (fundec, _) -> Base.is_formal_or_local v fundec
     | false, _, Declaration _ -> false
     | true , _, Declaration (_, vd, _, _) -> Base.is_formal_of_prototype v vd
  )
  || is_local_or_formal_of_caller v kf


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

