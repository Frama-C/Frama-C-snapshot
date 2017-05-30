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

exception Typing_error of string
let untypable s = raise (Typing_error s)

exception Not_yet of string
let not_yet s = raise (Not_yet s)

module Nb_typing = 
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "E_ACSL.Error.Nb_typing"
      let default () = 0
      let dependencies = [ Ast.self ]
     end)

let nb_untypable = Nb_typing.get

module Nb_not_yet = 
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "E_ACSL.Error.Nb_not_yet"
      let default () = 0
      let dependencies = [ Ast.self ]
     end)

let nb_not_yet = Nb_not_yet.get

let generic_handle f res x =
  try
    f x
  with
  | Typing_error s ->
    let msg = Format.sprintf "@[invalid E-ACSL construct@ `%s'.@]" s in
    Options.warning ~once:true ~current:true "@[%s@ Ignoring annotation.@]" msg;
    Nb_typing.set (Nb_typing.get () + 1);
    res
  | Not_yet s ->
    let msg = 
      Format.sprintf "@[E-ACSL construct@ `%s'@ is not yet supported.@]" s
    in 
    Options.warning ~once:true ~current:true "@[%s@ Ignoring annotation.@]" msg;
    Nb_not_yet.set (Nb_not_yet.get () + 1);
    res

let handle f x = generic_handle f x x

(*
Local Variables:
compile-command: "make"
End:
*)
