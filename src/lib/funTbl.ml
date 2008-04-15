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

(* $Id: funTbl.ml,v 1.4 2008/08/25 11:38:48 uid568 Exp $ *)

type t = (string, int  * Obj.t) Hashtbl.t

exception Not_Registered of string 
exception Incompatible_Type of string
exception AlreadyExists of string
  
let create = Hashtbl.create

let register tbl name sign f =
  if Hashtbl.mem tbl name then raise (AlreadyExists name); 
  Hashtbl.add tbl name (Type.id sign, Obj.repr f)
    
let get tbl name =
  try Hashtbl.find tbl name
  with Not_found -> raise (Not_Registered name)

let unsafe_apply tbl name = Obj.obj (snd (get tbl name))

let error_type name id id' =
  Format.sprintf 
    "%s has type %s but is used with type %s" 
    name (Type.name_of_id id') (Type.name_of_id id)

let apply tbl name sign =
  let id', f = get tbl name in
  let id = Type.id sign in
  if id <> id' then raise (Incompatible_Type (error_type name id id'));
  Obj.obj f

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
