(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

open Locations

type t =
    { deps_return : Lmap_bitwise.From_Model.LOffset.t;
      deps_table : Lmap_bitwise.From_Model.t }

let join x y =
  { deps_return =
      Lmap_bitwise.From_Model.LOffset.join x.deps_return y.deps_return ;
    deps_table = Lmap_bitwise.From_Model.join x.deps_table y.deps_table }

let pretty fmt { deps_return = r ; deps_table = t } =
  Format.fprintf fmt "%a@\n\\result %a@\n"
    Lmap_bitwise.From_Model.pretty t
    Lmap_bitwise.From_Model.LOffset.pretty r

(** same as pretty, but uses the type of the function to output more
    precise informations.
    @raise Error if the given type is not a function type
 *)
let pretty_with_type typ fmt { deps_return = r; deps_table = t } =
  let (rt_typ,_,_,_) = Cil.splitFunctionType typ in
  if Cil.isVoidType rt_typ then
    Format.fprintf fmt "@[<v>@[@;<2 0>@[%a@]@]@]@\n"
      Lmap_bitwise.From_Model.pretty t
  else if Lmap_bitwise.From_Model.LOffset.is_empty r then
    Format.fprintf fmt "@[<v>@[@;<2 0>@[%a@]\\result FROM \\nothing@]@]@\n"
      Lmap_bitwise.From_Model.pretty t
  else
    Format.fprintf fmt "@[<v>@[@;<2 0>@[%a@]\\result%a@]@]@\n"
      Lmap_bitwise.From_Model.pretty t
      (Lmap_bitwise.From_Model.LOffset.pretty_with_type (Some rt_typ)) r

let hash { deps_return = dr ; deps_table = dt } =
  Lmap_bitwise.From_Model.hash dt + 197*Lmap_bitwise.From_Model.LOffset.tag dr

let equal
    { deps_return = dr ; deps_table = dt }
    { deps_return = dr' ; deps_table = dt' } =
  Lmap_bitwise.From_Model.equal dt dt'
  && Lmap_bitwise.From_Model.LOffset.equal dr dr'

module Datatype =
  Project.Datatype.Register
    (struct
       type tt = t
       type t = tt
       open Lmap_bitwise
       let copy _ = assert false (* TODO: deep copy *)
       let descr =
	 Unmarshal.t_record
	   [| Lmap_bitwise.From_Model.LOffset.Datatype.descr;
	      Lmap_bitwise.From_Model.Datatype.descr |]
       let name = "function_froms"
     end)
let () = Datatype.register_comparable ~equal ~hash ()
