(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Format

type t = { l : Locations.location ; offset : Ival.t }

let bottom = { l = Locations.loc_bottom ; offset = Ival.bottom }

let make l d =
  if Locations.loc_equal l Locations.loc_bottom ||
    Ival.is_bottom d
  then bottom
  else { l = l ; offset = d }

let equal ls1 ls2 =
  Locations.loc_equal ls1.l ls2.l&& Ival.equal ls1.offset ls2.offset

let is_bottom ls =
  Locations.loc_equal ls.l Locations.loc_bottom

let pretty fmt ls =
  fprintf fmt "%a+%a" Locations.pretty ls.l Ival.pretty ls.offset

let is_included ls1 ls2 =
  let l1 = ls1.l in
  let l2 = ls2.l in
  Int_Base.equal l1.size l2.size &&
    Location_Bits.is_included l1.loc l2.loc &&
    Ival.is_included ls1.offset ls2.offset

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
