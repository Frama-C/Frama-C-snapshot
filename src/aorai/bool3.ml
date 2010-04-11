(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(* $Id: bool3.ml,v 1.2 2008-10-02 13:33:29 uid588 Exp $ *)


type bool3 =
    | True
    | False
    | Undefined


let bool3and c1 c2 =
  match (c1,c2) with
    | (True,True) -> True

    | (_,False)
    | (False,_) -> False

    | (Undefined,_)
    | (_,Undefined)
      -> Undefined

let bool3or c1 c2 =
  match (c1,c2) with
    | (True,_)
    | (_,True) -> True

    | (_,Undefined)
    | (Undefined,_) -> Undefined

    | (False,False) -> False

let bool3not c =
  match c with
    | True -> False
    | False -> True
    | Undefined -> Undefined





(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
