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

(* $Id: access_path.mli,v 1.6 2008/04/01 09:25:20 uid568 Exp $ *)

(* JS 2007/05/24: TODO: this comment should be remove ??? 
type path = Base.t * (Ival.t list) 

type t = ((path * Ival.t) list) BaseUtils.BaseMap.t

val instanciate_zone : Db.Model.t -> t -> Zone.t -> Zone.t

(* Examples :

instanciate_zone 
   {{ x -> &y ; y -> &a }}     (* concrete state *)
   {{ star_x -> [(x,[0]) , 0 }}   (* paths to logic variables *)
   {{ star_x -> [0..31] }}     (* generic outs *)

result:

   {{ y -> [0..31] }}          (* concrete outs *)


instanciate_zone 
   {{ x -> &y+14 ; y -> &a }}     (* concrete state *)
   {{ logic -> [x,[0]] , 4 ]   (* "x = &logic + 4"   *)
   {{ logic -> [320..351] }}     (* "*(x+6)=...;" *)

result:

   {{ y -> [640..671] }}          (* concrete outs *)


instanciate_zone 
   {{ x[1] -> &y ; y -> &a }}     (* concrete state *)
   [ [x,[32;0]] , &logic , 0 ]   (*    *)
   {{ logic -> [0..31] }}     (*  *)

result:

   {{ a -> [0..31] }}          (* concrete outs *) 
*)
*)

val option: string * Arg.spec * string

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
