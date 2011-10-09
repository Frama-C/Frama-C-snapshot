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

open Cil_types
open Cil

module M = Cil_datatype.Varinfo.Map

type t = exp M.t

let empty = M.empty

let add = M.add

let remove = M.remove

let expr ?(trans=true) e subst =
  let modified = ref false in
  let rec expr e =
    let visitor = object
      inherit nopCilVisitor
      method vexpr e = match e.enode with
      | Lval((Var x, NoOffset)) ->
          (try
             let e = M.find x subst in
             modified := true;
             let e = if trans then expr e else e in
             ChangeTo e
           with Not_found ->
             SkipChildren)
      | _ ->
          DoChildren
    end
    in
    visitCilExpr visitor e
  in
  let e = expr e in
  e, !modified

let lval ?trans x = expr ?trans (new_exp ~loc:Cil_datatype.Location.unknown (Lval x))

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
