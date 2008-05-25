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

(* $Id: subst.ml,v 1.6 2008/04/01 09:25:21 uid568 Exp $ *)

open Cil_types
open Cil

module M = 
  Map.Make(struct type t = varinfo let compare x y = compare x.vid y.vid end)

type t = exp M.t

let empty = M.empty

let add = M.add

let remove = M.remove

let expr ?(trans=true) e subst =
  let modified = ref false in
  let rec expr e =
    let visitor = object
      inherit nopCilVisitor
      method vexpr = function
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

let lval ?trans x = expr ?trans (Lval x)
