(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* $Id: why_output.ml,v 1.17 2008/04/01 09:25:21 uid568 Exp $ *)

open Fol
open Format
open Cilutil

let to_string f x = Pretty.sprint ~width:80 (f () x)

let rec pure_type fmt = function
  | PTint -> fprintf fmt "int"
  | PTbool -> fprintf fmt "bool"
  | PTunit -> fprintf fmt "unit"
  | PTreal -> fprintf fmt "real"
  | PTexternal([],id) -> fprintf fmt "%s" id
  | PTvar s -> fprintf fmt "'%s" s
  | PTexternal([t],id) ->
      fprintf fmt "%a %s" pure_type t id
  | PTexternal(l,id) -> fprintf fmt "(%a) %s"
      (print_list space pure_type) l id

let constant fmt = function
  | ConstInt n ->
      fprintf fmt "%s" n
  | ConstBool b ->
      fprintf fmt "%b" b
  | ConstUnit ->
      fprintf fmt "void"
  | ConstFloat f ->
      fprintf fmt "%s" f

let rec term fmt = function
  | Tconst c ->
      constant fmt c
  | Tvar id ->
      fprintf fmt "%s" id
  | Tapp (id, []) ->
      fprintf fmt "%s" id
  | Tapp (id, tl) ->
      fprintf fmt "%s(%a)" id (print_list comma term) tl
  | Tif (c,t,e) -> fprintf fmt "if %a then %a else %a" term c term t term e

let rec predicate fmt = function
  | Pvar id | Papp (id, [])->
      fprintf fmt "%s" id
  | Papp ("eq", [t1; t2]) ->
      fprintf fmt "(%a =@ %a)" term t1 term t2
  | Papp ("neq", [t1; t2]) ->
      fprintf fmt "(%a <>@ %a)" term t1 term t2
  | Papp (id, l) ->
      fprintf fmt "%s(%a)" id (print_list comma term) l
  | Ptrue ->
      fprintf fmt "true"
  | Pfalse ->
      fprintf fmt "false"
  | Pimplies (a, b) ->
      fprintf fmt "(@[%a ->@ %a@])" predicate a predicate b
  | Piff (a, b) ->
      fprintf fmt "(@[%a <->@ %a@])" predicate a predicate b
  | Pif (a, b, c) ->
      fprintf fmt "(@[if %a then@ %a else@ %a@])"
	term a predicate b predicate c
  | Pand (a, b) ->
      fprintf fmt "(@[%a and@ %a@])" predicate a predicate b
  | Por (a, b) ->
      fprintf fmt "(@[%a or@ %a@])" predicate a predicate b
  | Pxor (_a,_b) -> assert false (* writeme *)
  | Pnot a ->
      fprintf fmt "(not %a)" predicate a
  | Pforall (b,v,p) ->
      fprintf fmt "@[<hov 2>(forall %s:%a.@ %a)@]"
	b pure_type v predicate p
  | Pexists (b,v,p) ->
      fprintf fmt "@[<hov 2>(exists %s:%a.@ %a)@]"
	b pure_type v predicate p
  | Pnamed (n, p) ->
      fprintf fmt "@[%s: %a@]" n predicate p

let decl fmt = function
  | Function (s, tl, t) ->
      fprintf fmt "logic %s: %a -> %a@\n"
	s (print_list comma pure_type) tl pure_type t
  | Predicate (s, tl) ->
      fprintf fmt "logic %s: %a -> prop@\n"
	s (print_list comma pure_type) tl
  | Axiom (s, p) ->
      fprintf fmt "axiom %s: %a@\n" s predicate p
  | Goal (s, p) ->
      fprintf fmt "@[<hov 2>goal %s: %a@]@\n" s predicate p
  | Type (t) ->
      fprintf fmt "@[type %a@]@\n" pure_type t

let output ?prelude ~file p =
  let c = open_out file in
  begin match prelude with
    | Some f ->
	let cin = open_in f in
	begin try
	  while true do output_char c (input_char cin) done
	with End_of_file ->
	  close_in cin
	end
    | None -> ()
  end;
  let fmt = formatter_of_out_channel c in
  fprintf fmt "@[%a@]@." (print_list newline decl) p;
  close_out c



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
