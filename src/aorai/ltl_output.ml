(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

open Format open Pervasives
open Ltlast

let out_fmt=ref (formatter_of_out_channel stdout)

let rec ltl_form_to_string = function
    | LNext (f) ->
        "X("^(ltl_form_to_string f)^")"
    | LUntil (f1,f2) ->
        "("^(ltl_form_to_string f1)^" U "^(ltl_form_to_string f2)^")"
    | LFatally (f) ->
        "<>("^(ltl_form_to_string f)^")"
    | LGlobally (f) ->
        "[]("^(ltl_form_to_string f)^")"
    | LRelease  (f1,f2) ->
        "("^(ltl_form_to_string f1)^" V "^(ltl_form_to_string f2)^")"

    | LNot (f) ->
        "!("^(ltl_form_to_string f)^")"
    | LAnd (f1,f2) ->
        "("^(ltl_form_to_string f1)^" && "^(ltl_form_to_string f2)^")"
    | LOr  (f1,f2) ->
        "("^(ltl_form_to_string f1)^" || "^(ltl_form_to_string f2)^")"
    | LImplies  (f1,f2) ->
        "("^(ltl_form_to_string f1)^" -> "^(ltl_form_to_string f2)^")"
    | LIff (f1,f2) ->
        "("^(ltl_form_to_string f1)^" <-> "^(ltl_form_to_string f2)^")"

    | LTrue ->
        "1"
    | LFalse ->
        "0"

    | LCall (s) ->
        "callof_"^s
    | LReturn (s) ->
        "returnof_"^s
    | LCallOrReturn (s) ->
        "callorreturnof_"^s

    | LIdent (s) ->
        s

let output ltl_form file =
  let c = open_out file in
    out_fmt:=formatter_of_out_channel c ;
    fprintf !out_fmt "%s\n\n" (ltl_form_to_string ltl_form); 
    fprintf !out_fmt "@?"; (* Flush du flux *)
    close_out c;
    out_fmt:=formatter_of_out_channel stdout

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
