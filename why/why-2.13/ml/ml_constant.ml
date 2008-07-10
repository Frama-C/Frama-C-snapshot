(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of Ocaml constants to Jessie *)

open Ml_misc
open Ml_ocaml.Asttypes
open Jc_ast
open Jc_env

let constant = function
  | Const_int i -> JCCinteger(string_of_int i)
  | Const_float s -> JCCreal s
  | _ -> not_implemented Ml_ocaml.Location.none "ml_constant.ml: constant"

let constant_type = function
  | Const_int _ -> JCTnative Tinteger
  | Const_float _ -> JCTnative Treal
  | _ -> not_implemented Ml_ocaml.Location.none "ml_constant.ml: constant_type"

let constant_term c =
  make_term (JCTconst (constant c)) (constant_type c)

let constant_expr c =
  make_expr (JCTEconst (constant c)) (constant_type c)

(*
Local Variables: 
compile-command: "unset LANG; make -j -C .. bin/jessica.opt"
End: 
*)
