(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Utilities over Syntax                                              --- *)
(* -------------------------------------------------------------------------- *)

open Syntax

let rec range = function
  | E_ANY p | E_TRUE p | E_FALSE p -> p
  | E_INT x | E_REAL x | E_PVAR x -> fst x
  | E_BIN(a,_,_,b) | E_IF(a,_,_,b)
  | A_SET(a,_,b) | A_GET(a,b) -> Input.merge (left a) (left b)
  | E_UNA(p,_,a) -> Input.merge p (right a)
  | E_FUN(x,_,es) -> range_list (fst x) es
  | E_LET(x,_,_,_,b) | E_FORALL(x,_,_,_,b) | E_EXISTS(x,_,_,_,b) -> 
      Input.merge (fst x) (right b)
  | E_RECORD(pos,fs) -> range_fields pos fs
  | E_SETFIELD(e,_,fs) -> range_fields (left e) fs
  | E_GETFIELD(e,f,_) -> Input.merge (left e) (fst f)

and range_list p = function
  | [] -> p
  | [e] -> Input.merge p (right e)
  | _::es -> range_list p es

and range_fields p = function
  | [] -> p
  | [_,_,e] -> Input.merge p (right e)
  | _::fes -> range_fields p fes

and left = function
  | E_TRUE p | E_FALSE p | E_ANY p -> p
  | E_INT x | E_REAL x | E_PVAR x 
  | E_FUN(x,_,_) | E_LET(x,_,_,_,_) | E_FORALL(x,_,_,_,_) | E_EXISTS(x,_,_,_,_) 
      -> fst x
  | E_BIN(a,_,_,_) | E_IF(a,_,_,_)  | A_GET(a,_) | A_SET(a,_,_) -> left a
  | E_UNA(p,_,_) | E_RECORD(p,_) -> p
  | E_SETFIELD(e,_,_) | E_GETFIELD(e,_,_) -> left e

and right = function
  | E_TRUE p | E_FALSE p | E_ANY p -> p
  | E_INT x | E_REAL x | E_PVAR x | E_FUN(x,_,[]) -> fst x
  | E_BIN(_,_,_,b) | E_LET(_,_,_,_,b) | E_IF(_,_,_,b)
  | E_FORALL(_,_,_,_,b) | E_EXISTS(_,_,_,_,b) 
  | A_GET(_,b) | A_SET(_,_,b) -> right b
  | E_UNA(p,_,_) -> p
  | E_FUN(_,_,e::es) -> right_list e es
  | E_RECORD(p,fes) -> right_fields p fes
  | E_SETFIELD(e,_,fes) -> right_fields (right e) fes
  | E_GETFIELD(_,f,_) -> fst f

and right_list e = function
  | [] -> right e
  | e::es -> right_list e es

and right_fields p = function
  | [] -> p
  | (_,_,e)::fes -> right_fields (right e) fes

let raise_at e = function
  | Input.SyntaxError _ as err -> raise err
  | err -> raise (Input.locate (range e) err)

let error_at e msg = Input.error_at (range e) msg

let kloc = ref 0
let reset () = kloc := 0
let fresh () = incr kloc ; !kloc
