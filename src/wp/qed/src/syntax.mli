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
(** Qed Abstract Syntax                                                       *)
(* -------------------------------------------------------------------------- *)

type position = {
  p_file : string ;
  p_line : int ;
  p_bol : int ;
  p_start : int ;
  p_stop : int ;
}

type id = position * string

type t =
  | T_INT
  | T_REAL
  | T_BOOL
  | T_PROP
  | T_ALPHA of id
  | T_ARRAY of t * t
  | T_SORT of t list * id
  | T_RECORD of (id * t) list

type binop =
  | ADD | SUB | MUL | DIV | MOD
  | AND | OR | IMPLY | EQUIV
  | EQ | NEQ | LEQ | GEQ
  | LT | GT

type unop =
  | NOT | OPP

type e =
  | E_ANY    of position
  | E_PVAR   of id
  | E_INT    of id
  | E_REAL   of id
  | E_FUN    of id * int * e list
  | E_TRUE   of position
  | E_FALSE  of position
  | E_BIN    of e * position * binop * e
  | E_UNA    of position * unop * e
  | E_LET    of id * int * t option * e * e
  | E_FORALL of id * int * t option * e list * e
  | E_EXISTS of id * int * t option * e list * e
  | E_IF     of e * int * e * e
  | A_GET    of e * e
  | A_SET    of e * e * e
  | E_RECORD   of position * (id * int * e) list
  | E_SETFIELD of e * int * (id * int * e) list
  | E_GETFIELD of e * id * int

type arg = id * int * t option
