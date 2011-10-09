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

(** Value analysis of statements and functions bodies *)

open Cil_types
open Cil
open Cil_datatype
open Locations
open Abstract_interp
open Bit_utils
open Cvalue
open Ast_printer
open Value_util
open Eval_exprs
open Eval_logic

let remember_bases_with_locals bases_containing_locals left_loc evaled_exp =
  if Cvalue.V.contains_addresses_of_any_locals evaled_exp then
    let clobbered_set = Location_Bits.get_bases left_loc.loc  in
    bases_containing_locals :=
      Location_Bits.Top_Param.join clobbered_set !bases_containing_locals

let warn_locals_escape is_block fundec k locals =
  let pretty_base = Base.pretty in
  let pretty_block fmt = Pretty_utils.pp_cond is_block fmt "a block of " in
  let sv = fundec.svar in
  match locals with
    Location_Bytes.Top_Param.Top ->
      warning_once_current
        "locals escaping the scope of %t%a through %a"
        pretty_block
        !d_var sv
        pretty_base k
  | Location_Bytes.Top_Param.Set _ ->
      warning_once_current
        "locals %a escaping the scope of %t%a through %a"
        Location_Bytes.Top_Param.pretty locals
        pretty_block
        !d_var sv
        pretty_base k

let warn_locals_escape_result fundec locals =
  let d_var = !d_var in
  let sv = fundec.svar in
  match locals with
    Location_Bytes.Top_Param.Top ->
      warning_once_current
        "locals escaping the scope of %a through \\result"
        d_var sv
  | Location_Bytes.Top_Param.Set _ ->
      warning_once_current
        "locals %a escaping the scope of %a through \\result"
        Location_Bytes.Top_Param.pretty locals
        d_var sv
