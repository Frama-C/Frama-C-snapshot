(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Locations
open Ast_printer
open Value_util

let remember_bases_with_locals bases_containing_locals left_loc evaled_exp =
  if Cvalue.V.contains_addresses_of_any_locals evaled_exp then
    let clobbered_set = Location_Bits.get_bases left_loc.loc  in
    bases_containing_locals :=
      Location_Bits.Top_Param.join clobbered_set !bases_containing_locals

let remember_bases_with_locals_offsetmap bases_containing_locals left_loc offm size =
  try
    Cvalue.V_Offsetmap.iter_contents
      (fun v ->
        if Cvalue.V.contains_addresses_of_any_locals
          (Cvalue.V_Or_Uninitialized.get_v v)
        then
          let clobbered_set = Location_Bits.get_bases left_loc.loc  in
          bases_containing_locals :=
            Location_Bits.Top_Param.join clobbered_set !bases_containing_locals
      ) offm size
  with Exit -> ()


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


let offsetmap_top_addresses_of_locals ~exact is_local =
  let is_local_bytes = Location_Bytes.contains_addresses_of_locals is_local in
  fun offsetmap ->
    if Cvalue.V_Offsetmap.is_empty offsetmap
    then Location_Bytes.Top_Param.top, offsetmap
    else
      let loc_contains_addresses_of_locals t =
	let v = Cvalue.V_Or_Uninitialized.get_v t in
	is_local_bytes v
      in
      let locals, result =
	Cvalue.V_Offsetmap.top_stuff
	  loc_contains_addresses_of_locals
	  (fun v ->
	    Cvalue.V_Or_Uninitialized.unspecify_escaping_locals ~exact
	      is_local v)
	  Location_Bytes.Top_Param.join
	  Location_Bytes.Top_Param.bottom
	  offsetmap
      in
      locals, result

let state_top_addresses_of_locals ~exact warn_escape offsetmap_top_addresses_of_locals bases =
  let ff k offsm acc =
    let locals, r = offsetmap_top_addresses_of_locals ~exact offsm in
    let found_locals = not (Cvalue.V_Offsetmap.equal r offsm) in
    if found_locals then warn_escape k locals;
    Cvalue.Model.add_offsetmap k r acc
  in
  (fun (state:Cvalue.Model.t) ->
    let f base acc =
      try
	let offset_to_clean = Cvalue.Model.find_base base state
	in
	ff base offset_to_clean acc
      with Not_found -> acc
    in
    try
      (Location_Bits.Top_Param.fold
	  f
	  bases
	  (f Base.null state))
    with Location_Bits.Top_Param.Error_Top ->
      begin
	let result =
	  try
	    (Cvalue.Model.fold_base_offsetmap
		ff
		state
		Cvalue.Model.empty_map)
	  with Cvalue.Model.Error_Bottom -> Cvalue.Model.bottom
	in
	result
      end)
