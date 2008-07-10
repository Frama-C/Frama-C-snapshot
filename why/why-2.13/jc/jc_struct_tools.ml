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

open Jc_name
open Jc_pervasives
open Jc_env
open Jc_envset

(* keep the pointers only and return their tag_or_variant *)
let fields_tov = List.flatten $
  (List.map
     (fun fi -> match fi.jc_field_info_type with
	| JCTpointer(tov, _, _) -> [tov]
	| _ -> []))

let rec all_fields acc = function
  | JCvariant vi | JCunion vi -> acc
  | JCtag ({ jc_struct_info_parent = Some(p, pp) } as st, _) ->
      all_fields (st.jc_struct_info_fields @ acc) (JCtag(p, pp))
  | JCtag ({ jc_struct_info_parent = None } as st, _) ->
      st.jc_struct_info_fields @ acc

let all_fields = all_fields []

let rec all_memories select forbidden acc tov =
  Jc_options.lprintf "  all_memories(%s)@." (tag_or_variant_name tov);
  match tov with
    | JCtag(st, _) as tov ->
	if StringSet.mem st.jc_struct_info_name forbidden then
	  acc
	else
	  let fields = List.filter select (all_fields tov) in
	  (* add the fields to our list *)
	  let acc = List.fold_left
	    (fun acc fi -> StringMap.add (field_memory_name fi) fi acc)
	    acc
	    fields
	  in
	  (* continue recursively on the fields *)
	  let forbidden = StringSet.add st.jc_struct_info_name forbidden in
	  List.fold_left
	    (all_memories select forbidden)
	    acc
	    (fields_tov fields)
    | JCvariant vi | JCunion vi ->
	acc

let all_memories ?(select = fun _ -> true) tov =
  Jc_options.lprintf "all_memories(%s):@." (tag_or_variant_name tov);
  let map = all_memories select StringSet.empty StringMap.empty tov in
  let list = List.rev (StringMap.fold (fun _ ty acc -> ty::acc) map []) in
  Jc_options.lprintf "  Found %n memories.@." (List.length list);
  list

let rec all_types select forbidden acc tov =
  Jc_options.lprintf "  all_types(%s)@." (tag_or_variant_name tov);
  match tov with
    | JCtag(st, _) as tov ->
	if StringSet.mem st.jc_struct_info_name forbidden then
	  acc
	else
	  let vi = struct_variant st in
	  let forbidden = StringSet.add st.jc_struct_info_name forbidden in
	  List.fold_left
	    (all_types select forbidden)
	    (StringMap.add vi.jc_variant_info_name vi acc)
	    (fields_tov (List.filter select (all_fields tov)))
    | JCvariant vi | JCunion vi ->
	StringMap.add vi.jc_variant_info_name vi acc

let all_types ?(select = fun _ -> true) tov =
  Jc_options.lprintf "all_types(%s):@." (tag_or_variant_name tov);
  let map = all_types select StringSet.empty StringMap.empty tov in
  let list = List.rev (StringMap.fold (fun _ ty acc -> ty::acc) map []) in
  Jc_options.lprintf "  Found %n types.@." (List.length list);
  list

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)

