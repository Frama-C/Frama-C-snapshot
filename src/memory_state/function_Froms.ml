(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Locations

module Deps = 
struct

  type from_deps = {
    data: Zone.t;
    indirect: Zone.t;
  }

  let subst_from_deps f fd = {
    data = f fd.data;
    indirect = f fd.indirect;
  }

  module DatatypeFromDeps = Datatype.Make(struct
    type t = from_deps

    let name = "Function_Froms.Deps.deps_assigned"

    let pretty fmt {data; indirect} = 
      let bottom_data = Zone.is_bottom data in
      let bottom_indirect = Zone.is_bottom indirect in
      match bottom_indirect, bottom_data with
      | true, true ->
        Format.fprintf fmt "\\nothing"
      | true, false ->
        Format.fprintf fmt "direct: %a"
	  Zone.pretty data
      | false, true ->
        Format.fprintf fmt "indirect: %a"
	  Zone.pretty indirect
      | false, false ->
        Format.fprintf fmt "indirect: %a; direct: %a"
	  Zone.pretty indirect
	  Zone.pretty data

    let hash fd =
      Zone.hash fd.data + 37 * Zone.hash fd.indirect

    let compare fd1 fd2 =
      let c = Zone.compare fd1.data fd2.data in
      if c <> 0 then c
      else Zone.compare fd1.indirect fd2.indirect

    let equal = Datatype.from_compare

    let reprs =
      List.map (fun z -> {data = z; indirect = z}) Zone.reprs

    let structural_descr =
      Structural_descr.t_record [| Zone.packed_descr; Zone.packed_descr; |]
    let rehash = Datatype.identity

    let mem_project = Datatype.never_any_project
    let varname _ = "da"

    let internal_pretty_code = Datatype.undefined
    let copy = Datatype.undefined
  end)

  let data_deps z = { data = z; indirect = Zone.bottom }
  let indirect_deps z = { data = Zone.bottom; indirect = z }

  include DatatypeFromDeps

  let bottom = {
    data = Zone.bottom;
    indirect = Zone.bottom;
  }

  let top = {
    data = Zone.top;
    indirect = Zone.top;
  }

  let is_included fd1 fd2 =
    Zone.is_included fd1.data fd2.data &&
    Zone.is_included fd1.indirect fd2.indirect

  let join fd1 fd2 = {
    data = Zone.join fd1.data fd2.data;
    indirect = Zone.join fd1.indirect fd2.indirect
  }

  let join_and_is_included fd1 fd2 =
    let fd12 = join fd1 fd2 in
    (fd12, equal fd12 fd2)

  let defaultall base = {
    data = Zone.defaultall base;
    indirect = Zone.bottom
  }

  let default zone x y = {
    data = Zone.default zone x y;
    indirect = Zone.bottom
  }

  let add_data_dep fd data =
    { fd with data = Zone.join fd.data data }

  let add_indirect_dep fd indirect =
    { fd with indirect = Zone.join fd.indirect indirect }

  let to_zone {data; indirect} = Zone.join data indirect

  let subst = subst_from_deps

  let pretty_precise = pretty

  (* for backwards compatibility *)
  let pretty fmt fd =
    Zone.pretty fmt (to_zone fd)
end

module Memory = struct 
  include Lmap_bitwise.Make_bitwise(Deps)

  let pretty_ind_data =
    pretty_generic_printer Deps.pretty_precise "FROM"

  let find_precise = find

  let find m z =
    Deps.to_zone (find_precise m z)

end

type froms =
    { deps_return : Memory.LOffset.t;
      deps_table : Memory.t }

let top = {
  deps_return = Memory.LOffset.degenerate Deps.top;
  deps_table = Memory.top;
}

let join x y =
  { deps_return = Memory.LOffset.join x.deps_return y.deps_return ;
    deps_table = Memory.join x.deps_table y.deps_table }

let outputs { deps_table = t } =
  Memory.fold
    (fun z _ acc -> Locations.Zone.join z acc) t Locations.Zone.bottom

let addr_data_inputs ?(include_self=false) t =
  let aux b offm acc =
    Memory.LOffset.fold
      (fun itvs (self, z) acc ->
        let acc = Deps.join z acc in
        match include_self, self, b with
          | true, true, Some b ->
            Deps.add_data_dep acc (Zone.inject b itvs)
          | _ -> acc
      )
      offm
      acc
  in
  try
    let return = aux None t.deps_return Deps.bottom in
    let aux_table b = aux (Some b) in
    Memory.fold_base aux_table t.deps_table return
  with Memory.Cannot_fold -> Deps.top

let inputs ?(include_self=false) t = 
  let deps = addr_data_inputs ~include_self t in
  Deps.to_zone deps

let pretty fmt { deps_return = r ; deps_table = t } =
  Format.fprintf fmt "%a@\n\\result %a@\n"
    Memory.pretty t
    Memory.LOffset.pretty r

(** same as pretty, but uses the type of the function to output more
    precise information.
    @raise Error if the given type is not a function type
 *)
let pretty_with_type ~indirect typ fmt { deps_return = r; deps_table = t } =
  let (rt_typ,_,_,_) = Cil.splitFunctionType typ in
  if Memory.is_bottom t
  then Format.fprintf fmt
    "@[NON TERMINATING - NO EFFECTS@]"
  else
    let map_pretty =
      if indirect 
      then Memory.pretty_ind_data
      else Memory.pretty 
    in
    if Cil.isVoidType rt_typ 
    then begin
      if Memory.is_empty t
      then Format.fprintf fmt "@[NO EFFECTS@]"
      else map_pretty fmt t
    end
    else
      let pp_space fmt =
        if not (Memory.is_empty t) then
          Format.fprintf fmt "@ "
      in
      if Memory.LOffset.is_empty r then
        Format.fprintf fmt "@[<v>%a%t@[\\result FROM \\nothing@]@]"
          map_pretty t pp_space
      else
        Format.fprintf fmt "@[<v>%a%t@[\\result%a@]@]"
          map_pretty t pp_space
          (Memory.LOffset.pretty_with_type (Some rt_typ)) r

let pretty_with_type_indirect = pretty_with_type ~indirect:true
let pretty_with_type = pretty_with_type ~indirect:false

let hash { deps_return = dr ; deps_table = dt } =
  Memory.hash dt + 197 * Memory.LOffset.hash dr

let equal
    { deps_return = dr ; deps_table = dt }
    { deps_return = dr' ; deps_table = dt' } =
  Memory.equal dt dt'&& Memory.LOffset.equal dr dr'

include Datatype.Make
    (struct
      type t = froms
      let reprs =
        List.fold_left
          (fun acc o ->
            List.fold_left
              (fun acc m -> { deps_return = o; deps_table = m } :: acc)
              acc
              Memory.reprs)
          []
          Memory.LOffset.reprs
      let structural_descr =
        Structural_descr.t_record
          [| Memory.LOffset.packed_descr;
             Memory.packed_descr |]
       let name = "Function_Froms"
       let hash = hash
       let compare = Datatype.undefined
       let equal = equal
       let pretty = pretty
       let internal_pretty_code = Datatype.undefined
       let rehash = Datatype.identity
       let copy = Datatype.undefined
       let varname = Datatype.undefined
       let mem_project = Datatype.never_any_project
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
