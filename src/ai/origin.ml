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

type kind =
  | K_Misalign_read
  | K_Leaf
  | K_Merge
  | K_Arith

module LocationSetLattice = struct
  include Abstract_interp.Make_Lattice_Set(Cil_datatype.Location)
  let currentloc_singleton () = inject_singleton (Cil.CurrentLoc.get ())
end

type origin =
  | Misalign_read of LocationSetLattice.t
  | Leaf of LocationSetLattice.t
  | Merge of LocationSetLattice.t
  | Arith of LocationSetLattice.t
  | Well
  | Unknown


let current = function
  | K_Misalign_read -> Misalign_read (LocationSetLattice.currentloc_singleton())
  | K_Leaf -> Leaf (LocationSetLattice.currentloc_singleton())
  | K_Merge -> Merge (LocationSetLattice.currentloc_singleton())
  | K_Arith -> Arith (LocationSetLattice.currentloc_singleton())

let equal o1 o2 = match o1, o2 with
  | Well, Well | Unknown, Unknown -> true
  | Leaf o1, Leaf o2 | Arith o1, Arith o2 | Merge o1, Merge o2
  | Misalign_read o1, Misalign_read o2  ->
    LocationSetLattice.equal o1 o2
  | Misalign_read _, _ -> false
  | _, Misalign_read _ -> false
  |  Leaf _, _ -> false
  |  _, Leaf _ -> false
  | Merge _, _ -> false
  | _, Merge _ -> false
  | Arith _, _ -> false
  | _, Arith _ -> false
  | _, Well | Well, _ -> false

let compare o1 o2 = match o1, o2 with
  | Misalign_read s1, Misalign_read s2
  | Leaf s1, Leaf s2
  | Merge s1, Merge s2
  | Arith s1, Arith s2 ->
      LocationSetLattice.compare s1 s2

  | Well, Well | Unknown, Unknown -> 0

  | Misalign_read _, (Leaf _ | Merge _ | Arith _ | Well | Unknown)
  | Leaf _, (Merge _ | Arith _ | Well | Unknown)
  | Merge _, (Arith _ | Well | Unknown)
  | Arith _, (Well | Unknown)
  | Well, Unknown ->
      -1

  | Unknown, (Well | Arith _ | Merge _ | Leaf _ | Misalign_read _)
  | Well, (Arith _ | Merge _ | Leaf _ | Misalign_read _)
  | Arith _, (Merge _ | Leaf _ | Misalign_read _)
  | Merge _, (Leaf _ | Misalign_read _)
  | Leaf _, Misalign_read _
      -> 1

let top = Unknown
let is_top x = equal top x


let pretty fmt o = match o with
  | Unknown ->
      Format.fprintf fmt "Unknown"
  | Misalign_read o ->
      Format.fprintf fmt "Misaligned@ %a"
        LocationSetLattice.pretty o
  | Leaf o ->
      Format.fprintf fmt "Library function@ %a"
        LocationSetLattice.pretty o
  | Merge o ->
      Format.fprintf fmt "Merge@ %a"
        LocationSetLattice.pretty o
  | Arith o ->
      Format.fprintf fmt "Arithmetic@ %a"
        LocationSetLattice.pretty o
  | Well ->       Format.fprintf fmt "Well"

let pretty_as_reason fmt org =
  if not (is_top org) then
    Format.fprintf fmt " because of %a" pretty org


let hash o = match o with
  | Misalign_read o ->
    2001 +  (LocationSetLattice.hash o)
  | Leaf o ->
    2501 + (LocationSetLattice.hash o)
  | Merge o ->
    3001 + (LocationSetLattice.hash o)
  | Arith o ->
    3557 + (LocationSetLattice.hash o)
  | Well -> 17
  | Unknown -> 97

include Datatype.Make
    (struct
      type t = origin
      let name = "Origin"
      let structural_descr = Structural_descr.t_unknown
      let reprs = [ Well; Unknown ]
      let compare = compare
      let equal = equal
      let hash = hash
      let rehash = Datatype.undefined
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = pretty
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
     end)

let bottom = Arith(LocationSetLattice.bottom)

let join o1 o2 =
  let result =
    if o1 == o2
    then o1
    else
      match o1, o2 with
      | Unknown,_ | _, Unknown -> Unknown
      | Well,_ | _ , Well   -> Well
      | Misalign_read o1, Misalign_read o2 ->
          Misalign_read(LocationSetLattice.join o1 o2)
      | _, (Misalign_read _ as m) | (Misalign_read _ as m), _ -> m
      | Leaf o1, Leaf o2 ->
          Leaf(LocationSetLattice.join o1 o2)
      | (Leaf _ as m), _ | _, (Leaf _ as m) -> m
      | Merge o1, Merge o2 ->
          Merge(LocationSetLattice.join o1 o2)
      | (Merge _ as m), _ | _, (Merge _ as m) -> m
      | Arith o1, Arith o2 ->
          Arith(LocationSetLattice.join o1 o2)
            (* | (Arith _ as m), _ | _, (Arith _ as m) -> m *)
  in
  (*  Format.printf "Origin.join %a %a -> %a@." pretty o1 pretty o2 pretty result;
  *)
  result

let meet o1 o2 =
  if o1 == o2
  then o1
  else
    match o1, o2 with
      | Arith o1, Arith o2 ->
          Arith(LocationSetLattice.meet o1 o2)
      | (Arith _ as m), _ | _, (Arith _ as m) -> m
      | Merge o1, Merge o2 ->
          Merge(LocationSetLattice.meet o1 o2)
      | (Merge _ as m), _ | _, (Merge _ as m) -> m
      | Leaf o1, Leaf o2 ->
          Leaf(LocationSetLattice.meet o1 o2)
      | (Leaf _ as m), _ | _, (Leaf _ as m) -> m
      | Misalign_read o1, Misalign_read o2 ->
          Misalign_read(LocationSetLattice.meet o1 o2)
      | _, (Misalign_read _ as m) | (Misalign_read _ as m), _ -> m
      | Well, Well -> Well
      | Well,m | m, Well -> m
      | Unknown, Unknown -> Unknown

let narrow x _y = x (* TODO *)


let is_included o1 o2 =
  (equal o1 (meet o1 o2))


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
