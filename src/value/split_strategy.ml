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

open Abstract_interp

type split_strategy =
  | NoSplit
  | SplitEqList of Datatype.Big_int.t list
  | FullSplit
(* To be completed with more involved strategies *)

include
Datatype.Make_with_collections(struct
  type t = split_strategy
  let name = "Value.Split_strategy"
  let rehash = Datatype.identity
  let structural_descr = Structural_descr.t_abstract
  let reprs = [NoSplit]
  let compare s1 s2 = match s1, s2 with
    | NoSplit, NoSplit -> 0
    | NoSplit, _ -> -1
    | _, NoSplit -> 1
    | FullSplit, FullSplit -> 0
    | FullSplit, _ -> -1
    | _, FullSplit -> 1
    | SplitEqList l1, SplitEqList l2 ->
      Extlib.list_compare Int.compare l1 l2
	
  let equal = Datatype.from_compare
  let hash = function
    | NoSplit -> 17
    | FullSplit -> 19
    | SplitEqList l ->
      List.fold_left (fun acc i -> acc * 13 + 57 * Int.hash i) 1 l
  let copy = Datatype.identity
  let internal_pretty_code = Datatype.undefined
  let pretty fmt = function
    | NoSplit -> Format.pp_print_string fmt "no split"
    | FullSplit -> Format.pp_print_string fmt "full split"
    | SplitEqList l ->
      Format.fprintf fmt "Split on \\result == %a"
        (Pretty_utils.pp_list ~sep:",@ " Datatype.Big_int.pretty) l
  let varname _ = "v"
  let mem_project = Datatype.never_any_project
end)
