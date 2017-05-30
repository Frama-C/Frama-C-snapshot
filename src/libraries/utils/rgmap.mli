(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Associative maps for _ranges_ to _values_ with overlapping.

    The maps register a collection of entries, and looks for all
    entries containing some specified range. For instance, this data
    structure is well suited to attach tags to AST-nodes in GUI, where
    each node is associated to buffer offset ranges.

    When several entries cover a range, precedence goes to the tightest ones.
    When overlapping entries with the same width applies, the result of lookup is
    not specified. Remark that for AST-based ranges, overlapping ranges
    are always included one in the order.

    Current implementation has average [log(n)] complexity for adding
    [n] entries, and [log(n)^2] for lookup ranges, which is far from
    better than current implementation used in [Pretty_source] for instance.
*)

type 'a t
(** The type of range maps, containing of collection of ['a entry]. *)

type 'a entry = int * int * 'a
(** Entry [(a,b,v)] maps range [a..b] (both included) to value [v] in the map. *)

val empty : 'a t
(** The empty map. *)

val add : ?overlap:bool -> 'a entry -> 'a t -> 'a t
(** Returns a new map with the added entry. When [~overlap:true] is specified,
    overlapping entries with the same width are removed first, avoiding
    under-specified results. It is safe to ignore this attribute for AST-based
    maps. *)

val find : int -> int -> 'a t -> 'a entry
(** Find the tightest entry containing the specified range.
    @raise Not_found if no entry applies *)

val find_all : int -> int -> 'a t -> 'a entry list
(** Find all entries containing the specified range. Returns the empty list
    is none applies.

    When overlapping entries with the same width are present in the
    map, only one for each width is returned. *)

val iter : ('a entry -> unit) -> 'a t -> unit
(** Iter over all entries present in the map.
    Entries are present in increasing order of width. *)
