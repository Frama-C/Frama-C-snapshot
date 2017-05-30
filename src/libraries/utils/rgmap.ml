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

type 'a entry = int * int * 'a

(* Entries are stored in slices of increasing width.
   In a given slice, all entries have the same width, hence a total order is available for each slice.

   Lookup for any entry applying to some range inside a given slice only needs to check for
   subset _vs_ disjoint ranges. This can be implemented by a comparison order over ranges making
   included ranges equal: if two ranges are not included one in the other, they can be ordered.

   Finding entries covering a range in the entire map can then be performed by looking for the first
   applying entry in increasing width.

   The global complexity for lookup is around [log(n)^2], whereas insertion is only [log(n)].
   To evaluate complexity, take for instance the full binary tree of integers in range [0..n],
   with [n=2^N]. Consider one entry for each node of the tree, ranging all the integers it contains.
   There are [N] slices, with slice [i] having [2^(N-i)] entries of width [2^i].
   Lookup in each slice has logarithmic time, ie. [n-i]. Hence, global lookup has complexity [N^2].
*)

module Wmap = Map.Make
    (struct
      type t = int
      let compare (a:t) (b:t) = Pervasives.compare a b
    end)

module Rmap = Map.Make
    (struct
      type t = int * int
      (* makes included ranges equal *)
      let compare (a,b:t) (c,d:t) =
        (* Six cases to consider:
           1. [a;b;c;d] -1
           2. [a;c;b;d] -1
           3. [a;c;d;b] =0
           4. [c;a;b;d] =0
           5. [c;a;d;b] +1
           6. [c;d;a;b] +1
        *)
        if a < c then
          (* 1-2-3 *)
          if b < d then (* 1-2 *) -1 else (* 3 *) 0
        else
        if c < a then
          (* 4-5-6 *)
          if d < b then (* 5-6 *) 1 else (* 4 *) 0
        else
          (* have a=c ; then b <= d -> 4, and d <= b -> 3 *)
          0
    end)

type 'a t = 'a entry Rmap.t Wmap.t

let range (p,q,_) = (p,q)
let width (p,q,_) = q-p (* interval width minus one *)

let empty = Wmap.empty

let add ?(overlap=false) entry rmap =
  let w = width entry in
  if 0 <= w then
    let rg = range entry in
    let offsets =
      try
        let m = Wmap.find w rmap in
        if overlap && w > 1 then
          let (a,b) = rg in Rmap.remove (a,a) (Rmap.remove (b,b) m)
        else m
      with Not_found -> Rmap.empty in
    Wmap.add w (Rmap.add rg entry offsets) rmap
  else rmap

exception Found ;;

let find a b rmap =
  let rg = a,b in
  let width = b-a in
  let found = ref None in
  begin
    try Wmap.iter
          (fun w offsets ->
             if width <= w then
               try found := Some (Rmap.find rg offsets) ; raise Found
               with Not_found -> ())
          rmap with Found -> ()
  end ;
  match !found with
  | None -> raise Not_found
  | Some entry -> entry

let find_all a b rmap =
  let rg = a,b in
  let width = b-a in
  let found = ref [] in
  begin
    try Wmap.iter
          (fun w offsets ->
             if width <= w then
               try
                 let e = Rmap.find rg offsets in
                 found := e :: !found
               with Not_found -> ())
          rmap with Found -> ()
  end ;
  List.rev !found

let iter f rmap = Wmap.iter (fun _ ofs -> Rmap.iter (fun _ e -> f e) ofs) rmap
