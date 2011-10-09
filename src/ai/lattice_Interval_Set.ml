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

open Abstract_interp

module V = Int

exception Error_Top
exception Error_Bottom

module Interval = Make_Pair(V)(V)
type elt = Interval.t

type tt = Top | Set of elt list

type widen_hint = unit

let bottom = Set []
let top = Top

let check t =
  assert (match t with
          | Top -> true
          | Set s ->
              let last_stop = ref None in
              List.for_all
                (fun (a,b) -> V.compare a b <= 0 &&
                   match !last_stop with
                     None -> last_stop := Some b; true
                   | Some l -> last_stop := Some b; V.gt a l)
                s) ;
  t

let hash l = match l with
  Top -> 667
| Set l ->
    List.fold_left
      (fun acc p -> 371 * acc + Interval.hash p)
      443
      l

let tag = hash

let cardinal_zero_or_one v =
  match v with
    Top -> false
  | Set [x,y] -> V.equal x y
  | Set _ -> false

let cardinal_less_than v n =
  match v with
    Top -> raise Not_less_than
  | Set l ->
      let rec aux l card = match l with
        [] -> card
      | (x,y)::t ->
          let nn = V.of_int n in
          let card = V.add card ((V.succ (V.sub y x))) in
          if V.gt card nn
          then raise Not_less_than
          else aux t card
      in
      V.to_int (aux l V.zero)

let splitting_cardinal_less_than ~split_non_enumerable _v _n =
  ignore (split_non_enumerable);
  assert false

let compare e1 e2 =
  if e1 == e2 then 0
  else
    match e1,e2 with
    | Top,_ -> 1
    | _, Top -> -1
    | Set e1, Set e2 ->
      Extlib.list_compare Interval.compare e1 e2

let equal e1 e2 = compare e1 e2 = 0

let pretty fmt t =
  match t with
    | Top -> Format.fprintf fmt "TopISet"
    | Set s ->
        if s==[] then Format.fprintf fmt "BottomISet"
        else
          Pretty_utils.pp_iter
            ~pre:"@[<hov 1>{" ~suf:"}@]" ~sep:";@ "
            List.iter
            (fun fmt (b,e) ->
               Format.fprintf fmt "[%a..%a]" V.pretty b V.pretty e)
            fmt s

let widen _wh t1 t2 = if equal t1 t2 then t1 else top

let meet v1 v2 =
  if v1 == v2 then v1 else

      (match v1,v2 with
       | Top, v | v, Top -> v
       | Set s1 , Set s2 -> Set (
           let rec aux acc (l1:elt list) (l2:elt list) = match l1,l2 with
           | [],_|_,[] -> List.rev acc
           | (((b1,e1)) as i1)::r1,
               (((b2,e2)) as i2)::r2 ->
               let c = V.compare b1 b2 in
               if c = 0 then (* intervals start at the same value *)
                 let ce = V.compare e1 e2 in
                 if ce=0 then
                   aux ((b1,e1)::acc) r1 r2 (* same intervals *)
                 else
                   (* one interval is included in the other *)
                   let min,not_min,min_tail,not_min_tail =
                     if ce > 0 then i2,i1,r2,r1 else
                       i1,i2,r1,r2
                   in
                   aux ((min)::acc) min_tail
                     (((
                         (snd (min),
                          snd (not_min))))::
                        not_min_tail)
               else (* intervals start at different values *)
                 let _min,min_end,not_min_begin,min_tail,not_min_from =
                   if c > 0
                   then b2,e2,b1,r2,l1
                   else b1,e1,b2,r1,l2
                 in
                 let c_min = V.compare min_end not_min_begin in
                 if c_min >= 0 then
                   (* intersecting intervals *)
                   aux acc
                     ((
                        (not_min_begin,min_end))
                      ::min_tail)
                     not_min_from
                 else
                   (* disjoint intervals *)
                   aux acc min_tail not_min_from
           in aux [] s1 s2))

let join v1 v2 =
  if v1 == v2 then v1 else
    (match v1,v2 with
     | Top, _ | _, Top -> Top
     | Set (s1:elt list) , Set (s2:elt list) ->
         let rec aux (l1:elt list) (l2:elt list) = match l1,l2 with
         | [],l|l,[] -> l
         | (b1,e1)::r1,(b2,e2)::r2 ->
             let c = V.compare b1 b2 in
             let min_begin,min_end,min_tail,not_min_from =
               if c >= 0 then b2,e2,r2,l1
               else b1,e1,r1,l2
             in
             let rec enlarge_interval stop l1 look_in_me =
               match look_in_me with
               | [] -> stop,l1,[]
               | ((b,e))::r ->
                   if V.compare stop (V.pred b) >= 0
                   then
                     if V.compare stop e >= 0
                     then enlarge_interval  stop l1 r
                     else enlarge_interval  e r l1
                   else stop,l1,look_in_me
             in
             let stop,new_l1,new_l2 =
               enlarge_interval
                 min_end
                 min_tail
                 not_min_from
             in ((min_begin,stop))::
                  (aux new_l1 new_l2)
         in Set (aux s1 s2))

let inject l =  (Set l)

let inject_one ~size ~value =
  (inject [value,V.add value (V.pred size)])

let inject_bounds min max =
  if V.le min max
  then inject [min,max]
  else bottom

let transform _f = (* f must be non-decreasing *)
  assert false

let apply2 _f _s1 _s2 = assert false

let apply1 _f _s = assert false

let is_included t1 t2 =
  (t1 == t2) ||
    match t1,t2 with
    | _,Top -> true
    | Top,_ -> false
    | Set s1,Set s2 ->
        let rec aux l1 l2 = match l1 with
        | [] -> true
        | i::r ->
            let rec find (b,e as arg) l =
              match l with
              | [] -> raise Not_found
              | (b',e')::r ->
                  if V.compare b b' >= 0
                    && V.compare e' e >= 0
                  then  l
                  else if V.compare e' b >= 0 then
                    raise Not_found
                  else find arg r
            in
            try aux r (find i l2)
            with Not_found -> false
        in
        aux s1 s2

let link t1 t2 = join t1 t2 (* join is in fact an exact union *)

let is_included_exn v1 v2 =
  if not (is_included v1 v2) then raise Is_not_included

let intersects t1 t2 =
  let m = meet t1 t2 in
  not (equal m bottom)

let fold f v acc =
  match v with
    | Top -> raise Error_Top
    | Set s ->
        List.fold_right f s acc

let narrow = meet

include Datatype.Make
(struct
  type t = tt
  let name = Interval.name ^ " lattice_interval_set"
  let structural_descr =
    Structural_descr.Structure
      (Structural_descr.Sum
         [| [| Structural_descr.pack
                (Structural_descr.t_list (Descr.str Interval.descr)) |] |])
  let reprs = Top :: List.map (fun o -> Set [ o ]) Interval.reprs
  let equal = equal
  let compare = compare
  let hash = hash
  let rehash = Datatype.identity
  let copy = Datatype.undefined
  let internal_pretty_code = Datatype.undefined
  let pretty = pretty
  let varname = Datatype.undefined
  let mem_project = Datatype.never_any_project
 end)
let () = Type.set_ml_name ty None
