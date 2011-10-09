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

open Locations
open Format

type precise_path =
  | Location of Shifted_Location.t
  | Union of Shifted_Location.t * precise_path

type path =
    Top of Zone.t
  | Precise of precise_path


let rec precise_equal p q =
  match p,q with
  | Location s1, Location s2 -> Shifted_Location.equal s1 s2
  | Union (s1,p1), Union (s2,p2) ->
      Shifted_Location.equal s1 s2 && precise_equal p1 p2
  | _ -> false

let rec is_bottom_precise p =
  match p with
  | Location sl -> Shifted_Location.is_bottom sl
  | Union (_sl, p) ->
      assert (not (is_bottom_precise p));
      false

let equal a b =
  match a,b with
  | Top z1, Top z2 -> Zone.equal z1 z2
  | Precise p1, Precise p2 -> precise_equal p1 p2
  | _ -> false

let bottom  = Precise (Location Shifted_Location.bottom)
let top = Top Zone.top

let rec pretty_precise fmt p =
  match p with
  | Location (sl) ->
      Shifted_Location.pretty fmt sl
  | Union (b, p) when Shifted_Location.is_bottom b ->
      fprintf fmt "*%a" pretty_precise p
  | Union (sl,p) ->
      fprintf fmt "%a | *(%a)"
        Shifted_Location.pretty sl
        pretty_precise p

let pretty fmt p =
  match p with
  | Precise p -> fprintf fmt "<%a>" pretty_precise p
  | Top z -> fprintf fmt "<ANYTHING FROM %a>" Zone.pretty z

type t = path

exception Error_Bottom
exception Error_Top

let rec topify v =
  match v with
  | Location (ls) ->
      valid_enumerate_bits ~for_writing:false ls.Shifted_Location.l
  | Union(ls,p) ->
      Zone.join
        (valid_enumerate_bits ~for_writing:false ls.Shifted_Location.l)
        (topify p)

exception Shifted_locations_unjoinable

let try_join_shifted_loc
    ({Shifted_Location.l = l1; offset = v1} as sl1)
    ({Shifted_Location.l = l2; offset = v2} as sl2) =
  if Shifted_Location.is_bottom sl1
  then sl2
  else if Shifted_Location.is_bottom sl2
  then sl1
  else if Int_Base.equal l1.size l2.size
  then
    Shifted_Location.make
      (make_loc (Location_Bits.join l1.loc l2.loc) l1.size)
      (Ival.join v1 v2)
  else raise Shifted_locations_unjoinable

let rec join_precise t1 t2 =
  match t1,t2 with
  | Location (sl1), Location (sl2) ->
      begin try
          Precise (Location (try_join_shifted_loc sl1 sl2))
        with Shifted_locations_unjoinable ->
          Top
            (Zone.join
                (valid_enumerate_bits ~for_writing:false sl1.Shifted_Location.l)
                (valid_enumerate_bits ~for_writing:false sl2.Shifted_Location.l))
      end
  | Location sl, (Union (sl1,t2) as u)
  | (Union (sl1,t2) as u), Location sl ->
      begin try
          Precise (Union (try_join_shifted_loc sl sl1, t2))
        with Shifted_locations_unjoinable ->
          Top
            (Zone.join
                (valid_enumerate_bits ~for_writing:false (sl.Shifted_Location.l))
                (topify u))
      end
  |  Union ({Shifted_Location.l = l1} as u1, p1),
      Union ({Shifted_Location.l = l2} as u2, p2) ->
       begin match join_precise p1 p2 with
       | Top t ->
           Top
             (Zone.join t
                 (Zone.join
                     (valid_enumerate_bits ~for_writing:false l1)
                     (valid_enumerate_bits ~for_writing:false l2)))
       | Precise p ->
           begin try
               Precise (Union (try_join_shifted_loc u1 u2, p))
             with Shifted_locations_unjoinable ->
               Top
                 (Zone.join (topify p)
                     (Zone.join
                         (valid_enumerate_bits ~for_writing:false l1)
                         (valid_enumerate_bits ~for_writing:false l2)))
           end
       end

let join t1 t2 =
  match t1, t2 with
  | Top t1, Top t2 -> Top (Zone.join t1 t2)
  | Top t1, Precise p | Precise p, Top t1 -> Top (Zone.join t1 (topify p))
  | Precise p1, Precise p2 -> join_precise p1 p2

let rec is_included_precise p1 p2 =
  match p1, p2 with
    Location s1, Location s2 ->
      Shifted_Location.is_included s1 s2
  | Union (s1, t1), Union (s2, t2) ->
      Shifted_Location.is_included s1 s2 &&
        is_included_precise t1 t2
  | Union (_, t), Location _ ->
      assert (not (is_bottom_precise t));
      false
  | Location sl, Union (s, _) ->
      Shifted_Location.is_included sl s

let is_included t1 t2 =
  match t1, t2 with
    Top t1, Top t2 -> Zone.is_included t1 t2
  | Top _, Precise _ -> false
  | Precise p, Top t -> Zone.is_included (topify p) t
  | Precise p1, Precise p2 -> is_included_precise p1 p2

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
