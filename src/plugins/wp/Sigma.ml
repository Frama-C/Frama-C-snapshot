(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Generic Sigma Factory                                              --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F

module Make
    (C : Sigs.Chunk)
    (H : Qed.Collection.S with type t = C.t) :
  Sigs.Sigma with type chunk = C.t
                and module Chunk = H =
struct

  type chunk = C.t
  module Chunk = H
  type domain = H.Set.t

  let empty = H.Set.empty
  let union = H.Set.union

  type t = { id : int ; mutable map : var H.map }

  let id = ref 0 (* for debugging purpose *)
  let build map = let k = !id in incr id ; { id = k ; map = map }
  let create () = build H.Map.empty
  let copy s = build s.map

  let newchunk c =
    Lang.freshvar ~basename:(C.basename_of_chunk c) (C.tau_of_chunk c)

  let merge a b =
    let pa = ref Passive.empty in
    let pb = ref Passive.empty in
    let merge_chunk c x y =
      if Var.equal x y then x else
        let z = newchunk c in
        pa := Passive.bind ~fresh:z ~bound:x !pa ;
        pb := Passive.bind ~fresh:z ~bound:y !pb ;
        z in
    let w = H.Map.union merge_chunk a.map b.map in
    build w , !pa , !pb

  type kind =
    | Used of Lang.F.var
    | Unused

  let merge_list l =
    (** Get a map of the chunks (the data is not important) *)
    let union = List.fold_left (fun acc e -> H.Map.union (fun _ v1 _ -> v1) acc e.map) H.Map.empty l in
    (** The goal is to build a matrix chunk -> elt of the list -> Used/Unused
    *)
    (** Set the data of the map to []. *)
    let union = H.Map.map (fun _ -> []) union in
    (** For each elements of the list tell if each chunk is used *)
    let merge _ m e =
      match m, e with
      | Some m, Some e -> Some (Used e::m)
      | Some m, None -> Some (Unused::m)
      | None, _ -> assert false in
    let union = List.fold_left (fun acc e -> H.Map.merge merge acc e.map) union
        (** important so that the list in the map are in the correct order *)
        (List.rev l) in
    (** Build the passive for each element of the list, and the final domain *)
    let p = ref (List.map (fun _ -> Passive.empty) l) in
    let map c l =
      match List.filter (fun x -> not (Unused = x)) l with
      | [] -> assert false
      (** If all the sigmas use the same variable *)
      | (Used a)::l when List.for_all (function | Unused -> true | Used x -> Var.equal x a) l ->
          a
      | _ ->
          let z = newchunk c in
          let map2 p = function
            | Unused -> p
            | Used a -> Passive.bind ~fresh:z ~bound:a p
          in
          p := List.map2 map2 !p l;
          z
    in
    let union = H.Map.mapi map union in
    build union , !p

  let choose a b =
    let merge_chunck _ x y = if Var.compare x y <= 0 then x else y in
    build (H.Map.union merge_chunck a.map b.map)

  let get w c =
    try H.Map.find c w.map
    with Not_found ->
      let x = newchunk c in
      w.map <- H.Map.add c x w.map ; x

  let mem w c = H.Map.mem c w.map

  let join a b =
    let p = ref Passive.empty in
    H.Map.iter2
      (fun chunk x y ->
         match x,y with
         | Some x , Some y -> p := Passive.join x y !p
         | Some x , None -> b.map <- H.Map.add chunk x b.map
         | None , Some y -> a.map <- H.Map.add chunk y a.map
         | None , None -> ())
     a.map b.map ; !p

  let assigned a b written =
    let p = ref Bag.empty in
    H.Map.iter2
      (fun chunk x y ->
         if not (H.Set.mem chunk written) then
           match x,y with
           | Some x , Some y when x != y ->
               p := Bag.add (p_equal (e_var x) (e_var y)) !p
           | Some x , None -> b.map <- H.Map.add chunk x b.map
           | None , Some y -> a.map <- H.Map.add chunk y a.map
           | _ -> ())
      a.map b.map ; !p

  let value w c = e_var (get w c)

  let iter f w = H.Map.iter f w.map
  let iter2 f w1 w2 = H.Map.iter2 f w1.map w2.map

  let havoc w xs =
    let ys = H.Set.mapping newchunk xs in
    build (H.Map.union (fun _c _old y -> y) w.map ys)

  let havoc_chunk w c =
    let x = newchunk c in
    build (H.Map.add c x w.map)

  let havoc_any ~call w =
    let framer c x = if call && C.is_framed c then x else newchunk c in
    build (H.Map.mapi framer w.map)

  let remove_chunks w xs =
    build (H.Map.filter (fun c _ -> not (H.Set.mem c xs)) w.map)

  let domain w = H.Map.domain w.map

  let pretty fmt w =
    begin
      Format.fprintf fmt "@[<hov 2>@@%s%d[" C.self w.id ;
      H.Map.iter
        (fun c x -> Format.fprintf fmt "@ %a:%a" C.pretty c Var.pretty x) w.map ;
      Format.fprintf fmt " ]@]" ;
    end

  let writes seq =
    let effect = ref Chunk.Set.empty in
    iter2
      (fun chunk u v ->
         let written =
           match u,v with
           | Some x , Some y -> not (Var.equal x y)
           | None , Some _ -> true
           | Some _ , None -> false (** no need to create a new so it is the same *)
           | None, None -> assert false
         in
         if written then effect := Chunk.Set.add chunk !effect
      ) seq.Sigs.pre seq.Sigs.post ;
    !effect

end
