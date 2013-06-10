(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

module type S = sig
  include Hashtbl.S
  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

let hash = Hashtbl.hash
let hash_param = Hashtbl.hash_param

module Make(H: Hashtbl.HashedType) : S with type key = H.t  = struct

  include Hashtbl.Make(H)

  let fold_sorted ?(cmp=Pervasives.compare) f h acc =
    let module Aux = struct type t = key let compare = cmp end in
    let module M = Map.Make(Aux) in
    let add k v m =
      try
        let l = v :: M.find k m in
        M.add k l m
      with Not_found -> M.add k [v] m
    in
    let map = fold add h M.empty in
    let fold_k k l acc =
      List.fold_left (fun acc v -> f k v acc) acc (List.rev l)
    in
    M.fold fold_k map acc

  let iter_sorted ?cmp f h =
    fold_sorted ?cmp (fun k v () -> f k v) h ()

end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
