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

module type S =
sig
  include Map.S
  val merge: (key -> 'a option -> 'b option -> 'c option) -> 
    'a t -> 'b t -> 'c t
end

module Make(Ord:Map.OrderedType): (S with type key = Ord.t) =
  struct
    include Map.Make(Ord)
    let merge f m1 m2 =
      let traverse_first k v1 acc =
        let v2 = try Some (find k m2) with Not_found -> None in
        match f k (Some v1) v2 with
          | None -> acc
          | Some v -> add k v acc
      in
      let traverse_snd k v2 acc =
        if mem k acc then acc
        else
          match f k None (Some v2) with
            | None -> acc
            | Some v -> add k v acc
      in
      fold traverse_snd m2 (fold traverse_first m1 empty)
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
