(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t

  val exists: (key -> 'a -> bool) -> 'a t -> bool

  val max_binding: 'a t -> (key *'a)
  val min_binding: 'a t -> (key * 'a)
  val choose: 'a t -> (key * 'a)
  val cardinal: 'a t -> int
end

module Make(Ord:Map.OrderedType): (S with type key = Ord.t) =
  struct
    include Map.Make(Ord)

    let filter f m =
      let check_entry key data acc =
        if f key data then add key data acc else acc
      in fold check_entry m empty

    let exists f m =
      let module E = struct exception Found end in
      let check_entry key data () =
        if f key data then raise E.Found
      in
      try
        fold check_entry m (); false
      with E.Found -> true

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

    let min_binding m =
      let res = ref None in
      let first k b _acc = res := Some  (k, b); raise Exit in
      try fold first m (); raise Not_found
      with Exit -> match !res with None -> assert false | Some r -> r

    let choose = min_binding

    let max_binding m =
      let last k b _acc = Some (k, b) in
      match fold last m None with
        | None -> raise Not_found
        | Some res -> res 

    let cardinal m = fold (fun _ _ c -> succ c) m 0
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
