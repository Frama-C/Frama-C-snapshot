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

module type DATA = sig
  type t
  val equal: t -> t -> bool
end

module Make(D: DATA) = struct

  type t = { mutable first: D.t list; mutable last: D.t list }

  exception Empty

  let create () = { first = []; last = [] }

  let is_empty t = t.first = [] && t.last = []

  let clear t =
    t.first <- [];
    t.last <- []

  let add x t = t.first <- x :: t.first
  let add_at_end x t = t.last <- x :: t.last

  let singleton x =
    let q = create () in
    add x q;
    q

  let transfer t =
    assert (t.first = []);
    List.iter (fun x -> add x t) t.last;
    t.last <- []

  let top t =
    match t.first, t.last with
    | [], [] -> raise Empty
    | [], _ :: _ ->
        transfer t;
        (match t.first with
         | [] -> assert false
         | x :: _ -> x)
    | x :: _, _ -> x

  let mem x t =
    let list_mem x = List.exists (D.equal x) in
    list_mem x t.first || list_mem x t.last

  let filter f t =
    let l = List.find_all f t.last in
    List.fold_right (fun x acc -> if f x then x :: acc else acc) t.first l

  let find f t =
    try List.find f t.last
    with Not_found -> List.find f (List.rev t.first)

  (* the returned boolean is a flag which is [true] when removing occurs. *)
  let remove_from_list x =
    let rec aux acc = function
      | [] -> List.rev acc, false
      | y :: l when D.equal x y -> List.rev acc @ l, true
      | y :: l -> aux (y :: acc) l
    in
    aux []

  let remove_with_flag x t =
    let first, b = remove_from_list x t.first in
    if b then begin
      t.first <- first;
      b
    end else
      let last, b = remove_from_list x t.last in
      t.last <- last;
      b

  let remove x t = ignore (remove_with_flag x t)

  let move_at_top x t =
    if not (remove_with_flag x t) then invalid_arg "Qstack.move_at_top";
    add x t

  let move_at_end x t =
    if not (remove_with_flag x t) then invalid_arg "Qstack.move_at_end";
    add_at_end x t

  let iter f t =
    List.iter f t.first;
    List.fold_right (fun p () -> f p) t.last ()

  let map f t =
    t.first <- List.map f t.first;
    t.last <- List.rev_map (fun p -> f p) t.last

  let fold f acc t =
    let acc = List.fold_left f acc t.first in
    List.fold_right (fun x acc -> f acc x) t.last acc

  let length t = List.length t.first + List.length t.last

  let nth n t =
    try List.nth t.first n
    with Failure _ ->
      try List.nth (List.rev t.last) (n - List.length t.first)
      with Failure s -> invalid_arg s

  let idx x t =
    let i = ref 0 in
    try
      iter (fun e ->
              if D.equal e x then raise Exit;
              incr i)
        t;
      raise Not_found
    with Exit -> !i

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
