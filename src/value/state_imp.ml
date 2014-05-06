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

module Sindexed =
  Hashtbl.Make
    (struct
      type t = Cvalue.Model.subtree
      let hash = Cvalue.Model.hash_subtree
      let equal = Cvalue.Model.equal_subtree
    end)

let sentinel = Sindexed.create 1

type t =
    { mutable t : Cvalue.Model.t Sindexed.t ;
      mutable p : Hptmap.prefix ;
      mutable o : Cvalue.Model.t list ;
    }

let fold f acc { t = t ; o = o } =
  List.fold_left f (Sindexed.fold (fun _k v a -> f a v) t acc) o

let iter f { t = t ; o = o } =
  Sindexed.iter (fun _k v -> f v) t;
  List.iter f o

exception Found

let empty () = { t = sentinel ; p = Hptmap.sentinel_prefix ; o = [] }

let is_empty t = t.t == sentinel && t.o = []

let exists f s =
  try
    iter (fun v -> if f v then raise Found) s;
    false
  with Found -> true

let length s = List.length s.o + Sindexed.length s.t

exception Unchanged
let pretty fmt s =
  iter
    (fun state ->
      Format.fprintf fmt "set contains %a@\n"
        Cvalue.Model.pretty state)
    s

let add_to_list v s =
  if
    List.exists
      (fun e -> Cvalue.Model.is_included v e)
      s
  then raise Unchanged;
(*  let nl, ns =
    filter
      (fun e -> not (Cvalue.Model.is_included e v))
      w
  in *)
  v :: s

let rec add_exn v s =
  if not (Cvalue.Model.is_reachable v)
  then raise Unchanged;
  if s.t == sentinel
  then begin
      match s.o with
        [ v1 ; v2 ] ->
	  begin
    	        assert(not (Cvalue.Model.equal v1 v2));

                try
                  Cvalue.Model.comp_prefixes v1 v2;
                  s.o <- add_to_list v s.o
                with
                  Cvalue.Model.Found_prefix (p, subtree1, subtree2) ->
(*
                    Format.printf "COMP h1 %d@."
                      (Cvalue.Model.hash_subtree subtree1);
                    Format.printf "COMP h2 %d@."
                      (Cvalue.Model.hash_subtree subtree2);
*)
                    let t = Sindexed.create 13 in
                    Sindexed.add t subtree1 v1;
                    Sindexed.add t subtree2 v2;
                    s.t <- t;
                    s.p <- p;
                    s.o <- [];
                    add_exn v s
          end
      | _ ->              s.o <- add_to_list v s.o
    end
  else begin
      let subtree = Cvalue.Model.find_prefix v s.p in
      begin match subtree with
        None ->       s.o <- add_to_list v s.o
      | Some subtree ->
          let candidates = Sindexed.find_all s.t subtree in
(*        Format.printf "COMP indexed %d %d@."
            (List.length candidates)
            (List.length s.o); *)
          let v_incl = Cvalue.Model.is_included v in
          if List.exists v_incl candidates
            || List.exists v_incl s.o
          then raise Unchanged
          else Sindexed.add s.t subtree v
      end
    end

let merge_set_return_new set sb =
  let f acc (e, trace) =
    try
      add_exn e sb ;
      (e,trace) :: acc
    with Unchanged ->
      acc
  in
  let result = State_set.fold f [] set in
  State_set.of_list result


let add v s =
  try
    add_exn v s
  with Unchanged -> ()
;;

let singleton v =
  let r = empty () in
  add v r;
  r

let join s =
  fold
    Cvalue.Model.join
    Cvalue.Model.bottom
    s

let fold f acc s = fold (fun acc v -> f v acc) s acc

let to_list i = Sindexed.fold (fun _k v a -> v :: a) i.t i.o

let to_set i =
  State_set.of_list_forget_history (to_list i)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
