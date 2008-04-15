(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_stdlib.ml,v 1.11 2008/11/05 14:03:16 filliatr Exp $ *)

module List = struct
  include List

  let cons e l = e::l

  let as_singleton = function
    | [e] -> e
    | _ -> failwith "as_singleton"

  let mem_assoc_eq f k l =
    fold_left (fun v_opt (k',v') -> 
		 match v_opt with
		   | None ->
		       if f k k' then Some v' else None
		   | Some v -> Some v
	      ) None l

  let all_pairs l =
    let rec aux acc = function
      | e :: l ->
	  let acc = fold_left (fun acc e' -> (e,e') :: acc) acc l in
	  aux acc l
      | [] -> acc
    in
    aux [] l

end

module Set = struct

  module type OrderedType = Set.OrderedType

  module type S = sig
    include Set.S
    val of_list: elt list -> t
    val to_list: t -> elt list
  end

  module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
    include Set.Make(Ord)

    let of_list ls =
      List.fold_left (fun s e -> add e s) empty ls

    let to_list s =
      fold (fun e acc -> e :: acc) s []

  end
end

module Map = struct

  module type OrderedType = Map.OrderedType

  module type S = sig
    include Map.S
    val elements: 'a t -> (key * 'a) list
    val keys: 'a t -> key list
    val values: 'a t -> 'a list
    val to_list: 'a t -> (key * 'a) list
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val find_or_default: key -> 'a -> 'a t -> 'a
    val find_or_none: key -> 'a t -> 'a option
    val merge: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val add_merge: ('a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t
    val diff_merge: ('a -> 'a -> 'a) -> ('a -> bool) -> 'a t -> 'a t -> 'a t
    val inter_merge: ('a -> 'a -> 'a) -> ('a -> bool) -> 'a t -> 'a t -> 'a t
  end

  module Make(Ord : OrderedType) : S with type key = Ord.t = struct
    include Map.Make(Ord)

    let elements m =
      fold (fun k v acc -> (k,v) :: acc) m []

    let keys m = 
      fold (fun k _v acc -> k :: acc) m []

    let values m = 
      fold (fun _k v acc -> v :: acc) m []

    let to_list m =
      fold (fun k v acc -> (k,v) :: acc) m []

    let exists f m =
      fold (fun k v b -> b || f k v) m false

    let filter f m =
      fold (fun k v m ->
	      if f k v then add k v m else m
	   ) m empty

    let find_or_default k v m =
      try find k m with Not_found -> v

    let find_or_none k m =
      try Some(find k m) with Not_found -> None

    let merge f m1 m2 =
      fold (fun k v1 m ->
	      try
		let v2 = find k m2 in
		add k (f v1 v2) m
	      with Not_found ->
		add k v1 m
	   ) m1 m2

    let add_merge f k v m =
      let v = 
	try f v (find k m)
	with Not_found -> v
      in 
      add k v m

    let diff_merge f g m1 m2 =
      fold (fun k v1 m ->
	      try
		let v2 = find k m2 in
		let v = f v1 v2 in
		if g v then m else add k v m
	      with Not_found ->
		add k v1 m
	   ) m1 empty

    let inter_merge f g m1 m2 =
      fold (fun k v1 m ->
	      try
		let v2 = find k m2 in
		let v = f v1 v2 in
		if g v then m else add k v m
	      with Not_found -> m
	   ) m1 empty
	
  end
end

module StdHashtbl = Hashtbl

module Hashtbl = struct

  module type HashedType = Hashtbl.HashedType

  module type S = sig 
    include Hashtbl.S
    val keys: 'a t -> key list
    val values: 'a t -> 'a list
    val choose: 'a t -> (key * 'a) option
  end

  module type Std = sig
    type ('a, 'b) t
    val create : int -> ('a, 'b) t
    val clear : ('a, 'b) t -> unit
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val copy : ('a, 'b) t -> ('a, 'b) t
    val find : ('a, 'b) t -> 'a -> 'b
    val find_all : ('a, 'b) t -> 'a -> 'b list
    val mem : ('a, 'b) t -> 'a -> bool
    val remove : ('a, 'b) t -> 'a -> unit
    val replace : ('a, 'b) t -> 'a -> 'b -> unit
    val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
    val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    val length : ('a, 'b) t -> int
    val hash : 'a -> int
    val hash_param : int -> int -> 'a -> int
  end

  include (Hashtbl : Std)

  module Make(H : HashedType) : S with type key = H.t = struct
    include Hashtbl.Make(H)

    let keys t = 
      fold (fun k _v acc -> k :: acc) t []

    let values t = 
      fold (fun _k v acc -> v :: acc) t []

    let choose t =
      let p = ref None in
      begin try
	iter (fun k v -> p := Some(k,v); failwith "Hashtbl.choose") t
      with Failure "Hashtbl.choose" -> () end;
      !p
  end

end

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
