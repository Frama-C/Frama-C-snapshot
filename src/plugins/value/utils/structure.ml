(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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


type (_,_) eq = Eq : ('a,'a) eq

module type Key = sig
  type 'a key

  val create_key: string -> 'a key
  val eq_type : 'a key -> 'b key -> ('a, 'b) eq option

  val print: 'a key Pretty_utils.formatter
  val compare: 'a key -> 'b key -> int
  val equal: 'a key -> 'b key -> bool
  val hash : 'a key -> int
  val tag: 'a key -> int
end

module Make (X : sig end) = struct

  type 'a key = { tag: int;
                  name: string }

  let c = ref (-1)
  let id () = incr c; !c

  let create_key name = { tag = id (); name }

  let equal x y = x.tag = y.tag
  let eq_type : type a b. a key -> b key -> (a,b) eq option = fun a b ->
    if equal a b
    then Some ((Obj.magic (Eq : (a,a) eq)) : (a,b) eq)
    else None

  let compare x y = Transitioning.Stdlib.compare x.tag y.tag
  let hash x = x.tag
  let tag x = x.tag

  let print fmt x = Format.pp_print_string fmt x.name
end

module Key_Value = Make (struct end)
module Key_Location = Make (struct end)
module Key_Domain = Make (struct end)

module type Shape = sig
  include Key
  type 'a data

  type 'a structure =
    | Unit : unit structure
    | Leaf : 'a key * 'a data -> 'a structure
    | Node : 'a structure * 'b structure -> ('a * 'b) structure

  val eq_structure: 'a structure -> 'b structure -> ('a, 'b) eq option
end

module Shape (Key: Key) (Data: sig type 'a t end) = struct
  include Key
  type 'a data = 'a Data.t

  type 'a structure =
    | Unit : unit structure
    | Leaf : 'a key * 'a data -> 'a structure
    | Node : 'a structure * 'b structure -> ('a * 'b) structure

  let rec eq_structure : type a b. a structure -> b structure -> (a, b) eq option
    = fun a b ->
      match a, b with
      | Leaf (key1, _), Leaf (key2, _) -> Key.eq_type key1 key2
      | Node (l1, r1), Node (l2, r2) ->
        begin
          match eq_structure l1 l2, eq_structure r1 r2 with
          | Some Eq, Some Eq -> Some Eq
          | _, _ -> None
        end
      | Unit, Unit -> Some Eq
      | _, _ -> None
end

module type Internal = sig
  type t
  type 'a structure
  val structure : t structure
end

module type External = sig
  type t
  type 'a key
  val mem : 'a key -> bool
  val get : 'a key -> (t -> 'a) option
  val set : 'a key -> 'a -> t -> t
end

module Open
    (Shape : Shape)
    (M : sig type t val structure : t Shape.structure end)
= struct

  module KMap = struct
    include Map.Make (Datatype.Int)

    let singleton key data = singleton (Shape.tag key) data
    let find k map =
      try Some (find (Shape.tag k) map)
      with Not_found -> None
  end

  open Shape

  let rec mem : type a. 'v Shape.key -> a structure -> bool = fun key -> function
    | Unit -> false
    | Leaf (k, _) -> Shape.equal key k
    | Node (left, right) -> mem key left || mem key right

  let mem key = mem key M.structure


  type ('a, 'b) get = 'b Shape.key * ('a -> 'b)

  type 'a getter = Get : ('a, 'b) get -> 'a getter

  let merge _k a b = match a, b with
    | Some _, _ -> a
    | _, Some _ -> b
    | None, None -> assert false

  let lift_get f (Get (key, get)) = Get (key, fun t -> get (f t))

  let rec compute_getters : type a. a structure -> (a getter) KMap.t = function
    | Unit -> KMap.empty
    | Leaf (key, _) ->  KMap.singleton key (Get (key, fun (t : a) -> t))
    | Node (left, right) ->
      let l = compute_getters left and r = compute_getters right in
      let l = KMap.map (lift_get fst) l and r = KMap.map (lift_get snd) r in
      KMap.merge merge l r

  let getters = compute_getters M.structure
  let get (type a) (key: a Shape.key) : (M.t -> a) option =
    match KMap.find key getters with
    | None -> None
    | Some (Get (k, get)) -> match Shape.eq_type key k with
      | None -> None
      | Some Eq -> Some get


  type ('a, 'b) set = 'b Shape.key * ('b -> 'a -> 'a)

  type 'a setter = Set : ('a, 'b) set -> 'a setter

  let lift_set f (Set (key, set)) = Set (key, fun v b -> f (fun a -> set v a) b)

  let rec compute_setters : type a. a structure -> (a setter) KMap.t = function
    | Unit -> KMap.empty
    | Leaf (key, _) -> KMap.singleton key (Set (key, fun v _t -> v))
    | Node (left, right) ->
      let l = compute_setters left and r = compute_setters right in
      let l = KMap.map (lift_set (fun set (l, r) -> set l, r)) l
      and r = KMap.map (lift_set (fun set (l, r) -> l, set r)) r in
      KMap.merge merge l r

  let setters = compute_setters M.structure
  let set (type a) (key: a Shape.key) : (a -> M.t -> M.t) =
    match KMap.find key setters with
    | None -> fun _ t -> t
    | Some (Set (k, set)) -> match Shape.eq_type key k with
      | None -> fun _ t -> t
      | Some Eq -> set
end
