(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: datatype.ml,v 1.14 2008/11/20 10:21:37 uid568 Exp $ *)

open Project
open Project.Datatype 
  (* Even if [Project] is already open, use it in order to please to ocamldep.
     See ocaml bts#4618. *)

module Unit = Persistent(struct type t = unit let name = "unit" end)
module Int = Persistent(struct type t = int let name = "int " end)
module Bool =  Persistent(struct type t = bool let name = "bool" end)
module String = Imperative(struct include String let name = "string" end)
module BigInt = 
  Persistent(struct type t = Big_int.big_int let name = "big_int" end)

let persistent_map map f = if is_identity f then identity else map f

module L = List

module List(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t list
       let map = persistent_map List.map
       let rehash = map Data.rehash
       let copy = map Data.copy
       let name = extend_name "list" Data.name
     end)
  let equal l1 l2 = 
    try List.for_all2 Data.equal l1 l2
    with Invalid_argument _ -> false
  let compare _ _ = assert false (* TODO if required *)
  let gen_hash f = List.fold_left (fun acc d -> 257 * acc + f d) 1
  let hash = gen_hash Data.hash
  let physical_hash = gen_hash Data.physical_hash
   let () = 
    if Data.is_comparable_set () then 
      register_comparable ~hash ~equal ~compare ~physical_hash ()
end

module type HASHTBL = sig
  type key
  type 'a t
  val create: int -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val add: 'a t -> key -> 'a -> unit
  val replace: 'a t -> key -> 'a -> unit
  val length: 'a t -> int
  val find_all: 'a t -> key -> 'a list
end

module Make_Hashtbl(H:HASHTBL)(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t H.t
       (* mapping function preserving the binding order:
	  there is no easy way to perform such a think via the interface of
	  ocaml hashtbl :-( *)
       let map f tbl =	
	 (* first mapping which reverses the binding order *)
	 let h = H.create (H.length tbl) (* may be very memory-consuming *) in
	 H.iter (fun k v -> H.add h k (f v)) tbl;
	 (* copy which reverses again the binding order: so we get the right
	    order *)
	 let h2 = H.create (H.length tbl) (* may be very memory-consuming *) in
	 H.iter (fun k v -> H.add h2 k v) h;
	 h2
       let rehash = map Data.rehash
       let copy = map Data.copy
       let name = extend_name "hashtbl" Data.name
     end)
  (* observational equality is not provided for hash tables *)
  let hash _ = assert false
  let equal _ _ = assert false
  let compare _ _ = assert false
  (* we are not able to provide a better physical_hash function that
     Hashtbl.hash, even if Data.physical_hash is provided *)
  let () = 
    register_comparable ~hash ~equal ~compare ~physical_hash:Hashtbl.hash ()
end

module Ref(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t ref
       let physical_hash x = Data.physical_hash x
       let rehash x = ref (Data.rehash !x)
       let copy x = ref (Data.copy !x)
       let name = extend_name "ref" Data.name
     end)
  let hash x = Data.hash !x
  let physical_hash x = Data.physical_hash !x
  let equal x y = Data.equal !x !y
  let compare x y = Data.compare !x !y
  let () = 
    if Data.is_comparable_set () then 
      register_comparable ~hash ~equal ~compare ~physical_hash ()
end

module Option(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t option
       let map = persistent_map Extlib.opt_map
       let rehash = map Data.rehash
       let copy = map Data.copy
       let name = extend_name "option" Data.name
      end)
  let gen_hash f = function None -> -1 | Some x -> f x
  let hash = gen_hash Data.hash
  let physical_hash = gen_hash Data.physical_hash
  let equal x y = 
    match x,y with 
    | None, None -> true 
    | Some x, Some y -> Data.equal x y
    | None, Some _ | Some _, None -> false
  let compare _ _ = assert false (* TODO if required *)
  let () = 
    if Data.is_comparable_set () then 
      register_comparable ~hash ~equal ~compare ~physical_hash ()
end

module OptionRef(Data:S) = Ref(Option(Data))

module type SET = sig
  type elt
  type t
  val empty: t
  val singleton: elt -> t
  val add: elt -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module type MAP = sig
  type key
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Make_Map(Map:MAP)(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t Map.t
       let map f m = Map.fold (fun k d -> Map.add k (f d)) m Map.empty
       let map = persistent_map map
       let rehash = map Data.rehash
       let copy = map Data.copy
       let name = extend_name "map" Data.name
     end)
  (* observational equality is not provided for maps *)
  let hash _ = assert false
  let equal _ _ = assert false
  let compare _ _ = assert false
  (* we are not able to provide a better physical_hash function that
     Hashtbl.hash, even if Data.physical_hash is provided *)
  let () = 
    register_comparable ~hash ~equal ~compare ~physical_hash:Hashtbl.hash ()
end

module Make_Set(Set:SET)(Data:S with type t = Set.elt) = struct
  include Project.Datatype.Register
    (struct
       type t = Set.t
       let map f set = Set.fold (fun d -> Set.add (f d)) set Set.empty
       let map = persistent_map map
       let rehash = map Data.rehash
       let copy = map Data.copy
       let name = extend_name "set" Data.name
     end)
  (* observational equality is not provided for sets *)
  let hash _ = assert false
  let equal _ _ = assert false
  let compare _ _ = assert false
  (* we are not able to provide a better physical_hash function that
     Hashtbl.hash, even if Data.physical_hash is provided *)
  let () = 
    register_comparable ~hash ~equal ~compare ~physical_hash:Hashtbl.hash ()
 end

module Set(Data:S) = Make_Set(Set.Make(Data))(Data)

module Make_SetRef(Set:SET)(Data:S with type t = Set.elt) = 
  Ref(Make_Set(Set)(Data))

(** {3 Queues} *)

module Queue(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t Queue.t
       let map f q =
	 let q' = Queue.create () in
	 Queue.iter (fun x -> Queue.add (f x) q') q;
	 q'
       let rehash = map Data.rehash
       let copy = map Data.copy
       let name = extend_name "queue" Data.name
     end)
  (* observational equality is not provided for sets *)
  let hash _ = assert false
  let equal _ _ = assert false
  let compare _ _ = assert false
  (* we are not able to provide a better physical_hash function that
     Hashtbl.hash, even if Data.physical_hash is provided *)
  let () = 
    register_comparable ~hash ~equal ~compare ~physical_hash:Hashtbl.hash ()
end

(** {3 Tuples} *)

module Couple(D1:S)(D2:S) = struct
  include Project.Datatype.Register
    (struct
       type t = D1.t * D2.t
       let map f1 f2 = 
	 if is_identity f1 && is_identity f2 then identity 
	 else (fun (x, y) -> f1 x, f2 y)
       let rehash = map D1.rehash D2.rehash
       let copy = map D1.copy D2.copy
       let name = extend_name2 "couple" D1.name D2.name
     end)
  let gen_hash f1 f2 (x, y) = f1 x + 17 * f2 y
  let hash = gen_hash D1.hash D2.hash
  let physical_hash = gen_hash D1.physical_hash D2.physical_hash
  let equal (x1, y1) (x2, y2) = D1.equal x1 x2 && D2.equal y1 y2
  let compare (x1, y1) (x2, y2) =
    let n = D1.compare x1 x2 in
    if n = 0 then D2.compare y1 y2 else n
  let () = 
    if D1.is_comparable_set () || D2.is_comparable_set () then 
      register_comparable ~hash ~equal ~compare ~physical_hash ()
end

module Triple(D1:S)(D2:S)(D3:S) = struct
  include Project.Datatype.Register
    (struct
       type t = D1.t * D2.t * D3.t
       let map f1 f2 f3 = 
	 if is_identity f1 && is_identity f2 && is_identity f3 then identity 
	 else (fun (x,y,z) -> f1 x, f2 y, f3 z)
       let rehash = map D1.rehash D2.rehash D3.rehash
       let copy = map D1.copy D2.copy D3.copy
       let name = extend_name3 "triple" D1.name D2.name D3.name
     end)
  let gen_hash f1 f2 f3 (x,y,z) = f1 x + 17 * f2 y + 997 * f3 z
  let hash = gen_hash D1.hash D2.hash D3.hash
  let physical_hash = 
    gen_hash D1.physical_hash D2.physical_hash D3.physical_hash
  let equal (x1,y1,z1) (x2,y2,z2) =
    D1.equal x1 x2 && D2.equal y1 y2 && D3.equal z1 z2
  let compare _ _ = assert false (* TODO if required *)
  let () = 
    if D1.is_comparable_set () || D2.is_comparable_set () 
      || D3.is_comparable_set () 
    then 
      register_comparable ~hash ~equal ~compare ~physical_hash ()
end

(** {3 Project itself} *)

module Project = struct
  include Project.Datatype.Imperative
    (struct
       type t = Project.t
       let copy _ = failwith "never perform a project copy"
       let name = "project"
     end)
  let hash = Project.hash
  let equal = Project.equal
  let compare = Project.compare
  let () = register_comparable ~hash ~equal ~compare ()
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
