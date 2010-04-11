(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

open Project
open Project.Datatype
  (* Even if [Project] is already open, use it in order to please to ocamldep.
     See ocaml bts#4618. *)

module Unit = Persistent(struct type t = unit let name = "unit" end)
let () = Unit.register_comparable ~compare:Pervasives.compare ~equal:(=) ()

module Int = Persistent(struct type t = int let name = "int " end)
let () = Int.register_comparable ~compare:Pervasives.compare ~equal:(=) ()

module Bool =  Persistent(struct type t = bool let name = "bool" end)
let () = Bool.register_comparable ~compare:Pervasives.compare ~equal:(=) ()

module String = Imperative(struct include String let name = "string" end)
let () = String.register_comparable ~compare:Pervasives.compare ~equal:(=) ()

module BigInt =
  Persistent(struct type t = Big_int.big_int let name = "big_int" end)

module Formatter  = Imperative
  (struct
     type t = Format.formatter
     let copy _ = assert false
     let name = "formatter"
   end)

module OutChannel = Imperative
  (struct
     type t = out_channel
     let copy _ = assert false
     let name = "out_channel"
   end)

module InChannel  = Imperative
  (struct
     type t = in_channel
     let copy _ = assert false
     let name = "in_channel"
   end)

let persistent_map map f = if is_identity f then identity else map f

module List(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t list
       let map = persistent_map List.map
       let descr = Unmarshal.t_list Data.descr
       let copy = map Data.copy
       let name = extend_name "list" Data.name
     end)

  let equal l1 l2 =
    try List.for_all2 Data.equal l1 l2
    with Invalid_argument _ -> false
  let gen_hash f = List.fold_left (fun acc d -> 257 * acc + f d) 1
  let hash = gen_hash Data.hash

  let rec compare l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | e1 :: q1, e2 :: q2 ->
        let n = Data.compare e1 e2 in
        if n = 0 then compare q1 q2 else n


  let () =
    if Data.is_comparable_set () then
      register_comparable ~compare ~hash ~equal ();
    Extlib.may
      (fun f -> mem_project := Some (fun p l -> List.exists (f p) l))
      !Data.mem_project

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
	  there is no easy way to perform such a thing via the interface of
	  OCaml hashtbl :-( *)
       let map f tbl =
	 (* first mapping which reverses the binding order *)
	 let h = H.create (H.length tbl) (* may be very memory-consuming *) in
	 H.iter (fun k v -> H.add h k (f v)) tbl;
	 (* copy which reverses again the binding order: so we get the right
	    order *)
	 let h2 = H.create (H.length tbl) (* may be very memory-consuming *) in
	 H.iter (fun k v -> H.add h2 k v) h;
	 h2
       let descr =
(* TODO: if keys change hash when marshaled+unmarshaled, use Unmarshal.t_hashtbl_changedhashs *)
	 Unmarshal.t_hashtbl_unchangedhashs
	   Unmarshal.Abstract
	   Data.descr
       let copy = map Data.copy
       let name = extend_name "hashtbl" Data.name
     end)
  let () =
    (* the following is incorrect if the key is a project *)
    Extlib.may
      (fun f ->
	 mem_project :=
	   Some (fun p h ->
		   try H.iter (fun _k v -> if f p v then raise Exit) h; false
		   with Exit -> true))
      !Data.mem_project
end

module Ref(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t ref
       let descr = Unmarshal.t_ref Data.descr
       let copy x = ref (Data.copy !x)
       let name = extend_name "ref" Data.name
     end)
  let hash x = Data.hash !x
  let equal x y = Data.equal !x !y
  let compare x y = Data.compare !x !y
  let () =
    if Data.is_comparable_set () then
      register_comparable ~hash ~equal ~compare ();
    Extlib.may
      (fun f -> mem_project := Some (fun p x -> f p !x))
      !Data.mem_project
end

module Option(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t option
       let map = persistent_map Extlib.opt_map
       let descr = Unmarshal.t_option Data.descr
       let copy = map Data.copy
       let name = extend_name "option" Data.name
      end)
  let hash = function None -> -1 | Some x -> Data.hash x
  let equal x y = match x,y with
    | None, None -> true
    | Some x, Some y -> Data.equal x y
    | None, Some _ | Some _, None -> false
  let () =
    if Data.is_comparable_set () then register_comparable ~hash ~equal ();
    Extlib.may
      (fun f ->
	 mem_project :=
	   Some (fun p x -> match x with None -> false | Some x -> f p x))
      !Data.mem_project
end

module OptionRef(Data:S) = Ref(Option(Data))

module type SET = sig
  type elt
  type t
  val descr:Unmarshal.t
  val empty: t
  val singleton: elt -> t
  val add: elt -> t -> t
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module type MAP = sig
  type key
  type 'a t
  val descr: Unmarshal.t -> Unmarshal.t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Make_Map(Map:MAP)(Data:S) = struct
  include Project.Datatype.Register
    (struct
       type t = Data.t Map.t
       let map f m = Map.fold (fun k d -> Map.add k (f d)) m Map.empty
       let map = persistent_map map
       let descr = Project.no_descr
       let copy = map Data.copy
       let name = extend_name "map" Data.name
     end)
  let () =
    (* the following is incorrect if the key is a project *)
    Extlib.may
      (fun f ->
	 mem_project :=
	   Some (fun p h ->
		   try Map.iter (fun _k v -> if f p v then raise Exit) h; false
		   with Exit -> true))
	!Data.mem_project
end

module Make_Set(Set:SET)(Data:S with type t = Set.elt) = struct
  include Project.Datatype.Register
    (struct
       type t = Set.t
       let map f set = Set.fold (fun d -> Set.add (f d)) set Set.empty
       let map = persistent_map map
       let descr = Project.no_descr
       let copy = map Data.copy
       let name = extend_name "set" Data.name
     end)
  let () =
    Extlib.may
      (fun f ->
	 mem_project :=
	   Some (fun p h ->
		   try Set.iter (fun x -> if f p x then raise Exit) h; false
		   with Exit -> true))
	!Data.mem_project
 end

module Set(Data:S) = Make_Set(Unmarshal.SetWithDescr(Data))(Data)

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
       let descr = Project.no_descr
       let copy = map Data.copy
       let name = extend_name "queue" Data.name
     end)
  (* observational equality is not provided for queues *)
  let hash _ = assert false
  let equal _ _ = assert false
  let compare _ _ = assert false
  let () =
    register_comparable ~hash ~equal ~compare ();
    Extlib.may
      (fun f ->
	 mem_project :=
	   Some (fun p q ->
		   try Queue.iter (fun x -> if f p x then raise Exit) q; false
		   with Exit -> true))
      !Data.mem_project

end

(** {3 Tuples} *)

module Couple(D1:S)(D2:S) = struct
  include Project.Datatype.Register
    (struct
       type t = D1.t * D2.t
       let map f1 f2 =
	 if is_identity f1 && is_identity f2 then identity
	 else (fun (x, y) -> f1 x, f2 y)
       let descr = Unmarshal.t_tuple [| D1.descr; D2.descr |]
       let copy = map D1.copy D2.copy
       let name = extend_name2 "couple" D1.name D2.name
     end)
  let hash (x, y) = D1.hash x + 17 * D2.hash y
  let equal (x1, y1) (x2, y2) = D1.equal x1 x2 && D2.equal y1 y2
  let compare (x1, y1) (x2, y2) =
    let n = D1.compare x1 x2 in
    if n = 0 then D2.compare y1 y2 else n
  let () =
    if D1.is_comparable_set () || D2.is_comparable_set () then
      register_comparable ~hash ~equal ~compare ();
    match !D1.mem_project, !D2.mem_project with
    | None, None -> ()
    | Some f, None -> mem_project := Some (fun p (x, _) -> f p x)
    | None, Some f -> mem_project := Some (fun p (_, x) -> f p x)
    | Some f1, Some f2 ->
	mem_project := Some (fun p (x1, x2) -> f1 p x1 || f2 p x2)
end

module Triple(D1:S)(D2:S)(D3:S) = struct
  include Project.Datatype.Register
    (struct
       type t = D1.t * D2.t * D3.t
       let map f1 f2 f3 =
	 if is_identity f1 && is_identity f2 && is_identity f3 then identity
	 else (fun (x,y,z) -> f1 x, f2 y, f3 z)
       let descr = Unmarshal.t_tuple [| D1.descr; D2.descr; D3.descr |]
       let copy = map D1.copy D2.copy D3.copy
       let name = extend_name3 "triple" D1.name D2.name D3.name
     end)
  let hash (x, y, z) = D1.hash x + 17 * D2.hash y + 997 * D3.hash z
  let equal (x1,y1,z1) (x2,y2,z2) =
    D1.equal x1 x2 && D2.equal y1 y2 && D3.equal z1 z2
  let compare _ _ = assert false (* TODO if required *)
  let () =
    if D1.is_comparable_set () || D2.is_comparable_set ()
      || D3.is_comparable_set ()
    then
      register_comparable ~hash ~equal ~compare ();
    match !D1.mem_project, !D2.mem_project, !D3.mem_project with
    | None, None, None -> ()
    | Some f, None, None -> mem_project := Some (fun p (x, _, _) -> f p x)
    | None, Some f, None -> mem_project := Some (fun p (_, x, _) -> f p x)
    | None, None, Some f -> mem_project := Some (fun p (_, _, x) -> f p x)
    | Some f1, Some f2, None ->
	mem_project := Some (fun p (x1, x2, _) -> f1 p x1 || f2 p x2)
    | Some f1, None, Some f3 ->
	mem_project := Some (fun p (x1, _, x3) -> f1 p x1 || f3 p x3)
    | None, Some f2, Some f3 ->
	mem_project := Some (fun p (_, x2, x3) -> f2 p x2 || f3 p x3)
    | Some f1, Some f2, Some f3 ->
	mem_project :=
	  Some (fun p (x1, x2, x3) -> f1 p x1 || f2 p x2 || f3 p x3)
end

(** {3 Project itself} *)

module Project = Own

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
