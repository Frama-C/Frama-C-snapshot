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

(* wrap treatment on option *)

module Option = struct
  let equal f x1 x2 = match x1,x2 with
    | None,None -> true
    | Some y1,Some y2 -> f y1 y2
    | _ -> false
  (* returns [Some x] iff one argument is [None] and the other is [Some x] *)
  let some x1 x2 = match x1,x2 with
    | None,x | x,None -> x
    | _ -> None
  let app f x = match x with None -> None | Some s -> Some (f s)
  let fold f x y = match x with None -> y | Some s -> f s y
  let binapp f x1 x2 = match x1,x2 with
    | None,_ | _,None -> None
    | Some y1,Some y2 -> Some (f y1 y2)
  let transform f x1 x2 = match x1,x2 with
    | None,None -> None
    | None,Some x | Some x,None -> Some x
    | Some y1,Some y2 -> Some (f y1 y2)
  let pretty f fmt x = match x with
    | None -> ()
    | Some x -> f fmt x
end

(* very basic pair *)

module Pair = struct
  let any f x1 x2 = f x1 || f x2
  let both f x1 x2 = f x1 && f x2

  module Make (L1 : Set.OrderedType) (L2 : Set.OrderedType)
    : Set.OrderedType with type t = L1.t * L2.t =
  struct
    type t = L1.t * L2.t
    let compare x1 x2 = 
      let v = L1.compare (fst x1) (fst x2) in
      if (v = 0) then L2.compare (snd x1) (snd x2) else v
  end
end

(* commonly used maps/sets based on std lib *)

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module Int32Map = Map.Make (Int32)
module Int32Set = Set.Make (Int32)

module Int31Map : Map.S with type key = int = 
struct
  module M = Int32Map
  let to32 f = fun i32 -> f (Int32.to_int i32)

  type key = int                           type 'a t = 'a M.t
  let empty = M.empty                      let is_empty = M.is_empty
  let add i = M.add (Int32.of_int i)       let find i = M.find (Int32.of_int i)
  let remove i = M.remove (Int32.of_int i) let mem i = M.mem (Int32.of_int i)
  let iter f = M.iter (to32 f)             let fold f = M.fold (to32 f)
  let map = M.map                          let mapi f = M.mapi (to32 f)
  let compare = M.compare                  let equal = M.equal
end

module Int31Set : Set.S with type elt = int =
struct
  module S = Int32Set
  let to32 f = fun i32 -> f (Int32.to_int i32)

  type elt = int                           type t = S.t
  let empty = S.empty                      let is_empty = S.is_empty
  let mem i = S.mem (Int32.of_int i)       let add i = S.add (Int32.of_int i)
  let singleton i = S.singleton (Int32.of_int i)
  let remove i = S.remove (Int32.of_int i) let union = S.union
  let inter = S.inter                      let diff = S.diff
  let compare = S.compare                  let equal = S.equal
  let subset = S.subset                    let iter f = S.iter (to32 f)
  let fold f = S.fold (to32 f)             let for_all f = S.for_all (to32 f)
  let exists f = S.exists (to32 f)         let filter f = S.filter (to32 f)
  let partition f = S.partition (to32 f)   let cardinal = S.cardinal
  let elements s = List.map Int32.to_int (S.elements s)
  let min_elt s = Int32.to_int (S.min_elt s)
  let max_elt s = Int32.to_int (S.max_elt s)
  let choose s = Int32.to_int (S.choose s)
  let split i = S.split (Int32.of_int i)
end

(* to avoid warnings about missing cases in pattern-matching,
   when exact form of the list known due to context *)

let list1 l = match l with [a] -> a | _ -> assert false
let list2 l = match l with [a;b] -> a,b | _ -> assert false
let list3 l = match l with [a;b;c] -> a,b,c | _ -> assert false
let list4 l = match l with [a;b;c;d] -> a,b,c,d | _ -> assert false
let list5 l = match l with [a;b;c;d;e] -> a,b,c,d,e | _ -> assert false

