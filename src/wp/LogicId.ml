(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Logic Identifiers                                                  --- *)
(* -------------------------------------------------------------------------- *)

type id = string * int

module S = 
struct

  type t = id
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let pretty fmt (x,k) = Format.fprintf fmt "%s%%%d" x k
    
end

include S

module Iset = Set.Make(S)
module Imap = Map.Make(S)
module Ihmap = Hashtbl.Make(S)
module Kset = Set.Make(String)
module Kmap = Map.Make(String)

let idref = ref 0
let idfree = ref []
let dummy = ("",0)
let extern = ref Kmap.empty

let create a = 
  match !idfree with
    | [] -> incr idref ; a , !idref
    | k::ks -> idfree := ks ; a , k

let basename a = fst a

let library lnk =
  try Kmap.find lnk !extern
  with Not_found ->
    let eid = create lnk in
    extern := Kmap.add lnk eid !extern ; eid

type allocator = {
  mutable index : int ;  (* last used indice *)
  mutable free : int list ; (* free list *)
  mutable based : bool ; (* base only allocated *)
  mutable count : int ;  (* touched *)
}

type space = {
  mutable alloc : (string,allocator) Hashtbl.t ;
  mutable locals : Iset.t ;
  mutable reserved : Kset.t ;
  indices : indice Ihmap.t ;
} and indice =
  | Base | Idx of int | Link of string

let allocator space base =
  try Hashtbl.find space.alloc base
  with Not_found ->
    let a = { index=0 ; based=false ; free=[] ; count=0 } in
    Hashtbl.add space.alloc base a ; a

let unalloc space id =
  idfree := snd id :: !idfree ;
  try
    let k = Ihmap.find space.indices id in
    let a = Hashtbl.find space.alloc (fst id) in
    match k with
      | Base -> a.based <- false
      | Idx k -> a.free <- k :: a.free
      | Link k -> space.reserved <- Kset.remove k space.reserved
  with Not_found -> ()

let push space a =
  let id = create a in
  space.locals <- Iset.add id space.locals ; id

let pop space id =
  if not (Iset.mem id space.locals) then
    Wp_parameters.fatal "LogicId: non-local %a" pretty id ; 
  space.locals <- Iset.remove id space.locals ;
  unalloc space id

type mark = Iset.t

let mark space = space.locals
let unmark space mark =
  Iset.iter
    (fun id ->
       if not (Iset.mem id mark) then unalloc space id
    ) space.locals ;
  space.locals <- mark

let clear space =
  Iset.iter (unalloc space) space.locals ;
  space.locals <- Iset.empty

let reserve1 space key =
  if Kset.mem key space.reserved then
    Wp_parameters.fatal "Already reserved name '%s'" key ;
  space.reserved <- Kset.add key space.reserved ;
  try
    let a = Hashtbl.find space.alloc key in
    if a.based then Wp_parameters.fatal 
      "Reserved name '%s' clashes with named identifier" key
  with Not_found -> ()

let reserved space = List.iter (reserve1 space)

let link space id key =
  reserve1 space key ;
  Ihmap.add space.indices id (Link key)

let space () = 
  let s = {
    alloc = Hashtbl.create 131 ;
    locals = Iset.empty ;
    indices = Ihmap.create 257 ;
    reserved = Kset.empty ;
  } in
  Kmap.iter (fun lnk id -> link s id lnk) !extern ; s

let indice space unique id =
  try Ihmap.find space.indices id
  with Not_found ->
    let base = fst id in
    let a = allocator space base in
    let idx =
      if not a.based && 
	(unique || ( a.index=0 && a.count=1 )) &&
	not (Kset.mem base space.reserved)
      then (a.based <- true ; Base)
      else match a.free with
	| [] ->	
	    let k = succ a.index in
	    a.index <- k ; Idx k
	| k::ks ->
	    a.free <- ks ; Idx k
    in
    Ihmap.add space.indices id idx ; idx

let name space ?(unique=false) id = 
  match indice space unique id with
    | Base -> fst id
    | Idx k -> Printf.sprintf "%s_%d" (fst id) k
    | Link s -> s

let unique space id = ignore (indice space true id)

let pp_id space fmt id =
  match indice space false id with
    | Base -> Format.pp_print_string fmt (fst id)
    | Idx k -> Format.fprintf fmt "%s_%d" (fst id) k
    | Link s -> Format.pp_print_string fmt s

let touch space id =
  let a = allocator space (fst id) in
  a.count <- succ a.count

let copy space = {
  alloc = Hashtbl.copy space.alloc ;
  indices = Ihmap.copy space.indices ;
  locals = space.locals ;
  reserved = space.reserved ;
}

let iter f space = 
  Ihmap.iter 
    (fun id idx -> 
       let name = match idx with
	 | Base -> fst id
	 | Idx k -> Printf.sprintf "%s_%d" (fst id) k
	 | Link s -> s
       in f id name)
    space.indices

(* -------------------------------------------------------------------------- *)
(* --- Alphanumerical Sorting                                             --- *)
(* -------------------------------------------------------------------------- *)

type tk = Letter | Digit | Symbol | End

let letter s k =
  if k < String.length s then
    let c = s.[k] in
    if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') then Letter 
    else if ('0' <= c && c <= '9') then Digit else Symbol
  else End

let rec extend s k u =
  let k = succ k in
  let v = letter s k in
  if v == u then extend s k u else k
    
let rec compare_token a b ka kb =
  let ca = letter a ka in 
  let cb = letter b kb in
  match ca , cb with
    | End , End -> 0
    | End , _ -> (-1)
    | _ , End -> 1
    | Letter , Letter ->
	let pa = extend a ka Letter in
	let pb = extend b kb Letter in
	let sa = String.sub a ka (pa-ka) in
	let sb = String.sub b kb (pb-kb) in
	let ua = String.uppercase sa in
	let ub = String.uppercase sb in
	let ucmp = String.compare ua ub in
	if ucmp <> 0 then ucmp else
	  let scmp = String.compare sa sb in
	  if scmp <> 0 then scmp else
	    compare_token a b pa pb
    | Letter , _ -> (-1)
    | _ , Letter -> 1
    | Digit , Digit ->
	let pa = extend a ka Digit in
	let pb = extend b kb Digit in
	let sa = String.sub a ka (pa-ka) in
	let sb = String.sub b kb (pb-kb) in
	let kcmp = Pervasives.compare (int_of_string sa) (int_of_string sb) in
	if kcmp <> 0 then kcmp else
	  let scmp = String.compare sa sb in
	  if scmp <> 0 then scmp else
	    compare_token a b pa pb
    | Digit , _ -> (-1)
    | _ , Digit -> 1
    | Symbol , Symbol ->
	let cmp = Char.compare a.[ka] b.[kb] in
	if cmp <> 0 then cmp else
	  compare_token a b (succ ka) (succ kb) 
	    
let alpha x y = compare_token x y 0 0
