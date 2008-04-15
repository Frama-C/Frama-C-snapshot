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

(* ========================================================================= *)
(* Misc library functions to set up a nice environment.                      *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

let identity x = x;;

(* ------------------------------------------------------------------------- *)
(* Function composition.                                                     *)
(* ------------------------------------------------------------------------- *)

let ( ** ) = fun f g x -> f(g x);;

(* ------------------------------------------------------------------------- *)
(* GCD and LCM on arbitrary-precision numbers.                               *)
(* ------------------------------------------------------------------------- *)

let gcd_num n1 n2 =
  abs_num(num_of_big_int
      (Big_int.gcd_big_int (big_int_of_num n1) (big_int_of_num n2)));;

let lcm_num n1 n2 = abs_num(n1 */ n2) // gcd_num n1 n2;;

(* ------------------------------------------------------------------------- *)
(* A useful idiom for "non contradictory" etc.                               *)
(* ------------------------------------------------------------------------- *)

let non p x = not(p x);;

(* ------------------------------------------------------------------------- *)
(* Repetition of a function.                                                 *)
(* ------------------------------------------------------------------------- *)

let rec funpow n f x =
  if n < 1 then x else funpow (n-1) f (f x);;

let can f x = try f x; true with Failure _ -> false;;

(* ------------------------------------------------------------------------- *)
(* Handy list operations.                                                    *)
(* ------------------------------------------------------------------------- *)

let rec (--) = fun m n -> if m > n then [] else m::((m + 1) -- n);;

let rec (---) = fun m n -> if m >/ n then [] else m::((m +/ Int 1) --- n);;

let rec map2 f l1 l2 =
  match (l1,l2) with
    [],[] -> []
  | (h1::t1),(h2::t2) -> let h = f h1 h2 in h::(map2 f t1 t2)
  | _ -> failwith "map2: length mismatch";;

let rev =
  let rec rev_append acc l =
    match l with
      [] -> acc
    | h::t -> rev_append (h::acc) t in
  fun l -> rev_append [] l;;

let hd l =
  match l with
   h::t -> h
  | _ -> failwith "hd";;

let tl l =
  match l with
   h::t -> t
  | _ -> failwith "tl";;

let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

let rec end_itlist f l =
  match l with
        []     -> failwith "end_itlist"
      | [x]    -> x
      | (h::t) -> f h (end_itlist f t);;

let rec itlist2 f l1 l2 b =
  match (l1,l2) with
    ([],[]) -> b
  | (h1::t1,h2::t2) -> f h1 h2 (itlist2 f t1 t2 b)
  | _ -> failwith "itlist2";;

let rec zip l1 l2 =
  match (l1,l2) with
        ([],[]) -> []
      | (h1::t1,h2::t2) -> (h1,h2)::(zip t1 t2)
      | _ -> failwith "zip";;

let rec forall p l =
  match l with
    [] -> true
  | h::t -> p(h) & forall p t;;

let rec exists p l =
  match l with
    [] -> false
  | h::t -> p(h) or exists p t;;

let partition p l =
    itlist (fun a (yes,no) -> if p a then a::yes,no else yes,a::no) l ([],[]);;

let filter p l = fst(partition p l);;

let length =
  let rec len k l =
    if l = [] then k else len (k + 1) (tl l) in
  fun l -> len 0 l;;

let rec last l =
  match l with
    [x] -> x
  | (h::t) -> last t
  | [] -> failwith "last";;

let rec butlast l =
  match l with
    [_] -> []
  | (h::t) -> h::(butlast t)
  | [] -> failwith "butlast";;

let rec find p l =
  match l with
      [] -> failwith "find"
    | (h::t) -> if p(h) then h else find p t;;

let rec el n l =
  if n = 0 then hd l else el (n - 1) (tl l);;

let map f =
  let rec mapf l =
    match l with
      [] -> []
    | (x::t) -> let y = f x in y::(mapf t) in
  mapf;;

let rec allpairs f l1 l2 =
  itlist (fun x -> (@) (map (f x) l2)) l1 [];;

let distinctpairs l =
  filter (fun (a,b) -> a < b) (allpairs (fun a b -> a,b) l l);;

let rec chop_list n l =
  if n = 0 then [],l else
  try let m,l' = chop_list (n-1) (tl l) in (hd l)::m,l'
  with Failure _ -> failwith "chop_list";;

let replicate n a = map (fun x -> a) (1--n);;

let rec insertat i x l =
  if i = 0 then x::l else
  match l with
    [] -> failwith "insertat: list too short for position to exist"
  | h::t -> h::(insertat (i-1) x t);;

let rec forall2 p l1 l2 =
  match (l1,l2) with
    [],[] -> true
  | (h1::t1,h2::t2) -> p h1 h2 & forall2 p t1 t2
  | _ -> false;;

let index x =
  let rec ind n l =
    match l with
      [] -> failwith "index"
    | (h::t) -> if x = h then n else ind (n + 1) t in
  ind 0;;

let rec unzip l =
  match l with
    [] -> [],[]
  | (x,y)::t ->
      let xs,ys = unzip t in x::xs,y::ys;;

(* ------------------------------------------------------------------------- *)
(* Whether the first of two items comes earlier in the list.                 *)
(* ------------------------------------------------------------------------- *)

let rec earlier l x y =
  match l with
    h::t -> if h = y then false
              else if h = x then true
              else earlier t x y
  | [] -> false;;

(* ------------------------------------------------------------------------- *)
(* Application of (presumably imperative) function over a list.              *)
(* ------------------------------------------------------------------------- *)

let rec do_list f l =
  match l with
    [] -> ()
  | h::t -> f(h); do_list f t;;

(* ------------------------------------------------------------------------- *)
(* Association lists.                                                        *)
(* ------------------------------------------------------------------------- *)

let assoc x l = snd(find (fun p -> fst p = x) l);;

let rev_assoc x l = fst(find (fun p -> snd p = x) l);;

(* ------------------------------------------------------------------------- *)
(* Merging of sorted lists (maintaining repetitions).                        *)
(* ------------------------------------------------------------------------- *)

let rec merge ord l1 l2 =
  match l1 with
    [] -> l2
  | h1::t1 -> match l2 with
                [] -> l1
              | h2::t2 -> if ord h1 h2 then h1::(merge ord t1 l2)
                          else h2::(merge ord l1 t2);;

(* ------------------------------------------------------------------------- *)
(* Bottom-up mergesort.                                                      *)
(* ------------------------------------------------------------------------- *)

let sort ord =
  let rec mergepairs l1 l2 =
    match (l1,l2) with
        ([s],[]) -> s
      | (l,[]) -> mergepairs [] l
      | (l,[s1]) -> mergepairs (s1::l) []
      | (l,(s1::s2::ss)) -> mergepairs ((merge ord s1 s2)::l) ss in
  fun l -> if l = [] then [] else mergepairs [] (map (fun x -> [x]) l);;

(* ------------------------------------------------------------------------- *)
(* Common measure predicates to use with "sort".                             *)
(* ------------------------------------------------------------------------- *)

let increasing f x y = f x < f y;;

let decreasing f x y = f x > f y;;

(* ------------------------------------------------------------------------- *)
(* Eliminate repetitions of adjacent elements, with and without counting.    *)
(* ------------------------------------------------------------------------- *)

let rec uniq l =
  match l with
    (x::(y::_ as ys)) -> if x = y then uniq ys else x::(uniq ys)
   | _ -> l;;

let repetitions =
  let rec repcount n l =
    match l with
      x::(y::_ as ys) -> if y = x then repcount (n + 1) ys
                  else (x,n)::(repcount 1 ys)
    | [x] -> [x,n] in
  fun l -> if l = [] then [] else repcount 1 l;;

let rec tryfind f l =
  match l with
      [] -> failwith "tryfind"
    | (h::t) -> try f h with Failure _ -> tryfind f t;;

let rec mapfilter f l =
  match l with
    [] -> []
  | (h::t) -> let rest = mapfilter f t in
              try (f h)::rest with Failure _ -> rest;;

(* ------------------------------------------------------------------------- *)
(* Set operations on ordered lists.                                          *)
(* ------------------------------------------------------------------------- *)

let setify =
  let rec canonical lis =
     match lis with
       x::(y::_ as rest) -> x < y & canonical rest
     | _ -> true in
  fun l -> if canonical l then l else uniq (sort (<=) l);;

let union =
  let rec union l1 l2 =
    match (l1,l2) with
        ([],l2) -> l2
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then h1::(union t1 t2)
          else if h1 < h2 then h1::(union t1 l2)
          else h2::(union l1 t2) in
  fun s1 s2 -> union (setify s1) (setify s2);;

let intersect =
  let rec intersect l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> []
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then h1::(intersect t1 t2)
          else if h1 < h2 then intersect t1 l2
          else intersect l1 t2 in
  fun s1 s2 -> intersect (setify s1) (setify s2);;

let subtract =
  let rec subtract l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then subtract t1 t2
          else if h1 < h2 then h1::(subtract t1 l2)
          else subtract l1 t2 in
  fun s1 s2 -> subtract (setify s1) (setify s2);;

let subset,psubset =
  let rec subset l1 l2 =
    match (l1,l2) with
        ([],l2) -> true
      | (l1,[]) -> false
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then subset t1 t2
          else if h1 < h2 then false
          else subset l1 t2
  and psubset l1 l2 =
    match (l1,l2) with
        (l1,[]) -> false
      | ([],l2) -> true
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then psubset t1 t2
          else if h1 < h2 then false
          else subset l1 t2 in
  (fun s1 s2 -> subset (setify s1) (setify s2)),
  (fun s1 s2 -> psubset (setify s1) (setify s2));;

let rec set_eq s1 s2 = (setify s1 = setify s2);;

let insert x s = union [x] s;;

let smap f s = setify (map f s);;

(* ------------------------------------------------------------------------- *)
(* Union of a family of sets.                                                *)
(* ------------------------------------------------------------------------- *)

let unions s = setify(itlist (@) s []);;

(* ------------------------------------------------------------------------- *)
(* List membership. This does *not* assume the list is a set.                *)
(* ------------------------------------------------------------------------- *)

let rec mem x lis =
  match lis with
    [] -> false
  | (h::t) -> x = h or mem x t;;

(* ------------------------------------------------------------------------- *)
(* Finding all subsets or all subsets of a given size.                       *)
(* ------------------------------------------------------------------------- *)

let rec allsets m l =
  if m = 0 then [[]] else
  match l with
    [] -> []
  | h::t -> map (fun g -> h::g) (allsets (m - 1) t) @ allsets m t;;

let rec allsubsets s =
  match s with
    [] -> [[]]
  | (a::t) -> let res = allsubsets t in
              map (fun b -> a::b) res @ res;;

let allnonemptysubsets s = subtract (allsubsets s) [[]];;

(* ------------------------------------------------------------------------- *)
(* Explosion and implosion of strings.                                       *)
(* ------------------------------------------------------------------------- *)

let explode s =
  let rec exap n l =
     if n < 0 then l else
      exap (n - 1) ((String.sub s n 1) :: l) in
  exap (String.length s - 1) [];;

let implode l = itlist (^) l "";;

(* ------------------------------------------------------------------------- *)
(* Timing; useful for documentation but not logically necessary.             *)
(* ------------------------------------------------------------------------- *)

let time f x =
  let start_time = Sys.time() in
  let result = f x in
  let finish_time = Sys.time() in
  print_string
    ("CPU time (user): "^(string_of_float(finish_time -. start_time)));
  print_newline();
  result;;

(* ------------------------------------------------------------------------- *)
(* Representation of finite partial functions as balanced trees.             *)
(* Alas, there's no polymorphic one available in the standard library.       *)
(* So this is basically a copy of what's there.                              *)
(* ------------------------------------------------------------------------- *)

type ('a,'b)func =
    Empty
  | Node of ('a,'b)func * 'a * 'b * ('a,'b)func * int;;

let apply,undefined,(|->),undefine,dom,funset =
  let compare x y = if x = y then 0 else if x < y then -1 else 1 in
  let empty = Empty in
  let height = function
      Empty -> 0
    | Node(_,_,_,_,h) -> h in
  let create l x d r =
    let hl = height l and hr = height r in
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1)) in
  let bal l x d r =
    let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node(ll, lv, ld, lr, _) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
              Empty -> invalid_arg "Map.bal"
            | Node(lrl, lrv, lrd, lrr, _)->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
              Empty -> invalid_arg "Map.bal"
            | Node(rll, rlv, rld, rlr, _) ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1)) in
  let rec add x data = function
      Empty ->
        Node(Empty, x, data, Empty, 1)
    | Node(l, v, d, r, h) as t ->
        let c = compare x v in
        if c = 0 then
          Node(l, x, data, r, h)
        else if c < 0 then
          bal (add x data l) v d r
        else
          bal l v d (add x data r) in
  let rec find x = function
      Empty ->
        raise Not_found
    | Node(l, v, d, r, _) ->
        let c = compare x v in
        if c = 0 then d
        else find x (if c < 0 then l else r) in
  let rec mem x = function
      Empty ->
        false
    | Node(l, v, d, r, _) ->
        let c = compare x v in
        c = 0 or mem x (if c < 0 then l else r) in
  let rec merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (Node(l1, v1, d1, r1, h1), Node(l2, v2, d2, r2, h2)) ->
        bal l1 v1 d1 (bal (merge r1 l2) v2 d2 r2) in
  let rec remove x = function
      Empty ->
        Empty
    | Node(l, v, d, r, h) as t ->
        let c = compare x v in
        if c = 0 then
          merge l r
        else if c < 0 then
          bal (remove x l) v d r
        else
          bal l v d (remove x r) in
  let rec iter f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        iter f l; f v d; iter f r in
  let rec map f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h) in
  let rec mapi f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h) in
  let rec fold f m accu =
    match m with
      Empty -> accu
    | Node(l, v, d, r, _) ->
        fold f l (f v d (fold f r accu)) in
  let apply f x = try find x f with Not_found -> failwith "apply" in
  let undefined = Empty in
  let valmod x y f = add x y f in
  let undefine a f = remove a f in
  let dom f = setify(fold (fun x y a -> x::a) f []) in
  let funset f = setify(fold (fun x y a -> (x,y)::a) f []) in
apply,undefined,valmod,undefine,dom,funset;;

let tryapplyd f a d = try apply f a with Failure _ -> d;;
let tryapply f x = tryapplyd f x x;;
let tryapplyl f x = tryapplyd f x [];;
let (:=) = fun x y -> (x |-> y) undefined;;
let fpf assigs = itlist (fun x -> x) assigs undefined;;
let defined f x = can (apply f) x;;

(* ------------------------------------------------------------------------- *)
(* Install a (trivial) printer for finite partial functions.                 *)
(* ------------------------------------------------------------------------- *)

let print_fpf (f:('a,'b)func) = print_string "<func>";;

START_INTERACTIVE;;
#install_printer print_fpf;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Related stuff for standard functions.                                     *)
(* ------------------------------------------------------------------------- *)

let valmod a y f x = if x = a then y else f(x);;

let undef x = failwith "undefined function";;

(* ------------------------------------------------------------------------- *)
(* Union-find algorithm.                                                     *)
(* ------------------------------------------------------------------------- *)

type ('a)pnode = Nonterminal of 'a | Terminal of 'a * int;;

type ('a)partition = Partition of ('a,('a)pnode)func;;

let rec terminus (Partition f as ptn) a =
  match (apply f a) with
    Nonterminal(b) -> terminus ptn b
  | Terminal(p,q) -> (p,q);;

let tryterminus ptn a =
  try terminus ptn a with Failure _ -> (a,0);;

let canonize ptn a = try fst(terminus ptn a) with Failure _ -> a;;

let equate (a,b) (Partition f as ptn) =
  let (a',na) = tryterminus ptn a
  and (b',nb) = tryterminus ptn b in
  Partition
   (if a' = b' then f else
    if na <= nb then
       itlist identity [a' |-> Nonterminal b'; b' |-> Terminal(b',na+nb)] f
    else
       itlist identity [b' |-> Nonterminal a'; a' |-> Terminal(a',na+nb)] f);;

let unequal = Partition undefined;;

let equated (Partition f) = dom f;;

(* ------------------------------------------------------------------------- *)
(* First number starting at n for which p succeeds.                          *)
(* ------------------------------------------------------------------------- *)

let rec first n p = if p(n) then n else first (n +/ Int 1) p;;
