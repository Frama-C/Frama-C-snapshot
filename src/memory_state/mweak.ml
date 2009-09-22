(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: mweak.ml,v 1.13 2008-04-01 09:25:21 uid568 Exp $ *)


(** Weak array operations *)

type 'a t;;

external create: int -> 'a t = "caml_weak_create";;

let length x = Obj.size(Obj.repr x) - 1;;

external set : 'a t -> int -> 'a option -> unit = "caml_weak_set";;
external get: 'a t -> int -> 'a option = "caml_weak_get";;
external get_copy: 'a t -> int -> 'a option = "caml_weak_get_copy";;
external check: 'a t -> int -> bool = "caml_weak_check";;

let fill ar ofs len x =
  assert (if ofs < 0 || len < 0 || ofs + len > length ar
    then raise (Invalid_argument "Weak.fill"); true);
  for i = ofs to (ofs + len - 1) do
    set ar i x
  done

let blit ar1 of1 ar2 of2 len =
  assert (if of1 < 0 || of1 + len > length ar1 || of2 < 0 || of2 + len > length ar2
  then raise (Invalid_argument "Weak.blit"); true);
  
  if of2 > of1 then begin
      for i = 0 to len - 1 do
        set ar2 (of2 + i) (get ar1 (of1 + i))
      done
    end else begin
	for i = len - 1 downto 0 do
          set ar2 (of2 + i) (get ar1 (of1 + i))
	done
      end


(** Weak hash tables *)
type 'a weak_t = 'a t


module type S = sig
  type data
  type t
  val create : int -> t
  val clear : t -> unit
  val merge : t -> data -> data
  val add : t -> data -> unit
  val remove : t -> data -> unit
  val find : t -> data -> data
  val find_all : t -> data -> data list
  val mem : t -> data -> bool
  val iter : (data -> unit) -> t -> unit
  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
  val count : t -> int
  val stats : t -> int * int * int * int * int * int
end;;

module type WeakHashable = 
sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val pretty : Format.formatter -> t -> unit
end

module Make (H : WeakHashable) : (S with type data = H.t) = struct

  type 'a weak_t = 'a t;;
  let weak_create = create;;
  let emptybucket = weak_create 0;;

  type data = H.t;;

  type t = {
    mutable table : data weak_t array;
    mutable totsize : int;             (* sum of the bucket sizes *)
    mutable limit : int;               (* max ratio totsize/table length *)
  };;

  let get_index t d = (H.hash d land max_int) mod (Array.length t.table);;

  let create sz =
    let sz = if sz < 7 then 7 else sz in
    let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
    {
      table = Array.create sz emptybucket;
      totsize = 0;
      limit = 3;
    };;

  let clear t =
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- emptybucket;
    done;
    t.totsize <- 0;
    t.limit <- 3;
  ;;

  let fold f t init =
    let rec fold_bucket i b accu =
      if i >= length b then accu else
      match get b i with
      | Some v -> fold_bucket (i+1) b (f v accu)
      | None -> fold_bucket (i+1) b accu
    in
    Array.fold_right (fold_bucket 0) t.table init
  ;;

  let iter f t =
    let rec iter_bucket i b =
      if i >= length b then () else
      match get b i with
      | Some v -> f v; iter_bucket (i+1) b
      | None -> iter_bucket (i+1) b
    in
    Array.iter (iter_bucket 0) t.table
  ;;

  let count t =
    let rec count_bucket i b accu =
      if i >= length b then accu else
      count_bucket (i+1) b (accu + (if check b i then 1 else 0))
    in
    Array.fold_right (count_bucket 0) t.table 0
  ;;

  let next_sz n = min (3*n) (Sys.max_array_length - 1);;

 let stats t =
    let len = Array.length t.table in
    let lens = Array.map length t.table in
    Array.sort compare lens;
    let totlen = Array.fold_left ( + ) 0 lens in
    (len, count t, totlen, lens.(0), lens.(len/2), lens.(len-1))
  ;;

 let n_small = 8

 let weakcreate_small, weakfree_small =
   (fun n -> weak_create n) , (fun _ -> ())  
(*   let cache = Array.create (2 * n_small) emptybucket in
   (fun n ->
     let f = cache.(pred n) in
     if f != emptybucket
     then begin
(*	 Format.printf "%2d?" n; *)
	 cache.(pred n) <- emptybucket; 
	 f
       end
     else begin
	 let i = n - 1 + n_small in
	 let f = cache.(i) in
	 if f != emptybucket
	 then begin
(*	     Format.printf "%2d?" n;*)
	     cache.(i) <- emptybucket; 
	     f
	   end
	 else begin
(*	     Format.printf "%2d<" n; *)
	     weak_create n
	   end
       end),
   (fun w ->
     let n = length w in
     if cache.(pred n) == emptybucket
     then begin
(*	 Format.printf "%2d!" n;*)
	 cache.(pred n) <- w
       end
     else begin
	 let i = n - 1 + n_small in
	 if cache.(i) == emptybucket
         then begin
(*	     Format.printf "%2d!" n;*)
	     cache.(i) <- w
	   end
(*	 else Format.printf "%2d>" n*)
       end)
*)

  let rec resize t =
(*    Gc.print_stat stdout; 
    Gc.full_major ();
    Gc.print_stat stdout; *)
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
      fold (fun d () -> add newt d) t ();
   (* assert Array.length newt.table = newlen; *)
      t.table <- newt.table;
      if t.limit < 5 then t.limit <- succ t.limit; 
(*      Format.printf "Resizing; oldlen:%d newlen:%d limit:%d@." 
	oldlen newlen t.limit *)
    end

  and add_aux t d index =
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i =
      if i >= sz then begin
        let newsz, newbucket = 
	  if sz < n_small
	  then 
	    let newsz = succ sz in
	    newsz, weakcreate_small newsz
	      (* it is important to return only "succ sz" when weakcreate_small
		 is called because the bucket returned by weakcreate_small 
		 may not be initialized *)
	  else 
	    let newsz = sz + 3 in
	    if newsz >= Sys.max_array_length 
	    then failwith "Weak.Make : hash bucket cannot grow more";
	    newsz, weak_create newsz
	in
(*	if newsz >= 80 
	then begin
	    let len = Array.length t.table in
	    let lens = Array.map length t.table in
	    let totlen = Array.fold_left ( + ) 0 lens in
	    Format.printf "Clash when hashing...@\nLen:%d@\nNumber of elements:%d@\nDistribution:@."
	      len totlen;
	    Array.sort compare lens;
	    for i = 0 to pred len do
	      Format.printf "%d," lens.(i)
	    done;
	    Format.printf "Values in clashing bucket:@.";
	    for i = 0 to pred sz do
	      match get bucket i with
		None -> Format.printf "GCed@."
	      | Some v ->
		  Format.printf "V:%a@." H.pretty v
	    done;
	    assert false
	  end;
*)
	for j = 0 to sz - 1 do
          set newbucket j (get bucket j)
	done;
	if 1 <= sz && sz <= n_small then weakfree_small bucket;
        set newbucket i (Some d);
        t.table.(index) <- newbucket;
        t.totsize <- t.totsize + (newsz - sz);
        if t.totsize > t.limit * Array.length t.table then resize t;
      end 
      else begin
        if check bucket i
        then loop (i+1)
        else set bucket i (Some d)
      end
    in
    loop 0;

  and add t d = add_aux t d (get_index t d)
  ;;

  let find_or t d ifnotfound =
    let index = get_index t d in
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i =
      if i >= sz then ifnotfound index
      else begin
        match get_copy bucket i with
        | Some v when H.equal v d
           -> begin match get bucket i with
              | Some v -> v
              | None -> loop (i+1)
              end
        | _ -> loop (i+1)
      end
    in
    loop 0
  ;;

  let merge t d = find_or t d (fun index -> add_aux t d index; d);;

  let find t d = find_or t d (fun _ -> raise Not_found);;

  let find_shadow t d iffound ifnotfound =
    let index = get_index t d in
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i =
      if i >= sz then ifnotfound else begin
        match get_copy bucket i with
        | Some v when H.equal v d -> iffound bucket i
        | _ -> loop (i+1)
      end
    in
    loop 0
  ;;

  let remove t d = find_shadow t d (fun w i -> set w i None) ();;

  let mem t d = find_shadow t d (fun _ _ -> true) false;;

  let find_all t d =
    let index = get_index t d in
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i accu =
      if i >= sz then accu
      else begin
        match get_copy bucket i with
        | Some v when H.equal v d
           -> begin match get bucket i with
              | Some v -> loop (i+1) (v::accu)
              | None -> loop (i+1) accu
              end
        | _ -> loop (i+1) accu
      end
    in
    loop 0 []
  ;;

 
end;;


module MakeBig (H : WeakHashable)  : (S with type data = H.t)  = 
struct
  module HH = 
    struct
      include H
      let hash v = (hash v) lsr 4
    end

  module M = Make(HH)
    
   type t = M.t array
  type data = M.data

  let create sz = 
    let sz = sz / 16 in
    Array.init 16 (fun _i -> M.create sz)
      
  let clear (t:t) = Array.iter M.clear t

  let find t x =
    let n = H.hash x land 15 in
    M.find t.(n) x

  let add t x = 
    let n = H.hash x land 15 in
    M.add t.(n) x

  let merge t x = 
    let n = H.hash x land 15 in
  (*  Format.printf "merge chose table %d@." n; *)
    M.merge t.(n) x

  let iter _ = assert false
  let mem _ = assert false
  let find_all _ = assert false
  let remove _ = assert false
  let stats _ = assert false
  let count _ = assert false
  let fold _ = assert false


end
