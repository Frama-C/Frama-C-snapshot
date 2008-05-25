(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007                                                    *)
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

module MemoryFootprint = 
  Computation.Ref
    (struct include Datatype.Int let default = 2 end)
    (struct 
       let name = Project.Computation.Name.make "Buckx.MemoryFootprint" 
       let dependencies = [] 
     end)

module type WeakHashable = 
sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val pretty : Format.formatter -> t -> unit
  val id : string
end

module type S = sig
  type data
  type t

  val create : int -> t
  val merge : t -> data -> data
  val iter : t -> (data -> unit) -> unit
  val clear : t -> unit
  val release : t -> unit
  val shallow_copy : t -> t
  val overwrite : old:t -> fresh:t -> unit
  val pretty_debug : Format.formatter -> t -> int -> unit
end;;

(*
module MakeBig(H:WeakHashable) = 
struct
  module W = Weak.Make(H)
  type t = W.t ref
  type data = H.t
  let create c = ref (W.create c)
  let merge t d = 
    let r = W.merge !t d in
(*    Format.printf "%s: l'objet %a est dans la table@."
    H.id
    H.pretty r;
*)    r
  let iter t f = W.iter f !t
  let clear t = W.clear !t
  let release _t = ()
  let pretty_debug _ = assert false
  let shallow_copy t = ref (!t)
  let overwrite ~old ~fresh = old := !fresh
end
*)  

 
exception Full

let debug = false
open Bigarray

type char_array = (char, int8_unsigned_elt, c_layout) Array1.t
type nibble_array = (int, int_elt, c_layout) Array1.t

external mybigarray_alignment : ('a, 'b, 'c) Array1.t -> int
   = "mybigarray_alignment" 

external getperfcount : unit -> int = "getperfcount"
(*let getperfcount () = 0 *)

let create_char_array ~align n = 
  let mask = pred align in
  let r = Array1.create char c_layout (n + mask) in
  let offset = align - (mybigarray_alignment r land mask) in
  if offset = align
  then r
  else Array1.sub r offset n

let create_nibble_array n = Array1.create int c_layout n

external init_fat: char_array -> int -> unit = "init_fat" ;;

let n = 512
let mask = 511 (* n - 1 *)
let shift = 9 (* log2 n *)

let spill_n = 8
let spill_mask = 7 (* spill_n - 1 *)

let end_of_chain = '\254' 
let spill_chain = '\255'

let fat_entries_size = 
  254 (* fat *) + 1 (* free *) + 1 (* padding *) + 256 (* entries *)
let data_size = 254 (* just the fat, ma'm *)
  (* the above comment (a) is funny (b) means that there is a data cell 
     only for each "fat" cell in t.fat, as opposed to the entries,
     padding or "entry to the free list" *)
  
let spill_buckx_fat_size = spill_n * fat_entries_size 
  (* the size of the char array used to represent a spilling buckx *)
let fat_size = n * fat_entries_size 
  (* the size of the char array used to represent a buckx *)

  (* the following three constants are special offsets into a fat table *)
let free_entry = 255
let padding_entry = 254
let entries_start = 256

let heap_size = 20 * fat_size

let heap = ref (create_char_array ~align:4096 heap_size) 
let heap_free = ref 0
let spare_spill_fats = ref []
let spare_fats = ref []

let new_heap () =
  heap := create_char_array ~align:4096 heap_size;
  heap_free := 0

exception Make_one

let conditionally_new_heap () =
  if MemoryFootprint.get () >= 3
  then new_heap()
  else raise Make_one

let get_spill_fats () =
  match !spare_spill_fats with
    h :: t -> 
      spare_spill_fats := t;
      h
  | [] ->
      try
	if !heap_free = heap_size
	then conditionally_new_heap ();
	let old_heap_free = !heap_free in
	heap_free := spill_buckx_fat_size + old_heap_free;
	Array1.sub !heap old_heap_free spill_buckx_fat_size
      with Make_one -> 
	create_char_array ~align:256 spill_buckx_fat_size

let get_fats () = 
  match !spare_fats with
    h :: t -> 
      spare_fats := t;
      h
  | [] ->
      let old_heap_free = !heap_free in
      try
	if old_heap_free + fat_size > heap_size
	then begin
	    let old_heap = !heap in
	    while !heap_free < heap_size
	    do
	      let fat = Array1.sub old_heap !heap_free spill_buckx_fat_size in
	      spare_spill_fats := fat :: !spare_spill_fats;
	      heap_free := !heap_free + spill_buckx_fat_size
	    done;
	    conditionally_new_heap ()
	  end;
	let old_heap_free = !heap_free in
	heap_free := fat_size + old_heap_free;
	Array1.sub !heap old_heap_free fat_size
      with Make_one ->
	create_char_array ~align:256 fat_size

let release_spill_fats fat =
  spare_spill_fats := fat :: !spare_spill_fats

let release_fats fat =
  spare_fats := fat :: !spare_fats

module Make (H : WeakHashable) =
struct
  type data = H.t
  type t = 
      { fat : char_array ; 
	data : H.t Weak.t ; 
	hash : nibble_array ;
	mutable spill : t }

  let empty_string = Array1.create char c_layout 0
  let empty_nibble_array = Array1.create int c_layout 0

(* empty est une sentinelle à laquelle on compare physiquement des t.spill *)
  let rec empty =  
    { fat = empty_string ; 
      data = Weak.create 0 ; 
      hash = empty_nibble_array;
      spill = empty;      
    }

  let release t =
    if t.spill != empty then release_spill_fats t.spill.fat;
    release_fats t.fat

  let get_indexes_k_i ~full_hash ~full_shift = 
    let h = full_hash lsr full_shift in
    let k = h land mask in
    let i = ((h lsr shift) land 255) + entries_start in
    k, i

  let spill_index k = k land spill_mask

  external collect_Nones: t -> int -> unit = "collect_Nones" 

  let free_list_length fat base =
    let rec aux n chain =
      let next = Array1.get fat (base + int_of_char chain) in
      if next = end_of_chain
      then n
      else aux (succ n) next
    in
    aux 0 (char_of_int free_entry)

  let pretty_debug_buckx fmt t k =
    let fat = t.fat in
    let data = t.data in
    let base = k * fat_entries_size in
    Format.fprintf fmt "fat:@\n";
    for i = 0 to 253 do
      Format.fprintf fmt "  %03d -> %03d ..." 
	i (int_of_char (Array1.get fat (base + i)));
      begin match Weak.get_copy data (k * data_size + i) with
	Some _ -> (* H.pretty fmt d *) ()
      | None -> ()
      end;
      Format.fprintf fmt "@\n"
    done;
    Format.fprintf fmt "free list: %03d@\nentries:@\n" 
      (int_of_char (Array1.get fat (base + free_entry)));
    for i = 0 to 255 do
      let c = (Array1.get fat (base + entries_start + i)) in
      if c <> end_of_chain
      then
	Format.fprintf fmt "e %03d -> %03d@\n" i (int_of_char c);
    done;
    Format.fprintf fmt "@\nfree list length:%d@." (free_list_length fat base)
      
  let pretty fmt t =
    let l = Weak.length t.data in
    assert (l mod fat_entries_size = 0);
    for k = 0 to pred (l / fat_entries_size)
    do
      Format.fprintf fmt "table:%d@." k;
      pretty_debug_buckx fmt t k
    done

  let create_internal fat ~size =
(*    let before = getperfcount () in *)
    init_fat fat size;    
(*    let after = getperfcount () in
    let tim = after - before in
    Format.printf "buckx: Init time =%8d@." tim;*)
(*  reference implementation for init_fat
    for k = 0 to pred size do
      let base = k * fat_entries_size in

      for i = 0 to 253 do
	fat.[base + i] <- char_of_int (succ i);
      done;
      fat.[base + padding_entry] <- '\399';
      fat.[base + free_entry] <- '\000';
      String.fill 
	fat
	(base + entries_start) 256
	end_of_chain;
      done;
*)

(*    (* this code verifies that a fat is correctly initialized *)
    for k = 0 to pred size do
      let base = k * fat_entries_size in

      for i = 0 to 253 do
        assert(Array1.get fat (base + i) = char_of_int (succ i));
      done;
      assert ( Array1.get fat (base + padding_entry) = char_of_int 255);
      assert ( Array1.get fat (base + free_entry) = '\000');
      for i = entries_start to entries_start + 255 
      do
        assert (Array1.get fat (base + i) = end_of_chain);
      done;
    done;
*)

      let d = Weak.create (size * data_size) in
      let hash = create_nibble_array (size * data_size) in

      let r = { fat = fat ; data = d ; hash = hash ; spill = empty } in
(*      Format.printf "debug buckx : %a@." 
	(fun fmt -> pretty_debug_buckx fmt r) 0;  *)
      r
	
  let create () = 
    let fat = get_fats () in
    create_internal fat n

  let make_spill t =
    if t.spill == empty
    then begin
        let fat = get_spill_fats () in
	t.spill <- create_internal fat spill_n;
	if debug then Format.printf "Spilling buckx %s@." H.id
      end

  let get_free t ~base =
    let free = 
	(Array1.get t.fat (base + free_entry)) 
    in
    if free = end_of_chain then raise Full;
    Array1.set t.fat (base + free_entry)
      (Array1.get t.fat (base + int_of_char free));
    free

  let add_aux t d k index full_hash =
    assert (t != empty);
    let base = k * fat_entries_size in
    let free = get_free t ~base in
    let fat = t.fat in
    let data = t.data in
    let h = t.hash in
    let current = Array1.get fat (base + index) in
    Array1.set fat (base + int_of_char free) current;
    Array1.set fat (base + index) free;
    assert (not (Weak.check data (k * data_size + int_of_char free)));
    let pos_data = k * data_size + int_of_char free in
    Weak.set data pos_data (Some d);
    Array1.set h pos_data full_hash
      

  let make_spill_chain t k index =
    let fat = t.fat in
    let base = k * fat_entries_size in
    let rec aux index =
      let next = Array1.get fat (base + index) in
      if next = end_of_chain
      then Array1.set fat (base + index) spill_chain
      else begin
	  assert (
	      let k = base / fat_entries_size in
	      if next = spill_chain then begin
		  Format.printf "spill spilling: index = %d next = %d@ %a@."
		    index
		    (int_of_char next)
		    (function fmt -> pretty_debug_buckx fmt t) k;
		  false
		end
	      else true);
	  aux (int_of_char next)
	end
    in
    aux index

  let add_level2 t d k index ~this_chain_spills full_hash =
    try
      add_aux t d k index full_hash
    with
      Full ->
	if not this_chain_spills
	then begin (* make it spill then *)
	    make_spill_chain t k index;
	    make_spill t;
	  end;
	let k_spill = spill_index k in
	add_aux t.spill d k_spill index full_hash
	  (* let the exception Full through if it is raised 
	     while already spilling *)

  let merge t1 ~full_hash ~full_shift d =
    let k1, index = get_indexes_k_i ~full_hash ~full_shift in
    let rec find_in_table t k =
      let fat = t.fat in
      let data = t.data in
      let hashes = t.hash in
      let base = k * fat_entries_size in
      let rec loop chain =
	let next = Array1.get fat (base + chain) in
	if next = end_of_chain 
	then begin
	    add_level2 t1 d k1 index ~this_chain_spills:(t1!=t) full_hash;
	    d
	  end
	else if next = spill_chain
	then begin
	    let k_spill = spill_index k in
	    find_in_table t.spill k_spill
	  end
	else begin
	    let next = int_of_char next in
	    let data_index = k * data_size + next in
	    let hash_elt = Array1.get hashes data_index in
	    if hash_elt <> full_hash
	    then loop next
	    else begin
              match Weak.get_copy data data_index with
              | Some v ->
		  if H.equal v d
		  then begin 
		    match Weak.get data data_index with
		    | Some v -> v
		    | None -> 
			(* an interesting race has happened here.
			   Let us reuse the slot that just became available *)
			Weak.set data data_index (Some d);
			(* No "Array1.set hashes" because it is already ok *)
			d
		  end
		  else loop next
              | None -> shorten chain next
	    end
	end
      and shorten chain next =
	Array1.set fat (base + chain) (Array1.get fat (base + next));
	Array1.set fat (base + next) (Array1.get fat (base + free_entry));
	Array1.set fat (base + free_entry) (char_of_int next);
	loop chain
      in
      loop index
    in
    find_in_table t1 k1

  let iter_shallow f t =
    let data = t.data in
    let h = t.hash in
    for i = 0 to pred (Weak.length data) do
      match Weak.get data i with
	None -> ()
      | Some d -> 
	  f d (Array1.get h i)
    done

  let iter t f =
    iter_shallow f t;
    iter_shallow f t.spill

  let check t =
    let fat = t.fat in
    let vref = Array.create 254 true in
    for k = 0 to pred n do
      let v = Array.create 254 false in
      let base = k * fat_entries_size in
      let rec loop chain =
	let next = Array1.get fat (base + chain)
	in
	if next <> end_of_chain && next <> spill_chain
	then begin
	  let next = int_of_char next in
	  assert (not (v.(next)));
	  v.(next) <- true;
	  loop next
	end
      in
      for chain = entries_start to entries_start + 255 do 
	loop chain 
      done;
      loop free_entry;
      assert (v = vref);
    done

  let total_time = ref 0 
  let nb_time = ref 0 
  let forget_spill t =
(*    Format.printf "avant forget_spill:%a@." (function fmt -> pretty_debug_buckx fmt t) 0;*)
    release_spill_fats t.spill.fat;
    t.spill <- empty 

  let collect ~full_shift t = 
    (*    check t; 
	  Format.printf "avant collect:%a@." (function fmt -> pretty_debug_buckx fmt t) 0; *)
    (*    let sum = ref 0 in
	  for i = 0 to pred n do
	  sum := !sum + (free_list_length t.fat (i * fat_entries_size))
	  done; 
	  Format.printf "main. %5d i.e. %3d quartercells/subbucket@." 
	  !sum (!sum / (n/4)); 
	  let sum_s = ref 0 in
	  for i = 0 to pred spill_n do
	  sum_s := !sum_s + (free_list_length t.spill.fat (i * fat_entries_size))
	  done; 
	  Format.printf "spill %5d i.e. %3d quartercells/subbucket@." 
	  !sum_s 
	  (!sum_s / (spill_n/4));
	  Format.printf "space inefficiency %d ppm@." 
	  (1000 * (!sum_s + !sum) / (254 * (n + spill_n))); 
	  let nones = ref 0 in
	  for i = 0 to pred (n * 254) do
	  if not (Weak.check t.data i) then incr nones
	  done;
	  Format.printf "Nones in main. %d@." !nones;
	  let nones_s = ref 0 in
	  for i = 0 to pred (spill_n * 254) do
	  if not (Weak.check t.spill.data i) then incr nones_s
	  done;
	  Format.printf "Nones in spill %d@." !nones_s; *)
    (* Phase one : collect the 'None's in the data *)
    let before = if debug then getperfcount () else 0 in
(*       let fat = t.fat in
	 let data = t.data in
	 for k = 0 to pred n do
	 let base = k * fat_entries_size in
	 let rec loop chain =
	 let next = Array1.get fat (base + chain)
	 in
	 if next <> end_of_chain && next <> spill_chain
	 then begin
	 let next = int_of_char next in
         match Weak.check data (k * data_size + next) with
         | true ->
	 loop next
         | false -> shorten chain next
	 end
	 else Array1.set fat (base + chain) end_of_chain
	 and shorten chain next =
	 Array1.set fat (base + chain) (Array1.get fat (base + next));
	 Array1.set fat (base + next) (Array1.get fat (base + free_entry));
	 Array1.set fat (base + free_entry) (char_of_int next);
	 loop chain
	 in
	 for chain = entries_start to entries_start + 255 do 
	 loop chain 
	 done; 
	 done;
*)     
    collect_Nones t n;  
    if debug 
    then begin
      let after = getperfcount () in
      let tim = after - before in
      total_time := !total_time + tim ;
      incr nb_time;
      Format.printf "buckx: Collect time =%8d tot=%9d (%2d)@." 
	tim !total_time !nb_time;
	check t; 
	(*  check that no Nones are left in a chain *)
	Format.printf "checking forgotten Nones:@.";
	let fat = t.fat in
	let data = t.data in
	for k = 0 to pred n do
	  let base = k * fat_entries_size in
	  let rec loop chain =
	    let next = Array1.get fat (base + chain)
	    in
	    if next <> end_of_chain && next <> spill_chain
	    then begin
		let next = int_of_char next in
		begin
		  match Weak.check data (k * data_size + next) with
		  | true -> ()
		  | false -> Format.printf "k:%4d i:%4d@." k next; 
		end;
		loop next
	      end
	  in   
	  for chain = entries_start to entries_start + 255 do 
	    loop chain 
	  done; 
	  done;
	    Format.printf "checking forgotten Nones done@."; 
      end;
    (* Phase two : unspill *)
    let old_spill = t.spill in
    if old_spill == empty
    then 254
    else begin
      forget_spill t; 
      let copy_datum x full_hash = ignore (merge t ~full_hash ~full_shift x) in 
      iter_shallow copy_datum old_spill;

	(*      let sum = ref 0 in
		for i = 0 to pred n do
		sum := !sum + (free_list_length t.fat (i * fat_entries_size))
		done; 
		Format.printf "apres unspill:@\nmain. %5d@." 
		!sum; *)
	let new_spill = t.spill in
	if new_spill == empty 
	then 254
	else
	  (*	let sum_s = ref 0 in
		for i = 0 to pred spill_n do
		sum_s := !sum_s + (free_list_length t.spill.fat (i * fat_entries_size))
		done; 
		Format.printf "spill %5d@."
		!sum_s ;
	  *)
	  let max_occupied = ref '\000' in
	  let new_spill_fat = new_spill.fat in
	  for k = 0 to pred spill_n
	  do
	    let f = Array1.get new_spill_fat (k * fat_entries_size + free_entry) in
	    if f > !max_occupied then max_occupied := f
	  done;
	  254 - (int_of_char !max_occupied)
      end
end
    


module MakeBig (H : WeakHashable)  : (S with type data = H.t)  = 
struct
  module M = Make(H)
    
  type t = 
      { mutable tables : M.t array ; 
	mutable mask : int ; 
	mutable shift : int ;
	mutable need_to_resize : bool }

        (* [BM/VP] TODO: use the fmt *)
  let pretty_debug _fmt t k = 
    Array.iter 
      (fun t -> 
	 Format.printf "%a@."  (fun fmt t -> M.pretty_debug_buckx fmt t k) t) 
      t.tables

  let pretty _fmt t =
    Array.iter
      (fun t -> Format.printf "%a@." M.pretty t)
      t.tables

  type data = M.data

  let create _sz = 
    { tables = Array.init 1 (fun _i -> M.create ()) ;
      mask = 0 ;
      shift = 0 ;
      need_to_resize = false }

  let release t = Array.iter M.release t.tables
      
  let clear (t:t) = 
    release t;
    t.tables <- Array.init 1 (fun _i -> M.create ());
    t.mask <- 0;
    t.shift <- 0;
    t.need_to_resize <- false

  let find _ _ = assert false

  let add _ _ = assert false

  let unsafe_merge_with_hash t =
    let mask = t.mask in
    let full_shift = t.shift in
    let tables = t.tables in
    fun x full_hash ->
      let n = full_hash land mask in
      M.merge tables.(n) ~full_hash ~full_shift x

  let unsafe_merge t x =
    let full_hash = H.hash x in
    let n = full_hash land t.mask in
    M.merge t.tables.(n) ~full_hash ~full_shift:t.shift x

  let resize t x =
    let old_size = succ t.mask in
    let old_tables = t.tables in
    let new_size = old_size lsl 1 in
    let new_mask = pred new_size in
    let new_tables = Array.create new_size M.empty in
    let new_t = 
      { tables = new_tables ;
	mask = new_mask ; 
	shift = succ t.shift ;
	need_to_resize = false } 
    in
    try
      for i = 0 to new_mask do
	new_tables.(i) <- M.create ();
      done;
      begin try
	for i = 0 to pred old_size do
	  let table = old_tables.(i) in
	  let merge_element = unsafe_merge_with_hash new_t in
	  M.iter 
	    table
	    (fun elt full_hash -> 
	       try
		 ignore (merge_element elt full_hash)
	       with Full -> 
		 t.mask <- new_mask;
		 t.tables <- new_tables;
		 t.shift <- new_t.shift;
		 t.need_to_resize <- false;
		 Format.printf "debug resize %d -> %d : %a@\n%a@."
		   old_size new_size
		   H.pretty elt
		   pretty t;
		 assert false);
	  M.release table;
	done;
      with Full ->
	Format.printf "There was an error in the middle of resizing an internal data structure. Please report@."
      end;
      t.mask <- new_mask;
      t.tables <- new_tables;
      t.shift <- new_t.shift;
      t.need_to_resize <- false;
      unsafe_merge t x
    with Full -> 
      Format.printf "There was an error while the resizing of an internal data structure was almost finished. Please report@.";
      Format.printf "debug resize %d -> %d : %a@\n%a@."
	old_size new_size
	H.pretty x
	pretty t;
      assert false

  let reasonable_size () = 
    match MemoryFootprint.get () with 
      1 -> 8
    | 2 -> 16
    | _ -> 32

  let garbage_collect t = 
    let collect table m =
      min (M.collect ~full_shift:t.shift table) m
    in
    Array.fold_right collect t.tables max_int 

  let last_gc_count = ref 0

  let merge t x = 
    try
      unsafe_merge t x
    with Full -> 
      let old_size = succ t.mask in
      if debug then Format.printf "buckx %s is full (size %d)@." H.id old_size;
      let major () =
	if debug then Format.printf "buckx: Finishing major.@.";
	Gc.major ();
	if t.need_to_resize
	then begin
	  if debug then Format.printf "buckx: Will resize now@.";
	  resize t x
	end
	else begin
	  let min_room = garbage_collect t in
	  if debug then Format.printf "buckx: Recovered %d cells by spill sub-bucket@."
	      min_room;
	  if min_room <= 40 then t.need_to_resize <- true;
	  try
	    unsafe_merge t x
	  with Full -> resize t x
	end
      in
      let major_or_resize () =
	if old_size >=  (reasonable_size ())  
	then major ()
	else begin
	  if debug then Format.printf "buckx: Resizing.@.";
	  resize t x;
	end
      in
      let gc_count = (Gc.quick_stat ()).Gc.major_collections in
      if gc_count > !last_gc_count
      then begin
	if debug then Format.printf "buckx: Collecting without finishing major@.";
	last_gc_count := gc_count;
	let _result = garbage_collect t in
	if debug then Format.printf "buckx: Collected %d@." _result;
	try
	  unsafe_merge t x
	with Full -> major_or_resize ()
      end
      else major_or_resize ()

  let shallow_copy old =
    { tables = old.tables ;
      mask = old.mask ;
      shift = old.shift ;
      need_to_resize = old.need_to_resize }

  let overwrite ~old ~fresh = 
    old.tables <- fresh.tables;
    old.mask <- fresh.mask;
    old.shift <- fresh.shift;
    old.need_to_resize <- fresh.need_to_resize

  let iter _ = assert false
  let mem _ = assert false
  let find_all _ = assert false
  let remove _ = assert false
  let stats _ = assert false
  let count _ = assert false
  let fold _ = assert false

end



let () =
  let gc_params = Gc.get () in
  Gc.set { gc_params with Gc.minor_heap_size = 65536 };
