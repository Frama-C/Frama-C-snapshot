(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licences/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(* This is an implementation of Patricia trees, following Chris Okasaki's
   paper at the 1998 ML Workshop in Baltimore.
   Both big-endian and little-endian trees are provided. Both sets and maps
   are implemented on top of Patricia trees. *)

  (* A tree is
     big-endian if it expects the key's most significant bits to be
     tested first. *)

type prefix = int * int
let sentinel_prefix = (-1) , (-1)    

module Big_Endian = struct

  type mask = int

(* inlined
  let branching_bit p0 p1 =
    let v = p0 lxor p1 in
    (* compute highest bit. 
       First, set all bits with weight less than
       the highest set bit *)
    let v1 = v lsr 1 in
    let v2 = v lsr 2 in
    let v = v lor v1 in
    let v = v lor v2 in
    let v1 = v lsr 3 in
    let v2 = v lsr 6 in
    let v = v lor v1 in
    let v = v lor v2 in
    let v1 = v lsr 9 in
    let v2 = v lsr 18 in
    let v = v lor v1 in
    let v = v lor v2 in
    (* then get highest bit *)
    (succ v) lsr 1
*)

    (* The ``relevant'' bits in an integer [i] are those which are found (strictly) to the left of the single one bit
       in the mask [m]. We keep these bits, and set all others to 0. Okasaki uses a different convention, which allows
       big-endian Patricia trees to masquerade as binary search trees. This feature does not seem to be useful here. *)

    let mask i m =
      i land (lnot (2*m-1))

    (* The smaller [m] is, the more bits are relevant. *)

    let shorter (m:int) (n:int) = m > n
      

  end


(*i ------------------------------------------------------------------------ i*)
(*s \mysection{Patricia-tree-based maps} *)

module type Tagged_type = sig
  include Datatype.S
  val tag : t -> int
end

module Tag_comp : 
sig
  type t
  val get_tag : t -> int
  val get_comp : t -> bool
  val encode : int -> bool -> t
end =
struct
  type t = int
  let get_tag x = x land max_int
  let get_comp x = x < 0 
  let encode tag comp =
    if comp then tag lor min_int else tag
end 

module Comp_unused = 
struct
  let e = false
  let f _ _ = false
  let compose _ _ = false
  let default = false
end

module Make
  (Key:sig
    include Datatype.S
    val id : t -> int
  end)
  (V : Tagged_type)
  (Comp : sig val e: bool val f : Key.t -> V.t -> bool val compose : bool -> bool -> bool val default: bool end)
  (Initial_Values: sig val v : (Key.t * V.t) list list end)
  (Datatype_deps: sig val l : State.t list end)
 =
struct

    type key = Key.t

    type leaf_annot = bool
    type branch_annot = Tag_comp.t

    type tag = int

    (* A tree is either empty, or a leaf node, containing both
       the integer key and a piece of data, or a binary node.
       Each binary node carries two integers. The first one is
       the longest common prefix of all keys in this
       sub-tree. The second integer is the branching bit.
       It is an integer with a single one bit (i.e. a power of 2),
       which describes the bit being tested at this node. *)

    type tt =
      | Empty
      | Leaf of Key.t * V.t * bool
      | Branch of int * Big_Endian.mask * tt * tt * Tag_comp.t

    let compare =
      if Key.compare == Datatype.undefined ||
        V.compare == Datatype.undefined 
      then (
(*          Kernel.debug "(%s, %s) ptmap, missing comparison function: %b %b"
            (Type.name Key.ty) (Type.name V.ty)
            (Key.compare == Datatype.undefined)
            (V.compare == Datatype.undefined); *)
          Datatype.undefined
        )
      else 
	let rec compare t1 t2 = 
	  match t1, t2 with
          | Empty, Empty -> 0
          | Empty, _ -> -1
          | _, Empty -> 1
          | Leaf (k1,x1,_), Leaf (k2,x2,_) ->
	      let c = Key.compare k1 k2 in 
	      if c <> 0 then c else V.compare x1 x2
          | Leaf _, Branch _ -> -1
          | Branch _, Leaf _ -> 1
          | Branch (_p1,_m1,_l1,_r1,t1), Branch (_p2,_m2,_l2,_r2,t2) ->
	      let t1 = Tag_comp.get_tag t1 in
	      let t2 = Tag_comp.get_tag t2 in
              Datatype.Int.compare t1 t2
		(* Taken and adapted from JCF code for the implementation
                   without tag *)
		(*let c = Datatype.Int.compare p1 p2 in
	          if c <> 0 then c else
	          let c = Big_endian.compare m1 m2 in
	          if c <> 0 then c else
                  let c = compare l1 l2 in
                  if c <> 0 then c else
                  compare r1 r2
		*)
	in compare


    let contains_single_binding t =
      match t with
      | Empty
      | Branch _ -> None
      | Leaf (key, data, _) -> Some (key, data)

    let comp t = 
      match t with
	Empty -> Comp.e
      | Leaf (_,_,c) -> c
      | Branch (_,_,_,_,tc) -> Tag_comp.get_comp tc

    let rec min_binding t =
      match t with
	Empty -> raise Not_found
      | Branch (_,_,left,_,_) -> min_binding left
      | Leaf (key, data, _) -> key, data

    let rec max_binding t =
      match t with
	Empty -> raise Not_found
      | Branch (_,_,_,right,_) -> max_binding right
      | Leaf (key, data, _) -> key, data

    let rec iter f htr = 
      match htr with
      | Empty -> ()
      | Leaf (key, data, _) ->
	  f key data
      | Branch (_, _, tree0, tree1, _tl) ->
	  iter f tree0;
	  iter f tree1

    let prettykv fmt k v =
      Format.fprintf fmt "%a -> %a@."
	Key.pretty k
	V.pretty v

    let pretty fmt tree =
      Format.fprintf fmt "[[@.";
      iter (prettykv fmt) tree;
      Format.fprintf fmt "]]@."

    let tag tr =
      match tr with
	Empty -> 27
      | Leaf (k, v, _) -> Key.id k + 547 * V.tag v
      | Branch (_, _, _, _, tl) -> Tag_comp.get_tag tl

    let hash_internal tr =
	match tr with
	  Empty | Leaf _ -> tag tr
	| Branch(p,m,l,r, _tag) -> m + 3 * p + 2017 * (tag l) + (tag r)

    let hash_debug = hash_internal

    let equal_internal htr1 htr2 =
      (* do not use == or compare the toplevel tags. One of the arguments is
	 not hashconsed yet when this function is called *)
      match htr1, htr2 with
	Empty, Empty -> true
      | Leaf(k1, v1, _), Leaf(k2, v2, _) ->
	  Key.equal k1 k2 && (V.equal v1 v2)
      | Branch(p1,m1,l1,r1,_), Branch(p2,m2,l2,r2,_) ->
	  p1 = p2 && m1 = m2 && l1 == l2 && r1 == r2
      | _,_ -> false

    (* [iter f m] invokes [f k x], in turn, for each binding
       from key [k] to element [x] in the map [m]. Keys are
       presented to [f] according to some unspecified, but fixed, order. *)

    let empty = Empty

    let current_tag_before_initial_values = 1
    let current_tag = ref current_tag_before_initial_values
    let current_table = ref 0

(*
    let project_offset () =
      try
	100000 * (1 + ((Project.hash (Project.current ())) mod 999))
      with
	Project.NoProject -> 0
*)


    let initial_values =
      List.map
	(function [k,v] -> Leaf (k, v, Comp.f k v)
	| [] -> Empty
	| _ -> assert false)
	Initial_Values.v

    let rehash_ref = ref (fun _ -> assert false)

    module Datatype =
      Datatype.Make
	(struct
	  type t = tt
	  let name = "(" ^ Type.name Key.ty ^ ", " ^ Type.name V.ty ^ ") ptmap"
	  open Structural_descr
	  let r = Recursive.create ()
	  let structural_descr =
	    Structure
	      (Sum
		 [| [| Key.packed_descr; V.packed_descr; p_abstract |];
		    [| p_abstract;
		       p_abstract;
		       recursive_pack r;
		       recursive_pack r;
		       p_abstract |] |])
	  let () = Recursive.update r structural_descr
	  let reprs = [ Empty ]
	  let equal = ( == )
	  let compare = compare
	  let hash = tag
	  let rehash x = !rehash_ref x
	  let copy = Datatype.undefined
	  let internal_pretty_code = Datatype.pp_fail
	  let pretty = pretty
	  let varname = Datatype.undefined
	  let mem_project = Datatype.never_any_project
	 end)
    let () = Type.set_ml_name Datatype.ty None
    include Datatype

    module PatriciaHashtbl = Hashtbl.Make(Datatype)

    module PatriciaHashconsTbl =
      State_builder.Hashconsing_tbl
	(struct
	  include Datatype
	  let equal_internal = equal_internal
	  let hash_internal = hash_internal
	  let initial_values = initial_values
	end)
	(struct
          let name = Type.name ty ^ " hashconsing table"
          let dependencies = Datatype_deps.l
          let size = 137
          let kind = `Internal
	end)

    let self = PatriciaHashconsTbl.self

(*    let inform_counter = ref 0

    let inform() =
      let n = succ !inform_counter in
      inform_counter := n;
      if n land 16383 = 0
      then
	let c = PatriciaHashconsTbl.count () in
	Format.printf "%6d nodes %s@." c name
*)


    let wrap_Leaf k v =
  (*    inform(); *)
      assert (Key.id k >= 0); 
      (* The test k < p+m 
	 and the implementation of [highest_bit]
	 do not work with negative keys. *)
      let new_tr = Leaf (k, v, Comp.f k v) in
      PatriciaHashconsTbl.merge  new_tr

    let wrap_Branch p m l r =
(*      inform(); *)
      let tag = !current_tag in
      let comp = Comp.compose (comp l) (comp r) in
      let comp = 
	match l, r with
	| Branch (_,ml,_,_,_), Branch (_,mr,_,_,_) when ml + mr = m  ->
	    comp
	| Leaf (_,_,_), Leaf (_,_,_) -> 
	    comp
	| _ -> Comp.compose Comp.default comp
      in
      let new_tr = Branch (p, m, l, r, Tag_comp.encode tag comp) in
      let result = PatriciaHashconsTbl.merge new_tr in
      if result == new_tr
      then current_tag := (succ tag) land max_int ;
      result

    let rehash_node = function
      | Empty -> Empty
      | Leaf (k, v, _) -> wrap_Leaf k v
      | Branch (p,m,l,r,_) -> wrap_Branch p m l r

    let () = rehash_ref := rehash_node

    (* [find k m] looks up the value associated to the key [k] in the map [m],
       and raises [Not_found] if no value is bound to [k].

       This implementation takes branches \emph{without} checking whether the
       key matches the prefix found at the current node. This means that a
       query for a non-existent key shall be detected only when finally
       reaching a leaf, rather than higher up in the tree. This strategy is
       better when (most) queries are expected to be successful. *)

    let find key htr =
      let id = Key.id key in
      let rec find htr =
	match htr with
	| Empty ->
	    raise Not_found
	| Leaf (key', data, _) ->
	    if Key.equal key key' then
	      data
	    else
	      raise Not_found
	| Branch (_, mask, tree0, tree1, _) ->
	    find (if (id land mask) = 0 then tree0 else tree1)
      in
      find htr

 
		

    let mem key htr =
      let id = Key.id key in
      let rec find htr =
	match htr with
	| Empty ->
            false
	| Leaf (key', _, _) ->
	    Key.equal key key'
	| Branch (_, mask, tree0, tree1, _) ->
	    find (if (id land mask) = 0 then tree0 else tree1)
      in
      find htr


    (* The auxiliary function [join] merges two trees in the simple case where
       their prefixes disagree.

       Assume $t_0$ and $t_1$ are non-empty trees, with longest common prefixes
       $p_0$ and $p_1$, respectively. Further, suppose that $p_0$ and $p_1$
       disagree, that is, neither prefix is contained in the other. Then, no
       matter how large $t_0$ and $t_1$ are, we can merge them simply by
       creating a new [Branch] node that has $t_0$ and $t_1$ as children! *)
    let join p0 t0 p1 t1 =
      let m = (* Big_Endian.branching_bit p0 p1 in (inlined) *)
	let v = p0 lxor p1 in
	(* compute highest bit. 
	   First, set all bits with weight less than
	   the highest set bit *)
	let v1 = v lsr 1 in
	let v2 = v lsr 2 in
	let v = v lor v1 in
	let v = v lor v2 in
	let v1 = v lsr 3 in
	let v2 = v lsr 6 in
	let v = v lor v1 in
	let v = v lor v2 in
	let v1 = v lsr 9 in
	let v2 = v lsr 18 in
	let v = v lor v1 in
	let v = v lor v2 in
	(* then get highest bit *)
	(succ v) lsr 1
      in      
      let p = Big_Endian.mask p0 (* for instance *) m in
      if (p0 land m) = 0 then
	wrap_Branch p m t0 t1
      else
	wrap_Branch p m t1 t0

    (* The auxiliary function [match_prefix] tells whether a given key has a
       given prefix. More specifically, [match_prefix k p m] returns [true] if
       and only if the key [k] has prefix [p] up to bit [m].

       Throughout our implementation of Patricia trees, prefixes are assumed to
       be in normal form, i.e. their irrelevant bits are set to some
       predictable value. Formally, we assume 
       [Big_Endian.mask p m] equals [p] whenever
       [p] is a prefix with [m] relevant bits. This allows implementing
       [match_prefix] using only one call to [Big_Endian.mask]. 
       On the other hand, this
       requires normalizing prefixes, as done e.g. in [join] above, where
       [Big_Endian.mask p0 m] has to be used instead of [p0]. *)
    let match_prefix k p m =
      Big_Endian.mask k m = p

    let pretty_prefix (p,m) fmt tree =
      let rec pretty_prefix_aux tree = 
	match tree with
	  Empty -> ()
	| Leaf (k,v,_) -> 
	    if match_prefix (Key.id k) p m then prettykv fmt k v
	| Branch(p1,m1,l,r,_) ->
	    if m1 <= m
	    then begin
		if match_prefix p1 p m then iter (prettykv fmt) tree;
	      end
	    else if p land m1 = 0
	    then pretty_prefix_aux l
	    else pretty_prefix_aux r
      in
      Format.fprintf fmt "[[@.";
      pretty_prefix_aux tree;
      Format.fprintf fmt "]]@."

    type subtree = tt
    exception Found_prefix of prefix * subtree * subtree

    let rec comp_prefixes t1 t2 =
      assert (t1 != t2);
      let all_comp = comp t1 && comp t2 in
      match t1, t2 with
	Leaf (k1, _v1, _), Leaf (k2, _v2, _) ->
	  if Key.equal k1 k2 && all_comp 
	  then begin
(*	      Format.printf "PREF leaves:@.";
	      prettykv Format.std_formatter k1 _v1;
	      prettykv Format.std_formatter k1 _v2;  *)
	      raise (Found_prefix((Key.id k1, -1), t1, t2))
	    end
      | Branch (p1, m1, l1, r1, _), Branch (p2, m2, l2, r2, _) ->
	  if (p1 = p2) & (m1 = m2) 
	  then begin
	      if all_comp then begin
(*		  Format.printf "PREF subtree:@.";
		  pretty Format.std_formatter t1;
		  pretty Format.std_formatter t2;  *)
		  raise (Found_prefix((p1 ,m1), t1, t2));
		end;
	      let go_left = l1 != l2 in
	      if go_left
	      then begin
		  let go_right = r1 != r2 in
		  if go_right then comp_prefixes r1 r2;
		  comp_prefixes l1 l2;
		end
	      else begin
		  assert (r1 != r2);
		  comp_prefixes r1 r2;
		end
	    end
	  else if (Big_Endian.shorter m1 m2) & (match_prefix p2 p1 m1) 
	  then 
	    let sub1 = if (p2 land m1) = 0 then l1 else r1 in
	    if sub1 != t2 then comp_prefixes sub1 t2
	  else if (Big_Endian.shorter m2 m1) & (match_prefix p1 p2 m2)
	  then
	    let sub2 = if (p1 land m2) = 0 then l2 else r2 in
	    if sub2 != t1 then
	    comp_prefixes t1 sub2
      | _, _ -> ()
	  
    let rec find_prefix t (p, m as prefix) = 
      match t with
	Empty -> None
      | Leaf (k, _, c) -> 
	  if Key.id k = p && m = -1 && c then Some t else None
      | Branch (p1, m1, l, r, tc) ->
	  if p1 = p && m1 = m 
	  then (if Tag_comp.get_comp tc then Some t else None)
	  else if Big_Endian.shorter m m1
	  then None
	  else if match_prefix p p1 m1
	  then find_prefix (if p land m1 = 0 then l else r) prefix
	  else None

    let hash_subtree = tag

    let equal_subtree = equal

    (* [fine_add decide k d m] returns a map whose bindings are all bindings in
       [m], plus a binding of the key [k] to the datum [d]. If a binding from
       [k] to [d0] already exists, then the resulting map contains a binding
       from [k] to [decide d0 d]. *)

    type 'a decision = 'a -> 'a -> 'a

    exception Unchanged

    let basic_add decide k d m =
      let id = Key.id k in
      let rec add t =
	match t with
	| Empty ->
	    wrap_Leaf k d
	| Leaf (k0, d0, _) ->
	    if Key.equal k k0 then
	      let d' = decide k d0 d in
	      if d' == d0 then
		raise Unchanged
	      else
		wrap_Leaf k d'
	    else
	      join (Key.id k) (wrap_Leaf k d) (Key.id k0) t
	| Branch (p, m, t0, t1, _) ->
	    if match_prefix id p m then
	      if (id land m) = 0 then wrap_Branch p m (add t0) t1
	      else wrap_Branch p m t0 (add t1)
	    else
	      join id (wrap_Leaf k d) p t
      in
      add m

    let strict_add k d m =
      basic_add (fun _ _ -> raise Unchanged) k d m

    let fine_add decide k d m =
      try
	basic_add decide k d m
      with Unchanged ->
	m

    (** [add k d m] returns a map whose bindings are all bindings in [m], plus
	a binding of the key [k] to the datum [d]. If a binding already exists
	for [k], it is overridden. *)
    let add k d m =
      fine_add (fun _ _old_binding new_binding -> new_binding) k d m

    (** [singleton k d] returns a map whose only binding is from [k] to [d]. *)
    let singleton k d =
      wrap_Leaf k d

    (** [is_singleton m] returns [Some (k, d)] if [m] is a singleton map
       that maps [k] to [d]. Otherwise, it returns [None]. *)
    let is_singleton htr = match htr with
    | Leaf (k, d, _) ->
	Some (k, d)
    | Empty
    | Branch _ ->
	None

    (** [is_empty m] returns [true] if and only if the map [m] defines no
	bindings at all. *)
    let is_empty htr = match htr with
    | Empty ->
	true
    | Leaf _
    | Branch _ ->
	false

    (** [cardinal m] returns [m]'s cardinal, that is, the number of keys it
	binds, or, in other words, its domain's cardinal. *)
    let rec cardinal htr = match htr with
    | Empty ->
	0
    | Leaf _ ->
	1
    | Branch (_, _, t0, t1,  _) ->
	cardinal t0 + cardinal t1

    (** [remove k m] returns the map [m] deprived from any binding involving
	[k]. *)
    let remove key m =
      let id = Key.id key in
      let rec remove htr  = match htr with
	| Empty ->
	    raise Not_found
	| Leaf (key',  _, _) ->
	    if Key.equal key key' then
	      Empty
	    else
	      raise Not_found
	| Branch (prefix, mask, tree0, tree1,  _) ->
	    if (id land mask) = 0 then
	      let rtree0 = remove tree0 in
	      match rtree0 with
	      | Empty ->
		  tree1
	      | _ ->
		  wrap_Branch prefix mask rtree0 tree1
	    else
	      let rtree1 = remove tree1 in
	      match rtree1 with
	      | Empty ->
		  tree0
	      | _ ->
		  wrap_Branch prefix mask tree0 rtree1
      in
      try
	remove m
      with Not_found ->
	m

(*
    (** [find_and_remove k m] looks up the value [v] associated to the key [k]
	in the map [m], and raises [Not_found] if no value is bound to [k]. The
	call returns the value [v], together with the map [m] deprived from the
	binding from [k] to [v]. *)
    let find_and_remove key htr =
      let id = Key.id key in
      let rec find_and_remove htr =
      match htr with
      | Empty ->
	  raise Not_found
      | Leaf (key', data, _) ->
	  if Key.equal key key' then
	    data, Empty
	  else
	    raise Not_found
      | Branch (prefix, mask, tree0, tree1,  _) ->
	  if (id land mask) = 0 then
	    match find_and_remove tree0 with
	    | data, Empty  ->
		data, tree1
	    | data, tree0 ->
		data, (wrap_Branch prefix mask tree0 tree1)
	  else
	    match find_and_remove tree1 with
	    | data, Empty ->
		data, tree0
	    | data, tree1 ->
		data, (wrap_Branch prefix mask tree0 tree1)
      in
      find_and_remove htr
      *)

    (* [fine_union decide m1 m2] returns the union of the maps [m1] and
       [m2]. If a key [k] is bound to [x1] (resp. [x2]) within [m1]
       (resp. [m2]), then [decide] is called. It is passed [x1] and [x2], and
       must return the value which shall be bound to [k] in the final map. The
       operation returns [m2] itself (as opposed to a copy of it) when its
       result is equal to [m2]. *)

    let reverse decision k elem1 elem2 =
      decision k elem2 elem1

    let fine_union decide m1 m2 =
      let rec union s t =
	match s, t with
	| Empty, _ ->
	    t
	| (Leaf _ | Branch _), Empty ->
	    s
	| Leaf(key, value, _), _ ->
	    fine_add (reverse decide) key value t
	| Branch _, Leaf(key, value, _) ->
	    fine_add decide key value s
	| Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	    if (p = q) & (m = n) then
  	      (* The trees have the same prefix. Merge their sub-trees. *)
	      let u0 = union s0 t0
	      and u1 = union s1 t1 in
	      if t0 == u0 && t1 == u1 then t
	      else (wrap_Branch p m u0 u1)
	    else if (Big_Endian.shorter m n) & (match_prefix q p m) then
  	      (* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)
	      if (q land m) = 0 then
		(wrap_Branch p m (union s0 t) s1)
	      else
		(wrap_Branch p m s0 (union s1 t))
	    else if (Big_Endian.shorter n m) & (match_prefix p q n) then
	      (* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)
	      if (p land n) = 0 then
		let u0 = union s t0 in
		if t0 == u0 then t
		else (wrap_Branch q n u0 t1)
	      else
		let u1 = union s t1 in
		if t1 == u1 then t
		else (wrap_Branch q n t0 u1)
	    else
	      (* The prefixes disagree. *)
	      join p s q t
      in
      union m1 m2

    (** [union m1 m2] returns the union of the maps [m1] and [m2]. Bindings in
	[m2] take precedence over those in [m1]. The operation returns [m2]
	itself (as opposed to a copy of it) when its result is equal to
	[m2]. *)
    let union m1 m2 =
      fine_union (fun _ _ d' -> d') m1 m2

    (** [fold f m seed] invokes [f k d accu], in turn, for each binding from
	key [k] to datum [d] in the map [m]. Keys are presented to [f] in
	increasing order according to the map's ordering. The initial value of
	[accu] is [seed]; then, at each new call, its value is the value
	returned by the previous invocation of [f]. The value returned by
	[fold] is the final value of [accu]. *)
    let rec fold f m accu =
      match m with
      | Empty ->
	  accu
      | Leaf (key, data, _) ->
	  f key data accu
      | Branch (_, _, tree0, tree1, _) ->
	  fold f tree1 (fold f tree0 accu)

    (** [fold_rev] performs exactly the same job as [fold], but presents keys
	to [f] in the opposite order. *)
    let rec fold_rev f m accu =
      match m with
      | Empty ->
	  accu
      | Leaf (key, data, _) ->
	  f key data accu
      | Branch (_, _, tree0, tree1, _) ->
	  fold_rev f tree0 (fold_rev f tree1 accu)

    (** [map f m] returns the map obtained by composing the map [m] with the
	function [f]; that is, the map $k\mapsto f(m(k))$. *)
    let rec map f htr = match htr with
      | Empty ->
	  Empty
      | Leaf (key, data, _) ->
	  wrap_Leaf key (f data)
      | Branch (p, m, tree0, tree1, _) ->
	  wrap_Branch p m (map f tree0) (map f tree1)

    (** [endo_map] is similar to [map], but attempts to physically share its
	result with its input. This saves memory when [f] is the identity
	function. *)
    let rec endo_map f tree =
      match tree with
      | Empty ->
	  tree
      | Leaf (key, data, _) ->
	  let data' = f key data in
	  if data == data' then
	    tree
	  else
	    wrap_Leaf key data'
      | Branch (p, m, tree0, tree1, _) ->
	  let tree0' = endo_map f tree0 in
	  let tree1' = endo_map f tree1 in
	  if (tree0' == tree0) && (tree1' == tree1) then
	    tree
	  else
	    wrap_Branch p m tree0' tree1'

    let generic_fine_add decide k d m =
      (* there is an implicit argument which is a tree with a single binding.
	  Where the calls to [decide] are concerned, this implicit tree is the
	  second one *)
      let id = Key.id k in
      let rec add t =
	match t with
	|	Empty ->
	    wrap_Leaf k (decide k None (Some d))
	|	Leaf (k0, d0, _) ->
	    if Key.equal k k0 then
	      let d' = decide k (Some d0) (Some d) in
	      if d'==d0 then t else wrap_Leaf k d'
	    else
	      let endo =
		let decided = decide k0 (Some d0) None in
		if decided == d0 then t else wrap_Leaf k0 decided
	      in
	      join id (wrap_Leaf k (decide k None (Some d))) (Key.id k0) endo
	|	Branch (p, m, t0, t1, _) ->
	    if match_prefix id p m then
	      if (id land m) = 0 then
		let a_t0 = add t0 in
		let endo = endo_map (fun k x -> decide k (Some x) None) t1 in
		if a_t0 == t0 && endo == t1 then t
		else wrap_Branch p m a_t0 endo
	      else
		let a_t1 = add t1 in
		let endo =  endo_map (fun k x -> decide k (Some x) None) t0
		in
		if a_t1 == t1 && endo == t0 then t
		else wrap_Branch p m endo a_t1
	    else
	      let endo = endo_map (fun k x -> decide k (Some x) None) t in
	      join id (wrap_Leaf k (decide k None (Some d))) p endo
      in
      add m

     module Cacheable =
      struct
	type t = tt
	let hash = tag
	let sentinel = Empty
	let equal = (==)
      end

      module R =
      struct
	type t = tt
	let sentinel = Empty
      end

      exception Found of t

      let symetric_merge ~cache ~decide_none ~decide_some =
	let symetric_fine_add k d m =
	  (* this function to be called when one of the trees
	     is a single binding *)
	  let id = Key.id k in
	  let rec add t =
	    match t with
	    |	Empty ->
		  wrap_Leaf k (decide_none k d )
	    |	Leaf (k0, d0, _) ->
		  if Key.equal k k0 then
		    let d' = decide_some d0 d in
		    if d'==d0 then t else wrap_Leaf k d'
		  else
		    let endo =
		      let decid = decide_none k0 d0 in
		      if decid == d0 then t else wrap_Leaf k0 decid
		    in
		    join id (wrap_Leaf k (decide_none k d)) (Key.id k0) endo
	    |	Branch (p, m, t0, t1, _) ->
		  if match_prefix id p m then
		    if (id land m) = 0 then
		      let a_t0 = add t0 in
		      let endo = endo_map decide_none t1 in
		      if a_t0 == t0 && endo == t1 then t
		      else wrap_Branch p m a_t0 endo
		    else
		      let a_t1 = add t1 in
		      let endo =  endo_map decide_none t0
		      in
		      if a_t1 == t1 && endo == t0 then t
		      else wrap_Branch p m endo a_t1
		  else
		    let endo = endo_map decide_none t in
		    join id (wrap_Leaf k (decide_none k d)) p endo in

	  add m
	in
	let _name, _cache = cache in
	let module SymetricCache =
	  Binary_cache.Make_Symetric(Cacheable)(R)
	in
	Project.register_todo_before_clear (fun _ -> SymetricCache.clear ());
	let rec union s t =
	  if s==t then s else
	    SymetricCache.merge uncached_union s t
	and uncached_union s t =
	  match s, t with
	  | Empty, t | t, Empty ->
	      endo_map decide_none t
	  | Leaf(key, value, _), t | t, Leaf(key, value, _) ->
	      symetric_fine_add key value t
	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if (p = q) & (m = n) 
	      then
  		(* The trees have the same prefix. Merge their sub-trees. *)
		let u0 = union s0 t0
		and u1 = union s1 t1 in
		if t0 == u0 && t1 == u1 then t
		else wrap_Branch p m u0 u1

	      else if (Big_Endian.shorter m n) & (match_prefix q p m) then

  		(* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)

		if (q land m) = 0 then
		  let s0_t = union s0 t in
		  let s1_e = union s1 Empty in
		  if s0_t == s0 && s1_e == s1 then s
		  else wrap_Branch p m s0_t s1_e
		else
		  let s0_e = union s0 Empty in
		  let s1_t = union s1 t in
		  if s0_e == s0 && s1_t == s1 then s
		  else wrap_Branch p m s0_e s1_t

	      else if (Big_Endian.shorter n m) & (match_prefix p q n) then

		(* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)

		if (p land n) = 0 then
		  let s_t0 = union s t0 in
		  let e_t1 = union Empty t1 in
		  if t0 == s_t0 && e_t1 == t1 then t
		  else wrap_Branch q n s_t0 e_t1
		else
		  let s_t1 = union s t1 in
		  let e_t0 =  union Empty t0 in
		  if t1 == s_t1 && e_t0 == t0 then t
		  else wrap_Branch q n e_t0 s_t1
	      else
		(* The prefixes disagree. *)
		join p (union s Empty) q (union Empty t)
	in union

      let generic_merge ~cache ~decide =
	let _name, _cache = cache in
	let cache_merge =
	  if _cache = 0
	  then fun f x y -> f x y
	  else begin
	      let module Cache = Binary_cache.Make_Asymetric(Cacheable)(R)
	      in
	      Project.register_todo_before_clear (fun _ -> Cache.clear ());
	      Cache.merge
	    end
	in
	let rec union s t = if s==t then s else cache_merge compute s t
	and compute s t =
	  match s, t with
	  | Empty, _ ->
	      endo_map (fun k x -> decide k None (Some x)) t
	  | (Leaf _ | Branch _), Empty ->
	      endo_map (fun k x -> decide k (Some x) None) s

	  | Leaf(key, value, _), _ ->
	      generic_fine_add (reverse decide) key value t
	  | Branch _, Leaf(key, value, _) ->
	      generic_fine_add decide key value s

	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if (p = q) & (m = n) then

  		(* The trees have the same prefix. Merge their sub-trees. *)

		let u0 = union s0 t0
		and u1 = union s1 t1 in
		if t0 == u0 && t1 == u1 then t
		else wrap_Branch p m u0 u1

	      else if (Big_Endian.shorter m n) & (match_prefix q p m) then

  		(* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)

		if (q land m) = 0 then
		  let s0_t = union s0 t in
		  let s1_e = union s1 Empty in
		  if s0_t == s0 && s1_e == s1 then s
		  else wrap_Branch p m s0_t s1_e
		else
		  let s0_e = union s0 Empty in
		  let s1_t = union s1 t in
		  if s0_e == s0 && s1_t == s1 then s
		  else wrap_Branch p m s0_e s1_t

	      else if (Big_Endian.shorter n m) & (match_prefix p q n) then

		(* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)

		if (p land n) = 0 then
		  let s_t0 = union s t0 in
		  let e_t1 = union Empty t1 in
		  if t0 == s_t0 && e_t1 == t1 then t
		  else wrap_Branch q n s_t0 e_t1
		else
		  let s_t1 = union s t1 in
		  let e_t0 =  union Empty t0 in
		  if t1 == s_t1 && e_t0 == t0 then t
		  else wrap_Branch q n e_t0 s_t1
	      else
		(* The prefixes disagree. *)
		join p (union s Empty) q (union Empty t)
	in
	union

      let make_predicate 
	  cache_merge exn use_comp ~decide_fst ~decide_snd ~decide_both 
	  =
      let rec inclusion s t =
	if s!=t then
	  if use_comp && comp t 
	  then raise exn;
        match s, t with
        | Empty, _ ->
	    iter decide_snd t
        | (Leaf _ | Branch _), Empty ->
	    iter decide_fst s

        | Leaf(k1, v1, _), Leaf(k2, v2, _) ->
	    if Key.id k1 = Key.id k2 
	    then decide_both v1 v2
	    else begin
		decide_fst k1 v1;
		decide_snd k2 v2;
	      end
	| Leaf(key, _value, _), Branch(p,m,l,r,_) ->
	    let i = Key.id key in
	    if i < p+m
	    then begin
		inclusion s l;
		inclusion Empty r;
	      end
	    else begin
		inclusion Empty l;
		inclusion s r;
	      end
        | Branch (p,m,l,r,_) , Leaf(key, _value, _) ->
	    let i = Key.id key in
	    if i < p+m
	    then begin
		inclusion l t;
		inclusion r Empty;
	      end
	    else begin
		inclusion l Empty;
		inclusion r t;
	      end
        | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	    let compute s t =
	      try
		if (p = q) & (m = n) then
		  begin
  		    (* The trees have the same prefix. Compare their sub-trees. *)
		    inclusion s0 t0;
		    inclusion s1 t1
		  end
		else if (Big_Endian.shorter m n) & (match_prefix q p m) then

  		  (* [q] contains [p]. Compare [t] with a sub-tree of [s]. *)

		  if (q land m) = 0 then
		    begin
		      inclusion s0 t;
		      inclusion s1 Empty;
		    end
		  else
		    begin
		      inclusion s0 Empty;
		      inclusion s1 t
		    end
		else if (Big_Endian.shorter n m) & (match_prefix p q n) then

		  (* [p] contains [q]. Compare [s] with a sub-tree of [t]. *)

		  if (p land n) = 0 then
		    begin
		      inclusion s t0;
		      inclusion Empty t1
		    end
		  else
		    begin
		      inclusion s t1;
		      inclusion Empty t0
		    end
		else
		  begin
		    (* The prefixes disagree. *)
		    inclusion s Empty;
		    inclusion Empty t;
		  end;
		true
	      with e when e = exn -> false
	      | _ -> assert false
	    in
	    let result = cache_merge compute s t in
	    if not result then raise exn
      in
      inclusion 

    let generic_is_included exn ~cache ~decide_fst ~decide_snd ~decide_both =
      let _name, _cache = cache in
      let use_comp = _name = "lmap" in
      let module Cache =
	Binary_cache.Make_Binary(Cacheable)(Cacheable)
      in
      Project.register_todo_before_clear (fun _ -> Cache.clear ());
      make_predicate 
	Cache.merge
	exn
	use_comp
	~decide_fst ~decide_snd ~decide_both 

    let generic_symetric_existential_predicate exn ~decide_one ~decide_both =
      let use_comp = false in
      let module Cache =
	Binary_cache.Make_Symetric_Binary(Cacheable)
      in
      Project.register_todo_before_clear (fun _ -> Cache.clear ());
      make_predicate 
	Cache.merge
	exn
	use_comp
	~decide_fst:decide_one ~decide_snd:decide_one ~decide_both 

    let cached_fold ~cache ~temporary ~f ~joiner ~empty =
      let _name, cache = cache in
      let table = PatriciaHashtbl.create cache in
      if not temporary then
	Project.register_todo_before_clear 
	  (fun _-> PatriciaHashtbl.clear table);
      let counter = ref 0 in
      fun m ->
	let rec traverse t =
	  match t with
	    Empty -> empty
	  | Leaf(key, value, _) ->
	      f key value
	  | Branch(_p, _m, s0, s1, _) ->
	      try
		let result = PatriciaHashtbl.find table t in
(*		Format.printf "find %s %d@." name !counter; *)
		result
	      with Not_found ->
		let result0 = traverse s0 in
		let result1 = traverse s1 in
		let result = joiner result0 result1 in
		incr counter;
		if !counter >= cache
		then begin
		    (*	    Format.printf "Clearing %s fold table@." name;*)
		    PatriciaHashtbl.clear table;
		    counter := 0;
		  end;
(*		Format.printf "add  %s %d@." name !counter; *)
		PatriciaHashtbl.add table t result;
		result
	in
	traverse m

  let cached_map ~cache ~temporary ~f =
      let _name, cache = cache in
      let table = PatriciaHashtbl.create cache in
      if not temporary then
	Project.register_todo_before_clear
	  (fun _ -> PatriciaHashtbl.clear table);
      let counter = ref 0 in
      fun m ->
	let rec traverse t =
	  match t with
	    Empty -> empty
	  | Leaf(key, value, _) ->
	      wrap_Leaf key (f key value)
	  | Branch(p, m, s0, s1, _) ->
	      try
		let result = PatriciaHashtbl.find table t in
(*		Format.printf "find %s %d@." name !counter; *)
		result
	      with Not_found ->
		let result0 = traverse s0 in
		let result1 = traverse s1 in
		let result = wrap_Branch p m result0 result1 in
		incr counter;
		if !counter >= cache
		then begin
		    (*	    Format.printf "Clearing %s fold table@." name;*)
		    PatriciaHashtbl.clear table;
		    counter := 0;
		  end;
(*		Format.printf "add  %s %d@." name !counter; *)
		PatriciaHashtbl.add table t result;
		result
	in
	traverse m

    let rec split key htr =
      let id = Key.id key in
      let rec aux = function
        | Empty ->
            (Empty, None, Empty)
        | Leaf (key', data, _) ->
            if Key.equal key key' then
              (Empty, Some data, Empty)
            else
              (Empty, None, Empty)
      | Branch(_, mask, l, r, _) ->
          if (id land mask) = 0 then
            let (ll, pres, rl) = aux l in (ll, pres, union rl r)
          else
            let (lr, pres, rr) = aux r in (union l lr, pres, rr)
      in
      aux htr

end


(*
Local Variables:
compile-command: "make -C .."
End:
*)
