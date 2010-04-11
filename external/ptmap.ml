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

(*i ----------------------------------------------------------------------- i*)
(*s \mysection{Little-endian vs big-endian trees} *)

  (* A tree is little-endian if it expects the key's least significant bits
     to be tested first during a search. It is
     big-endian if it expects the key's most significant bits to be
     tested first.

     Most of the code is independent of this design choice, so it is
     written as a functor, parameterized by a small
     structure which defines endianness. Here is the interface which
     must be adhered to by such a structure. *)

let counter = ref 0

let hashconsingtable_counter = ref 0

let debug = ref false

module Big_Endian = struct

  type mask = int

  let lowest_bit x =
    x land (-x)

(*
  let rec highest_bit x =
    let m = lowest_bit x in
    if x = m then
      m
    else
      highest_bit (x - m)
*)
  let highest_bit v =
    let v = v lor (v lsr 1) in
    let v = v lor (v lsr 2) in
    let v = v lor (v lsr 4) in
    let v = v lor (v lsr 8) in
    let v = v lor (v lsr 16) in
    (succ v) lsr 1
      

    (* Performing a logical ``xor'' of [i0] and [i1] yields a bit field where all differences between [i0] and [i1]
       show up as one bits. (There must be at least one, since [i0] and [i1] are distinct.) The ``first'' one is
       the highest bit in this bit field, since we are checking most significant bits first.

       In Okasaki's paper, this loop is sped up by computing a conservative initial guess. Indeed, the bit at which
       the two prefixes disagree must be somewhere within the shorter prefix, so we can begin searching at the
       least-significant valid bit in the shorter prefix. Unfortunately, to allow computing the initial guess, the
       main code has to pass in additional parameters, e.g. a mask which describes the length of each prefix. This
       ``pollutes'' the endianness-independent code. For this reason, this optimization isn't implemented here. *)

    let branching_bit i0 i1 =
      highest_bit (i0 lxor i1)

    (* The ``relevant'' bits in an integer [i] are those which are found (strictly) to the left of the single one bit
       in the mask [m]. We keep these bits, and set all others to 0. Okasaki uses a different convention, which allows
       big-endian Patricia trees to masquerade as binary search trees. This feature does not seem to be useful here. *)

    let mask i m =
      i land (lnot (2*m-1))

    (* The smaller [m] is, the more bits are relevant. *)

    let shorter =
      (>)

  end


(*i ------------------------------------------------------------------------ i*)
(*s \mysection{Patricia-tree-based maps} *)

module type Tagged_type =
sig
  type t
  val tag : t -> int
  val equal : t -> t -> bool
  val pretty : Format.formatter -> t -> unit
  module Datatype : Project.Datatype.S with type t = t
end

module Make
  (Key:sig
     type t
     val name : string
     val id : t -> int
     val pretty : Format.formatter -> t -> unit
     val equal : t -> t -> bool
     module Datatype : Project.Datatype.S with type t = t
   end)
  (V : Tagged_type)
  (Initial_Values: sig val v : (Key.t * V.t) list list end) =
struct

    type key = Key.t

    type tag = int

    (* A tree is either empty, or a leaf node, containing both
       the integer key and a piece of data, or a binary node.
       Each binary node carries two integers. The first one is
       the longest common prefix of all keys in this
       sub-tree. The second integer is the branching bit.
       It is an integer with a single one bit (i.e. a power of 2),
       which describes the bit being tested at this node. *)

    type t =
      | Empty
      | Leaf of key * V.t
      | Branch of int * Big_Endian.mask * t * t * tag

    let contains_single_binding t =
      match t with
      | Empty
      | Branch _ -> None
      | Leaf (key, data) -> Some (key, data)

    let rec min_binding t =
      match t with
	Empty -> raise Not_found
      | Branch (_,_,left,_,_) -> min_binding left
      | Leaf (key, data) -> key, data

    let rec iter f htr = match htr with
      | Empty ->
	  ()
      | Leaf (key, data) ->
	  f key data
      | Branch (_, _, tree0, tree1, _tl) ->
	  iter f tree0;
	  iter f tree1

    let pretty fmt tree =
      Format.fprintf fmt "[[@.";
      let prettykv k v =
	Format.fprintf fmt "%a -> %a@."
	  Key.pretty k
	  V.pretty v
      in
      iter prettykv tree;
      Format.fprintf fmt "]]@."

    let tag tr =
      match tr with
	Empty -> 27
      | Leaf (k, v) -> Key.id k + 547 * V.tag v
      | Branch (_, _, _, _, tl) -> tl

    let hash_internal tr =
      let result =
	match tr with
	  Empty | Leaf _ -> tag tr
	| Branch(p,m,l,r, _tag) -> m + 3 * p + 2017 * (tag l) + (tag r)
      in
      result

    let hash_debug  = hash_internal

    let equal_internal htr1 htr2 =
      (* do not use == or compare the toplevel tags. One of the arguments is
	 not hashconsed yet when this function is called *)
      match htr1, htr2 with
	Empty, Empty -> true
      | Leaf(k1, v1), Leaf(k2, v2) ->
	  Key.equal k1 k2 && (V.equal v1 v2)
      | Branch(p1,m1,l1,r1,_), Branch(p2,m2,l2,r2,_) ->
	  p1 = p2 && m1 = m2 && l1 == l2 && r1 == r2
      | _,_ -> false

    type tt = t =
	      | Empty
	      | Leaf of key * V.t
	      | Branch of int * Big_Endian.mask * tt * tt * tag

    let name = Project.Datatype.extend_name2 "ptmap" Key.name V.Datatype.name

    module PatriciaHashtbl =
      Hashtbl.Make
	(struct
          type t = tt
          let equal a b = a == b
          let hash = tag
	end)

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
      let r = List.map
	(function [k,v] -> Leaf (k, v)
	| [] -> Empty
	| _ -> assert false)
	Initial_Values.v
      in
(*  Format.printf "initial values of %s:@." id;
    List.iter (fun x -> Cil.log "value = %a (%d)@." pretty x (Obj.magic x)) r;*)
      r

    module PatriciaHashconsTbl =
      Computation.HashconsTbl
	(struct
	  type t = tt
          let name = name
	  let equal_internal = equal_internal
	  let hash_internal = hash_internal
	  let initial_values = initial_values
	end)
	(struct
          let name = name
          let dependencies = [ Ast.self ]
          let size = 137
	end)

    let wrap_Leaf k v =
      let new_tr = Leaf (k, v) in
      PatriciaHashconsTbl.merge  new_tr

    let wrap_Branch p m l r =
      let new_tr = Branch (p, m, l, r, !current_tag ) in
      let result = PatriciaHashconsTbl.merge  new_tr in
      if result == new_tr
      then begin
	  incr current_tag;
	end;
	result

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
	| Leaf (key', data) ->
	    if Key.equal key key' then
	      data
	    else
	      raise Not_found
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
      let m = Big_Endian.branching_bit p0 p1 in
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
       predictable value. Formally, we assume [Big_Endian.mask p m] equals [p] whenever
       [p] is a prefix with [m] relevant bits. This allows implementing
       [match_prefix] using only one call to [Big_Endian.mask]. On the other hand, this
       requires normalizing prefixes, as done e.g. in [join] above, where
       [Big_Endian.mask p0 m] has to be used instead of [p0]. *)
    let match_prefix k p m =
      Big_Endian.mask k m = p

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
	| Leaf (k0, d0) ->
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
    | Leaf (k, d) ->
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
	| Leaf (key',  _) ->
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
      | Leaf (key', data) ->
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
	| Leaf(key, value), _ ->
	    fine_add (reverse decide) key value t
	| Branch _, Leaf(key, value) ->
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
      | Leaf (key, data) ->
	  f key data accu
      | Branch (_, _, tree0, tree1, _) ->
	  fold f tree1 (fold f tree0 accu)

    (** [fold_rev] performs exactly the same job as [fold], but presents keys
	to [f] in the opposite order. *)
    let rec fold_rev f m accu =
      match m with
      | Empty ->
	  accu
      | Leaf (key, data) ->
	  f key data accu
      | Branch (_, _, tree0, tree1, _) ->
	  fold_rev f tree0 (fold_rev f tree1 accu)

    (** It is valid to evaluate [iter2 f m1 m2] if and only if [m1] and [m2]
	have the same domain. Doing so invokes [f k x1 x2], in turn, for
	each key [k] bound to [x1] in [m1] and to [x2] in [m2]. Bindings
	are presented to [f] according to some unspecified, but fixed,
	order. *)
    let rec iter2 f t1 t2 =
      match t1, t2 with
      | Empty, Empty ->
	  ()
      | Leaf (key1, data1), Leaf (key2, data2) ->
	  assert (Key.equal key1 key2);
	  f key1 (* for instance *) data1 data2
      | Branch (p1, m1, left1, right1, _),
	  Branch (p2, m2, left2, right2, _) ->
	    assert (p1 = p2);
	    assert (m1 = m2);
	    iter2 f left1 left2;
	    iter2 f right1 right2
      | _, _ ->
	  assert false

    (** [map f m] returns the map obtained by composing the map [m] with the
	function [f]; that is, the map $k\mapsto f(m(k))$. *)
    let rec map f htr = match htr with
      | Empty ->
	  Empty
      | Leaf (key, data) ->
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
      | Leaf (key, data) ->
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
	|	Leaf (k0, d0) ->
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
	    |	Leaf (k0, d0) ->
		  if Key.equal k k0 then
		    let d' = decide_some d0 d in
		    if d'==d0 then t else wrap_Leaf k d'
		  else
		    let endo =
		      let decid = decide_none k d0 in
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

	let module Result =
	    struct
	      type t = tt
	      let sentinel = Empty
	    end
	in
	let module Symcacheable =
	    struct
	      type t = tt
	      let hash = tag
	      let equal = (==)
	      let sentinel = Empty
	    end
	in
	let module SymetricCache =
	  Binary_cache.Make_Symetric(Symcacheable)(Result)
	in
	Project.register_todo_before_clear (fun _ -> SymetricCache.clear ());
	let rec union s t =
	  if s==t then s else
	    SymetricCache.merge uncached_union s t
	and uncached_union s t =
	  match s, t with
	  | Empty, t | t, Empty ->
	      endo_map decide_none t
	  | Leaf(key, value), t | t, Leaf(key, value) ->
	      symetric_fine_add key value t
	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if !debug then
		Format.printf "PTMAP Br(%d %d) Br(%d %d)@." p m q n;
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
	in union


    let generic_merge ~cache ~decide =
      let _name, _cache = cache in
	let module Cacheable =
	    struct
	      type t = tt
	      let hash = tag
	      let sentinel = Empty
	      let equal = (==)
	    end
	in
	let module R =
	    struct
	      type t = tt
	      let sentinel = Empty
	    end
	in
	let module Cache = Binary_cache.Make_Asymetric(Cacheable)(R)
	in

      Project.register_todo_before_clear (fun _ -> Cache.clear ());
      fun m1 m2 ->
	let rec union s t =
	  if s==t then s else
	  match s, t with
	  | Empty, _ ->
	      endo_map (fun k x -> decide k None (Some x)) t
	  | (Leaf _ | Branch _), Empty ->
	      endo_map (fun k x -> decide k (Some x) None) s

	  | Leaf(key, value), _ ->
	      generic_fine_add (reverse decide) key value t
	  | Branch _, Leaf(key, value) ->
	      generic_fine_add decide key value s

	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      let compute () =
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
	      Cache.merge compute s t
	in
	union m1 m2

    let generic_is_included exn ~cache ~decide_fst ~decide_snd ~decide_both =
      let find_value  key value htr =
	let remains =
	  try
	    let v, remains = find_and_remove key htr in
	    decide_both value v;
	    remains
	  with Not_found ->
	    decide_fst key value;
	    htr
	in
	iter decide_snd remains
      in
      let reverse_find_value  key value htr =
	let remains =
	  try
	    let v, remains = find_and_remove key htr in
	    decide_both v value;
	    remains
	  with Not_found ->
	    decide_snd key value;
	    htr
	in
	iter decide_fst remains
      in
      let _name, _cache = cache in

      let module Cacheable =
	  struct
	    type t = tt
	    let hash = tag
	    let equal = (==)
	    let sentinel = Empty
	  end
      in
      let module Cache =
	Binary_cache.Make_Asymetric(Cacheable)(Binary_cache.Bool_Result)
      in
      Project.register_todo_before_clear (fun _ -> Cache.clear ());

      fun m1 m2 ->
	let rec inclusion s t =
	  if s!=t then
            match s, t with
            | Empty, _ ->
		iter decide_snd t
            | (Leaf _ | Branch _), Empty ->
		iter decide_fst s

            | Leaf(key, value), _ ->
		find_value key value t
            | Branch _, Leaf(key, value) ->
		reverse_find_value key value s

            | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
		let compute () =
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
		let result = Cache.merge compute s t in
		if not result then raise exn
	in
	inclusion m1 m2

    let cached_fold ~cache ~f ~joiner ~empty =
      let _name, cache = cache in
      let table = PatriciaHashtbl.create cache in
      Project.register_todo_before_clear (fun _-> PatriciaHashtbl.clear table);
      let counter = ref 0 in
      fun m ->
	let rec traverse t =
	  match t with
	    Empty -> empty
	  | Leaf(key, value) ->
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

  let cached_map ~cache ~f =
      let _name, cache = cache in
      let table = PatriciaHashtbl.create cache in
      Project.register_todo_before_clear (fun _ -> PatriciaHashtbl.clear table);
      let counter = ref 0 in
      fun m ->
	let rec traverse t =
	  match t with
	    Empty -> empty
	  | Leaf(key, value) ->
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

  module Datatype = Project.Datatype.Register
    (struct
       type t =
	   tt =
	 | Empty
	 | Leaf of key * V.t
	 | Branch of int * Big_Endian.mask * tt * tt * tag

       let copy _ = assert false (* TODO *)
       let name = name
       let rehash_node = function
	   Empty -> Empty
	 | Leaf (k, v) -> wrap_Leaf k v
	 | Branch (l,p,m,r,_) -> wrap_Branch l p m r
       open Unmarshal
       let descr =
	 let rec descr =
	   Transform
	     (Structure
	       (Sum [| [| Key.Datatype.descr; V.Datatype.descr |];
		       [| Abstract; Abstract; descr; descr; Abstract |] |]),
	     fun o -> let x : tt = Obj.obj o in Obj.repr (rehash_node x))
	 in
	 descr
       let rehash = Project.identity
     end)

  let () =
    Datatype.register_comparable ~hash:tag ()

  let equal = (==)

end


(*
Local Variables:
compile-command: "make -C .."
End:
*)
