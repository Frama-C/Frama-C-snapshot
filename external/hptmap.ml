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


type prefix = int * int
let sentinel_prefix = (-1) , (-1)    

module Big_Endian = struct

  type mask = int

  (* The ``relevant'' bits in an integer [i] are those which are
     found (strictly) to the left of the single one bit in the mask
     [m]. We keep these bits, and set all others to 0. Okasaki uses
     a different convention, which allows big-endian Patricia trees
     to masquerade as binary search trees. This feature does not
     seem to be useful here. *)

  let mask i m =
    i land (lnot (2*m-1))

  (* The smaller [m] is, the more bits are relevant. *)

  let shorter (m:int) (n:int) = m > n

end

(*i ------------------------------------------------------------------------ i*)
(*s \mysection{Patricia-tree-based maps} *)

module Tag_comp : 
sig
  type t
  val get_tag : t -> int
  val get_comp : t -> bool
  val encode : int -> bool -> t
  val pretty: Format.formatter -> t -> unit
end =
struct
  type t = int
  let get_tag x = x land max_int
  let get_comp x = x < 0 
  let encode tag comp =
    if comp then tag lor min_int else tag
  let pretty = Format.pp_print_int
end 
type tag = Tag_comp.t

module Comp_unused = 
struct
  let e = false
  let f _ _ = false
  let compose _ _ = false
  let default = false
end

type ('key, 'value) tree =
    | Empty
    | Leaf of 'key * 'value * bool
    | Branch of int (** prefix *) *
                Big_Endian.mask *
                ('key, 'value) tree *
                ('key, 'value) tree *
                tag

module type Id_Datatype = sig  
    include Datatype.S
    val id: t -> int
end

module Shape(Key: Id_Datatype) = struct
  type 'b t = (Key.t, 'b) tree
end

module Make
  (Key: Id_Datatype)
  (V : Datatype.S)
  (Compositional_bool : sig
     val e: bool
     val f : Key.t -> V.t -> bool
     val compose : bool -> bool -> bool
     val default: bool
   end)
  (Initial_Values: sig val v : (Key.t * V.t) list list end)
  (Datatype_deps: sig val l : State.t list end)
 =
struct

    type key = Key.t

    (* A tree is either empty, or a leaf node, containing both
       the integer key and a piece of data, or a binary node.
       Each binary node carries two integers. The first one is
       the longest common prefix of all keys in this
       sub-tree. The second integer is the branching bit.
       It is an integer with a single one bit (i.e. a power of 2),
       which describes the bit being tested at this node. *)

    type tt = (Key.t, V.t) tree

    let rec pretty_debug fmt = function
      | Empty -> Format.fprintf fmt "Empty"
      | Leaf (k, v, comp) as t ->
        Format.fprintf fmt "K<%d> (%a -> %a, %b)"
          (Extlib.address_of_value t) Key.pretty k V.pretty v comp
      | Branch (prefix, mask, t1, t2, tag) ->
        Format.fprintf fmt"B@[<v>@[(p%d, m%d, t%a)@]@  @[%a@]@  @[%a@]@]"
          prefix mask Tag_comp.pretty tag pretty_debug t1 pretty_debug t2


    let compare =
      if Key.compare == Datatype.undefined ||
        V.compare == Datatype.undefined 
      then (
          Kernel.debug "(%s, %s) ptmap, missing comparison function: %b %b"
            (Type.name Key.ty) (Type.name V.ty)
            (Key.compare == Datatype.undefined)
            (V.compare == Datatype.undefined);
          Datatype.undefined
        )
      else 
	let compare t1 t2 = 
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

    let compositional_bool t = 
      match t with
	Empty -> Compositional_bool.e
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

    let hash tr =
      match tr with
	Empty -> 27
      | Leaf (k, v, _) -> Key.id k + 547 * V.hash v
      | Branch (_, _, _, _, tl) -> Tag_comp.get_tag tl

    let hash_internal tr =
	match tr with
	  Empty | Leaf _ -> hash tr
	| Branch(p,m,l,r, _tag) -> m + 3 * p + 2017 * (hash l) + (hash r)

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

    let initial_values =
      List.map
	(function [k,v] -> Leaf (k, v, Compositional_bool.f k v)
	| [] -> Empty
	| _ -> assert false)
	Initial_Values.v

    let rehash_ref = ref (fun _ -> assert false)

    module Datatype =
      Datatype.Make_with_collections
	(struct
	  type t = tt
	  let name = "(" ^ Type.name Key.ty ^ ", " ^ Type.name V.ty ^ ") ptmap"
	  open Structural_descr
	  let r = Recursive.create ()
	  let structural_descr =
	    t_sum
	      [| [| Key.packed_descr; V.packed_descr; p_abstract |];
		 [| p_abstract;
		    p_abstract;
		    recursive_pack r;
		    recursive_pack r;
		    p_abstract |] |]
	  let () = Recursive.update r structural_descr
	  let reprs = [ Empty ]
	  let equal = ( == )
	  let compare = compare
	  let hash = hash
	  let rehash x = !rehash_ref x
	  let copy = Datatype.undefined
	  let internal_pretty_code = Datatype.pp_fail
	  let pretty = pretty
	  let varname = Datatype.undefined
	  let mem_project = Datatype.never_any_project
	 end)
    let () = Type.set_ml_name Datatype.ty None
    include Datatype

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
      let new_tr = Leaf (k, v, Compositional_bool.f k v) in
      PatriciaHashconsTbl.merge  new_tr

    let wrap_Branch p m l r =
      let open Compositional_bool in
      let tag = !current_tag in
      let comp = compose (compositional_bool l) (compositional_bool r) in
      let comp = 
	match l, r with
	| Branch (_,ml,_,_,_), Branch (_,mr,_,_,_) when ml + mr = m  ->
	    comp
	| Leaf (_,_,_), Leaf (_,_,_) -> 
	    comp
	| _ -> compose default comp
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


    (* This reference will contain a list of functions that will clear
       all the transient caches used in this module *)
    let clear_caches = ref []


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

    let find_key key htr =
      let id = Key.id key in
      let rec find htr =
	match htr with
	| Empty ->
	    raise Not_found
	| Leaf (key', _, _) ->
	    if Key.equal key key' then
	      key'
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
      let all_comp = compositional_bool t1 && compositional_bool t2 in
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
	  if (p1 = p2) && (m1 = m2) 
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
	  else if (Big_Endian.shorter m1 m2) && (match_prefix p2 p1 m1) 
	  then 
	    let sub1 = if (p2 land m1) = 0 then l1 else r1 in
	    if sub1 != t2 then comp_prefixes sub1 t2
	  else if (Big_Endian.shorter m2 m1) && (match_prefix p1 p2 m2)
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

    let hash_subtree = hash

    let equal_subtree = equal

    (* [fine_add decide k d m] returns a map whose bindings are all bindings in
       [m], plus a binding of the key [k] to the datum [d]. If a binding from
       [k] to [d0] already exists, then the resulting map contains a binding
       from [k] to [decide d0 d]. *)

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

    let fine_add decide k d m =
      try
	basic_add decide k d m
      with Unchanged ->
	m

    let add k d m =
      fine_add (fun _ _old_binding new_binding -> new_binding) k d m

    let singleton k d =
      wrap_Leaf k d

    let is_singleton htr = match htr with
    | Leaf (k, d, _) ->
	Some (k, d)
    | Empty
    | Branch _ ->
	None

    let is_empty htr = match htr with
    | Empty ->
	true
    | Leaf _
    | Branch _ ->
	false

    let rec cardinal htr = match htr with
    | Empty ->
	0
    | Leaf _ ->
	1
    | Branch (_, _, t0, t1,  _) ->
	cardinal t0 + cardinal t1

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
       must return the value which shall be bound to [k] in the final map. *)
    let fine_union decide m1 m2 =
      let rec union s t =
	match s, t with
	| Empty, _ ->
	    t
	| (Leaf _ | Branch _), Empty ->
	    s
	| Leaf(key, value, _), _ ->
	    fine_add (fun k v1 v2 -> decide k v2 v1) key value t
	| Branch _, Leaf(key, value, _) ->
	    fine_add decide key value s
	| Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	    if (p = q) && (m = n) then
  	      (* The trees have the same prefix. Merge their sub-trees. *)
	      let u0 = union s0 t0
	      and u1 = union s1 t1 in
	      if t0 == u0 && t1 == u1 then t
	      else (wrap_Branch p m u0 u1)
	    else if (Big_Endian.shorter m n) && (match_prefix q p m) then
  	      (* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)
	      if (q land m) = 0 then
		(wrap_Branch p m (union s0 t) s1)
	      else
		(wrap_Branch p m s0 (union s1 t))
	    else if (Big_Endian.shorter n m) && (match_prefix p q n) then
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
	[m2] take precedence over those in [m1]. *)
    let union m1 m2 =
      fine_union (fun _ _ d' -> d') m1 m2

    let rec fold f m accu =
      match m with
      | Empty ->
	  accu
      | Leaf (key, data, _) ->
	  f key data accu
      | Branch (_, _, tree0, tree1, _) ->
	  fold f tree1 (fold f tree0 accu)

    let rec fold_rev f m accu =
      match m with
      | Empty ->
	  accu
      | Leaf (key, data, _) ->
	  f key data accu
      | Branch (_, _, tree0, tree1, _) ->
	  fold_rev f tree0 (fold_rev f tree1 accu)


    let rec for_all f m =
      match m with
      | Empty -> true
      | Leaf (key, data, _) -> f key data
      | Branch (_, _, tree0, tree1, _) -> for_all f tree0 && for_all f tree1

    let rec exists f m =
      match m with
      | Empty -> false
      | Leaf (key, data, _) -> f key data
      | Branch (_, _, tree0, tree1, _) -> exists f tree0 || exists f tree1


    let rec map f htr = match htr with
      | Empty ->
	  Empty
      | Leaf (key, data, _) ->
	  wrap_Leaf key (f data)
      | Branch (p, m, tree0, tree1, _) ->
	  wrap_Branch p m (map f tree0) (map f tree1)

    let rec map' f htr = match htr with
      | Empty ->
          Empty
      | Leaf (key, data, _) ->
          (match f key data with
            | Some data' -> wrap_Leaf key data'
            | None -> Empty)
      | Branch (p, m, tree0, tree1, _) ->
	  wrap_Branch p m (map' f tree0) (map' f tree1)        

    (* The comment below is outdated: [map] and [endo_map] do not have the
       same signature for [f] *)
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
	let hash = hash
	let sentinel = Empty
	let equal = (==)
      end

      module R =
      struct
	type t = tt
	let sentinel = Empty
      end

      let symmetric_merge ~cache:_ ~decide_none ~decide_some =
	let symmetric_fine_add k d m =
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
	let module SymmetricCache =
	  Binary_cache.Make_Symmetric(Cacheable)(R)
	in
        clear_caches := SymmetricCache.clear :: !clear_caches;
	let rec union s t =
	  if s==t then s else
	    SymmetricCache.merge uncached_union s t
	and uncached_union s t =
	  match s, t with
	  | Empty, t | t, Empty ->
	      endo_map decide_none t
	  | Leaf(key, value, _), t | t, Leaf(key, value, _) ->
	      symmetric_fine_add key value t
	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if (p = q) && (m = n) 
	      then
  		(* The trees have the same prefix. Merge their sub-trees. *)
		let u0 = union s0 t0
		and u1 = union s1 t1 in
		if t0 == u0 && t1 == u1 then t
		else wrap_Branch p m u0 u1

	      else if (Big_Endian.shorter m n) && (match_prefix q p m) then

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

	      else if (Big_Endian.shorter n m) && (match_prefix p q n) then

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

      let symmetric_inter ~cache:_ ~decide_some =
	let module SymmetricCache =
	  Binary_cache.Make_Symmetric(Cacheable)(R)
	in
        clear_caches := SymmetricCache.clear :: !clear_caches;
	let rec inter s t =
	  if s==t then s else SymmetricCache.merge uncached_inter s t
	and uncached_inter s t =
	  match s, t with
	  | Empty, _ | _, Empty -> Empty
	  | Leaf (key1, value1, _), Leaf (key2, value2, _) ->
            if Key.equal key1 key2 then
              match decide_some key1 value1 value2 with
                | None -> Empty
                | Some value ->
                  if V.equal value value1 then s
                  else if V.equal value value2 then t
                  else wrap_Leaf key1 value
            else
              Empty
	  | (Leaf (key, value, _) as l), (Branch _ as t)
          | (Branch _ as t), (Leaf (key, value, _) as l) ->
            begin (* At most [key] will be in the result, search it in [t] *)
              try
                let value' = find key t in
                match decide_some key value value' with
                  | None -> Empty
                  | Some value'' ->
		    if V.equal value'' value then l else wrap_Leaf key value''
              with Not_found -> Empty
            end
	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if (p = q) && (m = n) 
	      then
  		(* The trees have the same prefix. Merge their sub-trees. *)
		let u0 = inter s0 t0
		and u1 = inter s1 t1 in
		if t0 == u0 && t1 == u1 then t
		else if u0 == Empty then u1
                else if u1 == Empty then u0
                else  wrap_Branch p m u0 u1
	      else if (Big_Endian.shorter m n) && (match_prefix q p m) then
  		(* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)
		if (q land m) = 0 then inter s0 t else inter s1 t
	      else if (Big_Endian.shorter n m) && (match_prefix p q n) then
		(* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)
		if (p land n) = 0 then inter s t0 else inter s t1
	      else
		(* The prefixes disagree. *)
                Empty
	in inter

      let rec inter_with_shape shape map =
	match shape, map with
	  | Empty, _ | _, Empty -> Empty
	  | Leaf (key1, _, _), Leaf (key2, _, _) ->
            if Key.equal key1 key2 then map else Empty
	  | Leaf (key, _, _), Branch _ ->
            begin (* At most [key] will be in the result, search it in [t] *)
              try
                let value = find key map in
                wrap_Leaf key value
              with Not_found -> Empty
            end
          | Branch _, Leaf (key, _, _) -> (* Search key in [shape] *)
            if mem key shape then map else Empty
	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if (p = q) && (m = n) 
	      then
  		(* The trees have the same prefix. Merge their sub-trees. *)
		let u0 = inter_with_shape s0 t0
		and u1 = inter_with_shape s1 t1 in
                if t0 == u0 && t1 == u1 then map
		else if u0 == Empty then u1
                else if u1 == Empty then u0
                else  wrap_Branch p m u0 u1
	      else if (Big_Endian.shorter m n) && (match_prefix q p m) then
  		(* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)
		if (q land m) = 0
                then inter_with_shape s0 map
                else inter_with_shape s1 map
	      else if (Big_Endian.shorter n m) && (match_prefix p q n) then
		(* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)
		if (p land n) = 0
                then inter_with_shape shape t0
                else inter_with_shape shape t1
	      else
		(* The prefixes disagree. *)
                Empty

      let rec from_shape f = function
        | Empty -> Empty
        | Leaf (key, value, _) -> wrap_Leaf key (f key value)
        | Branch (p, m, t1, t2, _) ->
          wrap_Branch p m (from_shape f t1) (from_shape f t2)


      let generic_merge ~cache ~decide ~idempotent =
	let _name, do_cache = cache in
	let cache_merge =
	  if do_cache then
            begin
	      let module Cache = Binary_cache.Make_Asymmetric(Cacheable)(R) in
              clear_caches := Cache.clear :: !clear_caches;
	      Cache.merge
	    end
          else fun f x y -> f x y
	in
	let rec union s t =
          if idempotent then
            if s==t then s else cache_merge compute s t
          else
            if s==Empty && t==Empty then s else cache_merge compute s t
	and compute s t =
	  match s, t with
	  | Empty, _ ->
	      endo_map (fun k x -> decide k None (Some x)) t
	  | (Leaf _ | Branch _), Empty ->
	      endo_map (fun k x -> decide k (Some x) None) s

	  | Leaf(key, value, _), _ ->
	      generic_fine_add (fun k v1 v2 -> decide k v2 v1) key value t
	  | Branch _, Leaf(key, value, _) ->
	      generic_fine_add decide key value s

	  | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	      if (p = q) && (m = n) then
  		(* The trees have the same prefix. Merge their sub-trees. *)
		let u0 = union s0 t0
		and u1 = union s1 t1 in
		if t0 == u0 && t1 == u1 then t
		else wrap_Branch p m u0 u1
	      else if (Big_Endian.shorter m n) && (match_prefix q p m) then
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
	      else if (Big_Endian.shorter n m) && (match_prefix p q n) then
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

      type decide_fast = Done | Unknown

      let make_predicate cache_merge exn ~decide_fast ~decide_fst ~decide_snd ~decide_both =
        let rec aux s t =
	if decide_fast s t = Unknown then
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
		aux s l;
		aux Empty r;
	      end
	    else begin
		aux Empty l;
		aux s r;
	      end
        | Branch (p,m,l,r,_) , Leaf(key, _value, _) ->
	    let i = Key.id key in
	    if i < p+m
	    then begin
		aux l t;
		aux r Empty;
	      end
	    else begin
		aux l Empty;
		aux r t;
	      end
        | Branch _, Branch _ ->
          (* Beware that [cache_merge compute] may swap the order of its
             arguments compared to [aux]. Do not use the result of the match
             in [aux] directly inside [compute]. *)
          let compute s t = match s, t with
            | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) -> begin
	      try
		if (p = q) && (m = n) then
		  begin
  		    (*The trees have the same prefix. Compare their sub-trees.*)
		    aux s0 t0;
		    aux s1 t1
		  end
		else if (Big_Endian.shorter m n) && (match_prefix q p m) then
  		  (* [q] contains [p]. Compare [t] with a sub-tree of [s]. *)
		  if (q land m) = 0 then
		    begin
		      aux s0 t;
		      aux s1 Empty;
		    end
		  else
		    begin
		      aux s0 Empty;
		      aux s1 t
		    end
		else if (Big_Endian.shorter n m) && (match_prefix p q n) then
		  (* [p] contains [q]. Compare [s] with a sub-tree of [t]. *)
		  if (p land n) = 0 then
		    begin
		      aux s t0;
		      aux Empty t1
		    end
		  else
		    begin
		      aux s t1;
		      aux Empty t0
		    end
		else
		  begin
		    (* The prefixes disagree. *)
		    aux s Empty;
		    aux Empty t;
		  end;
		true
	      with e when e = exn -> false
	      | _ -> assert false
              end
            | _ -> assert false (* Branch/Branch comparison *)
	    in
	    let result = cache_merge compute s t in
	    if not result then raise exn
      in
      aux 


    let generic_predicate exn ~cache:_ ~decide_fast ~decide_fst ~decide_snd ~decide_both =
      let module Cache =
	Binary_cache.Make_Binary(Cacheable)(Cacheable)
      in
      clear_caches := Cache.clear :: !clear_caches;
      make_predicate Cache.merge exn
	~decide_fast ~decide_fst ~decide_snd ~decide_both 

    let generic_symmetric_predicate exn ~decide_fast ~decide_one ~decide_both =
      let module Cache =
	Binary_cache.Make_Symmetric_Binary(Cacheable)
      in 
      clear_caches := Cache.clear :: !clear_caches;
      make_predicate Cache.merge exn
	~decide_fast ~decide_fst:decide_one ~decide_snd:decide_one ~decide_both 


  type predicate_type = ExistentialPredicate | UniversalPredicate
  type predicate_result = PTrue | PFalse | PUnknown

  let decide_fast_intersection s t =
    match s, t with
      | Empty, _ | _, Empty -> PFalse
      | _ -> if s == t  then PTrue else PUnknown

  let decide_fast_inclusion s t =
    if s == t || s == Empty then PTrue else PUnknown

  let make_binary_predicate cache_merge pt ~decide_fast ~decide_fst ~decide_snd ~decide_both =
    (** We cannot use [&&] and [||] under another name, as functions are not
        lazy in OCaml. Instead, we defer the evaluation of the right part by
        calling a function. Due to typing issues, we must actually define
        two functions... *)
    let comb1, comb2 =
      match pt with
        | UniversalPredicate ->   let f b f v1 v2 = b && f v1 v2 in f, f
        | ExistentialPredicate -> let f b f v1 v2 = b || f v1 v2 in f, f
    in
    let rec aux s t =
      match s, t with
      | Empty, Empty ->
        (match pt with
          | ExistentialPredicate -> false
          | UniversalPredicate -> true)

      | Leaf (key, data, _), Empty ->
        decide_fst key data

      | Empty, Leaf (key, data, _) ->
        decide_snd key data

      | Empty, Branch (_, _, tl, tr, _) ->
        comb1 (aux' Empty tl) aux' Empty tr

      | Branch (_, _, tl, tr, _), Empty ->
        comb1 (aux' tl Empty) aux' tr Empty

      | Leaf(k1, v1, _), Leaf(k2, v2, _) ->
	if Key.id k1 = Key.id k2 
	then decide_both k1 v1 v2
        else comb2 (decide_fst k1 v1) decide_snd k2 v2

      | Leaf(key, _value, _), Branch(p,m,l,r,_) ->
	let i = Key.id key in
	if i < p+m
	then comb1 (aux' Empty r) aux' s l
	else comb1 (aux' Empty l) aux' s r

      | Branch (p,m,l,r,_) , Leaf(key, _value, _) ->
	let i = Key.id key in
	if i < p+m
	then comb1 (aux' r Empty) aux' l t
	else comb1 (aux' l Empty) aux' r t

      | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	if (p = q) && (m = n) then
          (*The trees have the same prefix. Compare their sub-trees.*)
          comb1 (aux' s0 t0) aux' s1 t1
	else if (Big_Endian.shorter m n) && (match_prefix q p m) then
  	  (* [q] contains [p]. Compare [t] with a sub-tree of [s]. *)
	  if (q land m) = 0
          then comb1 (aux' s1 Empty) aux' s0 t
	  else comb1 (aux' s0 Empty) aux' s1 t
	else if (Big_Endian.shorter n m) && (match_prefix p q n) then
	  (* [p] contains [q]. Compare [s] with a sub-tree of [t]. *)
	  if (p land n) = 0
          then comb1 (aux' s t0) aux' Empty t1
	  else comb1 (aux' s t1) aux' Empty t0
	else (* The prefixes disagree. *)
	  comb1 (aux' s Empty) aux' Empty t
    and aux' s t =
      match decide_fast s t with
        | PFalse -> false
        | PTrue -> true
        | PUnknown -> cache_merge aux s t
    in
    aux'

  type cache_type =
    | NoCache
    | PersistentCache of string
    | TemporaryCache of string


  let binary_predicate ct pt ~decide_fast ~decide_fst ~decide_snd ~decide_both =
    let cache_merge = match ct with
      | NoCache -> (fun f x y -> f x y)
      | PersistentCache _name | TemporaryCache _name ->
        let module Cache = Binary_cache.Make_Binary(Cacheable)(Cacheable) in
        (match ct with
          | PersistentCache _ ->
            clear_caches := Cache.clear :: !clear_caches
          | _ -> ());
        Cache.merge
    in
    make_binary_predicate cache_merge pt
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let symmetric_binary_predicate ct pt ~decide_fast ~decide_one ~decide_both =
    let cache_merge = match ct with
      | NoCache -> (fun f x y -> f x y)
      | PersistentCache _name | TemporaryCache _name ->
        let module Cache = Binary_cache.Make_Symmetric_Binary(Cacheable) in
        (match ct with
          | PersistentCache _ ->
            clear_caches := Cache.clear :: !clear_caches
          | _ -> ());
        Cache.merge
    in
    make_binary_predicate cache_merge pt
      ~decide_fast ~decide_fst:decide_one ~decide_snd:decide_one ~decide_both


    let cached_fold ~cache_name:_ ~temporary ~f ~joiner ~empty =
      let cache_size = Binary_cache.cache_size in
      let cache = Array.make cache_size (Empty, empty) in
      let hash t = abs (hash t mod cache_size) in
      let reset () = Array.fill cache 0 cache_size (Empty, empty) in
      if not temporary then clear_caches := reset :: !clear_caches;
      fun m ->
	let rec traverse t =
          let mem result =
            cache.(hash t) <- (t, result);
            result
          in
          let find () =
            let t', r = cache.(hash t) in
            if equal t t' then r
            else raise Not_found
          in
	  match t with
          | Empty -> empty
	  | Leaf(key, value, _) ->
            (try
               find ()
             with Not_found ->
               mem (f key value)
            )
	  | Branch(_p, _m, s0, s1, _) ->
	      try
		find ()
	      with Not_found ->
		let result0 = traverse s0 in
		let result1 = traverse s1 in
		mem (joiner result0 result1)
	in
	traverse m

  let cached_map ~cache ~temporary ~f =
      let _name, cache = cache in
      let table = Hashtbl.create cache in
      if not temporary then
        clear_caches := (fun () -> Hashtbl.clear table) :: !clear_caches;
      let counter = ref 0 in
      fun m ->
	let rec traverse t =
	  match t with
	    Empty -> empty
	  | Leaf(key, value, _) ->
	      wrap_Leaf key (f key value)
	  | Branch(p, m, s0, s1, _) ->
	      try
		let result = Hashtbl.find table t in
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
		    Hashtbl.clear table;
		    counter := 0;
		  end;
(*		Format.printf "add  %s %d@." name !counter; *)
		Hashtbl.add table t result;
		result
	in
	traverse m

    let split key htr =
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

    let shape x = ((x : t) :> V.t Shape(Key).t)

    let clear_caches () = List.iter (fun f -> f ()) !clear_caches

end



(*
Local Variables:
compile-command: "make -C .."
End:
*)
