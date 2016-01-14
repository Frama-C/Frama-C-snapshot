(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licenses/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(* Set to true to see which caches are created *)
let debug_cache = false

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
    | Leaf of 'key * 'value * tag
    | Branch of int (** prefix *) *
                Big_Endian.mask *
                ('key, 'value) tree *
                ('key, 'value) tree *
                tag

let id tr = match tr with
  | Empty -> 0
  | Leaf (_, _, tag)
  | Branch (_, _, _, _, tag) -> Tag_comp.get_tag tag

let hash_generic = id

module type Id_Datatype = sig  
    include Datatype.S
    val id: t -> int
end

module type V = sig
  include Datatype.S
  val pretty_debug: t Pretty_utils.formatter
end

module Shape(Key: Id_Datatype) = struct
  type 'b t = (Key.t, 'b) tree
end

module Make
  (Key: Id_Datatype)
  (V : V)
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
    type v = V.t
    type 'a shape = 'a Shape(Key).t
    type prefix = int * int

    (* A tree is either empty, or a leaf node, containing both
       the integer key and a piece of data, or a binary node.
       Each binary node carries two integers. The first one is
       the longest common prefix of all keys in this
       sub-tree. The second integer is the branching bit.
       It is an integer with a single one bit (i.e. a power of 2),
       which describes the bit being tested at this node. *)

    type t = (Key.t, V.t) tree
    type hptmap = t (* Alias needed later *)

    let rec pretty_debug fmt = function
      | Empty -> Format.fprintf fmt "Empty"
      | Leaf (k, v, comp) as t ->
        Format.fprintf fmt "K<%d> (%a -><%d> %a, %a)"
          (Extlib.address_of_value t) Key.pretty k
          (Extlib.address_of_value v) V.pretty_debug v Tag_comp.pretty comp
      | Branch (prefix, mask, t1, t2, tag) ->
        Format.fprintf fmt"B@[<v>@[(p%d, m%d, t%a)@]@  @[%a@]@  @[%a@]@]"
          prefix mask Tag_comp.pretty tag pretty_debug t1 pretty_debug t2


    let compare =
      if Key.compare == Datatype.undefined ||
        V.compare == Datatype.undefined 
      then (
        Cmdline.Kernel_log.debug
          "(%s, %s) ptmap, missing comparison function: %b %b"
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
      | Leaf (_,_,tc)
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

    let empty = Empty

    (* Tags must be > 0, as we use 0 for the id of Empty. *)
    let current_tag_before_initial_values = 1 
    let current_tag = ref current_tag_before_initial_values

    let initial_values =
      let tc k v =
        let b = Compositional_bool.f k v in
        let tag = !current_tag in
        incr current_tag;
        Tag_comp.encode tag b
      in
      List.map
	(function [k,v] -> Leaf (k, v, tc k v)
	| [] -> Empty
	| _ -> assert false)
	Initial_Values.v

    let rehash_ref = ref (fun _ -> assert false)

    module D =
      Datatype.Make_with_collections
	(struct
	  type t = hptmap
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
	  let hash = hash_generic
	  let rehash x = !rehash_ref x
	  let copy = Datatype.undefined
	  let internal_pretty_code = Datatype.pp_fail
	  let pretty = pretty
	  let varname = Datatype.undefined
	  let mem_project = Datatype.never_any_project
	 end)
    let () = Type.set_ml_name D.ty None
    include (D: Datatype.S_with_collections with type t := t)

    module PatriciaHashconsTbl =
      State_builder.Hashconsing_tbl
        (struct
          include D
          (* At this stage, the root of the tree is _not_ hashconsed.
             The functions below cannot rely on the tags for it, only for
             the subtrees. *)

          let hash_internal tr = match tr with
            | Empty -> 37
            | Leaf (k, v, _) -> Key.id k + 547 * V.hash v
            | Branch(p,m,l,r, _tag) ->
              m + 3 * p + 2017 * (hash_generic l) + (hash_generic r)

          (* here, only one of the arguments is hash-consed *)
          let equal_internal htr1 htr2 =
            match htr1, htr2 with
            | Empty, Empty -> true
            | Leaf(k1, v1, _), Leaf(k2, v2, _) ->
              Key.equal k1 k2 && V.equal v1 v2
            | Branch(p1,m1,l1,r1,_), Branch(p2,m2,l2,r2,_) ->
              p1 = p2 && m1 = m2 && l1 == l2 && r1 == r2
            | _,_ -> false

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

    let id = hash_generic

    let wrap_Leaf k v =
      (* The test k < p+m and the implementation of [highest_bit] do not work
         with negative keys. *)
      assert (Key.id k >= 0); 
      let b = Compositional_bool.f k v in
      let tag = !current_tag in
      let new_tr = Leaf (k, v, Tag_comp.encode tag b) in
      let result = PatriciaHashconsTbl.merge new_tr in
      if result == new_tr
      then current_tag := (succ tag) land max_int ;
      result

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


    (* This reference will contain a list of functions that will clear
       all the transient caches used in this module *)
    let clear_caches = ref []


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
       requires normalizing prefixes, as done e.g. in [join] below, where
       [Big_Endian.mask p0 m] has to be used instead of [p0]. *)
    let match_prefix k p m =
      Big_Endian.mask k m = p


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

    (* Similar to [find], but checks the prefix found at the current node *)
    let find_check_missing key htr =
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
	| Branch (prefix, mask, tree0, tree1, _) ->
          if match_prefix id prefix mask then
	    find (if (id land mask) = 0 then tree0 else tree1)
          else raise Not_found
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
	| Branch (prefix, mask, tree0, tree1, _) ->
          if match_prefix id prefix mask then
	    find (if (id land mask) = 0 then tree0 else tree1)
          else raise Not_found
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
	| Branch (prefix, mask, tree0, tree1, _) ->
          if match_prefix id prefix mask then
	    find (if (id land mask) = 0 then tree0 else tree1)
          else false
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

    type subtree = t
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
	  if Key.id k = p && m = -1 && (Tag_comp.get_comp c)
          then Some t
          else None
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

    exception Unchanged

    let add k d m =
      let id = Key.id k in
      let rec add t =
	match t with
	| Empty ->
	    wrap_Leaf k d
	| Leaf (k0, d0, _) ->
	    if Key.equal k k0 then
	      if d == d0 then
		raise Unchanged
	      else
		wrap_Leaf k d
	    else
	      join id (wrap_Leaf k d) (Key.id k0) t
	| Branch (p, m, t0, t1, _) ->
	    if match_prefix id p m then
	      if (id land m) = 0 then wrap_Branch p m (add t0) t1
	      else wrap_Branch p m t0 (add t1)
	    else
	      join id (wrap_Leaf k d) p t
      in
      try add m
      with Unchanged -> m

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
          if match_prefix id prefix mask then
	    if (id land mask) = 0 then
	      let rtree0 = remove tree0 in
	      match rtree0 with
	      | Empty ->
		  tree1
	      | _ ->
                if rtree0 == tree0 then
                  htr
                else
		  wrap_Branch prefix mask rtree0 tree1
	    else
	      let rtree1 = remove tree1 in
	      match rtree1 with
	      | Empty ->
		  tree0
	      | _ ->
                if rtree1 == tree1 then
                  htr
                else
		  wrap_Branch prefix mask tree0 rtree1
          else
            raise Not_found
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

    let rehash_node = function
      | Empty -> Empty
      | Leaf (k, v, _) -> wrap_Leaf k v
      | Branch (p,m,l,r,_) ->
        if Descr.is_abstract Key.descr then
          (* The keys id have not been modified during de-marshalling.
             The shapes of [l] and [r] are compatible, just merge them. *)
          wrap_Branch p m l r
        else
          (* The ids may have been modified, the trees can overlap. Rebuild
             everything from scratch. *)
          fold add l r

    let () = rehash_ref := rehash_node


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
          let data' = f data in
          if data == data' then htr
          else
	    wrap_Leaf key data'
      | Branch (p, m, tree0, tree1, _) ->
          let tree0' = map f tree0 in
          let tree1' = map f tree1 in
          if tree0' == tree0 && tree1' == tree1 then htr
          else
	    wrap_Branch p m tree0' tree1'

    let rec map' f htr = match htr with
      | Empty -> Empty
      | Leaf (key, data, _) ->
          begin
            match f key data with
              | Some data' -> if data == data' then htr else wrap_Leaf key data'
              | None -> Empty
          end
      | Branch (p, m, tree0, tree1, _) ->
          let tree0' = map' f tree0 and tree1' = map' f tree1 in
          if tree0' == tree0 && tree1' == tree1
          then htr
          else if tree0' == Empty then tree1'
          else if tree1' == Empty then tree0'
          else wrap_Branch p m tree0' tree1'

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


  module Cacheable = struct
    type t = hptmap
    let hash = hash
    let sentinel = Empty
    let equal = (==)
  end

  module R = struct
    type t = hptmap
    let sentinel = Empty
  end

  module type I = sig
    val clear : unit -> unit
    val merge : (Cacheable.t -> Cacheable.t -> R.t)
      -> Cacheable.t -> Cacheable.t -> Cacheable.t
  end

  (* A (too ?) generic merge. *)
  let generic_merge
      ~(cache:        Hptmap_sig.cache_type)
      ~(symmetric:    bool)
      ~(idempotent:   bool)
      ~(increasing:   bool)
      ~(decide_both:  key -> v -> t -> v -> t -> t)
      ~(decide_left:  t -> t)
      ~(decide_right: t -> t)
    =
    (* Cache of the merges, depending on [cache] and [symmetric].*)
    let cache_merge = match cache with
      | Hptmap_sig.NoCache -> (fun f x y -> f x y)
      | Hptmap_sig.PersistentCache _name | Hptmap_sig.TemporaryCache _name ->
        if debug_cache then Format.eprintf "CACHE generic_merge %s@." _name;
          let module Cache =
            (val if symmetric
              then (module Binary_cache.Symmetric_Binary (Cacheable) (R) : I)
              else (module Binary_cache.Arity_Two (Cacheable) (Cacheable) (R) : I)
              : I)
          in
          if cache = Hptmap_sig.PersistentCache _name
          then clear_caches := Cache.clear :: !clear_caches;
          Cache.merge
    in
    (* Rewrap of branches.
       The initials branches and tree are provided in order to avoid the wrapping
       if the two branches have not been modified.
       If the merge is increasing, we don't need to test whether the branches
       are not empty. *)
    let rewrap p m u orig_u v orig_v orig_tree =
      if u == orig_u && v == orig_v then orig_tree
      else wrap_Branch p m u v
    in
    let rewrap =
      if increasing then rewrap
      else
        fun p m u orig_u v orig_v orig_tree ->
          if u == Empty then v else if v == Empty then u
          else rewrap p m u orig_u v orig_v orig_tree
    in
    (* Join two distinct branches. If the merge is increasing, we don't need to
       test their emptiness. *)
    let rejoin =
      if increasing then join
      else
        fun p u q v ->
          if u == Empty then v else if v == Empty then u
          else join p u q v
    in
    (* Called when one of the trees is a leaf [leaf] binding [key] to [data];
       the other side is [tree].
       [right] is true if the leaf come from the right tree. *)
    let merge_leaf right =
      (* [decide_leaf] and [decide_tree] are the actions to perform respectively
         on the [leaf] and on the [tree] when they are disjoint.
         If the merge is not symmetric, they depend on the side the leaf comes
         froms, and similarly for [decide_both] and [cache]. *)
      let decide_leaf = if right then decide_right else decide_left
      and decide_tree = if right then decide_left else decide_right
      and decide_both =
        if right || symmetric then decide_both
        else fun k v1 t1 v2 t2 -> decide_both k v2 t2 v1 t1
      and cache =
        if right && not symmetric
        then fun f s t -> cache_merge (fun t s -> f s t) t s
        else cache_merge
      in
      (* Reminder: [leaf] bind [key] to [data]. *)
      fun key data leaf tree ->
        let k_id = Key.id key in
        let rec merge_leaf tree = cache add leaf tree
        and add leaf tree =
          match tree with
            | Empty -> decide_leaf leaf
            | Leaf (key', data', _) ->
                if idempotent && leaf == tree then leaf
                else if Key.equal key key'
                then
                  decide_both key data' tree data leaf
                else
                  let tree' = decide_tree tree
                  and leaf' = decide_leaf leaf in
                  rejoin k_id leaf' (Key.id key') tree'
            | Branch (p, m, t0, t1, _) ->
                if match_prefix k_id p m then
                  if (k_id land m) = 0 then
                    let t0' = merge_leaf t0
                    and t1' = decide_tree t1 in
                    rewrap p m t0' t0 t1' t1 tree
                  else
                    let t1' = merge_leaf t1
                    and t0' = decide_tree t0 in
                    rewrap p m t0' t0 t1' t1 tree
                else
                  let tree' = decide_tree tree
                  and leaf' = decide_leaf leaf in
                  rejoin k_id leaf' p tree'
        in
        merge_leaf tree
    in
    let merge_right_leaf = merge_leaf true
    and merge_left_leaf = merge_leaf false in
    let rec merge s t =
      if idempotent && s == t then s
      else match s, t with
        | Empty, Empty        -> Empty
        | Empty, _            -> decide_right t
        | _, Empty            -> decide_left s
        | Leaf (key, v, _), _ -> merge_left_leaf key v s t
        | _, Leaf (key, v, _) -> merge_right_leaf key v t s
        | Branch (p, m, s0, s1, _), Branch (q, n, t0, t1, _) ->
            let descend = fun s t ->
              merge_branches s (p, m, s0, s1) t (q, n, t0, t1)
            in
            cache_merge descend s t
    (* Called for the recursive descend in two trees.
       [s] is [Branch (p, m, s0, s1)] and [t] is [Branch (q, n, t0, t1)]. *)
    and merge_branches s (p, m, s0, s1) t (q, n, t0, t1) =
      if (p = q) && (m = n) then
        (* The trees have the same prefix. Merge their sub-trees. *)
        let u0 = merge s0 t0  and u1 = merge s1 t1 in
        rewrap p m u0 s0 u1 s1 s
      else if (Big_Endian.shorter m n) && (match_prefix q p m) then
        (* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)
        if (q land m) = 0 then
          let s0_t = merge s0 t in
          let s1_e = decide_left s1 in
          rewrap p m s0_t s0 s1_e s1 s
        else
          let s0_e = decide_left s0 in
          let s1_t = merge s1 t in
          rewrap p m s0_e s0 s1_t s1 s
      else if (Big_Endian.shorter n m) && (match_prefix p q n) then
        (* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)
        if (p land n) = 0 then
          let s_t0 = merge s t0 in
          let e_t1 = decide_right t1 in
          rewrap q n s_t0 t0 e_t1 t1 t
        else
          let s_t1 = merge s t1 in
          let e_t0 = decide_right t0 in
          rewrap q n e_t0 t0 s_t1 t1 t
      else
        (* The prefixes disagree. *)
        let u0 = decide_left s and u1 = decide_right t in
        rejoin p u0 q u1
    in
    merge

  type empty_action = Neutral | Absorbing | Traversing of (key -> v -> v option)

  let merge =
    (* Called when one of the tree is empty *)
    let decide_none = function
      | Neutral      -> fun t -> t
      | Absorbing    -> fun _ -> Empty
      | Traversing f -> fun t -> map' f t (* TODO: add a cache? *)
    in
    fun ~cache ~symmetric ~idempotent ~decide_both ~decide_left ~decide_right ->
      let decide_both key value leaf value' leaf' =
        match decide_both key value value' with
          | Some v ->
              if v == value then leaf else if v == value' then leaf'
              else wrap_Leaf key v
          | None -> Empty
      in
      generic_merge ~cache ~symmetric ~idempotent ~increasing:false
        ~decide_both
        ~decide_left:(decide_none decide_left)
        ~decide_right:(decide_none decide_right)

  let generic_join ~cache ~symmetric ~idempotent ~decide =
    let decide_both key value leaf value' leaf' =
      let v = decide key (Some value) (Some value') in
      if v == value then leaf else if v == value' then leaf'
      else wrap_Leaf key v
    and decide_right = endo_map (fun k v -> decide k None (Some v))
    and decide_left = endo_map  (fun k v -> decide k (Some v) None)
    in
    generic_merge ~cache ~symmetric ~idempotent ~increasing:true
      ~decide_both ~decide_left ~decide_right

  let join ~cache ~symmetric ~idempotent ~decide =
   let decide_both key value leaf value' leaf' =
      let v = decide key value value' in
      if v == value then leaf else if v == value' then leaf'
      else wrap_Leaf key v
    and decide_none = fun t -> t
    in
    generic_merge ~cache ~symmetric ~idempotent ~increasing:true
      ~decide_both ~decide_left:decide_none ~decide_right:decide_none

  let inter ~cache ~symmetric ~idempotent ~decide =
   let decide_both key value leaf value' leaf' =
     match decide key value value' with
       | Some v ->
           if v == value then leaf else if v == value' then leaf'
           else wrap_Leaf key v
       | None -> Empty
    and decide_none = fun _ -> Empty
    in
    generic_merge ~cache ~symmetric ~idempotent ~increasing:false
      ~decide_both ~decide_left:decide_none ~decide_right:decide_none

  let fold2_join_heterogeneous (type arg) (type result) ~cache ~empty_left ~empty_right ~both ~join ~empty =
    let cache_merge = match cache with
      | Hptmap_sig.NoCache -> (fun f x y -> f x y)
      | Hptmap_sig.PersistentCache _name | Hptmap_sig.TemporaryCache _name ->
        if debug_cache then Format.eprintf "CACHE fold2_join_heterogeneous %s@." _name;
        let module Arg = struct
          type t = (Key.t, arg) tree
	  let hash : t -> int = hash_generic
	  let sentinel : t = Empty
	  let equal : t -> t -> bool = (==)
        end in
        let module Result = struct
          type t = result
          let sentinel : t = empty
        end in
        let module Cache = Binary_cache.Arity_Two(Cacheable)(Arg)(Result) in
        (match cache with
        | Hptmap_sig.PersistentCache _ ->
          clear_caches := Cache.clear :: !clear_caches
        | _ -> ());
        Cache.merge
    in
    let rec compute s t = cache_merge aux s t
    and aux s t =
      match s, t with
      | Empty, Empty -> empty
      | Empty, t -> empty_left t
      | s, Empty -> empty_right s

      | Leaf (ks, vs, _), Leaf (kt, vt, _) ->
        if Key.equal ks kt then
          both ks vs vt
        else
          join (empty_left t) (empty_right s)

      | Branch (p, m, s0, s1, _), Leaf(kt, _, _) ->
        let k_id = Key.id kt in
        if match_prefix k_id p m then
          if (k_id land m) = 0 then
            join (compute s0 t) (empty_right s1)
          else
            join (compute s1 t) (empty_right s0)
        else
          join (empty_right s) (empty_left t)

      | Leaf (ks, _, _), Branch(q, n, t0, t1, _) ->
        let k_id = Key.id ks in
	if match_prefix k_id q n then
	  if (k_id land n) = 0 then
            join (compute s t0) (empty_left t1)
	  else
            join (compute s t1) (empty_left t0)
	else
          join (empty_right s) (empty_left t)

      | Branch(p, m, s0, s1, _), Branch(q, n, t0, t1, _) ->
	if (p = q) && (m = n) then
	  (* The trees have the same prefix. recurse on the sub-trees *)
          join (compute s0 t0) (compute s1 t1)
	else if (Big_Endian.shorter m n) && (match_prefix q p m) then
          (* [q] contains [p]. Merge [t] with a sub-tree of [s]. *)
	  if (q land m) = 0 then
	    join (compute s0 t) (empty_right s1)
	  else
            join (compute s1 t) (empty_right s0)
	else if (Big_Endian.shorter n m) && (match_prefix p q n) then
	  (* [p] contains [q]. Merge [s] with a sub-tree of [t]. *)
	  if (p land n) = 0 then
            join (compute s t0) (empty_left t1)
	  else
            join (compute s t1) (empty_left t0)
	else
	  (* The prefixes disagree. *)
          join (empty_right s) (empty_left t)
    in
    fun s t -> compute s t


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


    let generic_predicate exn ~cache ~decide_fast ~decide_fst ~decide_snd ~decide_both =
      if debug_cache then Format.eprintf "CACHE generic_predicate %s@." (fst cache);
      let module Cache =
	Binary_cache.Binary_Predicate(Cacheable)(Cacheable)
      in
      clear_caches := Cache.clear :: !clear_caches;
      make_predicate Cache.merge exn
	~decide_fast ~decide_fst ~decide_snd ~decide_both 

    let generic_symmetric_predicate exn ~decide_fast ~decide_one ~decide_both =
      if debug_cache then Format.eprintf "CACHE generic_symmetric_predicate@.";
      let module Cache =
	Binary_cache.Symmetric_Binary_Predicate(Cacheable)
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


  let binary_predicate ct pt ~decide_fast ~decide_fst ~decide_snd ~decide_both =
    let cache_merge = match ct with
      | Hptmap_sig.NoCache -> (fun f x y -> f x y)
      | Hptmap_sig.PersistentCache _name | Hptmap_sig.TemporaryCache _name ->
        if debug_cache then Format.eprintf "CACHE binary_predicate %s@." _name;
        let module Cache =
              Binary_cache.Binary_Predicate(Cacheable)(Cacheable)
        in
        (match ct with
          | Hptmap_sig.PersistentCache _ ->
            clear_caches := Cache.clear :: !clear_caches
          | _ -> ());
        Cache.merge
    in
    make_binary_predicate cache_merge pt
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let symmetric_binary_predicate ct pt ~decide_fast ~decide_one ~decide_both =
    let cache_merge = match ct with
      | Hptmap_sig.NoCache -> (fun f x y -> f x y)
      | Hptmap_sig.PersistentCache _name | Hptmap_sig.TemporaryCache _name ->
        if debug_cache then Format.eprintf "CACHE symmetric_binary_predicate %s@." _name;
        let module Cache = Binary_cache.Symmetric_Binary_Predicate(Cacheable) in
        (match ct with
          | Hptmap_sig.PersistentCache _ ->
            clear_caches := Cache.clear :: !clear_caches
          | _ -> ());
        Cache.merge
    in
    make_binary_predicate cache_merge pt
      ~decide_fast ~decide_fst:decide_one ~decide_snd:decide_one ~decide_both


    let cached_fold ~cache_name ~temporary ~f ~joiner ~empty =
      if debug_cache then Format.eprintf "CACHE cached_fold %s@." cache_name;
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

  (** [union m1 m2] returns the union of the maps [m1] and [m2]. Bindings in
      [m2] take precedence over those in [m1]. *)
  let union =
    join ~cache:Hptmap_sig.NoCache ~symmetric:false
      ~idempotent:true ~decide:(fun _ _ d -> d)


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
          (* TODO: this function is suboptimal because it recurses even when
             the key will never be found: missing
             [if match_prefix id prefix mask then] *)
          if (id land mask) = 0 then
            let (ll, pres, rl) = aux l in (ll, pres, union rl r)
          else
            let (lr, pres, rr) = aux r in (union l lr, pres, rr)
      in
      aux htr

    let shape x = ((x : t) :> V.t shape)

    let clear_caches () = List.iter (fun f -> f ()) !clear_caches

end



(*
Local Variables:
compile-command: "make -C .."
End:
*)
