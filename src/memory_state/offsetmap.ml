(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Abstract_interp

(* This module uses Bigints everywhere. Set up some notations *)
let pretty_int = Int.pretty
let ( =~ ) = Integer.equal
let ( <>~ ) x y = not (Integer.equal x y)
let ( <~ ) = Integer.lt
let ( >~ ) = Integer.gt
let ( <=~ ) = Integer.le
let ( >=~ ) = Integer.ge
let ( +~ ) = Integer.add
let ( -~ ) = Integer.sub
(*let ( *~ ) = Integer.mul*)
let ( /~ ) = Integer.pos_div
let ( %~ ) = Integer.pos_rem
let succ = Integer.succ
let pred = Integer.pred


module Make (V : module type of Offsetmap_lattice_with_isotropy) = struct

  open Format

  exception Result_is_bottom

  type v = V.t
  type widen_hint = V.widen_hint

  type tt =
    | Empty
      (* min, the lower bound of the key interval, is always zero because
         trees are relative.

         max *
         offset_left * subtree_left *
         offset_right * subtree_right *
         rem * modu * value *
         tag
       *)
    | Node of
        Integer.t *
          Integer.t * tt *
          Integer.t * tt *
          Rel.t * Integer.t * V.t *
          int

  let equal (t1:tt) (t2:tt) = t1 == t2

  let compare t1 t2 = match t1, t2 with
    | Empty, Empty -> 0
    | Empty, Node _ -> -1
    | Node _, Empty -> 1
    | Node (_, _, _, _, _, _, _, _, h1), Node (_, _, _, _, _, _, _, _, h2) ->
        Datatype.Int.compare h1 h2

 (** Pretty printing *)

 let pretty_offset_aux s curr_off ppf tree =
   if tree == Empty
   then Format.fprintf ppf "@[empty at %a@]" pretty_int curr_off
   else
     let rec pretty_offset s curr_off ppf tree =
       match tree with
         | Empty -> ()
         | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
             pretty_offset "" (curr_off +~ offl) ppf subl;
             Format.fprintf ppf "@[%s[%a..%a] -> (%a, %a, %a);@]@ "
               s
               pretty_int curr_off
               pretty_int (max +~ curr_off)
               Rel.pretty rem
               pretty_int modu
               V.pretty v;
             pretty_offset "" (curr_off +~ offr) ppf subr;
     in pretty_offset s curr_off ppf tree
 ;;

 let _pretty_offset fmt (off, t) =
   Format.fprintf fmt "@[<v><off: %a>@ %a@]"
     pretty_int off (pretty_offset_aux "r" off) t;
 ;;

 let pretty fmt t =
   Format.fprintf fmt "@[<v>%a@]" (pretty_offset_aux "r" Integer.zero) t;
 ;;

 include
  (struct

    (* This function is almost injective. Can we do better, eg. by mapping Empty
       to 0 and skipping this value for all nodes? And it is worth it? *)
    let hash = function
      | Empty -> 311
      | Node(_,_,_,_,_,_,_,_,tag) -> tag

    let rehash_ref = ref (fun _ -> assert false)
    module D = Datatype.Make
      (struct
        type t = tt
        let name = V.name ^ " newoffsetmap"
        let reprs = [ Empty ]
        open Structural_descr
        let r = Recursive.create ()
        let structural_descr =
          let p_bint = Datatype.Big_int.packed_descr in
          t_sum
            [| [| p_bint;
                  p_bint;
                  recursive_pack r;
                  p_bint;
                  recursive_pack r;
                  p_bint;
                  p_bint;
                  V.packed_descr;
                  p_int |] |]
        let () = Recursive.update r structural_descr
        let equal = equal
        let hash = hash
        let compare = compare
        let rehash x = !rehash_ref x
        let copy = Datatype.undefined
        let internal_pretty_code = Datatype.undefined
        let pretty = pretty
        let varname = Datatype.undefined
        let mem_project = Datatype.never_any_project
       end)
    include D

  (* Basic operations on nodes *)
    let empty = Empty;;
    let is_empty t = t == Empty

    let equal_internal t1 t2 =
      match t1, t2 with
      | Empty, Empty -> true
      | Node _, Empty | Empty, Node _ -> false
      | Node (max1, offl1, subl1, offr1, subr1, rem1, modu1, v1, _),
          Node (max2, offl2, subl2, offr2, subr2, rem2, modu2, v2, _)
          ->
          subl1 == subl2 &&
            subr1 == subr2 &&
            offl1 =~ offl2 &&
            offr1 =~ offr2 &&
            V.equal v1 v2 &&
            max1 =~ max2 &&
            Rel.equal rem1 rem2 &&
            modu1 =~ modu2

    let hash_internal t =
      match t with
        Empty -> 97
      | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
          let h = Integer.hash max in
          let h = 31 * h + Integer.hash offl in
          let h = 31 * h + hash subl in
          let h = 31 * h + Integer.hash offr in
          let h = 31 * h + hash subr in
          let h = 31 * h + Rel.hash rem in
          let h = 31 * h + Integer.hash modu in
          let h = 31 * h + V.hash v in
          h

    module NewoHashconsTbl =
      State_builder.Hashconsing_tbl
        (struct
          include D
          let hash_internal = hash_internal
          let equal_internal = equal_internal
          let initial_values = []
         end)
        (struct
          let name = name
          let dependencies = [ Ast.self ]
          let size = 137
        end)
    let () = Ast.add_monotonic_state NewoHashconsTbl.self

    let counter = ref 0

    let singleton_tag t = 
      match t with
	Empty -> min_int
      | Node(_, _, _, _, _, _, _, _, tag) ->
	  tag land min_int

    let nNode cur offl subl offr subr f g v =
      let current_counter = !counter in
      let tag = 
	if V.cardinal_zero_or_one v
	then (singleton_tag subl) land (singleton_tag subr)
	else 0
      in
      let tag = tag lor current_counter in
      let tentative_new_node = Node(cur, offl, subl, offr, subr, f, g, v,tag) in
      let hashed_node = NewoHashconsTbl.merge tentative_new_node in
      if hashed_node == tentative_new_node
      then begin
         if current_counter = max_int 
         then Kernel.fatal "Internal maximum exeeded";
         counter := Pervasives.succ current_counter;
      end;
      hashed_node

    let rehash_node x = match x with
      | Empty -> empty
      | Node _ -> 
	  NewoHashconsTbl.merge x

    let () = rehash_ref := rehash_node

  end :
    sig
      include Datatype.S with type t = tt
      val empty : t
      val hash: t -> int
      val nNode :
        Integer.t ->
        Integer.t -> t ->
        Integer.t -> t ->
        Rel.t -> Integer.t -> V.t ->
        t
      val is_empty : t -> bool
      val singleton_tag : t -> int
    end)

 module Cacheable = struct
   type t = Integer.t * tt
   let hash (i, t: t) = Integer.hash i + 37 * hash t
   let equal (i1, t1: t) (i2, t2: t) = t1 == t2 && i1 =~ i2
   let sentinel = Integer.minus_one, empty
 end
 let clear_caches = ref []


  let equal_vv (rem1, modu1, v1) (rem2, modu2, v2) =
    rem1 =~ rem2 && modu1 =~ modu2 && V.equal v1 v2
  ;;

  let get_vv node curr_off =
    match node with
    | Empty -> assert false
    | Node (_, _, _, _, _, remrel, modu, v, _) ->
        let rem = (Rel.add_abs curr_off remrel) %~ modu in
        rem, modu, v
  ;;

  let _get_v = function
    | Empty -> assert false
    | Node (_, _, _, _, _, _, _, v, _) ->
        v
  ;;

  let get_max = function
    | Empty -> assert false
    | Node (max, _, _, _, _, _, _, _, _) ->
        max
  ;;

  let get_modu = function
    | Empty -> assert false
    | Node (_, _, _, _, _, _, modu, _, _) -> modu
  ;;

  let is_above min1 max1 min2 max2 =
    if min1 =~ Integer.zero then true
    else if min2 =~ Integer.zero then false
    else
      let signature_interval min max =
        Integer.logxor (pred min) max
      in
      signature_interval min1 max1 >~ signature_interval min2 max2
  ;;


  type zipper =
    | End
    | Right of Integer.t * t * zipper
    | Left of Integer.t * t * zipper;;
  (** Zippers : Offset of a node * Node * continuation of the zipper *)

  exception End_reached;;
  exception Empty_tree;;

  let _pr_zipper ppf z  =
    printf "[Zipper]---@.";
    let rec aux ppf = function
      | End -> printf "@ E@."
      | Right (o, Node(max, _, _, _, _subr, _, _, _, _),z ) ->
          fprintf ppf "@[<h 2> [%a,%a] R@\n%a@]"
            pretty_int o
            pretty_int (o +~ max)
            aux z
      | Left (o, Node(max, _, _, _, _subr, _, _, _, _),z ) ->
          fprintf ppf "@[<h 2> [%a,%a] L@\n%a@]"
            pretty_int o
            pretty_int (o +~ max)
            aux z
      |  Right (_, Empty, _) | Left (_, Empty, _) -> assert false
    in aux ppf z;
    printf "[/Zipper]---@.@.";
  ;;

 (** Returns an absolute position and an associated new tree *)
 let rec rezip zipper curr_off node =
   match zipper with
   | End -> curr_off, node
   | Right (offset, Node(max, offl, subl, _offr, _subr, rem, modu, v, _), z)
     ->
       rezip z offset
         (nNode max offl subl (curr_off -~ offset) node rem modu v)
   | Left (offset, Node(max, _offl, _subl, offr, subr, rem, modu, v, _), z)
     ->
       rezip z offset
         (nNode max (curr_off -~ offset) node offr subr rem modu v)
   | Right (_, Empty, _) | Left (_, Empty, _) -> assert false
 ;;

 (** Returns an absolute position, a node and a zipper *)
 let rec leftmost_child curr_off zipper node =
   match node with
   | Empty -> raise Empty_tree
   | Node (_, _, Empty, _, _, _, _, _, _) -> curr_off, node, zipper
   | Node (_, offl, subl, _, _, _, _, _, _) ->
       let new_offset = curr_off +~ offl in
       leftmost_child new_offset (Left (curr_off, node, zipper)) subl
 ;;

 (** Returns an absolute position, a node and a zipper *)
 let rec rightmost_child curr_off zipper node =
   match node with
   | Empty -> raise Empty_tree
   | Node (_, _, _, _, Empty, _, _, _, _) -> curr_off, node, zipper
   | Node (_, _offl, _subl, offr, subr, _, _, _, _) ->
       let new_offset = curr_off +~ offr in
       rightmost_child new_offset (Right (curr_off, node, zipper)) subr
 ;;

 (** Move to the right of the current node.
     Uses a zipper for that.
  *)
 let move_right curr_off node zipper =
   match node with
   | Node (_, _, _, offr, ((Node _ ) as subr), _, _, _, _) ->
       let new_offset = curr_off +~ offr in
       leftmost_child new_offset (Right (curr_off, node, zipper)) subr
   | Node (_, _, _, _, Empty, _, _, _, _) ->
        begin
         let rec unzip_until_left zipper =
           match zipper with
           | End -> raise End_reached
           | Right (_, _, z) -> unzip_until_left z
           | Left (offset, tree, z) -> offset, tree, z
         in unzip_until_left zipper
       end
   | Empty -> assert false
  ;;

 type imp_zipper = {
   mutable offset: Integer.t;
   mutable node: t;
   mutable zipper: zipper;
 };;

 let imp_move_right imp_z =
   let o, n, z = move_right imp_z.offset imp_z.node imp_z.zipper in
   imp_z.offset <- o;
   imp_z.node <- n;
   imp_z.zipper <- z;
 ;;

 (* Minimum and maximum bit bounds in the offsetmap (inclusively), assumming
    that [m] starts at [curr_off]. Usually not required, as we use [validity]
    arguments, that give the size of the offsetmap. Beware that this function
    returns something incorrect if [m] is [Empty]. *)
 let bounds_offset curr_off m =
   let rec min curr_off = function
     | Empty -> curr_off
     | Node (_, offl, subl, _, _, _, _, _, _) -> min (curr_off +~ offl) subl
   and max curr_off = function
     | Empty -> curr_off
     | Node (_, _, _, offr, subr, _, _, _, _) -> max (curr_off +~ offr) subr
   in
   (min curr_off m, max curr_off m)

 let _bounds m = bounds_offset Int.zero m

 (** Folding and iterating from the leftmost node to the rightmost one
     If t =  n0         fold f t i = f n2 (f n0 (f n1 i))
            / \         iter f t   = f n1; fn0; f n2;
           n1  n2
  *)
 let fold_offset f o t =
  let o, n, z = leftmost_child o End t in
  let rec aux_fold o t z pre =
    match t with
    | Empty -> pre
    | Node (max, _, _, _, _, r, m, v, _) ->
        let abs_max = max +~ o in
        let now = f (o, abs_max) (v, m, r) pre in
        try
          let  no, nt,  nz = move_right o t z in
          aux_fold no nt nz now
        with End_reached -> now
  in aux_fold o n z
 ;;

 let fold f t = fold_offset f Integer.zero t
  ;;

 let iter_offset f o t =
  let o, n, z = leftmost_child o End t in
  let rec aux_iter o t z =
     match t with
    | Empty -> ()
    | Node (max, _, _, _, _, r, m, v, _) ->
        begin
          let abs_max = max +~ o in
          f (o, abs_max) (v, m, r);
          try
            let no, nt, nz = move_right o t z  in
            aux_iter no nt nz
          with End_reached -> ()
        end
  in aux_iter o n z
 ;;

 let iter f t = iter_offset f Integer.zero t
 ;;

 (* Same as iter, but does not compute offsets (hence more efficient). *)
 let rec iter_on_values f t =
   match t with
     | Empty -> ()
     | Node (_, _, left, _, right, _, modu, v, _) ->
         iter_on_values f left;
         f v modu;
         iter_on_values f right
;;

 let rec fold_on_values f t acc =
   match t with
     | Empty -> acc
     | Node (_, _, left, _, right, _, modu, v, _) ->
         fold_on_values f right (f v modu ((fold_on_values f left acc)))
;;

 (** Smart constructor for nodes:
     it glues the node being allocated to potential candidates if needed
     (i.e. leftmost node of right subtree and rightmost node of left subtree),
  *)

 let make_node curr_off max offl subl offr subr rem modu v =
   let rem, modu =
     if V.is_isotropic v
     then Integer.zero, Integer.one
     else rem, modu
   in
   let curr_vv = (rem, modu, v) in

   let max, offr, subr =
     try
       let offset, nr, zr =
         leftmost_child (curr_off +~ offr) End subr in
       match nr with
       | Node (nmax, _, nsubl , noffr, nsubr, nrelrem, nmodu, nv, _) ->
           assert (is_empty nsubl);
           let nrem = (Rel.add_abs offset nrelrem) %~ nmodu in
           if equal_vv (nrem, nmodu, nv) curr_vv &&
             (V.cardinal_zero_or_one v || (offset %~ modu =~ rem))
           then
             begin
               let curr_offr, new_subr = rezip zr (offset +~ noffr) nsubr in
               let new_max = succ (max +~ nmax) in
               let new_offr = curr_offr -~ curr_off
               in
               new_max, new_offr, new_subr
             end
           else max, offr, subr
       | Empty -> assert false
     with Empty_tree -> max, offr, subr
   in
   let curr_off, max, offl, subl, offr =
     try
       let offset, nl, zl =
         rightmost_child (curr_off +~ offl) End subl in
       match nl with
       | Node (nmax, noffl, nsubl , _, noffr, nrelrem, nmodu, nv, _) ->
           assert (is_empty noffr);
           let nrem = (Rel.add_abs offset nrelrem) %~ nmodu in
           if equal_vv (nrem, nmodu, nv) curr_vv && (curr_off %~ modu =~ rem)
           then (
               let new_curr_offl, new_subl = rezip zl (offset +~ noffl) nsubl in
               let succ_nmax = succ nmax in
               let lmax = max +~ succ_nmax in
               let new_offl = new_curr_offl -~ offset in
               let new_offr = offr +~ succ_nmax in
               let new_coff = curr_off -~ succ_nmax in
               (*assert (new_coff =~ offset);*)
               new_coff, lmax, new_offl, new_subl, new_offr)
           else curr_off, max, offl, subl, offr
       |Empty -> assert false
     with Empty_tree -> curr_off, max, offl, subl, offr
   in
   let remrel = Rel.pos_rem (Rel.sub_abs rem curr_off) modu in
   curr_off, nNode max offl subl offr subr remrel modu v
 ;;

 (** Smart add node:
     Adds a node to the current tree and merges (new) consecutive intervals
     containing the same values
     The node is [min..max] rem, modu, v and
     the tree to which it is added is rooted at offset curr_off
     Hypothesis: the tree is in canonical form w.r.t having no
     mergeable intervals.
  *)
 let add_node min max rem modu v curr_off tree =
   let rec aux_add curr_off tree =
     match tree with
     | Empty ->
         let sz = max -~ min in
         make_node min sz Integer.zero empty (succ sz) empty rem modu v
     | Node (nmax, noffl, nsubl, noffr, nsubr, nremrel, nmodu, nv, _) ->
         let nrem = (Rel.add_abs curr_off nremrel) %~ nmodu in
         let abs_min = curr_off
         and abs_max = nmax +~ curr_off in
         if max <~ abs_min then
           begin
             if is_above min max abs_min abs_max then
               let new_offr = abs_min -~ min in
               (*Format.printf "add to the left above@."; *)
               make_node min (max -~ min) Integer.zero empty
                 new_offr tree rem modu v
             else
               begin
                 (*     Format.printf "L@ co:%a@ t:%a@ [%a...%a]@.@."
                        pretty_int curr_off
                        (pretty_offset curr_off) tree
                        pretty_int min pretty_int max
                        ; *)
                 let new_curr_offl, new_node =
                   aux_add (curr_off +~ noffl) nsubl
                 in
                 let new_offl = new_curr_offl -~ curr_off in
                 make_node
                   curr_off nmax new_offl new_node noffr nsubr nrem nmodu nv
               end
           end
         else
           begin
             if is_above min max abs_min abs_max then
               begin
                 let new_offl = abs_min -~ min in
                 let new_max = max -~ min in
                 make_node
                   min new_max new_offl tree (succ new_max) empty rem modu v
               end

             else
               begin
                 (*           Format.printf "add to the right Not ABOVE@."; *)
                 let new_curr_offr, new_node =
                   aux_add (curr_off +~ noffr) nsubr
                 in
                 let new_offr = new_curr_offr -~ abs_min in
                 make_node abs_min nmax noffl nsubl new_offr new_node nrem
                   nmodu nv
               end
           end

   in aux_add curr_off tree
 ;;

 let add_node_from_root ~min ~max ~rem ~modu ~v t =
   snd (add_node min max rem modu v Integer.zero t)

 let add_basic_node ~min ~max ~v m =
   if V.is_isotropic v then
     add_node_from_root ~min ~max ~rem:Integer.zero ~modu:Integer.one ~v m
   else
     let size = Integer.length min max in
     let v = V.anisotropic_cast ~size v in
     let rem = min %~ size in
     add_node_from_root ~min ~max ~rem ~modu:size ~v m


 (** Checks that [tree] is sanely built  *)

 let rec _check curr_off tree =
   match tree with
   | Empty -> ()
   | Node (max, offl, subl, offr, subr, rem, modu, _v, _) ->
       assert (Rel.check ~rem ~modu);
       assert (not (is_empty subl) || Integer.is_zero offl);
       assert (not (is_empty subr) || offr =~ succ max);
       let abs_min = curr_off
       and abs_max = curr_off +~ max in
       let aux offset tree =
         match tree with
         | Empty -> ()
         | Node (nmax, _, _, _, _, _, _, _, _) ->
             let nabs_min = curr_off +~ offset in
             let nabs_max = nmax +~ nabs_min in
             assert (is_above abs_min abs_max nabs_min nabs_max)
       in aux offl subl; aux offr subr;
       _check (curr_off +~ offl) subl;
       _check (curr_off +~ offr) subr;
 ;;

 (** Inclusion functions *)

 (* Auxiliary fonction for inclusion: check that, between [mabs_min] and
    [mabs_max], the values (r1, m1, v1) and (r2, m2, v2), respectively
    bound between (amin1, amax1) and (amin2, amax2), are included. *)
 let is_included_nodes_values (amin1 : Integer.t) (amax1 : Integer.t) r1 m1 v1 amin2 amax2 r2 m2 v2 mabs_min mabs_max =
   if V.is_isotropic v1 || V.is_isotropic v2 then
     V.is_included v1 v2
   else
     let max_test =
       if amax1 <~ amax2
       then (succ mabs_max) %~ m1 =~ r1
       else true
     in
     let ok_min = amin1 =~ amin2 || mabs_min %~ m1 =~ r1
     and ok_max = amax1 =~ amax2 || max_test
     in
     if r1 =~ r2 && m1 =~ m2 && ok_min && ok_max
     then V.is_included v1 v2
     else false

 (* Functional for inclusion test. *)
 let is_included_aux cache (o1, t1) (o2, t2) =
   match t1, t2 with
     | Empty, _ -> true (* BYTODO *)
     | _, Empty -> true (* BYTODO *)
     | Node (max1, offl1, subl1, offr1, subr1, r1rel, m1, v1, _),
       Node (max2, offl2, subl2, offr2, subr2, r2rel, m2, v2, _) ->
       let amin1 = o1 in
       let amax1 = max1 +~ o1 in
       let amin2 = o2 in
       let amax2 = max2 +~ o2 in
       let ol1 = o1 +~ offl1 in
       let ol2 = o2 +~ offl2 in
       let or1 = o1 +~ offr1 in
       let or2 = o2 +~ offr2 in
       let r1 = (Rel.add_abs o1 r1rel) %~ m1 in
       let r2 = (Rel.add_abs o2 r2rel) %~ m2 in
       if amax1 <~ amin2  then
         cache (o1, t1) (ol2, subl2) &&
         cache (or1, subr1) (o2, t2)
       else if amin1 >~ amax2 then
         cache (o1, t1) (or2, subr2) &&
         cache (ol1, subl1) (o2, t2)
       else begin (* this node of t2 covers part of the interval of t1 we are
                     focused on *)
         if amin1 =~ amin2 then
           let mabs_min = amin1 in
           begin
             (if amax1 =~ amax2 then begin
               (if (r1 =~ r2 && m1 =~ m2) ||
                   V.is_isotropic v1 || V.is_isotropic v2
                then V.is_included v1 v2
                else false)
               &&
                 cache (or1, subr1) (or2, subr2)
              end
              else if amax1 >~ amax2 then begin
                is_included_nodes_values
                  amin1 amax1 r1 m1 v1
                  amin2 amax2 r2 m2 v2 mabs_min amax2
                &&
                cache (o1, t1) (or2, subr2)
              end
              else
                begin (* amax1 <~ amax2 *)
                  is_included_nodes_values
                    amin1 amax1 r1 m1 v1
                    amin2 amax2 r2 m2 v2 mabs_min amax1
                  &&
                  cache (or1, subr1) (o2, t2)
                end
             ) &&
             cache (ol1, subl1) (ol2, subl2)
           end
         else
           (* treat the common interval and the right parts of the trees.
              The common interval starts at [mabs_min] and goes up to
              [min amax1 amax2]. *)
           let treat_current_right_nodes mabs_min =
             if amax1 =~ amax2 then begin
               is_included_nodes_values
                 amin1 amax1 r1 m1 v1
                 amin2 amax2 r2 m2 v2 mabs_min amax1
               &&
               cache (or1, subr1) (or2, subr2)
             end
             else if amax1 >~ amax2 then begin
               is_included_nodes_values
                 amin1 amax1 r1 m1 v1
                 amin2 amax2 r2 m2 v2 mabs_min amax2
               &&
               cache (o1, t1) (or2, subr2)
             end
             else
               begin (* amax1 <~ amax2 *)
                 is_included_nodes_values
                   amin1 amax1 r1 m1 v1
                   amin2 amax2 r2 m2 v2 mabs_min amax1
                 &&
                 cache (or1, subr1) (o2, t2)
               end;
           in
           (* Find the beginning of the common part of the two intervals (ie.
              [mabs_min] above, which is by definition [max amin1 amin2]), and
              treat this interval and the right trees. Then, check the inclusion
              of the subtree that starts just before [mabs_min] with the
              entire other tree. *)
           if amin1 >~ amin2 then begin
             treat_current_right_nodes amin1 &&
             cache (ol1, subl1) (o2, t2)
           end
           else begin (* amin1 <~ amin2 *)
             treat_current_right_nodes amin2 &&
             cache (o1, t1) (ol2, subl2)
           end
       end
 ;;

 module IsIncludedCache = Binary_cache.Make_Binary(Cacheable)(Cacheable)
 let () = clear_caches := IsIncludedCache.clear :: !clear_caches;;

 let is_included t1 t2 =
   let rec aux t1 t2 =
     if Cacheable.equal t1 t2
     then true
     else is_included_aux (IsIncludedCache.merge aux) t1 t2
   in
   aux (Integer.zero, t1) (Integer.zero, t2)
 ;;

 (** Joins two trees with no overlapping intervals.  *)

 let rec union t1_curr_off t1 t2_curr_off t2 =
   (* Format.printf "Union t1:%a t2:%a@."
      (pretty_offset t1_curr_off) t1
      (pretty_offset t2_curr_off) t2;
   *)
   match t1, t2 with
   | Empty, Empty ->
       assert (t1_curr_off =~ t2_curr_off);
       t1_curr_off, empty
   | Empty, Node _ -> t2_curr_off, t2
   | Node _, Empty -> t1_curr_off, t1
   | Node (lmax, loffl, lsubl, loffr, lsubr, lremrel, lmodu, lv, _),
       Node (rmax, roffl, rsubl, roffr, rsubr, rremrel, rmodu, rv, _) ->
         let labs_min = t1_curr_off
         and labs_max = lmax +~ t1_curr_off
         and rabs_min = t2_curr_off
         and rabs_max = rmax +~ t2_curr_off
         in
         let lrem = (Rel.add_abs t1_curr_off lremrel) %~ lmodu in
         let rrem = (Rel.add_abs t2_curr_off rremrel) %~ rmodu in
         if is_above labs_min labs_max rabs_min rabs_max
         then
           (* t2 is on the right of t1 *)
           let new_curr_offr, new_subr =
             union (t1_curr_off +~ loffr) lsubr t2_curr_off t2
           in
           make_node t1_curr_off lmax loffl lsubl
             (new_curr_offr -~ t1_curr_off) new_subr lrem lmodu lv
         else
           begin
             (* t1 is on the left of t2 *)
 (*            assert (is_above rabs_min rabs_max labs_min labs_max); *)
             let new_curr_offl, new_subl =
               union t1_curr_off t1 (t2_curr_off +~ roffl) rsubl
             in
             make_node t2_curr_off rmax
               (new_curr_offl -~ t2_curr_off) new_subl roffr rsubr
               rrem rmodu rv
           end
 ;;

 (** Merge two trees that span the same range. This function is a functional:
     [cache] must be used for recursive calls on subtrees.
     [f_aux] is the function that merges the intervals point-wise. *)
 let merge cache f_aux (o1, t1) (o2, t2) =
   match t1, t2 with
     | Empty, Empty -> assert false
     | Node _, Empty -> assert false
     | Empty, Node _ -> assert false
     | Node (max1, offl1, subl1, offr1, subr1, rem1rel, modu1, v1, _),
       Node (max2, offl2, subl2, offr2, subr2, rem2rel, modu2, v2, _) ->
       let abs_min1 = o1
       and abs_max1 = max1 +~ o1
       and abs_min2 = o2
       and abs_max2 = max2 +~ o2
       in
       let rem1 = (Rel.add_abs o1 rem1rel) %~ modu1 in
       let rem2 = (Rel.add_abs o2 rem2rel) %~ modu2 in
       if abs_min2 >~ abs_max1 then
         if is_above abs_min1 abs_max1 abs_min2 abs_max2
         then (* t2 is on the right of t1 *)
           let off, t = cache (o1 +~ offr1, subr1) (o2, t2) in
           make_node o1 max1 offl1 subl1 (off -~ o1) t rem1 modu1 v1
         else(* t1 is on the left of t2 *)
           begin
               (* Format.printf "t2:[%a %a] %a @.t1:[%a %a] %a@." pretty_int
                  abs_min2 pretty_int abs_max2 (pretty_debug_offset o2) t2
                  pretty_int abs_min1
                  pretty_int abs_max1 (pretty_debug_offset o1) t1; *)
               (*  assert (is_above abs_min2 abs_max2 abs_min1 abs_max1);  *)
             let off, t = cache (o1, t1) (o2 +~ offl2, subl2) in
             make_node o2 max2 (off -~ o2) t offr2 subr2 rem2 modu2 v2
           end
       else if abs_min1 >~ abs_max2 then
         if is_above abs_min1 abs_max1 abs_min2 abs_max2
         then
             (* t2 is on the left of t1 *)
           let off, t = cache (o1 +~ offl1, subl1) (o2, t2) in
           make_node o1 max1 (off -~ o1) t offr1 subl1 rem1 modu1 v1
         else
           begin
             assert (is_above abs_min2 abs_max2 abs_min1 abs_max1);
             (* t1 is on the right of t2 *)
             let off, t = cache (o1, t1) (o2 +~ offr2, subr2) in
             make_node o2 max2 offl2 subl2 (off -~ o2) t rem2 modu2 v2
           end
       else
           (* here n1 \inter n2 <> \emptyset:
              -compute the intersection interval: middle_abs_min, middle_abs_max
              - add the rest of the nodes to their left/right subtree
              depending on the size of the node
              - add the new node in the merged left subtree
              and plug the merged right tree in
           *)
         let (curr_offl, left_t), middle_abs_min =
           let abs_offl1 = o1 +~ offl1
           and abs_offl2 = o2 +~ offl2 in
           if abs_min1 =~ abs_min2  then
             cache (abs_offl1, subl1) (abs_offl2, subl2), abs_min1
           else if abs_min1 <~ abs_min2 then
             let new_offl1, new_subl1 =
               add_node abs_min1 (pred abs_min2)
                 rem1 modu1 v1 abs_offl1 subl1
             in cache (new_offl1, new_subl1) (abs_offl2, subl2), abs_min2
           else
             begin
               assert (abs_min1 >~ abs_min2);
               let new_offl2, new_subl2 =
                 add_node abs_min2 (pred abs_min1) rem2 modu2
                   v2 abs_offl2 subl2
               in cache (abs_offl1, subl1) (new_offl2, new_subl2), abs_min1
             end
         in
         let (curr_offr, right_t), middle_abs_max =
           let abs_offr1 = o1 +~ offr1
           and abs_offr2 = o2 +~ offr2 in
           if abs_max1 =~ abs_max2 then
             cache (abs_offr1, subr1) (abs_offr2, subr2), abs_max1
           else if abs_max1 <~ abs_max2 then
             let new_offr2, new_subr2 =
               add_node
                 (succ abs_max1) abs_max2 rem2 modu2 v2 abs_offr2 subr2
             in
             cache (abs_offr1, subr1) (new_offr2, new_subr2), abs_max1
           else
             begin
               assert (abs_max1 >~ abs_max2);
               let min = (succ abs_max2) in
               let new_offr1, new_subr1 =
                 add_node min abs_max1 rem1 modu1 v1 abs_offr1 subr1
               in
               cache (new_offr1, new_subr1) (abs_offr2, subr2), abs_max2
             end
         in

         let rem, modu, v =
           f_aux middle_abs_min middle_abs_max rem1 modu1 v1 rem2 modu2 v2
         in
         let curr_offl, left_t =
           add_node middle_abs_min middle_abs_max rem modu v curr_offl left_t
         in union curr_offl left_t curr_offr right_t
 ;;


 let extract_bits ~start ~stop ~modu v =
   assert (start <=~ stop && stop <=~ modu);
   let start,stop =
     if Cil.theMachine.Cil.theMachine.Cil_types.little_endian then
       start,stop
     else
       let mmodu = pred modu in
       mmodu -~ stop, mmodu -~ start
   in
   V.extract_bits ~start ~stop ~size:modu v
 ;;

 let merge_bits ~conflate_bottom ~offset ~length ~value ~total_length acc =
   assert (length +~ offset <=~ Integer.of_int total_length);
   if Cil.theMachine.Cil.theMachine.Cil_types.little_endian then
     V.little_endian_merge_bits
       ~conflate_bottom
       ~offset ~value ~total_length acc
   else
     V.big_endian_merge_bits
       ~conflate_bottom
       ~offset ~value ~total_length ~length acc
 ;;

 (*
   [offset] is the offset where the read has begun (ie the global read start).
   [size] is the total size we want to read from [offset].
   [curr_off] and [(rem, modu, v)] refer to the current node to be read.
   [acc] is the current state of accumulated reads.
 *)
 let extract_bits_and_stitch ~topify ~conflate_bottom ~offset ~size curr_off (rem, modu, v) max acc =
   let inform = ref false in
   let r =
     let abs_max = curr_off +~ max in
     (*  last bit to be read,
         be it in the current node or one of its successors *)
     let max_bit = pred (offset +~ size) in
     let extract_single_step min acc =
       assert (not (V.is_isotropic v));
       let interval_offset = min -~ offset in
       let merge_offset =
         if interval_offset >=~ Integer.zero then interval_offset else Integer.zero
       in
       let start = (min -~ rem) %~ modu in
       let modu_end = if rem =~ Integer.zero then pred modu else pred rem in
       (* where do we stop reading ?
          either at the end of the current slice (round_up_to_r min) or
          at the end of the interval (abs_max)
       *)
       let read_end =
         Integer.min 
           (Integer.min (Integer.round_up_to_r ~min ~r:modu_end ~modu) abs_max) 
	   max_bit 
       in
       let stop = (read_end -~ rem) %~ modu in
(*       Format.printf "Single step: merge offset %a length %a \
 start %a stop %a total length %a offset %a max bit %a\
 @\n current offset %a Rem %a modu %a V %a@."
         pretty_int merge_offset pretty_int (Integer.length start stop)
         pretty_int start pretty_int stop pretty_int size
         pretty_int offset pretty_int max_bit
         pretty_int curr_off pretty_int rem pretty_int modu V.pretty v ; *)
       let this_inform, read_bits = extract_bits ~topify ~start ~stop ~modu v in
       (* Format.printf "After single step: read bits %a@." V.pretty read_bits; *)
       inform := !inform || this_inform;
       let result = 
	 merge_bits ~topify ~conflate_bottom
           ~offset:merge_offset ~length:(Integer.length start stop)
           ~value:read_bits ~total_length:(Integer.to_int size) acc
       in
       (* Format.printf "After merge_bits: result %a@." V.pretty result; *)
       read_end, result
     in
     let start = Integer.max offset curr_off
     and stop = Integer.min max_bit abs_max in
     if V.is_isotropic v then
       let interval_offset = rem -~ start (* ? *) in
       let merge_offset =
         if interval_offset <~ Integer.zero
         then Integer.zero
         else interval_offset
       in merge_bits ~topify ~conflate_bottom ~offset:merge_offset
            ~length:(Integer.length start stop)
            ~value:v ~total_length:(Integer.to_int size) acc
     else
       let start_point = ref start in
       let acc = ref acc in

       while !start_point <=~ stop do
	 let read_end, result = extract_single_step !start_point !acc in
         acc := result;
         start_point := succ read_end;
       done;
       !acc;
   in
  (* Format.printf "extract_bits_and_stitch istart@ %a@ size %a\
     coff %a abs_max -- val %a@\n  acc %a res %a@."
     pretty_int offset pretty_int size pretty_int curr_off
     (\* pretty_int (curr_off +~ (get_max node)) *\)
     V.pretty v  V.pretty acc V.pretty r; *)
   !inform, r
 ;;


 (** Auxiliary function to join 2 trees with merge. The merge on two values
     is done by [merge_v]. Since this function can be [V.widen], the
     left/right order of arguments must be preserved. *)
 let f_aux_merge inform merge_v abs_min abs_max rem1 modu1 v1 rem2 modu2 v2 =
 (*  Format.printf "f_aux_merge: [%a, %a]@.(%a %a %a)@.(%a %a %a)@."
     pretty_int abs_min pretty_int abs_max pretty_int rem1 pretty_int
     modu1 V.pretty v1 pretty_int rem2 pretty_int modu2 V.pretty v2 ; *)
   let joined size v1 v2 = V.anisotropic_cast size (merge_v v1 v2) in
   if (rem1 =~ rem2 && modu1 =~ modu2) || V.is_isotropic v2
   then
     rem1, modu1, joined modu1 v1 v2
   else if V.is_isotropic v1 then
     rem2, modu2, joined modu2 v1 v2
   else
     let topify = Origin.K_Merge in
     let conflate_bottom = false in
     let offset = abs_min in
     let size = Integer.length abs_min abs_max in
     let rem = abs_min %~ size in
     let this_inform1, v1' =
       if modu1 =~ size && ((rem1 %~ size) =~ rem)
       then false, v1
       else extract_bits_and_stitch ~topify ~conflate_bottom
         ~offset ~size offset (rem1, modu1, v1) abs_max V.merge_neutral_element
     in
     let this_inform2, v2' =
       if modu2 =~ size && ((rem2 %~ size) =~ rem)
       then false, v2
       else extract_bits_and_stitch ~topify ~conflate_bottom
         ~offset ~size offset (rem2, modu2, v2) abs_max V.merge_neutral_element
     in
     inform := !inform || this_inform1 || this_inform2;
(*     Format.printf "1: (%a, %a, %a);@.2: (%a, %a, %a);@.[%a--%a] -> %a/%a@."
       pretty_int rem1 pretty_int modu1 V.pretty v1
       pretty_int rem2 pretty_int modu2 V.pretty v2
       pretty_int abs_min pretty_int abs_max
       V.pretty v1' V.pretty v2'; *)
     rem, size, merge_v v1' v2'
 ;;

 module JoinCache = Binary_cache.Make_Symmetric(Cacheable)(Cacheable)
 let () = clear_caches := JoinCache.clear :: !clear_caches;;

 (** Joining two trees that cover the same range *)
 let join t1 t2 =
   let inform = ref false in
   let f_join = f_aux_merge inform V.join in
   let rec aux_cache t1 t2 =
     if Cacheable.equal t1 t2 then t1
     else JoinCache.merge (merge aux_cache f_join) t1 t2
   in
   let _, r = aux_cache (Integer.zero, t1) (Integer.zero, t2) in
(*   if !inform then
     Kernel.result ~current:true ~once:true
       "Loss of precision during join operation"; *)
   r
 ;;

 let widen wh t1 t2 =
   let inform = ref false in
   (* Due to the way f_aux_merge is designed, we can obtain intervals on which
      the two bindings do not verify [is_included v1 v2]. The widening
      operations require this, so we correct the arguments here. *)
   let widen v1 v2 =
     let v2 = if not (V.is_included v1 v2) then V.join v1 v2 else v2 in
     V.widen wh v1 v2
   in
   let f_widen = f_aux_merge inform widen in
   let rec aux t1 t2 =
     if Cacheable.equal t1 t2 then t1
     else merge aux f_widen t1 t2
   in
   let _, r = aux (Integer.zero, t1) (Integer.zero, t2) in
(* if !inform then
     Kernel.result ~current:true ~once:true
       "Loss of precision during widening"; *)
   r
 ;;


 (* Given an integer i,
    find the interval the ith bit belongs to (thus its node)
    Returns: the zipper to navigate from the root to the node found,
    and the node itself
 *)
 exception Bit_Not_found (* TODO: not clear it does not leak outside *)
 let find_bit_offset i zipper offset tree =
   let rec aux_find tree curr_off z =
     match tree with
       | Empty -> raise Bit_Not_found
       | Node (max, offl, subl, offr, subr, _, _modu, _v, _) ->
         let abs_max = curr_off +~ max in
         if (i >=~ curr_off) && (i <=~ abs_max)
         then (z, curr_off, tree)
         else if i <~ curr_off
         then
           aux_find subl (curr_off +~ offl) (Left(curr_off, tree, z))
         else begin
           assert (i >~ abs_max);
           aux_find subr (curr_off +~ offr) (Right(curr_off, tree, z))
         end
   in
   aux_find tree offset zipper
 ;;

 let find_bit i tree = find_bit_offset i End Integer.zero tree
 ;;


 (* First and last bits are included in the interval. The returned value
    is at the very least isotropic, possibly topified. *)
 let find_imprecise_between (first_bit, last_bit) tree =
   let rec aux tree_offset tree =
     match tree with
     | Empty -> V.bottom
     | Node (max, offl, subl, offr, subr, _rrel, _m, v, _) ->
       let abs_max = max +~ tree_offset in
       let subl_value =
         if first_bit <~ tree_offset then
           let subl_abs_offset = tree_offset +~ offl in
           aux subl_abs_offset subl
         else V.bottom
       in
       let subr_value =
         if last_bit >~ abs_max then
           let subr_abs_offset = tree_offset +~ offr in
           aux subr_abs_offset subr
         else V.bottom
       in
       let current_node_value =
         if last_bit <~ tree_offset || first_bit >~ abs_max
         then V.bottom
         else
           if V.is_isotropic v
           then v
           else
             let origin = Origin.(current K_Misalign_read) in
             V.topify_with_origin origin v
       in
       V.join subl_value (V.join subr_value current_node_value)
   in
   aux Integer.zero tree

(* Query the offsetmap for the interval [start, start + size - 1], which is
   supposed to fit in the offsetmap. Assumes the offsetmap is rooted at
   offset 0 *)
 let find_itv ~topify ~conflate_bottom ~start ~size tree period_read_ahead =
   let z, cur_off, root = find_bit start tree in
   match root with
     | Empty ->
           (* Bit_Not_found has been raised by find_bit in this case *)
         assert false
     | Node (max, _, _, _, _subr, rrel, m, v, _) ->
         let r = (Rel.add_abs cur_off rrel) %~ m in
         let isize = pred (start +~ size) in
         let nsize = cur_off +~ max in
         let isotropic = V.is_isotropic v in
         if isize <=~ nsize && (isotropic || (m =~ size && start %~ m =~ r))
         then begin
             let read_ahead =
               if isotropic || (Integer.is_zero (period_read_ahead %~ m))
               then Some nsize
               else None
             in
             false, read_ahead, v
           end
         else
           let inform = ref false in
           let acc = ref V.merge_neutral_element in
           let impz = { node = root; offset = cur_off; zipper = z; } in
           while impz.offset <=~ isize do
             let this_inform, v =
               extract_bits_and_stitch ~topify ~conflate_bottom
                 ~offset:start ~size
                 impz.offset (get_vv impz.node impz.offset) (get_max impz.node)
                 !acc
             in
             inform := !inform || this_inform;
             acc := v;
             if impz.offset +~ (get_max impz.node) >=~ isize
             then impz.offset <- succ isize (* end the loop *)
             else
               (* Nominal behavior: do next binding *)
               imp_move_right impz
           done;
           !inform, None, !acc
 ;;

 (*  Finds the value associated to some offsets represented as an ival. *)
 let find ~with_alarms ~validity ~conflate_bottom ~offsets ~size tree  =
    let inform = ref false in
    let filtered_by_bound =
      Tr_offset.filter_by_bound_for_reading ~with_alarms offsets size validity
    in
    let r = try
      match filtered_by_bound with
       | Tr_offset.Interval(mn, mx, m) ->
           let r = mn %~ m in
           let mn = ref mn in
           let acc = ref V.bottom in
           let pred_size = pred size in
           while !mn <=~ mx do
             let this_inform, read_ahead, v =
               find_itv ~topify:Origin.K_Misalign_read ~conflate_bottom
                 ~start:!mn ~size tree m
             in
             inform := !inform || this_inform;
             acc := V.join v !acc;
             let naive_next = !mn +~ m in
             mn :=
               match read_ahead with
                 None -> naive_next
               | Some read_ahead ->
                   let max = read_ahead -~ pred_size in
                   let aligned_b = Integer.round_down_to_r ~max ~r ~modu:m in
                   Integer.max naive_next aligned_b
           done;
           !acc
       | Tr_offset.Set s ->
           List.fold_left
             (fun acc offset ->
                let this_inform, _, new_value =
                  find_itv ~topify:Origin.K_Misalign_read ~conflate_bottom
                    ~start:offset ~size tree Integer.zero
                in
                inform := !inform || this_inform;
                let result = V.join acc new_value in
                if V.equal result V.top then raise Not_found;
                result)
             V.bottom s
       | Tr_offset.Imprecise(mn, mx) ->
           find_imprecise_between (mn, mx) tree
       | Tr_offset.Invalid -> V.bottom
    with Bit_Not_found -> V.top
    in
    if !inform then begin
      let w = with_alarms.CilE.imprecision_tracing in
      Extlib.may
	(fun _ -> Kernel.warning ~current:true ~once:true
	  "extracting bits of a pointer")
	w.CilE.a_log;
      w.CilE.a_call ()
    end;
    r
 ;;

 (* Keep the part of the tree under a given limit offset. *)

 let rec keep_below offset curr_off tree =
   match tree with
     | Empty -> offset, empty
     | Node (max, offl, subl, offr, subr, rrel, m, v, _) ->
       let new_offl = offl +~ curr_off in
       if offset <~ curr_off then
         keep_below offset new_offl subl
       else if offset =~ curr_off then
         new_offl, subl
       else
         let sup = curr_off +~ max in
         if offset >~ sup then
           let new_offr, new_subr = keep_below offset (curr_off +~ offr) subr in
           curr_off,
           nNode max offl subl (new_offr -~ curr_off) new_subr rrel m v
         else
           let new_max = pred (offset -~ curr_off) in
           add_node
             curr_off (new_max +~ curr_off)
             ((Rel.add_abs curr_off rrel) %~ m) m v
             (curr_off +~ offl ) subl
 ;;

 let rec keep_above offset curr_off tree =
   match tree with
     | Empty -> (succ offset), empty
     | Node (max, offl, subl, offr, subr, rrel, m, v, _) ->
        let new_offr = offr +~ curr_off in
        let abs_max = curr_off +~ max in
        if offset >~ abs_max then
          (* This node should be forgotten,
             let's look at the right subtree
          *)
          keep_above offset new_offr subr
        else if offset =~ abs_max then
          (* we are at the limit,
             the right subtree is the answer
          *)
          new_offr, subr
        else
          if offset <~ curr_off then
            (* we want to keep this node and part of its left subtree *)
            let new_offl, new_subl =
              keep_above offset (curr_off +~ offl) subl
            in
            curr_off,
            nNode max (new_offl -~ curr_off) new_subl offr subr rrel m v
          else
            (* the cut happens somewhere in this node it should be cut
               accordingly and reinjected into its right subtree *)
            let new_reml = (Rel.add_abs curr_off rrel) %~ m in
            add_node (succ offset) abs_max new_reml m v new_offr subr
;;

let update_itv_with_rem ~exact ~offset ~abs_max ~size ~rem curr_off v tree =
  let off1, t1 = keep_above abs_max curr_off tree in
  let off2, t2 = keep_below offset curr_off tree in
  let rabs = (Rel.add_abs offset rem) %~ size in
  if exact then
     let off_add, t_add = add_node offset abs_max rabs size v off1 t1 in
     union off2 t2 off_add t_add
  else
   let v_is_isotropic = V.is_isotropic v in
   let z, o, t = find_bit_offset offset End curr_off tree in
   let left_tree = ref t2 in
   let left_offset = ref off2 in
   let impz = { node = t; offset = o; zipper = z; } in
   while impz.offset <=~ abs_max do
     match impz.node with
       | Empty -> assert false
       | Node (max, _offl, _subl, _offr, _subr, rrel, m_node, v_node, _) ->
         let rabs_node = (Rel.add_abs impz.offset rrel) %~ m_node in
         let new_r, new_m, new_v =
           if V.is_isotropic v_node || v_is_isotropic  ||
             (rabs =~ rabs_node && m_node =~ size)
           then
             let new_r, new_m =
               if v_is_isotropic
               then rabs_node, m_node
               else rabs, size
             in
             let cast_v =
               V.anisotropic_cast ~size:new_m (V.join v_node v)
             in
             new_r, new_m, cast_v

           else
             let origin = Origin.(current K_Merge) in
             let new_value = V.topify_with_origin origin (V.join v_node v) in
             let new_rem = Integer.zero and new_modu = Integer.one in
             new_rem, new_modu, new_value
         in
         let node_abs_max = impz.offset +~ max in
         let end_reached, write_max =
           if node_abs_max >=~ abs_max
           then true, abs_max
           else false, node_abs_max
         in
         let new_left_offset, new_left_tree =
           add_node
             (Integer.max impz.offset offset)
             write_max
             new_r new_m new_v !left_offset !left_tree in
         left_tree := new_left_tree;
         left_offset := new_left_offset;

         if not end_reached then imp_move_right impz
         else impz.offset <- succ abs_max
   done;
   union !left_offset !left_tree off1 t1
 ;;

 let update_itv = update_itv_with_rem ~rem:Rel.zero;;

 (* This function does a weak update of the entire [offsm], by adding the
    topification of [v]. The parameter [validity] is respected, and so is the
    current size of [offsm]: each interval already present in [offsm] and valid
    is overwritten. Interval already present but not valid are bound to
    [V.bottom]. *)
 let update_imprecise_everywhere ~validity o v offsm =
   if is_empty offsm then (
     assert (validity = Base.Invalid);
     raise Result_is_bottom
   );
   let v = V.topify_with_origin o v in
   let clip_min, clip_max = match validity with
     | Base.Invalid -> raise Result_is_bottom
     | Base.Known (min, max)
     | Base.Unknown (min, _, max) ->
         (fun min' -> Integer.max min min'),
         (fun max' -> Integer.min max max')
     | Base.Periodic (_, _, p) ->
         let min = Integer.zero and max = pred p in
         (fun min' -> Integer.max min min'),
         (fun max' -> Integer.min max max')
   in
   fold
     (fun (min, max) (bound_v, _, _) acc ->
        let new_v = V.join (V.topify_with_origin o bound_v) v in
        let new_min = clip_min min and new_max = clip_max max in
        let acc =
          if min <~ new_min (* Before validity *)
          then add_basic_node ~min ~max:(pred new_min) ~v:V.bottom acc
          else acc
        in
        let acc =
          if new_min <=~ new_max
          then add_basic_node ~min:new_min ~max:new_max ~v:new_v acc
          else acc (* Interval completely out of validity *)
        in
        let acc =
          if new_max <~ max (* After validity *)
          then add_basic_node ~min:(succ new_max) ~max ~v:V.bottom acc
          else acc
        in acc
     ) offsm empty
 ;;


 (** Update a set of intervals in a given rangemap all offsets starting from
     mn ending in mx must be updated with value v, every period *)
 let update_itvs ~exact ~mn ~mx ~period ~size v tree =
   assert(mx >=~ mn);
   let r = mn %~ period in
   let rec aux_update mn mx curr_off tree =
     match tree with
       | Empty -> curr_off, empty
       | Node (max, offl, subl, offr, subr, r_node, m_node, v_node, _) ->
           let abs_offl = offl +~ curr_off in
           let abs_offr = offr +~ curr_off in

           let new_offl, new_subl, undone_left =
             let last_read_max_offset = curr_off -~ size in
             if pred (mn +~ size) <~ curr_off then
               let new_mx = Integer.round_down_to_r
                 ~max:last_read_max_offset ~r ~modu:period
               in
               let new_mx, undone =
                 if new_mx >=~ mx
                 then mx, None
                 else new_mx, Some (new_mx +~ period)
               in
               let o, t = aux_update mn new_mx abs_offl subl in
               o, t, undone
             else abs_offl, subl, Some mn

           and new_offr, new_subr, undone_right =
             let abs_max = curr_off +~ max in
             let first_read_min_offset = succ abs_max in
             if mx >~ abs_max then
               let new_mn = Integer.round_up_to_r
                 ~min:first_read_min_offset ~r ~modu:period
               in
               let new_mn, undone =
                 if new_mn <=~ mn
                 then mn, None
                 else new_mn, Some (new_mn -~ period)
               in
               let o, t = aux_update new_mn mx abs_offr subr in
               o, t, undone
             else abs_offr, subr, Some mx

           in
           let o, t =
             add_node 
	       curr_off
               (curr_off +~ max)
               ((Rel.add_abs curr_off r_node) %~ m_node)
               m_node v_node new_offl new_subl 
	   in
           let curr_off, tree = union o t new_offr new_subr in
           match undone_left, undone_right with
             | Some min, Some max ->
                 begin
                   let update = update_itv ~exact in
                   if size =~ period
                   then
                     let abs_max = pred (size +~ max) in
                     update ~offset:min ~abs_max ~size curr_off v tree
                   else
                     let offset = ref min in
                     let o = ref curr_off in
                     let t = ref tree in
                     while !offset <=~ max do
                       let abs_max = pred (size +~ !offset) in
                       let o', t' =
                         update ~offset:!offset ~abs_max ~size !o v !t
                       in
                       o := o';
                       t := t';
                   offset := !offset +~ period;
                 done;
                 !o, !t;
               end
             | Some _, None
             | None, Some _
             | None, None -> curr_off, tree
   in
   snd (aux_update mn mx Integer.zero tree)
 ;;


 (* Same speficication as above, except that if too many writes are required,
    the result is automatically approximated *)
 let update_itvs_or_approx ~exact ~mn ~mx ~period ~size v m =
   let number = succ ((mx -~ mn) /~ period) in
   let plevel = !Lattice_Interval_Set.plevel in
   if number <=~ (Integer.of_int plevel) && (period >=~ size) then
     update_itvs ~exact ~mn ~mx ~period ~size v m
   else
     begin
       if size <~ period then
         (* We are going to write the locations that are between [size+1] and
            [period] unnecessarily, warn the user *)
         Kernel.result ~current:true ~once:true
           "more than %d(%a) locations to update in array. Approximating."
           !Lattice_Interval_Set.plevel pretty_int number;
       let abs_max = pred (mx +~ size) in
       snd (update_itv ~exact:false ~offset:mn ~abs_max ~size Integer.zero v m)
     end


let update ~with_alarms ~validity ~exact ~offsets ~size v t =
  let v = V.anisotropic_cast ~size v in
  let exact, reduced = Tr_offset.filter_by_bound_for_writing
    ~with_alarms ~exact offsets size validity
  in
  match reduced with
    | Tr_offset.Imprecise (mn, mx) ->
        let origin = Origin.(current K_Misalign_read) in
        let v = V.topify_with_origin origin v in
        snd (update_itv ~exact:false ~offset:mn ~abs_max:mx ~size:Integer.one
               Integer.zero v t) (* TODO: check *)

    | Tr_offset.Interval(mn, mx, m) ->
        update_itvs_or_approx exact mn mx m size v t

    | Tr_offset.Set s ->
        List.fold_left
          (fun acc offset ->
             let update = update_itv ~exact in
             let _, r = update ~offset ~size
               ~abs_max:(pred (offset +~ size)) Integer.zero v acc
             in
             r
          ) t s
    | Tr_offset.Invalid  ->
        if exact
        then raise Result_is_bottom
        else t


 let copy_single offset tree size period_read_ahead =
   let z, cur_off, root = find_bit offset tree in
   let cur_copy_offset = ref offset (* diffrent from cur_off, as we may
                                       be in the middle of the node *) in
   let impz = { node = root; offset = cur_off; zipper = z; } in
   let acc = ref empty in
   let iend = pred (offset +~ size) in
   let read_ahead =
     (* See if we can read everything in this node with some read-ahead *)
     let max, modu = get_max root, get_modu root in 
     let next_end = cur_off +~ max in
     if offset >=~ cur_off &&
       iend <~ cur_off +~ max &&
       Integer.is_zero (period_read_ahead %~ modu)
     then Some next_end
     else None
   in
   while
     (match impz.node with
       | Empty ->
           assert false
       | Node (max, _, _, _, _subr, rrel, m, v, _) ->
         let next_end = impz.offset +~ max in
         let nend = Integer.min iend next_end in
         let new_rel_end = nend -~ offset in
         let nbeg = !cur_copy_offset -~ offset in
         let abs_rem =
           (Rel.add_abs nbeg
              (Rel.sub rrel (Rel.sub_abs !cur_copy_offset impz.offset))) %~ m
         in
         let o, t = add_node nbeg new_rel_end abs_rem m v Integer.zero !acc in
         assert (o =~ Integer.zero);
         acc := t;
         let cond = iend >~ next_end in
         if cond then begin
           imp_move_right impz;
           cur_copy_offset := impz.offset;
         end;
         cond)
   do ();
   done;
   read_ahead, !acc
 ;;

 let is_single_interval ?(f=fun _ -> true) o =
   match o with
   | Node(_, _, Empty, _, Empty, _, _, v, _) -> f v
   | _ -> false

 let single_interval_value o =
   match o with
   | Node(_, _, Empty, _, Empty, _, _, v, _) -> Some v
   | _ -> None


 let copy_slice ~with_alarms ~validity ~offsets ~size tree =
    let filtered_by_bound =
      Tr_offset.filter_by_bound_for_reading ~with_alarms offsets size validity
    in
    let init =
      add_basic_node ~min:Integer.zero ~max:(pred size) ~v:V.bottom empty
    in
    let join acc t = if is_empty acc then t else join acc t in
    let result =
      match filtered_by_bound with
       | Tr_offset.Interval(mn, mx, m) ->
          let r = mn %~ m in
          let mn = ref mn in
          let acc_tree = ref init in
          let pred_size = pred size in
          while !mn <=~ mx do
            let read_ahead, new_tree =
              copy_single !mn tree size m
	    in
            acc_tree := join !acc_tree new_tree;
            let naive_next = !mn +~ m in
            mn := match read_ahead with
              | None -> naive_next
              | Some read_ahead ->
                let max = read_ahead -~ pred_size in
                let aligned_b = Integer.round_down_to_r ~max ~r ~modu:m in
                Integer.max naive_next aligned_b
          done;
          !acc_tree
       | Tr_offset.Set s ->
           List.fold_left
             (fun acc_tree offset ->
               let _, t = copy_single offset tree size Integer.zero in
               join acc_tree t
             ) init s
       | Tr_offset.Imprecise(mn, mx) ->
           let v = find_imprecise_between (mn, mx) tree in
           add_basic_node ~min:Integer.zero ~max:(pred size) ~v empty
       | Tr_offset.Invalid ->
           empty
    in
    result
 ;;

 let fold_between ~entire (imin, imax) f t acc =
   let rec aux curr_off t acc = match t with
     | Empty -> acc
     | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
         let abs_max = max +~ curr_off in
         let acc =
           if imin <~ curr_off then (
             aux (offl +~ curr_off) subl acc)
           else acc
         in
         let acc =
           if imax <~ curr_off || imin >~ abs_max
           then acc
           else
             if entire then
               (* Call f on the entire binding *)
               f (curr_off, abs_max) (v, modu, rem) acc
             else
               (* Cut the interval to [imin..imax] *)
               let lmin = Integer.max imin curr_off in
               let lmax = Integer.min imax abs_max in
               let lrem =
                 Rel.pos_rem (Rel.sub rem (Rel.sub_abs lmin curr_off)) modu
               in
               f (lmin, lmax) (v, modu, lrem) acc
         in
         if imax >~ abs_max
         then aux (offr +~ curr_off) subr acc
         else acc
   in
   aux Integer.zero t acc
 ;;

  let paste_slice_itv ~exact from start stop start_dest to_ =
    let update = update_itv_with_rem ~exact in
    let offset = start_dest -~ start in
    let treat_interval (imin, imax) (v, modu, rem) acc =
      let dmin, dmax = imin +~ offset, imax +~ offset in
      snd (update
             ~offset:dmin ~abs_max:dmax ~rem:rem ~size:modu Integer.zero v acc)
    in
    fold_between ~entire:false (start, stop) treat_interval from to_
  ;;


  let paste_slice ~with_alarms ~validity ~exact (src, start_src) ~size ~offsets dst =
    try
      let plevel = !Lattice_Interval_Set.plevel in
      let stop_src = Int.pred (Int.add start_src size) in
      ignore (Ival.cardinal_less_than offsets plevel);
      (* TODO: this should be improved if offsets if of the form [a..b]c%d
         with d >= size. In this case, the write do not overlap, and
         could be done in one run in the offsetmap itself *)
      let aux start_to (acc, success) =
        let stop_to = Int.pred (Int.add start_to size) in
        match validity with
          | Base.Invalid ->
              CilE.warn_mem_write with_alarms;
              acc, success
          | Base.Periodic (b, e, _)
          | Base.Known (b,e)
          | Base.Unknown (b,_,e) when Int.lt start_to b || Int.gt stop_to e ->
              CilE.warn_mem_write with_alarms;
              acc, success

          | Base.Known _ | Base.Unknown _ ->
              paste_slice_itv ~exact src start_src stop_src start_to acc,
              true

          | Base.Periodic (b, _e, period) ->
              assert (Int.equal b Int.zero) (* assumed in module Base *);
              let start_to = Int.rem start_to period in
              let stop_to = Int.pred (Int.add start_to size) in
              if Int.gt stop_to period then
                Kernel.not_yet_implemented "Paste of overly long \
                              values in periodic offsetmaps" (* TODO *);
              paste_slice_itv ~exact:false src start_src stop_src start_to acc,
              true
      in
      let res, success = Ival.fold_int aux offsets (dst, false) in
      if success then res else raise Result_is_bottom
    with Not_less_than ->
      Kernel.result ~current:true ~once:true
        "too many locations to update in array. Approximating.";
      (* Value to paste, since we cannot be precise *)
      let validity_src = Base.Known (start_src, Int.pred (start_src +~ size)) in
      let v = find ~with_alarms:CilE.warn_none_mode
        ~validity:validity_src ~conflate_bottom:false
        ~offsets:(Ival.inject_singleton start_src) ~size src
      in
      update ~with_alarms ~validity ~exact ~offsets ~size v dst


  let pretty_typ typ fmt m =
    let inset_utf8 = Unicode.inset_string () in
    let is_first = ref true in
    let pretty_binding fmt (bk, ek) (v, modu, rel_offs) =
      if not (V.equal v V.bottom) then begin (* TODOBY: temporary *)
      if !is_first then is_first:=false
      else Format.fprintf fmt "@\n";
      Format.fprintf fmt "@[" ;
      (* Print left-member and return misalign condition *)
      let force_misalign, _printed_type =
        match typ with
          | None ->
              Format.fprintf fmt "[rbits %a to %a]"
                pretty_int bk pretty_int ek ;
              (* misalign condition: *)
              (not (Rel.is_zero rel_offs) || (ek -~ bk <>~ pred modu))
              && not (V.is_isotropic v),
              None

          | Some typ ->
              (* returns misalign condition. *)
              Bit_utils.pretty_bits typ
                ~use_align:(not (V.is_isotropic v))
                ~align:rel_offs ~rh_size:modu ~start:bk ~stop:ek fmt
      in
      Format.fprintf fmt " %s@ @[<hv 1>%a@]" inset_utf8 V.pretty v ;
      if force_misalign
      then
        if Rel.is_zero rel_offs && (Int.length bk ek) %~ modu =~ Integer.zero
        then
          (if Int.length bk ek >~ modu then
             Format.fprintf fmt " repeated %%%a " pretty_int modu)
        else (
          let b_bits = Rel.pos_rem (Rel.sub Rel.zero rel_offs) modu  in
          let e_bits = Rel.add_abs (ek -~ bk) b_bits in
          Format.fprintf fmt "%s%%%a, bits %a to %a "
            (if e_bits >~ modu then " repeated " else "")
            pretty_int modu Rel.pretty b_bits pretty_int e_bits
        );
      Format.fprintf fmt "@]";
      end
    in
    if is_empty m then
      Format.fprintf fmt "@[[?] %s ANYTHING@]" inset_utf8
    else
    Format.fprintf fmt "@[%a@]"
      (fun fmt -> iter (pretty_binding fmt)) m

  let create_isotropic ~size v =
    assert (Int.gt size Int.zero);
    assert (V.is_isotropic v);
    add_basic_node ~min:Integer.zero ~max:(pred size) ~v empty

  let create ~size v ~size_v =
    assert (Int.gt size Int.zero);
    add_node_from_root ~min:Integer.zero ~max:(pred size) ~rem:Integer.zero
      ~modu:size_v ~v empty

  let cardinal_zero_or_one offsetmap =
    (singleton_tag offsetmap) <> 0

  let of_list fold l size_elt =
    let s = pred size_elt in
    let n = ref Integer.zero in
    let addw acc v =
      let e = !n +~ s in
      let r = add_basic_node ~min:!n ~max:e ~v acc in
      n := succ e;
      r
    in
    fold addw empty l

  let add (min, max) (v, modu, rem) m =
    snd (update_itv_with_rem ~exact:true
           ~offset:min ~abs_max:max ~rem ~size:modu Integer.zero v m)

  let find_imprecise ~validity m =
    match validity with
    | Base.Known (min, max) | Base.Unknown (min, _, max) ->
        find_imprecise_between (min, max) m
    | Base.Periodic (_min, _max, p) ->
        find_imprecise_between (Int.zero, pred p) m
    | Base.Invalid -> V.bottom

  let find_imprecise_everywhere m =
    match m with
    | Empty -> V.bottom
    | Node _ ->
      let bounds = bounds_offset Int.zero m in
      find_imprecise_between bounds m


  let clear_caches () = List.iter (fun f -> f ()) !clear_caches
end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
