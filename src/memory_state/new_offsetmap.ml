(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module Bint = Abstract_interp.Int;;

(*
  module Int for use with these offsetmaps is basically int64
*)
module Int = struct

  include Int64

  let pretty = Datatype.Int64.pretty
  let hash = Datatype.Int64.hash
  let equal = Datatype.Int64.equal

  let sentinel = zero;;
  let is_zero x = equal x zero
  let to_big_int = Abstract_interp.Int.of_int64
  let from_big_int = Abstract_interp.Int.to_int64

  let ge x y = x >= y
  let gt x y = x > y
  let le x y = x <= y
  let lt x y = x < y

  let max = Pervasives.max
  let min = Pervasives.min
  let length lb ub = succ (sub ub lb)

  let pos_div x y =
    let q = div x y in
    if (rem x y) >= zero
    then q
    else if y < zero
    then succ q
    else pred q

  let pos_rem x y =
    let r = rem x y in if r < zero then add r y else r

  let round_down_to_zero v modu =
    mul (pos_div v modu) modu

  (** [round_up_to_r m r modu] is the smallest number [n] such that
          [n]>=[m] and [n] = [r] modulo [modu] *)
   let round_up_to_r ~min ~r ~modu =
     add (add (round_down_to_zero (pred (sub min r)) modu) r) modu

   (** [round_down_to_r m r modu] is the largest number [n] such that
      [n]<=[m] and [n] = [r] modulo [modu] *)
   let round_down_to_r ~max:m ~r ~modu =
     add (round_down_to_zero (sub m r) modu) r

end

(** Implementation of "rangemaps" *)

module Make (V : Lattice_With_Isotropy.S) = struct

  open Format

  (* module Old = Offsetmap.Build(V) (* Uncomment for testing purposes *)*)

  type y = V.t
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
        Int.t *
          Int.t * tt *
          Int.t * tt *
          Int.t * Int.t * V.t *
          int

  let equal t1 t2 = t1 == t2

  let pretty_node ppf min max r m v  =
   Format.fprintf ppf "@[@[[%a..%a] -> (%a, %a, %a);@]@;@ @]"
     Int.pretty min
     Int.pretty max
     Int.pretty r
     Int.pretty m
     V.pretty v ;
   flush_all ();
 ;;

 let rec pretty_offset s curr_off ppf tree =
   match tree with
   | Empty -> ()
   | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
       pretty_offset "" (Int.add curr_off offl) ppf subl;
       Format.fprintf ppf "@[%s: [%a..%a] -> (%a, %a, %a);@]@;@ "
         s
         Int.pretty curr_off
         Int.pretty (Int.add max curr_off)
         Int.pretty (Int.pos_rem (Int.add rem curr_off) modu)
         Int.pretty modu
         V.pretty v;
       pretty_offset "" (Int.add curr_off offr) ppf subr;
 ;;

 let pretty ppf = pretty_offset "0" Int.zero  ppf ;;

 include
  (struct

    let hash t =
      match t with
        Empty -> 311
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
          Structure
            (Sum
               [| [| p_int64;
                     p_int64;
                     recursive_pack r;
                     p_int64;
                     recursive_pack r;
                     p_int64;
                     p_int64;
                     V.packed_descr;
                     p_int |] |])
        let () = Recursive.update r structural_descr
        let equal = equal
        let hash = hash
        let compare = Datatype.undefined
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
    let is_empty t = t = Empty

    let equal_internal t1 t2 =
      match t1, t2 with
      | Empty, Empty -> true
      | Node _, Empty | Empty, Node _ -> false
      | Node (max1, offl1, subl1, offr1, subr1, rem1, modu1, v1, _),
          Node (max2, offl2, subl2, offr2, subr2, rem2, modu2, v2, _)
          ->
          Int.equal rem1 rem2 &&
            Int.equal modu1 modu2 &&
            Int.equal max1 max2 &&
            Int.equal offl1 offl2 &&
            Int.equal offr1 offr2 &&
            V.equal v1 v2 &&
            subl1 == subl2 &&
            subr1 == subr2

    let hash_internal t =
      match t with
        Empty -> 97
      | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
          let h = Int.hash max in
          let h = 31 * h + Int.hash offl in
          let h = 31 * h + hash subl in
          let h = 31 * h + Int.hash offr in
          let h = 31 * h + hash subr in
          let h = 31 * h + Int.hash rem in
          let h = 31 * h + Int.hash modu in
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
          let kind = `Internal
        end)

    let counter = ref 0

    let nNode (a,b,c,d,e,f,g,h) =
      let tentative_new_node = Node(a,b,c,d,e,f,g,h,!counter) in
      let hashed_node = NewoHashconsTbl.merge tentative_new_node in
      if hashed_node != tentative_new_node
      then incr counter;
      hashed_node

    let rehash_node x = match x with
      | Empty -> empty
      | Node _ -> 
	  NewoHashconsTbl.merge x

    let () = rehash_ref := rehash_node

  end :
    sig
      type t = tt
      val empty : t
      val hash: t -> int
      val nNode : Int.t * Int.t * t * Int.t * t * Int.t * Int.t * V.t -> t
      val is_empty : t -> bool
    end)

  let equal_vv (rem1, modu1, v1) (rem2, modu2, v2) =
      Int.equal rem1 rem2 &&
        Int.equal modu1 modu2 &&
        V.equal v1 v2
    ;;

  let get_vv node curr_off =
    match node with
    | Empty -> assert false
    | Node (_, _, _, _, _, remrel, modu, v, _) ->
        let rem = Int.pos_rem (Int.add remrel curr_off) modu in
        rem, modu, v
  ;;

  let get_v = function
    | Empty -> assert false
    | Node (_, _, _, _, _, _, _, v, _) ->
        v
  ;;

  let get_max = function
    | Empty -> assert false
    | Node (max, _, _, _, _, _, _, _, _) ->
        max
  ;;

  let is_above min1 max1 min2 max2 =
    let signature_interval min max =
      Int.logand (Int.logxor (Int.pred min) max) Int.max_int in
    signature_interval min1 max1 > signature_interval min2 max2
  ;;


  type zipper =
    | End
    | Right of Int.t * t * zipper
    | Left of Int.t * t * zipper;;
  (** Zippers : Offset of a node * Node * continuation of the zipper *)

  exception End_reached;;
  exception Empty_tree;;

  let pr_zipper ppf z  =
    printf "[Zipper]---@.";
    let rec aux ppf = function
      | End -> printf "@ E@."
      | Right (o, Node(max, _, _, _, _subr, _, _, _, _),z ) ->
          fprintf ppf "@[<h 2> [%a,%a] R@\n%a@]"
            Int.pretty o
            Int.pretty (Int.add o max)
            aux z
      | Left (o, Node(max, _, _, _, _subr, _, _, _, _),z ) ->
          fprintf ppf "@[<h 2> [%a,%a] L@\n%a@]"
            Int.pretty o
            Int.pretty (Int.add o max)
            aux z
      |  Right (_, Empty, _) | Left (_, Empty, _) -> assert false
    in aux ppf z;
    printf "[/Zipper]---@.@.";
  ;;

 let pout_zipper = pr_zipper std_formatter;;

 (** Returns an absolute position and an associated new tree *)
 let rec rezip zipper curr_off node =
   match zipper with
   | End -> curr_off, node
   | Right (offset, Node(max, offl, subl, _offr, _subr, rem, modu, v, _), z)
     ->
       rezip z offset
         (nNode (max, offl, subl, Int.sub curr_off offset, node, rem, modu, v))
   | Left (offset, Node(max, _offl, _subl, offr, subr, rem, modu, v, _), z)
     ->
       rezip z offset
         (nNode (max, Int.sub curr_off offset, node, offr, subr, rem, modu, v))
   | Right (_, Empty, _) | Left (_, Empty, _) -> assert false
 ;;

 (** Returns an absolute position, a node and a zipper *)
 let rec leftmost_child curr_off zipper node =
   match node with
   | Empty -> raise Empty_tree
   | Node (_, _, Empty, _, _, _, _, _, _) -> curr_off, node, zipper
   | Node (_, offl, subl, _, _, _, _, _, _) ->
       let new_offset = Int.add curr_off offl in
       leftmost_child new_offset (Left (curr_off, node, zipper)) subl
 ;;

 (** Returns an absolute position, a node and a zipper *)
 let rec rightmost_child curr_off zipper node =
   match node with
   | Empty -> raise Empty_tree
   | Node (_, _, _, _, Empty, _, _, _, _) -> curr_off, node, zipper
   | Node (_, _offl, _subl, offr, subr, _, _, _, _) ->
       let new_offset = Int.add curr_off offr in
       rightmost_child new_offset (Right (curr_off, node, zipper)) subr
 ;;

 (** Move to the right of the current node.
     Uses a zipper for that.
  *)
 let rec move_right curr_off node zipper =
   match node with
   | Node (_, _, _, offr, ((Node _ ) as subr), _, _, _, _) ->
       let new_offset = Int.add curr_off offr in
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
   mutable offset: Int.t;
   mutable node: t;
   mutable zipper: zipper;
 };;

 let imp_move_right imp_z =
   let o, n, z = move_right imp_z.offset imp_z.node imp_z.zipper in
   imp_z.offset <- o;
   imp_z.node <- n;
   imp_z.zipper <- z;
 ;;

 (** Folding and iterating from the leftmost node to the rightmost one
     If t =  n0         fold f t i = f n2 (f n0 (f n1 i))
            / \         iter f t   = f n1; fn0; f n2;
           n1  n2
  *)
 let fold_offset f o t =
  assert (not (is_empty t));
  let o, n, z = leftmost_child o End t in
  let rec aux_fold o t z pre =
    match t with
    | Empty -> assert false
    | Node (max, _, _, _, _, r, m, v, _) ->
        let abs_max = Int.add max o in
        let now = f o abs_max r m v pre in
        try
          let  no, nt,  nz = move_right o t z in
          aux_fold no nt nz now
        with End_reached -> now
  in aux_fold o n z
 ;;

 let fold f t = fold_offset f Int.zero t
  ;;

 let iter_offset f o t =
  assert (not (is_empty t));
  let o, n, z = leftmost_child o End t in
  let rec aux_iter o t z =
     match t with
    | Empty -> assert false
    | Node (max, _, _, _, _, r, m, v, _) ->
        begin
          let abs_max = Int.add max o in
          let abs_r = Int.pos_rem (Int.add r o) m in
          f o abs_max abs_r m v;
          try
            let no, nt, nz = move_right o t z  in
            aux_iter no nt nz
          with End_reached -> ()
        end
  in aux_iter o n z
 ;;

 let iter f t = iter_offset f Int.zero t
  ;;


 (** Pretty printing *)

 let pretty_node ppf min max r m v  =
   Format.fprintf ppf "@[@[[%a..%a] -> (%a, %a, %a);@]@;@ @]"
     Int.pretty min
     Int.pretty max
     Int.pretty r
     Int.pretty m
     V.pretty v ;
   flush_all ();
 ;;

 let pretty_offset s curr_off ppf tree =
   if is_empty tree then Format.fprintf ppf "@[empty at %Ld@]" curr_off
   else
     let rec pretty_offset s curr_off ppf tree =
       match tree with
         | Empty -> ()
         | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
           pretty_offset "" (Int.add curr_off offl) ppf subl;
           Format.fprintf ppf "@[%s: [%a..%a] -> (%a, %a, %a);@]@;@ "
             s
             Int.pretty curr_off
             Int.pretty (Int.add max curr_off)
             Int.pretty (Int.pos_rem (Int.add rem curr_off) modu)
             Int.pretty modu
             V.pretty v;
           pretty_offset "" (Int.add curr_off offr) ppf subr;
     in pretty_offset s curr_off ppf tree
 ;;


 let pretty ppf = pretty_offset "0" Int.zero  ppf
  ;;

 let pretty_offset = pretty_offset "0";;

 let pretty_debug_offset curr_off ppf  tree =
   let rec aux_pdebug curr_off ppf  tree =
     match tree with
     | Empty -> Format.fprintf ppf "empty"
     | Node (max, offl, subl, offr, subr, rem, modu, v, _) ->
         Format.fprintf ppf "@[<h 2>@[[%a..%a]@ %a@ %a@ %a@]@\n@[<h 2>--\
                             %a -->@\n%a@]@\n@[<h 2>-- %a -->@\n%a@]@]"
           Int.pretty curr_off
           Int.pretty (Int.add curr_off max)
           Int.pretty (Int.pos_rem (Int.add rem curr_off) modu)
           Int.pretty modu
           V.pretty v
           Int.pretty offl
           (aux_pdebug (Int.add curr_off offl))  subl
           Int.pretty offr
           (aux_pdebug (Int.add curr_off offr))  subr
    in
      aux_pdebug curr_off  ppf tree;
      Format.fprintf ppf "@\n";
 ;;

 let pretty_debug ppf = pretty_debug_offset Int.zero ppf ;;

 let print_offset o t = pretty_debug_offset o Format.std_formatter t ;;

 let fprint ppf t =
  iter (fun min max r m v -> pretty_node ppf  min max r m v) t;
  Format.fprintf ppf "@.";
 ;;

 let print t = let ppf = Format.std_formatter in fprint ppf t ;;

 (** Given interval [min, max], returns the subtree starting at this
     interval.
     Raises Interval_not_found (min, max) when failing. *)
 exception Interval_not_found of Int.t * Int.t;;

 let subtree_from_interval min max tree_offset tree =
   let rec aux_sfi tree_offset tree =
     match tree with
     | Empty -> raise (Interval_not_found(min, max))
     | Node (nmax, offl, subl, offr, subr, _, _, _, _) ->
         let abs_min = tree_offset
         and abs_max = Int.add tree_offset nmax in
         if Int.equal min abs_min && Int.equal max abs_max
         then
           tree_offset, tree
         else
           if max > abs_max
           then
             aux_sfi (Int.add tree_offset offr) subr
           else if min < abs_min
           then
             aux_sfi (Int.add tree_offset offl) subl
           else raise (Interval_not_found(min, max))
   in aux_sfi tree_offset tree
 ;;


 (** Smart constructor for nodes:
     it glues the node being allocated to potential candidates if needed
     (i.e. leftmost node of right subtree and rightmost node of left subtree),
  *)

 let make_node curr_off max offl subl offr subr rem modu v =
   let rem, modu =
     if V.is_isotropic v
     then Int.zero, Int.one
     else rem, modu
   in
   let curr_vv = (rem, modu, v) in

   let max, offr, subr =
     try
       let offset, nr, zr =
         leftmost_child (Int.add curr_off offr) End subr in
       match nr with
       | Node (nmax, _, nsubl , noffr, nsubr, nrelrem, nmodu, nv, _) ->
           assert (is_empty nsubl);
           let nrem = Int.pos_rem (Int.add nrelrem offset) nmodu in
           if equal_vv (nrem, nmodu, nv) curr_vv &&
              (Int.equal (Int.pos_rem offset modu) rem)
           then
             begin
 (*              assert (Int.equal (Int.add offset (succ nmax)) Int.zero);*)
               let curr_offr, new_subr = rezip zr (Int.add offset noffr) nsubr
               in
               let new_max = Int.succ (Int.add max nmax) in
               let new_offr = Int.sub curr_offr curr_off
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
         rightmost_child (Int.add curr_off offl) End subl in
       match nl with
       | Node (nmax, noffl, nsubl , _, noffr, nrelrem, nmodu, nv, _) ->
           assert (is_empty noffr);
           let nrem = Int.pos_rem (Int.add nrelrem offset) nmodu in
           if equal_vv (nrem, nmodu, nv) curr_vv &&
               (Int.equal (Int.rem curr_off modu) rem)
           then (
               let new_curr_offl, new_subl =
                 rezip zl (Int.add offset noffl) nsubl in
               let succ_nmax = Int.succ nmax in
               let lmax = Int.add max succ_nmax in
               let new_offl = Int.sub new_curr_offl offset in
               let new_offr = Int.add offr succ_nmax in
               let new_coff = Int.sub curr_off succ_nmax in
               (*assert (new_coff = offset);*)
               new_coff, lmax, new_offl, new_subl, new_offr)
           else curr_off, max, offl, subl, offr
       |Empty -> assert false
     with Empty_tree -> curr_off, max, offl, subl, offr
   in
   let remrel = Int.pos_rem (Int.sub rem curr_off) modu in
   curr_off, nNode (max, offl, subl, offr, subr, remrel, modu, v)
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
         let sz = Int.sub max min in
         make_node min sz Int.zero empty (Int.succ sz) empty rem modu v
     | Node (nmax, noffl, nsubl, noffr, nsubr, nremrel, nmodu, nv, _) ->
         let nrem = Int.pos_rem (Int.add nremrel curr_off) nmodu in
         let abs_min = curr_off
         and abs_max = Int.add nmax curr_off in
         if max < abs_min then
           begin
             if is_above min max abs_min abs_max then
               let new_offr = Int.sub abs_min min in
               (*Format.printf "add to the left above@."; *)
               make_node min (Int.sub max min) Int.zero empty
                 new_offr tree rem modu v
             else
               begin
                 (*     Format.printf "L@ co:%a@ t:%a@ [%a...%a]@.@."
                        Int.pretty curr_off
                        (pretty_offset curr_off) tree
                        Int.pretty min Int.pretty max
                        ; *)
                 let new_curr_offl, new_node =
                   aux_add (Int.add curr_off noffl) nsubl
                 in
                 let new_offl = Int.sub new_curr_offl curr_off in
                 make_node
                   curr_off nmax new_offl new_node noffr nsubr nrem nmodu nv
               end
           end
         else
           begin
             if is_above min max abs_min abs_max then
               begin
                 (* Format.printf "add to the right ABOVE@.";  *)
                 let new_offl = Int.sub abs_min min in
                 (* Format.printf "1 %a %a@." Int.pretty  (Int.sub max curr_off)
                    Int.pretty new_offl; *)
                 let new_max = Int.sub max min in
                 (* Format.printf "add_node :[%a %a] o:%a t:%a@."
                    Int.pretty min Int.pretty new_max
                    Int.pretty new_offl pretty_debug tree;
                 *)
                 make_node
                   min new_max new_offl tree (Int.succ new_max) empty rem modu v
               end

             else
               begin
                 (*           Format.printf "add to the right Not ABOVE@."; *)
                 let new_curr_offr, new_node =
                   aux_add (Int.add curr_off noffr) nsubr
                 in
                 let new_offr = Int.sub new_curr_offr abs_min in
                 make_node abs_min nmax noffl nsubl new_offr new_node nrem
                   nmodu nv
               end
           end

   in aux_add curr_off tree
 ;;

 (** Translation functions to and fro old offstemaps, *)

 let to_list t =
   List.rev (fold (fun min max r m v y -> (min, max, r, m, v) :: y) t [])

 (** Checks that [tree] is sanely built  *)

 let rec check curr_off tree =
   match tree with
   | Empty -> ()
   | Node (max, offl, subl, offr, subr, rem, modu, _v, _) ->
       assert (Int.compare Int.zero rem <= 0);
       assert (Int.compare rem modu < 0);
       assert (not (is_empty subl) || Int.is_zero offl);
       assert (not (is_empty subr) || Int.equal offr (Int.succ max));
       let abs_min = curr_off
       and abs_max = Int.add curr_off max in
       let aux offset tree =
         match tree with
         | Empty -> ()
         | Node (nmax, _, _, _, _, _, _, _, _) ->
             let nabs_min = Int.add curr_off offset in
             let nabs_max = Int.add nmax nabs_min in
             assert (is_above abs_min abs_max nabs_min nabs_max)
       in aux offl subl; aux offr subr;
       check (Int.add curr_off offl) subl;
       check (Int.add curr_off offr) subr;
 ;;

 (** Inclusion functions *)

 (** Are the values of t1 included in those of t2 ?
     t1 and t2 must cover exactly the same range (see the 2 first assertions).
     The offset is absolute.
  *)

 let nc_is _included_generic_exn v_is_included_exn o1 t1 o2 t2 =
   assert ( o1 = o2);

   (* Is n1 included in n2 ? *)
   let is_similar (r1 : int64) (m1 : Int.t) v1 r2 m2 v2 =
     if (r1 = r2 && m1 = m2) || V.is_isotropic v1 || V.is_isotropic v2
     then
       v_is_included_exn v1 v2
     else raise Abstract_interp.Is_not_included
   in
   let is_included_node_exn _min1 _max1 r1 m1 v1
       _min2 _max2 r2 m2 v2 mabs_min mabs_max =
     if V.is_isotropic v1 || V.is_isotropic v2
     then
       v_is_included_exn v1 v2
     else if r1 =  r2 && m1 = m2 &&
       Int.rem mabs_min m1 = r1 &&
       Int.rem (Int.succ mabs_max) m1 = r1
     then
       v_is_included_exn v1 v2
     else raise Abstract_interp.Is_not_included
   in
   let rec aux_inc ((o1, t1, z1) as n1) ((o2, t2, z2) as n2) =
     if t1 != t2
     then
       match t1, t2 with
       | Empty, _
       | _, Empty -> assert false
       | Node (max1, _offl1, _subl1, _offr1, _subr1, r1rel, m1, v1, _),
           Node (max2, _offl2, _subl2, _offr2, _subr2, r2rel, m2, v2, _) ->
             let abs_min1 = o1 in
             let abs_min2 = o2 in
             let abs_max1 = Int.add o1 max1 in
             let abs_max2 = Int.add o2 max2 in
             let r1 = Int.pos_rem (Int.add r1rel o1) m1 in
             let r2 = Int.pos_rem (Int.add r2rel o2) m2 in
             if abs_min1 = abs_min2
             then
               begin
                 if abs_max1 = abs_max2
                 then
                   begin
                     is_similar r1 m1 v1 r2 m2 v2;
                     aux_inc (move_right o1 t1 z1) (move_right o2 t2 z2);
                   end
                 else if abs_max1 > abs_max2
                 then
                   begin
                     is_included_node_exn
                       abs_min1 abs_max1 r1 m1 v1
                       abs_min2 abs_max2 r2 m2 v2
                       abs_min1 abs_max2;
                     aux_inc n1 (move_right o2 t2 z2);
                   end
                 else
                   begin
                     assert (abs_max1 < abs_max2);
                     is_included_node_exn
                       abs_min1 abs_max1 r1 m1 v1
                       abs_min2 abs_max2 r2 m2 v2
                       abs_min1 abs_max1;
                     aux_inc (move_right o1 t1 z1) n2;
                   end
               end
             else if abs_min1 > abs_min2
             then let mabs_min = abs_min2 in
             if abs_max1 = abs_max2
             then
               begin
                 is_included_node_exn
                   abs_min1 abs_max1 r1 m1 v1
                   abs_min2 abs_max2 r2 m2 v2
                   mabs_min abs_max1;
                 aux_inc (move_right o1 t1 z1) (move_right o2 t2 z2);
               end

             else if abs_max1 > abs_max2
             then
               begin
                 is_included_node_exn
                   abs_min1 abs_max1 r1 m1 v1
                   abs_min2 abs_max2 r2 m2 v2
                   mabs_min abs_max2;
                 aux_inc n1 (move_right o2 t2 z2);
               end
             else
               begin
                 assert (abs_max1 < abs_max2);
                 is_included_node_exn
                   abs_min1 abs_max1 r1 m1 v1
                   abs_min2 abs_max2 r2 m2 v2
                   mabs_min abs_max1;
                 aux_inc (move_right o1 t1 z1) n2;
               end
             else
               begin
                 assert (abs_min1 < abs_min2);
                 let mabs_min = abs_min1 in
                 if abs_max1 = abs_max2
                 then
                   begin
                     is_included_node_exn
                       abs_min1 abs_max1 r1 m1 v1
                       abs_min2 abs_max2 r2 m2 v2
                       mabs_min abs_max1;
                     aux_inc (move_right o1 t1 z1) (move_right o2 t2 z2);
                   end
                 else if abs_max1 > abs_max2
                 then
                   begin
                     is_included_node_exn
                       abs_min1 abs_max1 r1 m1 v1
                       abs_min2 abs_max2 r2 m2 v2
                       mabs_min abs_max2;
                     aux_inc n1 (move_right o2 t2 z2);
                   end
                 else
                   begin
                     assert (abs_max1 < abs_max2);
                     is_included_node_exn
                       abs_min1 abs_max1 r1 m1 v1
                       abs_min2 abs_max2 r2 m2 v2
                       mabs_min abs_max1;
                     aux_inc (move_right o1 t1 z1) n2;
                   end
               end
   in
   let l1 = leftmost_child o1 End t1 in
   let l2 = leftmost_child o2 End t2 in
   aux_inc l1 l2
 ;;

 let is_included_generic_exn v_is_included_exn o1 t1 o2 t2 =

   assert (o1 = o2);
   Format.printf "Start is included@.";
   (* Is n1 included in n2 ? *)
   let is_similar (r1 : int64) (m1: Int.t) v1 r2 m2 v2 =
     if (r1 = r2 && m1 = m2) || V.is_isotropic v1 || V.is_isotropic v2
     then
       v_is_included_exn v1 v2
     else raise Abstract_interp.Is_not_included 
   in 
   let is_included_node_exn (amin1 : int64) (amax1 : int64) r1 m1 v1
       amin2 amax2 r2 m2 v2 mabs_min mabs_max =
     if V.is_isotropic v1 || V.is_isotropic v2 then
       v_is_included_exn v1 v2
     else
       let max_test = if amax1 < amax2
           then Int.rem (Int.succ mabs_max) m1 = r1
           else true
       in
       let ok_min = (amin1 = amin2) || Int.rem mabs_min m1 = r1
       and ok_max = amax1 = amax2 || max_test
       in
       if r1 = r2 && m1 = m2 && ok_min && ok_max
       then
         v_is_included_exn v1 v2
     else raise Abstract_interp.Is_not_included
   in
   let rec node_included  o1 t1 o2 t2 =
     (* Format.printf "*nodeINC @.t1: %a@. t2: %a@."
        (pretty_offset o1) t1 (pretty_offset o2) t2 ;
     *)
     if t1 == t2 then ()
     else
       match t1, t2 with
       | Empty, _ -> ()
       | _, Empty -> ()
       | Node (max1, offl1, subl1, offr1, subr1, r1rel, m1, v1, _),
           Node (max2, offl2, subl2, offr2, subr2, r2rel, m2, v2, _) ->
             let amin1 = o1 in
             let amax1 = Int.add max1 o1 in
             let amin2 = o2 in
             let amax2 = Int.add max2 o2 in
             let ol1 = Int.add o1 offl1 in
             let ol2 = Int.add o2 offl2 in
             let or1 = Int.add o1 offr1 in
             let or2 = Int.add o2 offr2 in
             let r1 = Int.pos_rem (Int.add r1rel o1) m1 in
             let r2 = Int.pos_rem (Int.add r2rel o2) m2 in
             if amax1 < amin2  then
               begin
                 node_included o1 t1 ol2 subl2;
                 node_included or1 subr1 o2 t2;
               end
             else if amin1 > amax2 then
               begin
                 node_included o1 t1 or2 subr2;
                 node_included ol1 subl1 o2 t2;
               end
             else  (* this node of t2 covers part of the
                      interval of t1 we are focused on
                    *)
               begin
                 if amin1 = amin2 then
                   let mabs_min = amin1 in
                   begin
                     if amax1 = amax2 then
                       begin
                         is_similar r1 m1 v1 r2 m2 v2;
                         node_included or1 subr1 or2 subr2;
                       end
                     else if amax1 > amax2 then
                       begin
                           is_included_node_exn amin1 amax1 r1 m1 v1
                           amin2 amax2 r2 m2 v2 mabs_min amax2;
                         node_included o1 t1 or2 subr2;
                       end
                     else
                       begin
                         assert (amax1 < amax2);
                         is_included_node_exn
                           amin1 amax1 r1 m1 v1
                           amin2 amax2 r2 m2 v2 mabs_min amax1;
                         node_included or1 subr1 o2 t2;
                       end;
                     node_included ol1 subl1 ol2 subl2;
                   end
                 else
                   let treat_current_right_nodes mabs_min =
                      if amax1 = amax2 then
                       begin
                         is_included_node_exn
                           amin1 amax1 r1 m1 v1
                           amin2 amax2 r2 m2 v2 mabs_min amax1;
                         node_included or1 subr1 or2 subr2;
                       end
                     else if amax1 > amax2 then
                       begin
                         is_included_node_exn
                           amin1 amax1 r1 m1 v1
                           amin2 amax2 r2 m2 v2 mabs_min amax2;
                         node_included o1 t1 or2 subr2;
                       end
                     else
                       begin
                         assert (amax1 < amax2);
                         is_included_node_exn
                           amin1 amax1 r1 m1 v1
                           amin2 amax2 r2 m2 v2 mabs_min amax1;
                         node_included or1 subr1 o2 t2;
                       end;
                   in
                   if amin1 > amin2 then
                     begin
                       treat_current_right_nodes amin2;
                       node_included ol1 subl1 o2 t2;
                     end
                   else
                     begin
                       assert (amin1 < amin2);
                       treat_current_right_nodes amin1;
                       node_included o1 t1 ol2 subl2;
                     end
               end
   in node_included o1 t1 o2 t2
 ;;

 let is_included t1 t2 =
  try
   is_included_generic_exn V.is_included_exn Int.zero t1 Int.zero t2;
   true
  with Abstract_interp.Is_not_included -> false
  ;;

 (** Joins two trees with no overlapping intervals.  *)

 let rec union t1_curr_off t1 t2_curr_off t2 =
   (* Format.printf "Union t1:%a t2:%a@."
      (pretty_offset t1_curr_off) t1
      (pretty_offset t2_curr_off) t2;
   *)
   match t1, t2 with
   | Empty, Empty ->
       assert (t1_curr_off = t2_curr_off);
       t1_curr_off, empty
   | Empty, Node _ -> t2_curr_off, t2
   | Node _, Empty -> t1_curr_off, t1
   | Node (lmax, loffl, lsubl, loffr, lsubr, lremrel, lmodu, lv, _),
       Node (rmax, roffl, rsubl, roffr, rsubr, rremrel, rmodu, rv, _) ->
         let labs_min = t1_curr_off
         and labs_max = Int.add lmax t1_curr_off
         and rabs_min = t2_curr_off
         and rabs_max = Int.add rmax t2_curr_off
         in
         let lrem = Int.pos_rem (Int.add lremrel t1_curr_off) lmodu in
         let rrem = Int.pos_rem (Int.add rremrel t2_curr_off) rmodu in
         if is_above labs_min labs_max rabs_min rabs_max
         then
           (* t2 is on the right of t1 *)
           let new_curr_offr, new_subr =
             union (Int.add t1_curr_off loffr) lsubr
               t2_curr_off t2
           in
           make_node t1_curr_off lmax loffl lsubl
             (Int.sub new_curr_offr t1_curr_off) new_subr lrem lmodu lv
         else
           begin
             (* t1 is on the left of t2 *)
 (*            assert (is_above rabs_min rabs_max labs_min labs_max); *)
             let new_curr_offl, new_subl =
               union t1_curr_off t1
                 (Int.add t2_curr_off roffl) rsubl in
             make_node t2_curr_off rmax
               (Int.sub new_curr_offl t2_curr_off) new_subl roffr rsubr
               rrem rmodu rv
           end
 ;;

 (** Merge two trees with the same number of nodes. *)

 let rec merge f o1 t1 o2 t2 lopt =
   match t1, t2 with
     | _, _  when (t1 == t2 && o1 = o2) ->  o1, t1
     | Empty, Empty -> assert false
     | Node _, Empty -> assert false
     | Empty, Node _ -> assert false
     | Node (max1, offl1, subl1, offr1, subr1, rem1rel, modu1, v1, _),
       Node (max2, offl2, subl2, offr2, subr2, rem2rel, modu2, v2, _) ->
       let abs_min1 = o1
       and abs_max1 = Int.add max1 o1
       and abs_min2 = o2
       and abs_max2 = Int.add max2 o2
       in
       let rem1 = Int.pos_rem (Int.add rem1rel o1) modu1 in
       let rem2 = Int.pos_rem (Int.add rem2rel o2) modu2 in
       if abs_min2 > abs_max1 then
         if is_above abs_min1 abs_max1 abs_min2 abs_max2
         then (* t2 is on the right of t1 *)
           let off, t = merge f (Int.add o1 offr1) subr1 o2 t2 lopt in
           make_node o1 max1 offl1 subl1
             (Int.sub off o1) t rem1 modu1 v1
         else(* t1 is on the left of t2 *)
           begin
               (* Format.printf "t2:[%a %a] %a @.t1:[%a %a] %a@." Int.pretty
                  abs_min2 Int.pretty abs_max2 (pretty_debug_offset o2) t2
                  Int.pretty abs_min1
                  Int.pretty abs_max1 (pretty_debug_offset o1) t1; *)
               (*  assert (is_above abs_min2 abs_max2 abs_min1 abs_max1);  *)
             let off, t = merge f o1 t1 (Int.add o2 offl2) subl2 lopt in
             make_node o2 max2 (Int.sub off o2) t offr2 subr2 rem2 modu2
               v2
           end
       else if abs_min1 > abs_max2 then
         if is_above abs_min1 abs_max1 abs_min2 abs_max2
         then
             (* t2 is on the left of t1 *)
           let off, t = merge f (Int.add o1 offl1) subl1 o2 t2 lopt in
           make_node o1 max1 (Int.sub off o1) t offr1 subl1
             rem1 modu1 v1
         else
           begin
             assert (is_above abs_min2 abs_max2 abs_min1 abs_max1);
             (* t1 is on the right of t2 *)
             let off, t = merge f o1 t1 (Int.add o2 offr2) subr2 lopt in
             make_node o2 max2 offl2 subl2 (Int.sub off o2) t
               rem2 modu2 v2
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
           let abs_offl1 = Int.add o1 offl1
           and abs_offl2 = Int.add o2 offl2 in
           if abs_min1 = abs_min2  then
             merge f abs_offl1 subl1 abs_offl2 subl2 lopt, abs_min1
           else if abs_min1 < abs_min2 then
             let new_offl1, new_subl1 =
               add_node abs_min1 (Int.pred abs_min2)
                 rem1 modu1 v1 abs_offl1 subl1
             in merge f new_offl1 new_subl1 abs_offl2 subl2 lopt
             , abs_min2
           else
             begin
               assert (abs_min1 > abs_min2);
               let new_offl2, new_subl2 =
                 add_node abs_min2 (Int.pred abs_min1) rem2 modu2
                   v2 abs_offl2 subl2
               in merge f abs_offl1 subl1 new_offl2 new_subl2 lopt
               , abs_min1
             end
         in
         let (curr_offr, right_t), middle_abs_max =
           let abs_offr1 = Int.add o1 offr1
           and abs_offr2 = Int.add o2 offr2 in
           if abs_max1 = abs_max2 then
             merge f abs_offr1 subr1
               abs_offr2 subr2 lopt, abs_max1
           else if abs_max1 < abs_max2 then
             let new_offr2, new_subr2 =
               add_node
                 (Int.succ abs_max1) abs_max2 rem2 modu2 v2 abs_offr2 subr2 in
             merge f abs_offr1 subr1 new_offr2 new_subr2 lopt,
             abs_max1
           else
             begin
               assert (abs_max1 > abs_max2);
               let min = (Int.succ abs_max2) in
               let new_offr1, new_subr1 =
                 add_node min abs_max1 rem1 modu1 v1 abs_offr1 subr1 in
               merge f new_offr1 new_subr1 abs_offr2 subr2 lopt, abs_max2
             end
         in

         let rem, modu, v, _l = f middle_abs_min middle_abs_max
           rem1 modu1 v1 rem2 modu2 v2 lopt
         in
         let curr_offl, left_t =
           add_node middle_abs_min middle_abs_max rem modu v curr_offl left_t
         in union curr_offl left_t curr_offr right_t
 ;;

 let merge f o1 t1 o2 t2 =
 (*  Format.printf "@.*** Start Merge@."; *)
     merge f o1 t1 o2 t2 []
 ;;

 (** Auxiliary function to join 2 trees with merge *)
 let f_join _abs_min _abs_max rem1 modu1 v1 rem2 modu2 v2 lopt =
 (*  Format.printf "f_join: [%a, %a]@.(%a %a %a)@.(%a %a %a)@."
     Int.pretty _abs_min Int.pretty _abs_max Int.pretty rem1 Int.pretty
     modu1 V.pretty v1 Int.pretty rem2 Int.pretty modu2 V.pretty v2 ; *)
   let joined size v1 v2 =
       V.anisotropic_cast (Int.to_big_int size) (V.join v1 v2)
   in
   if (rem1 = rem2 && modu1 = modu2)
       || V.is_isotropic v2
   then
     rem1, modu1, joined modu1 v1 v2, lopt
   else if V.is_isotropic v1 then
     rem2, modu2, joined modu2 v1 v2, lopt
   else
      Int.zero, Int.one, V.topify_merge_origin (V.join v1 v2), lopt
 ;;

 (** Joining two trees with possible overlapping intervals *)
 let join t1 t2 = merge f_join Int.zero t1 Int.zero t2 ;;

 (* Given an integer i,
    find the interval the ith bit belongs to (thus its node)
    Returns: the zipper to navigate from the root to the node found,
    and the node itself
 *)
 exception Bit_Not_found
 let find_bit_offset i zipper offset tree =
   let rec aux_find tree curr_off z =
     match tree with
       | Empty -> raise Bit_Not_found
       | Node (max, offl, subl, offr, subr, _, _modu, _v, _) ->
         let abs_max = Int.add curr_off max in
         if (Int.ge i curr_off) && (Int.le i abs_max)
         then (z, curr_off, tree)
         else if Int.lt i curr_off
         then
           aux_find subl (Int.add curr_off offl) (Left(curr_off, tree, z))
         else begin
           assert (Int.gt i abs_max);
           aux_find subr (Int.add curr_off offr) (Right(curr_off, tree, z))
         end
   in
   aux_find tree offset zipper
 ;;

 let find_bit = fun x y -> find_bit_offset x End Int.zero y  ;;

(* Go to next interval containing the ival
   Assumes the ival we look for
   is greater than what we have in the first node
*)
 let fforward_bit b zipper offset node  =
   assert (
     (Int.gt b (Int.add offset (get_max node))) ||
       Int.lt b offset
   );
   let rec unzip_until_left zipper old_offset  old_node =
     match zipper with
       | End -> old_offset, old_node, End
       | Right (o, t, z) -> unzip_until_left z o t
       | Left (offset, tree, z) -> offset, tree, z
   in
   let rec check_next_node z offset node =
     let o', t', z' = unzip_until_left z offset node in
     match z' with
       | End -> find_bit_offset b z' o' t'
       | _ ->
         if (Int.lt b o') then find_bit_offset b z offset node
         else check_next_node z' o' t'
   in check_next_node zipper offset node
 ;;

(* The following two functions ending with _big_int
   were poached from the original offsetmap
*)
 let extract_bits_big_int ~start ~stop ~modu v =
   assert (if Abstract_interp.Int.le start stop &&
             Abstract_interp.Int.le stop modu
     then true
     else ( Format.printf "ebbi start %a stop %a modu %a@."
	      Abstract_interp.Int.pretty start
	      Abstract_interp.Int.pretty stop
	      Abstract_interp.Int.pretty modu;
	    false));
   let start,stop =
     if Cil.theMachine.Cil.little_endian then
       start,stop
     else
       let mmodu = Abstract_interp.Int.pred modu in
       Abstract_interp.Int.sub mmodu stop,Abstract_interp.Int.sub mmodu start
   in
   V.extract_bits ~start ~stop v
 ;;

 let extract_bits ~start ~stop ~modu v =
   extract_bits_big_int
     ~start:(Int.to_big_int start)
     ~stop:(Int.to_big_int stop)
     ~modu:(Int.to_big_int modu) v
 ;;

 let merge_bits_big_int ~conflate_bottom ~offset ~length ~value
     ~total_length acc =
   assert (let total_length_i = Abstract_interp.Int.of_int total_length in
            Abstract_interp.Int.le (Abstract_interp.Int.add length offset)
              total_length_i);
   if Cil.theMachine.Cil.little_endian then
     V.little_endian_merge_bits
       ~conflate_bottom
       ~offset ~value ~total_length acc
   else
     V.big_endian_merge_bits
       ~conflate_bottom
       ~offset ~value ~total_length ~length acc
 ;;

 let merge_bits ~conflate_bottom ~offset ~length ~value ~total_length acc =
   merge_bits_big_int
     ~conflate_bottom
     ~offset:(Int.to_big_int offset)
     ~length:(Int.to_big_int length)
     ~value
     ~total_length acc
 ;;

 (*
   [offset] is the offset where the read has begun (ie the global read start).
   [size] is the total size we want to read from [offset].
   [curr_off] and [node] refers to the current node to be read.
   [acc] is the current state of accumulated reads.
 *)
 let extract_bits_and_stitch ~conflate_bottom ~offset ~size curr_off node acc =
   let r=
     let rem, modu, v = get_vv node curr_off in
     let abs_max = Int.add curr_off (get_max node) in
     (*  last bit to be read,
         be it in the current node or one of its successors *)
     let max_bit = Int.pred (Int.add offset size) in
     let extract_single_step min acc =
       assert (not (V.is_isotropic v));
       let interval_offset = Int.sub min offset in
       let merge_offset =
         if Int.ge interval_offset Int.zero
         then interval_offset
         else Int.zero
       in
       let start = Int.pos_rem (Int.sub min rem) modu in
       let modu_end = if rem = Int.zero then Int.pred modu else Int.pred rem in
       (* where do we stop reading ?
          either at the end of the current slice (round_up_to_r min) or
          at the end of the interval (abs_max)
       *)
       let read_end =
         Int.min 
	   (Int.min (Int.round_up_to_r ~min ~r:modu_end ~modu) abs_max) 
	   max_bit 
       in
       let stop = Int.pos_rem (Int.sub read_end rem) modu
       in
(*              Format.printf "Single step: merge offset %Ld length %Ld \
 start %Ld stop %Ld total length %Ld offset %Ld max bit %Ld\
 @\n current offset %Ld Rem %Ld modu %Ld V %a@."
         merge_offset (Int.length start stop) start stop
         size
         offset max_bit
         curr_off rem modu V.pretty v ;*)

       let _inform_extract_pointer_bits, read_bits =
         extract_bits ~start ~stop ~modu v
       in
       let result = 
	 merge_bits ~conflate_bottom
           ~offset:merge_offset ~length:(Int.length start stop)
           ~value:read_bits ~total_length:(Int.to_int size) acc
       in
       read_end, result
     in
     let start = Int.max offset curr_off
     and stop = Int.min max_bit abs_max in
     if V.is_isotropic v then
       let interval_offset = Int.sub rem start (* ? *) in
       let merge_offset =
         if Int.lt interval_offset Int.zero then Int.zero else interval_offset
       in merge_bits ~conflate_bottom ~offset:merge_offset
            ~length:(Int.length start stop)
            ~value:v ~total_length:(Int.to_int size) acc
     else
       let start_point = ref start in
       let acc = ref acc in

       while Int.le !start_point stop do
	 let read_end, result = 
	   extract_single_step !start_point !acc;
	 in
         acc := result;
         start_point := Int.succ read_end;
       done;
       !acc;
   in
(*   Format.printf "extract_bits_and_stitch istart@ %Ld@ size %Ld\
  coff %Ld abs_max %Ld val %a@\n  acc %a res %a@."
     offset size curr_off (Int.add curr_off (get_max node))
     V.pretty (get_v node) V.pretty acc V.pretty r; *)
   r
 ;;

 (* First and last bits are included in the interval *)
 let imprecise_find first_bit last_bit tree =
   let rec aux tree_offset tree =
     match tree with
     | Empty -> V.bottom
     | Node (max, offl, subl, offr, subr, _rrel, _m, v, _) ->
       let abs_max = Int.add max tree_offset in
       let subl_value =
         if Int.lt first_bit tree_offset then
           let subl_abs_offset = Int.add tree_offset offl in
           aux subl_abs_offset subl
         else V.bottom
       in
       let subr_value =
         if Int.gt last_bit abs_max then
           let subr_abs_offset = Int.add tree_offset offr in
           aux subr_abs_offset subr
         else V.bottom
       in
       let current_node_value =
         if Int.lt last_bit tree_offset || Int.gt first_bit abs_max
         then V.bottom
         else V.topify_misaligned_read_origin v
       in
       V.join subl_value (V.join subr_value current_node_value)
   in
   aux Int.zero tree

(* Searches for all intervals of the rangemap
   contained in the  the interval [offset, offset + size - 1].
   Assumes the rangemap is rooted at offset 0.
*)

 let find ~with_alarms ~conflate_bottom offset size tree period_read_ahead =
   ignore(with_alarms); (* FIXME *)
   let offset64 = Int.from_big_int offset in
   let size64 = Int.from_big_int size in
   let period_read_ahead = Int.from_big_int period_read_ahead in
   let z, cur_off, root = find_bit offset64 tree in
   match root with
     | Empty ->
           (* Bit_Not_found has been raised by find_bit in this case *)
         assert false
     | Node (max, _, _, _, _subr, rrel, m, v, _) ->
         let r = Int.pos_rem (Int.add rrel cur_off) m in
         let isize = Int.pred (Int.add offset64 size64) in
         let nsize = Int.add cur_off max in
         let isotropic = V.is_isotropic v in
         if
           (Int.le isize nsize) &&
             (isotropic ||
                 ((Int.equal m size64) && Int.equal (Int.pos_rem offset64 m) r))
         then begin
             let read_ahead =
               if isotropic || (Int.is_zero (Int.rem period_read_ahead m))
               then Some (Int.to_big_int nsize)
               else None
             in
             read_ahead, v
           end
         else
           let acc = ref V.singleton_zero in
           let impz = { node = root; offset = cur_off; zipper = z; } in
           while (Int.le impz.offset isize) do
             let (* _inform_extract_pointer_bits,*) v =
               extract_bits_and_stitch ~conflate_bottom
                 ~offset:offset64 ~size:size64
                 impz.offset impz.node !acc
             in
             acc := v;
             if (Int.add impz.offset (get_max impz.node)) >= isize
             then impz.offset <- Int.max_int (* end the loop *)
             else
               (* Nominal behavior: do next binding *)
               imp_move_right impz
           done;
           None, !acc
 ;;

 (*  Finds the value associated to a set of bit offsets represented as an ival.
 *)
 let find_ival ~conflate_bottom ~validity ~with_alarms ival tree size  =
    let filtered_by_bound =
      try
        Tr_offset.filter_by_bound_for_reading
          ~with_alarms ival size validity
      with
        Tr_offset.Unbounded -> raise Not_found (* return top *)
    in
    let result =
     match filtered_by_bound with
       | Tr_offset.Interval(mn, mx, m) ->
           let r = Abstract_interp.Int.pos_rem mn m in
           let mn = ref mn in
           let acc = ref V.bottom in
           let pred_size = Abstract_interp.Int.pred size in
           while Abstract_interp.Int.le !mn mx do
             let read_ahead, v =
               find ~conflate_bottom ~with_alarms !mn size tree m
             in
             acc := V.join v !acc;
             let naive_next = Abstract_interp.Int.add !mn m in
             mn :=
               match read_ahead with
                 None -> naive_next
               | Some read_ahead ->
                   let max = Abstract_interp.Int.sub read_ahead pred_size in
                   let aligned_b =
                     Abstract_interp.Int.round_down_to_r ~max ~r ~modu:m in
                   Abstract_interp.Int.max naive_next aligned_b
           done;
           !acc
       | Tr_offset.Set s ->
           Ival.O.fold
             (fun offset acc ->
                let _, new_value =
                  find ~conflate_bottom ~with_alarms
                    offset size tree Abstract_interp.Int.zero
                in
                let result = V.join acc new_value in
                if V.equal result V.top then raise Not_found;
                result)
             s
             V.bottom
       | Tr_offset.Imprecise(mn, mx) ->
           imprecise_find (Int.from_big_int mn) (Int.from_big_int mx) tree
   in result
 ;;

 (* Keep the part of the tree under a given limit offset. *)

 let rec keep_below offset curr_off tree =
   match tree with
     | Empty -> offset, empty
     | Node (max, offl, subl, offr, subr, rrel, m, v, _) ->
       let new_offl = Int.add offl curr_off in
       if Int.lt offset curr_off then
         keep_below offset new_offl subl
       else if Int.equal offset curr_off then
         new_offl, subl
       else
         let sup = Int.add curr_off max in
         if Int.gt offset sup then
           let new_offr, new_subr =
             keep_below offset (Int.add curr_off offr) subr
           in
           curr_off,
           nNode (max, offl, subl,
                 Int.sub new_offr curr_off, new_subr, rrel, m, v)
         else
           let new_max = (Int.pred (Int.sub offset curr_off)) in
           add_node
             curr_off (Int.add new_max curr_off)
             (Int.pos_rem (Int.add curr_off rrel) m) m v
             (Int.add curr_off offl ) subl
 ;;

 let rec keep_above offset curr_off tree =
   match tree with
     | Empty -> (Int.succ offset), empty
     | Node (max, offl, subl, offr, subr, rrel, m, v, _) ->
        let new_offr = Int.add offr curr_off in
        let abs_max = Int.add curr_off max in
        if Int.gt offset abs_max then
          (* This node should be forgotten,
             let's look at the right subtree
          *)
          keep_above offset new_offr subr
        else if Int.equal offset abs_max then
          (* we are at the limit,
             the right subtree is the answer
          *)
          new_offr, subr
        else
          if Int.lt offset curr_off then
            (* we want to keep this node and part of its left subtree *)
            let new_offl, new_subl =
              keep_above offset (Int.add curr_off offl) subl
            in
            curr_off,
            nNode(max,
                 Int.add curr_off new_offl, new_subl,
                 offr, subr, rrel, m, v)
          else
            (* the cut happens somewhere in this node
               it should be cut accordingly
               and reinjected into its right subtree
            *)
            let new_rrel = (Int.pos_rem (Int.add curr_off rrel) m) in
            add_node (Int.succ offset) abs_max new_rrel m v new_offr subr
;;


 let update  ~offset ~abs_max ~size curr_off tree v =
(*   let offset64 = Int.from_big_int offset in
   let size64 = Int.from_big_int size in
*)
   assert( abs_max = Int.pred (Int.add size offset)) ; (* TODO: remove *)
   let off1, t1 = keep_above abs_max curr_off tree
   and off2, t2 = keep_below offset curr_off tree in
   let rabs = Int.pos_rem offset size in
   let off_add, t_add = add_node offset abs_max rabs size v off1 t1 in
   union off2 t2 off_add t_add
 ;;

 let update_imprecise ~offset ~abs_max ~size curr_off tree v =
   (*   let offset64 = Int.from_big_int offset in
        let size64 = Int.from_big_int size in
   *)
   let off1, t1 = keep_above abs_max curr_off tree
   and off2, t2 = keep_below offset curr_off tree in
   let rabs = Int.pos_rem offset size in
   let v_is_isotropic = V.is_isotropic v in
   let z, o, t = find_bit_offset offset End curr_off tree in
   let left_tree = ref t2 in
   let left_offset = ref off2 in
   let impz = { node = t; offset = o; zipper = z; } in
   while Int.le impz.offset abs_max do
     match impz.node with
       | Empty -> assert false
       | Node (max, _offl, _subl, _offr, _subr, rrel, m_node, v_node, _) ->
         let rabs_node = Int.pos_rem (Int.add impz.offset rrel) m_node in
(*         Format.printf "rabs:%Ld rabs_node:%Ld coff:%Ld rrel:%Ld@." rabs
           rabs_node !cur_off rrel; *)
         let new_r, new_m, new_v =
           if V.is_isotropic v_node || v_is_isotropic  ||
             (Int.equal rabs rabs_node && Int.equal m_node size)
           then
             let new_r, new_m =
               if v_is_isotropic
               then rabs_node, m_node
               else rabs, size
             in
             let cast_v =
               V.anisotropic_cast ~size:(Int.to_big_int new_m) (V.join v_node v)
             in
             new_r, new_m, cast_v

           else
             let new_value = V.topify_merge_origin (V.join v_node v) in
             let new_rem = 0L and new_modu = 1L in
             new_rem, new_modu, new_value
         in
         let node_abs_max = Int.add impz.offset max in
         let end_reached, write_max =
           if Int.ge node_abs_max abs_max then true, abs_max
           else false, node_abs_max in

         let new_left_offset, new_left_tree =
           add_node
             (Int.max impz.offset offset)
             write_max
             new_r new_m new_v !left_offset !left_tree in
         left_tree := new_left_tree;
         left_offset := new_left_offset;

         if not end_reached then imp_move_right impz
         else impz.offset <- Int.max_int
   done;
   union !left_offset !left_tree off1 t1
 ;;

 module T = struct type t = tt
            let sentinel = empty;;
            let equal = equal;;
            let hash = hash;;
 end
 ;;

 module Vs = struct type t = V.t
            let sentinel = V.bottom;;
            let equal = V.equal;;
            let hash = V.hash;;
 end
 ;;

 module OT = struct
   type t = Int.t * tt
   let sentinel = Int.zero, empty
 end

module UpdateIvalCache = Binary_cache.Make_Het1_1_4(T)(Vs)(Int)(OT)
let () = Project.register_todo_before_clear (fun _ -> UpdateIvalCache.clear ())

(** Update a set of intervals in a given rangemap
    all offsets starting from mn ending in mx must be updated
    with value v, every period
*)
 let update_ival ~left_cover ~right_cover ~exact
     ~mn ~mx ~period ~size tree v =
   assert(mx >= mn);
(*   Format.printf "update_ival tree: %a %Ld %Ld exact:%B v:%a@."
     (pretty_offset curr_off) tree mn mx exact
     V.pretty v;  *)
   let r = Int.pos_rem mn period in
   let rec aux_update_ival left_cover right_cover mn mx curr_off tree =
     let f () =
(*       Format.printf "aux_update_ival ( %a %a in@\n%a@." 
	 Int.pretty mn
	 Int.pretty mx
	 pretty tree; *)
       let r =
       match tree with
         | Empty -> curr_off, empty
         | Node (max, offl, subl, offr, subr, r_node, m_node, v_node, _) ->
               (* Look at the cache if min_cover and max_cover are true *)
(*           Format.printf "aux_update_ival: %Ld %Ld %B %B@."
               mn mx left_cover right_cover; *)
           let abs_offl = Int.add offl curr_off in
           let abs_offr = Int.add offr curr_off in

           let new_offl, new_subl, undone_left =
             let last_read_max_offset = Int.sub curr_off size in
             if Int.pred (Int.add mn size) < curr_off then
               let new_mx = Int.round_down_to_r
                 ~max:last_read_max_offset
                 ~r ~modu:period in
               let new_mx, right_cover, undone =
                 if Int.gt new_mx mx
                 then mx, false, None
                 else new_mx, true, Some (Int.add new_mx period)
               in
               let o, t =
                 aux_update_ival left_cover right_cover mn new_mx abs_offl subl
               in o, t, undone
             else abs_offl, subl, Some mn

           and new_offr, new_subr, undone_right =
             let abs_max = (Int.add curr_off max) in
             let first_read_min_offset = Int.succ abs_max in
             if Int.gt mx abs_max then
               let new_mn = Int.round_up_to_r ~min:first_read_min_offset ~r
                 ~modu:period in
               let new_mn, left_cover, undone =
                 if Int.lt new_mn mn then
                   mn, false, None
                 else new_mn, true, Some (Int.sub new_mn period)
               in
               let o, t =
                 aux_update_ival left_cover right_cover new_mn mx abs_offr subr
               in o, t, undone
             else abs_offr, subr, Some mx

           in
           let o, t =
             add_node 
	       curr_off
	       (Int.add curr_off max)
	       (Int.pos_rem r_node m_node)
               m_node v_node new_offl new_subl 
	   in
           let curr_off, tree = union o t new_offr new_subr in
           match undone_left, undone_right with
             | Some min, Some max ->
               begin
                 let update = if exact then update else update_imprecise in
(*                 Format.printf "Update tree %a min:%Ld max:%Ld size:%Ld@."
                   (pretty_offset !o) !t
                   min
                   max size; *)
                 if Int.equal size period
                 then
                   let abs_max = Int.pred (Int.add size max) in
(*		   Format.printf "minmaxsize %a %a %a@."
		     Int.pretty min
		     Int.pretty max
		     Int.pretty size; *)
                   update ~offset:min ~abs_max ~size curr_off tree v
                 else
                 let offset = ref min in
                 let o = ref curr_off in
                 let t = ref tree in
                 while Int.le !offset max do
                   let abs_max = Int.pred (Int.add size !offset) in
                   let o', t' =
                     update ~offset:!offset ~abs_max ~size !o !t v in
                   o := o';
                   t := t';
                   (* Format.printf "Inter tree:%a@." (pretty_offset o') t';*)
                   offset := Int.add !offset period;
                 done;
                 !o, !t;
               end
             | Some _, None
             | None, Some _
             | None, None -> curr_off, tree
       in
(*       Format.printf "aux_update_ival ) %a %a@." 
	 Int.pretty mn
	 Int.pretty mx; *)
       r
     in
     match left_cover, right_cover, exact with
       | true, true, false ->
         let rread_alignment = Int.pos_rem (Int.sub mn curr_off) period in
         UpdateIvalCache.merge f tree v curr_off rread_alignment period size
       | _, _, _ -> f ()

     in
   let _, t = aux_update_ival left_cover right_cover mn mx Int.zero tree in
   t
 ;;

 let update_ival min max ~exact ~mn ~mx ~period ~size tree v =
   let r = Int.pos_rem mn period in
   let m1 = Int.round_up_to_r ~r ~min ~modu:period in
   let left_cover = Int.equal m1 mn in
   let m2 = Int.round_down_to_r ~r ~max ~modu:period in
   let right_cover = Int.equal m2 mx in
(*   try *)
     update_ival ~left_cover ~right_cover ~exact ~mn ~mx ~period ~size tree v
(*   with e ->
     Format.printf "UPDATE_IVAL %a %a %B %a %a %a %a %a %a@."
       Int.pretty min
       Int.pretty max
       exact
       Int.pretty mn
       Int.pretty mx
       Int.pretty period
       Int.pretty size
       pretty tree
       V.pretty v;
     raise e*)
 ;;

(* TODO: do something about the read ahead *)
 let copy_single offset tree size _period_read_ahead =
   let z, cur_off, root = find_bit offset tree in
   let cur_copy_offset = ref offset in
   let impz = { node = root; offset = cur_off; zipper = z; } in
   let acc = ref empty in
   let iend = Int.pred (Int.add offset size) in
   while
     (match impz.node with
       | Empty ->
         (* Bit_Not_found has been raised by find_bit in this case *)
         assert false
       | Node (max, _, _, _, _subr, rrel, m, v, _) ->
         let nend = Int.add impz.offset max in
         let new_abs_end = Int.sub (Int.max iend nend) offset in
         let o, t = add_node (Int.sub !cur_copy_offset offset) new_abs_end
           (Int.add impz.offset rrel) m v Int.zero !acc in
         assert (o = Int.zero);
         acc := t;
         let cond = Int.gt iend nend in
         if cond then begin
           imp_move_right impz;
           cur_copy_offset := impz.offset;
         end;
         cond)
   do ();
   done;
   None, !acc (* FIXME here : see TODO above *)
 ;;

 let copy_ival ~with_alarms ~validity ival tree size =
    let filtered_by_bound =
      try Tr_offset.filter_by_bound_for_reading ~with_alarms ival size validity
      with Tr_offset.Unbounded -> raise Not_found (* return top *)
    in
    let result =
      match filtered_by_bound with
       | Tr_offset.Interval(mn, mx, m) ->
          let r = Abstract_interp.Int.pos_rem mn m in
          let mn = ref mn in
          let acc_tree = ref empty in
          let pred_size = Abstract_interp.Int.pred size in
          while Abstract_interp.Int.le !mn mx do
            let read_ahead, new_tree =
              copy_single 
		(Int.from_big_int !mn) 
		tree 
		(Int.from_big_int size) None 
	    in
            let o, t = join !acc_tree new_tree in
            assert (o = Int.zero);
            acc_tree := t;
            let naive_next = Abstract_interp.Int.add !mn m in
            mn := match read_ahead with
              | None -> naive_next
              | Some read_ahead ->
                let max = Abstract_interp.Int.sub read_ahead pred_size in
                let aligned_b =
                  Abstract_interp.Int.round_down_to_r ~max ~r ~modu:m in
                Abstract_interp.Int.max naive_next aligned_b
                ;
          done;
          !acc_tree
       | Tr_offset.Set s ->
           Ival.O.fold
             (fun offset acc_tree ->
               let _, t =
		 copy_single
                   (Int.from_big_int offset)
		   tree
                   (Int.from_big_int size) 
		   None
	       in
               let o, t =
		 join acc_tree t
               in
               assert (o = Int.zero);
               t)
             s 
	     empty
       | Tr_offset.Imprecise(_mn, _mx) -> assert false (* TODO *)
    in
    result
 ;;

end
(* Below is some code for testing purposes : do not remove for now *)
(* module Test =  Make (Cvalue.V) ;;
 *
 * open Cvalue.V
 * open Int
 * open Test
 *
 * ;;
 *
 * let ppf = Format.std_formatter;;
 *
 * let t = Empty;;
 * (\*
 * Random.init 42;;
 *
 * let make_random_tree n ub =
 *   let gen_symb =
 *     let i = ref 0 in
 *     fun () ->
 *       incr i;
 *       "a"^(string_of_int !i)
 *   in
 *   let new_base () =
 *     Base.create_string (gen_symb ())
 *   in
 *   let make_interval lb ub =
 *     let max = Int.sub (Int.succ ub) lb in
 *      Random.int64 max
 *   in let valeurs =
 *     [Ival.singleton_zero; Ival.zero_or_one; Ival.zero; Ival.one] in
 *   let bases = (new_base ()) :: [new_base ()]
 *   and maxmod = 64L
 *   and maxrem = 6L
 *   and len = List.length valeurs
 *   in let nlist = List.length bases
 *   in let make_value () =
 *     let base = List.nth bases (Random.int nlist) in
 *     let v = inject base (List.nth valeurs (Random.int len))
 *     and r = Random.int64 maxrem
 *     and m = Int.succ (Random.int64 maxmod)
 *     in r, m, v
 *   in let rec node lb i t =
 *     let r, m, v = make_value () in
 *     if i = 0 then
 *       snd (Int.add_node 0L lb ub r m v t)
 *     else
 *       begin
 *         assert (compare lb 0L >= 0);
 *         let ub1 = add lb (make_interval lb ub)
 *         in
 *       (\*  Format.printf "[%Ld...%Ld]" lb ub1;*\)
 *         let _, t1 = add_node 0L lb ub1 r m v t in
 *         let new_lb = Int.succ ub1
 *         in
 *         if compare new_lb ub > 0 then t1 else node new_lb (i - 1) t1;
 *       end;
 *   in node 0L n Empty
 * ;;
 *
 * let _, t1 = add_node 0L 0L 31L 0L 32L singleton_zero t;;
 *
 * let _, t2 = add_node 0L 32L 63L  0L 32L singleton_zero t1;;
 *
 * let _, t3 = add_node 0L 64L 88L 0L 32L zero_or_one t2;;
 *
 * let _, t4 = add_node 0L 89L 101L 0L 32L zero_or_one t3;;
 *
 * let _, t5 = add_node 0L 102L 104L 0L 32L zero_or_one t4;;
 *
 * let d6, t6 = add_node 0L (-32L) (-1L) 0L 32L singleton_zero
 *     t5;;
 *
 * (\*
 * pretty_debug ppf t6;;
 * Test.pretty ppf t6;;
 * *\)
 * (\*let offset, _node, zipper = leftmost_child 0L End t6;;*\)
 *
 * (\* node coff max offl subl offr subr rem modu v *\)
 * *\)
 *
 *
 * let base_a = Base.create_string "a"
 * and base_b = Base.create_string "b"
 * ;;
 *
 * let val1 = inject base_a Ival.singleton_zero
 * and val2 = inject base_b Ival.singleton_zero
 * ;;
 *
 * let c6, u6 = make_node (-32L) 31L 0L t 32L t 0L 32L singleton_zero;;
 *
 * let c5, u5 = make_node 102L 2L  0L t 3L t 0L 32L zero_or_one;;
 *
 * let c4, u4 = make_node 89L 12L 0L t (Int.sub c5 89L) u5 0L 32L zero_or_one;;
 *
 * let c3, u3 = make_node 32L 31L 0L t 32L t 0L 32L singleton_zero;;
 *
 * let c2, u2 = make_node 64L 24L (Int.sub c3 64L) u3 (Int.sub c4 64L) u4 0L 32L zero_or_one ;;
 *
 * let c1, u1 = make_node 0L 31L  (Int.sub c6 0L) u6 (Int.sub c2 0L) u2 0L 8L zero_or_one;;
 *
 * let ppf = Format.std_formatter;;
 * pretty_offset c1 ppf u1;;
 * let pretty_offset a p c  =
 *   Format.fprintf p  "@.";
 *   pretty_offset a p c
 * ;;
 *
 * Format.fprintf ppf "Testing keep_below@.";;
 * let o1, t1 = keep_below (63L) c1 u1 in pretty_offset o1 ppf t1;;
 * let o1, t1 = keep_below (64L) c1 u1 in pretty_offset o1 ppf t1;;
 * let o1, t1 = keep_below (65L) c1 u1 in pretty_offset o1 ppf t1;;
 *
 * Format.fprintf ppf "Testing keep_above@.";;
 * let o1, t1 = keep_above (62L) c1 u1 in pretty_offset o1 ppf t1;;
 * let o1, t1 = keep_above (63L) c1 u1 in pretty_offset o1 ppf t1;;
 * let o1, t1 = keep_above (64L) c1 u1 in pretty_offset o1 ppf t1;;
 *
 * Format.fprintf ppf "Testing update imprecise@.";;
 * let o,t = update_imprecise
 *   ~offset:8L
 *   ~size:8L c1 u1 val1
 * in pretty_offset o ppf t;;
 *
 * let o,t = update_imprecise
 *   ~offset:6L
 *   ~size:8L c1 u1 val1
 * in pretty_offset o ppf t;;
 *
 * (\*
 * List.iter2 check [c1;c2;c3;c4;c5;c6] [u1;u2;u3;u4;u5;u6];;
 * Test.equal u1 t6;;
 * (\*
 * pretty_debug ppf u1;;
 *
 * flush_all ();;
 * *\)
 *
 *
 *
 *
 *
 * let at_off, at = add_node  0L 0L 31L 0L 32L val1 t;;
 * let ca, at2 = add_node 0L 32L 80L 0L 48L val1 at;;
 *
 * let bt_off, bt = add_node  0L 0L 31L 0L 32L val2 t;;
 * let _, bt2 = add_node 0L 32L 64L 0L 32L val2 bt;;
 * let _, bt3 = add_node 0L 65L 79L 0L 1L singleton_zero bt2;;
 * let _, bt4 = add_node 0L 80L 128L 0L 1L val2 bt3;;
 *
 * *\)
 *
 * (\*let x = print bt3;;
 *
 * let l = to_list bt3;;
 *
 * *\)
 *     (\*
 * let a = ref Empty;;
 * let aa = ref Old.empty;;
 * let b = ref Empty;;
 * let bb = ref Old.empty;;
 * let c = ref 0L;;
 *
 * for i = 0 to 79 do
 *   let offs2 x = Int.add x (Int.of_int (81*i)) in
 *   let boffs2 x = Bint.of_int64 (offs2 x) in
 *     a:=snd( add_node 0L (offs2 0L)
 *       (offs2 31L) 0L 32L val1 !a);
 *   aa := Old.add_internal (boffs2 0L, boffs2 31L) (Bint.zero, Bint.of_int64 32L, val1) !aa;
 *
 *    a:= snd (add_node 0L (offs2 32L) (offs2 80L) 0L 48L val1 !a);
 *     aa := Old.add_internal (boffs2 32L, boffs2 80L) (Bint.zero, Bint.of_int64 48L, val1) !aa;
 * done;;
 *
 * for i = 0 to 80 do
 *   let offs x = Int.add x (Int.of_int (80*i)) in
 *   let boffs x = Bint.of_int64 (offs x) in
 *    b:= snd (add_node 0L (offs 0L) (offs 31L) 0L 32L val2 !b);
 *   bb:= Old.add_internal (boffs 0L, boffs 31L) (Bint.zero, Bint.of_int64 32L, val2) !bb;
 *
 *     b:= snd (add_node 0L (offs 32L) (offs 64L) 0L 16L val2 !b);
 *     bb:= Old.add_internal (boffs 32L, boffs 64L) (Bint.zero, Bint.of_int64 16L, val2) !bb;
 *
 *    b:= snd (add_node 0L (offs 65L) (offs 79L) 0L 1L singleton_zero
 *               !b);
 *     bb:= Old.add_internal (boffs 65L, boffs 79L) (Bint.zero, Bint.of_int64 1L, singleton_zero) !bb;
 * done
 * ;;
 *
 *
 * (\* (\\* let off, new_t = join at2 bt3;; *\\) *\)
 * (\* (\\* Format.printf "%a@.%a@.%a@." pretty at2 pretty bt3 pretty new_t;; *\\) *\)
 * (\* Format.printf "a---@.%a@.aa--@.%a@." pretty !a Old.pretty_compare !aa *\)
 * (\* ;; *\)
 * (\* Format.printf "b---@.%a@.bb--@.%a@." pretty !b Old.pretty_compare !bb *\)
 * (\* ;; *\)
 *
 * assert (Test.equal !a (translate_from_old !aa));;
 * assert (Test.equal !b (translate_from_old !bb));;
 *
 *  let noff, nt = join (translate_from_old !aa)(translate_from_old !bb) ;;
 *  let  ot = translate_from_old (snd (Old.join !aa !bb));;
 * if (Test.equal nt ot) then Format.printf "Join is ok@.";;
 * *\)
 * (\* Format.printf "b---@.%a@.bb--@.%a@." pretty nt Old.pretty_compare ot *\)
 * (\* ;; *\)
 *
 * (\* print2 nt;; *\)
 * (\*
 * Format.printf "%a@.%a@.%a@." pretty !a pretty !b pretty nt
 * ;;*\)
 * (\* let b = ref bt3;; *\)
 * (\* let i = 1 ;; *\)
 * (\*   let offs x = Int.add x (Int.of_int (80*i));; *\)
 * (\*    b:= snd (add_node 0L (offs 0L) (offs 31L) 0L 32L val2 !b);; *\)
 * (\*    b:= snd (add_node 0L (offs 32L) (offs 64L) 0L 32L val2 !b);; *\)
 * (\*    b:= snd (add_node 0L (offs 65L) (offs 79L) 0L 1L singleton_zero *\)
 * (\*               !b);; *\)
 *
 *
 * (\* let i = 2 ;; *\)
 * (\*   let offs x = Int.add x (Int.of_int (80*i));; *\)
 * (\*    b:= snd (add_node 0L (offs 0L) (offs 31L) 0L 32L val2 !b);; *\)
 *
 * (\* let before = !b;; *\)
 * (\* (\\*b:= snd (add_node 0L (offs 32L) (offs 64L) 0L 32L val2 !b);;*\\) *\)
 *
 * (\* (\\*Debugging purposes *\\) *\)
 * (\* let coff, tr = subtree_from_interval 160L 191L 0L before;;  *\)
 *
 * (\* let c, t = make_node 0L 144L 0L Empty  160L tr  0L 32L val2;;  *\)
 * (\* print_offset c t;; *\)
 *
 * (\* let c, t = make_node 192L 32L (Int.sub c 192L) t 32L Empty  0L 32L val2;; *\)
 * (\* print_offset c t;; *\)
 * (\*
 * let ub = 819488344203502342L
 * and n = 900;;
 *
 * print_string "Random generation of trees\n";;
 * let tr = make_random_tree n ub;;
 * print_string "TR\n";;
 * print tr;;
 *
 * print_string "TRR\n";;
 * let trr = make_random_tree n ub;;
 * print trr;;
 *
 * let ok b =  if b then print_string "ok\n" else print_string "ko\n";;
 *
 * let _, trrr = join tr trr;;print_string "TRRRR\n";;
 * print trrr;;
 * let a = is_included trr trrr ;;
 * ok a;;
 *
 * let b = is_included tr trr;;
 * ok b;;
 * *\)
 *
 * (\*
 * Local Variables:
 * compile-command: "make -C ../..  src/memory_state/new_offsetmap.cmo "
 * End:
 * *\) *)
