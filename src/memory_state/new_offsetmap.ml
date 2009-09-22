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


module Bint = Abstract_interp.Int
;;
    
module Int = struct
  include Int64
  
  let pretty ppf v =
    Format.fprintf ppf "%Ld" v
;;

let equal x y =
    compare x y = 0
;;

let equal0 x = equal x zero
;;

let (%) x y = rem x y
;;

let to_big_int = Abstract_interp.Int.of_int64
  end
;;

(** Implementation of "rangemaps" *)
module Make (V : Lattice_With_Isotropy.S) = struct

  open Int
  open Format
    
 (* module Old = Offsetmap.Build(V) *)

    
  type y = V.t
  type widen_hint = V.widen_hint
  type t =
    | Empty
  (*  min is always zero
      max * offset_left * subtree_left * offset_right *
   * subtree_right * rem * modu * value
   *)
    | Node of  Int.t * Int.t * t * Int.t * t *
              Int.t * Int.t * V.t


  let empty = Empty;;
  
  let equal_vv (rem1, modu1, v1) (rem2, modu2, v2) =
    Int.equal rem1 rem2 &&
    Int.equal modu1 modu2 &&
    V.equal v1 v2
;;
      
let rec equal t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node _, Empty | Empty, Node _ -> false
  | Node (max, offl, subl, offr, subr, rem, modu, v),
      Node (max1, offl1, subl1, offr1, subr1, rem1, modu1, v1)
        ->
          equal_vv (rem,modu,v) (rem1, modu1, v1) &&
          Int.equal max max1 && Int.equal offl1 offl &&
          Int.equal offr1 offr &&
          equal subl1 subl && equal subr1 subr
;;

  let get_vv = function
    | Empty -> assert false
    | Node (_, _, _, _, _, rem, modu, v) ->
        rem, modu, v
;;     

  let get_max = function
    | Empty -> assert false
    | Node (max, _, _, _, _, _, _, _) ->
        max
;;     
  
  
  let is_above min1 max1 min2 max2 = 
    let signature_interval min max =
      Int.logand (Int.logxor (Int.pred min) max) Int.max_int in
    signature_interval min1 max1 > signature_interval min2 max2
;;


(** Zippers : Offset of a node * Node * continuation of the zipper *)
type zipper =
  | End
  | Right of Int.t * t * zipper
  | Left of Int.t * t * zipper;;

exception End_reached;;
exception Empty_tree;;

let pr_zipper ppf z  =
  printf "[Zipper]---@.";
  let rec aux ppf = function
  | End -> printf "@ E@."
  | Right (o, Node(max, _, _, _, _subr, _, _, _),z ) ->
      fprintf ppf "@[<h 2> [%a,%a] R@\n%a@]" Int.pretty o Int.pretty (add o
                                                                   max)
        aux z
  | Left (o, Node(max, _, _, _, _subr, _, _, _),z ) ->
      fprintf ppf "@[<h 2> [%a,%a] L@\n%a@]" Int.pretty o
        Int.pretty (add o max)
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
  | Right (offset, Node(max, offl, subl, _offr, _subr, rem, modu, v), z)
    ->
      rezip z offset (Node (max, offl, subl,
                     sub curr_off offset, node, rem, modu, v))
  | Left (offset, Node(max, _offl, _subl, offr, subr, rem, modu, v), z)
    ->
      rezip z offset (Node(max, sub curr_off offset, node,
                    offr, subr, rem, modu, v))
  | Right (_, Empty, _) | Left (_, Empty, _) -> assert false
;;

(** Returns an absolute position, a node and a zipper *)
let rec leftmost_child curr_off zipper node =
  match node with
  | Empty -> raise Empty_tree
  | Node (_, _, Empty, _, _, _, _, _) -> curr_off, node, zipper
  | Node (_, offl, subl, _, _, _, _, _) ->
      let new_offset = add curr_off offl in
      leftmost_child new_offset (Left (curr_off, node, zipper)) subl
;;

(** Returns an absolute position, a node and a zipper *)
let rec rightmost_child curr_off zipper node =
  match node with
  | Empty -> raise Empty_tree
  | Node (_, _, _, _, Empty, _, _, _) -> curr_off, node, zipper
  | Node (_, _offl, _subl, offr, subr, _, _, _) ->
      let new_offset = add curr_off offr in
      rightmost_child new_offset (Right (curr_off, node, zipper)) subr
;;


(** Move to the right of the current node
    Uses a zipper for that
 *)
let rec move_right curr_off node zipper =
  match node with
  | Node (_, _, _, offr, ((Node _ ) as subr), _, _, _) ->
      let new_offset = add curr_off offr in
      leftmost_child new_offset (Right (curr_off, node, zipper)) subr
  | Node (_, _, _, _, Empty, _, _, _) ->
       begin
        let rec unzip_until_left zipper =
          match zipper with
          | End -> raise End_reached
          | Right (_, _, z) -> unzip_until_left z
          | Left (offset, tree, z) -> offset, tree, z
        in unzip_until_left zipper
      end   
  | Empty ->
       assert false
 ;;       


(** Folding and iterating from the leftmost node to the rightmost one
    If t =  n0         fold f t i = f n2 (f n0 (f n1 i))
           / \         iter f t   = f n1; fn0; f n2;
          n1  n2
 *)
 
let fold_offset f o t =
 assert (t <> Empty);
 let o, n, z = leftmost_child o End t in
 let rec aux_fold o t z pre =
   match t with
   | Empty -> assert false
   | Node (max, _, _, _, _, r, m, v) ->
       let abs_max = add max o in
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
 assert (t <> Empty);
 let o, n, z = leftmost_child o End t in
 let rec aux_iter o t z =
    match t with
   | Empty -> assert false
   | Node (max, _, _, _, _, r, m, v) ->
       begin
         let abs_max = add max o in
         f o abs_max r m v;
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

let rec pretty_offset s curr_off ppf tree =
      match tree with
      | Empty -> ()
      | Node (max, offl, subl, offr, subr, rem, modu, v) ->
          pretty_offset "" (add curr_off offl) ppf subl;
          Format.fprintf ppf "@[%s: [%a..%a] -> (%a, %a, %a);@]@;@ "
            s
            Int.pretty curr_off
            Int.pretty (add max curr_off)
            Int.pretty rem
            Int.pretty modu
            V.pretty v;
            pretty_offset "" (add curr_off offr) ppf subr;
 ;;


let pretty ppf = pretty_offset "0" Int.zero  ppf
 ;;

let pretty_offset = pretty_offset "0";;

let pretty_debug_offset curr_off ppf  tree =
  let rec aux_pdebug curr_off ppf  tree = 
    match tree with
    | Empty -> Format.fprintf ppf "empty" 
    | Node (max, offl, subl, offr, subr, rem, modu, v) ->
        Format.fprintf ppf "@[<h 2>@[[%a..%a]@ %a@ %a@ %a@]@\n@[<h 2>-- %a -->@\n%a@]@\n@[<h 2>-- %a -->@\n%a@]@]"
          Int.pretty curr_off  
          Int.pretty (add curr_off max)
          Int.pretty rem
          Int.pretty modu
          V.pretty v
          Int.pretty offl
          (aux_pdebug (add curr_off offl))  subl
          Int.pretty offr
          (aux_pdebug (add curr_off offr))  subr
   in
     aux_pdebug curr_off  ppf tree;
     Format.fprintf ppf "@\n";
;;

let pretty_debug ppf = pretty_debug_offset Int.zero ppf 
;;

let print_offset o t = pretty_debug_offset o Format.std_formatter t
 ;;

let fprint ppf t =
 iter (fun min max r m v -> pretty_node ppf  min max r m v) t;
 Format.fprintf ppf "@."; 
;;

let print t = 
 let ppf = Format.std_formatter in fprint ppf t
;;


      

(** Given interval [min, max], returns the subtree starting at this
    interval
    raises Interval_not_found (min, max) when failing
*)

exception Interval_not_found of Int.t * Int.t;;

let subtree_from_interval min max tree_offset tree =
  let rec aux_sfi tree_offset tree =
    match tree with
    | Empty -> raise (Interval_not_found(min, max))
    | Node (nmax, offl, subl, offr, subr, _, _, _) ->
        let abs_min = tree_offset
        and abs_max = add tree_offset nmax in
        if Int.equal min abs_min && Int.equal max abs_max
        then
          tree_offset, tree
        else
          if max > abs_max
          then
            aux_sfi (add tree_offset offr) subr
          else if min < abs_min
          then
            aux_sfi (add tree_offset offl) subl
          else raise (Interval_not_found(min, max))
  in aux_sfi tree_offset tree
;;
          

(** Smart constructor for nodes:
    glues the node being constructet to potential candidates if needed
    (i.e. leftmost node of right subtree
     and rightmost node of left subtree)
 *)
          
let make_node curr_off max offl subl offr subr rem modu v =
  let rem, modu = if V.is_isotropic v then (Int.zero, Int.one)
    else  (rem, modu) in
  let curr_vv = (rem, modu, v) in
    
  let max, offr, subr =
    try
      let offset, nr, zr =
        leftmost_child (add curr_off offr) End subr in 
      match nr with
      | Node (nmax, _, nsubl , noffr, nsubr, nrem, nmodu, nv) ->
          assert (nsubl = Empty);
          if equal_vv (nrem, nmodu, nv) curr_vv &&
             (Int.equal (Int.rem offset modu) rem)
          then
            begin
(*              assert (Int.equal (add offset (succ nmax)) Int.zero);*)
(*              *)
               let curr_offr, new_subr = rezip zr (add offset noffr) nsubr
              in
              let new_max = succ (add max nmax) in
              let new_offr = sub curr_offr curr_off
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
        rightmost_child (add curr_off offl) End subl in
      match nl with
      | Node (nmax, noffl, nsubl , _, noffr, nrem, nmodu, nv) ->
          assert (noffr = Empty);
          if equal_vv (nrem, nmodu, nv) curr_vv &&
              (Int.equal (Int.rem curr_off modu) rem)
          then (
             let new_curr_offl, new_subl = rezip zl (add offset noffl) nsubl in
             let lmax = succ (add max nmax) in
            let new_offl =
              sub new_curr_offl offset in
            let new_offr = succ (add offr nmax) in
            let new_coff = sub curr_off (succ nmax) in
              (*assert (new_coff = offset);*)
               new_coff, lmax, new_offl, new_subl, new_offr)
          else curr_off, max, offl, subl, offr
      |Empty -> assert false
    with Empty_tree -> curr_off, max, offl, subl, offr
  in
   curr_off,
  Node (max, offl, subl, offr, subr, rem, modu, v)
;;

(** Smart add node:
    Adds a node to the current tree and merges (new) consecutive intervals
    containing the same values
    The node is [min..max] rem, modu, v and
    the tree to which it is added is tree at offset curr_off
    Hypothesis: the tree is in canonical form w.r.t to having no
    mergeable intervals.
 *)
let add_node curr_off min max rem modu v tree =
    let rec aux_add curr_off tree =
    match tree with
    | Empty ->
        let sz = sub max min
        and o = min in
        make_node o sz Int.zero Empty (succ sz) Empty rem modu v
            
    | Node (nmax, noffl, nsubl, noffr, nsubr, nrem, nmodu, nv) ->
     let abs_min = curr_off 
     and abs_max = add nmax curr_off in
     if max < abs_min then
       begin
         if is_above min  max abs_min abs_max then
           let new_offr = Int.sub abs_min min in
           (*Format.printf "add to the left above@."; *)
           make_node min (sub max min) zero Empty
             new_offr tree rem modu v
         else
           begin
        (*     Format.printf "L@ co:%a@ t:%a@ [%a...%a]@.@."
               Int.pretty curr_off
               (pretty_offset curr_off) tree
               Int.pretty min Int.pretty max
               ; *)
             let new_curr_offl, new_node =
               aux_add (add curr_off noffl) nsubl
             in 
             let new_offl = sub new_curr_offl curr_off in
             make_node
               curr_off nmax new_offl new_node noffr nsubr nrem nmodu nv
           end
       end
     else
      begin
       if is_above min max abs_min abs_max then
         begin
          (* Format.printf "add to the right ABOVE@.";  *)
           let new_offl = sub abs_min min in
(*          Format.printf "1 %a %a@." Int.pretty  (sub max curr_off) 
            Int.pretty new_offl; *)
           let new_max = sub max min in
(*         Format.printf "add_node :[%a %a] o:%a t:%a@."
           Int.pretty min Int.pretty new_max
           Int.pretty new_offl pretty_debug tree;
 *)         
           make_node
             min new_max new_offl tree (succ new_max) Empty rem modu v
         end
         
       else
         begin
(*           Format.printf "add to the right Not ABOVE@."; *)
           let new_curr_offr, new_node =
             aux_add (add curr_off noffr) nsubr
           in
           let new_offr = sub new_curr_offr abs_min in
           make_node abs_min nmax noffl nsubl new_offr new_node nrem
             nmodu nv
         end
       end
         
    in aux_add curr_off tree
;;


(** Translation functions  *)

let to_list t =
  List.rev (fold (fun min max r m v y -> (min, max, r, m, v) :: y) t [])

let to_offsetmap _t = ()
  ;;

(** Checks that the tree at is sanely built  *)

let rec check curr_off tree =
  match tree with
  | Empty -> ()
  | Node (max, offl, subl, offr, subr, rem, modu, _v) ->
      assert (Int.compare zero rem <= 0);
      assert (Int.compare rem modu < 0);
      assert (subl <> Empty || Int.equal0 offl);
      assert (subr <> Empty || Int.equal offr (succ max));
      let abs_min = curr_off
      and abs_max = add curr_off max in
      let aux offset tree =
        match tree with
        | Empty -> ()
        | Node (nmax, _, _, _, _, _, _, _) ->
            let nabs_min = add curr_off offset in
            let nabs_max = add nmax nabs_min in
            assert (is_above abs_min abs_max nabs_min nabs_max)
      in aux offl subl; aux offr subr;
      check (add curr_off offl) subl;
      check (add curr_off offr) subr;
;;
                
      


(** Inclusion functions *)
 

(** Are the values of t1 included in those of t2
    t1 and t2 must cover exactly the same range
    (see the 2 first assertions)
    The offset is absolute
 *)

let nc_is _included_generic_exn v_is_included_exn o1 t1 o2 t2 =
  assert ((not (t1 <> Empty && t2 <> Empty)) || 
          let o1, n1, _ = rightmost_child o1 End t1
          and o2, n2, _ = rightmost_child o2 End t2 in
          Int.add o1 (get_max n1) = Int.add o2 (get_max n2));
  assert ( o1 = o2);

  
  (* Is n1 included in n2 ? *)
  let is_similar r1 m1 v1 r2 m2 v2 =
    if (r1 = r2 && m1 = m2) || V.is_isotropic v1 || V.is_isotropic v2
    then
      v_is_included_exn v1 v2
    else raise Abstract_interp.Is_not_included
        
  in let is_included_node_exn _min1 _max1 r1 m1 v1
      _min2 _max2 r2 m2 v2 mabs_min mabs_max =
    if V.is_isotropic v1 || V.is_isotropic v2 then
      v_is_included_exn v1 v2
    else if r1 =  r2 && m1 = m2 &&
            mabs_min % m1 = r1 &&
             (succ mabs_max) % m1 = r1 
    then
      v_is_included_exn v1 v2
    else raise Abstract_interp.Is_not_included
  in let rec aux_inc ((o1, t1, z1) as n1) ((o2, t2, z2) as n2) =
  if t1 = t2 then ()
    else
      match t1, t2 with
      | Empty, _
      | _, Empty -> assert false
      | Node (max1, _offl1, _subl1, _offr1, _subr1, r1, m1, v1),
          Node (max2, _offl2, _subl2, _offr2, _subr2, r2, m2, v2) ->
            let abs_min1 = o1
            and abs_min2 = o2
            and abs_max1 = add o1 max1
            and abs_max2 = add o2 max2 in
            
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
  let l1 = leftmost_child o1 End t1
  and l2 = leftmost_child o2 End t2 in
  aux_inc l1 l2
;;

let is_included_generic_exn v_is_included_exn o1 t1 o2 t2 =
  assert ((not (t1 <> Empty && t2 <> Empty)) || 
          let o1, n1, _ = rightmost_child o1 End t1
          and o2, n2, _ = rightmost_child o2 End t2 in
          Int.add o1 (get_max n1) = Int.add o2 (get_max n2));
  assert ( o1 = o2);
  Format.printf "Start is included@.";
  (* Is n1 included in n2 ? *)
  let is_similar r1 m1 v1 r2 m2 v2 =
    if (r1 = r2 && m1 = m2) || V.is_isotropic v1 || V.is_isotropic v2
    then
      v_is_included_exn v1 v2
    else raise Abstract_interp.Is_not_included
        
  in let is_included_node_exn amin1 amax1 r1 m1 v1
      amin2 amax2 r2 m2 v2 mabs_min mabs_max =
    if V.is_isotropic v1 || V.is_isotropic v2 then
      v_is_included_exn v1 v2
    else
      let max_test = if amax1 < amax2
          then (succ mabs_max) % m1 = r1
          else true 
      in
      let ok_min = (amin1 = amin2) || mabs_min % m1 = r1
      and ok_max = amax1 = amax2 || max_test
                in
      if r1 =  r2 && m1 = m2 && ok_min && ok_max             
      then
        v_is_included_exn v1 v2
    else raise Abstract_interp.Is_not_included

  in let rec node_included  o1 t1 o2 t2 =
    
(*         Format.printf "*nodeINC @.t1: %a@. t2: %a@."
         (pretty_offset o1) t1 (pretty_offset o2) t2 ;
 *)
    if t1 == t2 then ()
    else
      match t1, t2 with
      | Empty, _ -> ()
      | _, Empty -> () 
      | Node (max1, offl1, subl1, offr1, subr1, r1, m1, v1),
          Node (max2, offl2, subl2, offr2, subr2, r2, m2, v2) ->
            let amin1 = o1
            and amax1 = add max1 o1
            and amin2 = o2
            and amax2 = add max2 o2
            and ol1 = add o1 offl1
            and ol2 = add o2 offl2
            and or1 = add o1 offr1
            and or2 = add o2 offr2
            in
             
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


let is_included o1 t1 o2 t2 =
 try
  is_included_generic_exn V.is_included_exn o1 t1 o2 t2;
  true
 with Abstract_interp.Is_not_included -> false
 ;;
 
(** Joins two trees with no overlapping intervals  *)
let rec union t1_curr_off t1 t2_curr_off t2 =
  match t1, t2 with
  | Empty, Empty ->
      assert (t1_curr_off = t2_curr_off);
      t1_curr_off, Empty
  | Empty, Node _ -> t2_curr_off, t2
  | Node _, Empty -> t1_curr_off, t1
  | Node (lmax, loffl, lsubl, loffr, lsubr, lrem, lmodu, lv),
      Node (rmax, roffl, rsubl, roffr, rsubr, rrem, rmodu, rv) ->
        let labs_min = t1_curr_off
        and labs_max = add lmax t1_curr_off
        and rabs_min = t2_curr_off
        and rabs_max = add rmax t2_curr_off in
        if is_above labs_min labs_max rabs_min rabs_max
        then
          (* t2 is on the right of t1 *)
          let new_curr_offr, new_subr =
            union (add t1_curr_off loffr) lsubr
              t2_curr_off t2
          in 
          make_node t1_curr_off lmax loffl lsubl
            (sub new_curr_offr t1_curr_off) new_subr lrem lmodu lv
        else
          begin
            (* t1 is on the left of t2 *)
(*            assert (is_above rabs_min rabs_max labs_min labs_max); *)
            let new_curr_offl, new_subl =
              union t1_curr_off t1
                (add t2_curr_off roffl) rsubl in
            make_node t2_curr_off rmax
              (sub new_curr_offl t2_curr_off) new_subl roffr rsubr
              rrem rmodu rv
          end
;;

(* (\** Prerequisites: min >= abs_min and max <= abs_max *\) *)
(* (\** This is enforced in the code by an assertion  *\)       *)
(* let change_binding curr_off min max rem modu v node = *)
(*   match node with *)
(*   | Empty -> *)
(*       let new_max = sub max curr_off in *)
(*       curr_off, *)
(*        Node(new_max , zero, Empty, succ new_max, Empty, rem, modu, v) *)
(*   |  Node (nmax, noffl, nsubl, noffr, nsubr, _nrem, _nmodu, _nv ) -> *)
(*       begin *)
(*         let abs_max = add curr_off nmax *)
(*         and abs_min = curr_off in *)
(*         begin *)
(*           assert ((min >= abs_min) && (max <= abs_max));         *)
(*           match (min = abs_min), (max = abs_max) with *)
(*           | true, true -> *)
(*               make_node *)
(*                 curr_off (sub max curr_off) *)
(*                  noffl nsubl noffr nsubr rem modu v  *)
(*           | true, false *)
              
(*           | false, true *)
(*           | false, false -> *)
(*               assert false *)
(*         end *)
(*       end *)
(* ;; *)


(** Merge two trees of same number of nodes*)

let rec merge f o1 t1 o2 t2 lopt =
(*  Format.printf "MERGE1:%a@." (pretty_offset o1) t1;
  Format.printf "MERGE2:%a@." (pretty_offset o2) t2;
 *)
  match t1, t2 with
  | _, _  when (t1 == t2 && o1 = o2) ->  o1, t1
  | Empty, Empty -> assert false
  | Node _, Empty -> assert false
  | Empty, Node _ -> assert false
  | Node (max1, offl1, subl1, offr1, subr1, rem1, modu1, v1),
      Node (max2, offl2, subl2, offr2, subr2, rem2, modu2, v2) ->
        let abs_min1 = o1
        and abs_max1 = add max1 o1
        and abs_min2 = o2
        and abs_max2 = add max2 o2 in
        if abs_min2 > abs_max1 then
          if is_above abs_min1 abs_max1 abs_min2 abs_max2
          then (* t2 is on the right of t1 *)
            let off, t = merge f (add o1 offr1) subr1 o2 t2 lopt in
            make_node o1 max1 offl1 subl1
              (sub off o1) t rem1 modu1 v1
         else(* t1 is on the left of t2 *)
            begin
             (* Format.printf "t2:[%a %a] %a @.t1:[%a %a] %a@." Int.pretty
                abs_min2 Int.pretty abs_max2 (pretty_debug_offset o2) t2
                Int.pretty abs_min1
                Int.pretty abs_max1 (pretty_debug_offset o1) t1; *)
            (*  assert (is_above abs_min2 abs_max2 abs_min1 abs_max1);  *)
              let off, t = merge f o1 t1 (add o2 offl2) subl2 lopt in
              make_node o2 max2 (sub off o2) t offr2 subr2 rem2 modu2
              v2
            end
        else if abs_min1 > abs_max2 then
          if is_above abs_min1 abs_max1 abs_min2 abs_max2
          then
            (* t2 is on the left of t1 *)
            let off, t = merge f (add o1 offl1) subl1 o2 t2 lopt in
            make_node o1 max1 (sub off o1) t offr1 subl1
              rem1 modu1 v1
          else
            begin
            assert (is_above abs_min2 abs_max2 abs_min1 abs_max1);
              (* t1 is on the right of t2 *)
            let off, t = merge f o1 t1 (add o2 offr2) subr2 lopt in
            make_node o2 max2 offl2 subl2 (sub off o2) t
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
            let abs_offl1 = add o1 offl1
            and abs_offl2 = add o2 offl2 in
            if abs_min1 = abs_min2  then
              merge f abs_offl1 subl1 abs_offl2 subl2 lopt, abs_min1
            else if abs_min1 < abs_min2 then
               let new_offl1, new_subl1 =
                 add_node abs_offl1 abs_min1 (pred abs_min2)
                  rem1 modu1 v1 subl1 
               in merge f new_offl1 new_subl1 abs_offl2 subl2 lopt
                  , abs_min2
            else
              begin
                assert (abs_min1 > abs_min2);
                let new_offl2, new_subl2 =
                  add_node abs_offl2 abs_min2 (pred abs_min1) rem2 modu2
                    v2 subl2
                in merge f abs_offl1 subl1 new_offl2 new_subl2 lopt
                   , abs_min1
              end
          in
          let (curr_offr, right_t), middle_abs_max =
            let abs_offr1 = add o1 offr1
            and abs_offr2 = add o2 offr2 in
            if abs_max1 = abs_max2 then
              merge f abs_offr1 subr1
                abs_offr2 subr2 lopt, abs_max1
            else if abs_max1 < abs_max2 then
              let new_offr2, new_subr2 =
                add_node abs_offr2
                  (succ abs_max1) abs_max2 rem2 modu2 v2 subr2 in
              (*Format.printf "HERE3:%a:%a@." Int.pretty new_offr2 pretty new_subr2;*)
              merge f abs_offr1 subr1 new_offr2 new_subr2 lopt, 
                abs_max1
            else
              begin
                assert (abs_max1 > abs_max2);
                let min = (succ abs_max2) in
                let new_offr1, new_subr1 =
                 add_node abs_offr1 min  abs_max1 rem1 modu1 v1 subr1 in
                (*Format.printf "HERE4:%a:%a@." Int.pretty new_offr1 pretty new_subr1;*)
                merge f new_offr1 new_subr1 abs_offr2 subr2 lopt, abs_max2
              end
          in
          
          let rem, modu, v, _l = f middle_abs_min middle_abs_max
                               rem1 modu1 v1 rem2 modu2 v2 lopt
          in
          let curr_offl, left_t =
            add_node curr_offl middle_abs_min middle_abs_max rem modu
              v left_t
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
      V.anisotropic_cast (to_big_int size)
      (*(V.topify_merge_origin *)(V.join v1 v2)
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


(*
let widen _hint t1 t2 =
  let f_widen _abs_min _abs_max rem1 modu1 v1 rem2 modu2 v2 =
    if (rem1 = rem2 && modu1 = modu2)
      || V.is_isotropic v2
    then rem1, modu1, V.widen v1
    else rem1, modu1, V.widen v1
  in merge f_widen Int.zero t1 Int.zero t2
;;
 *) 
(** Changes a binding : the [min, max] interval already exists *)
(** In particular it is not allowed to add the binding before the*)
(** lowest abs_min or after the greates abs_max*)
let change_binding min max rem modu v tree =
  let rec aux_add curr_off zipper min max rem modu v tree =
    match tree with
    | Empty ->
        rezip zipper curr_off
         (Node(sub max curr_off, zero, Empty, succ max, Empty, rem, modu, v))
    | Node (nmax, noffr, nsubr, noffl, nsubl, _nrem, _nmodu, _nv ) ->
        let abs_max = add nmax curr_off
        and abs_min = curr_off in
        if max < abs_min then
          (* our given interval is on the left of the current node *)
          let new_offset = add noffl curr_off in
          aux_add
            new_offset
            (Left (curr_off, tree, zipper))
            min max rem modu v nsubl
        else if min > abs_max then
          (* our given interval is on the right of the current node *)
          let new_offset = add noffr curr_off in
          aux_add
            new_offset
            (Right (curr_off, tree, zipper)) 
            min max rem modu v nsubr
        else
          (* we've got work to do *)
          assert false
(*           let (curr_offl, left_t), middle_abs_min = *)
(*             if min = abs_min  then *)
(*               min *)
(*             else if abs_min < min then *)
(*               min *)
(*             else *)
(*               abs_min *)
(*           in *)
(*           let (curr_offr, right_t), middle_abs_max = *)
(*             if abs_max1 = abs_max2 then *)
(*               merge f (add t1_off offr1) subr1 *)
(*                 (add t2_off offr2) subr2, abs_max1 *)
(*             else if abs_max1 < abs_max2 then *)
(*               add_node (add t2_off offr2) *)
(*                 (succ abs_max1) abs_max2 rem2 modu2 v2 subr2, abs_max1 *)
(*             else add_node (add t1_off offr1) (succ abs_max2) abs_max1 *)
(*                 rem1 modu1 v1 subr1, abs_max2 *)
(*           in *)
          
(*           let rem, modu, v = f middle_abs_min middle_abs_max *)
(*               rem1 modu1 v1 rem2 modu2 v2 *)
(*           in *)
(*           let curr_offl, left_t = *)
(*             add_node curr_offl middle_abs_min middle_abs_max rem modu *)
(*               v left_t *)
(*           in rezip zipper (union curr_offl left_t curr_offr right_t) *)
            
  in
  aux_add Int.zero End min max rem modu v tree
    
(*let add_binding min max rem modu v tree =
  let rec aux_add curr_off zipper min max rem modu v tree =
    match tree with
    | Empty ->
        rezip zipper
          Node(sub max curr_off, zero, Empty, zero, Empty, rem, modu, v)
    | Node (nmax, noffr, nsubr, noffl, nsubr, nrem, nmodu, nv ) ->
        let abs_max = add nmax curr_off
        and abs_min = curr_off in
        if max < abs_min then
          let new_offset = add noffl curr_off in
          aux_add
            new_offset
            (Left (curr_off, tree, zipper))
            min max rem modu v nsubl
        else if min > abs_max then
          let new_offset = add noffr curr_off in
          aux_add
            new_offset
            (Right (curr_off, tree, zipper)) 
            min max rem modu v nsubr
        else
*)

(*let get_values min max tree =
  let extract_from_interval imin imax = ()
  in
  let rec aux_gv curr_off tree =
  match tree with
  | Empty -> raise Not_found
  | Node (nmax, noffl, nsubl, noffr, nsubr, nrem, nmodu, nv) ->
      let abs_min = curr_off
      and abs_max = add nmax curr_off in
      if min > abs_max then
        aux_gv (add curr_off noffr) min max nsubr
      else if max < abs_min then
        aux_gv (add curr_off noffl) min max nsubl
      else 
        let x = extract_from_interval abs_min abs_max min max in
          if (min >= abs_min) and (max <= abs_max) then x
          else 
            let l = (aux_gv
        
*)        

end

(*
module Test =  Make (Cvalue_type.V) ;;

open Cvalue_type.V
open Int
open Test

;;

let ppf = Format.std_formatter;;
 
let t = Empty;;

Random.init 42;;

let make_random_tree n ub =
  let gen_symb =
    let i = ref 0 in
    fun () ->
      incr i;
      "a"^(string_of_int !i)
  in
  let new_base () =
    Base.create_string (gen_symb ())
  in
  let make_interval lb ub =
    let max = Int.sub (succ ub) lb in
     Random.int64 max
  in let valeurs =
    [Ival.singleton_zero; Ival.zero_or_one; Ival.zero; Ival.one] in
  let bases = (new_base ()) :: [new_base ()] 
  and maxmod = 64L
  and maxrem = 6L
  and len = List.length valeurs
  in let nlist = List.length bases
  in let make_value () =
    let base = List.nth bases (Random.int nlist) in
    let v = inject base (List.nth valeurs (Random.int len))
    and r = Random.int64 maxrem
    and m = succ (Random.int64 maxmod)
    in r, m, v
  in let rec node lb i t =
    let r, m, v = make_value () in
    if i = 0 then
      snd (add_node 0L lb ub r m v t)
    else
      begin
        assert (compare lb 0L >= 0);
        let ub1 = add lb (make_interval lb ub)
        in
      (*  Format.printf "[%Ld...%Ld]" lb ub1;*)
        let _, t1 = add_node 0L lb ub1 r m v t in
        let new_lb = succ ub1 
        in
        if compare new_lb ub > 0 then t1 else node new_lb (i - 1) t1;
      end;
  in node 0L n Empty
;;

let _, t1 = add_node 0L 0L 31L 0L 32L singleton_zero t;;

let _, t2 = add_node 0L 32L 63L  0L 32L singleton_zero t1;;

let _, t3 = add_node 0L 64L 88L 0L 32L zero_or_one t2;;

let _, t4 = add_node 0L 89L 101L 0L 32L zero_or_one t3;;

let _, t5 = add_node 0L 102L 104L 0L 32L zero_or_one t4;;

let d6, t6 = add_node 0L (-32L) (-1L) 0L 32L singleton_zero
    t5;;


pretty_debug ppf t6;;                                           
Test.pretty ppf t6;;

(*let offset, _node, zipper = leftmost_child 0L End t6;;*)

(* node coff max offl subl offr subr rem modu v *)

let c6, u6 = make_node (-32L) 31L 0L t 32L t 0L 32L singleton_zero;;

let c5, u5 = make_node 102L 2L  0L t 3L t 0L 32L zero_or_one;;

let c4, u4 = make_node 89L 12L 0L t (sub c5 89L) u5 0L 32L zero_or_one;;

let c3, u3 = make_node 32L 31L 0L t 32L t 0L 32L singleton_zero;;

let c2, u2 = make_node 64L 24L (sub c3 64L) u3 (sub c4 64L) u4 0L 32L zero_or_one ;;

let c1, u1 = make_node 0L 31L  (sub c6 0L) u6 (sub c2 0L) u2 0L 32L singleton_zero;;

List.iter2 check [c1;c2;c3;c4;c5;c6] [u1;u2;u3;u4;u5;u6];;
Test.equal u1 t6;;

pretty_debug ppf u1;;                                           
Test.pretty ppf u1;;
flush_all ();;


let base_a = Base.create_string "a"
and base_b = Base.create_string "b"
;;

let val1 = inject base_a Ival.singleton_zero
and val2 = inject base_b Ival.singleton_zero
;;

let at_off, at = add_node  0L 0L 31L 0L 32L val1 t;;
let ca, at2 = add_node 0L 32L 80L 0L 48L val1 at;;

let bt_off, bt = add_node  0L 0L 31L 0L 32L val2 t;;
let _, bt2 = add_node 0L 32L 64L 0L 32L val2 bt;;
let _, bt3 = add_node 0L 65L 79L 0L 1L singleton_zero bt2;;

(*let x = print bt3;;

let l = to_list bt3;;

*)
    (*
let a = ref Empty;;
let aa = ref Old.empty;;
let b = ref Empty;;
let bb = ref Old.empty;;
let c = ref 0L;;

for i = 0 to 79 do
  let offs2 x = Int.add x (Int.of_int (81*i)) in
  let boffs2 x = Bint.of_int64 (offs2 x) in
    a:=snd( add_node 0L (offs2 0L)
      (offs2 31L) 0L 32L val1 !a);
  aa := Old.add_internal (boffs2 0L, boffs2 31L) (Bint.zero, Bint.of_int64 32L, val1) !aa;

   a:= snd (add_node 0L (offs2 32L) (offs2 80L) 0L 48L val1 !a);
    aa := Old.add_internal (boffs2 32L, boffs2 80L) (Bint.zero, Bint.of_int64 48L, val1) !aa;
done;;

for i = 0 to 80 do
  let offs x = Int.add x (Int.of_int (80*i)) in
  let boffs x = Bint.of_int64 (offs x) in
   b:= snd (add_node 0L (offs 0L) (offs 31L) 0L 32L val2 !b);
  bb:= Old.add_internal (boffs 0L, boffs 31L) (Bint.zero, Bint.of_int64 32L, val2) !bb;

    b:= snd (add_node 0L (offs 32L) (offs 64L) 0L 16L val2 !b);
    bb:= Old.add_internal (boffs 32L, boffs 64L) (Bint.zero, Bint.of_int64 16L, val2) !bb;

   b:= snd (add_node 0L (offs 65L) (offs 79L) 0L 1L singleton_zero
              !b);
    bb:= Old.add_internal (boffs 65L, boffs 79L) (Bint.zero, Bint.of_int64 1L, singleton_zero) !bb;
done
;;


(* (\* let off, new_t = join at2 bt3;; *\) *)
(* (\* Format.printf "%a@.%a@.%a@." pretty at2 pretty bt3 pretty new_t;; *\) *)
(* Format.printf "a---@.%a@.aa--@.%a@." pretty !a Old.pretty_compare !aa *)
(* ;; *)
(* Format.printf "b---@.%a@.bb--@.%a@." pretty !b Old.pretty_compare !bb *)
(* ;; *)

assert (Test.equal !a (translate_from_old !aa));;
assert (Test.equal !b (translate_from_old !bb));;

 let noff, nt = join (translate_from_old !aa)(translate_from_old !bb) ;; 
 let  ot = translate_from_old (snd (Old.join !aa !bb));;
if (Test.equal nt ot) then Format.printf "Join is ok@.";;
*)
(* Format.printf "b---@.%a@.bb--@.%a@." pretty nt Old.pretty_compare ot *)
(* ;; *)

(* print2 nt;; *)
(*
Format.printf "%a@.%a@.%a@." pretty !a pretty !b pretty nt
;;*)
(* let b = ref bt3;; *)
(* let i = 1 ;; *)
(*   let offs x = Int.add x (Int.of_int (80*i));; *)
(*    b:= snd (add_node 0L (offs 0L) (offs 31L) 0L 32L val2 !b);; *)
(*    b:= snd (add_node 0L (offs 32L) (offs 64L) 0L 32L val2 !b);; *)
(*    b:= snd (add_node 0L (offs 65L) (offs 79L) 0L 1L singleton_zero *)
(*               !b);; *)


(* let i = 2 ;; *)
(*   let offs x = Int.add x (Int.of_int (80*i));; *)
(*    b:= snd (add_node 0L (offs 0L) (offs 31L) 0L 32L val2 !b);; *)

(* let before = !b;; *)
(* (\*b:= snd (add_node 0L (offs 32L) (offs 64L) 0L 32L val2 !b);;*\) *)

(* (\*Debugging purposes *\) *)
(* let coff, tr = subtree_from_interval 160L 191L 0L before;;  *)

(* let c, t = make_node 0L 144L 0L Empty  160L tr  0L 32L val2;;  *)
(* print_offset c t;; *)

(* let c, t = make_node 192L 32L (sub c 192L) t 32L Empty  0L 32L val2;; *)
(* print_offset c t;; *)

let ub = 819488344203502342L
and n = 900;;

print_string "Random generation of trees\n";;
let tr = make_random_tree n ub;;
print_string "TR\n";;
print tr;;

print_string "TRR\n";;
let trr = make_random_tree n ub;;
print trr;;

let ok b =  if b then print_string "ok\n" else print_string "ko\n";;

let _, trrr = join tr trr;;print_string "TRRRR\n";;
print trrr;;
let a = is_included trr trrr ;; 
ok a;;

let b = is_included tr trr;; 
ok b;;

*)
(*
Local Variables:
compile-command: "gmake -C ../..  src/memory_state/new_offsetmap.cmo " 
End:
*)
