(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Objective Caml                       *)
(*                                                                        *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright (C) 1996 INRIA                                              *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This file is distributed under the terms of the GNU Library General   *)
(*  Public License version 2, with the special exception on linking       *)
(*  described below. See the GNU Library General Public License version   *)
(*  2 for more details (enclosed in the file licenses/LGPLv2).            *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License.  By "a publicly distributed version   *)
(*  of the Library", we mean either the unmodified Library as             *)
(*  distributed by INRIA, or a modified version of the Library that is    *)
(*  distributed under the conditions defined in clause 2 of the GNU       *)
(*  Library General Public License.  This exception does not however      *)
(*  invalidate any other reasons why the executable file might be         *)
(*  covered by the GNU Library General Public License.                    *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

type fuzzy_order = Above | Below | Match

module type S = sig
  type key
  type value

  type rangemap

  include Datatype.S with type t = rangemap

  val create :  t -> key -> value -> t -> t
  val empty: t
  val is_empty: t -> bool
  val add: key -> value -> t -> t
  val singleton: key -> value -> t
  val find: key -> t -> value
  val remove: key -> t -> t
  val mem: key -> t -> bool
  val iter: (key -> value -> unit) -> t -> unit
  val map: (value -> value) -> t -> t
  val mapi: (key -> value -> value) -> t -> t
  val mapii: (key -> value -> key*value) -> t -> t
  val fold: (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (key -> value -> bool) -> t -> bool
  val exists: (key -> value -> bool) -> t -> bool
  val filter: (key -> value -> bool) -> t -> t
  val partition: (key -> value -> bool) -> t -> t * t
  val cardinal: t -> int
  val bindings: t -> (key * value) list
  val min_binding: t -> (key * value)
  val max_binding: t -> (key * value)
  val choose: t -> (key * value)
  val merge: (key -> value option -> value option -> value option) -> t -> t -> t
  val for_all2: (key -> value option -> value option -> bool) -> t -> t -> bool
  val exists2: (key -> value option -> value option -> bool) -> t -> t -> bool
  val iter2: (key -> value option -> value option -> unit) -> t -> t -> unit
  val fold2: (key -> value option -> value option -> 'a -> 'a) -> t -> t -> 'a -> 'a
end


module type Value = sig
  include Datatype.S
  val fast_equal: t -> t -> bool
end

module Make(Ord: Datatype.S)(Value: Value) = struct

  type key = Ord.t
  type value = Value.t

  type rangemap =
    | Empty
    | Node of rangemap * key * Value.t * rangemap * int * int
    (* the last two are height and hash in this order *)

  let height = function
    | Empty -> 0
    | Node(_,_,_,_,h,_) -> h

  let hash = function
    | Empty -> 0
    | Node(_,_,_,_,_,h) -> h

  let create l x d r =
    let x_h = Ord.hash x in
    let d_h = Value.hash d in
    let hl = height l and hr = height r in
    let hashl = hash l and hashr = hash r in
    let hashbinding = 31 * x_h + d_h in
    let hashtree = hashl lxor hashbinding lxor hashr in
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1), hashtree)

   let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h,_) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h,_) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Rangemap.bal"
        | Node(ll, lv, ld, lr, _, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Rangemap.bal"
              | Node(lrl, lrv, lrd, lrr, _, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Rangemap.bal"
        | Node(rl, rv, rd, rr, _, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Rangemap.bal"
              | Node(rll, rlv, rld, rlr, _, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        create l x d r

   let empty = Empty

   let is_empty = function Empty -> true | _ -> false

   let singleton x v = create Empty x v Empty

   let rec add x data = function
        Empty ->
          create Empty x data Empty
      | Node(l, v, d, r, _, _) as node ->
          let c = Ord.compare x v in
          if c = 0 then
            if Value.fast_equal d data then node
            else create l x data r
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

  let rec find x = function
    | Empty ->
      raise Not_found
    | Node(l, v, d, r, _, _) ->
      let c = Ord.compare x v in
      if c = 0 then d
      else find x (if c < 0 then l else r)

  let rec mem x = function
    | Empty ->
      false
    | Node(l, v, _d, r, _, _) ->
      let c = Ord.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec max_binding = function
    | Empty -> raise Not_found
    | Node(_l, x, d, Empty, _, _) -> (x, d)
    | Node(_l, _x, _d, r, _, _) -> max_binding r

  let rec min_binding = function
    | Empty -> raise Not_found
    | Node(Empty, x, d, _r, _, _) -> (x, d)
    | Node(l, _x, _d, _r, _, _) -> min_binding l

  let choose = min_binding

  let rec remove_min_binding = function
    | Empty -> invalid_arg "Rangemap.remove_min_elt"
    | Node(Empty, _x, _d, r, _, _) -> r
    | Node(l, x, d, r, _, _) -> bal (remove_min_binding l) x d r

  let merge t1 t2 = match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let (x, d) = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

  let rec remove x = function
    | Empty ->
      Empty
    | Node(l, v, d, r, _, _h) ->
      let c = Ord.compare x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        bal (remove x l) v d r
      else
        bal l v d (remove x r)

  let rec iter f = function
    | Empty -> ()
    | Node(l, v, d, r, _, _) ->
      iter f l; f v d; iter f r

  let rec map f = function
    | Empty               -> Empty
    | Node(l, v, d, r, _, _h) -> create (map f l) v (f d) (map f r)

  let rec mapi f = function
    | Empty               -> Empty
    | Node(l, v, d, r, _, _h) -> create (mapi f l) v (f v d) (mapi f r)

  let rec mapii f = function
    | Empty -> Empty
    | Node(l, v, d, r, _, _) ->
      let new_v, new_d = f v d in
      create (mapii f l) new_v new_d (mapii f r)

  let rec fold f m accu = match m with
    | Empty -> accu
    | Node(l, v, d, r, _, _) ->
      fold f r (f v d (fold f l accu))

  let rec for_all p = function
      Empty -> true
    | Node(l, v, d, r, _, _) -> p v d && for_all p l && for_all p r

  let rec exists p = function
      Empty -> false
    | Node(l, v, d, r, _, _) -> p v d || exists p l || exists p r

  let filter p s =
    let rec filt accu = function
      | Empty -> accu
      | Node(l, v, d, r, _, _) ->
          filt (filt (if p v d then add v d accu else accu) l) r in
    filt Empty s

  let partition p s =
    let rec part (t, f as accu) = function
      | Empty -> accu
      | Node(l, v, d, r, _, _) ->
          part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
    part (Empty, Empty) s

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add v d r
      | (_, Empty) -> add v d l
      | (Node(ll, lv, ld, lr, lh, _), Node(rl, rv, rd, rr, rh, _)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, d, r, _, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node (l1, v1, d1, r1, h1, _), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node (l2, v2, d2, r2, _h2, _)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

  type enumeration = End | More of key * Value.t * rangemap * enumeration

  let rec cons_enum m e = match m with
    | Empty -> e
    | Node(l, v, d, r, _, _) -> cons_enum l (More(v, d, r, e))

  let compare m1 m2 =
    let rec compare_aux e1 e2 = match (e1, e2) with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        let c = Ord.compare v1 v2 in
        if c <> 0 then c else
          let c = Value.compare d1 d2 in
          if c <> 0 then c else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    compare_aux (cons_enum m1 End) (cons_enum m2 End)

  let equal m1 m2 =
    let rec equal_aux e1 e2 = match (e1, e2) with
      | (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        Ord.equal v1 v2 && Value.equal d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let fold2 f m1 m2 r =
    let rec aux e1 e2 r = match e1, e2 with
      | (End, End) -> r
      | (End, More (k, v, t, e)) ->
          f k None (Some v) (aux End (cons_enum t e) r)
      | (More (k, v, t, e), End) ->
          f k (Some v) None (aux (cons_enum t e) End r)
      | (More (k1, v1, t1, e1'), More (k2, v2, t2, e2')) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then
            f k1 (Some v1) (Some v2)
              (aux (cons_enum t1 e1') (cons_enum t2 e2') r)
          else if c < 0 then
            f k1 (Some v1) None
              (aux (cons_enum t1 e1') e2 r)
          else
            f k2 (Some v2) None
              (aux e1 (cons_enum t2 e2') r)
      in aux (cons_enum m1 End) (cons_enum m2 End) r

  (* iter2, exists2 and for_all2 are essentially the same implementation
     as fold2 with the appropriate default value and operator, but
     we cannot use fold, as ";", "||" and "&&" are lazy... *)
  let iter2 f m1 m2 =
    let rec aux e1 e2 = match e1, e2 with
      | (End, End) -> ()
      | (End, More (k, v, t, e)) ->
          f k None (Some v); aux End (cons_enum t e)
      | (More (k, v, t, e), End) ->
          f k (Some v) None; aux (cons_enum t e) End
      | (More (k1, v1, t1, e1'), More (k2, v2, t2, e2')) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then (
            f k1 (Some v1) (Some v2);
            aux (cons_enum t1 e1') (cons_enum t2 e2')
          ) else if c < 0 then (
            f k1 (Some v1) None;
            aux (cons_enum t1 e1') e2
          ) else (
            f k2 (Some v2) None;
            aux e1 (cons_enum t2 e2')
          )
    in aux (cons_enum m1 End) (cons_enum m2 End)

  let exists2 f m1 m2 =
    let rec aux e1 e2 = match e1, e2 with
      | (End, End) -> false
      | (End, More (k, v, t, e)) ->
          f k None (Some v) || aux End (cons_enum t e)
      | (More (k, v, t, e), End) ->
          f k (Some v) None || aux (cons_enum t e) End
      | (More (k1, v1, t1, e1'), More (k2, v2, t2, e2')) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then
            f k1 (Some v1) (Some v2) ||
            aux (cons_enum t1 e1') (cons_enum t2 e2')
          else if c < 0 then
            f k1 (Some v1) None ||
            aux (cons_enum t1 e1') e2
          else
            f k2 (Some v2) None ||
            aux e1 (cons_enum t2 e2')
    in aux (cons_enum m1 End) (cons_enum m2 End)


  let for_all2 f m1 m2 =
    let rec aux e1 e2 = match e1, e2 with
      | (End, End) -> true
      | (End, More (k, v, t, e)) ->
          f k None (Some v) && aux End (cons_enum t e)
      | (More (k, v, t, e), End) ->
          f k (Some v) None && aux (cons_enum t e) End
      | (More (k1, v1, t1, e1'), More (k2, v2, t2, e2')) ->
          let c = Ord.compare k1 k2 in
          if c = 0 then
            f k1 (Some v1) (Some v2) &&
            aux (cons_enum t1 e1') (cons_enum t2 e2')
          else if c < 0 then
            f k1 (Some v1) None &&
            aux (cons_enum t1 e1') e2
          else
            f k2 (Some v2) None &&
            aux e1 (cons_enum t2 e2')
    in aux (cons_enum m1 End) (cons_enum m2 End)


  let rec cardinal = function
    | Empty -> 0
    | Node(l, _, _, r, _, _) -> cardinal l + 1 + cardinal r

  let rec bindings_aux accu = function
    | Empty -> accu
    | Node(l, v, d, r, _, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

  let bindings s =
    bindings_aux [] s




  let rec fold_range o f m accu = match m with
    | Empty -> accu
    | Node(l, v, d, r, _, _) ->
      let compar = o v in
      let accu1 = match compar with
        | Match | Above -> fold_range o f l accu
        | Below -> accu
      in
      let accu2 = match compar with
        | Match -> f v d accu1
        | Above | Below -> accu1
      in
      match compar with
      | Match | Below -> fold_range o f r accu2
      | Above -> accu2

  let cons k v l = (k,v) :: l

  let concerned_intervals fuzzy_order i m =
    fold_range (fuzzy_order i) cons m []

  let remove_whole fuzzy_order i m =
    fold_range (fuzzy_order i) (fun k _v acc -> remove k acc) m m

  let add_whole fuzzy_order i v m =
    let removed = remove_whole fuzzy_order i m in
    add i v removed

  exception Empty_rangemap


  (* This is actually a copy of [min_binding], but raises [Empty_rangemap]
     instead of [Not_found]... *)
  let rec lowest_binding m = match m with
    | Node(Empty,k,v,_,_, _) -> k,v
    | Node(t,_,_,_,_, _) -> lowest_binding t
    | Empty -> raise Empty_rangemap

  exception No_such_binding

  let rec lowest_binding_above o m = match m with
    | Node(l,k,v,r,_, _) ->
      if o k
      then begin
        try
          lowest_binding_above o l
        with No_such_binding -> k,v
      end
      else lowest_binding_above o r
    | Empty -> raise No_such_binding





  include Datatype.Make
  (struct
    type t = rangemap
    let name = "(" ^ Ord.name ^ ", " ^ Value.name ^ ") rangemap"
    open Structural_descr
    let r = Recursive.create ()
    let structural_descr =
      t_sum
        [| [| recursive_pack r;
              Ord.packed_descr;
              Value.packed_descr;
              recursive_pack r;
              p_int;
              p_int |] |]
    let () = Recursive.update r structural_descr
    let reprs =
      List.fold_left
        (fun acc k ->
          List.fold_left
            (fun acc v -> (Node(Empty, k, v, Empty, 0, 0)) :: acc)
            acc
            Value.reprs)
        [ Empty ]
        Ord.reprs
    let equal = equal
    let compare = compare
    let hash = hash
    let rehash = Datatype.identity
    let copy =
      if Ord.copy == Datatype.undefined || Value.copy == Datatype.undefined
      then Datatype.undefined
      else
        let rec aux = 
          function
            | Empty -> Empty
            | Node (l,x,d,r,_,_) ->
              let l = aux l in
              let x = Ord.copy x in
              let d = Value.copy d in
              let r = aux r in
              create l x d r
        in aux

    let internal_pretty_code = Datatype.undefined
    let pretty = Datatype.undefined
    let varname = Datatype.undefined
    let mem_project =
      if Ord.mem_project == Datatype.never_any_project &&
        Value.mem_project == Datatype.never_any_project then
          Datatype.never_any_project
      else
        (fun s -> exists (fun k v -> Ord.mem_project s k || Value.mem_project s v))
   end)
  let () = Type.set_ml_name ty None

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
