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

module V = Int

module Interval = Datatype.Pair(V)(V)
type itv = Interval.t


let plevel = ref 200

module Unhashconsed_Int_Intervals = struct

  exception Error_Top

  type tt = Top | Set of itv list

  let bottom = Set []
  let top = Top

  let _check t =
    assert (
      match t with
        | Top -> true
        | Set s ->
            let last_stop = ref None in
            List.for_all
              (fun (a,b) -> V.compare a b <= 0 &&
                 match !last_stop with
                     None -> last_stop := Some b; true
                   | Some l -> last_stop := Some b; V.gt a l)
              s) ;
    t

  let hash l = match l with
    | Top -> 667
    | Set l ->
        List.fold_left
          (fun acc p -> 371 * acc + Interval.hash p)
          443
          l

(*
  let cardinal_zero_or_one v =
    match v with
      | Top -> false
      | Set [x,y] -> V.equal x y
      | Set _ -> false

  let cardinal_less_than v n =
    match v with
      | Top -> raise Not_less_than
      | Set l ->
	  let nn = V.of_int n in
          let rec aux l card = match l with
            | [] -> card
            | (x,y)::t ->
                let card = V.add card (V.length x y) in
                if V.gt card nn
                then raise Not_less_than
                else aux t card
          in
          V.to_int (aux l V.zero)
*)

  let compare e1 e2 =
    if e1 == e2 then 0
    else
      match e1,e2 with
        | Top,_ -> 1
        | _, Top -> -1
        | Set e1, Set e2 ->
            Extlib.list_compare Interval.compare e1 e2

  let equal e1 e2 = compare e1 e2 = 0

  let pretty fmt t =
    match t with
      | Top -> Format.fprintf fmt "TopISet"
      | Set s ->
          if s==[] then Format.fprintf fmt "BottomISet"
          else
            Pretty_utils.pp_iter
              ~pre:"@[<hov 1>{" ~suf:"}@]" ~sep:";@ "
              List.iter
              (fun fmt (b,e) ->
                 Format.fprintf fmt "[%a..%a]" V.pretty b V.pretty e)
              fmt s

  let meet v1 v2 =
    if v1 == v2 then v1
    else
      (match v1,v2 with
         | Top, v | v, Top -> v
         | Set s1 , Set s2 -> Set (
             let rec aux acc (l1:itv list) (l2:itv list) = match l1,l2 with
               | [],_|_,[] -> List.rev acc
               | (((b1,e1)) as i1)::r1,
                   (((b2,e2)) as i2)::r2 ->
                   let c = V.compare b1 b2 in
                   if c = 0 then (* intervals start at the same value *)
                     let ce = V.compare e1 e2 in
                     if ce=0 then
                       aux ((b1,e1)::acc) r1 r2 (* same intervals *)
                     else
                       (* one interval is included in the other *)
                       let min,not_min,min_tail,not_min_tail =
                         if ce > 0 then i2,i1,r2,r1 else
                           i1,i2,r1,r2
                       in
                       aux ((min)::acc) min_tail
                         (((
                             (snd (min),
                              snd (not_min))))::
                            not_min_tail)
                   else (* intervals start at different values *)
                     let _min,min_end,not_min_begin,min_tail,not_min_from =
                       if c > 0
                       then b2,e2,b1,r2,l1
                       else b1,e1,b2,r1,l2
                     in
                     let c_min = V.compare min_end not_min_begin in
                     if c_min >= 0 then
                       (* intersecting intervals *)
                       aux acc
                         ((
                            (not_min_begin,min_end))
                          ::min_tail)
                         not_min_from
                     else
                       (* disjoint intervals *)
                       aux acc min_tail not_min_from
             in aux [] s1 s2))

  let join v1 v2 =
    if v1 == v2 then v1
    else
      (match v1,v2 with
         | Top, _ | _, Top -> Top
         | Set (s1:itv list) , Set (s2:itv list) ->
             let rec aux (l1:itv list) (l2:itv list) = match l1,l2 with
               | [],l|l,[] -> l
               | (b1,e1)::r1,(b2,e2)::r2 ->
                   let c = V.compare b1 b2 in
                   let min_begin,min_end,min_tail,not_min_from =
                     if c >= 0 then b2,e2,r2,l1
                     else b1,e1,r1,l2
                   in
                   let rec enlarge_interval stop l1 look_in_me =
                     match look_in_me with
                       | [] -> stop,l1,[]
                       | ((b,e))::r ->
                           if V.compare stop (V.pred b) >= 0
                           then
                             if V.compare stop e >= 0
                             then enlarge_interval  stop l1 r
                             else enlarge_interval  e r l1
                           else stop,l1,look_in_me
                   in
                   let stop,new_l1,new_l2 =
                     enlarge_interval
                       min_end
                       min_tail
                       not_min_from
                   in ((min_begin,stop))::
                        (aux new_l1 new_l2)
             in Set (aux s1 s2))

  let inject l =  (Set l)

  let inject_one ~size ~value =
    (inject [value,V.add value (V.pred size)])

  let inject_bounds min max =
    if V.le min max
    then inject [min,max]
    else bottom

  let is_included t1 t2 =
    (t1 == t2) ||
      match t1,t2 with
        | _,Top -> true
        | Top,_ -> false
        | Set s1,Set s2 ->
            let rec aux l1 l2 = match l1 with
              | [] -> true
              | i::r ->
                  let rec find (b,e as arg) l =
                    match l with
                      | [] -> raise Not_found
                      | (b',e')::r ->
                          if V.compare b b' >= 0
                            && V.compare e' e >= 0
                          then  l
                          else if V.compare e' b >= 0 then
                            raise Not_found
                          else find arg r
                  in
                  try aux r (find i l2)
                  with Not_found -> false
            in
            aux s1 s2

  let link t1 t2 = join t1 t2 (* join is in fact an exact union *)

  let intersects t1 t2 =
    let m = meet t1 t2 in
    not (equal m bottom)

  let fold f v acc =
    match v with
      | Top -> raise Error_Top
      | Set s ->
          List.fold_right f s acc

  let narrow = meet

  include Datatype.Make
  (struct
     type t = tt
     let name = Interval.name ^ " lattice_interval_set"
     let structural_descr =
       Structural_descr.t_sum
         [| [| Structural_descr.pack
                (Structural_descr.t_list (Descr.str Interval.descr)) |] |]
     let reprs = Top :: List.map (fun o -> Set [ o ]) Interval.reprs
     let equal = equal
     let compare = compare
     let hash = hash
     let rehash = Datatype.identity
     let copy = Datatype.undefined
     let internal_pretty_code = Datatype.undefined
     let pretty = pretty
     let varname = Datatype.undefined
     let mem_project = Datatype.never_any_project
   end)
  let () = Type.set_ml_name ty None

  let pretty_typ typ fmt i =
    let typ =
      match typ with
        Some t -> t
      | None ->
          Cil_types.TArray
            (Cil_types.TInt(Cil_types.IUChar,[]),
             Some (Cil.kinteger64
                     ~loc:(Cil.CurrentLoc.get ())
                     Cil_types.IULongLong
                     (Integer.of_int64 922337203685477580L)
                     (* See Cuoq for rational *)),
             Cil.empty_size_cache (),
             [])
    in
    match i with
    | Top -> Format.fprintf fmt "[..]"
    | Set s ->
        if s=[] then Format.fprintf fmt "BottomISet"
        else begin
          let pp_one fmt (b,e)=
            assert (Int.le b e) ;
            ignore (Bit_utils.pretty_bits typ
                      ~use_align:false
                      ~align:Rel.zero
                      ~rh_size:Int.one
                      ~start:b ~stop:e fmt) in
          let pp_stmt fmt r = Format.fprintf fmt "%a;@ " pp_one r in
          match s with
            | [] -> Format.pp_print_string fmt "{}"
            | [r] -> pp_one fmt r
            | s ->
                Format.fprintf fmt "@[<hov 1>{" ;
                List.iter (pp_stmt fmt) s ;
                Format.fprintf fmt "}@]" ;
        end

  let from_ival_size_aux ival size =
    let max_elt_int = !plevel in
    let max_elt = Int.of_int max_elt_int in
    let add_offset x acc = join (inject_one ~value:x ~size) acc in
    match ival with
    | Ival.Top(None, _, _, _)
    | Ival.Top(_, None, _, _) | Ival.Float _ -> top
    | Ival.Top(Some mn, Some mx, _r, m) ->
        if Int.le m size
        then inject_one ~value:mn ~size:(Int.add (Int.sub mx mn) size)
        else
          let elts = Int.native_div (Int.sub mx mn) m in
          if Int.gt elts max_elt then begin
            (* too many elements to enumerate *)
            Kernel.result ~once:true ~current:true
              "more than %d(%a) elements to enumerate. Approximating."
              max_elt_int Int.pretty elts;
            inject_bounds mn (Int.pred (Int.add mx size))
          end
          else Int.fold add_offset ~inf:mn ~sup:mx ~step:m bottom
    | Ival.Set(s) ->
        Array.fold_right
          add_offset
          s
          bottom

  let from_ival_size ival size =
    match size with
    | Int_Base.Top -> top
    | Int_Base.Value int -> from_ival_size_aux ival int

  let diff x y =
    if x == y 
    then bottom
    else
      ( match x, y with
      | _, Top -> bottom
      | Top, _ -> Top
      | Set sx , Set sy -> Set (
            let rec aux acc (l1:itv list) (l2:itv list) = 
	      match l1 with
            | [] -> List.rev acc (* nothing left *)
	    | (l, u as itv)::tail ->
		let rec relevant_rhs rhs =
		  match rhs with
		  | (_, ur) :: tail when Int.lt ur l-> relevant_rhs tail
		  | _ -> rhs
		in
		let l2 = relevant_rhs l2 in
		match l2 with
		  [] -> List.rev_append acc l1 (* nothing left to remove *)
		| (lr, ur) :: _ ->
		    if Int.lt u lr
		    then
		      aux (itv :: acc) tail l2
		    else 
		      let l1 = 
			if Int.lt ur u then (Int.succ ur, u) :: tail else tail
		      in
		      let acc = 
			if Int.lt l lr
			then 
			  (l, Int.pred lr) :: acc 
			else acc
		      in
		      aux acc l1 l2
	    in
	    aux [] sx sy))
end

module Int_Intervals = struct

  type tt =
    { h:int;
      v: Unhashconsed_Int_Intervals.t;
      tag:int }

  exception Error_Top = Unhashconsed_Int_Intervals.Error_Top

  let id { tag=id } = id

  let pretty_debug fmt x = Unhashconsed_Int_Intervals.pretty fmt x.v
  let pretty = pretty_debug

  let hash_internal {h=h} = h

  let equal_internal {v=v;h=h} {v=v';h=h'} =
    h = h' && Unhashconsed_Int_Intervals.equal v v'

  let name = "int_intervals"

  module IntIntervalsHashtbl =
    Buckx.MakeBig
      (struct
         type t = tt
         let equal = equal_internal
         let hash = hash_internal
         let pretty = pretty
         let id = name
       end)

  let table = IntIntervalsHashtbl.create 139
  let current_tag = ref 0 ;;

  let wrap x =
    let tag = !current_tag in
    let new_i =
      { h = Unhashconsed_Int_Intervals.hash x;
        v = x;
        tag = tag}
    in
    let result = IntIntervalsHashtbl.merge table new_i in
    if result == new_i then current_tag := succ tag;
    result

(* initial values go here *)
  let top = wrap Unhashconsed_Int_Intervals.top
  let bottom = wrap Unhashconsed_Int_Intervals.bottom
(* end of initial values *)

  let compare_itvs i1 i2 = Unhashconsed_Int_Intervals.compare i1.v i2.v

  (* Purely for implementation purposes, nothing to do with the ordering
     induced by the underlying lattice *)
  let compare i1 i2 = Datatype.Int.compare i1.tag i2.tag

  include
    Datatype.Make
      (struct
         type t = tt
         let structural_descr =
           Structural_descr.t_record
             [| Structural_descr.p_int;
                Unhashconsed_Int_Intervals.packed_descr;
                Structural_descr.p_int |]
         let reprs = [ top; bottom ]
         let name = "Lattice_Interval_Set.Int_Intervals"
         let compare = compare
         let equal = ( == )
         let copy = Datatype.undefined
         let hash x = x.h
         let rehash x = wrap x.v
         let internal_pretty_code = Datatype.undefined
         let pretty = pretty
         let varname = Datatype.undefined
         let mem_project = Datatype.never_any_project
       end)

  let diff x y = wrap (Unhashconsed_Int_Intervals.diff x.v y.v)

  let meet x y = wrap (Unhashconsed_Int_Intervals.meet x.v y.v)
  let link x y = wrap (Unhashconsed_Int_Intervals.link x.v y.v)
  let join x y = wrap (Unhashconsed_Int_Intervals.join x.v y.v)
  let narrow x y = wrap (Unhashconsed_Int_Intervals.narrow x.v y.v)

(*
 THERE IS ONLY ONE HASHCONSING TABLE FOR Int_intervals.
   IT IS SHARED BETWEEN PROJECTS
*)

  let intersects x y =
    Unhashconsed_Int_Intervals.intersects x.v y.v

  let is_included x y =
    Unhashconsed_Int_Intervals.is_included x.v y.v

  let join_and_is_included a b =
    let ab = join a b in (ab, equal a b)

  let inject i =
    wrap (Unhashconsed_Int_Intervals.inject i)

  let pretty_typ typ fmt x =
    Unhashconsed_Int_Intervals.pretty_typ typ fmt x.v

  let from_ival_size iv s =
    wrap (Unhashconsed_Int_Intervals.from_ival_size iv s)

  let fold f x acc =
    Unhashconsed_Int_Intervals.fold f x.v acc

  let is_top x = equal x top

  exception Not_a_set

  let project_set x =
    match x.v with
      Unhashconsed_Int_Intervals.Top -> raise Not_a_set
    | Unhashconsed_Int_Intervals.Set s -> s

  let project_singleton x =
    match x.v with
    | Unhashconsed_Int_Intervals.Top -> None
    | Unhashconsed_Int_Intervals.Set [e] -> Some e
    | Unhashconsed_Int_Intervals.Set _ -> None

  let inject_bounds b e =
    wrap (Unhashconsed_Int_Intervals.inject_bounds b e)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
