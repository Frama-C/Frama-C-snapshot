(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(* Offsets encoding *)
open Abstract_interp
open Abstract_value
open CilE
let debug_now = ref false

exception Found_Top
exception Result_is_bottom

type itv = Int.t * Int.t

module type S = sig
  type t 
  type y
  type widen_hint

  module Datatype : Project.Datatype.OUTPUT with type t = t

  val tag : t -> int

  val empty : t
  val is_empty : t -> bool
  val equal : t->t->bool
  val pretty_typ : Cil_types.typ option -> Format.formatter -> t -> unit
  val pretty : Format.formatter -> t -> unit
  val pretty_debug : Format.formatter -> t -> unit
    
    
  val is_included : t -> t -> bool
  val is_included_exn : t -> t -> unit
  val is_included_exn_generic : (y -> y -> unit) -> t -> t -> unit

  val is_included_actual_generic : 
  BaseUtils.BaseSet.t ->
  BaseUtils.BaseSet.t ref ->
    Locations.Location_Bytes.t BaseUtils.BaseMap.t ref -> t -> t -> unit

  val join : t -> t -> (Int.t * Int.t) list * t

  val widen : widen_hint -> t -> t -> t
    
  val find_ival : 
    validity:Base.validity -> with_alarms:CilE.warn_mode
    -> Ival.t -> t -> Int.t -> y -> y
    (** May raise [Not_found] if V.top is found *)

  val concerned_bindings_ival : 
    offsets:Ival.t -> offsetmap:t -> size:Int.t -> y list -> y list
    (** Returns the list of the values associated to at least one bit of the 
	ival. For this function Top is not a binding ! *)

  val update_ival :
    with_alarms:CilE.warn_mode ->
    validity:Base.validity ->
    exact:bool ->
    offsets:Ival.t ->
    size:Int.t ->
    t -> y -> t
    (** May raise [Result_is_bottom] if this is completely out of bound *)

  val overwrite : t -> y -> Origin.t -> t

  val over_intersection : t -> t -> t
    (** An over-approximation of the intersection.  The arguments can not be
	arbitrary offsetmaps: the algorithm would be too complicated. The
	provided algorithm should work fine with offsetmaps that correspond to
	the relation view and the memory view of the same analysed code. *)

  val from_string : string -> t

  val add_whole :  itv -> y -> t -> t
  val remove_whole :  itv -> t -> t

  val fold_whole : 
    size:Int.t -> (Ival.t -> Int.t -> y -> 'a -> 'a) -> t -> 'a -> 'a
    (** May raise [Invalid_argument "Offsetmap.Make.fold"] *)

  val shift_ival : Ival.t -> t -> t option -> t option
    (** [shift_ival shift o acc] returns the join of [acc] and 
	of [o] shifted by all values in [shift].
	Raises [Found_Top] when the result is [Top]. *)

  val copy_paste : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy_merge : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy : t -> Int.t -> Int.t -> t

  val merge_by_itv :  t -> t -> Int_Intervals.t -> t
  val shift : Int.t -> t -> t
  val sized_zero : size_in_bits:Int.t -> t

  val reciprocal_image : t -> Base.t -> Int_Intervals.t * Ival.t
    (** [reciprocal_image m b] is the set of bits in the offsetmap [m] 
	that may lead to Top([b]) and  the set of offsets in [m]
	where one can read an address [b]+_ *)

  val create_initial: v:y -> modu:Int.t -> t

  val reduce_by_int_intervals: t -> Abstract_value.Int_Intervals.t -> t

  val top_stuff : (y -> bool) -> (y -> y) -> t -> t

  val iter_contents : (y -> unit) -> t -> Int.t -> unit
    (** Iter on the contents of offsetmap of given size *)
end

module Build(V:Lattice_With_Isotropy.S) =
struct
  open Abstract_interp
  type value = Int.t*Int.t*V.t

      (* Invariants:
         1) Same as Rangemap.t
         2) 2 contiguous intervals that end on proper boundaries
                 may not map to the same value
         3) No binding to V.top is present
      *)

  type t = value Int_Interv_Map.t
  type y = V.t
  type widen_hint = V.widen_hint


  let hash v =
    Int_Interv_Map.fold
      (fun (b,e) (r,m,v) acc ->
	 assert (Int.lt r m);
	 assert (Int.le Int.zero r);
	 assert (if V.is_isotropic v then Int.is_one m else true);
	 5 * acc + (Int.hash b) + 29 * (Int.hash e) +  299 * (V.hash v))
      v
      19871

  let empty = Int_Interv_Map.empty

  let is_empty v = Int_Interv_Map.is_empty v

  let shift s v = Int_Interv_Map.shift s v

  let pretty_debug fmt m =
    Int_Interv_Map.pretty
      (fun fmt (r,m,v) ->
	 Format.fprintf fmt "{r=%a;m=%a;v=%a}"
	   Int.pretty r
	   Int.pretty m
	   V.pretty v)
      fmt
      m

  let same_values (off,modulo,v) (off',modulo',v') =
(*    Format.printf "same_values v %a v' %a eq %b@."
      V.pretty v V.pretty v' (V.equal v v'); *)
    Int.eq off off' && Int.eq modulo modulo' && (V.equal v v' )

(*
  let check x =
    let last = ref None in
    (*Format.printf "@[check: %a@.@]" pretty_debug x;*)
    let check_interval (b,e) (r,m,v as vv) =
      (*Format.printf "@[check got v: %a@.@]" V.pretty v;*)
      assert (not (V.equal v V.top));
      (*Format.printf "@[check got it v: %a@.@]" V.pretty v;*)
      assert (not (V.is_isotropic v) || Int.eq m Int.one);
      assert (Int.lt r m);
      (match !last with
       | None -> ()
       | Some last ->
           if Int.eq (fst last) (Int.pred b) && Int.eq (Int.pos_rem b m) r then
             assert (if same_values (snd last) vv then
                       (Format.printf "%a" pretty_debug x;false)
                     else true));
      last := Some (e,vv)
    in
    assert(Int_Interv_Map.iter
             check_interval
             x;
           true);
    x
*)
  let check_aligned (bk,ek) offs modu =
    Int.eq (Int.rem bk modu) offs (* start is aligned *)
    &&
    Int.is_zero (Int.rem (Int.succ (Int.sub ek offs)) modu)
    (* end is aligned *)

  let pretty_typ typ fmt m =
    let inset_utf8 = Unicode.inset_string () in
    let is_first = ref true in
    let pretty_binding (bk,ek) (offs,modu,v) =
      let force_misalign = ref false in
      let left = match typ with
        | None ->
            force_misalign :=
              not ((Int.eq (Int.rem bk modu) offs)
                   && (Int.eq (Int.sub ek bk) (Int.pred modu)))
            && not (V.is_isotropic v);
            Format.sprintf "[rbits %a to %a]" Int.pretty_s bk
              Int.pretty_s ek
        | Some typ ->
(*	    Format.printf  "pretty_typ v:%a iso:%b@."
	      V.pretty v
	      (V.is_isotropic v);*)
	    let s,force =
              Bit_utils.pretty_bits typ
		~use_align:(not (V.is_isotropic v))
		~align:offs ~rh_size:modu ~start:bk ~stop:ek
            in force_misalign := force; s
      in
      if !is_first then is_first:=false
      else Format.fprintf fmt "@\n";
      if not !force_misalign then
        Format.fprintf fmt "@[%s %s@ @[<hv 1>%a@]@]"
          left inset_utf8 V.pretty v
      else
        Format.fprintf fmt
          "@[%s %s@ @[<hv 1>%a@] %s %a%%%a @]"
          left
          inset_utf8
          V.pretty v
          (if (Int.eq (Int.rem bk modu) offs)
             && (Int.eq (Int.rem (Int.succ ek) modu) offs)
           then "repeated"
           else "misaligned")
          Int.pretty offs
          Int.pretty modu
    in
    if Int_Interv_Map.is_empty m then
      Format.fprintf fmt "@[[?] %s ANYTHING@]" inset_utf8
    else
    Format.fprintf fmt "@[%a@]"
      (fun _fmt -> Int_Interv_Map.iter pretty_binding) m

  let pretty fmt = pretty_typ None fmt

  let reciprocal_image m base =
    Int_Interv_Map.fold
      (fun (bi,ei as itv) (r,modu,v) (acc1,acc2) ->
         let acc1 = if Locations.Location_Bytes.may_reach base (V.project v)
         then Int_Intervals.join acc1 (Int_Intervals.inject [itv])
         else acc1
         in
         let acc2 = if (Locations.Location_Bytes.intersects
                          (Locations.Location_Bytes.inject base Ival.top)
                          (V.project v))
           && Int.compare modu (Int.of_int (Bit_utils.sizeofpointer ())) = 0
         then
           let first = Int.round_up_to_r ~min:bi ~r ~modu in
           let last = Int.mul (Int.pred (Int.div (Int.succ (Int.sub ei first)) modu)) modu in
           if Int.lt last Int.zero then acc2
           else Ival.join acc2 (Ival.inject_top (Some first) (Some (Int.add first last)) r modu)
         else acc2
         in
         acc1,acc2)
      m
      (Int_Intervals.bottom, Ival.bottom)

  let create_vv (bi, ei) v =
    if V.is_isotropic v
    then Int.zero, Int.one, v
    else
      let new_modu = Int.length bi ei in
      let new_offs = Int.rem bi new_modu in
      new_offs, new_modu, v

  let remove_whole i m =
    (Int_Interv_Map.remove_whole Int_Interv.fuzzy_order i m)

 let extract_bits ~with_alarms ~start ~stop ~modu v =
   assert (Int.le start stop && Int.le stop modu);
   let start,stop =
     if !Cil.little_endian then
       start,stop
     else
       let mmodu = Int.pred modu in
         Int.sub mmodu stop,Int.sub mmodu start
   in
     V.extract_bits ~with_alarms ~start ~stop v

 let merge_bits ~offset ~length ~value ~total_length acc =
   assert ( let total_length_i = Int.of_int total_length in
	    Int.le (Int.add length offset) total_length_i);
   if !Cil.little_endian then
     V.little_endian_merge_bits ~offset ~value ~total_length acc
   else
     V.big_endian_merge_bits ~offset ~value ~total_length ~length acc

 (* Assumes one wants a value from V.t
   TODO : this could merge consecutive values or
   even compute a subvalue. *)
 let find ~with_alarms ((bi,ei) as i) m =
   assert (Int.le bi ei);
   let concerned_intervals =
     Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order i m
   in
   let result =
     match concerned_intervals with
     | [(b,e),(offs,modu,v)] ->
	 if (Int.le b bi) && (Int.ge e ei) then
           if V.is_isotropic v
             || ((Int.eq modu (Int.length bi ei))
                  && (Int.eq (Int.rem bi modu) offs))
           then
             v
           else if (* [(bi-offs)/modu = (ei-offs)/modu] i.e. [bi] and [ei] are in the same slice. *)
               Int.eq
		 (Int.pos_div (Int.sub bi offs) modu)
		 (Int.pos_div (Int.sub ei offs) modu)
           then extract_bits
             ~with_alarms
             ~start:(Int.pos_rem (Int.sub bi offs) modu)
             ~stop:(Int.pos_rem (Int.sub ei offs) modu)
             ~modu
             v
	   else V.topify_misaligned_read_origin v
	     (* the result depends on several instances of
		the same repeated value but is completely covered*)
           else V.top (* the result depends on unbound bits *)
     | [] -> V.top
     | _ ->
         try
           Int_Interv.check_coverage (bi,ei) concerned_intervals;
           let total_length = Int.to_int (Int.length bi ei)
           in
           List.fold_right
	     (fun ((b1,e1),(offs,modu,v)) acc ->
(*	       Format.printf "find debugging bi:%a ei:%a b1:%a e1:%a@."
		 Int.pretty bi Int.pretty ei
		 Int.pretty b1 Int.pretty e1; *)
               let treat_value offs1 acc =
(*		 Format.printf "find treat_value debugging offs:%a@."
		   Int.pretty offs; *)
                 let offset = Int.sub offs1 bi in
                 let offset,start =
		   if Int.lt offset Int.zero
		   then
                     Int.zero, Int.neg offset
                   else
		     offset, Int.zero
                 in
                 let stop = Int.pred modu in
                 let stop =
                   let end_ = Int.min (Int.pred (Int.add offs1 modu)) e1 in
                   let over = Int.sub end_ ei in
                   if Int.gt over Int.zero
		   then
                     Int.sub stop over
                   else
		     stop
                 in
		 assert (not (V.is_isotropic v));
                 let value =
 (*                  Format.printf "extract_bits from start:%a to stop:%a (modu:%a) b1,e1:%a,%a in %a@."
                     Int.pretty start
                     Int.pretty stop
                     Int.pretty modu
                     Int.pretty b1
                     Int.pretty e1
                     V.pretty v
                   ; *)
                   extract_bits ~with_alarms ~start ~stop ~modu v
                 in
                 merge_bits ~offset ~length:(Int.length start stop)
		   ~value ~total_length acc
               in
               let length =Int.length b1 e1 in
               if V.is_isotropic v then begin
		   let offs_start = Int.max b1 bi in
		   let offs_stop = Int.min e1 ei in
		   let offset = Int.sub offs bi in
		   let offset =
		     if Int.lt offset Int.zero then Int.zero else offset
		   in
		   merge_bits ~offset ~length:(Int.length offs_start offs_stop)
		     ~value:v ~total_length acc
		 end
               else if (Int.eq (Int.rem length modu) Int.zero) &&
                   (Int.eq (Int.rem b1 modu) offs)
               then
	         Int.fold
	           treat_value
	           ~inf:(Int.max b1 (Int.round_down_to_r ~max:bi ~r:offs ~modu))
	           ~sup:(Int.min e1 ei)
	           ~step:modu
                   acc
               else
                 V.join
		   (V.topify_misaligned_read_origin v)
		   acc)
             concerned_intervals
             V.singleton_zero
         with Is_not_included -> (* from [Int_Interv.check_coverage] *)
           V.top (* the result depends on several intervals and is not covered
                    completly. *)
   in
   result

  let add_if_not_default i (_,_,v as vv) (m:t) =
    let result =
      if V.equal v V.top  then m else
	Int_Interv_Map.add i vv m
    in
    result


(* Merge the values before and after with the inserted value if
   necessary.*)
  let add_internal ((bi,ei) as i) (_new_offs,_new_modu,v as new_vv) m =
    let (new_offs,new_modu,_) as new_vv =
      if V.is_isotropic v then (Int.zero,Int.one,v) else new_vv
    in
    let extend_left = Int.eq new_offs (Int.pos_rem bi new_modu) in
    let extend_right = Int.is_zero
      (Int.pos_rem (Int.sub (Int.pred new_offs) ei) new_modu) in
    match
      Int_Interv_Map.cleanup_overwritten_bindings
        ~extend_left
        ~extend_right
        same_values
        i
        new_vv
        m
    with
    | None -> m
    | Some(new_bi, new_ei, cleaned_m) ->
        (* Format.printf "new_bi:%a new_ei:%a cleaned:%a@\n"
          Int.pretty new_bi Int.pretty  new_ei (pretty None) cleaned_m;*)

        (* Add the new binding *)
	let result = add_if_not_default (new_bi,new_ei) new_vv cleaned_m in
	result


  let top_stuff f topify offsm =
    assert (not (is_empty offsm));
    Int_Interv_Map.fold
      (fun (_,_ as i) (r,m,v) acc ->
	 assert (Int.lt r m);
	 assert (Int.le Int.zero r);
	 assert (if V.is_isotropic v then Int.is_one m else true);
         assert  (not (V.equal V.top v));
	 if f v
	 then
	   let topified_v = topify v in
	   add_internal i (r, m, topified_v) acc
	 else acc)
      offsm
      offsm


(* Highest level insertion.
   [add (be, ei) v m] inserts [v] in [m] at interval [be,ei] assuming the
   length of [v] is [ei-bi+1]. *)
  let add ((bi,ei) as i) v m =
    let result =
      let v = V.anisotropic_cast ~size:(Int.length bi ei) v in
      let new_vv = create_vv i v in
        add_internal i new_vv m
    in
      (*Format.printf "add %a %a %a@\nSTART:%a@\nRESULT:%a@\n"
        Int.pretty bi Int.pretty ei V.pretty v
        (pretty None) m (pretty None) result;*)
      result

  let add_whole i v m =
    let removed =  (Int_Interv_Map.remove_whole Int_Interv.fuzzy_order i m)
    in
    (add i v removed)


  let split_interval be en offs modu ~treat_aligned ~treat_misaligned acc =
    (* Format.printf "split_interval:be=%a en=%a modu=%a@\n" Int.pretty be
      Int.pretty en Int.pretty modu; *)
    if Int.lt (Int.length be en) modu
    then treat_misaligned be en acc
    else
      let start_aligned = Int.round_up_to_r be offs modu in
      (* [start_aligned] is equal to [offs] modulo [modu] *)
      let result1 =
        if Int.neq start_aligned be
	then begin
(*	  Format.printf "split_interval:treat_misaligned:be=%Ld en=%Ld@\n"
	    be (Int.pred start_aligned);*)
          treat_misaligned be (Int.pred start_aligned) acc
	end
        else acc
      in
      let last_aligned =
        Int.round_down_to_r en (Int.pos_rem (Int.pred offs) modu) modu
      (* [last_aligned] is equal to [offs-1] modulo [modu] *)
      in
      let result2 =
        treat_aligned
          ~inf:start_aligned
          ~sup:last_aligned
          result1
      in
      let result3 =
        if Int.neq last_aligned en
	then begin
(*	  Format.printf "split_interval:treat_misaligned:be=%Ld en=%Ld@\n"
	    (Int.succ last_aligned) en;*)
          treat_misaligned (Int.succ last_aligned) en result2
	end
        else result2
      in
      (* Format.printf "split_interval:finished@\n";*)
      result3

  let map ~treat_aligned ~treat_misaligned:_ (m:t) =
    let result =
      Int_Interv_Map.fold
	(fun (be,en) (offs1, modu1, v1) acc ->
           let treat_aligned ~inf ~sup acc =
             let new_itv = (inf,sup) in
             let v2 = treat_aligned v1 in
             add_internal new_itv (offs1, modu1, v2) acc
           in
           let treat_misaligned _be1 _en1 _acc = assert false in
           split_interval be en offs1 modu1
             ~treat_aligned ~treat_misaligned acc)
	m
	empty
    in result

  let is_instance_one_itv (itv1,_triple1) offsetmap2 =
    let itvs2 =
      Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order itv1 offsetmap2
    in
    Int_Interv.check_coverage itv1 itvs2;
    let treat_one_itv2 (_itv2,_triple2) = ()
    in
    List.iter treat_one_itv2 itvs2

  let eq_triple (a,b,c) (a',b',c') =
    Int.eq a a' && Int.eq b b' && (V.equal c c' )

  let from_string s =
    let r = ref empty in
    let char_width = 8 in
    let l = String.length s in
    for i = 0 to pred l do
      let b = i * char_width in
      let e = pred (b + char_width) in
      r := add (Int.of_int b, Int.of_int e) (V.of_char s.[i]) !r
    done;
    let b = l * char_width in
    let e = pred (b + char_width) in
    add (Int.of_int b, Int.of_int e) (V.singleton_zero) !r

   let is_included_exn_generic v_is_included_exn m1 m2 =
     (*    Format.printf "Offsetmap.is_included_exn_generic %a@\nIN %a@\n"
           (pretty None) m1 (pretty None) m2 ; *)
  (*   if m1 != m2 then -- done by caller *)
       Int_Interv_Map.iter
	 (fun (bi,ei as i) (offs2, modu2, v2) ->
            let itvs1 = Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order i m1 in
            begin
              match itvs1 with
              | [] -> raise Is_not_included
              | ((_,ex),_)::_ ->
                  if Int.lt ex ei then raise Is_not_included
            end;
            ignore
              (List.fold_right
                 (fun ((bx,ex),(offs1,modu1,v1)) acc  ->
                    if Int.gt bx acc then raise Is_not_included;
                    (* [m1] has top for something present in [m2] *)
                    v_is_included_exn v1 v2; (* raise Is_not_included
                                                if [v2] does not include [v1] *)
		    if not (V.is_isotropic v2)
		    then begin
                      if (not (V.is_isotropic v1))
			&& (Int.neq offs1 offs2 || Int.neq modu1 modu2)
                      then raise Is_not_included;
                      (* The alignment is different *)

                      if not (Int.eq bx bi) &&
			(not (Int.is_zero (Int.rem (Int.sub bx offs1) modu1)))
                      then raise Is_not_included;
                      if not (Int.eq ex ei) &&
			(not (Int.is_zero
				(Int.rem (Int.sub (Int.pred offs1) ex) modu1)))
                      then raise Is_not_included;
                      (* the interval [i] is covered by pieces only in [m1],
                         which is less precise than what is in [m2] *)
		    end;
                    Int.succ ex)
                 itvs1
		 bi))
         m2
(*    ;Format.printf "Offsetmap.is_included_exn_generic : WAS included@\n" *)

   let is_included_exn = is_included_exn_generic V.is_included_exn

   let is_included_actual_generic bases q instanciation actual formal =
     let v_is_included v1 v2 =
(*       try *)
	 V.is_included_actual_generic bases q instanciation v1 v2
(*       with Is_not_included ->
	 Format.printf "Not included : %a %a@."
	   V.pretty v1 V.pretty v2; ignore (assert false) ;
	 raise Is_not_included*)
     in
     is_included_exn_generic v_is_included actual formal

  (* For all k,v in m2, v contains (find k m1).
     This is correct only because the default value is top. *)
  let is_included m1 m2 =
    let r = try
      is_included_exn m1 m2;
      true
    with Is_not_included -> false
    in
   (* Format.printf "Offsetmap.is_included(%b) %a@\nIN %a@\n"
      r
      pretty m1 pretty m2 ;*)
    r

  let equal m1 m2 =
    try
      Int_Interv_Map.equal
        same_values
        m1 m2
    with Int_Interv.Cannot_compare_intervals -> false

  let relations_extract_limit = 10

  let unsafe_join, widen =
    let ex_singletons = ref [] in
    let ex_singletons_card = ref 0 in
    let generic (f:size:Int.t -> offs:Int.t option ->
V.t -> V.t -> V.t) m1 m2 =
      let r = if m1 == m2 then m1 else
        begin
          Int_Interv_Map.fold
	    (fun (be,en) (offs1, modu1, v1 as triple1) acc ->
               let itvs2 =
                 Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order (be,en) m2
               in
               List.fold_left
                 (fun acc ((xb,xe),(offs2,modu2,v2 as triple2)) ->
		    let inter_b = Int.max xb be in
		    let inter_e = Int.min en xe in
                    let do_topify acc =
                      add_internal (inter_b,inter_e)
                        (Int.zero, Int.one,
                         f ~size:Int.one ~offs:None
                           (V.topify_merge_origin v1)
                           (V.topify_merge_origin v2))
                        acc
                    in
                    let treat_misaligned _be2 _en2 acc =
		      if eq_triple triple1 triple2
		      then
		        add_internal (inter_b,inter_e) triple1 acc
		      else if Int.eq offs1 offs2 && Int.eq modu1 modu2
		      then
			add_internal (inter_b,inter_e)
			  (offs1,modu1,f ~size:modu1 ~offs:None v1 v2) acc
		      else if V.is_isotropic v1
		      then
                        add_internal (inter_b,inter_e)
                          (offs2,modu2,f ~size:modu2 ~offs:None v1 v2) acc
                      else if V.is_isotropic v2
		      then
                        add_internal (inter_b,inter_e)
                          (offs1,modu1,f ~size:modu1 ~offs:None v1 v2) acc
                      else do_topify acc
		    in
                    if V.is_isotropic v1 then
                      let treat_aligned ~inf ~sup acc =
                        let new_itv = (inf,sup) in
                        let new_v = f ~size:modu2 ~offs:(Some inf) v1 v2 in
                        add_internal new_itv (offs2,modu2,new_v) acc
                      in
                      split_interval inter_b inter_e offs2 modu2
                        ~treat_aligned
                        ~treat_misaligned
                        acc
                    else if (V.is_isotropic v2)
                      || (Int.eq offs2 offs1 && Int.eq modu1 modu2) then
                        let treat_aligned ~inf ~sup acc =
                          let new_itv = (inf,sup) in
                          let new_v = f ~size:modu1 ~offs:(Some inf) v1 v2 in
                          add_internal new_itv (offs1,modu1,new_v) acc
                        in split_interval inter_b inter_e offs1 modu1
                             ~treat_aligned
                             ~treat_misaligned
                             acc
                    else do_topify acc)
                 acc
                 itvs2)
	    m1
	    empty
        end
      in (*Format.printf "join/widen V1:%a V2:%a leads to %a@\n"
           (pretty None) m1 (pretty None) m2 (pretty None) r
           ;*)
      r
    in
    let f_for_join ~size ~offs v w =
      let joined = V.join v w in
      let joined = V.anisotropic_cast ~size joined in
      ( match offs with
	Some offs when
	    V.cardinal_zero_or_one v && V.cardinal_zero_or_one w
	    && (not (V.cardinal_zero_or_one joined))
	    && (!ex_singletons_card < relations_extract_limit) ->
	      incr ex_singletons_card;
	      ex_singletons := (offs, size) :: !ex_singletons
      | _ -> ());
      joined
    in
    (fun x y ->
      let result = generic f_for_join x y in
      let singletons = !ex_singletons in
      ex_singletons := [];
      ex_singletons_card := 0;
      singletons, result),
    (fun wh -> generic
       (fun ~size ~offs:_ v w ->
          let widened = V.widen wh v w in
          let r = V.anisotropic_cast ~size widened in
          (* Format.printf "widen: size:%a v:%a w:%a r:%a@\n"
             Int.pretty size
             V.pretty  v
             V.pretty w
             V.pretty r;*)
          r))

  exception Not_aligned

  exception Continue_here of itv * value * t * t * t

  let over_intersection m1 m2 =
    (*    Format.printf "over_intersection:@\n%a@\nand@\n%a@."
	  (pretty None) m1
	  (pretty None) m2; *)
    let rec over_intersection_rec continue acc =
      try
	let (_b1, _e1 as itv1),v1 = Int_Interv_Map.find_above continue m1 in
	try
	  let (_b2, _e2 as itv2),v2 = Int_Interv_Map.find_above continue m2 in
	  treat_the_lowest_binding itv1 v1 itv2 v2 acc
	with Int_Interv_Map.No_such_binding ->
	  (* m2 is finished *)
	  (Int_Interv_Map.fold
	      (fun (bi, ei as itv) vv (_cont, acc) ->
		if Int.ge bi continue
		then ei, Int_Interv_Map.add itv vv acc
		else ei, acc)
	      m1
	      (continue, acc))
      with Int_Interv_Map.No_such_binding ->
	(* m1 is finished *)
	(Int_Interv_Map.fold
	    (fun (bi, ei as itv) vv (_cont, acc) ->
	      if Int.ge bi continue
	      then ei, Int_Interv_Map.add itv vv acc
	      else ei, acc)
	    m2
	    (continue, acc))
    and treat_the_lowest_binding (b1,_e1 as itv1) (_,_,_v1 as v1) (b2,_e2 as itv2) (_,_,_v2 as v2) acc =
      (*      Format.printf "treat_the_lowest_binding: %a..%a -> %a    %a..%a -> %a@."
	      Int.pretty b1 Int.pretty _e1 V.pretty _v1
	      Int.pretty b2 Int.pretty _e2 V.pretty _v2; *)
      let itv, vv, first_m, other_m =
	if Int.lt b1 b2
	then itv1,v1,m1,m2
	else itv2,v2,m2,m1
      in
      treat_lowest_binding itv vv first_m other_m acc
    and treat_lowest_binding
	(b, e as itv) (offs,modu,v as vv) first_m other_m acc =
      let concerned_intervals =
	Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order itv other_m
      in
      let treat_interval ((bc, ec as _itvc), (offsc,moduc,vc as vvc))
	  (next, acc) =
	(*	Format.printf "treat_interval: %a..%a -> %a, continue=%a@."
		Int.pretty bc Int.pretty ec V.pretty vc
		Int.pretty next;*)
	let acc =
	  if Int.eq bc next
	  then acc
	  else begin
	      (* add a binding to vv in the result where there is no
		 binding in other_m *)
	      assert (Int.lt next bc);
	      Int_Interv_Map.add (next, Int.pred bc) vv acc
	    end
	in
	let same_align = Int.eq moduc modu && Int.eq offsc offs in
	if Int.neq moduc Int.one &&
	  Int.neq modu Int.one &&
	  (not same_align)
	then begin
	    Format.printf "An assumption made for the implementation of this tool turns out to be invalid. Please report the appearance of this message. If possible, please provide a reasonably-sized example that provokes it. The correctness of the computations is not affected and the analysis will continue\n";
	    raise Not_aligned
	  end;
	let inter_vv =
	  if same_align
	  then (offs, modu, V.narrow v vc)
	  else (Int.zero, Int.one,
	       V.narrow (V.under_topify v) (V.under_topify vc))
	in
	let over_reach = Int.gt ec e in
	let actual_end =
	  if over_reach
	  then e
	  else ec
	in
	let new_next = Int.succ actual_end in
	let new_acc = Int_Interv_Map.add (bc, actual_end) inter_vv acc in
	if over_reach
	then (* if we arrive here, we are necessarily treating
		the last interval in the list, but we have to
		chain to the treatment of the over-reaching part *)
	  raise (Continue_here( (new_next,ec), vvc, other_m, first_m, new_acc))
	else
	  new_next, new_acc
      in
      try
	let next, acc =
	  List.fold_right treat_interval concerned_intervals (b,acc)
	in
	let acc =
	  if Int.gt next e
	  then acc
	  else Int_Interv_Map.add (next, e) vv acc
	in
	over_intersection_rec (Int.succ e) acc
      with Continue_here(itv, vvc, other_m, first_m, new_acc) ->
	treat_lowest_binding itv vvc other_m first_m new_acc
    in
    let result =
      try
	let itv1, v1 = Int_Interv_Map.lowest_binding m1 in
	try
	  let itv2, v2 = Int_Interv_Map.lowest_binding m2 in
	  snd (treat_the_lowest_binding itv1 v1 itv2 v2 empty)
	with Int_Interv_Map.Empty_rangemap ->
	  (* m2 is empty *)
	  m1
      with Int_Interv_Map.Empty_rangemap ->
	(* m1 is empty *)
	m2
    in
(*    Format.printf "over_intersection:@\n%a@\nand@\n%a@\n->@\n%a@."
      (pretty None) m1
      (pretty None) m2
      (pretty None) result; *)
    result

  let join m1 m2 =
(*    Format.printf "Computing size of args@.";
    Format.printf "Size of Offsetmap.join args : %d - %d@." (Size.size_kb m1) (Size.size_kb m2) ;
    Format.printf "Starting Offsetmap.join@.";*)
    let singletons,r1 = unsafe_join m1 m2 in
(*    Format.printf "Done Offsetmap.join@.";*)
    assert (let _,r2 = unsafe_join m2 m1 in
            equal r1 r2 ||
              (Format.printf
                 "Non commuting join %a@\n with %a@\n leads to %a@\n and %a@\n"
                 pretty m1 pretty m2
		 pretty r1 pretty r2;
               false));
    (*if !debug_now
    then
      Format.printf "Joining(offsetmap) %a@\n with %a@\n leads to %a@\n"
      pretty m1 pretty m2 pretty r1;*)
    assert
      (if (equal m1 m2 && not (equal r1 m1))
       then begin
         Format.printf
           "equal m1 m2 different result %a@ leads to %a@\n"
           pretty m1 pretty r1;
         assert false
       end; true);
    singletons, r1


  let add_approximate_including_spaces mn mx r m size v existing_offsetmap =
    let treat_itv (b,e as itv) (rem,modu,value as vv) acc=
      if (Int.lt e mn)
	|| (Int.ge b (Int.add mx size))
      then (* non intersecting interval *)
	add_internal itv vv acc
      else begin
	let acc,new_b =
	  if Int.lt b mn
	  then
            (add_internal
	       (b,Int.pred mn)
	       vv
	       acc,
             mn)
	  else acc,b
	in
	let acc,new_e =
	  if Int.gt e (Int.pred (Int.add mx size))
	  then
            let mx = Int.add mx size in
            let acc = add_internal (mx,e) vv acc in
            acc, Int.pred mx
	  else acc,e
	in
        let new_r,new_modu,cond =
	  if Int.eq m size
	  then
            if Int.eq r rem && Int.eq m modu then r,modu,true
              (* else if V.is_isotropic v then rem,modu,true *)
            else if V.is_isotropic value then r,m,true
            else Int.zero,Int.one,false
          else begin
            if Int.gt size m
	    then warn_once
	      "trying to write auto-overlapping(every %a) val(%a) of size %a"
	      Int.pretty m
	      V.pretty v
	      Int.pretty size
            else if Int.lt size m
	    then begin
	      let number = Int.succ (Int.div (Int.sub mx mn) m) in
              warn_once
		"more than %d(%a) locations to update in array. Approximating."
		(Cmdline.ArrayPrecisionLevel.get ())
		Int.pretty number
	    end;
	    Int.zero,Int.one,false
          end
        in
        let new_v =
	  (if cond
	   then
	     V.anisotropic_cast ~size:new_modu
	       (V.join v value)
	   else
	     V.join
	       (V.topify_misaligned_read_origin v)
	       (V.topify_misaligned_read_origin value))
	in
        add_internal
          (new_b,new_e)
          (new_r,new_modu,new_v)
          acc
      end
    in
    Int_Interv_Map.fold
      treat_itv
      existing_offsetmap
      empty

  let add_approximate offset size v offsetmap_orig =
    add_approximate_including_spaces offset offset
      (Int.pos_rem offset size) size size v offsetmap_orig

(* compute the resulting offsetmap if [v] is potentially written anywhere *)
  let overwrite offsetmap_orig v o =
    let v = V.topify_with_origin o v in
    Int_Interv_Map.fold
      (fun itv (_offs,_modu,bound_v) acc ->
	let new_v = V.join (V.topify_with_origin o bound_v) v in
	add_internal itv (Int.zero, Int.one, new_v) acc)
      offsetmap_orig
      empty

(* add a binding in the case where the "offset" part of the location
   is known only as an interval of integers modulo *)
  let add_top_binding_offsetmap ~with_alarms mn mx r m size v existing_offsetmap =
    match mn,mx with
    | None, _ | _, None ->
	(match with_alarms.imprecision_tracing with
        | Aignore -> ()
        | Acall f -> f ()
        | Alog -> warn_once "Writing at unbounded offset: approximating");
(*	Format.eprintf "add_top_binding: %a %a %a %a@."
	  Ival.pretty 
	  (Ival.Top (mn, mx, r, m))
	  Int.pretty size
	  V.pretty v
	  pretty existing_offsetmap; *)
	overwrite existing_offsetmap v (Origin.Arith (LocationSetLattice.currentloc_singleton()))

    | Some mn, Some mx ->
	let number = Int.succ (Int.div (Int.sub mx mn) m) in
	if Int.le number (Int.of_int (Cmdline.ArrayPrecisionLevel.get ())) &&
	  (Int.gt m size)
	then
	  Int.fold
	    (fun offs acc -> add_approximate offs size v acc)
	    ~inf:mn
	    ~sup:mx
	    ~step:m
	    existing_offsetmap
	else
	  add_approximate_including_spaces mn mx r m size v existing_offsetmap

  exception Had_top of V.t * Int.t

  type interval_or_set = Set of Ival.O.t | Interval of Int.t * Int.t * Int.t

  let empty_interval_or_set = Set (Ival.O.empty)

  let reduce_ival_by_bound ~read ~with_alarms ival size bound_min bound_max =
    let max_in_bound = Int.succ (Int.sub bound_max size) in
    let is_in_bound x = match x with
    | Ival.Top (mn,mx,r,m) ->
	let out, new_mn =
	  match mn with
	  | Some mn when (Int.ge mn bound_min) -> false, mn
	  | _ -> true, (Int.round_up_to_r ~r ~modu:m ~min:bound_min)
	in
	let out, new_mx =
	  match mx with
	  | Some mx when (Int.le mx max_in_bound) -> out, mx
	  | _ -> true, (Int.round_down_to_r ~r ~modu:m ~max:max_in_bound)
	in
	if out then
          (if read then warn_mem_read with_alarms else warn_mem_write with_alarms
          (*   ;Format.printf "OBA:%a new_mn:%a new_mx:%a@\n"
               Ival.pretty x Int.pretty new_mn Int.pretty new_mx *) );
	if Int.le new_mn new_mx
	then Interval(new_mn, new_mx, m)
	else empty_interval_or_set
    | _ -> assert false
    in
    match ival with
    | Ival.Top (_mn,_mx,_r,_m) -> is_in_bound ival
    | Ival.Float _ -> is_in_bound Ival.top
    | Ival.Set s ->
	Set(Ival.O.fold
	      (fun offset acc ->
		 let pseudo_interval =
		   Ival.Top(Some offset, Some offset,Int.zero, Int.one)
		 in
		 match is_in_bound pseudo_interval with
		   Interval _ -> Ival.O.add offset acc
		 | _ -> acc)
	      s
	      Ival.O.empty)

  let create_initial ~v ~modu =
    let vv = if V.is_isotropic v then Int.zero,Int.one,v
    else Int.zero,modu,v
    in
    add_if_not_default (Int.zero,Bit_utils.max_bit_address ()) vv empty

  (* Raises [Not_found] if V.top is found *)
  let find_ival ~validity ~with_alarms offsets offsetmap size acc =
    (*Format.eprintf "find_ival: %a in %a@." Ival.pretty offsets pretty
      offsetmap;*)
    let filtered_by_bound =
      match validity with
      | Base.Known (bound_min,bound_max) | Base.Unknown (bound_min,bound_max) ->
	  reduce_ival_by_bound ~read:true ~with_alarms offsets size bound_min bound_max
      | Base.All -> begin (* no clipping can be performed *)
	  match offsets with
	  | Ival.Top (Some mn,Some mx,_r,m) -> Interval(mn, mx, m)
	  | Ival.Top (None,_,_,_) | Ival.Top (_,None,_,_) |
		Ival.Float _ ->
	      raise Not_found (* return top *)
	  | Ival.Set o -> Set o
	end
    in
    let result = match filtered_by_bound with
    | Interval(mn, mx, m) ->
        if Int.lt mx mn then V.bottom
        else
          ((*Format.eprintf "find_ival itv is mn=%a mx=%a m = %a@."
             Int.pretty mn Int.pretty mx Int.pretty m;*)
           let concerned_intervals =
             Int_Interv_Map.concerned_intervals
               Int_Interv.fuzzy_order
               (mn,Int.pred (Int.add mx size)) offsetmap
           in
           let common_r = Int.pos_rem mn m in
           let magix = Int.pos_rem common_r size in
           let next_index_to_read = ref mn in
           let last_partial_value = ref None in
           let result = List.fold_right
             (fun ((bi,ei),(remain,modulo,v)) acc ->
                (*Format.eprintf "find_ival bi=%a ei=%a next_index_to_read=%a@\n"
                  Int.pretty bi
                  Int.pretty ei
                  Int.pretty !next_index_to_read;*)
                if Int.lt ei !next_index_to_read
                then begin
                  (*Format.printf "Skipping useless interval@\n";*)
                  acc
                end else
                  let donotdegenerate =
                    match !last_partial_value with
                    | None when Int.le bi !next_index_to_read -> true
                    | Some next_bit when Int.le bi next_bit ->
                        assert (Int.eq bi next_bit);
                        true
                    | _ -> false
                  in
                  if donotdegenerate then
                    let last_index_covered = (* m*((ei-common_r)/m)+common_r *)
                      Int.min
                        (Int.add
                           common_r
                           (Int.mul (Int.pos_div (Int.sub ei common_r) m) m))
                        mx
                    in
                    let new_v =
                      if Int.le (Int.pred (Int.add last_index_covered size)) ei
                      then (* the last one is covered *)
                        let value_read =
                          if ((Int.eq modulo size && Int.eq remain magix)
                              || V.is_isotropic v)
                            && !last_partial_value = None
                          then v
                          else V.topify_misaligned_read_origin v
                        in
                        next_index_to_read := Int.add last_index_covered m;
                        last_partial_value := None;
                        value_read
                      else (* the last one is not covered *)
                        let top_v = V.topify_misaligned_read_origin v in
                        next_index_to_read := last_index_covered;
                        last_partial_value := Some (Int.succ ei);
                        top_v
                    in 
                    (*Format.eprintf "find_ival new_v=%a@." V.pretty new_v; *)
                    V.join acc new_v
                  else raise Not_found (* return top *))
             concerned_intervals
             V.bottom
           in
           let dodegenerate =
             match !last_partial_value with
             | None when Int.gt !next_index_to_read mx -> false
             | _ -> true
           in
           if dodegenerate then raise Not_found else result)
    | Set s ->
        (*Format.eprintf "find_ival(Set) %a@." Ival.pretty (Ival.Set s);*)
	Ival.O.fold
	  (fun offset acc ->
             let itv = offset, Int.pred(Int.add offset size) in
	     let new_value = find ~with_alarms itv offsetmap in
             (*Format.eprintf "find_ival(Set) new_v<%a>@." V.pretty new_value ;*)
             let result = V.join acc new_value in
             if V.equal result V.top then
               raise Not_found; (* return top *)
	     result)
	  s
          acc
    in
    (*Format.eprintf "find_ival returns: %a@." V.pretty result;*)
    result


  let update_ival ~with_alarms ~validity ~exact ~offsets ~size
      offsetmap_orig v =
 (*   Format.printf "update_ival got: %a %a %a@\n"
      Ival.pretty offsets
      Int.pretty size
      pretty offsetmap_orig; *)
    let fold_set s =
      Ival.O.fold
	(fun offset acc ->
	   let itv = offset, Int.pred(Int.add offset size) in
           let new_offsetmap =
             (if exact
	      then add itv v acc
	      else add_approximate offset size v acc)
           in
           new_offsetmap)
	s
	offsetmap_orig
    in
    let result =
      match validity with
      | Base.Known (bound_min, bound_max) | Base.Unknown (bound_min, bound_max) -> begin
	  (*	Format.eprintf "update_ival size_of_varid %a@\n"
		Int.pretty bound; *)
	  let reduced =
            reduce_ival_by_bound ~read:false ~with_alarms
              offsets size bound_min bound_max
	  in
	  match reduced with
	    Interval(mn, mx, m) ->
	      (*	    Format.eprintf "update_ival  -> [%a %a] %% %a@\n"
			    Int.pretty mn
			    Int.pretty mx
			    Int.pretty m; *)
	      add_top_binding_offsetmap ~with_alarms
		(Some mn) (Some mx) (Int.pos_rem mn m) m
		size v offsetmap_orig
	  | Set o when not (Ival.O.is_empty o) -> fold_set o
          | Set _  -> raise Result_is_bottom
	end
      | Base.All -> begin
	  match offsets with
	  | Ival.Top (mn,mx,r,m) ->
	      assert (if not exact then true else (Format.printf "Internal error 193: %a@." Ival.pretty offsets; false));
	      add_top_binding_offsetmap ~with_alarms
		mn mx r m size v offsetmap_orig
	  | Ival.Set o -> fold_set o
	  | Ival.Float _ ->
	      assert (not exact);
	      add_top_binding_offsetmap ~with_alarms None None Int.zero Int.one
		size v offsetmap_orig
	end
    in
(*    Format.printf "update_ival result: %a@\n"
      pretty result; *)
    result

  (* returns the list of the values associated to at least one bit of the ival.
  For this function Top is not a binding ! *)
  let concerned_bindings_ival ~offsets ~offsetmap ~size acc =
    match offsets with
    | Ival.Float _ -> assert false
      | Ival.Top (mn,mx,_,_) ->
          begin
            match mn, mx with
              | None, _ | _, None ->
                  Int_Interv_Map.fold (fun _k (_,_,v) acc -> v::acc)
                    offsetmap
                    []
              | Some mn, Some mx ->
                  let concerned_itv =
                    Int_Interv_Map.concerned_intervals
                      Int_Interv.fuzzy_order
                      (mn, (Int.pred (Int.add mx size)))
                      offsetmap
                  in
                  List.fold_left
		    (fun acc (_,(_,_,v)) -> v::acc)
		    acc
		    concerned_itv
          end
      | Ival.Set s ->
	  Ival.O.fold
	    (fun offset acc ->
	       let itv = offset, Int.pred(Int.add offset size) in
               let concerned_itv =
                 Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order itv offsetmap
               in
               List.fold_left (fun acc (_,(_,_,v)) -> v::acc) acc concerned_itv)
            s
            acc

  let copy_paste from start stop start_to _to =
    let result =
      let ss = start,stop in
      let to_ss = start_to, Int.sub (Int.add stop start_to) start in
        (* First removing the bindings of the destination interval *)
      let _to = Int_Interv_Map.remove_itv Int_Interv.fuzzy_order to_ss _to in
      let concerned_itv =
        Int_Interv_Map.concerned_intervals Int_Interv.fuzzy_order ss from
      in
      let offset = Int.sub start_to start in
      let treat_interval acc (i,(offs,modu, v)) =
        let new_vv = (Int.pos_rem (Int.add offs offset) modu),modu,v in
        let src_b,src_e = Int_Interv.clip_itv ss i in
        let dest_i = Int.add src_b offset, Int.add src_e offset in
          (*Format.printf "treat_itv: ib=%a ie=%a v=%a dib=%a die=%a@\n"
            Int.pretty (fst i) Int.pretty (snd i)
            V.pretty v
            Int.pretty (fst dest_i) Int.pretty (snd dest_i);*)
          add_internal dest_i new_vv acc
      in
        List.fold_left treat_interval _to concerned_itv
    in
      (* Format.printf "Offsetmap.copy_paste from:%a start:%a stop:%a start_to:%a to:%a result:%a@\n"
        pretty from
        Int.pretty start
        Int.pretty stop
        Int.pretty start_to
        pretty _to
        pretty result;*)
      result


  let copy_merge from start stop start_to _to =
    let old_value =
      copy_paste _to start_to
        (Int.sub (Int.add start_to stop) start)
        start empty
    in
    let _,merged_value = join old_value from in
      copy_paste merged_value start stop start_to _to

  let copy from start stop =
    copy_paste from start stop Int.zero empty

  let merge_by_itv (orig:t) (_new:t) (itvs:Int_Intervals.t) =
    try
      Int_Intervals.fold
        (fun (start,stop) acc -> copy_paste _new start stop start acc)
        itvs
        orig
    with Int_Intervals.Error_Top -> _new

  let (==>) a b = (not a) || b

  let fold_whole ~size f m acc =
    let result =
      Int_Interv_Map.fold
	(fun (be,en) (offs1, modu1, v1) acc ->
           if
             (V.is_isotropic v1 ==>
                not (Int.is_zero (Int.pos_rem (Int.succ (Int.sub en be)) size)))
             &&
             (not (V.is_isotropic v1) ==>
                (Int.neq modu1 size
                 || Int.neq (Int.pos_rem be modu1) offs1
                 || Int.neq (Int.pos_rem (Int.succ en) modu1) offs1))
           then begin
             Format.printf "Got size %a modu1:%a@\n" Int.pretty size
               Int.pretty modu1;
             raise (Invalid_argument "Offsetmap.Make.fold")
           end ;
           let offs1 = if V.is_isotropic v1 then Int.pos_rem be size
           else offs1
           in
           f
             (Ival.inject_top
                (Some be)
                (Some (Int.sub (Int.succ en) size))
                offs1
                size)
             size
             v1
             acc)
        m
        acc
    in result


  let shift_ival ival m acc =
    match ival with
      | Ival.Top _ | Ival.Float _ -> raise Found_Top
          (* Approximating because we do no want to shift
             with too many values *)
      | Ival.Set s ->
          Ival.O.fold
            (fun v acc ->
               let shifted = shift v m in
               match acc with None -> Some shifted
                 | Some acc -> Some (snd (join acc shifted)))
            s acc

  let sized_zero ~size_in_bits =
    assert (Int.gt size_in_bits Int.zero);
    (*Format.printf "size in bits: %a (%a)@." Int.pretty size_in_bits
      V.pretty V.singleton_zero;*)
    (Int_Interv_Map.add
        (Int.zero, Int.pred size_in_bits)
        (Int.zero, Int.one, V.singleton_zero)
        empty)

  let reduce_by_int_intervals offsetmap iset =
    try
    Int_Interv_Map.fold
      (fun itv vv acc ->
	 let itv_iset = Int_Intervals.inject [itv] in
	 let inter = Int_Intervals.meet itv_iset iset in
	 Int_Intervals.fold
	   (fun itv acc ->
	      Int_Interv_Map.add itv vv acc)
	   inter
	   acc)
      offsetmap
      empty
    with Int_Intervals.Error_Top (* from Int_Intervals.fold *) ->
      assert false (* shouldn't happen *)

  let iter_contents f o size = 
    let itv = (Int.zero,Int.pred size) in
    let concerned_intervals = Int_Interv_Map.concerned_intervals  Int_Interv.fuzzy_order itv o in
    (try 
      Int_Interv.check_coverage itv concerned_intervals;
     with Is_not_included -> f V.top);
    List.iter (fun (_,(_,_,b)) -> f b) concerned_intervals
        
end

module Make(V:Lattice_With_Isotropy.S) = struct

  module M=Build(V)
  type t =
      { h:int;
        v: M.t;
	tag:int }
  type tt = t
  type y = M.y

  let id = V.id ^ " offsetmap"

  let hash_internal {v=_;h=h} = h

  let tag {tag=t} = t

  let equal_internal {v=v;h=h} {v=v';h=h'} =
    h = h' && M.equal v v'

  module OffsetmapHashtbl =
    Buckx.MakeBig(struct
                   type t = tt
                   let equal = equal_internal
                   let hash = hash_internal
		   let pretty fmt x = M.pretty fmt x.v
		   let id = "offsetmap"
                   end)

 let table = OffsetmapHashtbl.create 1379
 let current_tag = ref 0 ;;


  let wrap x =
    let tag = !current_tag in
(*    if tag mod 10000 = 0
    then begin
	let a,b,c,d,e,f = OffsetmapHashtbl.stats table in
	Format.printf "offsetmap table: %4d %4d %4d %4d %4d %4d@."
	  a b c d e f;
	OffsetmapHashtbl.iter
	  (fun o ->
	     Format.printf "%8d %8d %a@\n@."
	       o.h
	       o.tag
	       (M.pretty None)
	       o.v)
	  table
      end;
*)
    let new_offsetmap =
      { h = M.hash x;
        v = x;
        tag = tag}
    in
    let result = OffsetmapHashtbl.merge table new_offsetmap in
    if result.tag = tag
    then begin
     let new_current = succ tag in
     if new_current = 0
     then begin
	 Format.printf "An internal limit of the analyser has been reached. The solutions are to report to the developers, or to use the 64-bit version of this software@.";
	 exit 1;
       end;
     current_tag := new_current;
      end;
    result


    module rec PatriciaReHashtbl : Hashtbl.S with type key = tt =
      Hashtbl.Make
	(struct
           type t = tt
           let equal a b =
             assert (let result = a.tag = b.tag in result = (a == b));
             a == b
           let hash = tag
	 end)

    and Rehash_Table : sig val find : tt -> t
                           val add : t -> t -> unit
                           val clear : unit -> unit
    end = struct
      let table = PatriciaReHashtbl.create 17
      let add =  PatriciaReHashtbl.add table
      let find = PatriciaReHashtbl.find table
      let clear () =  PatriciaReHashtbl.clear table
    end

  let rehash x =
    try
      Rehash_Table.find x
    with Not_found ->
      let rv =
	Int_Interv_Map.map (function r,m,v -> r, m, V.Datatype.rehash v) x.v
      in
      let result = wrap rv in
      Rehash_Table.add x result;
      result

 let empty = wrap M.empty

 let rehash_initial_values () =
   assert (empty == OffsetmapHashtbl.merge table empty) ;
   5

 module Datatype =
   Project.Datatype.Register
     (struct
	type t = tt
	let rehash = rehash
	let copy _ = assert false (* TODO *)
	let before_load () = current_tag := rehash_initial_values ()
	let after_load = Rehash_Table.clear
	let name = Project.Datatype.Name.make id
	let dependencies = [ V.Datatype.self ]
      end)

 let equal o1 o2 = o1 == o2

 module Symcacheable =
 struct
   type t = tt
   let hash = tag
   let equal = (==)
   let sentinel = empty
 end

 module R =
 struct
   type t = (Int.t * Int.t) list * tt
   let sentinel = [], empty
 end

 module SymetricCache = Binary_cache.Make_Symetric(Symcacheable)(R)

 let () = Project.register_todo_on_clear SymetricCache.clear


 let join  m1 m2 =
   if m1 == m2 then [],m1
   else
     let compute () =
       let singletons, joined = M.join m1.v m2.v in
       singletons, wrap joined
     in
     SymetricCache.merge compute m1 m2

  let is_empty v = M.is_empty v.v
  let pretty_typ t fmt v = M.pretty_typ t fmt v.v
  let pretty fmt v = M.pretty fmt v.v
  let pretty_debug fmt v = M.pretty_debug fmt v.v

 module Cacheable =
	    struct
	      type t = tt
	      let hash = tag
	      let equal = (==)
	      let sentinel = empty
	    end

 module Cache = Binary_cache.Make_Asymetric(Cacheable)(Binary_cache.Bool_Result)

  let is_included m1 m2 =
    if m1 == m2 then true
    else
      let m1v = m1.v in
      let m2v = m2.v in
      if Int_Interv_Map.height m2v <= 3
      then M.is_included m1v m2v
      else
	let compute () = M.is_included m1v m2v in
	Cache.merge compute m1 m2

  let is_included_exn m1 m2 =
    if not (is_included m1 m2) then raise Is_not_included

  let is_included_exn_generic f v1 v2 =
    M.is_included_exn_generic f v1.v v2.v

  type widen_hint = M.widen_hint

  let find ~with_alarms i v = M.find ~with_alarms i v.v

  let is_included_actual_generic a b c v1 v2 =
    M.is_included_actual_generic a b c v1.v v2.v

  let widen w v1 v2 = wrap (M.widen w v1.v v2.v)

  let find_ival ~validity ~with_alarms a v
      = M.find_ival ~validity ~with_alarms a v.v

  let concerned_bindings_ival ~offsets ~offsetmap ~size =
    M.concerned_bindings_ival ~offsets ~offsetmap:offsetmap.v ~size

  let update_ival ~with_alarms ~validity ~exact ~offsets ~size v y =
    wrap (M.update_ival ~with_alarms ~validity ~exact ~offsets ~size v.v y)

  let overwrite v y o =
    wrap (M.overwrite v.v y o)

  let over_intersection v1 v2 =
    wrap (M.over_intersection v1.v v2.v)

  let from_string s =
    wrap (M.from_string s)

  let add_whole itv y t =
    wrap (M.add_whole itv y t.v)

  let remove_whole itv t =
    wrap (M.remove_whole itv t.v)

  let fold_whole ~size f t acc =
    M.fold_whole ~size f t.v acc

  let shift_ival i v acc =
    let acc = match acc with
    | None as acc -> acc
    | Some acc -> Some acc.v
    in
    match M.shift_ival i v.v acc with
    | None as acc -> acc
    | Some acc -> Some (wrap acc)

  let copy_paste t a b c t' =
    wrap (M.copy_paste t.v  a b c t'.v)

  let copy_merge t a b c t' =
    wrap (M.copy_merge t.v  a b c t'.v)

  let copy v a b =
    wrap (M.copy v.v a b)

  let merge_by_itv t t' a =
    wrap (M.merge_by_itv t.v t'.v a)

  let shift a v =
    wrap (M.shift a v.v)

  let sized_zero ~size_in_bits = wrap (M.sized_zero ~size_in_bits)

  let reciprocal_image v =
    M.reciprocal_image v.v

  let create_initial ~v ~modu =
    wrap (M.create_initial ~v ~modu)

  let reduce_by_int_intervals v a =
    wrap (M.reduce_by_int_intervals v.v a)

  let top_stuff condition topify om =
    wrap (M.top_stuff condition topify om.v)

  let iter_contents f o size = 
    M.iter_contents f o.v size

end

(*
Local Variables:
compile-command: "make -C ../.. -j 3"
End:
*)
