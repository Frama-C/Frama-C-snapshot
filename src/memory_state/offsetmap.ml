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

open Abstract_interp
open Abstract_value
open CilE

exception Found_Top
exception Result_is_bottom
exception Result_is_same

type itv = Int.t * Int.t

module type S = sig
  type y
  type widen_hint
  include Datatype.S
  val tag : t -> int
  val empty : t
  val is_empty : t -> bool
  val pretty_c_assert_typ :
    string -> Cil_types.typ -> (unit->unit) -> Format.formatter -> t -> unit
  val pretty_typ : Cil_types.typ option -> Format.formatter -> t -> unit
  val pretty_debug : Format.formatter -> t -> unit
  val reduce : Ival.t -> size:Int.t -> y -> t -> t
  val is_included : t -> t -> bool
  val is_included_exn : t -> t -> unit
  val is_included_exn_generic : (y -> y -> unit) -> t -> t -> unit
  val join : t -> t -> (Int.t * Int.t) list * t
  val widen : widen_hint -> t -> t -> t
  val find_ival :
    conflate_bottom: bool -> validity:Base.validity ->
    with_alarms:CilE.warn_mode -> Ival.t -> t -> Int.t -> y
    (** May raise [Not_found] if V.top is found *)

  val cardinal_zero_or_one: Base.validity -> t -> bool

  val find_imprecise_entire_offsetmap : validity:Base.validity -> t -> y
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
  val from_cstring : Base.cstring -> t
  val add_internal : itv -> Int.t * Int.t * y -> t -> t
  val add_whole :  itv -> y -> t -> t
  val remove_whole :  itv -> t -> t
  val fold_whole :
    size:Int.t -> (Ival.t -> Int.t -> y -> 'a -> 'a) -> t -> 'a -> 'a
    (** May raise [Invalid_argument "Offsetmap.Make.fold"] *)
  val fold_single_bindings :
    size:Int.t -> (Ival.t -> Int.t -> y -> 'a -> 'a) -> t -> 'a -> 'a
    (** May raise [Invalid_argument "Offsetmap.Make.fold"] *)
  val fold_internal :
    (itv -> (Int.t * Int.t * y) -> 'a -> 'a) -> t -> 'a -> 'a
  val shift_ival : Ival.t -> t -> t option -> t option
    (** [shift_ival shift o acc] returns the join of [acc] and
        of [o] shifted by all values in [shift].
        Raises [Found_Top] when the result is [Top]. *)
  val copy_paste : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy_merge : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy_offsmap : t -> Int.t -> Int.t -> t
  val copy_ival :
    validity:Base.validity ->
    with_alarms:CilE.warn_mode ->
    Ival.t -> t -> Int.t -> t
  val merge_by_itv :  t -> t -> Int_Intervals.t -> t
  val shift : Int.t -> t -> t
  val sized_zero : size_in_bits:Int.t -> t
  val reciprocal_image : t -> Base.t -> Int_Intervals.t * Ival.t
    (** [reciprocal_image m b] is the set of bits in the offsetmap [m]
        that may lead to Top([b]) and  the set of offsets in [m]
        where one can read an address [b]+_ *)
  val create_initial: v:y -> modu:Int.t -> t
  val reduce_by_int_intervals: t -> Abstract_value.Int_Intervals.t -> t
  val top_stuff : (y -> bool) -> (y -> 'a * y) -> ('a -> 'a -> 'a) -> 'a -> t -> 'a * t
  val iter_contents : (y -> unit) -> t -> Int.t -> unit
    (** Iter on the contents of offsetmap of given size *)
  val fold : (Int.t * Int.t -> Int.t * Int.t * y -> 'a -> 'a) -> t -> 'a -> 'a
  val is : t -> y -> bool
end

module Build(V:Lattice_With_Isotropy.S) = struct

  open Abstract_interp

  module New = New_offsetmap.Make(V)

  module Int_Int_V = Datatype.Triple(Int)(Int)(V)
  module M = Int_Interv_Map.Make(Int_Int_V)

  type value = Int_Int_V.t

      (* Invariants:
         1) Same as Rangemap.t
         2) 2 contiguous intervals that end on proper boundaries
                 may not map to the same value
         3) No binding to V.top is present
      *)

  type t = M.t
  let packed_descr = M.packed_descr
  type y = V.t
  type widen_hint = V.widen_hint

  let hash v = M.hash v

  let empty = M.empty
  let is_empty v = M.is_empty v

  let shift s v = M.shift s v

  let pretty_c_assert_typ name _typ print_ampamp fmt offs =
    let pretty_binding (bk,ek) (offst,modu,v) =
      if Int.is_zero (Int.rem bk Int.eight)
        && (Int.equal (Int.pos_rem bk modu) offst)
      then
        let ek = Int.succ ek in
        if Int.is_zero (Int.rem ek Int.eight)
        then

          let step = if V.is_isotropic v then 1 else (Int.to_int modu) / 8 in
          let start = ref ((Int.to_int bk) / 8) in
          let ek = Int.to_int ek in
          let ek = ek / 8 in
          while !start < ek do
            let lv =
              if !start = 0
              then
                Format.sprintf "&%s" name
              else
                Format.sprintf "((unsigned char*)&%s+%d)"
                  name
                  !start
            in
            V.pretty_c_assert print_ampamp lv step fmt v;
            start := !start + step
          done;
        else ()
      else ()
    in
    M.iter pretty_binding offs

  let pretty_debug fmt m =
    M.pretty
      (fun fmt (r,m,v) ->
         Format.fprintf fmt "{r=%a;m=%a;v=%a}"
           Int.pretty r
           Int.pretty m
           V.pretty v)
      fmt
      m

  let pretty_compare fmt m =
    M.iter
      (fun (x, y) (r, m, v) ->
        Format.fprintf fmt "@[[%a..%a] -> (%a, %a, %a);@]@;@ "
          Int.pretty x
          Int.pretty y
          Int.pretty r
          Int.pretty m
          V.pretty v
      )
      m

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
             assert (if Int_Int_V.equal (snd last) vv then
                       (Format.printf "%a" pretty_debug x;false)
                     else true));
      last := Some (e,vv)
    in
    assert(M.iter
             check_interval
             x;
           true);
    x
*)
  let check_aligned (bk,ek) offs modu =
    Int.equal (Int.rem bk modu) offs (* start is aligned *)
    &&
    Int.is_zero (Int.rem (Int.succ (Int.sub ek offs)) modu)
    (* end is aligned *)

  let pretty_typ typ fmt m =
    let inset_utf8 = Unicode.inset_string () in
    let is_first = ref true in
    let pretty_binding fmt (bk,ek) (offs,modu,v) =
      if !is_first then is_first:=false
      else Format.fprintf fmt "@\n";
      Format.fprintf fmt "@[" ;
      (* Print left-member and return misalign condition *)
      let force_misalign =
        match typ with
          | None ->
              Format.fprintf fmt "[rbits %a to %a]"
                Int.pretty bk Int.pretty ek ;
              (* misalign condition: *)
              not ((Int.equal (Int.rem bk modu) offs)
                   && (Int.equal (Int.sub ek bk) (Int.pred modu)))
              && not (V.is_isotropic v)

          | Some typ ->
              (* returns misalign condition. *)
              Bit_utils.pretty_bits typ
                ~use_align:(not (V.is_isotropic v))
                ~align:offs ~rh_size:modu ~start:bk ~stop:ek fmt
      in
      Format.fprintf fmt " %s@ @[<hv 1>%a@]" inset_utf8 V.pretty v ;
      if force_misalign
      then
        Format.fprintf fmt " %s %a%%%a "
          (if (Int.equal (Int.rem bk modu) offs)
             && (Int.equal (Int.rem (Int.succ ek) modu) offs)
           then "repeated"
           else "misaligned")
          Int.pretty offs
          Int.pretty modu ;
      Format.fprintf fmt "@]"
    in
    if M.is_empty m then
      Format.fprintf fmt "@[[?] %s ANYTHING@]" inset_utf8
    else
    Format.fprintf fmt "@[%a@]"
      (fun fmt -> M.iter (pretty_binding fmt)) m

  let pretty fmt = pretty_typ None fmt

  let fold_internal f m acc =
    M.fold f m acc

exception Not_translatable
;;

let translate_from_old omap =
  if is_empty omap then raise Not_translatable;
  let conv = Int.to_int64 in
    fold_internal
      ( fun (a, b) (r, m, v) (c, acc) ->
(*      Format.printf "translating %a %a@."
          Int.pretty a Int.pretty b; *)
	let acc =
          if not (Int.equal (Int.succ c) a)
          then snd (New.add_node
            (conv (Int.succ c)) (conv (Int.pred a))
            Int64.zero Int64.one V.top Int64.zero acc)
	  else acc
	in
        let o, t =
          New.add_node
            (conv a) (conv b) (conv r) (conv m) v Int64.zero acc in
        assert (o = Int64.zero);
        b, t
       )
      omap (Int.minus_one, New.empty)
;;

let translate_from_old omap = snd (translate_from_old omap)

let reciprocal_image m base =
  let treat_binding (bi,ei as itv) (r,modu,v) (acc1,acc2) =
    let acc1 = if Locations.Location_Bytes.may_reach base (V.project v)
      then Int_Intervals.join acc1 (Int_Intervals.inject [itv])
      else acc1
    in
    let acc2 =
      if (Locations.Location_Bytes.intersects
             (Locations.Location_Bytes.inject base Ival.top)
             (V.project v))
        && Int.compare modu (Int.of_int (Bit_utils.sizeofpointer ())) = 0
      then
        let first = Int.round_up_to_r ~min:bi ~r ~modu in
        let last =
          Int.mul
            (Int.pred (Int.div (Int.succ (Int.sub ei first)) modu))
            modu
        in
        if Int.lt last Int.zero then acc2
        else
          Ival.join
            acc2
            (Ival.inject_top (Some first) (Some (Int.add first last)) r modu)
      else acc2
    in
    acc1,acc2
  in
  M.fold treat_binding m (Int_Intervals.bottom, Ival.bottom)

  let create_vv (bi, ei) v =
    if V.is_isotropic v
    then Int.zero, Int.one, v
    else
      let new_modu = Int.length bi ei in
      let new_offs = Int.rem bi new_modu in
      new_offs, new_modu, v

  let remove_whole i m =
    (M.remove_whole Int_Interv.fuzzy_order i m)

 let extract_bits ~start ~stop ~modu v =
   assert (Int.le start stop && Int.le stop modu);
   let start,stop =
     if Cil.theMachine.Cil.little_endian then
       start,stop
     else
       let mmodu = Int.pred modu in
         Int.sub mmodu stop,Int.sub mmodu start
   in
     V.extract_bits ~start ~stop v

 let merge_bits ~offset ~length ~value ~total_length acc =
   assert ( let total_length_i = Int.of_int total_length in
            Int.le (Int.add length offset) total_length_i);
   if Cil.theMachine.Cil.little_endian then
     V.little_endian_merge_bits ~offset ~value ~total_length acc
   else
     V.big_endian_merge_bits ~offset ~value ~total_length ~length acc

 exception Bottom_found

 let extract_bits_and_stitch ~conflate_bottom (bi, ei) concerned_intervals =
   try
     Int_Interv.check_coverage (bi,ei) concerned_intervals;
     let total_length = Int.to_int (Int.length bi ei)
     in
     let treat_concerned_interval
         ((b1,e1),(offs,modu,v)) (acc_inform, acc_value) =
         (*            Format.printf "find debugging bi:%a ei:%a b1:%a e1:%a@."
                       Int.pretty bi Int.pretty ei
                       Int.pretty b1 Int.pretty e1; *)
         let treat_value offs1 (acc_inform, acc_value) =
           (*            Format.printf "find treat_value debugging offs:%a@."
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
           let inform_extract_pointer_bits, value =
             extract_bits ~start ~stop ~modu v
           in
           inform_extract_pointer_bits || acc_inform,
           merge_bits ~conflate_bottom ~offset ~length:(Int.length start stop)
             ~value ~total_length acc_value
         in
         let length =Int.length b1 e1 in
         if V.is_isotropic v then begin
             if conflate_bottom && V.equal V.bottom v then raise Bottom_found;
             let offs_start = Int.max b1 bi in
             let offs_stop = Int.min e1 ei in
             let offset = Int.sub offs bi in
             let offset =
               if Int.lt offset Int.zero then Int.zero else offset
             in
             acc_inform,
             merge_bits ~offset ~length:(Int.length offs_start offs_stop)
               ~conflate_bottom
               ~value:v ~total_length acc_value
           end
         else if (Int.is_zero (Int.rem length modu)) &&
             (Int.equal (Int.rem b1 modu) offs)
         then
           Int.fold
             treat_value
             ~inf:(Int.max b1 (Int.round_down_to_r ~max:bi ~r:offs ~modu))
             ~sup:(Int.min e1 ei)
             ~step:modu
             (acc_inform, acc_value)
         else
           acc_inform,
           V.join
             (V.topify_misaligned_read_origin v)
             acc_value
     in
     List.fold_right
       treat_concerned_interval
       concerned_intervals
       (false, V.singleton_zero)
   with Is_not_included -> (* from [Int_Interv.check_coverage] *)
     false, V.top (* the result depends on several intervals and is not covered
                    completely. *)
   | Bottom_found -> false, V.bottom

 (* Assumes one wants a value from V.t
   This merges consecutive values when singleton integers and
   conversely extract bits the best it can when it has to.
    Could perhaps be improved. *)
 let find ~conflate_bottom ((bi,ei) as i) m period_read_ahead =
   assert (Int.le bi ei);
   let concerned_intervals =
     M.concerned_intervals Int_Interv.fuzzy_order i m
   in
     match concerned_intervals with
     | [(b,e),(offs,modu,v)] ->
         if (Int.le b bi) && (Int.ge e ei) then
           let isotropic = V.is_isotropic v in
           if isotropic
             || ((Int.equal modu (Int.length bi ei))
                  && (Int.equal (Int.rem bi modu) offs))
           then
             let read_ahead =
               if Int.is_zero (Int.rem period_read_ahead modu)
               then Some e
               else None
             in
             false, read_ahead, v
           else
             let inform, v =
               if (* [(bi-offs)/modu = (ei-offs)/modu]
                     i.e. [bi] and [ei] are in the same slice. *)
                 Int.equal
                   (Int.pos_div (Int.sub bi offs) modu)
                   (Int.pos_div (Int.sub ei offs) modu)
               then
                 extract_bits
                   ~start:(Int.pos_rem (Int.sub bi offs) modu)
                   ~stop:(Int.pos_rem (Int.sub ei offs) modu)
                   ~modu
                   v
               else
                 extract_bits_and_stitch
                   ~conflate_bottom
                   i concerned_intervals
                   (* the result depends on several instances of
                      the same repeated value but is completely covered*)
             in
             inform, None, v
         else
           false, None, V.top (* the result depends on unbound bits *)
     | [] -> false, None, V.top
     | _ ->
         let inform, v =
           extract_bits_and_stitch
             ~conflate_bottom i concerned_intervals
         in
         inform, None, v

 let find_imprecise bi ei m =
   assert (Int.le bi ei);
   let i = bi, ei in
   let concerned_intervals =
     M.concerned_intervals Int_Interv.fuzzy_order i m
   in
   try
     Int_Interv.check_coverage (bi,ei) concerned_intervals;
     List.fold_left
       (fun acc (_, (_,_,v)) ->
         V.join
           acc
           ( if V.is_isotropic v
             then v
             else V.topify_misaligned_read_origin v ) )
       V.bottom
       concerned_intervals
   with Is_not_included (* from check_coverage *) -> V.top

  let add_if_not_default i (_,_,v as vv) (m:t) =
    let result =
      if V.equal v V.top then m else
        M.add i vv m
    in
    result

  exception More_than_one

  let cardinal_zero_or_one validity offsetmap =
    let r =
    match validity with
    | Base.All -> false
    | Base.Periodic (min, max, _)
    | Base.Known(min, max) | Base.Unknown(min, max) ->
        begin try
            let up_to =
              M.fold
                (fun (bi,ei) (_r,_m,v) min ->
                  if Int.gt bi min then raise More_than_one;
                  if not (V.cardinal_zero_or_one v) then raise More_than_one;
                  (Int.succ ei))
                offsetmap
                min
            in
            Int.gt up_to max
          with More_than_one -> false
        end
    in
    r

  let find_imprecise_entire_offsetmap ~validity m =
    match validity with
    | Base.All -> V.top
    | Base.Periodic _ -> assert false (* TODO *)
    | Base.Known (bound_min,bound_max) | Base.Unknown (bound_min,bound_max)
          when Int.lt bound_max bound_min ->
        V.bottom
    | Base.Known (bound_min,bound_max) | Base.Unknown (bound_min,bound_max) ->
        let next = ref bound_min in
        try
          let r =
            M.fold
              (fun (bi,ei) (_r,_m,v) acc ->
                if Int.equal bi !next
                then begin
                    next := Int.succ ei;
                    V.join
                      acc
                      ( if V.is_isotropic v
                        then v
                        else V.topify_misaligned_read_origin v )
                  end
                else begin
                    assert (Int.gt bi !next);
                    raise Found_Top
                  end;)
              m
              V.bottom
          in
          if Int.gt !next bound_max
          then r
          else V.top
        with Found_Top -> V.top

  (* Merge neighboring values with the inserted value if necessary.*)
  let add_internal ((bi,ei) as i) (_new_offs,_new_modu,v as new_vv) m =
    let (new_offs,new_modu,_) as new_vv =
      if V.is_isotropic v then (Int.zero,Int.one,v) else new_vv
    in
    let v_is_singleton = V.cardinal_zero_or_one v in
    let extend_left =
      v_is_singleton || Int.equal new_offs (Int.pos_rem bi new_modu)
    in
    let extend_right =
      v_is_singleton || Int.is_zero
      (Int.pos_rem (Int.sub (Int.pred new_offs) ei) new_modu)
    in
    match
      M.cleanup_overwritten_bindings
        ~extend_left
        ~extend_right
        Int_Int_V.equal
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

  let top_stuff f topify join_locals acc_locals offsm =
    assert (not (is_empty offsm));
    M.fold
      (fun (_,_ as i) (r,m,v) (acc_locals, acc_o as acc) ->
         assert (Int.lt r m);
         assert (Int.le Int.zero r);
         assert (if V.is_isotropic v then Int.is_one m else true);
         assert  (not (V.equal V.top v));
         if f v
         then
           let locals, topified_v = topify v in
           (join_locals acc_locals locals),
           add_internal i (r, m, topified_v) acc_o
         else acc)
      offsm
      (acc_locals, offsm)

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

  let reduce ival ~size v m =
    try
      let bi = Ival.project_int ival in
      let ei = Int.pred (Int.add bi size) in
      let i = bi, ei in
      let rem, modu, old_v = M.find i m in
      if not (Int.equal rem (Int.pos_rem bi modu))
      then raise Result_is_same;
      if not (Int.equal modu size)
      then raise Result_is_same;
      let new_v = V.narrow old_v v in
      if (V.equal v new_v)
      then raise Result_is_same;
        M.add i (rem, modu, v) (M.remove i m)
    with Int_Interv.Cannot_compare_intervals ->
      raise Result_is_same

  let add_whole i v m =
    let removed =  (M.remove_whole Int_Interv.fuzzy_order i m)
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
        if not (Int.equal start_aligned be)
        then begin
(*        Format.printf "split_interval:treat_misaligned:be=%Ld en=%Ld@\n"
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
        if not (Int.equal last_aligned en)
        then begin
(*        Format.printf "split_interval:treat_misaligned:be=%Ld en=%Ld@\n"
            (Int.succ last_aligned) en;*)
          treat_misaligned (Int.succ last_aligned) en result2
        end
        else result2
      in
      (* Format.printf "split_interval:finished@\n";*)
      result3

  let map ~treat_aligned ~treat_misaligned:_ (m:t) =
    let result =
      M.fold
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
      M.concerned_intervals Int_Interv.fuzzy_order itv1 offsetmap2
    in
    Int_Interv.check_coverage itv1 itvs2;
    let treat_one_itv2 (_itv2,_triple2) = ()
    in
    List.iter treat_one_itv2 itvs2

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

  let from_wstring s =
    let pwchar_width = 
      Int.of_int (pred (Cil.bitsSizeOf Cil.theMachine.Cil.wcharType)) 
    in
    let addw (b,acc) wchar =
      let e = Int.add b pwchar_width in
      Int.succ e, (add (b, e) (V.of_int64 wchar) acc)
    in
    snd (List.fold_left addw (Int.zero,empty) s)

  let from_cstring cs =
    match cs with
      Base.CSWstring w -> from_wstring w
    | Base.CSString s -> from_string s

   let is_included_exn_generic v_is_included_exn m1 m2 =
     (*    Format.printf "Offsetmap.is_included_exn_generic %a@\nIN %a@\n"
           (pretty None) m1 (pretty None) m2 ; *)
  (*   if m1 != m2 then -- done by caller *)
       M.iter
         (fun (bi,ei as i) (offs2, modu2, v2) ->
            let itvs1 = M.concerned_intervals Int_Interv.fuzzy_order i m1 in
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
                        && not ((Int.equal offs1 offs2 &&
                                    Int.equal modu1 modu2))
                      then raise Is_not_included;
                      (* The alignment is different *)

                      if not (Int.equal bx bi) &&
                        (not (Int.is_zero (Int.rem (Int.sub bx offs1) modu1)))
                      then raise Is_not_included;
                      if not (Int.equal ex ei) &&
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
;;
(*
  let is_included m1 m2 =
   let  r = try
      is_included_exn m1 m2;
      true
     with Is_not_included -> false
   in
   let t1 = translate_from_old m1
   and t2 = translate_from_old m2 in

   let rnew = New.is_included t1 t2
   in
     if r <> rnew
   then
       begin
         Format.printf "*** ISINC@ %b %b @.m1: %a@.t1: %a@.m2: %a@.t2: %a@."
           r rnew
          pretty_compare m1
         New.pretty t1
          pretty_compare m2
         New.pretty t2 ;
         (assert false)
       end
   else r
;;
*)
  let equal m1 m2 =
    try M.equal m1 m2 with Int_Interv.Cannot_compare_intervals -> false

  let relations_extract_limit = 10

  let unsafe_join, widen =
    let ex_singletons = ref [] in
    let ex_singletons_card = ref 0 in
    let generic (f:size:Int.t -> offs:Int.t option -> V.t -> V.t -> V.t) m1 m2 =
      let r = if m1 == m2 then m1 else
        begin
          M.fold
            (fun (be,en) (offs1, modu1, v1 as triple1) acc ->
               let itvs2 =
                 M.concerned_intervals Int_Interv.fuzzy_order (be,en) m2
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
                      if Int_Int_V.equal triple1 triple2
                      then add_internal (inter_b,inter_e) triple1 acc
                      else if Int.equal offs1 offs2 && Int.equal modu1 modu2
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
                      || (Int.equal offs2 offs1 && Int.equal modu1 modu2) then
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
        let (_b1, _e1 as itv1),v1 = M.find_above continue m1 in
        try
          let (_b2, _e2 as itv2),v2 = M.find_above continue m2 in
          treat_the_lowest_binding itv1 v1 itv2 v2 acc
        with M.No_such_binding ->
          (* m2 is finished *)
          (M.fold
              (fun (bi, ei as itv) vv (_cont, acc) ->
                if Int.ge bi continue
                then ei, M.add itv vv acc
                else ei, acc)
              m1
              (continue, acc))
      with M.No_such_binding ->
        (* m1 is finished *)
        (M.fold
            (fun (bi, ei as itv) vv (_cont, acc) ->
              if Int.ge bi continue
              then ei, M.add itv vv acc
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
        M.concerned_intervals Int_Interv.fuzzy_order itv other_m
      in
      let treat_interval ((bc, ec as _itvc), (offsc,moduc,vc as vvc))
          (next, acc) =
        (*      Format.printf "treat_interval: %a..%a -> %a, continue=%a@."
                Int.pretty bc Int.pretty ec V.pretty vc
                Int.pretty next;*)
        let acc =
          if Int.equal bc next
          then acc
          else begin
              (* add a binding to vv in the result where there is no
                 binding in other_m *)
              assert (Int.lt next bc);
              M.add (next, Int.pred bc) vv acc
            end
        in
        let same_align = Int.equal moduc modu && Int.equal offsc offs in
        if (not (Int.is_one moduc)) &&
          (not (Int.is_one modu)) &&
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
        let new_acc = M.add (bc, actual_end) inter_vv acc in
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
          else M.add (next, e) vv acc
        in
        over_intersection_rec (Int.succ e) acc
      with Continue_here(itv, vvc, other_m, first_m, new_acc) ->
        treat_lowest_binding itv vvc other_m first_m new_acc
    in
    let result =
      try
        let itv1, v1 = M.lowest_binding m1 in
        try
          let itv2, v2 = M.lowest_binding m2 in
          snd (treat_the_lowest_binding itv1 v1 itv2 v2 empty)
        with M.Empty_rangemap ->
          (* m2 is empty *)
          m1
      with M.Empty_rangemap ->
        (* m1 is empty *)
        m2
    in
(*    Format.printf "over_intersection:@\n%a@\nand@\n%a@\n->@\n%a@."
      (pretty None) m1
      (pretty None) m2
      (pretty None) result; *)
    result

  let join m1 m2 =
    let singletons,r1 = unsafe_join m1 m2 in
    assert (let _,r2 = unsafe_join m2 m1 in
            equal r1 r2 ||
              (Format.printf
                 "Non commuting join %a@\n with %a@\n leads to %a@\n and %a@\n"
                 pretty m1 pretty m2
                 pretty r1 pretty r2;
               false));
    singletons, r1
;;

(*
(* Test functions related to new offsetmaps *)
  let join m1 m2 =
    let s, r = join m1 m2
    and l1, n1 = (translate_from_old m1)
    and l2, n2 = (translate_from_old m2) in
   (* pretty_compare Format.std_formatter m1; *)
   (* Format.printf "N1:%a@." pretty n1;
    Format.printf "N2:%a@." pretty n2; *)
    if not (Int.equal l1 l2) then s, r  else
    let _o, t = join n1 n2   in
    let _, t1 = (translate_from_old r) in
    if not (equal t1  t) then
      begin
        Format.printf "@.**** Trees not equal@.";
        pretty_compare Format.std_formatter m1;
         Format.printf "@.";
        pretty_compare Format.std_formatter m2;
        print t1;
        print t;
        assert false;
      end;
    s, r
*)
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
          if Int.equal m size
          then
            if (Int.equal r rem && Int.equal m modu) || V.is_isotropic value
            then r,m,true
            else Int.zero,Int.one,false
          else begin
            assert (Int.lt size m);
            let number = Int.succ (Int.div (Int.sub mx mn) m) in
            Kernel.result ~current:true ~once:true
              "more than %d(%a) locations to update in array. Approximating."
              (Kernel.ArrayPrecisionLevel.get())
              Int.pretty number;
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
    M.fold
      treat_itv
      existing_offsetmap
      empty

  let add_approximate offset size v offsetmap_orig =
    if V.is_isotropic v
    then begin
        let e_max = Int.add offset (Int.pred size) in
        let concerned_intervals =
          M.concerned_intervals
            Int_Interv.fuzzy_order
            (offset, e_max)
            offsetmap_orig
        in
        let treat_itv acc ((b,e), (rem,modu,value)) =
          let new_b = Int.max b offset in
          let new_e = Int.min e e_max in
          let new_v = V.join v value in
          add_internal (new_b, new_e) (rem, modu, new_v) acc
        in
        List.fold_left treat_itv offsetmap_orig concerned_intervals
      end
    else
    add_approximate_including_spaces offset offset
      (Int.pos_rem offset size) size size v offsetmap_orig

  let add_imprecise mn mx v offsetmap_orig =
     add_approximate_including_spaces mn mx Int.zero Int.one Int.one
      v offsetmap_orig

  let overwrite offsetmap_orig v o =
    let v = V.topify_with_origin o v in
    M.fold
      (fun itv (_offs,_modu,bound_v) acc ->
        let new_v = V.join (V.topify_with_origin o bound_v) v in
        add_internal itv (Int.zero, Int.one, new_v) acc)
      offsetmap_orig
      empty

(* add a binding in the case where the "offset" part of the location
   is known only as an interval of integers modulo *)
  let add_top_binding_offsetmap mn mx r m size v existing_offsetmap =
    let number = Int.succ (Int.div (Int.sub mx mn) m) in
    let plevel =                (Kernel.ArrayPrecisionLevel.get()) in
    if Int.le number (Int.of_int plevel) && (Int.gt m size)
    then
      Int.fold
        (fun offs acc -> add_approximate offs size v acc)
        ~inf:mn
        ~sup:mx
        ~step:m
        existing_offsetmap
    else
      add_approximate_including_spaces mn mx r m size v existing_offsetmap

  let create_initial ~v ~modu =
    let vv = if V.is_isotropic v then Int.zero,Int.one,v
    else Int.zero,modu,v
    in
    add_if_not_default (Int.zero,Bit_utils.max_bit_address ()) vv empty

  let find_ival ~conflate_bottom ~validity ~with_alarms offsets offsetmap size =
    (*Format.eprintf "find_ival: %a in %a@." Ival.pretty offsets pretty
      offsetmap;*)
    let r =
    try
      let filtered_by_bound =
        Tr_offset.filter_by_bound_for_reading ~with_alarms offsets size validity
      in
      let inform = ref false in
      let value = ref V.bottom in
      let pred_size = Int.pred size in
      let find_and_accumulate offset period_read_ahead =
        let itv = offset, Int.add offset pred_size in
        let new_inform, read_ahead, new_value =
          find ~conflate_bottom itv offsetmap period_read_ahead
        in
        let new_value = V.join new_value !value in
        value := new_value;
        inform := !inform || new_inform;
        if V.equal new_value V.top
        then Some (Bit_utils.max_bit_size())
        else read_ahead
      in
      begin match filtered_by_bound with
      | Tr_offset.Imprecise (mn, mx) ->
          value := find_imprecise mn mx offsetmap
      | Tr_offset.Interval (mn, mx, m) ->
          assert(Int.gt m pred_size);
          let current = ref mn in
          let r = Int.pos_rem mn m in
          while Int.le !current mx do
            let read_ahead = find_and_accumulate !current m in
            let next = Int.add !current m in
            let cursor =
              match read_ahead with
                None -> next
              | Some e ->
                  let max = Int.sub e pred_size in
                  let aligned_b = Int.round_down_to_r ~max ~r ~modu:m in
                  if Int.ge aligned_b next
                  then aligned_b
                  else next
            in
            current := cursor
          done;
      | Tr_offset.Set s ->
          (*Format.eprintf "find_ival(Set) %a@." Ival.pretty (Ival.Set s);*)
          Ival.O.iter (fun x -> ignore (find_and_accumulate x Int.zero)) s;
      end;
      if !inform then begin
        match with_alarms.imprecision_tracing with
        | Aignore -> ()
        | Acall f -> f ()
        | Alog _ ->
          Kernel.warning ~current:true ~once:true
            "extracting bits of a pointer";
      end;
      !value
    with Tr_offset.Unbounded -> V.top
    in
    if M.is_empty offsetmap && not (V.equal V.top r) then
      Kernel.debug ~once:true "%a+%a, size %a, %s, validity %a, result %a"
        (M.pretty Int_Int_V.pretty) offsetmap Ival.pretty offsets Int.pretty size (if conflate_bottom then "conflate" else "") Base.pretty_validity validity V.pretty r;
    r

  let find_ival ~conflate_bottom ~validity ~with_alarms offsets offsetmap size =
    let result_old =
      find_ival ~conflate_bottom ~validity ~with_alarms offsets offsetmap size
    in
    if (not (Kernel.PreciseUnions.get())) || V.cardinal_zero_or_one result_old
    then result_old
    else
      try
(*
	  Format.printf "DO:@\nconflate_bottom:%B ival:%a size:%a@\n Old: %a@. "
	    conflate_bottom
	    Ival.pretty offsets Int.pretty size
	    V.pretty result_old; 
*)
	let new_omap = translate_from_old offsetmap in
	let new_result = New.find_ival ~conflate_bottom ~validity ~with_alarms offsets new_omap size in
	if not (V.is_included new_result result_old) 
	then begin
            Format.printf "Please report@\nFIND_IVAL:@\nconflate_bottom:%B ival:%a size:%a@\nOld: %a@\nNew: %a@\n %a@."
	      conflate_bottom
              Ival.pretty offsets Int.pretty size
              V.pretty result_old V.pretty new_result
              New.pretty new_omap; 
	    assert false
	  end;
	new_result
      with
      | Not_translatable ->
	  if not (is_empty offsetmap)
	  then
            Format.printf "NOT TRANSLATED %a@."
              pretty offsetmap;
	  result_old
      | Not_found -> 
	  Format.printf "GOT Not_found %a@."
            pretty offsetmap;
	  result_old

  let update_ival ~with_alarms ~validity ~exact ~offsets ~size
      offsetmap_orig v =
    (*   Format.printf "update_ival got: %a %a %a@\n"
         Ival.pretty offsets
         Int.pretty size
         pretty offsetmap_orig; *)
    try
      let exact, reduced =
        Tr_offset.filter_by_bound_for_writing ~with_alarms ~exact offsets size validity
      in
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
      match reduced with
        | Tr_offset.Imprecise (mn, mx) ->
          let v = V.topify_misaligned_read_origin v in
          add_imprecise mn mx v offsetmap_orig
        | Tr_offset.Interval(mn, mx, m) ->
          begin
            let res =
              add_top_binding_offsetmap
                mn mx (Int.pos_rem mn m) m
                size v offsetmap_orig in
(*
  try
              let t1 = translate_from_old res in
              let new_omap = translate_from_old offsetmap_orig in
              if New.is_empty new_omap then raise Not_translatable;
              let min, max =
                match validity with
                  | Base.All -> begin (* no clipping can be performed *)
                    raise Not_translatable
                  end
                  | Base.Known (bound_min, bound_max)
                  | Base.Unknown (bound_min, bound_max) ->
                    bound_min, bound_max
                  | Base.Periodic (_bound_min, _, period) ->
                    Int.zero, Int.pred period
              in
              let min = Int.to_int64 min
              and max = Int.to_int64 max
              and mn = Int.to_int64 mn
              and mx = Int.to_int64 mx
              and period = Int.to_int64 m
              and size = Int.to_int64 size
              in
              let t2 =
                New.update_ival min max exact mn mx period size  new_omap v
              in
              if not (New.is_included t2 t1) then
                (
                  Format.fprintf Format.std_formatter "arg:%a@.ival:%a v:%a@."
                    pretty offsetmap_orig
                    Ival.pretty offsets
                    V.pretty v
                  ;
                  Format.fprintf Format.std_formatter "Old:%a@." New.pretty t1;
                  Format.fprintf Format.std_formatter "New:%a@." New.pretty t2;
		  assert false
                );
              res
            with
   | Not_translatable ->  *)
                res
          end

        | Tr_offset.Set o when not (Ival.O.is_empty o) -> fold_set o
        | Tr_offset.Set _  ->
          if exact
          then raise Result_is_bottom
          else offsetmap_orig
    with
        Tr_offset.Unbounded ->
          (match with_alarms.imprecision_tracing with
            | Aignore -> ()
            | Acall f -> f ()
            | Alog _ ->
              Kernel.warning ~once:true ~current:true
                "Writing at unbounded offset: approximating");
          overwrite
            offsetmap_orig
            v
            (Origin.Arith (LocationSetLattice.currentloc_singleton()))

  (* returns the list of the values associated to at least one bit of the ival.
  For this function Top is not a binding ! *)
  let concerned_bindings_ival ~offsets ~offsetmap ~size acc =
    match offsets with
    | Ival.Float _ -> assert false
      | Ival.Top (mn,mx,_,_) ->
          begin
            match mn, mx with
              | None, _ | _, None ->
                  M.fold (fun _k (_,_,v) acc -> v::acc)
                    offsetmap
                    []
              | Some mn, Some mx ->
                  let concerned_itv =
                    M.concerned_intervals
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
	  let s = Ival.set_of_array s in
          Ival.O.fold
            (fun offset acc ->
               let itv = offset, Int.pred(Int.add offset size) in
               let concerned_itv =
                 M.concerned_intervals Int_Interv.fuzzy_order itv offsetmap
               in
               List.fold_left (fun acc (_,(_,_,v)) -> v::acc) acc concerned_itv)
            s
            acc

  let copy_paste from start stop start_to _to =
    let result =
      let ss = start,stop in
      let to_ss = start_to, Int.sub (Int.add stop start_to) start in
        (* First removing the bindings of the destination interval *)
      let _to = M.remove_itv Int_Interv.fuzzy_order to_ss _to in
      let concerned_itv =
        M.concerned_intervals Int_Interv.fuzzy_order ss from
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

  let copy_offsmap from start stop period_read_ahead =
      let ss = start,stop in
      let concerned_itv =
        M.concerned_intervals Int_Interv.fuzzy_order ss from
      in
      let offset = Int.sub Int.zero start in
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
      let read_ahead =
        match concerned_itv with
          [(bc,ec), (_rc,mc,_vc)] when Int.le bc start && Int.gt ec stop &&
              Int.is_zero (Int.pos_rem period_read_ahead mc)
              ->
                Some ec
        | _ -> None
      in
      read_ahead, List.fold_left treat_interval empty concerned_itv

  (* TODO: factor with find_ival *)
  let copy_ival ~validity ~with_alarms offsets offsetmap size =
    (*Format.eprintf "copy_ival: %a in %a@." Ival.pretty offsets pretty
      offsetmap;*)
    try
      let filtered_by_bound =
        Tr_offset.filter_by_bound_for_reading ~with_alarms offsets size validity
      in
      let pred_size = Int.pred size in
      let i0 = Int.zero, pred_size in
      let value = ref (M.add i0 (Int.zero, Int.one, V.bottom) empty) in
      let find_and_accumulate offset period_read_ahead =
        let end_ = Int.add offset pred_size in
        let read_ahead, new_value =
           copy_offsmap offsetmap offset end_ period_read_ahead
        in
        let _, new_value = join new_value !value in
        value := new_value;
        read_ahead
      in
      begin match filtered_by_bound with
      | Tr_offset.Imprecise(mn ,mx) ->
          let v = find_imprecise mn mx offsetmap in
          value :=
            add_if_not_default
              i0
              (Int.zero, Int.one, v)
              empty
      | Tr_offset.Interval(mn, mx, m) ->
          assert (Int.gt m pred_size);
          let current = ref mn in
          let r = Int.pos_rem mn m in
          while Int.le !current mx do
            let read_ahead = find_and_accumulate !current m in
            let next = Int.add !current m in
            let cursor =
              match read_ahead with
                None -> next
              | Some e ->
                  let max = Int.sub e pred_size in
                  let aligned_b = Int.round_down_to_r ~max ~r ~modu:m in
                  if Int.ge aligned_b next
                  then aligned_b
                  else next
            in
            current := cursor
          done;
      | Tr_offset.Set s ->
          (*Format.eprintf "find_ival(Set) %a@." Ival.pretty (Ival.Set s);*)
          Ival.O.iter (fun x -> ignore (find_and_accumulate x Int.zero)) s;
      end;
      !value
    with Tr_offset.Unbounded -> empty

 let copy_offsmap from start stop =
   snd (copy_offsmap from start stop Int.zero)

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
      M.fold
        (fun (be,en) (offs1, modu1, v1) acc ->
           if
             (V.is_isotropic v1 ==>
                not (Int.is_zero (Int.pos_rem (Int.succ (Int.sub en be)) size)))
             &&
             (not (V.is_isotropic v1) ==>
                (not (Int.equal modu1 size
                 && Int.equal (Int.pos_rem be modu1) offs1
                 && Int.equal (Int.pos_rem (Int.succ en) modu1) offs1)))
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

  let fold_single_bindings ~size f m acc =
    let f_adj ival size v acc =
      Ival.fold_enum ~split_non_enumerable:(-1)
        (fun i acc -> f i size v acc)
        ival
        acc
    in
    fold_whole ~size f_adj m acc

  let shift_ival ival m acc = match ival with
    | Ival.Top _ | Ival.Float _ -> raise Found_Top
      (* Approximating because we do no want to shift
         with too many values *)
    | Ival.Set s ->
	let s = Ival.set_of_array s in
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
    (M.add
        (Int.zero, Int.pred size_in_bits)
        (Int.zero, Int.one, V.singleton_zero)
        empty)

  let reduce_by_int_intervals offsetmap iset =
    try
    M.fold
      (fun itv vv acc ->
         let itv_iset = Int_Intervals.inject [itv] in
         let inter = Int_Intervals.meet itv_iset iset in
         Int_Intervals.fold
           (fun itv acc ->
              M.add itv vv acc)
           inter
           acc)
      offsetmap
      empty
    with Int_Intervals.Error_Top (* from Int_Intervals.fold *) ->
      assert false (* shouldn't happen *)

  let iter_contents f o size =
    let itv = (Int.zero,Int.pred size) in
    let concerned_intervals =
      M.concerned_intervals Int_Interv.fuzzy_order itv o
    in
    (try Int_Interv.check_coverage itv concerned_intervals;
     with Is_not_included -> f V.top);
    List.iter (fun (_,(_,_,b)) -> f b) concerned_intervals

  let fold f m = M.fold f m

  exception Other

  let is m v =
    let f _k (_,_,v1) acc =
      if acc || not (V.equal v v1) then raise Other;
      true
    in
    try
      M.fold f m false
    with Other -> false

end

module Make(V:Lattice_With_Isotropy.S) = struct

  module M = Build(V)
  type tt = { v: M.t; tag:int }
  type y = M.y

  let name = V.name ^ " offsetmap"

  let hash_internal {v=v} = M.M.hash v
  let tag {tag=t} = t

  let equal_internal {v=v} {v=v'} =
    M.M.hash v = M.M.hash v' && M.equal v v'

  let empty = { v = M.empty; tag = 0 }

  let pretty_c_assert_typ s t f fmt v = M.pretty_c_assert_typ s t f fmt v.v
  let pretty_typ t fmt v = M.pretty_typ t fmt v.v
  let pretty fmt v = M.pretty fmt v.v

  let compare l1 l2 = Datatype.Int.compare l1.tag l2.tag

  let rehash_ref = ref (fun _ -> assert false)
  module D = Datatype.Make
    (struct
      type t = tt
      let name = name
      let structural_descr =
        Structural_descr.t_record [| M.packed_descr; Structural_descr.p_int |]
      let reprs = List.map (fun m -> { v = m; tag = -1 }) M.M.reprs
      let equal = ( == )
      let compare = compare
      let hash = tag
      let rehash x = !rehash_ref x
      let copy = Datatype.undefined
      let pretty = pretty
      let internal_pretty_code = Datatype.undefined
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
     end)
  include D

  module OffsetmapHashconsTbl =
    State_builder.Hashconsing_tbl
      (struct
        include D
        let equal_internal = equal_internal
        let hash_internal = hash_internal
        let initial_values = [ empty ]
       end)
      (struct
         let name = name
         let dependencies = [ Ast.self ]
         let size = 137
         let kind = `Internal
       end)

  let wrap =
    let current_tag = ref 1 in
    fun x ->
      let tag = !current_tag in
      let new_offsetmap = { v = x; tag = tag} in
      let result = OffsetmapHashconsTbl.merge new_offsetmap in
      if result == new_offsetmap
      then begin
        let new_current = succ tag in
        current_tag := new_current;
      end;
      result

  let () = rehash_ref := fun x -> wrap x.v

  module Symcacheable = struct
    type t = tt
    let hash = tag
    let equal = (==)
    let sentinel = empty
  end

 module R = struct
   type t = (Int.t * Int.t) list * tt
   let sentinel = [], empty
 end

 module SymetricCache = Binary_cache.Make_Symetric(Symcacheable)(R)
 let () = Project.register_todo_before_clear (fun _ -> SymetricCache.clear ())

 let join m1 m2 =
   if m1 == m2 then [],m1
   else
     let compute x y =
       let singletons, joined = M.join x.v y.v in
       singletons, wrap joined
     in
     SymetricCache.merge compute m1 m2

  let is_empty v = M.is_empty v.v
  let pretty_debug fmt v = M.pretty_debug fmt v.v
  let pretty_compare fmt v = M.pretty_compare fmt v.v

 module Cacheable =
            struct
              type t = tt
              let hash = tag
              let equal = (==)
              let sentinel = empty
            end

  module Cache =
    Binary_cache.Make_Binary(Cacheable)(Cacheable)
  let () = Project.register_todo_before_clear (fun _ -> Cache.clear ())

  let compute_is_included m1 m2 =
    M.is_included m1.v m2.v

  let is_included m1 m2 =
    if m1 == m2 then true
    else
      Cache.merge compute_is_included m1 m2

  let is_included_exn m1 m2 =
    if not (is_included m1 m2) then raise Is_not_included

  let is_included_exn_generic f v1 v2 =
    M.is_included_exn_generic f v1.v v2.v

  type widen_hint = M.widen_hint

  let find i v = M.find i v.v

  let cardinal_zero_or_one v o =
    M.cardinal_zero_or_one v o.v

  (* TODO: cache *)
  let find_imprecise_entire_offsetmap ~validity offsetmap =
    M.find_imprecise_entire_offsetmap ~validity offsetmap.v

  let widen w v1 v2 = wrap (M.widen w v1.v v2.v)

  let find_ival ~conflate_bottom ~validity ~with_alarms a v
      = M.find_ival ~conflate_bottom ~validity ~with_alarms a v.v

  let concerned_bindings_ival ~offsets ~offsetmap ~size =
    M.concerned_bindings_ival ~offsets ~offsetmap:offsetmap.v ~size

  let update_ival ~with_alarms ~validity ~exact ~offsets ~size v y =
    wrap (M.update_ival ~with_alarms ~validity ~exact ~offsets ~size v.v y)

  let overwrite v y o =
    wrap (M.overwrite v.v y o)

  let over_intersection v1 v2 =
    wrap (M.over_intersection v1.v v2.v)

  let from_cstring s =
    wrap (M.from_cstring s)

  let add_whole itv y t =
    wrap (M.add_whole itv y t.v)

  let add_internal itv tr t =
    wrap (M.add_internal itv tr t.v)

  let reduce ival ~size y t =
    wrap (M.reduce ival ~size y t.v)

  let remove_whole itv t =
    wrap (M.remove_whole itv t.v)

  let fold_whole ~size f t acc =
    M.fold_whole ~size f t.v acc

  let fold_single_bindings ~size f t acc =
    M.fold_single_bindings ~size f t.v acc

 let fold_internal f t acc =
    M.fold_internal f t.v acc

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

  let copy_offsmap v a b =
    wrap (M.copy_offsmap v.v a b)

  let copy_ival ~validity ~with_alarms offsets offsetmap size =
    wrap (M.copy_ival ~validity ~with_alarms offsets offsetmap.v size)

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

  let top_stuff condition topify join_locals acc_locals om =
    let locals, r =
      M.top_stuff condition topify join_locals acc_locals om.v
    in
    locals, wrap r

  let iter_contents f o size =
    M.iter_contents f o.v size

  let fold f v = M.fold f v.v

  let is m v = M.is m.v v
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
