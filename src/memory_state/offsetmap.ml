(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Lattice_Interval_Set
open CilE

exception Found_Top
exception Result_is_bottom
exception Result_is_same

type itv = Int.t * Int.t

(* Forward declaration for option controlled withing the Value plugin *)
let precise_unions = ref false

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
  val join : t -> t -> t
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
  val sized_isotropic : y -> size_in_bits:Int.t -> t
  val reciprocal_image : t -> Base.t -> Int_Intervals.t * Ival.t
    (** [reciprocal_image m b] is the set of bits in the offsetmap [m]
        that may lead to Top([b]) and  the set of offsets in [m]
        where one can read an address [b]+_ *)
  val create_initial: v:y -> modu:Int.t -> t
  val reduce_by_int_intervals: t -> Lattice_Interval_Set.Int_Intervals.t -> t
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

  let i159 = My_bigint.of_int 159
  let common_key = 
    [| My_bigint.zero, My_bigint.small_nums.(15);  (*  15 % 5 = 0 *)
       My_bigint.zero, My_bigint.small_nums.(31);  (*  31 % 5 = 1 *)
       My_bigint.zero, My_bigint.small_nums.(7);   (*   7 % 5 = 2 *)
       My_bigint.zero, My_bigint.of_int 63;        (*  63 % 5 = 3 *)
       My_bigint.zero, i159          ;       (* 159 % 5 = 4 *) |]

  let add_M_keyshare (b, e as i) v m =
    let i =
    if My_bigint.is_zero b && My_bigint.le e i159
    then
      let p = (My_bigint.to_int e) mod 5 in
      let candidate = common_key.(p) in
      if My_bigint.equal (snd candidate) e
      then candidate
      else i
    else i
    in
    M.add i v m

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

  let empty = M.empty
  let is_empty v = M.is_empty v

  let shift s v = M.shift s v

  let pretty_c_assert_typ name _typ print_ampamp fmt offs =
    let pretty_binding (bk,ek) (offst,modu,v) =
      let iso = V.is_isotropic v in
      if My_bigint.is_zero (My_bigint.rem bk My_bigint.eight)
        && (My_bigint.equal (My_bigint.pos_rem bk modu) offst)
	&& (iso || (My_bigint.is_zero (My_bigint.rem modu My_bigint.eight)))
      then
        let ek = My_bigint.succ ek in
        if My_bigint.is_zero (My_bigint.rem ek My_bigint.eight)
        then

          let step = if iso then 1 else (My_bigint.to_int modu) / 8 in
          let start = ref ((My_bigint.to_int bk) / 8) in
          let ek = My_bigint.to_int ek in
          let ek = ek / 8 in
          while !start + step <= ek do
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

  let _check_aligned (bk,ek) offs modu =
    My_bigint.equal (My_bigint.rem bk modu) offs (* start is aligned *)
    &&
    My_bigint.is_zero (My_bigint.rem (My_bigint.succ (My_bigint.sub ek offs)) modu)
    (* end is aligned *)

  let pretty_typ typ fmt m =
    let inset_utf8 = Unicode.inset_string () in
    let is_first = ref true in
    let pretty_binding fmt (bk,ek) (offs,modu,v) =
      if !is_first then is_first:=false
      else Format.fprintf fmt "@\n";
      Format.fprintf fmt "@[" ;
      (* Print left-member and return misalign condition *)
      let force_misalign, _printed_type =
        match typ with
          | None ->
              Format.fprintf fmt "[rbits %a to %a]"
                Int.pretty bk Int.pretty ek ;
              (* misalign condition: *)
              not ((My_bigint.equal (My_bigint.rem bk modu) offs)
                   && (My_bigint.equal (My_bigint.sub ek bk) (My_bigint.pred modu)))
              && not (V.is_isotropic v),
              None

          | Some typ ->
              (* returns misalign condition. *)
              Bit_utils.pretty_bits typ
                ~use_align:(not (V.is_isotropic v))
                ~align:offs ~rh_size:modu ~start:bk ~stop:ek fmt
      in
      Format.fprintf fmt " %s@ @[<hv 1>%a@]" inset_utf8 V.pretty v ;
      if force_misalign
      then
        if (My_bigint.equal (My_bigint.rem bk modu) offs)
          && My_bigint.equal (My_bigint.rem (My_bigint.succ ek) modu) offs
        then
          Format.fprintf fmt " repeated %a%%%a "
            Int.pretty offs Int.pretty modu
        else (
          let b_abs = Int.rem (Int.sub bk offs) modu in
          let e_abs = Int.add b_abs (My_bigint.sub ek bk) in
          Format.fprintf fmt "%s%%%a, bits %a to %a "
            (if Int.gt e_abs modu then " repeated " else "")
            Int.pretty modu Int.pretty b_abs Int.pretty e_abs
        );
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
  let conv = My_bigint.to_int64 in
    fold_internal
      ( fun (a, b) (r, m, v) (c, acc) ->
(*      Format.printf "translating %a %a@."
          Int.pretty a Int.pretty b; *)
	let acc =
          if not (My_bigint.equal (My_bigint.succ c) a)
          then snd (New.add_node
            (conv (My_bigint.succ c)) (conv (My_bigint.pred a))
            Int64.zero Int64.one V.top Int64.zero acc)
	  else acc
	in
        let o, t =
          New.add_node
            (conv a) (conv b) (conv r) (conv m) v Int64.zero acc in
        assert (o = Int64.zero);
        b, t
       )
      omap (My_bigint.minus_one, New.empty)
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
        && Int.compare modu (My_bigint.of_int (Bit_utils.sizeofpointer ())) = 0
      then
        let first = Int.round_up_to_r ~min:bi ~r ~modu in
        let last =
          My_bigint.mul
            (My_bigint.pred (My_bigint.div (My_bigint.succ (My_bigint.sub ei first)) modu))
            modu
        in
        if My_bigint.lt last My_bigint.zero then acc2
        else
          Ival.join
            acc2
            (Ival.inject_top (Some first) (Some (My_bigint.add first last)) r modu)
      else acc2
    in
    acc1,acc2
  in
  M.fold treat_binding m (Int_Intervals.bottom, Ival.bottom)

  let create_vv (bi, ei) v =
    if V.is_isotropic v
    then My_bigint.zero, My_bigint.one, v
    else
      let new_modu = Int.length bi ei in
      let new_offs = My_bigint.rem bi new_modu in
      new_offs, new_modu, v

  let remove_whole i m =
    (M.remove_whole Int_Interv.fuzzy_order i m)

 let extract_bits ~start ~stop ~modu v =
   assert (My_bigint.le start stop && My_bigint.le stop modu);
   let start,stop =
     if Cil.theMachine.Cil.little_endian then
       start,stop
     else
       let mmodu = My_bigint.pred modu in
         My_bigint.sub mmodu stop,My_bigint.sub mmodu start
   in
     V.extract_bits ~start ~stop ~size:modu v

 let merge_bits ~offset ~length ~value ~total_length acc =
   assert ( let total_length_i = My_bigint.of_int total_length in
            My_bigint.le (My_bigint.add length offset) total_length_i);
   if Cil.theMachine.Cil.little_endian then
     V.little_endian_merge_bits ~offset ~value ~total_length acc
   else
     V.big_endian_merge_bits ~offset ~value ~total_length ~length acc

 exception Bottom_found

 let extract_bits_and_stitch ~topify ~conflate_bottom (bi, ei) concerned_intervals =
   try
     Int_Interv.check_coverage (bi,ei) concerned_intervals;
     let total_length = My_bigint.to_int (Int.length bi ei)
     in
     let treat_concerned_interval
         ((b1,e1),(offs,modu,v)) (acc_inform, acc_value) =
         (*            Format.printf "find debugging bi:%a ei:%a b1:%a e1:%a@."
                       Int.pretty bi Int.pretty ei
                       Int.pretty b1 Int.pretty e1; *)
         let treat_value offs1 (acc_inform, acc_value) =
           (*            Format.printf "find treat_value debugging offs:%a@."
                         Int.pretty offs; *)
           let offset = My_bigint.sub offs1 bi in
           let offset,start =
             if My_bigint.lt offset My_bigint.zero
             then
               My_bigint.zero, My_bigint.neg offset
             else
               offset, My_bigint.zero
           in
           let stop = My_bigint.pred modu in
           let stop =
             let end_ = My_bigint.min (My_bigint.pred (My_bigint.add offs1 modu)) e1 in
             let over = My_bigint.sub end_ ei in
             if My_bigint.gt over My_bigint.zero
             then
               My_bigint.sub stop over
             else
               stop
           in
           assert (not (V.is_isotropic v));
           let inform_extract_pointer_bits, value =
             extract_bits ~topify ~start ~stop ~modu v
           in
           inform_extract_pointer_bits || acc_inform,
           merge_bits ~topify ~conflate_bottom
             ~offset ~length:(Int.length start stop)
             ~value ~total_length acc_value
         in
         let length =Int.length b1 e1 in
         if V.is_isotropic v then begin
             if conflate_bottom && V.equal V.bottom v then raise Bottom_found;
             let offs_start = My_bigint.max b1 bi in
             let offs_stop = My_bigint.min e1 ei in
             let offset = My_bigint.sub offs bi in
             let offset =
               if My_bigint.lt offset My_bigint.zero then My_bigint.zero else offset
             in
             acc_inform,
             merge_bits ~topify ~conflate_bottom 
               ~offset ~length:(Int.length offs_start offs_stop)
               ~value:v ~total_length acc_value
           end
         else if (My_bigint.is_zero (My_bigint.rem length modu)) &&
             (My_bigint.equal (My_bigint.rem b1 modu) offs)
         then
           Int.fold
             treat_value
             ~inf:(My_bigint.max b1 (My_bigint.round_down_to_r ~max:bi ~r:offs ~modu))
             ~sup:(My_bigint.min e1 ei)
             ~step:modu
             (acc_inform, acc_value)
         else
           acc_inform,
           V.join (V.topify_with_origin_kind topify v) acc_value
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
 let find ~topify ~conflate_bottom ((bi,ei) as i) m period_read_ahead =
   assert (My_bigint.le bi ei);
   let concerned_intervals =
     M.concerned_intervals Int_Interv.fuzzy_order i m
   in
     match concerned_intervals with
     | [(b,e),(offs,modu,v)] ->
         if (My_bigint.le b bi) && (My_bigint.ge e ei) then
           let isotropic = V.is_isotropic v in
           if isotropic
             || ((My_bigint.equal modu (My_bigint.length bi ei))
                  && (My_bigint.equal (My_bigint.rem bi modu) offs))
           then
             let read_ahead =
               if My_bigint.is_zero (My_bigint.rem period_read_ahead modu)
               then Some e
               else None
             in
             false, read_ahead, v
           else
             let inform, v =
               if (* [(bi-offs)/modu = (ei-offs)/modu]
                     i.e. [bi] and [ei] are in the same slice. *)
                 My_bigint.equal
                   (My_bigint.pos_div (My_bigint.sub bi offs) modu)
                   (My_bigint.pos_div (My_bigint.sub ei offs) modu)
               then
                 extract_bits
                   ~topify
                   ~start:(My_bigint.pos_rem (My_bigint.sub bi offs) modu)
                   ~stop:(My_bigint.pos_rem (My_bigint.sub ei offs) modu)
                   ~modu
                   v
               else
                 extract_bits_and_stitch ~topify ~conflate_bottom
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
           extract_bits_and_stitch ~topify ~conflate_bottom
             i concerned_intervals
         in
         inform, None, v

 let find_imprecise bi ei m =
   assert (My_bigint.le bi ei);
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
        add_M_keyshare i vv m
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
                  if My_bigint.gt bi min then raise More_than_one;
                  if not (V.cardinal_zero_or_one v) then raise More_than_one;
                  (My_bigint.succ ei))
                offsetmap
                min
            in
            My_bigint.gt up_to max
          with More_than_one -> false
        end
    in
    r

  let find_imprecise_entire_offsetmap ~validity m =
    match validity with
    | Base.All -> V.top
    | Base.Periodic _ -> assert false (* TODO *)
    | Base.Known (bound_min,bound_max) | Base.Unknown (bound_min,bound_max)
          when My_bigint.lt bound_max bound_min ->
        V.bottom
    | Base.Known (bound_min,bound_max) | Base.Unknown (bound_min,bound_max) ->
        let next = ref bound_min in
        try
          let r =
            M.fold
              (fun (bi,ei) (_r,_m,v) acc ->
                if My_bigint.equal bi !next
                then begin
                    next := My_bigint.succ ei;
                    V.join
                      acc
                      ( if V.is_isotropic v
                        then v
                        else V.topify_misaligned_read_origin v )
                  end
                else begin
                    assert (My_bigint.gt bi !next);
                    raise Found_Top
                  end;)
              m
              V.bottom
          in
          if My_bigint.gt !next bound_max
          then r
          else V.top
        with Found_Top -> V.top

  (* Merge neighboring values with the inserted value if necessary.*)
  let add_internal ((bi,ei) as i) (_new_offs,_new_modu,v as new_vv) m =
    let (new_offs,new_modu,_) as new_vv =
      if V.is_isotropic v then (My_bigint.zero,My_bigint.one,v) else new_vv
    in
    let v_is_singleton = V.cardinal_zero_or_one v in
    let extend_left =
      v_is_singleton || My_bigint.equal new_offs (My_bigint.pos_rem bi new_modu)
    in
    let extend_right =
      v_is_singleton || My_bigint.is_zero
      (My_bigint.pos_rem (My_bigint.sub (My_bigint.pred new_offs) ei) new_modu)
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


let translate_to_old m =
  let conv = My_bigint.of_int64 in
  let treat_binding o abs_max r m v acc =
    let o = conv o in
    let itv = o, conv abs_max in
    let m = conv m in
    let r = My_bigint.pos_rem (My_bigint.add (conv r) o) m in
    let vv = r, m, v in
    add_internal itv vv acc
  in
  New.fold treat_binding m empty

  let top_stuff f topify join_locals acc_locals offsm =
    assert (not (is_empty offsm));
    M.fold
      (fun (_,_ as i) (r,m,v) (acc_locals, acc_o as acc) ->
         assert (My_bigint.lt r m);
         assert (My_bigint.le My_bigint.zero r);
         assert (if V.is_isotropic v then My_bigint.is_one m else true);
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
      let v = V.anisotropic_cast ~size:(My_bigint.length bi ei) v in
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
      let ei = My_bigint.pred (My_bigint.add bi size) in
      let i = bi, ei in
      let rem, modu, old_v = M.find i m in
      if not (My_bigint.equal rem (My_bigint.pos_rem bi modu))
      then raise Result_is_same;
      if not (My_bigint.equal modu size)
      then raise Result_is_same;
      let new_v = V.narrow old_v v in
      if (V.equal v new_v)
      then raise Result_is_same;
        add_M_keyshare i (rem, modu, v) (M.remove i m)
    with Int_Interv.Cannot_compare_intervals ->
      raise Result_is_same

  let add_whole i v m =
    let removed =  (M.remove_whole Int_Interv.fuzzy_order i m)
    in
    (add i v removed)

  let split_interval be en offs modu ~treat_aligned ~treat_misaligned acc =
    (* Format.printf "split_interval:be=%a en=%a modu=%a@\n" Int.pretty be
      Int.pretty en Int.pretty modu; *)
    if My_bigint.lt (My_bigint.length be en) modu
    then treat_misaligned be en acc
    else
      let start_aligned = My_bigint.round_up_to_r be offs modu in
      (* [start_aligned] is equal to [offs] modulo [modu] *)
      let result1 =
        if not (My_bigint.equal start_aligned be)
        then begin
(*        Format.printf "split_interval:treat_misaligned:be=%Ld en=%Ld@\n"
            be (My_bigint.pred start_aligned);*)
          treat_misaligned be (My_bigint.pred start_aligned) acc
        end
        else acc
      in
      let last_aligned =
        My_bigint.round_down_to_r en (My_bigint.pos_rem (My_bigint.pred offs) modu) modu
      (* [last_aligned] is equal to [offs-1] modulo [modu] *)
      in
      let result2 =
        treat_aligned
          ~inf:start_aligned
          ~sup:last_aligned
          result1
      in
      let result3 =
        if not (My_bigint.equal last_aligned en)
        then begin
(*        Format.printf "split_interval:treat_misaligned:be=%Ld en=%Ld@\n"
            (My_bigint.succ last_aligned) en;*)
          treat_misaligned (My_bigint.succ last_aligned) en result2
        end
        else result2
      in
      (* Format.printf "split_interval:finished@\n";*)
      result3

  let from_string s =
    let r = ref empty in
    let char_width = 8 in
    let l = String.length s in
    for i = 0 to pred l do
      let b = i * char_width in
      let e = pred (b + char_width) in
      r := add (My_bigint.of_int b, My_bigint.of_int e) (V.of_char s.[i]) !r
    done;
    let b = l * char_width in
    let e = pred (b + char_width) in
    add (My_bigint.of_int b, My_bigint.of_int e) (V.singleton_zero) !r

  let from_wstring s =
    let pwchar_width = 
      My_bigint.of_int (pred (Cil.bitsSizeOf Cil.theMachine.Cil.wcharType)) 
    in
    let addw (b,acc) wchar =
      let e = My_bigint.add b pwchar_width in
      My_bigint.succ e, (add (b, e) (V.of_int64 wchar) acc)
    in
    snd (List.fold_left addw (My_bigint.zero,empty) s)

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
                  if My_bigint.lt ex ei then raise Is_not_included
            end;
            ignore
              (List.fold_right
                 (fun ((bx,ex),(offs1,modu1,v1)) acc  ->
                    if My_bigint.gt bx acc then raise Is_not_included;
                    (* [m1] has top for something present in [m2] *)
                    v_is_included_exn v1 v2; (* raise Is_not_included
                                                if [v2] does not include [v1] *)
                    if not (V.is_isotropic v2)
                    then begin
                      if (not (V.is_isotropic v1))
                        && not ((My_bigint.equal offs1 offs2 &&
                                    My_bigint.equal modu1 modu2))
                      then raise Is_not_included;
                      (* The alignment is different *)

                      if not (My_bigint.equal bx bi) &&
                        (not (My_bigint.is_zero (My_bigint.rem (My_bigint.sub bx offs1) modu1)))
                      then raise Is_not_included;
                      if not (My_bigint.equal ex ei) &&
                        (not (My_bigint.is_zero
                                (My_bigint.rem (My_bigint.sub (My_bigint.pred offs1) ex) modu1)))
                      then raise Is_not_included;
                      (* the interval [i] is covered by pieces only in [m1],
                         which is less precise than what is in [m2] *)
                    end;
                    My_bigint.succ ex)
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

  let unsafe_join, widen =
    let generic (f:size:My_bigint.t -> offs:My_bigint.t option -> V.t -> V.t -> V.t) m1 m2 =
      let r = if m1 == m2 then m1 else
        begin
          M.fold
            (fun (be,en) (offs1, modu1, v1 as triple1) acc ->
               let itvs2 =
                 M.concerned_intervals Int_Interv.fuzzy_order (be,en) m2
               in
               List.fold_left
                 (fun acc ((xb,xe),(offs2,modu2,v2 as triple2)) ->
                    let inter_b = My_bigint.max xb be in
                    let inter_e = My_bigint.min en xe in
                    let do_topify acc =
                      add_internal (inter_b,inter_e)
                        (My_bigint.zero, My_bigint.one,
                         f ~size:My_bigint.one ~offs:None
                           (V.topify_merge_origin v1)
                           (V.topify_merge_origin v2))
                        acc
                    in
                    let treat_misaligned _be2 _en2 acc =
                      if Int_Int_V.equal triple1 triple2
                      then add_internal (inter_b,inter_e) triple1 acc
                      else if My_bigint.equal offs1 offs2 && My_bigint.equal modu1 modu2
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
                      || (My_bigint.equal offs2 offs1 && My_bigint.equal modu1 modu2) then
                        let treat_aligned ~inf ~sup acc =
                          let new_itv = (inf,sup) in
                          let new_v = f ~size:modu1 ~offs:(Some inf) v1 v2 in
                          add_internal new_itv (offs1,modu1,new_v) acc
                        in split_interval inter_b inter_e offs1 modu1
                             ~treat_aligned
                             ~treat_misaligned
                             acc
                    else
                      let (_, _, v1') =
                        find
                          ~topify:Origin.K_Merge
                          ~conflate_bottom:false
                          (inter_b, inter_e) m1 My_bigint.zero
                      and (_, _, v2') =
                        find
                          ~topify:Origin.K_Merge
                          ~conflate_bottom:false
                          (inter_b, inter_e) m2 My_bigint.zero
                      in
                      let v' = V.join v1' v2' in
                      let l = My_bigint.length inter_b inter_e in
                      add_internal (inter_b,inter_e)
                        (My_bigint.rem inter_b l, l , v') acc
                 )
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
    let f_for_join ~size ~offs:_ v w =
      let joined = V.join v w in
      V.anisotropic_cast ~size joined 
    in
    generic f_for_join,
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
                if My_bigint.ge bi continue
                then ei, M.add itv vv acc
                else ei, acc)
              m1
              (continue, acc))
      with M.No_such_binding ->
        (* m1 is finished *)
        (M.fold
            (fun (bi, ei as itv) vv (_cont, acc) ->
              if My_bigint.ge bi continue
              then ei, M.add itv vv acc
              else ei, acc)
            m2
            (continue, acc))
    and treat_the_lowest_binding (b1,_e1 as itv1) (_,_,_v1 as v1) (b2,_e2 as itv2) (_,_,_v2 as v2) acc =
      (*      Format.printf "treat_the_lowest_binding: %a..%a -> %a    %a..%a -> %a@."
              Int.pretty b1 Int.pretty _e1 V.pretty _v1
              Int.pretty b2 Int.pretty _e2 V.pretty _v2; *)
      let itv, vv, first_m, other_m =
        if My_bigint.lt b1 b2
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
          if My_bigint.equal bc next
          then acc
          else begin
              (* add a binding to vv in the result where there is no
                 binding in other_m *)
              assert (My_bigint.lt next bc);
              add_M_keyshare (next, My_bigint.pred bc) vv acc
            end
        in
        let same_align = My_bigint.equal moduc modu && My_bigint.equal offsc offs in
        if (not (My_bigint.is_one moduc)) &&
          (not (My_bigint.is_one modu)) &&
          (not same_align)
        then begin
            Format.printf "An assumption made for the implementation of this tool turns out to be invalid. Please report the appearance of this message. If possible, please provide a reasonably-sized example that provokes it. The correctness of the computations is not affected and the analysis will continue\n";
            raise Not_aligned
          end;
        let inter_vv =
          if same_align
          then (offs, modu, V.narrow v vc)
          else (My_bigint.zero, My_bigint.one,
               V.narrow (V.under_topify v) (V.under_topify vc))
        in
        let over_reach = My_bigint.gt ec e in
        let actual_end =
          if over_reach
          then e
          else ec
        in
        let new_next = My_bigint.succ actual_end in
        let new_acc = add_M_keyshare (bc, actual_end) inter_vv acc in
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
          if My_bigint.gt next e
          then acc
          else add_M_keyshare (next, e) vv acc
        in
        over_intersection_rec (My_bigint.succ e) acc
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
    let r1 = unsafe_join m1 m2 in
    assert (let r2 = unsafe_join m2 m1 in
            equal r1 r2 ||
              (Format.printf
                 "Non commuting join %a@\n with %a@\n leads to %a@\n and %a@\n"
                 pretty m1 pretty m2
                 pretty r1 pretty r2;
               false));
    r1
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
    if not (My_bigint.equal l1 l2) then s, r  else
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
      if (My_bigint.lt e mn)
        || (My_bigint.ge b (My_bigint.add mx size))
      then (* non intersecting interval *)
        add_internal itv vv acc
      else begin
        let acc,new_b =
          if My_bigint.lt b mn
          then
            (add_internal
               (b,My_bigint.pred mn)
               vv
               acc,
             mn)
          else acc,b
        in
        let acc,new_e =
          if My_bigint.gt e (My_bigint.pred (My_bigint.add mx size))
          then
            let mx = My_bigint.add mx size in
            let acc = add_internal (mx,e) vv acc in
            acc, My_bigint.pred mx
          else acc,e
        in
        let new_r,new_modu,cond =
          if My_bigint.equal m size
          then
            if (My_bigint.equal r rem && My_bigint.equal m modu) || V.is_isotropic value
            then r,m,true
            else My_bigint.zero,My_bigint.one,false
          else begin
            assert (My_bigint.lt size m);
            let number = My_bigint.succ (My_bigint.div (My_bigint.sub mx mn) m) in
            Kernel.result ~current:true ~once:true
              "more than %d(%a) locations to update in array. Approximating."
              !Lattice_Interval_Set.plevel
              Int.pretty number;
            My_bigint.zero,My_bigint.one,false
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
        let e_max = My_bigint.add offset (My_bigint.pred size) in
        let concerned_intervals =
          M.concerned_intervals
            Int_Interv.fuzzy_order
            (offset, e_max)
            offsetmap_orig
        in
        let treat_itv acc ((b,e), (rem,modu,value)) =
          let new_b = My_bigint.max b offset in
          let new_e = My_bigint.min e e_max in
          let new_v = V.join v value in
          add_internal (new_b, new_e) (rem, modu, new_v) acc
        in
        List.fold_left treat_itv offsetmap_orig concerned_intervals
      end
    else
    add_approximate_including_spaces offset offset
      (My_bigint.pos_rem offset size) size size v offsetmap_orig

  let add_imprecise mn mx v offsetmap_orig =
     add_approximate_including_spaces mn mx My_bigint.zero My_bigint.one My_bigint.one
      v offsetmap_orig

  let overwrite offsetmap_orig v o =
    let v = V.topify_with_origin o v in
    M.fold
      (fun itv (_offs,_modu,bound_v) acc ->
        let new_v = V.join (V.topify_with_origin o bound_v) v in
        add_internal itv (My_bigint.zero, My_bigint.one, new_v) acc)
      offsetmap_orig
      empty

(* add a binding in the case where the "offset" part of the location
   is known only as an interval of integers modulo *)
  let add_top_binding_offsetmap mn mx r m size v existing_offsetmap =
    let number = My_bigint.succ (My_bigint.div (My_bigint.sub mx mn) m) in
    let plevel = !Lattice_Interval_Set.plevel in
    if My_bigint.le number (My_bigint.of_int plevel) && (My_bigint.gt m size)
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
    let vv = if V.is_isotropic v then My_bigint.zero,My_bigint.one,v
    else My_bigint.zero,modu,v
    in
    add_if_not_default (My_bigint.zero,Bit_utils.max_bit_address ()) vv empty

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
      let pred_size = My_bigint.pred size in
      let find_and_accumulate offset period_read_ahead =
        let itv = offset, My_bigint.add offset pred_size in
        let new_inform, read_ahead, new_value =
          find ~topify:Origin.K_Misalign_read ~conflate_bottom
            itv offsetmap period_read_ahead
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
          assert(My_bigint.gt m pred_size);
          let current = ref mn in
          let r = My_bigint.pos_rem mn m in
          while My_bigint.le !current mx do
            let read_ahead = find_and_accumulate !current m in
            let next = My_bigint.add !current m in
            let cursor =
              match read_ahead with
                None -> next
              | Some e ->
                  let max = My_bigint.sub e pred_size in
                  let aligned_b = My_bigint.round_down_to_r ~max ~r ~modu:m in
                  if My_bigint.ge aligned_b next
                  then aligned_b
                  else next
            in
            current := cursor
          done;
      | Tr_offset.Set s ->
          (*Format.eprintf "find_ival(Set) %a@." Ival.pretty (Ival.Set s);*)
          Ival.O.iter (fun x -> ignore (find_and_accumulate x My_bigint.zero)) s;
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
    if (not !precise_unions) || V.cardinal_zero_or_one result_old
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


let boris = false

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
          let itv = offset, My_bigint.pred(My_bigint.add offset size) in
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
	  if boris && My_bigint.ge (My_bigint.add mn (My_bigint.shift_left m My_bigint.four)) mx
	  then begin
	      try
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
                      My_bigint.zero, My_bigint.pred period
		in
		let min = My_bigint.to_int64 min
		and max = My_bigint.to_int64 max
		and mn = My_bigint.to_int64 mn
		and mx = My_bigint.to_int64 mx
		and period = My_bigint.to_int64 m
		and size = My_bigint.to_int64 size
		in
		let t2 =
                  New.update_ival min max exact mn mx period size  new_omap v
		in
		translate_to_old t2
	      with
	      | Not_translatable ->
		  add_top_binding_offsetmap
                    mn mx (My_bigint.pos_rem mn m) m
                    size v offsetmap_orig
	    end
	  else

            let res =
              add_top_binding_offsetmap
                mn mx (My_bigint.pos_rem mn m) m
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
              My_bigint.zero, My_bigint.pred period
              in
              let min = My_bigint.to_int64 min
              and max = My_bigint.to_int64 max
              and mn = My_bigint.to_int64 mn
              and mx = My_bigint.to_int64 mx
              and period = My_bigint.to_int64 m
              and size = My_bigint.to_int64 size
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
                      (mn, (My_bigint.pred (My_bigint.add mx size)))
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
               let itv = offset, My_bigint.pred(My_bigint.add offset size) in
               let concerned_itv =
                 M.concerned_intervals Int_Interv.fuzzy_order itv offsetmap
               in
               List.fold_left (fun acc (_,(_,_,v)) -> v::acc) acc concerned_itv)
            s
            acc

  let copy_paste from start stop start_to to_ =
    let result =
      let ss = start,stop in
      let to_ss = start_to, My_bigint.sub (My_bigint.add stop start_to) start in
        (* First removing the bindings of the destination interval *)
      let to_ = M.remove_itv Int_Interv.fuzzy_order to_ss to_ in
      let concerned_itv =
        M.concerned_intervals Int_Interv.fuzzy_order ss from
      in
      let offset = My_bigint.sub start_to start in
      let treat_interval acc (i,(offs,modu, v)) =
        let new_vv = (My_bigint.pos_rem (My_bigint.add offs offset) modu),modu,v in
        let src_b,src_e = Int_Interv.clip_itv ss i in
        let dest_i = My_bigint.add src_b offset, My_bigint.add src_e offset in
          (*Format.printf "treat_itv: ib=%a ie=%a v=%a dib=%a die=%a@\n"
            Int.pretty (fst i) Int.pretty (snd i)
            V.pretty v
            Int.pretty (fst dest_i) Int.pretty (snd dest_i);*)
          add_internal dest_i new_vv acc
      in
        List.fold_left treat_interval to_ concerned_itv
    in
      (* Format.printf "Offsetmap.copy_paste from:%a start:%a stop:%a start_to:%a to:%a result:%a@\n"
        pretty from
        Int.pretty start
        Int.pretty stop
        Int.pretty start_to
        pretty _to
        pretty result;*)
      result

  let copy_merge from start stop start_to to_ =
    let old_value =
      copy_paste to_ start_to
        (My_bigint.sub (My_bigint.add start_to stop) start)
        start empty
    in
    let merged_value = join old_value from in
    copy_paste merged_value start stop start_to to_

  let copy_offsmap from start stop period_read_ahead =
      let ss = start,stop in
      let concerned_itv =
        M.concerned_intervals Int_Interv.fuzzy_order ss from
      in
      let offset = My_bigint.sub My_bigint.zero start in
      let treat_interval acc (i,(offs,modu, v)) =
        let new_vv = (My_bigint.pos_rem (My_bigint.add offs offset) modu),modu,v in
        let src_b,src_e = Int_Interv.clip_itv ss i in
        let dest_i = My_bigint.add src_b offset, My_bigint.add src_e offset in
          (*Format.printf "treat_itv: ib=%a ie=%a v=%a dib=%a die=%a@\n"
            Int.pretty (fst i) Int.pretty (snd i)
            V.pretty v
            Int.pretty (fst dest_i) Int.pretty (snd dest_i);*)
          add_internal dest_i new_vv acc
      in
      let read_ahead =
        match concerned_itv with
          [(bc,ec), (_rc,mc,_vc)] when My_bigint.le bc start && My_bigint.gt ec stop &&
              My_bigint.is_zero (My_bigint.pos_rem period_read_ahead mc)
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
      let pred_size = My_bigint.pred size in
      let i0 = My_bigint.zero, pred_size in
      let value = ref (add_M_keyshare i0 (My_bigint.zero, My_bigint.one, V.bottom) empty) in
      let find_and_accumulate offset period_read_ahead =
        let end_ = My_bigint.add offset pred_size in
        let read_ahead, new_value =
           copy_offsmap offsetmap offset end_ period_read_ahead
        in
        let new_value = join new_value !value in
        value := new_value;
        read_ahead
      in
      begin match filtered_by_bound with
      | Tr_offset.Imprecise(mn ,mx) ->
          let v = find_imprecise mn mx offsetmap in
          value :=
            add_if_not_default
              i0
              (My_bigint.zero, My_bigint.one, v)
              empty
      | Tr_offset.Interval(mn, mx, m) ->
          assert (My_bigint.gt m pred_size);
          let current = ref mn in
          let r = My_bigint.pos_rem mn m in
          while My_bigint.le !current mx do
            let read_ahead = find_and_accumulate !current m in
            let next = My_bigint.add !current m in
            let cursor =
              match read_ahead with
                None -> next
              | Some e ->
                  let max = My_bigint.sub e pred_size in
                  let aligned_b = My_bigint.round_down_to_r ~max ~r ~modu:m in
                  if My_bigint.ge aligned_b next
                  then aligned_b
                  else next
            in
            current := cursor
          done;
      | Tr_offset.Set s ->
          (*Format.eprintf "find_ival(Set) %a@." Ival.pretty (Ival.Set s);*)
          Ival.O.iter (fun x -> ignore (find_and_accumulate x My_bigint.zero)) s;
      end;
      !value
    with Tr_offset.Unbounded -> empty

 let copy_offsmap from start stop =
   snd (copy_offsmap from start stop My_bigint.zero)

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
                not (My_bigint.is_zero (My_bigint.pos_rem (My_bigint.succ (My_bigint.sub en be)) size)))
             &&
             (not (V.is_isotropic v1) ==>
                (not (My_bigint.equal modu1 size
                 && My_bigint.equal (My_bigint.pos_rem be modu1) offs1
                 && My_bigint.equal (My_bigint.pos_rem (My_bigint.succ en) modu1) offs1)))
           then begin
             Format.printf "Got size %a modu1:%a@\n" Int.pretty size
               Int.pretty modu1;
             raise (Invalid_argument "Offsetmap.Make.fold")
           end ;
           let offs1 = if V.is_isotropic v1 then My_bigint.pos_rem be size
           else offs1
           in
           f
             (Ival.inject_top
                (Some be)
                (Some (My_bigint.sub (My_bigint.succ en) size))
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
          | Some acc -> Some (join acc shifted))
        s acc

  let sized_isotropic v ~size_in_bits =
    assert (My_bigint.gt size_in_bits My_bigint.zero);
    assert (V.is_isotropic v);
    (add_M_keyshare
        (My_bigint.zero, My_bigint.pred size_in_bits)
        (My_bigint.zero, My_bigint.one, v)
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
    let itv = (My_bigint.zero,My_bigint.pred size) in
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
       end)
  let () = Ast.add_monotonic_state OffsetmapHashconsTbl.self

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

 module SymetricCache = Binary_cache.Make_Symetric(Symcacheable)(Symcacheable)
 let () = Project.register_after_set_current_hook ~user_only:false (fun _ -> SymetricCache.clear ())

 let join m1 m2 =
   if m1 == m2 then m1
   else
     let compute x y =
       let joined = M.join x.v y.v in
       wrap joined
     in
     SymetricCache.merge compute m1 m2

  let is_empty v = M.is_empty v.v
  let pretty_debug fmt v = M.pretty_debug fmt v.v
  let _pretty_compare fmt v = M.pretty_compare fmt v.v

 module Cacheable =
            struct
              type t = tt
              let hash = tag
              let equal = (==)
              let sentinel = empty
            end

  module Cache =
    Binary_cache.Make_Binary(Cacheable)(Cacheable)
  let () = Project.register_after_set_current_hook ~user_only:false (fun _ -> Cache.clear ())

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

  let sized_isotropic v ~size_in_bits = wrap (M.sized_isotropic v ~size_in_bits)

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
