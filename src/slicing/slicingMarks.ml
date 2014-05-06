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

(** Everything related with the marks. Mainly quite low level function. *)

(**/**)

let debug = false

(**/**)

(** a [Mark] is used to represent some information about the status of
 * a PDF element in a slice.
 *)
module Mark : sig
  val bottom : SlicingInternals.mark
  val spare : SlicingInternals.mark
  val data : SlicingInternals.mark
  val ctrl : SlicingInternals.mark
  val addr : SlicingInternals.mark
  val mk_adc : bool -> bool -> bool -> SlicingInternals.mark

  val is_bottom : SlicingInternals.mark -> bool
  val is_top : SlicingInternals.mark -> bool
  val is_included : SlicingInternals.mark -> SlicingInternals.mark -> bool

   (** this operation has to be commutative.
    It is used to merge two slices into one.
   *)
  val merge : SlicingInternals.mark -> SlicingInternals.mark -> SlicingInternals.mark

  val inter : SlicingInternals.mark -> SlicingInternals.mark -> SlicingInternals.mark

  (** this operation add a new information to the old value.
   * @return (new_mark, is_new)
             where is_new=true if the new_mark is not included in the old one.
   *)
  val combine : old:SlicingInternals.mark -> SlicingInternals.mark -> bool * SlicingInternals.mark

  (** [minus m1 m2] provides the mark [m] that you have to merge with [m2] to
    * get at least [m1]. So : [m1 <= m U m2]
    * If [m1 <= m2] then [m = bot].
    * *)
  val minus : SlicingInternals.mark -> SlicingInternals.mark -> SlicingInternals.mark

  val pretty : Format.formatter -> SlicingInternals.mark -> unit

end = struct

  let spare = SlicingInternals.Spare

  (* Internal constructor *)
  let create_adc a d c =  SlicingInternals.Cav (PdgTypes.Dpd.make ~a ~d ~c ())

  let bottom = SlicingInternals.Cav PdgTypes.Dpd.bottom
  let top = SlicingInternals.Cav PdgTypes.Dpd.top

  let addr = create_adc true false  false
  let data = create_adc false true  false
  let ctrl = create_adc false false true

  let m_ad = create_adc true  true  false
  let m_ac = create_adc true  false true
  let m_dc = create_adc false true  true

  let create adc =
    match adc with
      | false, false, false -> bottom
      | true,  false, false -> addr
      | false, true,  false -> data
      | false, false, true  -> ctrl
      | true,  true,  false -> m_ad
      | true,  false, true  -> m_ac
      | false, true,  true  -> m_dc
      | true,  true,  true  -> top

  (* External constructor sharing same values *)
  let mk_adc a d c = create (a, d, c)
  let mk_mark dpd = create (PdgTypes.Dpd.adc_value dpd)

  let is_bottom m = (m = bottom)
  let is_top m = (m = top)

  let is_included m1 m2 =
     match m1,m2 with
       | SlicingInternals.Spare, SlicingInternals.Spare -> true
       | SlicingInternals.Spare, SlicingInternals.Cav _ -> not (is_bottom m2)
       | SlicingInternals.Cav _, SlicingInternals.Spare -> is_bottom m1 
       | SlicingInternals.Cav d1, SlicingInternals.Cav d2 -> PdgTypes.Dpd.is_included d1 d2

   let merge m1 m2 =
     match m1,m2 with
       | SlicingInternals.Spare, SlicingInternals.Spare -> m1
       | SlicingInternals.Spare, SlicingInternals.Cav _ -> if is_bottom m2 then m1 else m2
       | SlicingInternals.Cav _,  SlicingInternals.Spare -> if is_bottom m1 then m2 else m1
       | SlicingInternals.Cav d1, SlicingInternals.Cav d2 -> mk_mark (PdgTypes.Dpd.combine d1 d2)

  let inter m1 m2 =
    if is_bottom m1 then m1
    else if is_bottom m2 then m2
    else (* m1 and m2 are not bottom => the result cannot be bottom *)
      match m1,m2 with
        | SlicingInternals.Spare, _ -> m1
        | _, SlicingInternals.Spare -> m2
        | SlicingInternals.Cav d1, SlicingInternals.Cav d2 ->
            let m = mk_mark (PdgTypes.Dpd.inter d1 d2) in
              if is_bottom m then spare else m

   let combine ~old m =
     match old, m with
     | SlicingInternals.Spare, SlicingInternals.Spare -> (false, old)
     | SlicingInternals.Cav old_d, SlicingInternals.Spare ->
         if PdgTypes.Dpd.is_bottom old_d then (true, m) else (false, old)
     | SlicingInternals.Spare, SlicingInternals.Cav new_d ->
         if PdgTypes.Dpd.is_bottom new_d then (false, old) else (true, m)
     | SlicingInternals.Cav old_d, SlicingInternals.Cav new_d ->
         let new_d = PdgTypes.Dpd.combine old_d new_d in
         if old_d = new_d then (false, old) else (true, mk_mark new_d)

  let minus m1 m2 =
     match m1,m2 with
       | SlicingInternals.Spare, SlicingInternals.Spare -> bottom
       | SlicingInternals.Spare, SlicingInternals.Cav d2 -> if PdgTypes.Dpd.is_bottom d2 then m1 else bottom
       | SlicingInternals.Cav _, SlicingInternals.Spare -> m1 (* even if [PdgTypes.Dpd.is_bottom d1] because m1 = bot *)
       | SlicingInternals.Cav d1, SlicingInternals.Cav d2 -> mk_mark (PdgTypes.Dpd.minus d1 d2)

  let pretty fmt m =
    match m with
    | SlicingInternals.Cav d -> PdgTypes.Dpd.pretty fmt d
    | SlicingInternals.Spare -> Format.fprintf fmt "[ S ]"

end

(** a [SlicingInternals.pdg_mark] is associated with each element of the PDG in a slice.
  * The first component gives the mark propagated from a user request, while
  * the second one is used to propagate informations to the called functions.
  *)

let mk_m1 m1 = { SlicingInternals.m1 = m1 ; m2 = Mark.bottom } 

let mk_m2 m2 = { SlicingInternals.m1 = Mark.bottom ; m2 = m2} 

let bottom_mark = { SlicingInternals.m1 = Mark.bottom ; m2 = Mark.bottom } 
let user_mark m = Mark.merge m.SlicingInternals.m1 m.SlicingInternals.m2
let is_bottom_mark m = (Mark.is_bottom (user_mark m))

module MarkPair = struct
  let mk_m1_spare = mk_m1 Mark.spare
  let mk_gen_spare = mk_m2 Mark.spare

  let is_top m = (Mark.is_top m.SlicingInternals.m1) && (Mark.is_top m.SlicingInternals.m2)

  let is_ctrl m = (Mark.is_included Mark.ctrl (user_mark m))
  let is_addr m = (Mark.is_included Mark.addr (user_mark m))
  let is_data m = (Mark.is_included Mark.data (user_mark m))

  let is_spare m =
    not (is_bottom_mark m) && not (is_ctrl m || is_addr m || is_data m)

  let compare = SlicingInternals.compare_pdg_mark

  let _is_included ma mb =
    Mark.is_included ma.SlicingInternals.m1 mb.SlicingInternals.m1
    && Mark.is_included ma.SlicingInternals.m2 mb.SlicingInternals.m2

  let pretty fmt m =
    Format.fprintf fmt "@[<hv><%a,@ %a>@]" 
      Mark.pretty m.SlicingInternals.m1 Mark.pretty m.SlicingInternals.m2

  let to_string m =
    Pretty_utils.sfprintf "%a" pretty m

  let minus ma mb =
    { SlicingInternals.m1 = Mark.minus ma.SlicingInternals.m1 mb.SlicingInternals.m1;
      m2 = Mark.minus ma.SlicingInternals.m2 mb.SlicingInternals.m2 }

  (** see {! Mark.merge} *)
  let merge ma mb =
    let m1 = Mark.merge ma.SlicingInternals.m1 mb.SlicingInternals.m1 in
    let m2 = Mark.merge ma.SlicingInternals.m2 mb.SlicingInternals.m2 in
      { SlicingInternals.m1 = m1 ; m2 = m2 }

  (** merge only ma_1 et mb_1, m_2 is always bottom *)
  let merge_user_marks ma mb =
    let m1 = Mark.merge ma.SlicingInternals.m1 mb.SlicingInternals.m1 in
      { SlicingInternals.m1 = m1 ; m2 = Mark.bottom }

  let rec merge_all marks =
    match marks with
      | [] -> bottom_mark
      | m :: [] -> m (* to avoid merging with bottom every time ! *)
      | m :: tl -> merge m (merge_all tl)

  let inter ma mb =
    let m1 = Mark.inter ma.SlicingInternals.m1 mb.SlicingInternals.m1 in
    let m2 = Mark.inter ma.SlicingInternals.m2 mb.SlicingInternals.m2 in
      { SlicingInternals.m1 = m1 ; m2 = m2 }

  let rec inter_all marks =
    match marks with
      | [] -> bottom_mark
      |  m :: [] -> m
      | m :: tl -> inter m (inter_all tl)

  (** [combine ma mb] is used to add the [mb] to the [ma].
    * @return two marks : the first one is the new mark (= merge),
    *   and the second is the one to propagate.
    *   Notice that if the mark to propagate is bottom,
    *   it means that [mb] was included in [ma].
    *)
  let combine ma mb =
    let combine_m ma mb =
      let is_new, mr = Mark.combine ma mb in
      let m_to_prop = if is_new then mr else Mark.bottom in
        mr, m_to_prop
    in
    let new_m1, prop1 = combine_m ma.SlicingInternals.m1 mb.SlicingInternals.m1 in
    let new_m2, prop2 = combine_m ma.SlicingInternals.m2 mb.SlicingInternals.m2 in
    { SlicingInternals.m1 = new_m1 ; m2 = new_m2 }, 
    { SlicingInternals.m1 = prop1 ; m2 = prop2 }

  (** we want to know if the called function [g] with output marks
  * [m_out_called] compute enough things to be used in [f] call
  * with output marks [m_out_call].
  * Remember the [mf1] marks propagates as [mg2] and the marks to add
  * can only be [m2] marks.
  * TODO : write this down in the specification
  *        and check with Patrick if it is ok.
  * *)
  let missing_output ~call:m_out_call ~called:m_out_called =
    if debug then
      Format.printf "check_out : call=%a called=%a\n" pretty m_out_call
                                                    pretty m_out_called;
    let mf1 = m_out_call.SlicingInternals.m1 in
    let mf2 = m_out_call.SlicingInternals.m2 in
    let mg1 = m_out_called.SlicingInternals.m1 in
    let mg2 = m_out_called.SlicingInternals.m2 in
    let needed_mg2 = (* we need (mf1 + mf2) for this out in the call *)
      Mark.merge mf1 mf2 in
    let min_mg2 = (* let remove from needed_mg2 what we have in mg1 *)
      Mark.minus needed_mg2 mg1 in
    if Mark.is_included min_mg2 mg2 then None
    else let m2 = mk_m2 min_mg2 in
      if debug then
        Format.printf "check_out missing output -> %a\n" pretty m2;
      (Some m2)

  (** tells if the caller ([f]) computes enough inputs for the callee ([g]).
  * Remember that [mg1] has to be propagated as [mf1],
  * but [mg2] has to be propagated as [mf2=spare] *)
  let missing_input ~call:m_in_call ~called:m_in_called =
    let mf1 = m_in_call.SlicingInternals.m1 in
    let mf2 = m_in_call.SlicingInternals.m2 in
    let mg1 = m_in_called.SlicingInternals.m1 in
    let mg2 = m_in_called.SlicingInternals.m2 in
    let new_mf1 = if Mark.is_included mg1 mf1 then Mark.bottom else mg1 in
    let new_mf2 =
      if (not (Mark.is_bottom mg2)) && (Mark.is_bottom mf2) then
        Mark.spare
      else Mark.bottom
    in let new_m =     { SlicingInternals.m1 = new_mf1 ; m2 = new_mf2 } in
      if is_bottom_mark new_m then None else Some new_m

end

(** [SigMarks] works on the marks in function signatures.
  *)
module SigMarks = struct

  open PdgIndex

  type t = SlicingInternals.pdg_mark Signature.t

  let pretty = Signature.pretty MarkPair.pretty

  let get_input_mark (sgn:t) n = Signature.find_input sgn n
  let get_in_ctrl_mark (sgn:t) = Signature.find_in_ctrl sgn
  let get_in_top_mark (sgn:t) = Signature.find_in_top sgn

  let get_all_input_marks (sgn:t) =
    Signature.fold_all_inputs (fun acc (k, m) -> (k, m)::acc) [] sgn

  let get_matching_input_marks (sgn:t) z =
    Signature.fold_all_inputs
      (fun acc (k, m) ->
        match k with
          | PdgIndex.Signature.InCtrl | PdgIndex.Signature.InNum _ ->
              (k, m) :: acc
          | PdgIndex.Signature.InImpl z' ->
              if Locations.Zone.intersects z z' then (k, m) :: acc else acc
      ) [] sgn

  exception Visible
  let raise_if_visible () (_, m) =
    if is_bottom_mark m then () else raise Visible

  let some_visible_out cm =
    try Signature.fold_all_outputs raise_if_visible () cm ; false
    with Visible -> true

  let is_topin_visible cm =
    try
      let m = get_in_top_mark cm in
        not (is_bottom_mark m)
    with Not_found -> false

  let ctrl_visible cm =
    try
      let ctrl_m = get_in_ctrl_mark cm in
        not (is_bottom_mark ctrl_m)
    with Not_found -> false

  let some_visible_in cm =
    try Signature.fold_num_inputs raise_if_visible () cm ; ctrl_visible cm
    with Visible -> true

  let merge_inputs_m1_mark cm =
    Signature.fold_all_inputs (fun acc (_, m) -> MarkPair.merge_user_marks acc m)
                   bottom_mark cm

  (** @return an under-approxamation of the mark for the given location.
  * If the location is not included in the union of the implicit inputs,
  * it returns bottom.
  * Else, it returns the intersection of the inputs that intersect the location.
  *)
  let get_input_loc_under_mark cm loc =
    if debug then
      Format.printf "get_input_loc_under_mark of %a"
        Locations.Zone.pretty loc;
    assert (not (Locations.Zone.equal Locations.Zone.bottom loc));
    let do_in (marked_inputs, marks) (in_loc, m) =
      if is_bottom_mark m then (marked_inputs, [])
      else if Locations.Zone.intersects in_loc loc
      then
        let marked_inputs = Locations.Zone.link marked_inputs in_loc in
        let marks = m::marks in
          (marked_inputs, marks)
      else
          (marked_inputs, marks)
    in
    let marked_inputs = Locations.Zone.bottom in
    let marked_inputs, marks =
      Signature.fold_impl_inputs do_in (marked_inputs, []) cm in
    let m =
      if Locations.Zone.is_included loc marked_inputs
      then MarkPair.inter_all marks
      else bottom_mark
    in
      if debug then
        Format.printf "get_input_loc_under_mark : m = %a"
          MarkPair.pretty m;
      m

  let something_visible cm =
    some_visible_out cm || some_visible_in cm || ctrl_visible cm

  let get_marked_out_zone call_marks =
    let add (out0, out_zone) (out_key, m_out)  =
      if is_bottom_mark m_out then (out0, out_zone)
      else match out_key with
            | PdgIndex.Signature.OutRet -> true, out_zone
            | PdgIndex.Signature.OutLoc z ->
                out0, Locations.Zone.join out_zone z
    in Signature.fold_all_outputs add  (false, Locations.Zone.bottom) call_marks


end

(** The mark associated with a call stmt is composed of
 * marks for the call inputs (numbered form 1 to [max_in])
 * and marks for the call outputs (numbered from 0 to [max_out] *)

(** {2 Exported things} *)

(** {3 on marks} *)

let mk_gen_spare = MarkPair.mk_gen_spare
let mk_user_spare = MarkPair.mk_m1_spare
let mk_user_mark ~data ~addr ~ctrl =
  if addr || data || ctrl then
    mk_m1 (Mark.mk_adc  addr data ctrl)
  else mk_user_spare

let is_top_mark = MarkPair.is_top
let is_spare_mark = MarkPair.is_spare
let is_ctrl_mark = MarkPair.is_ctrl
let is_addr_mark = MarkPair.is_addr
let is_data_mark = MarkPair.is_data

let merge_marks = MarkPair.merge_all
let combine_marks = MarkPair.combine
let inter_marks = MarkPair.inter_all
let minus_marks = MarkPair.minus
let compare_marks = MarkPair.compare
let pretty_mark = MarkPair.pretty
let mark_to_string = MarkPair.to_string

let missing_input_mark = MarkPair.missing_input
let missing_output_mark = MarkPair.missing_output

(** {3 on signatures} *)

type sig_marks = SigMarks.t

let empty_sig = PdgIndex.Signature.empty
let get_input_mark = SigMarks.get_input_mark
let get_all_input_marks = SigMarks.get_all_input_marks
let get_matching_input_marks = SigMarks.get_matching_input_marks
let merge_inputs_m1_mark = SigMarks.merge_inputs_m1_mark
let get_input_loc_under_mark = SigMarks.get_input_loc_under_mark
(*let same_output_visibility = SigMarks.same_output_visibility*)
let get_in_ctrl_mark = SigMarks.get_in_ctrl_mark
let something_visible = SigMarks.something_visible
let some_visible_out = SigMarks.some_visible_out
let is_topin_visible = SigMarks.is_topin_visible
let get_marked_out_zone = SigMarks.get_marked_out_zone
let pretty_sig = SigMarks.pretty

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
