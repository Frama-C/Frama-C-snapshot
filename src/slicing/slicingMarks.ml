(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Everything related with the marks. Mainly quite low level function. *)

(**/**)

module T = SlicingInternals

let debug = false

(**/**)

(** a [Mark] is used to represent some information about the status of
 * a PDF element in a slice.
 *)
module Mark : sig
  type t = T.t_mark
  val bottom : t
  val top : t
  val spare : t
  val data : t
  val ctrl : t
  val addr : t
  val mk_adc : bool -> bool -> bool -> t

  val is_bottom : t -> bool
  val is_top : t -> bool
  val is_included : t -> t -> bool

  (** Total order over the marks. Used only for sorting...
  * Use rather [is_included] to make a clever comparison. *)
  val compare : t -> t -> int

  (** this operation has to be commutative.
    It is used to merge two slices into one.
   *)
  val merge : t -> t -> t

  val inter : t -> t -> t

  (** this operation add a new information to the old value.
   * @return (new_mark, is_new)
             where is_new=true if the new_mark is not included in the old one.
   *)
  val combine : old:t -> t -> bool * t

  (** [minus m1 m2] provides the mark [m] that you have to merge with [m2] to
    * get at least [m1]. So : [m1 <= m U m2]
    * If [m1 <= m2] then [m = bot].
    * *)
  val minus : t -> t -> t

  val pretty : Format.formatter -> t -> unit

end = struct
  module D = PdgTypes.Dpd

  type t = T.t_mark


  let spare = T.Spare

  (* Internal constructor *)
  let create_adc a d c =  T.Cav (D.make ~a ~d ~c ())

  let bottom = T.Cav D.bottom
  let top = T.Cav D.top

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
  let mk_mark dpd = create (D.adc_value dpd)

  let is_bottom m = (m = bottom)
  let is_top m = (m = top)

  (** Total order over the marks. Used only for sorting... *)
  let compare : t -> t -> int = T.compare_mark

  let is_included m1 m2 =
     match m1,m2 with
       | T.Spare, T.Spare -> true
       | T.Spare, T.Cav _ -> if is_bottom m2 then false else true
       | T.Cav _, T.Spare -> if is_bottom m1 then true else false
       | T.Cav d1, T.Cav d2 -> D.is_included d1 d2

   let merge m1 m2 =
     match m1,m2 with
       | T.Spare, T.Spare -> m1
       | T.Spare, T.Cav _ -> if is_bottom m2 then m1 else m2
       | T.Cav _,  T.Spare -> if is_bottom m1 then m2 else m1
       | T.Cav d1, T.Cav d2 -> mk_mark (D.combine d1 d2)

  let inter m1 m2 =
    if is_bottom m1 then m1
    else if is_bottom m2 then m2
    else (* m1 and m2 are not bottom => the result cannot be bottom *)
      match m1,m2 with
        | T.Spare, _ -> m1
        | _, T.Spare -> m2
        | T.Cav d1, T.Cav d2 ->
            let m = mk_mark (D.inter d1 d2) in
              if is_bottom m then spare else m

   let combine ~old m =
     match old, m with
     | T.Spare, T.Spare -> (false, old)
     | T.Cav old_d, T.Spare ->
         if D.is_bottom old_d then (true, m) else (false, old)
     | T.Spare, T.Cav new_d ->
         if D.is_bottom new_d then (false, old) else (true, m)
     | T.Cav old_d, T.Cav new_d ->
         let new_d = D.combine old_d new_d in
         if old_d = new_d then (false, old) else (true, mk_mark new_d)

  let minus m1 m2 =
     match m1,m2 with
       | T.Spare, T.Spare -> bottom
       | T.Spare, T.Cav d2 -> if D.is_bottom d2 then m1 else bottom
       | T.Cav _, T.Spare -> m1 (* even if [D.is_bottom d1] because m1 = bot *)
       | T.Cav d1, T.Cav d2 -> mk_mark (D.minus d1 d2)

  let pretty fmt m =
    match m with
    | T.Cav d -> D.pretty fmt d
    | T.Spare -> Format.fprintf fmt "[ S ]"

  let to_string m = Pretty_utils.sfprintf "%a" pretty m

end

(** a [MarkPair] is associated with each element of the PDG in a slice.
  * The first component gives the mark propagated from a user request, while
  * the second one is used to propagate informations to the called functions.
  *)
module MarkPair = struct
  type t = T.t_pdg_mark

  (* To do hash-consing *)
  let create = SlicingInternals.create_sl_mark

  let mk_m1 m1 = create ~m1 ~m2:Mark.bottom
  let mk_m2 m2 = create ~m1:Mark.bottom ~m2
  let mk_m m1 m2 = create ~m1 ~m2

  let mk_m1_data = mk_m1 Mark.data
  let mk_m1_addr = mk_m1 Mark.addr
  let mk_m1_ctrl = mk_m1 Mark.ctrl
  let mk_m1_cav = mk_m1 (Mark.mk_adc true true true)
  let mk_m1_spare = mk_m1 Mark.spare

  let mk_gen_spare = mk_m2 Mark.spare

  let bottom = create ~m1:Mark.bottom ~m2:Mark.bottom

  let user_mark m = Mark.merge m.T.m1 m.T.m2

  let is_bottom m = (Mark.is_bottom (user_mark m))
  let is_top m = (Mark.is_top m.T.m1) && (Mark.is_top m.T.m2)

  let is_ctrl m = (Mark.is_included Mark.ctrl (user_mark m))
  let is_addr m = (Mark.is_included Mark.addr (user_mark m))
  let is_data m = (Mark.is_included Mark.data (user_mark m))

  let is_spare m =
    not (is_bottom m) && not (is_ctrl m || is_addr m || is_data m)

  let compare = T.compare_pdg_mark

  let is_included ma mb =
    (Mark.is_included ma.T.m1 mb.T.m1) && (Mark.is_included ma.T.m2 mb.T.m2)

  let pretty fmt m =
    let pm fmt m = Format.fprintf fmt "%a" Mark.pretty m
    in Format.fprintf fmt "<%a,%a>" pm m.T.m1 pm m.T.m2

  let to_string m =
    Pretty_utils.sfprintf "%a" pretty m

  let minus ma mb =
    mk_m (Mark.minus ma.T.m1 mb.T.m1) (Mark.minus ma.T.m2 mb.T.m2)

  (** see {! Mark.merge} *)
  let merge ma mb =
    let m1 = Mark.merge ma.T.m1 mb.T.m1 in
    let m2 = Mark.merge ma.T.m2 mb.T.m2 in
      mk_m m1 m2

  (** merge only ma_1 et mb_1, m_2 is always bottom *)
  let merge_user_marks ma mb =
    let m1 = Mark.merge ma.T.m1 mb.T.m1 in
      mk_m m1 Mark.bottom

  let rec merge_all marks =
    match marks with
      | [] -> bottom
      | m :: [] -> m (* to avoid merging with bottom every time ! *)
      | m :: tl -> merge m (merge_all tl)

  let inter ma mb =
    let m1 = Mark.inter ma.T.m1 mb.T.m1 in
    let m2 = Mark.inter ma.T.m2 mb.T.m2 in
      mk_m m1 m2

  let rec inter_all marks =
    match marks with
      | [] -> bottom
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
    let new_m1, prop1 = combine_m ma.T.m1 mb.T.m1 in
    let new_m2, prop2 = combine_m ma.T.m2 mb.T.m2 in
      (mk_m new_m1 new_m2), (mk_m prop1  prop2)

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
    let mf1 = m_out_call.T.m1 in
    let mf2 = m_out_call.T.m2 in
    let mg1 = m_out_called.T.m1 in
    let mg2 = m_out_called.T.m2 in
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
    let mf1 = m_in_call.T.m1 in
    let mf2 = m_in_call.T.m2 in
    let mg1 = m_in_called.T.m1 in
    let mg2 = m_in_called.T.m2 in
    let new_mf1 = if Mark.is_included mg1 mf1 then Mark.bottom else mg1 in
    let new_mf2 =
      if (not (Mark.is_bottom mg2)) && (Mark.is_bottom mf2) then
        Mark.spare
      else Mark.bottom
    in let new_m = mk_m new_mf1 new_mf2 in
      if is_bottom new_m then None else Some new_m

end

(** Signature to define what we have to be able to do on marks to
 * use {!module:F_SigMarks} *)
module type T_Mark = sig
  type t
  val bottom : t
  val is_bottom : t -> bool
  val merge : t -> t -> t
  val inter_all : t list -> t
  val merge_user_marks : t -> t -> t

  (** returns the mark that is missing in the [call] input (if any)
   * to be able to call the called function. *)
  val missing_input : call:t -> called:t -> t option

  (** returns the mark that is missing in [called] output (if any)
   * to be able to call that function for this [call]. *)
  val missing_output : call:t -> called:t -> t option

  (** generated [spare] = the smallest visible mark *)
  val mk_gen_spare : t

  val pretty : Format.formatter -> t -> unit
end

(** [SigMarks] works on the marks in function signatures.
  *)
module F_SigMarks (M : T_Mark) = struct

  open PdgIndex

  type tm = M.t
  type t = tm Signature.t

  let empty = Signature.empty

  let pretty = Signature.pretty M.pretty

  let get_input_mark (sgn:t) n = Signature.find_input sgn n
  let get_in_ctrl_mark (sgn:t) = Signature.find_in_ctrl sgn
  let get_in_top_mark (sgn:t) = Signature.find_in_top sgn

  let get_all_input_marks (sgn:t) =
    Signature.fold_all_inputs (fun acc (k, m) -> (k, m)::acc) [] sgn

  exception Visible
  let raise_if_visible () (_, m) =
    if M.is_bottom m then () else raise Visible

  let some_visible_out cm =
    try Signature.fold_all_outputs raise_if_visible () cm ; false
    with Visible -> true

  let is_topin_visible cm =
    try
      let m = get_in_top_mark cm in
        if M.is_bottom m then false else true
    with Not_found -> false

  let ctrl_visible cm =
    try
      let ctrl_m = get_in_ctrl_mark cm in
        if M.is_bottom ctrl_m then false else true
    with Not_found -> false

  let some_visible_in cm =
    try Signature.fold_num_inputs raise_if_visible () cm ; ctrl_visible cm
    with Visible -> true

  let merge_inputs_m1_mark cm =
    Signature.fold_all_inputs (fun acc (_, m) -> M.merge_user_marks acc m)
                   M.bottom cm

  (** @return an under-approxamation of the mark for the given location.
  * If the location is not included in the union of the implicit inputs,
  * it returns bottom.
  * Else, it returns the intersection of the inputs that intersect the location.
  *)
  let get_input_loc_under_mark cm loc =
    if debug then
      Format.printf "get_input_loc_under_mark of %a@."
        Locations.Zone.pretty loc;
    assert (not (Locations.Zone.equal Locations.Zone.bottom loc));
    let do_in (marked_inputs, marks) (in_loc, m) =
      if M.is_bottom m then (marked_inputs, [])
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
      then M.inter_all marks
      else M.bottom
    in
      if debug then
        Format.printf "get_input_loc_under_mark : m = %a@."
          M.pretty m;
      m

  let something_visible cm =
    some_visible_out cm || some_visible_in cm || ctrl_visible cm

  (** @return the mark that has to be associated to the call statement.
   * It summarize the marks of all nodes.
   *)
  let rec combined_marks cm =
    let add_m m (_, m2) = M.merge m m2 in
    Signature.fold add_m M.bottom cm

  let add_spare out_marks max_out =
    let rec add_out lst n =
      if n < 0 then lst
      else if not (List.mem_assoc n lst) then
        (n, M.mk_gen_spare) :: (add_out lst (n-1))
      else add_out lst (n-1)
    in add_out out_marks max_out

         (*
  let same_output_visibility sig1 sig2 =
    let check sig_b () (num_out, m_a) =
      let m_b =
        try Signature.find_output sig_b num_out
        with Not_found -> M.bottom
      in if (M.is_bottom m_a) <> (M.is_bottom m_b)
      then raise SlicingMacros.Break
    in
    let same =
      try
        Signature.fold_outputs (check sig2) () sig1;
        Signature.fold_outputs (check sig1) () sig2;
        true
      with SlicingMacros.Break -> false
    in same
    *)

  (** check if the output marks in [called_marks] are enough for the
  * [call_marks].
  * @return a list of (output number, mark) that are missing,
  * and a boolean that says if the modification of the called function
  * would make more visible outputs.
  * *)
         (*
  let check_output called_marks (new_marks, more_outputs) (num_out, m_call) =
    let m_called =
      try Signature.find_output called_marks num_out
      with Not_found -> M.bottom
    in
    let missing_m = M.missing_output ~call:m_call ~called:m_called in
    let new_marks, more_outputs = match missing_m with
      | None -> new_marks, more_outputs
      | Some missing_m ->
          let new_output = M.is_bottom m_called in
            (num_out, missing_m) :: new_marks, more_outputs || new_output
    in new_marks, more_outputs
    *)

  let get_called_marks called_marks_opt =
    let called_marks = match called_marks_opt with
      | Some called_marks -> called_marks
      | None -> empty
    in
      if debug then
        Format.printf "with called = %a\n" pretty called_marks;
      called_marks

        (*
  let check_output_marks new_call_marks called_marks_opt =
    let called_marks = get_called_marks called_marks_opt in
      List.fold_left (check_output called_marks) ([], false) new_call_marks

  let check_called_output_marks call_marks called_marks_opt =
    if debug then
      Format.printf "check_called_output_marks : call = %a\n"
      pretty call_marks;
    let called_marks = get_called_marks called_marks_opt in
      Signature.fold_outputs (check_output called_marks) ([], false) call_marks
      *)

  let check_input sgn result (in_key, mark) =
    let add_if_needed m_sgn (in_key, m_input) (marks, more) =
      if debug then
        Format.printf "check_input : sgn=%a ; needed=%a\n"
          M.pretty m_sgn M.pretty m_input;
      let missing_m = M.missing_input ~call:m_sgn ~called:m_input in
        match missing_m with
          | None -> marks, more
          | Some missing_m ->
              let new_input = M.is_bottom m_sgn in
                (in_key, missing_m) :: marks, more || new_input
    in
    let m_sgn =
      try Signature.find_in_info sgn in_key
      with Not_found -> M.bottom
    in add_if_needed m_sgn (in_key, mark) result

  let check_input_marks sgn input_marks =
    List.fold_left (check_input sgn) ([], false) input_marks

  (** check if the input marks in [call_marks] are enough to call a slice with
   * [called_marks].
   * @return a list of (input number, mark) that are missing,
   * and a boolean that says if the propagation in the call
   * would make more visible inputs in the call signature.
   * *)
  let check_called_input_marks call_marks called_marks_opt =
    match called_marks_opt with
        | Some called_marks ->
            let result = Signature.fold_all_inputs (check_input call_marks)
                                               ([], false) called_marks in
              result (*missing_marks, more_inputs*)
        | None -> (* called function need no inputs ? *) ([], false)

  let get_marked_out_zone call_marks =
    let add (out0, out_zone) (out_key, m_out)  =
      if MarkPair.is_bottom m_out then (out0, out_zone)
      else match out_key with
            | PdgIndex.Signature.OutRet -> true, out_zone
            | PdgIndex.Signature.OutLoc z ->
                out0, Locations.Zone.join out_zone z
    in Signature.fold_all_outputs add  (false, Locations.Zone.bottom) call_marks


end

(** The mark associated with a call stmt is composed of
 * marks for the call inputs (numbered form 1 to [max_in])
 * and marks for the call outputs (numbered from 0 to [max_out] *)
module SigMarks = F_SigMarks (MarkPair)

(** {2 Exported things} *)

(** {3 on marks} *)
type t_mark = MarkPair.t

let bottom_mark = MarkPair.bottom
let mk_gen_spare = MarkPair.mk_gen_spare
let mk_user_spare = MarkPair.mk_m1_spare
let mk_user_mark ~data ~addr ~ctrl =
  if addr || data || ctrl then
    MarkPair.mk_m1 (Mark.mk_adc  addr data ctrl)
  else mk_user_spare

let is_bottom_mark = MarkPair.is_bottom
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

type t_sig_marks = SigMarks.t

let empty_sig = SigMarks.empty
let get_input_mark = SigMarks.get_input_mark
let get_all_input_marks = SigMarks.get_all_input_marks
let merge_inputs_m1_mark = SigMarks.merge_inputs_m1_mark
let get_input_loc_under_mark = SigMarks.get_input_loc_under_mark
(*let same_output_visibility = SigMarks.same_output_visibility*)
let get_in_ctrl_mark = SigMarks.get_in_ctrl_mark
let something_visible = SigMarks.something_visible
let some_visible_out = SigMarks.some_visible_out
let is_topin_visible = SigMarks.is_topin_visible
                         (*
let check_output_marks = SigMarks.check_output_marks
let check_called_output_marks = SigMarks.check_called_output_marks
*)
let check_input_marks = SigMarks.check_input_marks
let check_called_input_marks = SigMarks.check_called_input_marks
let get_marked_out_zone = SigMarks.get_marked_out_zone
let pretty_sig = SigMarks.pretty
