(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

open Cil_types
open Value_util
open Locations


(* Literal strings can only be compared if their contents are recognizably
   different (or the strings are physically the same). *)
let are_comparable_string pointer1 pointer2 =
  try
    Locations.Location_Bytes.iter_on_strings ~skip:None
      (fun base1 s1 offs1 len1 ->
         Locations.Location_Bytes.iter_on_strings ~skip:(Some base1)
           (fun _ s2 offs2 len2 ->
              let delta = offs1 - offs2 in
              let start = if delta <= 0 then -delta else 0
              and max = min len2 (len1 - delta) in
              let length = max - start + 1 in
              let sub1 = String.sub s1 (start + delta) length
              and sub2 = String.sub s2 start length in
              if String.compare sub1 sub2 = 0
              then raise Not_found)
           pointer1)
      pointer2;
    true
  with
    | Not_found -> false
    | Invalid_argument _s -> assert false

(* Under-approximation of the fact that a pointer is actually correct w.r.t.
   what can be created through pointer arithmetics. See C99 6.5.6 and 6.5.8
   for the definition of possible pointers, and in particular the definition
   of "one past". Value does not currently check that all pointers are
   possible, but flags impossible ones using pointer_comparable alarms when
   performing a comparison.

   In practice, function pointers are considered possible or one past
   when their offset is 0. For object pointers, the offset is checked
   against the validity of each base, taking past-one into account. *)
let possible_pointer ~one_past location =
  try
    let location = loc_bytes_to_loc_bits location in
    let is_possible_offset base offs =
      if Base.is_function base then
        if Ival.is_zero offs then () else raise Base.Not_valid_offset
      else
        let size = if one_past then Integer.zero else Integer.one in
        Base.is_valid_offset ~for_writing:false size base offs
    in
    match location with
      | Location_Bits.Top _ -> false
      | Location_Bits.Map m ->
          Location_Bits.M.iter is_possible_offset m;
          true
  with
  | Int_Base.Error_Top | Base.Not_valid_offset -> false

(* Are [ev1] and [ev2] safely comparable, or does their comparison involves
   invalid pointers, or is undefined (typically pointers in different bases). *)
let are_comparable op ev1 ev2 =
  (* If both of the operands have arithmetic type, the comparison is valid. *)
  if Location_Bytes.is_included ev1 Location_Bytes.top_int
     && Location_Bytes.is_included ev2 Location_Bytes.top_int
  then true
  else
    let null_1, rest_1 = Location_Bytes.split Base.null ev1
    and null_2, rest_2 = Location_Bytes.split Base.null ev2 in
    (* Note that here, rest_1 and rest_2 cannot be both bottom. *)
    let is_bottom1 = Location_Bytes.is_bottom rest_1
    and is_bottom2 = Location_Bytes.is_bottom rest_2 in
    let arith_compare_ok =
      if op = Eq || op = Ne
      then
        (* A pointer can be compared to a null pointer constant
           by equality operators. *)
        (Ival.is_included null_1 Ival.zero || is_bottom2)
        && (Ival.is_included null_2 Ival.zero || is_bottom1)
      else
        (* Pointers cannot be compared to arithmetic values by
           relational operators. *)
        Ival.is_bottom null_1 && Ival.is_bottom null_2
    in
    if not arith_compare_ok
    then false
    else
    (* Both pointers have to be almost valid (they can be pointers to one past
       an array object. *)
    if (not (possible_pointer ~one_past:true rest_1)) ||
       (not (possible_pointer ~one_past:true rest_2))
    then false
    else
    (* Equality operators allow the comparison between an almost valid pointer
       and the null pointer (other cases where is_bottom1 or is_bottom2 have
       been managed by arith_compare_ok). *)
    if is_bottom1 || is_bottom2
    then true
    else
      (* If both pointers point to the same base, the comparison is valid. *)
      let single_base_ok =
        try
          let base_1, _ = Location_Bytes.find_lonely_key rest_1
          and base_2, _ = Location_Bytes.find_lonely_key rest_2 in
          Base.equal base_1 base_2
        with Not_found -> false
      in
      if single_base_ok
      then true
      else if not (op = Eq || op = Ne)
      (* For relational operators, the comparison of pointers on different
         bases is undefined. *)
      then false
      else
        (* If both addresses are valid, they can be compared for equality. *)
      if (possible_pointer ~one_past:false rest_1) &&
         (possible_pointer ~one_past:false rest_2)
      then
        (* But beware of the comparisons of literal strings. *)
        are_comparable_string rest_1 rest_2
      else false


exception Recursive_call

(** Check that [kf] is not already present in the call stack *)
let check_no_recursive_call kf =
  try
  List.iter
    (function (g,_) ->
      if kf == g
      then begin
        if Value_parameters.IgnoreRecursiveCalls.get()
        then begin
          warning_once_current
            "@[recursive call@ during@ value@ analysis@ of %a @[(%a <- %a)@].@ \
             Using specification of %a.@]"
            Kernel_function.pretty kf Kernel_function.pretty kf
            pretty_call_stack (call_stack ()) Kernel_function.pretty kf;
          Db.Value.recursive_call_occurred kf;
          raise Recursive_call
        end
        else begin
	  warning_once_current "@[@[detected@ recursive@ call@ (%a <- %a)@]@;@[Use %s@ to@ ignore@ (beware@ this@ will@ make@ the analysis@ unsound)@]@]"
            Kernel_function.pretty kf
            pretty_call_stack (call_stack ())
            Value_parameters.IgnoreRecursiveCalls.option_name;
          raise Db.Value.Aborted

        end
      end)
    (call_stack ());
    true
  with Recursive_call -> false

(* Warn if [lv] changes during a call [lvret = kf()] *)
let warn_modified_result_loc ~with_alarms kf locret state lvret =
  if with_alarms.CilE.others != CilE.a_ignore then
    match lvret with
      | Var _, NoOffset -> () (* Skip trivially constant l-values *)
      | _ ->
           (* Go through Db.Value to avoid recursivity between modules *)
           let locret' = !Db.Value.lval_to_loc_state state lvret in
           if not (Location.equal locret locret') then
             (* There might be a false warning if the location is partially
                invalid before the call, and is reduced to its valid part
                during the call *)
             let validlocret = valid_part ~for_writing:true locret in
             let validlocret' = valid_part ~for_writing:true locret' in
             if not (Location.equal validlocret validlocret') then
               let loc = Cil_datatype.Location.unknown in
               let exp = Cil.mkAddrOrStartOf ~loc lvret in
               Valarms.do_warn with_alarms.CilE.others
                 (fun () ->
                    Value_parameters.warning ~current:true ~once:true
                      "@[possible@ side-effect@ modifying %a@ within@ call@ \
                       to %a@]%t" Printer.pp_exp exp Kernel_function.pretty kf
                      Value_util.pp_callstack;
                 )


let warn_locals_escape is_block fundec k locals =
  let pretty_base = Base.pretty in
  let pretty_block fmt = Pretty_utils.pp_cond is_block fmt "a block of " in
  let sv = fundec.svar in
  match locals with
  | Base.SetLattice.Top ->
      warning_once_current
        "locals escaping the scope of %t%a through %a"
        pretty_block
        Printer.pp_varinfo sv
        pretty_base k
  | Base.SetLattice.Set _ ->
      warning_once_current
        "locals %a escaping the scope of %t%a through %a"
        Base.SetLattice.pretty locals
        pretty_block
        Printer.pp_varinfo sv
        pretty_base k

let warn_locals_escape_result fundec locals =
  let sv = fundec.svar in
  match locals with
  | Base.SetLattice.Top ->
      warning_once_current
        "locals escaping the scope of %a through \\result"
        Printer.pp_varinfo sv
  | Base.SetLattice.Set _ ->
      warning_once_current
        "locals %a escaping the scope of %a through \\result"
        Base.SetLattice.pretty locals
        Printer.pp_varinfo sv

let warn_imprecise_lval_read ~with_alarms lv loc contents =
  if with_alarms.CilE.imprecision_tracing.CilE.a_log then
  let pretty_gm fmt s =
    let s = Base.SetLattice.(inject (O.remove Base.null s)) in
    Base.SetLattice.pretty fmt s
  in
  let pretty_param fmt param =
    match param with
    | Base.SetLattice.Top -> Format.fprintf fmt "is imprecise"
    | Base.SetLattice.Set s ->
        Format.fprintf fmt "is a garbled mix of %a" pretty_gm s
  in
  let pretty_param_b fmt param =
    match param with
    | Base.SetLattice.Top ->
        Format.fprintf fmt "The contents@ are imprecise"
    | Base.SetLattice.Set s ->
          Format.fprintf fmt "It contains@ a garbled@ mix@ of@ %a" pretty_gm s
  in
  let something_to_warn =
    match loc.loc with Location_Bits.Top _ -> true
      | Location_Bits.Map _ ->
          match contents with
          | Location_Bytes.Top _ -> true
          | Location_Bytes.Map _ -> false
  in
  if something_to_warn then Valarms.do_warn with_alarms.CilE.imprecision_tracing
    (fun  () ->
    Value_parameters.result ~current:true ~once:true
      "@[<v>@[Reading left-value %a.@]@ %t%t%t@]"
      Printer.pp_lval lv
      (fun fmt ->
         match lv with
         | Mem _, _ ->
             (match loc.loc with
             | Location_Bits.Top (param,o) when Origin.equal o Origin.top  ->
                 Format.fprintf fmt "@[The location %a.@]@ "
                   pretty_param param
             | Location_Bits.Top (param,orig) ->
                 Format.fprintf fmt "@[The location @[%a@]@ because of@ %a.@]@ "
                   pretty_param param
                   Origin.pretty orig
             | Location_Bits.Map _ ->
                 Format.fprintf fmt "@[The location is @[%a@].@]@ "
                   Location_Bits.pretty loc.loc)
         | Var _, _ -> ())
      (fun fmt ->
         match contents with
         | Location_Bytes.Top (param,o) when Origin.equal o Origin.top ->
                 Format.fprintf fmt "@[%a.@]"
                   pretty_param_b param
         | Location_Bytes.Top (param,orig) ->
             Format.fprintf fmt "@[%a@ because of@ %a.@]"
               pretty_param_b param
               Origin.pretty orig
         | Location_Bytes.Map _ -> ())
      pp_callstack)

(* Auxiliary function for [do_assign] below. When computing the
   result of [lv = exp], warn if the evaluation of [exp] results in
   an imprecision. [loc_lv] is the location pointed to by [lv].
   [exp_val] is the part of the evaluation of [exp] that is imprecise. *)
let warn_right_exp_imprecision ~with_alarms lv loc_lv exp_val =
  Valarms.do_warn with_alarms.CilE.imprecision_tracing
    (fun () ->
       match exp_val with
         | Location_Bytes.Top(_topparam,origin) ->
             Value_parameters.result ~once:true ~current:true
               "@[<v>@[Assigning imprecise value to %a%t.@]%a%t@]"
               Printer.pp_lval lv
               (fun fmt -> match lv with
                  | (Mem _, _) ->
                    Format.fprintf fmt "@ (pointing to %a)"
                      (Locations.pretty_english ~prefix:false) loc_lv
                  | (Var _, _) -> ())
               (fun fmt org ->
                  if not (Origin.is_top origin) then
                    Format.fprintf fmt
                      "@ @[The imprecision@ originates@ from@ %a@]"
                      Origin.pretty org)
               origin
               pp_callstack
         | Location_Bytes.Map _ -> ())

(* Auxiliary function for do_assign (currently), that warns when the
   left-hand side and the right-hand side of an assignment overlap *)
let warn_overlap ~with_alarms (lv, left_loc) (exp_lv, right_loc) =
  let big_enough size =
    try Integer.gt size (Integer.of_int (Cil.bitsSizeOf Cil.intType))
    with Cil.SizeOfError _ -> true
  in
  if with_alarms.CilE.others.CilE.a_log then
    match right_loc.size with
      | Int_Base.Value size when big_enough size ->
    	  if Location_Bits.partially_overlaps size right_loc.loc left_loc.loc
	  then begin
            Valarms.set_syntactic_context (Valarms.SySep (lv, exp_lv));
            let msg fmt =
              Format.fprintf fmt  "@ (%a,@ size %a bits;@ %a,@ size %a bits)"
                (Locations.pretty_english ~prefix:false) left_loc
                Int_Base.pretty left_loc.Locations.size
                (Locations.pretty_english ~prefix:false) right_loc
                Int_Base.pretty right_loc.Locations.size
            in
            Valarms.warn_overlap msg with_alarms;
          end
      | _ -> ()


exception Got_imprecise of Cvalue.V.t
let offsetmap_contains_imprecision offs =
  try
    Cvalue.V_Offsetmap.iter_on_values
      (fun v ->
         match Cvalue.V_Or_Uninitialized.get_v v with
           | Location_Bytes.Map _ -> ()
           | Location_Bytes.Top _ as v -> raise (Got_imprecise v)
      ) offs;
    None
  with Got_imprecise v -> Some v

let warn_reduce_indeterminate_offsetmap ~with_alarms typ offsm loc state =
  if Cil.isArithmeticOrPointerType typ then (
    let uninit = ref false in
    let escaping = ref false in
    let res = ref offsm in
    let reduce loc =
      let size = Int_Base.project loc.size in
      let _alarm, state =
        Cvalue.Model.paste_offsetmap ~reducing:true ~from:!res
          ~dst_loc:loc.loc ~size ~exact:true state
      in
      state
    in
    let reduce () =
      match loc with
      | `NoLoc -> state
      | `PreciseLoc ploc ->
        if Precise_locs.cardinal_zero_or_one ploc then
          let loc = Precise_locs.imprecise_location ploc in
          reduce loc
        else state
      | `Loc loc ->
        if Locations.cardinal_zero_or_one loc then reduce loc else state
    in
    let warn () =
      if !uninit then Valarms.warn_uninitialized with_alarms;
      if !escaping then Valarms.warn_escapingaddr with_alarms;
    in
    try
      Cvalue.V_Offsetmap.iter
        (fun itv (v, size, offs) ->
          let open Cvalue.V_Or_Uninitialized in
          match v with
          | C_init_noesc _ -> ()
          | C_init_esc v'  | C_uninit_esc v' | C_uninit_noesc v' ->
            begin match v with
            | C_init_esc _ -> escaping := true
            | C_uninit_noesc _ -> uninit := true
            | C_uninit_esc _ -> escaping := true; uninit := true
            | _ -> assert false
            end;
            if Cvalue.V.is_bottom v' then raise Exit;
            res := Cvalue.V_Offsetmap.add itv (C_init_noesc v', size, offs) !res
        ) offsm;
      warn ();
      let state =  if !uninit || !escaping then reduce () else state in
      `Res (!res, state)
    with Exit ->
      warn ();
      `Bottom
  ) else
    `Res (offsm, state)

let maybe_warn_indeterminate ~with_alarms v =
  let open Cvalue.V_Or_Uninitialized in
  match v with
  | C_uninit_esc _ ->
    Valarms.warn_uninitialized with_alarms;
    Valarms.warn_escapingaddr with_alarms;
    true
  | C_uninit_noesc _ ->
    Valarms.warn_uninitialized with_alarms;
    true
  | C_init_esc _ ->
    Valarms.warn_escapingaddr with_alarms;
    true
  | C_init_noesc _ -> false

let maybe_warn_completely_indeterminate ~with_alarms loc vi v =
  if Cvalue.V.is_bottom v && not (Cvalue.V_Or_Uninitialized.is_bottom vi) &&
    with_alarms.CilE.unspecified.CilE.a_log
  then
    Valarms.do_warn with_alarms.CilE.unspecified
      (fun () ->
        Kernel.warning ~current:true ~once:true
          "completely indeterminate value %a."
          (Locations.pretty_english ~prefix:true) loc)

let warn_float_addr ~with_alarms msg =
  Valarms.do_warn with_alarms.CilE.imprecision_tracing
    (fun () ->
       Value_parameters.result ~once:true ~current:true
         "@[float@ value@ contains@ addresses (%t)]%t"
         msg Value_util.pp_callstack
    );
;;

let warn_float ~with_alarms ?(non_finite=false) ?(addr=false) flkind msg =
  if addr then warn_float_addr ~with_alarms msg;
  if addr || non_finite then
    Valarms.warn_nan_infinite with_alarms flkind msg;
;;

let maybe_warn_div ~with_alarms e =
  if Cvalue.V.contains_zero e then
    let addresses =
      try ignore (Cvalue.V.project_ival e); false
      with Cvalue.V.Not_based_on_null -> true
    in
    Valarms.warn_div with_alarms ~addresses

let warn_top () =
  Value_parameters.abort ~current:true ~once:true
    "completely imprecise state during evaluation. Aborting."


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
