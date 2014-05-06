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

open Cil_types
open Value_util
open Locations


exception Distinguishable_strings

(* Does the comparison of [ev1] and [ev2] involve the comparison of
   invalid pointers, or is undefined (typically pointers in different bases) *)
let check_not_comparable op ev1 ev2 =
  try
    if not (Location_Bytes.is_included ev1 Location_Bytes.top_int)
      || not (Location_Bytes.is_included ev2 Location_Bytes.top_int)
    then begin
        let null_1, rest_1 = Location_Bytes.split Base.null ev1 in
        let null_2, rest_2 = Location_Bytes.split Base.null ev2 in
        let is_bottom1 = Location_Bytes.is_bottom rest_1 in
        let is_bottom2 = Location_Bytes.is_bottom rest_2 in

        (* First check if a non-zero integer is compared to an address *)
        if  ((not (Ival.is_included null_1 Ival.zero)) && (not is_bottom2))
         || ((not (Ival.is_included null_2 Ival.zero)) && (not is_bottom1))
        then raise Not_found;

        if not (is_bottom1 && is_bottom2)
        then begin
            let loc_bits1 = loc_bytes_to_loc_bits rest_1 in
            let loc_bits2 = loc_bytes_to_loc_bits rest_2 in
            let single_base_ok =
              begin try
                (* If they are both in the same base and both almost valid,
                   it's also fine, but beware of empty rest for comparisons
                   to NULL, or of function pointers *)
                let extract_base is_bot loc =
                  if is_bot then Base.null
                  else begin
                    let base, offs = Location_Bits.find_lonely_key loc in
                    if Base.is_function base then
                      (if not (Ival.equal Ival.zero offs)
                       then raise Base.Not_valid_offset)
                    else
                      Base.is_valid_offset ~for_writing:false
                        Integer.zero base offs;
                    base
                  end
                in
                let base_1 = extract_base is_bottom1 loc_bits1
                and base_2 = extract_base is_bottom2 loc_bits2
                in
                  is_bottom1 || is_bottom2 || (Base.equal base_1 base_2)
                with
                  Not_found -> false
              end
            in
            if not single_base_ok
            then begin
                if op = Eq || op = Ne
                then begin
                    (* If both addresses are valid, they can be compared 
		       for equality. *)
                    let loc1 = make_loc loc_bits1 Int_Base.one in
                    let loc2 = make_loc loc_bits2 Int_Base.one in
                    if (not (Locations.is_valid_or_function loc1)) ||
                      (not (Locations.is_valid_or_function loc2))
                    then raise Not_found;
                    (* But wait! literal strings can only be compared 
		       if their contents are recognizably different! 
		       (or the strings are physically the same) *)
                    Locations.Location_Bytes.iter_on_strings
                      ~skip:None
                      (fun base1 s1 offs1 len1 ->
                        Locations.Location_Bytes.iter_on_strings
                          ~skip:(Some base1)
                          (fun _ s2 offs2 len2 ->
                            let delta = offs1-offs2 in
                            begin
                              try
                                let start = if delta <= 0 then (-delta) else 0
                                in
                                for i = start to min len2 (len1 - delta)
                                do
(*                                Format.printf "%S %S %d %d@."
                                    s1 s2 i delta; *)
                                  if s2.[i] <> s1.[i + delta]
                                  then raise Distinguishable_strings;
                                done;
                                raise Not_found
                              with Distinguishable_strings -> ();
                            end)
                          rest_1)
                      rest_2
                  end
                else raise Not_found
              end
          end
      end;
    false
  with Not_found | Base.Not_valid_offset ->
    true


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
          Value_parameters.not_yet_implemented "recursive call"

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
               CilE.do_warn with_alarms.CilE.others
                 (fun (_emit, suffix) ->
                    Value_parameters.warning ~current:true ~once:true
                      "@[possible@ side-effect@ modifying %a@ within@ call@ \
                         to %a@]%t"
                      Printer.pp_exp exp Kernel_function.pretty kf suffix;
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
  if with_alarms.CilE.imprecision_tracing.CilE.a_log <> None
  then
  let pretty_param fmt param =
    match param with
    | Base.SetLattice.Top -> Format.fprintf fmt "is imprecise"
    | Base.SetLattice.Set _s ->
        Format.fprintf fmt "is a garbled mix of %a"
          Base.SetLattice.pretty param
  in
  let pretty_param_b fmt param =
    match param with
    | Base.SetLattice.Top ->
        Format.fprintf fmt "The contents@ are imprecise"
    | Base.SetLattice.Set _s ->
          Format.fprintf fmt "It contains@ a garbled@ mix@ of@ %a"
            Base.SetLattice.pretty param
  in
  let something_to_warn =
    match loc.loc with Location_Bits.Top _ -> true
      | Location_Bits.Map _ ->
          match contents with
          | Location_Bytes.Top _ -> true
          | Location_Bytes.Map _ -> false
  in
  if something_to_warn then CilE.do_warn with_alarms.CilE.imprecision_tracing
    (fun  _ ->
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
  CilE.do_warn with_alarms.CilE.imprecision_tracing
    (fun _ ->
       match exp_val with
         | Location_Bytes.Top(_topparam,origin) ->
             Value_parameters.result ~once:true ~current:true
               "@[<v>@[Assigning imprecise value to %a%t.@]%a%t@]"
               Printer.pp_lval lv
               (fun fmt -> match lv with
                  | (Mem _, _) ->
                      Format.fprintf fmt "@ (i.e. %a)" Locations.pretty loc_lv
                  | (Var _, _) -> ())
               (fun fmt org ->
                  if not (Origin.is_top origin) then
                    Format.fprintf fmt
                      "@ @[The imprecision@ originates@ from@ %a@]"
                      Origin.pretty org)
               origin
               pp_callstack
         | Location_Bytes.Map _ ->
             if not (Got_Imprecise_Value.get ()) &&
               not (Cvalue.V.cardinal_zero_or_one exp_val)
             then begin
               Got_Imprecise_Value.set true;
               if (Value_parameters.ValShowProgress.get())
               then
                 Value_parameters.result ~current:true
                   "assigning non deterministic value for the first time";
             end)


(* Auxiliary function for do_assign (currently), that warns when the
   left-hand side and the right-hand side of an assignment overlap *)
let warn_overlap ~with_alarms (lv, left_loc) (exp_lv, right_loc) =
  let big_enough size =
    try Integer.gt size (Integer.of_int (Cil.bitsSizeOf Cil.intType))
    with Cil.SizeOfError _ -> true
  in
  if with_alarms.CilE.others.CilE.a_log <> None then
    match right_loc.size with
      | Int_Base.Value size when big_enough size ->
    	  if Location_Bits.partially_overlaps size right_loc.loc left_loc.loc
	  then begin
            CilE.set_syntactic_context (CilE.SySep (lv, exp_lv));
            CilE.warn_overlap (left_loc, right_loc) with_alarms;
          end
      | _ -> ()


exception Got_imprecise of Cvalue.V.t
let offsetmap_contains_imprecision offs =
  try
    Cvalue.V_Offsetmap.iter_on_values
      (fun v _ ->
         match Cvalue.V_Or_Uninitialized.get_v v with
           | Location_Bytes.Map _ -> ()
           | Location_Bytes.Top _ as v -> raise (Got_imprecise v)
      ) offs;
    None
  with Got_imprecise v -> Some v

let warn_indeterminate_offsetmap ~with_alarms typ offsm =
  if Cil.isArithmeticOrPointerType typ then (
    let uninit = ref false in
    let escaping = ref false in
    let warn () =
      if !uninit then CilE.warn_uninitialized with_alarms;
      if !escaping then CilE.warn_escapingaddr with_alarms;
    in
    try
      let res = ref offsm in
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
      Some !res
    with Exit ->
      warn ();
      None
  ) else
    Some offsm


let warn_float_addr ~with_alarms msg =
  CilE.do_warn with_alarms.CilE.imprecision_tracing
    (fun (_, pp) ->
       Value_parameters.result ~once:true ~current:true
         "@[float@ value@ contains@ addresses (%t)]%t" msg pp
    );
;;

let warn_float ~with_alarms ?(overflow=false) ?(addr=false) flkind msg =
  if addr then warn_float_addr ~with_alarms msg;
  if addr || overflow then
    CilE.warn_nan_infinite with_alarms flkind msg;
;;


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
