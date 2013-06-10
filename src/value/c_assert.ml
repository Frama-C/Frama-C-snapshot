(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
open Cvalue

let pretty_int_range fmt print_ampamp typname lv v =
  let v = V.project_ival v in
  match Ival.min_and_max v with
    | Some mn, Some mx ->
	let mn_repr = 
	  if Int.equal mn (Int.of_string "-2147483648")
	  then "-2147483648LL"
	  else Int.to_string mn
	in
        if Int.equal mn mx
        then begin
          print_ampamp();
          Format.fprintf fmt "*(%s*)%s == %s" typname lv mn_repr
        end
        else begin
	  let mx_repr = 
	    if Int.equal mx (Int.of_string "-2147483648")
	    then "-2147483648LL"
	    else Int.to_string mx
	  in
          print_ampamp();
          Format.fprintf fmt "%s <= *(%s*)%s && *(%s*)%s <= %s"
            mn_repr typname lv typname lv mx_repr
        end
    | _ -> ()

let pretty_float_range fmt print_ampamp typname lv v =
  let use_hex = true in
  let pp_float = Ival.F.pretty_normal ~use_hex in
  let mn, mx = V.min_and_max_float v in
  if Ival.F.equal mn mx
  then begin
    print_ampamp();
    Format.fprintf fmt "*(%s*)%s == %a"
      typname lv pp_float mn
  end
  else begin
    print_ampamp();
    Format.fprintf fmt "%a <= *(%s*)%s && *(%s*)%s <= %a"
      pp_float mn typname lv typname lv pp_float mx;
  end

let types = Hashtbl.create 7;;

let () =
  Hashtbl.add types 1
    [V.inject_ival (Ival.inject_range
                      (Some Int.zero) (Some (Int.of_int 255))),
     "unsigned char", pretty_int_range;
     V.inject_ival (Ival.inject_range
                      (Some (Int.of_int (-128))) (Some (Int.of_int 127))),
     "char", pretty_int_range];
  Hashtbl.add types 2
    [V.inject_ival (Ival.inject_range
                      (Some Int.zero) (Some (Int.of_int 65535))),
     "unsigned short", pretty_int_range;
     V.inject_ival (Ival.inject_range
                      (Some (Int.of_int (-32768))) (Some (Int.of_int 32767))),
     "short", pretty_int_range];
  Hashtbl.add types 4
    [ V.top_float,
      "float", pretty_float_range;
      V.inject_ival (Ival.inject_range
                       (Some Int.zero) (Some (Int.of_string "4294967295"))),
      "unsigned int", pretty_int_range;
      V.inject_ival (Ival.inject_range
                       (Some (Int.of_string "-2147483648"))
                       (Some (Int.of_string  "2147483647"))),
      "int", pretty_int_range];
  Hashtbl.add types 8
    [ V.top_float,
      "double", pretty_float_range];
;;


let value_pretty print_ampamp lv s_bytes fmt v =
  try
    let candidate_types = Hashtbl.find types s_bytes in
    let rec find_typ = function
      | [] -> ()
      | (range, _, _) :: t when not (V.is_included v range) ->
          find_typ t
      | (_range, typname, pr) :: _ ->
          pr fmt print_ampamp typname lv v
    in
    find_typ candidate_types
  with V.Not_based_on_null -> ()

let value_uninit_pretty prampamp lv s fmt = function
  | V_Or_Uninitialized.C_init_noesc v ->
      value_pretty prampamp lv s fmt v
  | _ -> ()


let offsetmap_pretty name print_ampamp fmt offsm =
  let pretty_binding (bk,ek) (v, modu, offset) =
    let iso = V_Or_Uninitialized.is_isotropic v in
    if Integer.is_zero (Integer.rem bk Integer.eight)
      && (Rel.is_zero offset)
      && (iso || (Integer.is_zero (Integer.rem modu Integer.eight)))
    then
      let ek = Integer.succ ek in
      if Integer.is_zero (Integer.rem ek Integer.eight)
      then
        let step = if iso then 1 else (Integer.to_int modu) / 8 in
        let start = ref ((Integer.to_int bk) / 8) in
        let ek = Integer.to_int ek in
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
          value_uninit_pretty print_ampamp lv step fmt v;
          start := !start + step
        done;
      else ()
    else ()
  in
  Cvalue.V_Offsetmap.iter pretty_binding offsm

let state_pretty fmt m =
  Format.fprintf fmt "@[";
  (match m with
     | Model.Bottom -> Format.fprintf fmt "0"
     | Model.Map m ->
         let first = ref true in
         let print_ampamp () =
           if !first
           then first := false
           else Format.fprintf fmt "@\n&& ";
         in
         Model.LBase.iter
           (fun base offs ->
              match base with
                | Base.Var(v,_) ->
                    let name = v.Cil_types.vname in
		    if name <> "crc32_tab" (* Specialized for Csmith *)
		    then offsetmap_pretty name print_ampamp fmt offs
            | _ -> ())
          m
     | Model.Top -> Format.fprintf fmt "1"
  );
  Format.fprintf fmt "@]"

let pretty_state_as_c_assert = state_pretty
