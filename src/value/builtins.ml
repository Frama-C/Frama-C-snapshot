(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

open Cvalue_type
open Abstract_interp
open Cil

let table = Hashtbl.create 7

let register_builtin name f =
  Hashtbl.add table name f

let () = Db.Value.register_builtin := register_builtin

let find_builtin name =
  Hashtbl.find table name

let mem_builtin name = Hashtbl.mem table name

let () = Db.Value.mem_builtin := mem_builtin

let wrap_double i =
  Some
    (V_Offsetmap.update_ival
       ~with_alarms:CilE.warn_none_mode
       ~validity:Base.All
       ~offsets:Ival.zero
       ~exact:true
       ~size:(Int.of_int (bitsSizeOf doubleType))
       V_Offsetmap.empty
       (V_Or_Uninitialized.initialized i))

let wrap_int i =
  Some
    (V_Offsetmap.update_ival
       ~with_alarms:CilE.warn_none_mode
       ~validity:Base.All
       ~offsets:Ival.zero
       ~exact:true
       ~size:(Int.of_int (bitsSizeOf intType))
       V_Offsetmap.empty
       (V_Or_Uninitialized.initialized i))

exception Found_misaligned_base

let frama_C_cos state actuals =
  match actuals with
    [_, arg] -> 
      begin
	let r =
	  try
	    let i = Cvalue_type.V.find_ival arg in
	    let f = Ival.project_float i in
	    Cvalue_type.V.inject_ival
	      (Ival.inject_float (Ival.Float_abstract.cos_float f))
	  with Cvalue_type.V.Not_based_on_null ->
	    Value_parameters.result ~once:true ~current:true 
	      "Builtin Frama_C_cos applied to address";
	    Cvalue_type.V.topify_arith_origin arg
	in
	(wrap_double r), state, Locations.Location_Bits.Top_Param.bottom
      end
  | _ -> 
      Value_parameters.error "Invalid argument for Frama_C_cos function";
      Value_util.do_degenerate None;
      raise Db.Value.Aborted

let () = register_builtin "Frama_C_cos" frama_C_cos

let frama_C_sqrt state actuals =
  match actuals with
    [_, arg] -> begin
	let r =
	  try
	    let i = Cvalue_type.V.find_ival arg in
	    let f = Ival.project_float i in
	    let result_alarm, f =
	      Ival.Float_abstract.sqrt_float (Value_util.get_rounding_mode()) f
	    in
	    if result_alarm
	    then 
	      CilE.warn_result_nan_infinite 
		(Value_util.warn_all_quiet_mode ()) ;
	    Cvalue_type.V.inject_ival (Ival.inject_float f)

	  with
	    Cvalue_type.V.Not_based_on_null ->
	      Value_parameters.result ~once:true ~current:true
		"float sqrt applied to address";
	      Cvalue_type.V.topify_arith_origin arg
	  | Ival.Float_abstract.Bottom ->
	      ignore (CilE.warn_once "sqrt: TODO -- a proper alarm");
	      V.bottom
 	in
	(wrap_double r), state, Locations.Location_Bits.Top_Param.bottom
      end
  | _ -> Value_parameters.error
      "Invalid argument for Frama_C_sqrt function";
      Value_util.do_degenerate None;
      raise Db.Value.Aborted

let () = register_builtin "Frama_C_sqrt" frama_C_sqrt

exception Base_aligned_error

let frama_C_is_base_aligned state actuals =
  try begin
      match actuals with
	[_,x; _,y] ->
	  let i = Cvalue_type.V.find_ival y in
	  begin match i with
	    Ival.Set si ->
	      Locations.Location_Bytes.fold_i
		(fun b _o () ->
		  Ival.O.iter
		    (fun int ->
		      if not (Base.is_aligned_by b int)
		      then raise Found_misaligned_base)
		    si)
		x
		();
	      (wrap_int Cvalue_type.V.singleton_one),
              state,
              Locations.Location_Bits.Top_Param.bottom
	  | _ -> raise Found_misaligned_base
	  end
      | _ -> raise Base_aligned_error
    end
  with Base_aligned_error ->
    Cilmsg.error "Invalid arguments for Frama_C_is_base_aligned function" ;
    Value_util.do_degenerate None;
    raise Db.Value.Aborted
  | Found_misaligned_base
  | Not_found (* from find_ival *) ->
      (wrap_int Cvalue_type.V.zero_or_one),
      state,
      Locations.Location_Bits.Top_Param.bottom

let () = register_builtin "Frama_C_is_base_aligned" frama_C_is_base_aligned

exception Offset_error

let frama_c_offset state actuals =
  try begin
      match actuals with
	[_,x] ->
	  begin
	    let value =
	      try
		let offsets =
		  Locations.Location_Bytes.fold_i
		    (fun _b o a -> Ival.join a o)
		    x
		    Ival.bottom
		in
		Cvalue_type.V.inject_ival offsets
	      with Locations.Location_Bytes.Error_Top ->
		error
		  "Builtin Frama_C_offset is applied to a value that is not guaranteed \
                 to be an address";
		Cvalue_type.V.top_int
	    in
	    (wrap_int value), state, Locations.Location_Bits.Top_Param.bottom
	  end
      | _ -> raise Offset_error
    end
  with Offset_error ->
    Cilmsg.error "Invalid arguments for builtin Frama_C_offset" ;
    Value_util.do_degenerate None;
    raise Db.Value.Aborted

let () = register_builtin "Frama_C_offset" frama_c_offset

(*
let name_cea_offset = 
let is_cea_offset name =  name = name_cea_offset
*)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
