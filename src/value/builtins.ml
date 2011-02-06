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

open Cvalue_type
open Abstract_interp
open Cil
open Locations
open Value_util

let table = Hashtbl.create 7

let register_builtin name f =
  Hashtbl.add table name f

let () = Db.Value.register_builtin := register_builtin

let find_builtin name =
  Hashtbl.find table name

let mem_builtin name = Hashtbl.mem table name

let () = Db.Value.mem_builtin := mem_builtin

let offsetmap_of_value ~typ v =
  V_Offsetmap.update_ival
       ~with_alarms:CilE.warn_none_mode
       ~validity:Base.All
       ~offsets:Ival.zero
       ~exact:true
       ~size:(Int.of_int (bitsSizeOf typ))
       V_Offsetmap.empty
       (V_Or_Uninitialized.initialized v)

let wrap_int i = Some (offsetmap_of_value ~typ:intType i)
let wrap_ptr p = Some (offsetmap_of_value ~typ:intPtrType p)
let wrap_double d = Some (offsetmap_of_value ~typ:doubleType d)

exception Found_misaligned_base

let frama_C_cos state actuals =
  match actuals with
    [_, arg, _] ->
      begin
	let r =
	  try
	    let i = Cvalue_type.V.project_ival arg in
	    let f = Ival.project_float i in
	    Cvalue_type.V.inject_ival
	      (Ival.inject_float (Ival.Float_abstract.cos_float f))
	  with Cvalue_type.V.Not_based_on_null ->
	    Value_parameters.result ~once:true ~current:true
	      "Builtin Frama_C_cos applied to address";
	    Cvalue_type.V.topify_arith_origin arg
	in
	(wrap_double r), state, Location_Bits.Top_Param.bottom
      end
  | _ ->
      Value_parameters.error "Invalid argument for Frama_C_cos function";
      do_degenerate None;
      raise Db.Value.Aborted

let () = register_builtin "Frama_C_cos" frama_C_cos

let frama_C_sqrt state actuals =
  match actuals with
    [_, arg, _] -> begin
	let r =
	  try
	    let i = Cvalue_type.V.project_ival arg in
	    let f = Ival.project_float i in
	    let result_alarm, f =
	      Ival.Float_abstract.sqrt_float (get_rounding_mode()) f
	    in
	    if result_alarm
	    then
(*	      CilE.warn_result_nan_infinite
		(warn_all_quiet_mode ()) ; *)
	      Value_parameters.result ~once:true ~current:true
		"float sqrt: assert (Ook)";
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
	(wrap_double r), state, Location_Bits.Top_Param.bottom
      end
  | _ -> Value_parameters.error
      "Invalid argument for Frama_C_sqrt function";
      do_degenerate None;
      raise Db.Value.Aborted

let () = register_builtin "Frama_C_sqrt" frama_C_sqrt

exception Base_aligned_error

let frama_C_is_base_aligned state actuals =
  try begin
      match actuals with
	[_,x,_; _,y,_] ->
	  let i = Cvalue_type.V.project_ival y in
	  begin match i with
	    Ival.Set si ->
	      Location_Bytes.fold_i
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
              Location_Bits.Top_Param.bottom
	  | _ -> raise Found_misaligned_base
	  end
      | _ -> raise Base_aligned_error
    end
  with Base_aligned_error ->
    Cilmsg.error "Invalid arguments for Frama_C_is_base_aligned function" ;
    do_degenerate None;
    raise Db.Value.Aborted
  | Found_misaligned_base
  | Not_found (* from project_ival *) ->
      (wrap_int Cvalue_type.V.zero_or_one),
      state,
      Location_Bits.Top_Param.bottom

let () = register_builtin "Frama_C_is_base_aligned" frama_C_is_base_aligned

exception Offset_error

let frama_c_offset state actuals =
  try begin
      match actuals with
	[_,x,_] ->
	  begin
	    let value =
	      try
		let offsets =
		  Location_Bytes.fold_i
		    (fun _b o a -> Ival.join a o)
		    x
		    Ival.bottom
		in
		Cvalue_type.V.inject_ival offsets
	      with Location_Bytes.Error_Top ->
		error
		  "Builtin Frama_C_offset is applied to a value not guaranteed to be an address";
		Cvalue_type.V.top_int
	    in
	    (wrap_int value), state, Location_Bits.Top_Param.bottom
	  end
      | _ -> raise Offset_error
    end
  with Offset_error ->
    Cilmsg.error "Invalid arguments for builtin Frama_C_offset" ;
    do_degenerate None;
    raise Db.Value.Aborted

let () = register_builtin "Frama_C_offset" frama_c_offset

exception Invalid_CEA_alloc_infinite
exception Not_found_lonely_key

module Dynamic_Alloc_Table =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Locations.Location_Bytes)
    (struct
       let dependencies = [Db.Value.self]
       let size = 79
       let name = "Dynamic_Alloc_Table"
       let kind = `Internal
     end)

let frama_c_alloc_infinite state actuals =
  try
    let file = match actuals with
    | [_,file,_] -> file
    | _ -> raise Invalid_CEA_alloc_infinite
    in
    let file_base,_file_offset =
      try
	Cvalue_type.V.find_lonely_key file
      with Not_found -> raise Not_found_lonely_key
    in
    let file = match file_base with
    | Base.String (_,s) -> s
    | Base.Var (s,_) | Base.Initialized_Var (s,_) -> s.Cil_types.vname
    | Base.Null | Base.Cell_class _ -> raise Invalid_CEA_alloc_infinite

    in
    let loc =
      Dynamic_Alloc_Table.memo
	(fun file ->
	  let new_name =
            if Extlib.string_prefix ~strict:true "Frama_C_alloc_" file
	    then file
	    else Format.sprintf "Frama_C_alloc_%s" file
	  in
	  let new_name = Cabs2cil.fresh_global new_name in
	  let unbounded_type =
            Cil_types.TArray
              (intType,
               Some (new_exp ~loc:Cil_datatype.Location.unknown
                       (Cil_types.Const (Cil_types.CStr "NOSIZE"))),
               empty_size_cache (),[])
	  in
	  let new_varinfo =
	    makeGlobalVar ~logic:true new_name unbounded_type
	  in
	  let new_offsetmap =
	    Cvalue_type.V_Offsetmap.sized_zero (Bit_utils.memory_size ())
	  in
	  let new_base =
	    Cvalue_type.Default_offsetmap.create_initialized_var
	      new_varinfo
	      Base.All
	      new_offsetmap
	  in
	  Location_Bytes.inject new_base Ival.zero)
	file
    in
    wrap_ptr loc, state, Location_Bits.Top_Param.bottom
  with
  | Ival.Error_Top | Invalid_CEA_alloc_infinite
  | Not_found_lonely_key (* from [find_lonely_key] *)
    -> Value_parameters.error
      "Invalid argument for Frama_C_alloc_infinite function";
      do_degenerate None;
      raise Db.Value.Aborted
  | Not_found -> assert false

let () = register_builtin "Frama_C_alloc_infinite" frama_c_alloc_infinite


let frama_c_memcpy state actuals =
  try
    match actuals with
      | [exp_dst,dst,_; exp_src,src,_ ; exp_size,size,_] ->
	  let size = Cvalue_type.V.project_ival size in
	  let min,max = Ival.min_and_max size in
	  let min = match min with
	    | None -> Int.zero
	    | Some m -> Int.max m Int.zero
	  and max = match max with
            | None -> assert false (* TODO *)
            | Some m -> m
          in
	  let size_min = Int.mul Int.eight min in
	  let right = loc_bytes_to_loc_bits src in
	  let left = loc_bytes_to_loc_bits dst in
          let right_loc = make_loc right (Int_Base.inject size_min) in
          let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
          let array_src = Logic_utils.array_with_range exp_src term_size
          and array_dst = Logic_utils.array_with_range exp_dst term_size
          in
          CilE.set_syntactic_context (CilE.SyMemLogic array_src);
          begin
	    match Relations_type.Model.copy_offsetmap
             ~with_alarms:(warn_all_quiet_mode ())
             right_loc
             state
            with
              | None ->
                  None,
                  Relations_type.Model.bottom,
                  Location_Bits.Top_Param.bottom

              | Some offsetmap ->
                  CilE.set_syntactic_context (CilE.SyMemLogic array_dst);
		  let new_state =
                    Relations_type.Model.paste_offsetmap
		      offsetmap left Int.zero size_min state
                  in
		  let fuzz = Int.sub max min in
		  if Int.is_zero fuzz
		  then None, new_state, Location_Bits.get_bases right
		  else
		    let fuzz = Int.mul Int.eight fuzz in
		    let fuzz = Int_Base.inject fuzz in
		    let ival_min = Ival.inject_singleton size_min in
		    let left = Location_Bits.location_shift ival_min left in
		    let right = Location_Bits.location_shift ival_min right in
                    CilE.set_syntactic_context (CilE.SyMemLogic array_src);
		    let garb =
		      Relations_type.Model.find
			~conflate_bottom:false
			~with_alarms:(warn_all_quiet_mode ())
			state
			(make_loc right fuzz)
		    in
                    CilE.set_syntactic_context (CilE.SyMemLogic array_dst);
                    let new_state =
		      Relations_type.Model.add_binding
			~exact:false
			~with_alarms:(warn_all_quiet_mode ())
			new_state
			(make_loc left fuzz)
			garb
                    in
		    None, new_state, Location_Bits.get_bases right
          end
      | _ ->
	  raise Db.Value.Aborted
  with
      V.Not_based_on_null | Lmap.Cannot_copy | Db.Value.Aborted ->
        Value_parameters.error
	  "Invalid call to Frama_C_memcpy%a"
	  pretty_actuals
	  actuals;
        do_degenerate None;
        raise Db.Value.Aborted

let () = register_builtin "Frama_C_memcpy" frama_c_memcpy

(*
let name_cea_offset =
let is_cea_offset name =  name = name_cea_offset
*)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
