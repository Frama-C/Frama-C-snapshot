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

open Cvalue
open Abstract_interp
open Cil
open Locations
open Value_util
open Cil_types

let table = Hashtbl.create 17

let register_builtin name f =
  Hashtbl.add table name f

let () = Db.Value.register_builtin := register_builtin

let find_builtin name =
  Hashtbl.find table name

let mem_builtin name = Hashtbl.mem table name

let () = Db.Value.mem_builtin := mem_builtin
let overridden_by_builtin s =
  try ignore (Value_parameters.BuiltinsOverrides.find s); true
  with Not_found -> false

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

let double_double_fun name caml_fun state actuals =
  match actuals with
    [_, arg, _] ->
      begin
        let r =
          try
            let i = Cvalue.V.project_ival arg in
            let f = Ival.project_float i in
            Cvalue.V.inject_ival
              (Ival.inject_float (caml_fun f))
          with Cvalue.V.Not_based_on_null ->
            Value_parameters.result ~once:true ~current:true "%s"
              ("Builtin " ^ name ^ " applied to address");
            Cvalue.V.topify_arith_origin arg
        in
        (wrap_double r), state, Location_Bits.Top_Param.bottom
      end
  | _ ->
      Value_parameters.error "%s"
        ("Invalid argument for " ^ name ^ " function");
      do_degenerate None;
      raise Db.Value.Aborted


let frama_C_cos = double_double_fun "Frama_C_cos" Ival.Float_abstract.cos_float
let frama_C_cos_precise = 
  double_double_fun "Frama_C_cos_precise" Ival.Float_abstract.cos_float_precise

let () = register_builtin "Frama_C_cos" frama_C_cos

let () = register_builtin "Frama_C_cos_precise" frama_C_cos_precise

let frama_C_sin = double_double_fun "Frama_C_sin" Ival.Float_abstract.sin_float
let () = register_builtin "Frama_C_sin" frama_C_sin

let frama_C_sin_precise = 
  double_double_fun "Frama_C_sin_precise" Ival.Float_abstract.sin_float_precise
let () = register_builtin "Frama_C_sin_precise" frama_C_sin_precise

let frama_C_exp = double_double_fun "Frama_C_exp" Ival.Float_abstract.exp_float
let () = register_builtin "Frama_C_exp" frama_C_exp

(*
external cos_rd : float -> float = "caml_cos_rd"
external cos_ru : float -> float = "caml_cos_ru"
external crlibm_init : unit -> unit = "caml_crlibm_init"
*)

let frama_C_compare_cos state actuals =
  match actuals with
    [_, arg, _; _, res, _; _, eps, _] -> begin
          try
            let iarg = Cvalue.V.project_ival arg in
            let farg = Ival.project_float iarg in
            let larg,uarg = Ival.Float_abstract.min_and_max_float farg in
            let larg = Ival.F.to_float larg in
            let uarg = Ival.F.to_float uarg in
            let ires = Cvalue.V.project_ival res in
            let fres = Ival.project_float ires in
            let lres,ures = Ival.Float_abstract.min_and_max_float fres in
            let lres = Ival.F.to_float lres in
            let ures = Ival.F.to_float ures in
            let ieps = Cvalue.V.project_ival eps in
            let feps = Ival.project_float ieps in
            let _,ueps = Ival.Float_abstract.min_and_max_float feps in
            let ueps = Ival.F.to_float ueps in
(*          crlibm_init();
            let lref = cos_rd uarg in (* cos is decreasing *)
            let uref = cos_ru larg in (* cos is decreasing *) *)
            Ival.set_round_nearest_even();
              (* system cos probably isn't designed for non-default rounding *)
            let lref = cos uarg in
            let uref = cos larg in
            Ival.set_round_upward();
            let lallow = uref -. ueps in
            Ival.set_round_downward();
            let uallow = lref +. ueps in
            if lallow <= lres && ures <= uallow
            then
              Value_parameters.result "CC %1.16f %1.16f %1.16f %1.16f %1.16f %1.16f OK"
                larg uarg
                lres ures
                lref uref
            else
              Value_parameters.result "CC %1.16f %1.16f %1.16f %1.16f %1.16f %1.16f KO"
                larg uarg
                lres ures
                lref uref;
            None, state, Location_Bits.Top_Param.bottom
          with _ -> Value_parameters.error
            "Invalid argument for Frama_C_compare_cos function";
            do_degenerate None;
            raise Db.Value.Aborted
      end
  | _ -> Value_parameters.error
            "Invalid argument for Frama_C_compare_cos function";
            do_degenerate None;
            raise Db.Value.Aborted

let () = register_builtin "Frama_C_compare_cos" frama_C_compare_cos

let frama_C_sqrt state actuals =
  match actuals with
    [_, arg, _] -> begin
        let r =
          try
            let i = Cvalue.V.project_ival arg in
            let f = Ival.project_float i in
            let result_alarm, f =
              Ival.Float_abstract.sqrt_float (get_rounding_mode()) f
            in
            if result_alarm
            then
(*            CilE.warn_result_nan_infinite
                (warn_all_quiet_mode ()) ; *)
              Value_parameters.result ~once:true ~current:true
                "float sqrt: assert (Ook)";
            Cvalue.V.inject_ival (Ival.inject_float f)

          with
            Cvalue.V.Not_based_on_null ->
              Value_parameters.result ~once:true ~current:true
                "float sqrt applied to address";
              Cvalue.V.topify_arith_origin arg
          | Ival.Float_abstract.Bottom ->
            Value_parameters.warning ~once:true ~current:true
              "sqrt: TODO -- a proper alarm";
            V.bottom
        in
        (wrap_double r), state, Location_Bits.Top_Param.bottom
      end
  | _ -> Value_parameters.error
      "Invalid argument for Frama_C_sqrt function";
      do_degenerate None;
      raise Db.Value.Aborted

let () = register_builtin "Frama_C_sqrt" frama_C_sqrt



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
        Cvalue.V.find_lonely_key file
      with Not_found -> raise Not_found_lonely_key
    in
    let file = match file_base with
    | Base.String (_,e) -> 
	( match Base.get_string e with
	  Base.CSString s -> s
	| Base.CSWstring _ -> assert false )
    | Base.Var (s,_) | Base.Initialized_Var (s,_) -> s.Cil_types.vname
    | Base.Null -> raise Invalid_CEA_alloc_infinite

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
            Cvalue.V_Offsetmap.sized_zero (Bit_utils.memory_size ())
          in
          let new_base =
            Cvalue.Default_offsetmap.create_initialized_var
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

let frama_c_dump_assert state _actuals =
  Value_parameters.result ~current:true "Frama_C_dump_assert_each called:@\n(%a)@\nEnd of Frama_C_dump_assert_each output"
    Cvalue.Model.pretty_c_assert state;
  None, state, Location_Bits.Top_Param.bottom

let () = register_builtin "Frama_C_dump_assert_each" frama_c_dump_assert


(* -------------------------------------------------------------------------- *)
(* --- Builtins not registered in the table                               --- *)
(* -------------------------------------------------------------------------- *)

exception Invalid_CEA_alloc

let alloc_with_validity initial_state actuals =
  try
    let size = match actuals with
      | [_,size,_] -> size
      | _ -> raise Invalid_CEA_alloc
    in
    let size =
      try
        let size = Cvalue.V.project_ival size in
        Ival.project_int size
      with Ival.Not_Singleton_Int | V.Not_based_on_null ->
        raise Invalid_CEA_alloc
    in
    if Int.le size Int.zero then raise Invalid_CEA_alloc;
    let new_name = Format.sprintf "Frama_C_alloc" in
    let new_name = Cabs2cil.fresh_global new_name in
    let bounded_type =
      TArray(
        charType,
        Some (new_exp ~loc:Cil_datatype.Location.unknown
                (Const (CInt64 (size,IInt ,None)))),
        empty_size_cache (),
        [])
    in
    let new_varinfo = makeGlobalVar ~logic:true new_name bounded_type in
    let size_in_bits = Int.mul (Bit_utils.sizeofchar()) size in
    let new_offsetmap = Cvalue.V_Offsetmap.sized_zero ~size_in_bits in
    let new_base =
      Cvalue.Default_offsetmap.create_initialized_var
        new_varinfo
        (Base.Known (Int.zero, Int.pred size_in_bits))
        new_offsetmap
    in
    let loc_without_size = Location_Bytes.inject new_base Ival.zero in
    (wrap_ptr loc_without_size),
    initial_state,
    Location_Bits.Top_Param.bottom
  with Ival.Error_Top | Invalid_CEA_alloc ->
    Value_parameters.error
      "Invalid argument for Frama_C_alloc_size function";
    do_degenerate None;
    raise Db.Value.Aborted

let dump_state initial_state =
  let l = fst (CurrentLoc.get ()) in
  Value_parameters.result
    "DUMPING STATE of file %s line %d@\n%a=END OF DUMP=="
    l.Lexing.pos_fname l.Lexing.pos_lnum
    Cvalue.Model.pretty initial_state;
  None, initial_state, Location_Bits.Top_Param.bottom

module DumpFileCounters =
  State_builder.Hashtbl (Datatype.String.Hashtbl)(Datatype.Int)
    (struct let size = 3
            let kind = `Correctness
            let dependencies = [Db.Value.self]
            let name = "Builtins.DumpFileCounters"
     end)
let dump_state_file name initial_state args =
  (try
     let size = String.length name in
     let name = 
       if size > 23 (* Frama_C_dump_each_file_ + 'something' *) then
         String.sub name 23 (size - 23)
       else failwith "no filename specified"
     in
     let n = try DumpFileCounters.find name with Not_found -> 0 in
     DumpFileCounters.add name (n+1);
     let file = Format.sprintf "%s_%d" name n in
     let ch = open_out file in
     let fmt = Format.formatter_of_out_channel ch in
     let l = fst (CurrentLoc.get ()) in
     Value_parameters.feedback ~current:true "Dumping state in file '%s'%t"
       file Value_util.pp_callstack;
     Format.fprintf fmt "DUMPING STATE at file %s line %d@."
       l.Lexing.pos_fname l.Lexing.pos_lnum;
     if args <> [] then Format.fprintf fmt "Args: %a@." pretty_actuals args;
     Cvalue.Model.pretty fmt initial_state;
     close_out ch
   with e ->
     Value_parameters.warning ~current:true ~once:true
       "Error during, or invalid call to Frama_C_dump_each_file (%s). Ignoring"
       (Printexc.to_string e)
  );
  None, initial_state, Location_Bits.Top_Param.bottom

let dump_args name initial_state actuals =
  Value_parameters.result "Called %s%a%t"
    name
    pretty_actuals actuals
    Value_util.pp_callstack;
  None, initial_state, Location_Bits.Top_Param.bottom


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
