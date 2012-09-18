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

open Cvalue
open Abstract_interp
open Cil
open Locations
open Value_util
open Cvalue_convert
open Cil_types

exception Invalid_CEA_alloc_infinite
exception Not_found_lonely_key

module Dynamic_Alloc_Infinite_Table =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Locations.Location_Bytes)
    (struct
       let dependencies = [Db.Value.self]
       let size = 79
       let name = "Dynamic_Alloc_Infinite_Table"
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
      Dynamic_Alloc_Infinite_Table.memo
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
          let size = Bit_utils.memory_size () in
          let new_offsetmap =
            Cvalue.V_Offsetmap.sized_isotropic
	      Cvalue.V_Or_Uninitialized.singleton_zero 
	      size
          in
          let new_base =
            Cvalue.Default_offsetmap.create_initialized_var
              new_varinfo
              (Base.Known (Int.zero, Int.pred size))
              new_offsetmap
          in
          Location_Bytes.inject new_base Ival.zero)
        file
    in
     { Db.Value.builtin_values = [wrap_ptr loc, state] ;
       builtin_clobbered = Location_Bits.Top_Param.bottom }
  with
  | Ival.Error_Top | Invalid_CEA_alloc_infinite
  | Not_found_lonely_key (* from [find_lonely_key] *)
    -> Value_parameters.error
      "Invalid argument for Frama_C_alloc_infinite function: %a"
        pretty_actuals actuals;
      do_degenerate None;
      raise Db.Value.Aborted
  | Not_found -> assert false

let () =
  Builtins.register_builtin "Frama_C_alloc_infinite" frama_c_alloc_infinite


exception Invalid_CEA_alloc

module Dynamic_Alloc_Bases =
  State_builder.Ref
    (Base_Set_Lattice.O)
    (struct
       let dependencies = [Ast.self]
       let name = "Dynamic_Alloc_Bases"
       let default () = Base_Set_Lattice.O.empty
     end)
let () = Ast.add_monotonic_state Dynamic_Alloc_Bases.self


let malloc_var ?(loc=Cil_datatype.Location.unknown) name typ size size_arr state =
  let esize_arr = Cil.kinteger64 ~loc IInt size_arr in
  let type_base =
    if Int.equal Int.one size_arr
    then typ
    else TArray (typ, Some esize_arr, empty_size_cache (), [])
  in
  let var = makeGlobalVar ~logic:true name type_base in
  Library_functions.register_new_var var type_base;
  let size_in_bits = Int.mul (Bit_utils.sizeofchar()) size in
  let offsm =
    V_Offsetmap.sized_isotropic V_Or_Uninitialized.uninitialized ~size_in_bits  
  in
  let validity = Base.Known (Int.zero, Int.pred size_in_bits) in
  ignore (Base.create_logic var validity);
  let new_base = Default_offsetmap.create_initialized_var var validity offsm in
  Dynamic_Alloc_Bases.set
    (Base_Set_Lattice.O.add new_base (Dynamic_Alloc_Bases.get ()));
  let locv = Location_Bytes.inject new_base Ival.zero in
  { Db.Value.builtin_values = [wrap_ptr locv, state] ;
    builtin_clobbered = Location_Bits.Top_Param.bottom },
  var


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
    let new_name = Cabs2cil.fresh_global "Frama_C_alloc" in
    fst (malloc_var new_name charType size size initial_state)
  with Ival.Error_Top | Invalid_CEA_alloc ->
    Value_parameters.error
      "Invalid argument for Frama_C_alloc_size function";
    do_degenerate None;
    raise Db.Value.Aborted

let () = Builtins.register_builtin "Frama_C_alloc_size" alloc_with_validity


module MallocedByStack =
  State_builder.Hashtbl(Value_aux.Callstack.Hashtbl)(Cil_datatype.Varinfo)
    (struct
       let name = "Value.Builtins_malloc.MallocedByStack"
       let size = 17
       let dependencies = [Ast.self]
     end)
let () = Ast.add_monotonic_state MallocedByStack.self


let alloc_once_by_stack : Db.Value.builtin_sig = fun state actuals->
  let stack = for_callbacks_stack () in
  try
    let exp, size_v = match actuals with
      | [exp,size,_] -> exp, size
      | _ -> Value_parameters.abort "Invalid argument(s) for stack malloc"
    in
    let cst = Cil.isIntegerConstant exp in
    try
      let v = MallocedByStack.find stack in
      if cst then
        Value_parameters.result ~current:true ~once:true
          "@[Re-allocating@ variable@ %a@]" Cil.d_var v
      else
        Value_parameters.result ~current:true ~once:true
          "@[Re-allocating@ variable@ %a@ of size %d@ (requested: %a)@]"
          Cil.d_var v (sizeOf_int v.vtype) V.pretty size_v;
      (* Reset state to uninitialized *)
      let state = Model.remove_base (Base.find v) state in
      let locv = Location_Bytes.inject (Base.find v) Ival.zero in
      { Db.Value.builtin_values = [wrap_ptr locv, state] ;
        builtin_clobbered = Location_Bits.Top_Param.bottom }

    with Not_found -> (* Variable has not yet been allocated *)
      let size =
        if not cst then Int.of_int 100000
        else Ival.project_int (Cvalue.V.project_ival size_v)
      in
      let kf_caller = fst (List.nth stack 1) in
      let line = match snd (List.hd stack) with
        | Kglobal -> 0
        | Kstmt s -> (fst (Cil_datatype.Stmt.loc s)).Lexing.pos_lnum
      in
      let new_name = Pretty_utils.sfprintf
        "__malloc_%a_l%d" Kernel_function.pretty kf_caller line
      in
      let new_name = Cabs2cil.fresh_global new_name in
      let typ, size_arr = match (List.hd (call_stack ())).call_site with
        | Kstmt {skind = Instr (Call (Some lv, _, _, _))} ->
            (match Cil.unrollType (typeOfLval lv) with
               | TPtr (t, _) when not (Cil.isVoidType t) ->
                   (try
                      let s = Int.of_int (sizeOf_int t) in
                      if Int.equal (Int.rem size s) Int.zero
                      then t, Int.div size s
                      else charType, size
                    with Cil.SizeOfError _ -> charType, size)
               | _ -> charType, size
            )
        | _ -> charType, size
      in
      let res, v = malloc_var ~loc:exp.eloc new_name typ size size_arr state in
      if cst then
        Value_parameters.result ~current:true ~once:true
          "Allocating variable %a" Cil.d_var v
      else
        Value_parameters.result ~current:true ~once:true
          "@[Allocating@ variable@ %a@ of size %a@ instead of@ requested %a@]"
          Cil.d_var v Int.pretty size V.pretty size_v;
      MallocedByStack.add stack v;      
      res
  with Ival.Error_Top | Invalid_CEA_alloc ->
    Value_parameters.error
      "Invalid argument for Frama_C_alloc_size function";
    do_degenerate None;
    raise Db.Value.Aborted

let () = Builtins.register_builtin "Frama_C_alloc_by_stack" alloc_once_by_stack


(* Change all references to bases into ESCAPINGADDR into the given state,
   and remove thoses bases from the state entirely *)
let free ~exact bases state =
  let state_removed = Base_Set_Lattice.O.fold
    (fun base state -> Cvalue.Model.remove_base base state)
    bases state
  in
  let state = if exact then state_removed else Model.join state state_removed in
  let is_the_base_to_free x = Base_Set_Lattice.O.mem x bases in
  let offsetmap_top_addresses_of_locals =
    Locals_scoping.offsetmap_top_addresses_of_locals is_the_base_to_free
  in
  let state_top_addresses_of_locals =
    Locals_scoping.state_top_addresses_of_locals 
      (fun _ _ -> ()) (* no informative message *)
      offsetmap_top_addresses_of_locals ~exact Location_Bits.Top_Param.top
  in
  state_top_addresses_of_locals state

(** Builtin for [free] function *)
let frama_c_free state actuals =
  try begin match actuals with
    | [ _, arg, _ ] ->
        (* Categorizes the bases in arg *)
        let f base offset (acc, card) =
	  let allocated_base =
            Base_Set_Lattice.O.mem base (Dynamic_Alloc_Bases.get())
	  in
          (* Does arg contain at least one invalid value? *)
	  if (not allocated_base && not (Base.is_null base))
            || Ival.contains_non_zero offset
	  then Value_util.warning_once_current
	    "Wrong free: assert(pass a freeable address)";
          (* Collect the bases that are correct and that will be freed.
             Also collect the fact that more than one base will be removed,
             including NULL. *)
	  if allocated_base && Ival.contains_zero offset
	  then (Base_Set_Lattice.O.add base acc, card+1)
	  else
            let null = Base.is_null base && Ival.contains_zero offset
            in (acc, card + if null then 1 else 0)
	in
	let good_bases, card =
          Cvalue.V.fold_i f arg (Base_Set_Lattice.O.empty, 0)
        in
        let state =
          if card = 0 then Model.bottom
          else if card = 1 then free ~exact:true good_bases state
          else free ~exact:false good_bases state
        in
        (* TODO: reduce on arg if it is an lval *)
	{ Db.Value.builtin_values = [None, state];
	  builtin_clobbered = Location_Bits.Top_Param.bottom }
    | _ ->
        Value_parameters.error
	  "Invalid argument for Frama_C_free function";
        do_degenerate None;
        raise Db.Value.Aborted
    end
  with
  | Db.Value.Outside_builtin_possibilities ->
      Value_parameters.error
	"Outside possibilities for Frama_C_free function";
      do_degenerate None;
      raise Db.Value.Aborted

let () = Builtins.register_builtin "Frama_C_free" frama_c_free


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
