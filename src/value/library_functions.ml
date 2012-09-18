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

open Cil_types
open Cil
open Locations
open Abstract_interp
open Bit_utils

module Retres =
  Kernel_function.Make_Table
    (Cil_datatype.Varinfo)
    (struct
      let name = "retres_variable"
      let size = 9
      let dependencies = [Ast.self]
     end)
let () = Ast.add_monotonic_state Retres.self

let () =
  State_dependency_graph.Static.add_dependencies
    ~from:Retres.self [ Db.Value.self ]

let get = Retres.memo
  (fun kf ->
    let vi = Kernel_function.get_vi kf in
    let typ = Cil.getReturnType vi.vtype in
    makeVarinfo false false "__retres" typ)

let add_retres_to_state ~with_alarms kf offsetmap state =
  let retres_vi = get kf in
  let retres_base = Base.create_varinfo retres_vi in
  let loc = Location_Bits.inject retres_base Ival.zero in
  let size =
    try  Int.of_int (bitsSizeOf retres_vi.vtype)
    with SizeOfError _ ->
      Value_parameters.abort "library function return type size unknown. \
                                Please report"
  in
  let state = Cvalue.Model.paste_offsetmap
    with_alarms offsetmap loc Int.zero size true state
  in
  retres_vi, state


(** Associates [kernel_function] to a fresh base for the address returned by
    the [kernel_function]. *)
module Returned_Val =
  Kernel_function.Make_Table
    (Base)
    (struct
       let dependencies = [Ast.self]
       let size = 7
       let name = "Leaf_Table"
     end)
let () = Ast.add_monotonic_state Returned_Val.self

let register_new_var v typ =
  if isFunctionType typ then
    Globals.Functions.replace_by_declaration (Cil.empty_funspec()) v v.vdecl
  else
    Globals.Vars.add_decl v


let returned_value kf state =
  (* Process return of function *)
  let return_type = unrollType (Kernel_function.get_return_type kf) in
  match return_type with
  | TComp _ when is_fully_arithmetic return_type ->
      Cvalue.V.top_int, state
  | TPtr(typ,_) | (TComp _ as typ) -> begin
      let new_base =
        Returned_Val.memo
          (fun kf ->
             (* Value_parameters.warning
               "Undefined function returning a pointer: %a"
                Kernel_function.pretty kf; *)
             let new_varinfo =
               makeGlobalVar
                 ~logic:true
                 (Cabs2cil.fresh_global
                    ("alloced_return_" ^ Kernel_function.get_name kf))
                 typ
             in
             register_new_var new_varinfo typ;
	     let validity = Base.Known (Int.zero, max_bit_address ()) in
	     ignore (Base.create_logic new_varinfo validity);
             let new_offsetmap =
               Cvalue.V_Offsetmap.sized_isotropic 
		 Cvalue.V_Or_Uninitialized.singleton_zero
		 (memory_size ())
             in
             Cvalue.Default_offsetmap.create_initialized_var
               new_varinfo
	       validity
               new_offsetmap)
          kf
      in
      let initial_value =
        if isIntegralType typ
        then Cvalue.V.top_int
        else if isFloatingType typ
        then Cvalue.V.top_float
        else
          Cvalue.V.inject_top_origin
            (Origin.Leaf (LocationSetLattice.currentloc_singleton()))
            (Cvalue.V.Top_Param.O.singleton new_base)
            (*top_leaf_origin ()*)
      in
      let modu = try
        if isVoidType typ then Int.one else Int_Base.project (osizeof typ)
      with Int_Base.Error_Top ->
        assert (Cvalue.V.is_isotropic initial_value);
        Int.one
      in
      let returned_loc =
        try
          Location_Bytes.inject
            new_base
            (Ival.filter_ge_int (Some Int.zero)
               (Ival.create_all_values
                   ~signed:true
                   ~modu
                   ~size:(sizeofpointer ())))
        with Int_Base.Error_Top ->
          Location_Bytes.inject
            new_base
            Ival.top
      in
      let state =
        Cvalue.Model.create_initial
          ~base:new_base
          ~v:initial_value ~modu:(Int.mul Int.eight modu) ~state
      in
      returned_loc, state
    end
  | TInt _ | TEnum _ ->  Cvalue.V.top_int, state
  | TFloat _ ->  Cvalue.V.top_float, state
  | TBuiltin_va_list _ ->
      Cvalue.V.top_leaf_origin()
        (* Only some builtins may return this type *),
      state
  | TVoid _ -> Cvalue.V.top (* this value will never be used *), state
  | TFun _ | TNamed _ | TArray _ -> assert false


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
