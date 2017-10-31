(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
open Cvalue

module Retres =
  Kernel_function.Make_Table
    (Datatype.Option(Cil_datatype.Varinfo))
    (struct
      let name = "Value.Library_functions.Retres"
      let size = 9
      let dependencies = [Ast.self]
     end)
let () = Ast.add_monotonic_state Retres.self

let () =
  State_dependency_graph.add_dependencies ~from:Retres.self [ Db.Value.self ]

let get_retres_vi = Retres.memo
  (fun kf ->
    let vi = Kernel_function.get_vi kf in
    let typ = Cil.getReturnType vi.vtype in
    if Cil.isVoidType typ then
      None
    else
      try
        ignore (Cil.bitsSizeOf typ);
        let name = Format.asprintf "\\result<%a>" Kernel_function.pretty kf in
        Some (makeVarinfo false false name typ)
      with Cil.SizeOfError _ ->
        Value_parameters.abort ~current:true
          "function %a returns a value of unknown size. Aborting"
          Kernel_function.pretty kf
  )

(*  Associates [kernel_function] to a fresh base for the address returned by
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

let create_alloced_return typ kf =
  let f kf =
    let v_name =
      Cabs2cil.fresh_global
        ("alloced_return_" ^ Kernel_function.get_name kf)
    in
    let new_varinfo = Value_util.create_new_var v_name typ in
    let validity = Base.validity_from_size (max_bit_size ()) in
    Base.register_memory_var new_varinfo validity
  in
  Returned_Val.memo f kf

let returned_value kf state =
  (* Process return of function *)
  let return_type = unrollType (Kernel_function.get_return_type kf) in
  match return_type with
  | TComp _ when is_fully_arithmetic return_type ->
      Cvalue.V.top_int, state
  | TPtr(typ,_) | (TComp _ as typ) -> begin
      let size = max_bit_size () in
      let new_base = create_alloced_return typ kf in
      let initial_value =
        match Cil.unrollType typ with
          | TInt _ | TEnum _ -> V.top_int
          | TFloat (fk, _) -> begin
            match Value_util.float_kind fk with
            | Fval.Float32 -> V.top_single_precision_float
            | Fval.Float64 -> V.top_float
          end
          | _ ->
              let origin = Origin.current Origin.K_Leaf in
              V.inject_top_origin origin (Base.Hptset.singleton new_base)
      in
      (* top_float is not isotropic, we need a size to initialize the
         offsetmap bound to [base] *)
      let size_v (* bits *) =
        try
          if isVoidType typ then Int.one else Int_Base.project (sizeof typ)
        with Abstract_interp.Error_Top ->
          assert (Cvalue.V.is_isotropic initial_value);
          Int.one
      in
      let returned_base =
        let size = sizeofpointer() - 1 in
        let modu =
          let size_o = Int.div size_v (Bit_utils.sizeofchar ()) in
          (* Catch cases 1/8 (isotropic values) and 0/8 (if [typ] is an empty
             struct or incomplete *)
          if Int.is_zero size_o then Int.one else size_o
        in
        Location_Bytes.inject
          new_base (Ival.create_all_values_modu ~signed:false ~modu ~size)
      in
      let returned_value = V.join V.top_int returned_base in
      let v = Cvalue.V_Or_Uninitialized.initialized initial_value in
      let offsm = Cvalue.V_Offsetmap.create ~size v ~size_v in
      (* TODO: this overwrites the entire previous states *)
      let state = Cvalue.Model.add_base new_base offsm state in
      returned_value, state
    end
  | TInt _ | TEnum _ ->  Cvalue.V.top_int, state
  | TFloat _ ->  Cvalue.V.top_float, state
  | TBuiltin_va_list _ ->
      Value_parameters.error ~current:true ~once:true
        "functions returning variadic arguments must be stubbed%t"
        Value_util.pp_callstack;
      V.top_int, state
  | TVoid _ -> Cvalue.V.top (* this value will never be used *), state
  | TFun _ | TNamed _ | TArray _ -> assert false


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
