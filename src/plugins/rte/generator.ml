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

module type S = sig
  val is_computed: kernel_function -> bool
  val set: kernel_function -> bool -> unit
end

module Make
  (M:sig
    val name:string
    val parameter: Typed_parameter.t
    val additional_parameters: Typed_parameter.t list
  end)
  =
struct

  module H = 
    Kernel_function.Make_Table
      (Datatype.Bool)
      (struct
	let name = "RTE.Computed." ^ M.name
	let size = 17
	let dependencies =
          let extract p = State.get p.Typed_parameter.name in
	  Ast.self
	  :: Options.Trivial.self 
	  :: List.map extract (M.parameter :: M.additional_parameters) 
       end)

  let is_computed =
    (* Nothing to do for functions without body. *)
    let default kf = not (Kernel_function.is_definition kf) in
    fun kf -> H.memo default kf
  let set = H.replace
  let self = H.self
  let triple = M.name, set, is_computed

end

module Initialized =
  Make
    (struct
       let name = "initialized"
       let parameter = Options.DoInitialized.parameter
       let additional_parameters = [ Kernel.SafeArrays.parameter ]
     end)

module Mem_access =
  Make
    (struct
       let name = "mem_access"
       let parameter = Options.DoMemAccess.parameter
       let additional_parameters = [ Kernel.SafeArrays.parameter ]
     end)

module Pointer_call =
  Make
    (struct
       let name = "pointer_call"
       let parameter = Options.DoPointerCall.parameter
       let additional_parameters = []
     end)

module Div_mod =
  Make
    (struct
       let name = "division_by_zero"
       let parameter = Options.DoDivMod.parameter
       let additional_parameters = []
     end)

module Shift =
  Make
    (struct
       let name = "shift_value_out_of_bounds"
       let parameter = Options.DoShift.parameter
       let additional_parameters = []
     end)

module Signed_overflow =
  Make
    (struct
       let name = "signed_overflow"
       let parameter = Kernel.SignedOverflow.parameter
       let additional_parameters = []
     end)

module Signed_downcast =
  Make
    (struct
       let name = "downcast"
       let parameter = Kernel.SignedDowncast.parameter
       let additional_parameters = []
     end)

module Unsigned_overflow =
  Make
    (struct
       let name = "unsigned_overflow"
       let parameter = Kernel.UnsignedOverflow.parameter
       let additional_parameters = []
     end)

module Unsigned_downcast =
  Make
    (struct
       let name = "unsigned_downcast"
       let parameter = Kernel.UnsignedDowncast.parameter
       let additional_parameters = []
     end)

module Float_to_int =
  Make
    (struct
       let name = "float_to_int"
       let parameter = Options.DoFloatToInt.parameter
       let additional_parameters = []
     end)


module Finite_float =
  Make
    (struct
       let name = "finite_float"
       let parameter = Kernel.FiniteFloat.parameter
       let additional_parameters = []
     end)


module Called_precond =
  Make
    (struct
       let name = "precondition"
       let parameter = Options.DoCalledPrecond.parameter
       let additional_parameters = []
     end)

let proxy =
  State_builder.Proxy.create
    "RTE" 
    State_builder.Proxy.Backward
    [ Mem_access.self;
      Pointer_call.self;
      Div_mod.self;
      Shift.self;
      Signed_overflow.self;
      Signed_downcast.self;
      Unsigned_overflow.self;
      Unsigned_downcast.self;
      Float_to_int.self;
      Called_precond.self ]

let self = State_builder.Proxy.get proxy
let () = Db.RteGen.self := self

let precond_status = Called_precond.triple
let div_mod_status = Div_mod.triple
let shift_status = Shift.triple
let signed_overflow_status = Signed_overflow.triple
let signed_downcast_status = Signed_downcast.triple
let initialized_status = Initialized.triple
let mem_access_status = Mem_access.triple
let pointer_call_status = Pointer_call.triple
let float_to_int_status = Float_to_int.triple
let unsigned_overflow_status = Unsigned_overflow.triple
let unsigned_downcast_status = Unsigned_downcast.triple
let float_to_int = Float_to_int.triple
let finite_float = Finite_float.triple

let all_status =
  [ precond_status;
    initialized_status;
    mem_access_status;
    pointer_call_status;
    div_mod_status;
    shift_status;
    signed_overflow_status;
    signed_downcast_status;
    unsigned_overflow_status;
    unsigned_downcast_status;
    float_to_int_status;
  ]

let emitter =
    Emitter.create
      "rte"
      [ Emitter.Property_status; Emitter.Alarm ]
      ~correctness:[ Kernel.SafeArrays.parameter ]
      ~tuning:[]

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
