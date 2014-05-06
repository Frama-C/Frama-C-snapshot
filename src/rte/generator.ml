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

module type S = sig
  val is_computed: kernel_function -> bool
  val set: kernel_function -> bool -> unit
  val self: State.t
end

module Make
  (M:sig
    val name:string
    val default: kernel_function -> bool
    val parameter: Typed_parameter.t
    val additional_parameters: Typed_parameter.t list
  end)
  =
struct

  module H = 
    Kernel_function.Make_Table
      (Datatype.Bool)
      (struct
	let name = M.name
	let size = 17
	let dependencies =
          let extract p = State.get p.Typed_parameter.name in
	  Ast.self
	  :: Options.Trivial.self 
	  :: List.map extract (M.parameter :: M.additional_parameters) 
       end)

  let is_computed kf = H.memo M.default kf
  let set = H.replace
  let self = H.self
  let triple = M.name, set, is_computed

end

module Signed =
  Make
    (struct
       let name = "signed_overflow"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Kernel.SignedOverflow.parameter
       let additional_parameters = []
     end)

module Mem_access =
  Make
    (struct
       let name = "mem_access"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Options.DoMemAccess.parameter
       let additional_parameters = [ Kernel.SafeArrays.parameter ]
     end)

module Div_mod =
  Make
    (struct
       let name = "division_by_zero"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Options.DoDivMod.parameter
       let additional_parameters = []
     end)

module Shift =
  Make
    (struct
       let name = "shift_value_out_of_bounds"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Options.DoShift.parameter
       let additional_parameters = []
     end)

module Downcast =
  Make
    (struct
       let name = "downcast"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Kernel.SignedDowncast.parameter
       let additional_parameters = []
     end)

module Unsigned_overflow =
  Make
    (struct
       let name = "unsigned_overflow"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Kernel.UnsignedOverflow.parameter
       let additional_parameters = []
     end)

module Unsigned_downcast =
  Make
    (struct
       let name = "unsigned_downcast"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Kernel.UnsignedDowncast.parameter
       let additional_parameters = []
     end)

module Float_to_int =
  Make
    (struct
       let name = "float_to_int"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Options.DoFloatToInt.parameter
       let additional_parameters = []
     end)

module Called_precond =
  Make
    (struct
       let name = "precondition"
       let default kf = not (Kernel_function.is_definition kf)
       let parameter = Options.DoCalledPrecond.parameter
       let additional_parameters = []
     end)

let proxy =
  State_builder.Proxy.create
    "RTE" 
    State_builder.Proxy.Backward
    [ Signed.self;
      Mem_access.self;
      Div_mod.self;
      Shift.self;
      Downcast.self;
      Unsigned_downcast.self;
      Unsigned_overflow.self;
      Float_to_int.self;
      Called_precond.self ]

let self = State_builder.Proxy.get proxy
let () = Db.RteGen.self := self

let precond_status () = Called_precond.triple
let signed_status () = Signed.triple
let div_mod_status () = Div_mod.triple
let shift_status () = Shift.triple
let downcast_status () = Downcast.triple
let mem_access_status () = Mem_access.triple
let float_to_int_status () = Float_to_int.triple
let unsigned_overflow_status () = Unsigned_overflow.triple
let unsigned_downcast_status () = Unsigned_downcast.triple

let all_status () =
  [ precond_status ();
    signed_status ();
    mem_access_status ();
    div_mod_status ();
    shift_status ();
    downcast_status ();
    float_to_int_status ();
    unsigned_overflow_status ();
    unsigned_downcast_status ();
  ]

let emitter =
    Emitter.create
      "rte"
      [ Emitter.Property_status; Emitter.Alarm ]
      ~correctness:[ Kernel.SafeArrays.parameter ]
      ~tuning:[]

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
 *)
