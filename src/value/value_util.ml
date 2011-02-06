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

open Db_types
open Cil_types

let get_rounding_mode () =
  if Value_parameters.AllRoundingModes.get ()
  then Ival.Float_abstract.Any
  else Ival.Float_abstract.Nearest_Even

type called_function =
    { called_kf : kernel_function;
      call_site : kinstr;
      called_merge_current : degenerate:bool -> unit}

let call_stack : called_function list ref = ref []

let call_stack_for_callbacks : (kernel_function * kinstr) list ref = ref []


let do_degenerate lv =
  List.iter
    (fun {called_merge_current = merge_current } ->
      merge_current ~degenerate:true)
    !call_stack;
  !Db.Value.degeneration_occurred (CilE.current_stmt ()) lv

let warn_all_quiet_mode () =
  if Value_parameters.verbose_atleast 1 then
    CilE.warn_all_mode
  else
    { CilE.warn_all_mode with CilE.imprecision_tracing = CilE.Aignore }


let pretty_actuals fmt actuals =
  Pretty_utils.pp_flowlist (fun fmt (_,x,_) -> Cvalue_type.V.pretty fmt x)
    fmt actuals

let pretty_call_stack fmt callstack =
  Pretty_utils.pp_flowlist ~left:"" ~sep:" <-" ~right:""
     (fun fmt {called_kf = kf} -> Kernel_function.pretty_name fmt kf)
    fmt
    callstack

let clear_call_stack () =
  call_stack := [];
  call_stack_for_callbacks := []

let pop_call_stack () =
  call_stack := List.tl !call_stack;
  call_stack_for_callbacks := List.tl !call_stack_for_callbacks

let push_call_stack cf =
  call_stack := cf :: !call_stack;
  call_stack_for_callbacks :=
    (cf.called_kf, cf.call_site) :: !call_stack_for_callbacks

let current_kf () = (List.hd !call_stack).called_kf

let call_stack () = !call_stack
let for_callbacks_stack () = !call_stack_for_callbacks

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
