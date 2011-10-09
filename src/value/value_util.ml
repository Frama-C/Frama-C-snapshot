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

open Cil_types

(** Callstacks related types and functions *)

type called_function =
    { called_kf : kernel_function;
      call_site : kinstr;
      called_merge_current : degenerate:bool -> unit}

let call_stack : called_function list ref = ref []
let call_stack_for_callbacks : (kernel_function * kinstr) list ref = ref []

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

let pretty_call_stack fmt callstack =
  Pretty_utils.pp_flowlist ~left:"" ~sep:" <- " ~right:""
     (fun fmt {called_kf = kf} -> Kernel_function.pretty fmt kf)
    fmt
    callstack

let pretty_callbacks_call_stack fmt callstack =
  Format.fprintf fmt "@[<hv>";
  List.iter (fun (kf, ki) ->
    Kernel_function.pretty fmt kf;
    match ki with
      | Kglobal -> ()
      | Kstmt stmt -> Format.fprintf fmt " :: %a <-@ "
          Cil_datatype.Location.pretty (Cil_datatype.Stmt.loc stmt)
  ) callstack;
  Format.fprintf fmt "@]"

let pp_callstack fmt =
  if Value_parameters.PrintCallstacks.get () then
    Format.fprintf fmt "@ stack: %a"
      pretty_callbacks_call_stack !call_stack_for_callbacks
;;

(** Misc *)

let get_rounding_mode () =
  if Value_parameters.AllRoundingModes.get ()
  then Ival.Float_abstract.Any
  else Ival.Float_abstract.Nearest_Even

let do_degenerate lv =
  List.iter
    (fun {called_merge_current = merge_current } ->
      merge_current ~degenerate:true)
    (call_stack ());
  !Db.Value.degeneration_occurred (CilE.current_stmt ()) lv

(** Assertions emitted during the analysis *)

let emitter_value = 
  Emitter.create
    "value analysis"
    ~correctness:Value_parameters.parameters_correctness
    ~tuning:Value_parameters.parameters_tuning

let emit_status ppt s =
  Property_status.emit ~distinct:true emitter_value ~hyps:[] ppt s

let warn_all_mode =
  CilE.warn_all_mode { CilE.warn_emitter = emitter_value;
                       warn_deps = [Db.Value.self] }

let warn_all_quiet_mode () =
  if Value_parameters.verbose_atleast 1 then
    warn_all_mode
  else
    { warn_all_mode with CilE.imprecision_tracing = CilE.Aignore }

let get_slevel kf =
  let name = Kernel_function.get_name kf in
  Value_parameters.SlevelFunction.find name

let set_loc kinstr =
  match kinstr with
  | Kglobal -> Cil.CurrentLoc.clear ()
  | Kstmt s -> Cil.CurrentLoc.set (Cil_datatype.Stmt.loc s)

module Got_Imprecise_Value =
  State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Eval.Got_Imprecise_Value"
       let dependencies = [ Db.Value.self ]
       let kind = `Internal
       let default () = false
     end)

let pretty_actuals fmt actuals =
  Pretty_utils.pp_flowlist (fun fmt (_,x,_) -> Cvalue.V.pretty fmt x)
    fmt actuals

let pretty_current_cfunction_name fmt =
  Kernel_function.pretty fmt (current_kf())

let warning_once_current fmt =
  Value_parameters.warning ~current:true ~once:true fmt


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
