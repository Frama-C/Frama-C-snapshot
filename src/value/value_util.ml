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
