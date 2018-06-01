(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Eval


(** An abstract domain based on Simple_Cvalue that will litterally just print
    what goes through it. *)
module Simple : Simpler_domains.Simple_Cvalue = struct

  let feedback = Value_parameters.feedback ~current:true

  (* --- Datatype --- *)

  (* In this domain, the states contain nothing. We use [unit] as type formal
     the state and we reuse [Datatype.Unit] as a base for our domain. *)
  include Datatype.Unit
  let name = "Printer domain"

  (* --- Lattice operators --- *)

  let top = ()
  let is_included _v1 _v2 =
    feedback "is_included";
    true

  let join _v1 _v2 =
    feedback "join";
    top

  let widen _kf _stmt _v1 v2 =
    feedback "widen";
    v2

  (* --- Query functions --- *)

  let extract_expr _state _exp =
    `Value (Cvalue.V.top)

  let extract_lval _state _lval _typ _loc =
    `Value (Cvalue.V.top)

  (* --- Transfer functions --- *)

  let pp_list = Pretty_utils.pp_list ~sep:",@ "

  let pp_cvalue fmt value =
    Bottom.pretty Cvalue.V.pretty fmt value

  let pp_cvalue_assigned fmt value =
    pp_cvalue fmt (Eval.value_assigned value)

  let pp_arg fmt arg =
    Format.fprintf fmt "%a = %a"
      Printer.pp_exp arg.concrete
      pp_cvalue_assigned arg.avalue

  let assign _kinstr loc exp cvalue_assigned _valuation state =
    feedback "assign %a with %a = %a"
      Printer.pp_lval loc.lval
      Printer.pp_exp exp
      pp_cvalue_assigned cvalue_assigned;
    `Value state

  let assume _stmt exp truth _valuation state =
    feedback "assume %a is %b"
      Printer.pp_exp exp
      truth;
    `Value state

  let start_call _stmt call _valuation state =
    feedback "start call %s(%a)"
      (Kernel_function.get_name call.kf)
      (pp_list pp_arg) call.arguments;
    state

  let finalize_call _stmt call ~pre:_ ~post =
    feedback  "finalize call to %s" (Kernel_function.get_name call.kf);
    `Value post

  let approximate_call _stmt call state =
    feedback "approxmate call %s(%a)"
      (Kernel_function.get_name call.kf)
      (pp_list pp_arg) call.arguments;
    `Value [state]

  (* --- Initialization of variables --- *)

  let pp_vi_list fmt l =
    pp_list Printer.pp_varinfo fmt l

  let pp_init_val fmt = function
   | Abstract_domain.Zero -> Format.fprintf fmt "0"
   | Abstract_domain.Top  -> Format.fprintf fmt "Top"

  let empty () =
    feedback "empty";
    ()

  let introduce_globals vi_list state =
    feedback "introduce_globals %a" pp_vi_list vi_list;
    state

  let initialize_variable lval ~initialized:_ init state =
    feedback "initialize_variable %a with %a"
      Printer.pp_lval lval
      pp_init_val init;
    state

  let enter_scope _kf vi_list state =
    feedback  "enter_scope %a" pp_vi_list vi_list;
    state

  let leave_scope _kf vi_list state =
    feedback "leave_scope %a"  pp_vi_list vi_list;
    state

  (* --- Pretty printer --- *)

  let show_expr _state _fmt exp =
    feedback "show_expr %a" Printer.pp_exp exp
end

include Domain_builder.Complete_Simple_Cvalue (Simple)

