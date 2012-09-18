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
open Abstract_interp
open Locations

exception Actual_is_bottom
exception WrongFunctionType (* at a call through a pointer *)

(* We cannot statically check that a call through a function pointer is
   correct wrt the number of arguments and their types (see the examples at
   the end of tests/misc/fun_ptr.i). Thus, we make additional checks  here:
   the arguments size are correct, and the number of arguments is sufficient.*)
let check_arg_size expr formal =
  if Cil.bitsSizeOf (Cil.typeOf expr) <> Cil.bitsSizeOf (formal.vtype)
  then raise WrongFunctionType

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ -> raise WrongFunctionType (* Too few arguments *)
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let actualize_formals ?(check = fun _ _ -> ()) kf state actuals =
  let formals = Kernel_function.get_formals kf in
  let treat_one_formal acc (expr, _actual_val,actual_o) formal =
    (check expr formal: unit);
    let loc_without_size =
      Location_Bits.inject (Base.create_varinfo formal) (Ival.zero)
    in
    Cvalue.Model.paste_offsetmap CilE.warn_none_mode
      actual_o
      loc_without_size
      Int.zero
      (Int_Base.project (Bit_utils.sizeof_vid formal))
      true
      acc
  in
  fold_left2_best_effort treat_one_formal state actuals formals

let main_initial_state_with_formals kf (state:Cvalue.Model.t) =
  match kf.fundec with
    | Declaration (_, _, None, _) -> state
    | Declaration (_, _, Some l, _)
    | Definition ({ sformals = l }, _) ->
	if l <> [] && Value_parameters.InterpreterMode.get()
	then begin
	    Value_parameters.error "Entry point %a has arguments"
	      Kernel_function.pretty kf;
	    exit 0;
	  end;
        List.fold_right
          Initial_state.initialize_var_using_type
          l
          state

let compute_actual ~with_alarms (one_library_fun, all_library_funs) state e =
  let interpreted_expr, o = match e with
  | { enode = Lval l }
      when not (Eval_exprs.is_bitfield l ()) ->
      let _, _, interpreted_expr =
	Eval_exprs.eval_lval ~conflate_bottom:false ~with_alarms None state l
      in
      if one_library_fun then
	ignore (Eval_exprs.eval_lval
                  ~conflate_bottom:true ~with_alarms None state l);
      if Cvalue.V.is_bottom interpreted_expr
      then begin
	  if not one_library_fun then (* alarm *)
	    ignore (Eval_exprs.eval_lval
                      ~conflate_bottom:true ~with_alarms None state l);
	  if all_library_funs
	  then begin
	      Value_parameters.result ~current:true
		"Non-termination@ in@ evaluation@ of@ library function@ call@ lvalue@ argument@ @[%a@]" Cil.d_lval l;
	    end;
	  raise Actual_is_bottom;
	end;
      CilE.set_syntactic_context (CilE.SyUnOp e);
      let r = Eval_exprs.do_cast ~with_alarms (Cil.typeOf e) interpreted_expr in
      let _, o = Eval_exprs.offsetmap_of_lv ~with_alarms state l in
      (match o with
      | Some o -> r, o
      | None ->
	  Format.printf "failure in evaluation of function arguments@\n\
		    lval %a -> %a@."
            Cil.d_lval l Cvalue.V.pretty interpreted_expr;
	  assert false)
  | _ ->
      let interpreted_expr = Eval_exprs.eval_expr ~with_alarms state e in
      if Cvalue.V.is_bottom interpreted_expr
      then begin
	  Value_parameters.result ~current:true
	    "Non-termination@ in@ evaluation@ of@ function@ call@ expression@ argument@ @[%a@]"
	    Cil.d_exp e;
	  raise Actual_is_bottom
	end;
      let typ = Cil.typeOf e in
      interpreted_expr,
      Cvalue_convert.offsetmap_of_value ~typ interpreted_expr
  in
  e, interpreted_expr, o



let () =
  Db.Value.add_formals_to_state :=
    (fun state kf exps ->
       try
         let compute_actual = compute_actual
           ~with_alarms:CilE.warn_none_mode (false, false) in
         let actuals = List.map (compute_actual state) exps in
         actualize_formals kf state actuals
       with Actual_is_bottom -> Cvalue.Model.bottom)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
