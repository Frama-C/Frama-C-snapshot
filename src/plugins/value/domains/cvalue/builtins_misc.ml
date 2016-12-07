(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

open Abstract_interp
open Locations
open Value_util


let frama_C_assert state actuals =
  let do_bottom () =
    warning_once_current "Frama_C_assert: false";
    Cvalue.Model.bottom
  in
  match actuals with
  | [arg_exp, arg, _arg_offsm] -> begin
        let state =
	  if Cvalue.V.is_zero arg 
	  then do_bottom ()
	  else if Cvalue.V.contains_zero arg 
	  then begin
	      try
		let state =
		  Eval_exprs.reduce_by_cond state
		    { Eval_exprs.exp = arg_exp ; positive = true }
		in
		warning_once_current "Frama_C_assert: unknown";
		state
	      with Eval_exprs.Reduce_to_bottom -> 
		do_bottom ()
	    end
	  else begin
	      warning_once_current "Frama_C_assert: true";
	      state
	    end
        in
	{ Value_types.c_values = [ None, state ] ;
	  c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.NoCache;
        }
      end
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () = Builtins.register_builtin "Frama_C_assert" frama_C_assert


let frama_c_bzero state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback "Call to builtin bzero(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
    match actuals with
    | [(exp_dst, dst, _); (exp_size, size, _)] ->
        let with_alarms = warn_all_quiet_mode () in
        let size =
          try
	    let size = Cvalue.V.project_ival size in
            Int.mul Int.eight (Ival.project_int size)
          with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int ->
            raise Db.Value.Outside_builtin_possibilities
        in
        let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
        let array_dst = Logic_utils.array_with_range exp_dst term_size in
        Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst);
        if not (Cvalue.V.cardinal_zero_or_one dst)
        then raise Db.Value.Outside_builtin_possibilities;
        let left = loc_bytes_to_loc_bits dst
        and offsm_repeat =
          Cvalue.V_Offsetmap.create_isotropic ~size
            (Cvalue.V_Or_Uninitialized.initialized Cvalue.V.singleton_zero)
        in
        let state =
          if Int.gt size Int.zero then
            Eval_op.paste_offsetmap ~reducing:false ~with_alarms
              ~from:offsm_repeat ~dst_loc:left ~size:size ~exact:true state
          else state
        in
        { Value_types.c_values = [ None, state ] ;
	  c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.Cacheable;
        }
    | _ -> raise (Builtins.Invalid_nb_of_args 3)

let () = Builtins.register_builtin "Frama_C_bzero" frama_c_bzero


(* -------------------------------------------------------------------------- *)
(* --- Multi-names builtins, not registered in the table                  --- *)
(* -------------------------------------------------------------------------- *)

let dump_state initial_state _ =
  let l = fst (Cil.CurrentLoc.get ()) in
  Value_parameters.result
    "DUMPING STATE of file %s line %d@\n%a\n=END OF DUMP=="
    (Filepath.pretty l.Lexing.pos_fname) l.Lexing.pos_lnum
    Cvalue.Model.pretty initial_state;
       { Value_types.c_values = [None, initial_state];
	 c_clobbered = Base.SetLattice.bottom;
         c_from = None;
         c_cacheable = Value_types.NoCache;
       }

module DumpFileCounters =
  State_builder.Hashtbl (Datatype.String.Hashtbl)(Datatype.Int)
    (struct let size = 3
            let dependencies = [Db.Value.self]
            let name = "Builtins.DumpFileCounters"
     end)
let dump_state_file name initial_state args =
  (try
     let size = String.length name in
     let name = 
       if size > 23 
	 (* 0    5    1    5    2    5 *)
	 (*  Frama_C_dump_each_file_ + 'something' *) then
         String.sub name 23 (size - 23)
       else failwith "no filename specified"
     in
     let n = try DumpFileCounters.find name with Not_found -> 0 in
     DumpFileCounters.add name (n+1);
     let file = Format.sprintf "%s_%d" name n in
     let ch = open_out file in
     let fmt = Format.formatter_of_out_channel ch in
     let l = fst (Cil.CurrentLoc.get ()) in
     Value_parameters.feedback ~current:true "Dumping state in file '%s'%t"
       file Value_util.pp_callstack;
     Format.fprintf fmt "DUMPING STATE at file %s line %d@."
       (Filepath.pretty l.Lexing.pos_fname) l.Lexing.pos_lnum;
     if args <> [] then Format.fprintf fmt "Args: %a@." pretty_actuals args;
     Cvalue.Model.pretty fmt initial_state;
     close_out ch
   with e ->
     Value_parameters.warning ~current:true ~once:true
       "Error during, or invalid call to Frama_C_dump_each_file (%s). Ignoring"
       (Printexc.to_string e)
  );
  { Value_types.c_values = [None, initial_state];
    c_clobbered = Base.SetLattice.bottom;
    c_from = None;
    c_cacheable = Value_types.NoCache;
  }


(* Builtin for Frama_C_show_each family of functions *)
let dump_args name initial_state actuals =
  (* Print one argument *)
  let pp_one fmt (actual, v, offsm) =
    (* YYY: catch pointers to arrays, and print the contents of the array *)
    Format.fprintf fmt "@[";
    let card = Cvalue.V_Offsetmap.fold_on_values (fun _ n -> n+1) offsm 0 in
    if card > 1 (*|| true (* TODO: uninit & co *)*) then begin
      let typ = Cil.typeOf actual in
      Cvalue.V_Offsetmap.pretty_generic ~typ () fmt offsm;
      Eval_op.pretty_stitched_offsetmap fmt typ offsm
    end else if card = 0 then (* the value as a Cvalue.V is misleading *)
      Format.fprintf fmt "%s" (Unicode.emptyset_string ())
    else
      Cvalue.V.pretty fmt v;
    Format.fprintf fmt "@]";
  in
  let pp = Pretty_utils.pp_list ~pre:"@[<hv>" ~sep:",@ " ~suf:"@]" pp_one in
  Value_parameters.result "Called %s(%a)%t" name pp actuals
    Value_util.pp_callstack;
     { Value_types.c_values = [ None, initial_state] ;
       c_clobbered = Base.SetLattice.bottom;
       c_from = None;
       c_cacheable = Value_types.Cacheable;
     }
