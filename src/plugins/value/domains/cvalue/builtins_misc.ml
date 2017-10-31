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
           let state = !Db.Value.reduce_by_cond state arg_exp true in
           if Cvalue.Model.is_reachable state
           then (warning_once_current "Frama_C_assert: unknown"; state)
           else do_bottom ()
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


(* -------------------------------------------------------------------------- *)
(* --- Multi-names builtins, not registered in the table                  --- *)
(* -------------------------------------------------------------------------- *)

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

