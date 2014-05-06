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

open Cil_datatype

 type state_imp = State_imp.t;;

 type record =
      {
	superposition : State_imp.t ;
        mutable widening : int ;
        mutable widening_state : Cvalue.Model.t ;
	mutable counter_unroll : int ;
      }

  let empty_record () =
    { superposition = State_imp.empty () ;
      widening = Value_parameters.WideningLevel.get () ;
      widening_state = Cvalue.Model.bottom ;
      counter_unroll = 0;
    }

  type t = record Stmt.Hashtbl.t

  let create () =
    Stmt.Hashtbl.create 257

  let clear t = Stmt.Hashtbl.clear t

  let find_current current_table kinstr =
    try
      Stmt.Hashtbl.find current_table kinstr
    with Not_found ->
      let record = empty_record () in
      Stmt.Hashtbl.add current_table kinstr record;
      record

  let find_widening_info current_table kinstr =
    let r = find_current current_table kinstr in
    r.widening, r.widening_state

  let update_and_tell_if_changed current_table kinstr d =
    let record = find_current current_table kinstr in
    if Cvalue.Model.is_reachable record.widening_state
    then
      let (j,tr) = State_set.join d in
      if Cvalue.Model.is_included j record.widening_state
      then State_set.empty
      else State_set.singleton (j,tr)
    else
      State_imp.merge_set_return_new d record.superposition


   let update_widening_info current_table kinstr wcounter wstate =
     let record = find_current current_table kinstr in
     record.widening <- wcounter;
     record.widening_state <- wstate

  let merge_db_table hash_states callstack =
   let treat_stmt k sum =
      let current_state = Db.Value.noassert_get_stmt_state k in
      let is_top_already =
        Cvalue.Model.is_top current_state
      in
      if not is_top_already
      then Db.Value.update_table k sum;
      if Value_parameters.ResultsCallstack.get () then 
	Db.Value.update_callstack_table ~after:false k callstack sum
   in
   Stmt.Hashtbl.iter treat_stmt (Lazy.force hash_states)

   let superpositions current_table =
     let r = Stmt.Hashtbl.create (Stmt.Hashtbl.length current_table)
     in
     Stmt.Hashtbl.iter
       (fun k record ->
         let sup = State_imp.to_list record.superposition in
         Stmt.Hashtbl.add r k sup)
       current_table;
     r

   let states current_table =
     let r = Stmt.Hashtbl.create (Stmt.Hashtbl.length current_table)
     in
     Stmt.Hashtbl.iter
       (fun k record ->
         Stmt.Hashtbl.add r k
           (Cvalue.Model.join
               (State_imp.join record.superposition)
                record.widening_state))
       current_table;
     r


   let find_superposition current_table s =
     let record = find_current current_table s in
     let s = State_imp.to_set record.superposition in
     if Cvalue.Model.is_reachable record.widening_state
     (* Forget about the trace. TODO: preserve the trace. *)
     then State_set.add (record.widening_state, Trace.top) s
     else s


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
